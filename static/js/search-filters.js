/* search-filters.js — Epistemic effort + archive filters for the search page.
 *
 * Loads /data/epistemic-meta.json (a map of URL → epistemic fields) and
 * /data/archive-meta.json (a map of /archive/ page URL → link-rot status)
 * and restricts search results to pages matching the active filters.
 * Works for both Pagefind keyword results and semantic results.
 *
 * Naming: the epistemic `status` filter (draft / working model / …) and
 * the archive link status (live / moved / rotted / error) are distinct
 * dimensions — the latter lives under `archiveMode` / `archiveStatus`
 * and its own button classes, never touching the epistemic namespace.
 *
 * Reuses the same CSS classes and filter-panel markup as library.html
 * so the two pages look and behave identically.
 *
 * Where filtering happens (F04)
 * -----------------------------
 * Semantic: filtering is pushed *into* the ranker via
 * window.lnSemanticSearch.setResultFilter, so it runs over the whole
 * scored list before the top-8 cut. Excluding a high-ranked passage now
 * promotes the next passing one instead of leaving a hole.
 *
 * Keyword: Pagefind ranks, counts, and paginates inside its own bundle
 * and its index carries no epistemic fields, so hits can only be removed
 * after render. Two corrections keep that honest: the result count and
 * the empty state are recomputed from the filtered set rather than
 * repeating Pagefind's unfiltered total, and further result pages are
 * loaded automatically until enough passing results are on screen (or the
 * result set is exhausted). Moving the epistemic fields into the Pagefind
 * index as real filters would be the structural fix and would let
 * Pagefind's own filter API do this before ranking.
 *
 * Policy for unclassified pages
 * -----------------------------
 * passes(null) === true: a page with no epistemic frontmatter (the vita,
 * indexes, utility pages) is never removed by the epistemic filters —
 * those narrow the classified material only. The archive/link-status
 * filters do apply to every result. This is stated to the reader in the
 * filter panel (content/search.md, .filter-note).
 */
(function () {
    'use strict';

    /* v2: the scope and practicality scales gained `local` and `low`
       (they now match build/Marks.hs), which shifted the saved button
       indices; a v1 state would silently mean something else. */
    var KEY = 'search-filter-state-v2';

    var SCALES = {
        scope:        ['personal', 'local', 'average', 'broad', 'civilizational'],
        novelty:      ['conventional', 'moderate', 'idiosyncratic', 'innovative'],
        practicality: ['abstract', 'low', 'moderate', 'high', 'exceptional'],
        stability:    ['volatile', 'revising', 'fairly stable', 'stable', 'established']
    };

    var state = {
        status: [],
        confidence: null,
        importance: null,
        evidence: null,
        score: null,
        scope: null,
        novelty: null,
        practicality: null,
        stability: null,
        archiveMode: null,     /* null | 'exclude' | 'only' */
        archiveStatus: []      /* link-rot statuses; archive results only */
    };

    var epistemicMeta = null;  /* URL → {status, confidence, …} loaded lazily */
    var archiveMeta = null;    /* /archive/ URL → {status: live|moved|…} */

    /* ---- Persistence ---- */

    /* A stored value is taken only if it has the shape its field has
       here; anything else leaves the default (audit J14). Storage is the
       reader's own, but a hand edit, another version of this script or a
       broken write must not throw at load: a non-array `status` used to
       make hasActiveFilters throw before the panel was wired. */
    function validStored(k, v) {
        if (k === 'status' || k === 'archiveStatus') {
            return Array.isArray(v) && v.every(function (x) { return typeof x === 'string'; });
        }
        if (v === null) return true;
        if (k === 'archiveMode') return v === 'exclude' || v === 'only';
        if (SCALES.hasOwnProperty(k)) {
            return typeof v === 'number' && v % 1 === 0 && v >= 0 && v < SCALES[k].length;
        }
        return typeof v === 'number' && isFinite(v);
    }

    function load() {
        try {
            var raw = localStorage.getItem(KEY);
            if (raw) {
                var obj = JSON.parse(raw);
                if (!obj || typeof obj !== 'object') return;
                for (var k in state) {
                    if (Object.prototype.hasOwnProperty.call(obj, k) && validStored(k, obj[k])) {
                        state[k] = obj[k];
                    }
                }
            }
        } catch (e) {}
    }

    function save() {
        try { localStorage.setItem(KEY, JSON.stringify(state)); } catch (e) {}
    }

    /* ---- Metadata loading ---- */

    var metaPromise = null;
    var archiveMetaPromise = null;

    function loadEpistemicMeta() {
        if (epistemicMeta) return Promise.resolve(epistemicMeta);
        if (metaPromise) return metaPromise;
        metaPromise = fetch('/data/epistemic-meta.json')
            .then(function (r) { return r.ok ? r.json() : {}; })
            .catch(function () { return {}; })
            .then(function (data) { epistemicMeta = data; return data; });
        return metaPromise;
    }

    function loadArchiveMeta() {
        if (archiveMeta) return Promise.resolve(archiveMeta);
        if (archiveMetaPromise) return archiveMetaPromise;
        archiveMetaPromise = fetch('/data/archive-meta.json')
            .then(function (r) { return r.ok ? r.json() : {}; })
            .catch(function () { return {}; })
            .then(function (data) { archiveMeta = data; return data; });
        return archiveMetaPromise;
    }

    /* Both maps load together: epistemicMeta doubles as the
       "metadata is ready" marker in the apply/observer paths. */
    function loadMeta() {
        return Promise.all([loadEpistemicMeta(), loadArchiveMeta()]);
    }

    /* ---- Filtering logic ---- */

    function passes(meta) {
        if (!meta) return true;  /* no metadata = don't filter out */

        if (state.status.length) {
            var s = (meta.status || '').toLowerCase();
            if (!s || state.status.indexOf(s) === -1) return false;
        }
        if (state.confidence !== null) {
            if (!meta.confidence || +meta.confidence < state.confidence) return false;
        }
        if (state.importance !== null) {
            if (!meta.importance || +meta.importance < state.importance) return false;
        }
        if (state.evidence !== null) {
            if (!meta.evidence || +meta.evidence < state.evidence) return false;
        }
        if (state.score !== null) {
            if (!meta.score || +meta.score < state.score) return false;
        }

        var ords = ['scope', 'novelty', 'practicality', 'stability'];
        for (var i = 0; i < ords.length; i++) {
            var k = ords[i];
            if (state[k] !== null) {
                var v = (meta[k] || '').toLowerCase();
                var idx = SCALES[k].indexOf(v);
                if (idx === -1 || idx < state[k]) return false;
            }
        }

        return true;
    }

    /* Archive dimension. `archiveMode` governs whether /archive/ pages
       appear at all (exclude) or alone (only); `archiveStatus` further
       restricts archive results to the selected link-rot statuses and
       never affects non-archive results. */
    function isArchiveUrl(p) {
        return !!p && p.indexOf('/archive/') === 0;
    }

    function passesArchive(url) {
        var isArch = isArchiveUrl(url);
        if (state.archiveMode === 'exclude' && isArch) return false;
        if (state.archiveMode === 'only' && !isArch) return false;
        if (state.archiveStatus.length && isArch && archiveMeta) {
            var rec = archiveMeta[url];
            var s = rec && rec.status;
            if (!s || state.archiveStatus.indexOf(s) === -1) return false;
        }
        return true;
    }

    function hasActiveFilters() {
        if (state.status.length) return true;
        if (state.archiveMode !== null || state.archiveStatus.length) return true;
        var fields = ['confidence', 'importance', 'evidence', 'score',
                      'scope', 'novelty', 'practicality', 'stability'];
        for (var i = 0; i < fields.length; i++) {
            if (state[fields[i]] !== null) return true;
        }
        return false;
    }

    /* ---- URL extraction ---- */

    /* Normalise a URL to a pathname for lookup in epistemicMeta.
       Pagefind results use full URLs; semantic results use relative paths.
       epistemicMeta keys are emitted as routed paths (".../index.html"),
       while result links use the clean directory form (".../"), so the
       trailing-slash form must be expanded before lookup. */
    function normUrl(href) {
        if (!href) return null;
        try {
            var u = new URL(href, window.location.origin);
            var p = u.pathname;
            if (p.charAt(p.length - 1) === '/') p += 'index.html';
            return p;
        } catch (e) {
            return href;
        }
    }

    /* ---- Apply filters to rendered results ---- */

    /* Does one result URL survive the active filters? */
    function accepts(url) {
        return passesArchive(url) && passes(url ? epistemicMeta[url] : null);
    }

    /* ---- Pagefind ---- */

    /* Enough visible hits to make a first page worth reading. Mirrors
       Pagefind UI's default pageSize, so an unfiltered search and a
       filtered one show a comparable amount before "Load more". */
    var PF_TARGET_VISIBLE = 5;
    /* Hard stop so a filter that excludes nearly everything cannot walk
       the entire index one page at a time. */
    var PF_MAX_AUTOLOAD   = 10;

    var pfAutoloads   = 0;
    var pfLastCount   = -1;
    var pfTerm        = null;
    var pfOriginalMsg = null;   /* last message Pagefind itself wrote */
    var pfWrittenMsg  = null;   /* last message we wrote over it */

    function pfInput()   { return document.querySelector('#search .pagefind-ui__search-input'); }
    function pfMessage() { return document.querySelector('#search .pagefind-ui__message'); }
    function pfMoreBtn() {
        var b = document.querySelector('#search .pagefind-ui__button');
        return (b && !b.disabled) ? b : null;
    }

    /* Pagefind's English message is "[COUNT] results for [TERM]" or
       "No results for [TERM]"; a leading integer is the count, its
       absence means zero. Returns null when no Pagefind-authored message
       has been seen yet. */
    function pfTotal() {
        if (pfOriginalMsg === null) return null;
        var m = /^\s*(\d[\d,]*)\b/.exec(pfOriginalMsg);
        return m ? parseInt(m[1].replace(/,/g, ''), 10) : 0;
    }

    function pfWriteMessage(el, text) {
        if (!el) return;
        /* The mutation observer below watches this subtree; writing the
           same string again would re-trigger it forever. */
        if (el.textContent === text) return;
        el.textContent = text;
        pfWrittenMsg = text;
    }

    function pfRestoreMessage(el) {
        if (!el || pfOriginalMsg === null) return;
        if (el.textContent !== pfWrittenMsg) return;   /* Pagefind owns it again */
        pfWriteMessage(el, pfOriginalMsg);
        pfWrittenMsg = null;
    }

    /* Load further result pages until enough passing results are visible.
       Each click is gated on the previous one having actually added
       results, so a stalled or exhausted list stops the loop. */
    function pfLoadMore(visible, total) {
        var btn = pfMoreBtn();
        if (!btn) return;
        if (total !== null && visible >= Math.min(PF_TARGET_VISIBLE, total)) return;
        if (visible >= PF_TARGET_VISIBLE) return;
        if (pfAutoloads >= PF_MAX_AUTOLOAD) return;

        var count = document.querySelectorAll('#search .pagefind-ui__result').length;
        if (count === pfLastCount) return;   /* last click produced nothing new */
        pfLastCount = count;
        pfAutoloads++;
        btn.click();
    }

    function applyToPagefind() {
        var msgEl   = pfMessage();
        var results = document.querySelectorAll('#search .pagefind-ui__result');

        /* A new query resets the autoload budget and the remembered
           message. */
        var input = pfInput();
        var term  = input ? input.value.trim() : '';
        if (term !== pfTerm) {
            pfTerm        = term;
            pfAutoloads   = 0;
            pfLastCount   = -1;
            pfOriginalMsg = null;
            pfWrittenMsg  = null;
        }

        /* Capture Pagefind's own text before overwriting it; anything we
           did not write ourselves is authored by Pagefind. */
        if (msgEl && msgEl.textContent !== pfWrittenMsg) {
            pfOriginalMsg = msgEl.textContent;
        }

        if (!epistemicMeta || !hasActiveFilters()) {
            results.forEach(function (el) { el.classList.remove('search-filtered'); });
            pfRestoreMessage(msgEl);
            return;
        }

        var visible = 0, pending = 0;
        results.forEach(function (el) {
            var link = el.querySelector('.pagefind-ui__result-link');
            if (!link) { pending++; return; }
            var ok = accepts(normUrl(link.getAttribute('href')));
            el.classList.toggle('search-filtered', !ok);
            if (ok) visible++;
        });

        var total = pfTotal();
        pfSummarise(msgEl, visible, total, term);
        /* Pagefind adds a page of results as placeholders and fills each
           in as its data arrives; the observer below runs this again as
           they do. Until every one is in, how many pass is not known: a
           page was loaded on the strength of the placeholders, and a
           filter that two pages would satisfy loaded all of them. */
        if (!pending) pfLoadMore(visible, total);
    }

    /* Counts and empty state describe the filtered set, not Pagefind's
       unfiltered total. */
    function pfSummarise(msgEl, visible, total, term) {
        if (!msgEl || total === null || total === 0) return;  /* nothing filtered yet */
        var about = term ? ' for “' + term + '”' : '';
        var more  = !!pfMoreBtn();
        var text;
        if (visible === 0) {
            text = more
                ? 'Looking for results' + about + ' that match the active filters…'
                : 'No results' + about + ' match the active filters (' + total
                  + ' before filtering).';
        } else if (more) {
            text = visible + ' matching result' + (visible === 1 ? '' : 's')
                 + about + ' so far — ' + total + ' found before filtering.';
        } else {
            text = visible + ' of ' + total + ' result' + (total === 1 ? '' : 's')
                 + about + ' match the active filters.';
        }
        pfWriteMessage(msgEl, text);
    }

    /* ---- Semantic ---- */

    function semanticHook() {
        var api = window.lnSemanticSearch;
        return (api && typeof api.setResultFilter === 'function') ? api : null;
    }

    /* Push the predicate into the ranker so it applies before top-K. */
    function applyToSemantic() {
        var api = semanticHook();
        if (!api) return;
        if (!epistemicMeta || !hasActiveFilters()) {
            api.setResultFilter(null);
            return;
        }
        api.setResultFilter(function (entry) {
            return accepts(normUrl(entry && entry.url));
        });
    }

    function applyFilters() {
        applyToPagefind();
        applyToSemantic();
        syncUI();
        save();
    }

    /* ---- UI sync ---- */

    function activeCount() {
        var n = 0;
        if (state.status.length) n++;
        if (state.archiveMode !== null) n++;
        if (state.archiveStatus.length) n++;
        var fields = ['confidence', 'importance', 'evidence', 'score',
                      'scope', 'novelty', 'practicality', 'stability'];
        for (var i = 0; i < fields.length; i++) {
            if (state[fields[i]] !== null) n++;
        }
        return n;
    }

    function syncUI() {
        var badge = document.querySelector('.filter-toggle-badge');
        var n = activeCount();
        if (badge) badge.textContent = n ? ' (' + n + ')' : '';

        document.querySelectorAll('.filter-status-btn').forEach(function (btn) {
            btn.classList.toggle('is-active', state.status.indexOf(btn.dataset.value) !== -1);
        });

        document.querySelectorAll('.filter-archive-mode-btn').forEach(function (btn) {
            btn.classList.toggle('is-active', state.archiveMode === btn.dataset.value);
        });

        document.querySelectorAll('.filter-archive-status-btn').forEach(function (btn) {
            btn.classList.toggle('is-active', state.archiveStatus.indexOf(btn.dataset.value) !== -1);
        });

        var ci = document.getElementById('filter-confidence');
        if (ci) ci.value = state.confidence !== null ? state.confidence : '';
        var si = document.getElementById('filter-score');
        if (si) si.value = state.score !== null ? state.score : '';

        document.querySelectorAll('.filter-threshold-btn').forEach(function (btn) {
            btn.classList.toggle('is-active', state[btn.dataset.field] === +btn.dataset.value);
        });

        document.querySelectorAll('.filter-ordinal-btn').forEach(function (btn) {
            btn.classList.toggle('is-active', state[btn.dataset.field] === +btn.dataset.index);
        });
    }

    /* ---- Init ---- */

    document.addEventListener('DOMContentLoaded', function () {
        load();

        var panel  = document.getElementById('search-filters');
        var toggle = document.querySelector('.library-filter-toggle');

        if (activeCount() > 0 && panel && toggle) {
            panel.hidden = false;
            toggle.setAttribute('aria-expanded', 'true');
        }

        syncUI();

        /* Load metadata eagerly if filters are active */
        if (hasActiveFilters()) {
            loadMeta().then(applyFilters);
        }

        /* Toggle panel */
        if (toggle && panel) {
            toggle.addEventListener('click', function () {
                var opening = panel.hidden;
                panel.hidden = !opening;
                toggle.setAttribute('aria-expanded', opening ? 'true' : 'false');
            });
        }

        /* Status buttons */
        document.querySelectorAll('.filter-status-btn').forEach(function (btn) {
            btn.addEventListener('click', function () {
                var v = btn.dataset.value;
                var i = state.status.indexOf(v);
                if (i === -1) state.status.push(v);
                else state.status.splice(i, 1);
                loadMeta().then(applyFilters);
            });
        });

        /* Archive mode buttons (exclude / only) — single-select toggle */
        document.querySelectorAll('.filter-archive-mode-btn').forEach(function (btn) {
            btn.addEventListener('click', function () {
                var v = btn.dataset.value;
                state.archiveMode = (state.archiveMode === v) ? null : v;
                loadMeta().then(applyFilters);
            });
        });

        /* Archive link-status buttons — multi-select, archive results only */
        document.querySelectorAll('.filter-archive-status-btn').forEach(function (btn) {
            btn.addEventListener('click', function () {
                var v = btn.dataset.value;
                var i = state.archiveStatus.indexOf(v);
                if (i === -1) state.archiveStatus.push(v);
                else state.archiveStatus.splice(i, 1);
                loadMeta().then(applyFilters);
            });
        });

        /* Threshold buttons (importance, evidence) */
        document.querySelectorAll('.filter-threshold-btn').forEach(function (btn) {
            btn.addEventListener('click', function () {
                var f = btn.dataset.field;
                var v = +btn.dataset.value;
                state[f] = (state[f] === v) ? null : v;
                loadMeta().then(applyFilters);
            });
        });

        /* Ordinal buttons (scope, novelty, practicality, stability) */
        document.querySelectorAll('.filter-ordinal-btn').forEach(function (btn) {
            btn.addEventListener('click', function () {
                var f = btn.dataset.field;
                var idx = +btn.dataset.index;
                state[f] = (state[f] === idx) ? null : idx;
                loadMeta().then(applyFilters);
            });
        });

        /* Number inputs (confidence, trust/score) */
        ['confidence', 'score'].forEach(function (field) {
            var el = document.getElementById('filter-' + (field === 'score' ? 'score' : field));
            if (!el) return;
            el.addEventListener('input', function () {
                var v = el.value.trim();
                var n = parseInt(v, 10);
                /* Non-numeric input deactivates the filter (null) rather
                   than coercing to an always-matching >= 0 threshold. */
                state[field] = (v !== '' && !isNaN(n))
                    ? Math.max(0, Math.min(100, n))
                    : null;
                loadMeta().then(applyFilters);
            });
        });

        /* Clear all */
        var clearBtn = document.querySelector('.filter-clear-btn');
        if (clearBtn) {
            clearBtn.addEventListener('click', function () {
                state.status = [];
                state.confidence = null;
                state.importance = null;
                state.evidence = null;
                state.score = null;
                state.scope = null;
                state.novelty = null;
                state.practicality = null;
                state.stability = null;
                state.archiveMode = null;
                state.archiveStatus = [];
                applyFilters();
            });
        }

        /* Observe Pagefind result changes to re-apply filters, recompute
           the count, and pull further pages. Pagefind dynamically rebuilds
           the results container, and applyToPagefind is written to be
           idempotent (it rewrites the message only when the text actually
           changes) so it cannot drive this observer in a loop. */
        var searchEl = document.getElementById('search');
        if (searchEl) {
            new MutationObserver(function () {
                if (epistemicMeta) applyToPagefind();
            }).observe(searchEl, { childList: true, subtree: true });
        }
    });
}());
