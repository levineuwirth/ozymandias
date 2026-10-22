/* semantic-search.js — Client-side semantic search using paragraph embeddings.
 *
 * At build time, tools/embed.py produces:
 *   /data/semantic-index.bin   raw Float32Array (N_paragraphs × 384 dims)
 *   /data/semantic-meta.json   [{url, title, heading, excerpt}, ...]
 *
 * At query time, transformers.js embeds the user's query with all-MiniLM-L6-v2
 * (same model used at build time) and ranks paragraphs by cosine similarity.
 * All computation is client-side; no server required.
 *
 * Model: Xenova/all-MiniLM-L6-v2 (~22 MB quantized, cached by browser after first load)
 * Model files served from /models/all-MiniLM-L6-v2/ (same-origin; run tools/download-model.sh)
 * Index format: raw little-endian Float32, shape [N, 384], unit-normalized
 *
 * CSP: requires cdn.jsdelivr.net in script-src (transformers.js library).
 *      connect-src stays 'self' — model weights are served same-origin.
 *
 * Dependency pin
 * --------------
 * CDN below is pinned to an exact patch release rather than a floating
 * major (`@2`), so the library cannot change under the site without a
 * source commit. To update:
 *   1. `curl -s https://registry.npmjs.org/@xenova/transformers | jq -r '."dist-tags"'`
 *      (or `npm view @xenova/transformers versions`) and pick the new 2.x patch.
 *   2. Edit CDN here; nothing else references the version.
 *   3. Re-test one first-load search in a private window (cold cache):
 *      the library fetches its own WASM/ONNX runtime from the same pinned
 *      tree, so a bad pin fails at model load, not at import.
 * Staying on 2.x is deliberate — 3.x moved to @huggingface/transformers
 * with a different model-path contract.
 *
 * Public API — window.lnSemanticSearch, consumed by search-filters.js
 * ------------------------------------------------------------------
 *   setResultFilter(fn | null)
 *       fn(metaEntry, url) -> boolean. Applied to the *whole* scored list
 *       before top-K, so an active filter promotes lower-ranked matches
 *       instead of leaving holes in the eight results (F04). Passing a
 *       function or null re-ranks and re-renders immediately from cached
 *       scores — no re-embedding, no model traffic.
 *   rerank()
 *       Recompute top-K and re-render from the cached scores.
 *   activateTab('keyword' | 'semantic')
 *       Programmatic tab switch, same path as a click.
 *   isFiltered()
 *       True while a result filter is installed.
 */
(function () {
    'use strict';

    var MODEL      = 'all-MiniLM-L6-v2';          /* local name, no Xenova/ prefix */
    var MODEL_PATH = '/models/';                   /* served same-origin */
    var DIM        = 384;
    var TOP_K      = 8;
    var CDN        = 'https://cdn.jsdelivr.net/npm/@xenova/transformers@2.17.2';

    /* Approximate first-load weight, quoted to the visitor so the wait is
       explained rather than mysterious. Keep in step with the files under
       static/models/. */
    var MODEL_MB   = 23;

    var extractor = null;   /* loaded lazily on first search */
    var vectors   = null;   /* Float32Array, shape [N, DIM] */
    var meta      = null;   /* [{url, title, heading, excerpt}] */
    var indexReady = false;

    var queryEl   = document.getElementById('semantic-query');
    var statusEl  = document.getElementById('semantic-status');
    var resultsEl = document.getElementById('semantic-results');

    if (!queryEl) return;   /* not on the search page */

    /* ------------------------------------------------------------------
       Index loading — fetch once, lazily
    ------------------------------------------------------------------ */

    /* In-flight promise so concurrent first searches share a single
       index fetch (mirrors loadModelPromise below). Without this guard,
       two rapid keystrokes would each fetch semantic-index.bin and
       semantic-meta.json before the first resolves. */
    var loadIndexPromise = null;

    function loadIndex() {
        if (indexReady) return Promise.resolve();
        if (loadIndexPromise) return loadIndexPromise;

        loadIndexPromise = Promise.all([
            fetch('/data/semantic-index.bin').then(function (r) {
                if (!r.ok) throw new Error('semantic-index.bin not found');
                return r.arrayBuffer();
            }),
            fetch('/data/semantic-meta.json').then(function (r) {
                if (!r.ok) throw new Error('semantic-meta.json not found');
                return r.json();
            }),
        ]).then(function (results) {
            vectors   = new Float32Array(results[0]);
            meta      = results[1];
            /* Consistency check: a stale CDN-cached bin/json pair would
               otherwise produce NaN scores and silently garbage ranking. */
            if (vectors.length !== meta.length * DIM) {
                console.warn('semantic-search: index/meta size mismatch ('
                    + vectors.length + ' floats vs ' + meta.length + ' × ' + DIM
                    + ') — regenerate with `make build` (tools/embed.py).');
                vectors = null;
                meta    = null;
                throw indexError('index/meta size mismatch');
            }
            indexReady = true;
        }).catch(function (err) {
            /* Allow a retry on the next call instead of caching the
               failed promise forever. */
            loadIndexPromise = null;
            if (!err.lnKind) err.lnKind = 'index';
            throw err;
        });
        return loadIndexPromise;
    }

    /* Tagged errors so the visitor-facing copy can distinguish "the index
       is missing" from "the model would not load" without string-matching
       message text. */
    function indexError(detail) {
        var e = new Error('semantic index unavailable: ' + detail);
        e.lnKind = 'index';
        return e;
    }

    /* ------------------------------------------------------------------
       Model loading — dynamic import from CDN, lazy
    ------------------------------------------------------------------ */

    /* In-flight promise so concurrent searches share a single model
       load. Without this guard, two rapid keystrokes would each call
       `import(CDN)` and `pipeline(...)`, wasting CPU and memory before
       the second resolves. */
    var loadModelPromise = null;

    /* Per-file download progress, aggregated across the several files
       transformers.js pulls (tokenizer, config, ONNX weights) so the
       visitor sees one honest percentage rather than a bar that restarts. */
    var progressFiles = {};

    function onModelProgress(p) {
        if (!p || p.status !== 'progress' || !p.total) return;
        progressFiles[p.file || '?'] = { loaded: p.loaded || 0, total: p.total };
        var loaded = 0, total = 0;
        for (var k in progressFiles) {
            if (!progressFiles.hasOwnProperty(k)) continue;
            loaded += progressFiles[k].loaded;
            total  += progressFiles[k].total;
        }
        if (!total) return;
        var pct = Math.min(99, Math.floor((loaded / total) * 100));
        announceModelLoading(pct);
    }

    /* Failed imports so far. Chromium keeps a failed module fetch in the
       page's module map, and import() of that URL then fails again without
       asking the network: after one dropped connection "Try again" could
       never recover. A retry imports a URL of its own; jsDelivr serves the
       same file whatever the query, and CSP source matching ignores it. */
    var importFailures = 0;

    function loadModel() {
        if (extractor) return Promise.resolve(extractor);
        if (loadModelPromise) return loadModelPromise;
        announceModelLoading(null);
        var url = importFailures ? CDN + '?retry=' + importFailures : CDN;
        loadModelPromise = import(url).catch(function (err) {
            importFailures++;
            throw err;
        }).then(function (mod) {
            /* Point transformers.js at our self-hosted model files. */
            mod.env.localModelPath   = MODEL_PATH;
            mod.env.allowRemoteModels = false;
            return mod.pipeline('feature-extraction', MODEL, {
                quantized: true,
                progress_callback: onModelProgress
            });
        }).then(function (pipe) {
            extractor = pipe;
            progressFiles = {};
            return extractor;
        }).catch(function (err) {
            /* Allow a retry on the next call instead of caching the
               failed promise forever. */
            loadModelPromise = null;
            progressFiles = {};
            if (!err.lnKind) err.lnKind = 'model';
            throw err;
        });
        return loadModelPromise;
    }

    /* First-load explanation. Only speaks while a search is actually
       waiting on the model, and never overwrites a fresher generation's
       status (F02). */
    function announceModelLoading(pct) {
        if (extractor) return;
        if (activeGeneration === null) return;
        var msg = pct === null
            ? 'Preparing semantic search — fetching the language model (about '
              + MODEL_MB + ' MB, once per browser).'
            : 'Preparing semantic search — ' + pct + '% of a one-time ~'
              + MODEL_MB + ' MB download.';
        setStatus(msg, activeGeneration);
    }

    /* ------------------------------------------------------------------
       Search
    ------------------------------------------------------------------ */

    function cosineSims(queryVec) {
        /* queryVec is already unit-normalized; dot product = cosine similarity */
        var N       = meta.length;
        var scores  = new Float32Array(N);
        for (var i = 0; i < N; i++) {
            var dot = 0;
            var off = i * DIM;
            for (var d = 0; d < DIM; d++) dot += queryVec[d] * vectors[off + d];
            scores[i] = dot;
        }
        return scores;
    }

    /* Result filter installed by search-filters.js. Applied to the full
       candidate list *before* the top-K cut, which is the whole point of
       the hook: with a filter active, the eight slots are filled by the
       eight best *passing* passages rather than by whatever survives of
       the unfiltered eight. */
    var resultFilter = null;

    function candidates() {
        var out = [];
        for (var i = 0; i < meta.length; i++) {
            if (resultFilter && !resultFilter(meta[i], meta[i].url)) continue;
            out.push(i);
        }
        return out;
    }

    function topK(scores) {
        var indices = candidates();
        indices.sort(function (a, b) { return scores[b] - scores[a]; });
        return indices.slice(0, TOP_K).map(function (i) {
            return { idx: i, score: scores[i] };
        });
    }

    /* Generation token: every input change — including a clear — and every
       runSearch call invalidates all still-in-flight predecessors, so a
       stale query's results and status text can never land after a newer
       one (F02). activeGeneration is the generation currently entitled to
       write status text, or null when nothing is in flight. */
    var searchGeneration = 0;
    var activeGeneration = null;

    /* Cached scores for the last completed query, so filter changes can
       re-rank without re-embedding. */
    var lastScores = null;
    var lastQuery  = '';

    function invalidate() {
        searchGeneration++;
        activeGeneration = null;
    }

    function fresh(gen) {
        return gen === searchGeneration;
    }

    function runSearch(query) {
        var gen = ++searchGeneration;
        activeGeneration = gen;

        query = (query || '').trim();
        if (!query) { invalidate(); clearResults(); setStatus(''); return; }

        lastQuery = query;
        setStatus(extractor ? 'Searching…' : '', gen);
        if (!extractor) announceModelLoading(null);

        var indexPromise = loadIndex();
        var modelPromise = loadModel();

        Promise.all([indexPromise, modelPromise]).then(function (results) {
            /* Freshness check *before* inference, not only before render:
               embedding a superseded query costs real CPU. */
            if (!fresh(gen)) return null;
            setStatus('Searching…', gen);
            return results[1](query, { pooling: 'mean', normalize: true });
        }).then(function (output) {
            if (!output || !fresh(gen)) return;   /* superseded by a newer query */
            lastScores = cosineSims(output.data);   /* Float32Array, length 384 */
            var hits   = topK(lastScores);
            renderResults(hits);
            setStatus(hits.length ? '' : emptyMessage(), gen);
            activeGeneration = null;
        }).catch(function (err) {
            if (!fresh(gen)) return;              /* superseded by a newer query */
            activeGeneration = null;
            showFailure(err, gen);
        });
    }

    function emptyMessage() {
        return resultFilter
            ? 'No results match the active filters. Clear a filter, or try the keyword tab.'
            : 'No results found.';
    }

    /* Re-rank from cached scores. Used when the filter set changes: no
       model, no network, no inference. */
    function rerank() {
        if (!indexReady || !lastScores) return;
        var hits = topK(lastScores);
        renderResults(hits);
        /* Never speak over a search that is still running. */
        if (activeGeneration === null) setStatus(hits.length ? '' : emptyMessage());
    }

    function setResultFilter(fn) {
        resultFilter = (typeof fn === 'function') ? fn : null;
        rerank();
    }

    /* ------------------------------------------------------------------
       Rendering
    ------------------------------------------------------------------ */

    function renderResults(hits) {
        if (!hits.length) { clearResults(); return; }

        var html = '<ol class="semantic-results-list">';
        for (var i = 0; i < hits.length; i++) {
            var h = hits[i];
            var m = meta[h.idx];
            var sameHeading = m.heading === m.title;
            html += '<li class="semantic-result">'
                 + '<a class="semantic-result-title" href="' + esc(m.url) + '">'
                 + esc(m.title) + '</a>';
            if (!sameHeading) {
                html += '<span class="semantic-result-heading"> § ' + esc(m.heading) + '</span>';
            }
            html += '<p class="semantic-result-excerpt">' + esc(m.excerpt) + '</p>'
                 + '</li>';
        }
        html += '</ol>';
        resultsEl.innerHTML = html;
    }

    function clearResults() {
        resultsEl.innerHTML = '';
    }

    /* Visitor-facing failure copy. Build diagnostics stay in the console:
       a reader has no `make build` to run, and "see console for details"
       is not a thing to say to one. */
    function showFailure(err, gen) {
        if (gen !== undefined && !fresh(gen)) return;

        var kind = err && err.lnKind;
        var msg;
        if (kind === 'index') {
            msg = 'Semantic search is unavailable right now — its index could not be loaded.';
            console.warn('semantic-search: index unavailable —'
                + ' /data/semantic-index.bin and /data/semantic-meta.json are'
                + ' produced by tools/embed.py during `make build`.', err);
        } else if (kind === 'model') {
            msg = 'The semantic search model could not be loaded. It is a large one-time '
                + 'download and needs a working connection.';
            console.warn('semantic-search: model load failed (CDN import or'
                + ' /models/ files).', err);
        } else {
            msg = 'Semantic search ran into a problem.';
            console.warn('semantic-search:', err);
        }

        setStatus(msg, gen);
        renderFallback();
    }

    /* Retry + a route to the keyword index, so a failure is never a dead end. */
    function renderFallback() {
        clearResults();

        var box = document.createElement('p');
        box.className = 'semantic-fallback';

        var retry = document.createElement('button');
        retry.type = 'button';
        retry.className = 'filter-btn semantic-retry-btn';
        retry.textContent = 'Try again';
        retry.addEventListener('click', function () {
            if (lastQuery) runSearch(lastQuery);
        });

        var toKeyword = document.createElement('button');
        toKeyword.type = 'button';
        toKeyword.className = 'filter-btn semantic-keyword-btn';
        toKeyword.textContent = 'Search by keyword instead';
        toKeyword.addEventListener('click', function () {
            switchToKeyword(lastQuery);
        });

        box.appendChild(retry);
        box.appendChild(document.createTextNode(' '));
        box.appendChild(toKeyword);
        resultsEl.appendChild(box);
    }

    /* Hand the query to the Pagefind panel. search.js owns the PagefindUI
       instance; its input reacts to a normal input event, so no coupling
       to that module is needed. */
    function switchToKeyword(query) {
        activateTab('keyword');
        var input = document.querySelector('#search .pagefind-ui__search-input');
        if (!input) return;
        if (query) {
            input.value = query;
            input.dispatchEvent(new Event('input', { bubbles: true }));
        }
        input.focus();
    }

    /* Status writes carry the generation that produced them; an obsolete
       search can no longer overwrite the current one's text (F02). */
    function setStatus(msg, gen) {
        if (gen !== undefined && !fresh(gen)) return;
        statusEl.textContent = msg;
    }

    /* Defer to the shared utility (loaded synchronously from
       templates/partials/head.html) so this file cannot drift from
       popups.js, annotations.js, or build/Utils.hs. */
    function esc(s) {
        return window.lnUtils.escapeHtml(s);
    }

    /* ------------------------------------------------------------------
       Tabs — full ARIA tabs pattern (A06)

       role=tablist / tab / tabpanel with aria-controls and
       aria-labelledby come from the markup in content/search.md; this
       module maintains aria-selected, the roving tabindex, and panel
       visibility. Activation is *manual*: Left/Right/Home/End move focus,
       Enter/Space (native button activation) switches. Automatic
       activation-on-focus is wrong here because selecting Semantic can
       start a ~23 MB model download.
    ------------------------------------------------------------------ */

    var STORAGE_KEY = 'search-tab';
    var tabs        = Array.prototype.slice.call(document.querySelectorAll('.search-tab'));
    var currentTab  = 'keyword';

    /* Focus deliberately stays on the tab button after activation, per the
       APG tabs pattern: moving it into the panel would break the
       Left/Right/Home/End sequence a keyboard user is in the middle of. */
    function activateTab(target) {
        if (target !== 'keyword' && target !== 'semantic') return;
        currentTab = target;

        tabs.forEach(function (b) {
            var active = b.dataset.tab === target;
            b.classList.toggle('is-active', active);
            b.setAttribute('aria-selected', active ? 'true' : 'false');
            b.setAttribute('tabindex', active ? '0' : '-1');
        });
        document.querySelectorAll('.search-panel').forEach(function (p) {
            var active = p.dataset.panel === target;
            p.classList.toggle('is-active', active);
            p.hidden = !active;
            /* PagefindUI rebuilds the inside of #search; re-assert the
               panel semantics so they survive that, and supply them for
               builds whose markup predates this change. */
            if (!p.getAttribute('role')) p.setAttribute('role', 'tabpanel');
            var labelId = 'search-tab-' + p.dataset.panel;
            if (!p.getAttribute('aria-labelledby') && document.getElementById(labelId)) {
                p.setAttribute('aria-labelledby', labelId);
            }
        });
        try { localStorage.setItem(STORAGE_KEY, target); } catch (e) {}

        if (target === 'semantic') onSemanticShown();
    }

    function focusTab(i) {
        if (!tabs.length) return;
        var idx = (i + tabs.length) % tabs.length;
        tabs.forEach(function (b, j) {
            b.setAttribute('tabindex', j === idx ? '0' : '-1');
        });
        tabs[idx].focus();
    }

    tabs.forEach(function (btn, i) {
        btn.addEventListener('click', function () {
            activateTab(btn.dataset.tab);
        });
        btn.addEventListener('keydown', function (e) {
            var handled = true;
            switch (e.key) {
                case 'ArrowLeft':  focusTab(i - 1); break;
                case 'ArrowRight': focusTab(i + 1); break;
                case 'Home':       focusTab(0); break;
                case 'End':        focusTab(tabs.length - 1); break;
                default:           handled = false;
            }
            if (handled) e.preventDefault();
        });
    });

    /* ------------------------------------------------------------------
       Input handling — debounced, 400 ms
    ------------------------------------------------------------------ */

    var debounceTimer = null;

    queryEl.addEventListener('input', function () {
        clearTimeout(debounceTimer);
        /* Invalidate first, unconditionally. The old code returned early
           on an empty box without bumping the token, so an in-flight
           search could still paint results under an emptied input (F02). */
        invalidate();
        var q = queryEl.value.trim();
        if (!q) {
            lastScores = null;
            lastQuery  = '';
            clearResults();
            setStatus('');
            return;
        }
        debounceTimer = setTimeout(function () { runSearch(q); }, 400);
    });

    /* ------------------------------------------------------------------
       Initial state

       ?q= prefills both panels. The semantic search itself only runs when
       the Semantic tab is actually showing: the old unconditional
       runSearch(initial) downloaded ~23 MB of model for visitors who
       arrived on a keyword link (F03). search.js prefills and triggers the
       Pagefind side.
    ------------------------------------------------------------------ */

    var params  = new URLSearchParams(window.location.search);
    var initial = params.get('q');
    var pendingInitial = null;

    if (initial && initial.trim()) {
        queryEl.value  = initial;
        pendingInitial = initial.trim();
    }

    /* Runs whenever the semantic panel becomes visible. */
    function onSemanticShown() {
        if (pendingInitial) {
            var q = pendingInitial;
            pendingInitial = null;
            runSearch(q);
        }
    }

    /* Restore last-used tab (falls back to keyword if unset or unrecognised).
       activateTab also normalises the initial roving tabindex/hidden state. */
    var saved = null;
    try { saved = localStorage.getItem(STORAGE_KEY); } catch (e) {}
    activateTab(saved === 'semantic' ? 'semantic' : 'keyword');

    /* ------------------------------------------------------------------
       Public API
    ------------------------------------------------------------------ */

    window.lnSemanticSearch = {
        setResultFilter: setResultFilter,
        rerank: rerank,
        activateTab: function (name) { activateTab(name); },
        currentTab: function () { return currentTab; },
        isFiltered: function () { return !!resultFilter; }
    };
}());
