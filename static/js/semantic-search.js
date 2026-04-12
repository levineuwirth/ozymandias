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
 */
(function () {
    'use strict';

    var MODEL      = 'all-MiniLM-L6-v2';          /* local name, no Xenova/ prefix */
    var MODEL_PATH = '/models/';                   /* served same-origin */
    var DIM        = 384;
    var TOP_K      = 8;
    var CDN        = 'https://cdn.jsdelivr.net/npm/@xenova/transformers@2';

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

    function loadIndex() {
        if (indexReady) return Promise.resolve();

        return Promise.all([
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
            indexReady = true;
        });
    }

    /* ------------------------------------------------------------------
       Model loading — dynamic import from CDN, lazy
    ------------------------------------------------------------------ */

    /* In-flight promise so concurrent searches share a single model
       load. Without this guard, two rapid keystrokes would each call
       `import(CDN)` and `pipeline(...)`, wasting CPU and memory before
       the second resolves. */
    var loadModelPromise = null;

    function loadModel() {
        if (extractor) return Promise.resolve(extractor);
        if (loadModelPromise) return loadModelPromise;
        setStatus('Loading model…');
        loadModelPromise = import(CDN).then(function (mod) {
            /* Point transformers.js at our self-hosted model files. */
            mod.env.localModelPath   = MODEL_PATH;
            mod.env.allowRemoteModels = false;
            return mod.pipeline('feature-extraction', MODEL, { quantized: true });
        }).then(function (pipe) {
            extractor = pipe;
            return extractor;
        }).catch(function (err) {
            /* Allow a retry on the next call instead of caching the
               failed promise forever. */
            loadModelPromise = null;
            throw err;
        });
        return loadModelPromise;
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

    function topK(scores) {
        var indices = Array.from({ length: meta.length }, function (_, i) { return i; });
        indices.sort(function (a, b) { return scores[b] - scores[a]; });
        return indices.slice(0, TOP_K).map(function (i) {
            return { idx: i, score: scores[i] };
        });
    }

    function runSearch(query) {
        query = query.trim();
        if (!query) { clearResults(); return; }

        setStatus('Searching…');

        var indexPromise = loadIndex().catch(function (err) {
            setStatus('Semantic index not available — run make build first.');
            throw err;
        });
        var modelPromise = loadModel();

        Promise.all([indexPromise, modelPromise]).then(function (results) {
            var pipe = results[1];
            return pipe(query, { pooling: 'mean', normalize: true });
        }).then(function (output) {
            var queryVec = output.data;   /* Float32Array, length 384 */
            var scores   = cosineSims(queryVec);
            var hits     = topK(scores);
            renderResults(hits);
            setStatus(hits.length ? '' : 'No results found.');
        }).catch(function (err) {
            if (err.message && err.message.indexOf('not available') === -1) {
                setStatus('Search error — see console for details.');
                console.error('semantic-search:', err);
            }
        });
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

    function setStatus(msg) {
        statusEl.textContent = msg;
    }

    /* Defer to the shared utility (loaded synchronously from
       templates/partials/head.html) so this file cannot drift from
       popups.js, annotations.js, or build/Utils.hs. */
    function esc(s) {
        return window.lnUtils.escapeHtml(s);
    }

    /* ------------------------------------------------------------------
       Tab switching — persists choice in localStorage
    ------------------------------------------------------------------ */

    var STORAGE_KEY = 'search-tab';

    function activateTab(target) {
        document.querySelectorAll('.search-tab').forEach(function (b) {
            var active = b.dataset.tab === target;
            b.classList.toggle('is-active', active);
            b.setAttribute('aria-selected', active ? 'true' : 'false');
        });
        document.querySelectorAll('.search-panel').forEach(function (p) {
            p.classList.toggle('is-active', p.dataset.panel === target);
        });
        try { localStorage.setItem(STORAGE_KEY, target); } catch (e) {}
    }

    document.querySelectorAll('.search-tab').forEach(function (btn) {
        btn.addEventListener('click', function () { activateTab(btn.dataset.tab); });
    });

    /* Restore last-used tab (falls back to keyword if unset or unrecognised) */
    var saved = null;
    try { saved = localStorage.getItem(STORAGE_KEY); } catch (e) {}
    if (saved === 'semantic') activateTab('semantic');

    /* ------------------------------------------------------------------
       Input handling — debounced, 400 ms
    ------------------------------------------------------------------ */

    var debounceTimer = null;
    queryEl.addEventListener('input', function () {
        clearTimeout(debounceTimer);
        var q = queryEl.value.trim();
        if (!q) { clearResults(); setStatus(''); return; }
        debounceTimer = setTimeout(function () { runSearch(q); }, 400);
    });

    /* Pre-fill from ?q= on load — mirror keyword search behaviour */
    var params = new URLSearchParams(window.location.search);
    var initial = params.get('q');
    if (initial) {
        queryEl.value = initial;
        runSearch(initial);
    }
}());
