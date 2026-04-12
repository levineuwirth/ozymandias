/* search.js — Pagefind UI initialisation for /search.html.
   Loaded only on pages with search: true in frontmatter.
   Pre-fills the search box from the ?q= query parameter so that
   the selection popup's "Here" button lands ready to go.
   Also instruments search timing and displays elapsed ms. */
(function () {
    'use strict';

    window.addEventListener('DOMContentLoaded', function () {
        var ui = new PagefindUI({
            element: '#search',
            showImages: false,
            excerptLength: 30,
        });

        /* Timing instrumentation ------------------------------------------ */
        var timingEl  = document.getElementById('search-timing');
        var searchEl  = document.getElementById('search');
        var startTime = null;

        if (timingEl && searchEl) {
            /* Input field is created synchronously by PagefindUI above. */
            var input = searchEl.querySelector('.pagefind-ui__search-input');
            if (input) {
                input.addEventListener('input', function () {
                    if (input.value.trim().length > 0) {
                        startTime = performance.now();
                    } else {
                        startTime = null;
                        timingEl.textContent = '';
                    }
                });
            }

            /* Watch for Pagefind rebuilding the results area. */
            new MutationObserver(function () {
                if (startTime !== null) {
                    var elapsed = Math.round(performance.now() - startTime);
                    /* U+2009 thin space between number and unit */
                    timingEl.textContent = elapsed + '\u2009ms';
                    startTime = null;
                }
            }).observe(searchEl, { childList: true, subtree: true });
        }

        /* Pre-fill from URL parameter and trigger the search -------------- */
        var params = new URLSearchParams(window.location.search);
        var q      = params.get('q');
        if (q) {
            startTime = performance.now();
            ui.triggerSearch(q);
        }
    });
}());
