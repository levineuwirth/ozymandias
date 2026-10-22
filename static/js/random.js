/* random.js — "Random Page" for the homepage.
   Fetches /random-pages.json (essays + blog posts, generated at build time)
   and navigates to a uniformly random entry on click.
   Attaches to: #random-page-btn (legacy button) or [data-random] (link row).

   A [data-random] link carries a real href, the page it falls back to:
   where it goes without JavaScript, when the list cannot be had (a failed
   fetch, an error status, no list, or none of its entries a site path),
   and on a click that asks for a new tab or window, which is left to the
   browser (audit J13). It used to be href="#", and every failure did
   nothing. */
(function () {
    'use strict';

    function goRandom(e) {
        if (e.button !== 0 || e.metaKey || e.ctrlKey || e.shiftKey || e.altKey) return;
        var el = e.currentTarget;
        e.preventDefault();
        fetch('/random-pages.json')
            .then(function (r) {
                if (!r.ok) throw new Error('random-pages.json: ' + r.status);
                return r.json();
            })
            .then(function (pages) {
                /* Site paths only: a string with one leading slash. Not
                   null, a number, another origin (//host, https:) or a
                   javascript: URL, whatever the list holds. */
                var paths = Array.isArray(pages) ? pages.filter(function (p) {
                    return typeof p === 'string' && /^\/(?!\/)/.test(p);
                }) : [];
                if (!paths.length) throw new Error('no pages');
                window.location.href = paths[Math.floor(Math.random() * paths.length)];
            })
            .catch(function () {
                var href = el.getAttribute('href');
                if (href && href !== '#') window.location.href = el.href;
            });
    }

    document.addEventListener('DOMContentLoaded', function () {
        var els = document.querySelectorAll('#random-page-btn, [data-random]');
        els.forEach(function (el) {
            el.addEventListener('click', goRandom);
        });
    });
}());
