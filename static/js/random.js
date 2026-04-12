/* random.js — "Random Page" for the homepage.
   Fetches /random-pages.json (essays + blog posts, generated at build time)
   and navigates to a uniformly random entry on click.
   Attaches to: #random-page-btn (legacy button) or [data-random] (link row). */
(function () {
    'use strict';

    function goRandom(e) {
        e.preventDefault();
        fetch('/random-pages.json')
            .then(function (r) { return r.json(); })
            .then(function (pages) {
                if (!pages.length) return;
                window.location.href = pages[Math.floor(Math.random() * pages.length)];
            })
            .catch(function () {});
    }

    document.addEventListener('DOMContentLoaded', function () {
        var els = document.querySelectorAll('#random-page-btn, [data-random]');
        els.forEach(function (el) {
            el.addEventListener('click', goRandom);
        });
    });
}());
