/* reading.js — How far the reader is through the page, from 0 at the top
   to 1 at the end: --read-progress, kept on scroll and resize on each
   element marked data-read-progress, which draws it (the contents' line,
   toc.css; the phone bar's, nav.css; the reading bar, reading.css). One
   measure for all of them, set on them alone: on <html> it restyled the
   whole document on every scroll event, and the contents' tracking fell
   behind a quick scroll. Loaded with the reading-progress switch on pages
   that have one (default.html); without it, each stays empty. */
(function () {
    'use strict';

    document.addEventListener('DOMContentLoaded', function () {
        var marked = document.querySelectorAll('[data-read-progress]');
        if (!marked.length) return;
        var root = document.documentElement;

        function update() {
            var scrollable = root.scrollHeight - window.innerHeight;
            var progress   = scrollable > 0 ? Math.min(1, window.scrollY / scrollable) : 0;
            var value      = progress.toFixed(4);
            for (var i = 0; i < marked.length; i++) {
                marked[i].style.setProperty('--read-progress', value);
            }
        }

        update();
        window.addEventListener('scroll', update, { passive: true });
        window.addEventListener('resize', update, { passive: true });
    });
}());
