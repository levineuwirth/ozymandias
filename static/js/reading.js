/* reading.js — Scroll-progress bar for reading (fiction, poetry) mode. */
(function () {
    'use strict';

    function update() {
        var bar = document.getElementById('reading-progress');
        if (!bar) return;
        var scrollTop  = window.scrollY || document.documentElement.scrollTop;
        var docHeight  = document.documentElement.scrollHeight - window.innerHeight;
        var pct        = docHeight > 0 ? (scrollTop / docHeight) * 100 : 0;
        bar.style.width = Math.min(pct, 100).toFixed(2) + '%';
    }

    document.addEventListener('DOMContentLoaded', function () {
        update();
        window.addEventListener('scroll', update, { passive: true });
        window.addEventListener('resize', update, { passive: true });
    });
}());
