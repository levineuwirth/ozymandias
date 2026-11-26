/* version-history.js — the footer's Version history, fitted to its line
   (templates/partials/page-footer.html, css/aftermatter.css).

   Where the history stands on a line beside the record or the map, it lists
   the entries that fit in the height of the tallest of them, and folds the
   rest under a "More" button. Alone on its line (a phone's stack, or a
   window too narrow to hold it beside the others), or without this script,
   it lists every entry. It never folds the newest; and once opened, the
   button stays only while there is something it could fold.

   The line is measured with every entry shown, so the result depends on the
   width alone; and a neighbor's height is that of what it holds, not of its
   box, which the row stretches to the tallest section, this one included. */
(function () {
    'use strict';

    function init() {
        var section = document.getElementById('version-history');
        var list = section && section.querySelector('.version-history-list');
        if (!list || list.children.length < 2) return;
        var items = Array.prototype.slice.call(list.children);
        var row = section.parentElement;
        var open = false;

        if (!list.id) list.id = 'version-history-list';
        var button = document.createElement('button');
        button.type = 'button';
        button.className = 'version-history-more';
        button.setAttribute('aria-controls', list.id);
        button.hidden = true;
        list.parentNode.insertBefore(button, list.nextSibling);

        /* Heights below the section's top, each read with that top: showing
           or folding entries can move the page under the reader (the
           browser's scroll anchoring) between one reading and the next. */
        function below(el) {
            return el.getBoundingClientRect().bottom - section.getBoundingClientRect().top;
        }

        function held(el) {
            var bottom = 0;
            for (var c = el.firstElementChild; c; c = c.nextElementSibling) {
                if (c.tagName === 'SCRIPT' || c.hidden) continue;
                bottom = Math.max(bottom, below(c));
            }
            return bottom;
        }

        function label(expanded) {
            button.textContent = expanded ? 'Fewer' : 'More';
            button.setAttribute('aria-expanded', expanded ? 'true' : 'false');
        }

        function fit() {
            items.forEach(function (li) { li.hidden = false; });
            button.hidden = false;
            label(open);

            var limit = null;
            for (var s = row.firstElementChild; s; s = s.nextElementSibling) {
                if (s === section || s.tagName === 'SCRIPT') continue;
                if (Math.abs(s.getBoundingClientRect().top - section.getBoundingClientRect().top) > 2) continue;
                var b = held(s);
                limit = limit === null ? b : Math.max(limit, b);
            }
            var last = items[items.length - 1];
            if (limit === null || below(last) <= limit + 1) {   /* alone on its line, or all fit */
                button.hidden = true;
                return;
            }
            if (open) return;                                  /* "Fewer" folds them again */
            var room = below(button) - below(last);
            var keep = 1;
            while (keep < items.length && below(items[keep]) + room <= limit + 1) keep++;
            items.slice(keep).forEach(function (li) { li.hidden = true; });
        }

        button.addEventListener('click', function () {
            open = !open;
            fit();
        });

        var pending = false;
        function soon() {
            if (pending) return;
            pending = true;
            requestAnimationFrame(function () { pending = false; fit(); });
        }
        fit();
        if (window.ResizeObserver) new ResizeObserver(soon).observe(row);
        else window.addEventListener('resize', soon);
        if (document.fonts && document.fonts.ready) document.fonts.ready.then(soon);
    }

    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init);
    else init();
}());
