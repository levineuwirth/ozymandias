/* math.js — on pages with math: a display formula too wide for its column
   can be scrolled from the keyboard.

   The math itself needs no script. The generator writes it as MathML
   (build/Filters/Math.hs), which the browser draws in the site's math face,
   each formula in a <span class="math inline"> or <span class="math
   display">; a display formula's wrapper scrolls what is too wide
   (typography.css). A scroller the keyboard cannot reach can only be
   scrolled with a pointer (WCAG 2.1.1), so the ones that overflow, and only
   those, take a tab stop: one for every equation was the flood audit J04
   removed. Rechecked as their width changes, when the math face arrives,
   and for math a transclusion brings (`ozy:content-added`; utils.js loads
   this file then on a page that had none). */
(function () {
    'use strict';

    if (window.ozyMath) return;
    window.ozyMath = true;

    function markScroller(el) {
        if (el.scrollWidth > el.clientWidth + 1) {
            el.setAttribute('tabindex', '0');
            /* A group, not a region: a page has several, and landmarks
               must each have a name of their own. */
            el.setAttribute('role', 'group');
            el.setAttribute('aria-label', 'Equation, scrolls sideways');
        } else if (el.getAttribute('role') === 'group') {
            el.removeAttribute('tabindex');
            el.removeAttribute('role');
            el.removeAttribute('aria-label');
        }
    }

    var scrollers = window.ResizeObserver ? new ResizeObserver(function (entries) {
        entries.forEach(function (e) { markScroller(e.target); });
    }) : null;

    function displays(scope) {
        var root = scope && scope.querySelectorAll ? scope : document;
        var found = Array.prototype.slice.call(root.querySelectorAll('.math.display'));
        if (root.nodeType === 1 && root.matches('.math.display')) found.unshift(root);
        return found;
    }

    function watch(scope) {
        displays(scope).forEach(function (el) {
            markScroller(el);
            if (scrollers) scrollers.observe(el);
        });
    }

    /* A formula widens without its box changing size when the math face
       arrives, which the observer does not see: look again then. */
    function recheck() { displays(document).forEach(markScroller); }
    if (document.fonts && document.fonts.ready) document.fonts.ready.then(recheck);
    window.addEventListener('load', recheck);

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', function () { watch(document); });
    } else {
        watch(document);
    }

    document.addEventListener('ozy:content-added', function (e) {
        watch(e.detail && e.detail.container);
    });
})();
