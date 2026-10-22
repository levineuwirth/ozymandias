/* katex-bootstrap.js — Render every <span class="math"> / <div class="math">
   block once KaTeX has finished loading.

   Pandoc emits math blocks with the `math` class and the LaTeX source as
   the element's text content. KaTeX is loaded with `defer` so this
   bootstrap can simply run on DOMContentLoaded — KaTeX guarantees its
   own definitions are available by then.

   Used to live as an inline `onload="..."` attribute on the KaTeX
   <script> tag in templates/default.html, which blocked any future
   strict CSP. Externalized here so the entire site can run with
   `script-src 'self'` plus a single CDN allowance.

   Dynamic content (smaller finding 7): rendering used to happen once, on
   DOMContentLoaded, so math inside a transcluded passage stayed as raw
   LaTeX. It now takes a root and listens for `ln:content-added`. Each
   element is marked once rendered, because re-rendering a finished block
   would typeset KaTeX's own output as if it were source.
*/
(function () {
    'use strict';

    function renderIn(root) {
        if (typeof katex === 'undefined') return;
        var scope = root && root.querySelectorAll ? root : document;

        var nodes = Array.from(scope.querySelectorAll('.math'));
        /* querySelectorAll only looks *below* the root; a transcluded
           fragment can itself be the math element. */
        if (scope.nodeType === 1 && scope.classList
            && scope.classList.contains('math')) {
            nodes.unshift(scope);
        }

        nodes.forEach(function (el) {
            if (el.tagName !== 'SPAN' && el.tagName !== 'DIV') return;
            if (el.dataset.katexRendered === '1') return;
            var src = el.textContent;
            try {
                katex.render(src, el, {
                    displayMode:  el.classList.contains('display'),
                    output:       'htmlAndMathml',
                    throwOnError: false
                });
                el.dataset.katexRendered = '1';
            } catch (_) {
                /* leave the original source visible if KaTeX rejects it */
            }
        });
    }

    window.renderMath = renderIn;

    /* A display equation wider than its column scrolls sideways
       (typography.css: overflow-x: auto), and a scroller the keyboard
       cannot reach can only be scrolled with a pointer (WCAG 2.1.1). The
       ones that overflow, and only those, take a tab stop: one for every
       equation was the flood audit J04 removed. Rechecked as their width
       changes. */
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

    function watchScrollers(scope) {
        if (!scrollers) return;
        (scope && scope.querySelectorAll ? scope : document)
            .querySelectorAll('.katex-display').forEach(function (el) { scrollers.observe(el); });
    }

    /* A formula widens without its box changing size when KaTeX's fonts
       arrive, which the observer does not see: look again then. */
    function recheckScrollers() {
        document.querySelectorAll('.katex-display').forEach(markScroller);
    }
    if (document.fonts && document.fonts.ready) document.fonts.ready.then(recheckScrollers);
    window.addEventListener('load', recheckScrollers);

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', function () {
            renderIn(document);
            watchScrollers(document);
        });
    } else {
        renderIn(document);
        watchScrollers(document);
    }

    document.addEventListener('ln:content-added', function (e) {
        renderIn(e.detail && e.detail.container);
        watchScrollers(e.detail && e.detail.container);
    });
})();
