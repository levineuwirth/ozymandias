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
*/
(function () {
    'use strict';

    function renderAll() {
        if (typeof katex === 'undefined') return;
        var nodes = Array.from(document.getElementsByClassName('math'));
        nodes.forEach(function (el) {
            if (el.tagName !== 'SPAN' && el.tagName !== 'DIV') return;
            var src = el.textContent;
            try {
                katex.render(src, el, {
                    displayMode:  el.classList.contains('display'),
                    output:       'htmlAndMathml',
                    throwOnError: false
                });
            } catch (_) {
                /* leave the original source visible if KaTeX rejects it */
            }
        });
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', renderAll);
    } else {
        renderAll();
    }
})();
