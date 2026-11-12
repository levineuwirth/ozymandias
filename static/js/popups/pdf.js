/* popups/pdf.js — a local PDF's first page.
   With the pdf-viewer switch: served after popups.js, in the same file
   (EngineFiles.jsModules), and bound through window.ozyPopups. */
(function () {
    'use strict';

    var P = window.ozyPopups;
    if (!P) return;   /* popups.js did not run (a touch-only device returns before binding) */

    function esc(s) {
        return window.ozyUtils.escapeHtml(s);
    }

    var cache = Object.create(null);   /* url → html; only successful results stored */


    function store(href, html) {
        cache[href] = html;
        return html;
    }

    /* Local PDF — shows the build-time first-page thumbnail (.thumb.png).
       Returns null (no popup) if the thumbnail file does not exist. */
    function pdfContent(target) {
        var src   = target.dataset.pdfSrc;
        if (!src) return Promise.resolve(null);
        var thumb = src.replace(/\.pdf$/i, '.thumb.png');
        if (cache[thumb]) return Promise.resolve(cache[thumb]);
        /* HEAD request: verify thumbnail exists before committing to a popup. */
        return fetch(thumb, { method: 'HEAD', credentials: 'same-origin' })
            .then(function (r) {
                if (!r.ok) return null;
                return store(thumb,
                    '<div class="popup-pdf">'
                    + '<img class="popup-pdf-thumb" src="' + esc(thumb) + '" alt="PDF first page">'
                    + '</div>');
            })
            .catch(function () { return null; });
    }

    /* PDF links — rewritten to viewer URL by Links.hs; thumbnail on hover */
    P.bind(60, 'a.pdf-link[data-pdf-src]', pdfContent);
}());
