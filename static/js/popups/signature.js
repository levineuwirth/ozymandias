/* popups/signature.js — the footer's PGP signature.
   With the signature-link switch: served after popups.js, in the same file
   (EngineFiles.jsModules), and bound through window.ozyPopups. */
(function () {
    'use strict';

    var P = window.ozyPopups;
    if (!P) return;   /* popups.js did not run (a touch-only device returns before binding) */

    function esc(s) {
        return window.ozyUtils.escapeHtml(s);
    }

    var cache = Object.create(null);   /* url → html; only successful results stored */

    /* PGP signature — fetch the .sig file and display ASCII armor */
    function sigContent(target) {
        var href = target.getAttribute('href');
        if (!href) return Promise.resolve(null);
        if (cache[href]) return Promise.resolve(cache[href]);
        return fetch(href, { credentials: 'same-origin' })
            .then(function (r) { return r.ok ? r.text() : null; })
            .then(function (text) {
                if (!text) return null;
                var html = '<div class="popup-sig"><pre>' + esc(text.trim()) + '</pre></div>';
                cache[href] = html;
                return html;
            });
    }

    /* PGP signature links in footer */
    P.bind(70, 'a.footer-sig-link', sigContent);
}());
