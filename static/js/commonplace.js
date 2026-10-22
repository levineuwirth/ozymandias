/* commonplace.js — the /commonplace view toggle.
 *
 * Two renderings of the same entries are emitted into the page by
 * build/Commonplace.hs: #cp-themed (grouped by theme) and #cp-chrono
 * (newest first). Only one is shown; this swaps between them and
 * remembers the choice.
 *
 * Externalised from an inline <script> in templates/commonplace.html so
 * the site's Content-Security-Policy can drop 'unsafe-inline' for
 * scripts. Behaviour is unchanged, including the localStorage key
 * ('cp-view'), so a returning reader keeps the view they chose.
 *
 * Loaded from templates/default.html behind $if(commonplace)$ — the same
 * flag that gates /css/commonplace.css — and therefore deferred, which
 * is why the DOMContentLoaded listener the inline version needed is
 * gone: a deferred script already runs after parsing.
 */
(function () {
    'use strict';

    var themed = document.getElementById('cp-themed');
    var chrono = document.getElementById('cp-chrono');
    var btns   = document.querySelectorAll('.cp-toggle-btn');

    if (!themed || !chrono) return;

    function show(id) {
        themed.hidden = (id !== 'cp-themed');
        chrono.hidden = (id !== 'cp-chrono');
        btns.forEach(function (b) {
            b.classList.toggle('is-active', b.dataset.target === id);
        });
        try {
            localStorage.setItem('cp-view', id);
        } catch (e) {
            /* Private browsing or a full quota: the toggle still works,
               it just will not be remembered. */
        }
    }

    btns.forEach(function (btn) {
        btn.addEventListener('click', function () { show(btn.dataset.target); });
    });

    var saved = null;
    try {
        saved = localStorage.getItem('cp-view');
    } catch (e) {
        saved = null;
    }
    if (saved === 'cp-chrono') show('cp-chrono');
}());
