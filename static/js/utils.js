/* utils.js — Tiny shared helpers loaded before any other script.
   Loaded synchronously (no `defer`) from templates/partials/head.html so
   that defer'd scripts can rely on `window.lnUtils` existing at run time.

   Keep this file dependency-free and minimal. It's the lowest-level
   layer in the JS stack — anything heavier should live in a feature
   module instead. */
(function (global) {
    'use strict';

    var lnUtils = global.lnUtils || {};

    /* Escape a string for safe interpolation into HTML text content or
       double-quoted attribute values. The order of replacements matters:
       `&` MUST come first, otherwise the `&amp;` injected by other rules
       gets re-escaped to `&amp;amp;`.

       Mirror of `Utils.escapeHtml` in build/Utils.hs. */
    lnUtils.escapeHtml = function (s) {
        return String(s)
            .replace(/&/g,  '&amp;')
            .replace(/</g,  '&lt;')
            .replace(/>/g,  '&gt;')
            .replace(/"/g,  '&quot;')
            .replace(/'/g,  '&#39;');
    };

    /* Safe localStorage wrapper. Safari throws SecurityError in private
       browsing mode on every access — including writes — so reads AND
       writes need to be guarded. Every consumer that touches storage
       should route through this helper so degradation is uniform. */
    lnUtils.safeStorage = {
        get: function (key) {
            try { return localStorage.getItem(key); }
            catch (_) { return null; }
        },
        set: function (key, value) {
            try { localStorage.setItem(key, value); return true; }
            catch (_) { return false; }
        },
        remove: function (key) {
            try { localStorage.removeItem(key); return true; }
            catch (_) { return false; }
        }
    };

    global.lnUtils = lnUtils;
})(window);
