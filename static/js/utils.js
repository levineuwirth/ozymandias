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

    /* The reader's text size (the settings panel; theme.js restores it
       before the first paint): seven sizes, 17 to 29px in steps of 2,
       around base.css's 23px. Stored as px, and only when not the
       default; a stored 0, 1 or 2 is the earlier three-size scale
       (20/23/26px), read as those sizes. Anything else is no choice. */
    lnUtils.TEXT_SIZE = { min: 17, max: 29, step: 2, base: 23, legacy: [20, 23, 26] };

    lnUtils.storedTextSize = function () {
        var T   = lnUtils.TEXT_SIZE;
        var raw = lnUtils.safeStorage.get('text-size');
        if (raw === null || !/^\d{1,2}$/.test(raw)) return null;
        var n = parseInt(raw, 10);
        if (n < T.legacy.length) return T.legacy[n];
        return (n >= T.min && n <= T.max) ? n : null;
    };

    /* The page's identity for anything stored per-URL (audit C03).

       A directory-routed page — every content/<section>/<slug>/index.md —
       is served under two paths: /essays/x/ and /essays/x/index.html.
       The site's own links, canonical tag, sitemap and feed all say the
       first; nginx serves the second to anyone who types it, and a
       from-disk preview only has the second. Scripts that key on
       location.pathname therefore had two identities for one page, and a
       reader who collapsed a section or highlighted a sentence under one
       spelling found neither under the other.

       `current` is the canonical spelling and the one to write. `legacy`
       is the index.html spelling — null for a flat .html page, where
       there is only ever one — and consumers should read it as a
       fallback so state saved before this fix is still found.

       Deliberately not migrated on read: rewriting storage on page load
       is a side effect for a problem that costs one extra lookup. */
    lnUtils.pagePaths = function (pathname) {
        var path  = typeof pathname === 'string' ? pathname : location.pathname;
        var INDEX = 'index.html';
        /* The separator is part of the test: /essays/reindex.html is a
           flat page whose name happens to end in those ten characters. */
        var isDirIndex = path.slice(-(INDEX.length + 1)) === '/' + INDEX;
        var current = isDirIndex ? path.slice(0, -INDEX.length) : path;
        return {
            current: current,
            legacy:  current.charAt(current.length - 1) === '/'
                ? current + INDEX
                : null
        };
    };

    /* Calendar days as integers (days since 1970-01-01), for "how long
       ago" phrases.

       isoDay("2026-10-01") is that date's number, or null when the text
       is not a real YYYY-MM-DD date. localDay(d) is the number of the
       date a wall calendar shows at instant `d` (default: now) in the
       reader's time zone. Their difference counts whole calendar days,
       whatever the hour, and DST, which makes some local days 23 or 25
       hours long, cannot add or drop one. now.js and popups.js's date
       popups both count this way (audit J09): the popups used to measure
       from the date's UTC midnight to the present instant, so a date
       read as "yesterday" in a Los Angeles evening and as the future,
       with no popup, on a Tokyo morning. */
    var DAY_MS = 86400000;

    lnUtils.isoDay = function (s) {
        var m = /^(\d{4})-(\d{2})-(\d{2})/.exec(typeof s === 'string' ? s : '');
        if (!m) return null;
        var t = Date.UTC(+m[1], +m[2] - 1, +m[3]);
        var d = new Date(t);
        /* Date.UTC rolls 2026-02-31 into March, and years below 100
           into the 1900s: refuse what does not come back unchanged. */
        if (d.getUTCFullYear() !== +m[1] || d.getUTCMonth() !== +m[2] - 1
            || d.getUTCDate() !== +m[3]) return null;
        return t / DAY_MS;
    };

    lnUtils.localDay = function (date) {
        var d = date || new Date();
        return Date.UTC(d.getFullYear(), d.getMonth(), d.getDate()) / DAY_MS;
    };

    global.lnUtils = lnUtils;
})(window);
