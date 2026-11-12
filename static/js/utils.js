/* utils.js — Tiny shared helpers loaded before any other script.
   Loaded synchronously (no `defer`) from templates/partials/head.html so
   that defer'd scripts can rely on `window.ozyUtils` existing at run time.

   Keep this file dependency-free and minimal. It's the lowest-level
   layer in the JS stack — anything heavier should live in a feature
   module instead. What every page needs to share lives here: what
   site.yaml tells the scripts (ozySite), and the one entry point for
   content added after load (ozyEnhance and `ozy:content-added`), which a
   site's own scripts may use whatever the site's switches. */
(function (global) {
    'use strict';

    var ozyUtils = global.ozyUtils || {};

    /* What site.yaml tells the scripts, by the page's meta tags: no fetch and
       no CSP change. Read once, here, so the tags come before this script in
       <head> (templates/partials/head.html). */
    function siteMeta(name) {
        return global.document.querySelector('meta[name="ozy:' + name + '"]');
    }
    function siteValue(name, attribute) {
        var meta = siteMeta(name);
        return (meta && meta.getAttribute(attribute || 'content')) || '';
    }
    global.ozySite = {
        /* The switches the site has on (Config.switchContext), for a
           script that loads a switch's files on demand. */
        switches:            siteValue('switches').split(' ').filter(Boolean),
        author:              siteValue('author'),
        authorSort:          siteValue('author-sort'),
        host:                siteValue('host'),
        epistemicAbout:      siteValue('epistemic-about'),
        epistemicAboutLabel: siteValue('epistemic-about', 'data-label'),
        popupProviders:      siteValue('popup-providers').split(' ').filter(Boolean)
    };

    /* Escape a string for safe interpolation into HTML text content or
       double-quoted attribute values. The order of replacements matters:
       `&` MUST come first, otherwise the `&amp;` injected by other rules
       gets re-escaped to `&amp;amp;`.

       Mirror of `Utils.escapeHtml` in build/Utils.hs. */
    ozyUtils.escapeHtml = function (s) {
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
    ozyUtils.safeStorage = {
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
       around the default. Stored as px, and only when not the default;
       a stored 0, 1 or 2 is the earlier three-size scale (20/23/26px),
       read as those sizes. Anything else is no choice. */
    ozyUtils.TEXT_SIZE = {
        min: 17, max: 29, step: 2, legacy: [20, 23, 26],
        /* The default is the stylesheet's, tokens.css's --text-size-default
           (a site's theme.css may change it), read when asked: settings.js
           asks after the stylesheets have loaded. Without one, the middle
           of the range. */
        get base() {
            var px = parseFloat(getComputedStyle(document.documentElement)
                .getPropertyValue('--text-size-default'));
            return px > 0 ? px : (this.min + this.max) / 2;
        }
    };

    ozyUtils.storedTextSize = function () {
        var T   = ozyUtils.TEXT_SIZE;
        var raw = ozyUtils.safeStorage.get('text-size');
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
    ozyUtils.pagePaths = function (pathname) {
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

    ozyUtils.isoDay = function (s) {
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

    ozyUtils.localDay = function (date) {
        var d = date || new Date();
        return Date.UTC(d.getFullYear(), d.getMonth(), d.getDate()) / DAY_MS;
    };

    /* ------------------------------------------------------------------
       Content added after load

       `window.ozyEnhance(container[, {force: true, source: url}])` is the
       one idempotent entry point for content inserted into the page after
       load (transclude.js's passages; a site script's own). It calls the
       known reinitialisers (sidenotes, popups, collapse) and then
       dispatches a bubbling `ozy:content-added` CustomEvent on the
       container, with `detail = {container, source}`. Any subsystem that
       needs to see injected content — KaTeX rendering, code-copy buttons,
       annotations, lightbox, a site's script — should listen for that
       event rather than growing its own ad hoc hook:

           document.addEventListener('ozy:content-added', function (e) {
               myInit(e.detail.container);
           });

       Calling ozyEnhance twice on the same container is a no-op unless
       `force` is passed, so listeners may assume one call per insertion.
       Code and math that arrive with the content load their highlighter
       and typesetter, with the code and math switches on (their files
       are published only then).

       gallery.js does not listen yet: a transcluded display equation or
       score fragment gets no expand button. No page transcludes either
       today.
    ------------------------------------------------------------------ */

    var CONTENT_EVENT = 'ozy:content-added';

    function switchOn(name) {
        return global.ozySite.switches.indexOf(name) !== -1;
    }

    function enhance(container, opts) {
        if (!container || container.nodeType !== 1) return;
        opts = opts || {};
        if (container.dataset.ozyEnhanced === '1' && !opts.force) return;
        container.dataset.ozyEnhanced = '1';

        /* sidenotes.js — pair newly injected note references with their
           notes and reposition the margin. A page that transcludes always loads it
           (has-sidenotes counts transclusion markers). */
        if (typeof global.reinitSidenotes === 'function') {
            global.reinitSidenotes(container);
        }

        /* popups.js — bind hover popups for newly injected links so
           transcluded content has the same preview behaviour as the
           host page. */
        if (typeof global.reinitPopups === 'function') {
            global.reinitPopups(container);
        }

        /* collapse.js exposes reinitCollapse for newly added headings. */
        if (typeof global.reinitCollapse === 'function') {
            global.reinitCollapse(container);
        }

        /* Everything else (math, code-copy, annotations, lightbox) joins
           in by listening for this event. */
        container.dispatchEvent(new CustomEvent(CONTENT_EVENT, {
            bubbles: true,
            detail: { container: container, source: opts.source || null }
        }));

        ensureMath(container);
        highlightCode(container);
    }

    /* This page's version of the first-party assets: what this script loads on
       demand comes from there, so an old tab stays on its old set. */
    var ASSETS = siteValue('assets');

    /* `imports`: what the stylesheet @imports (a vendor sheet, into the
       engine's layer), preloaded beside it as head.html does for a page's
       own, or its request would wait for the stylesheet to arrive. */
    function ensureStyle(href, imports) {
        var doc = global.document;
        var present = Array.from(doc.querySelectorAll('link[rel="stylesheet"]'))
            .some(function (el) {
                return new URL(el.href, doc.baseURI).pathname === href;
            });
        if (present) return;
        (imports || []).forEach(function (url) {
            var early = doc.createElement('link');
            early.rel = 'preload';
            early.as = 'style';
            early.href = url;
            doc.head.appendChild(early);
        });
        var css = doc.createElement('link');
        css.rel = 'stylesheet';
        css.href = href;
        doc.head.appendChild(css);
    }

    function loadScript(src) {
        return new Promise(function (resolve, reject) {
            var script = global.document.createElement('script');
            script.src = src;
            script.onload = resolve;
            script.onerror = function () {
                script.remove();
                reject(new Error('Could not load ' + src));
            };
            global.document.head.appendChild(script);
        });
    }

    /* Code may arrive in a source popup or a transclusion on a page that
       did not need Prism at first. Share one download, then enhance each
       fragment (including a detached popup before it is shown). Without
       the code switch there is no Prism to load: the code stays plain. */
    var codeLoading = null;
    function highlightCode(container) {
        if (!switchOn('code')) return Promise.resolve();
        var selector = 'code[class*="language-"], code[class*="lang-"]';
        var codes = Array.from(container.querySelectorAll(selector));
        if (container.matches && container.matches(selector)) codes.unshift(container);
        if (!codes.length) return Promise.resolve();
        ensureStyle(ASSETS + '/css/modules/syntax.css');
        if (!codeLoading) codeLoading = global.Prism
            ? Promise.resolve() : loadScript(ASSETS + '/js/prism.min.js');
        return codeLoading.then(function () {
            codes.forEach(function (code) {
                var lang = /(?:^|\s)lang(?:uage)?-([\w-]+)/i.exec(code.className);
                if (lang && global.Prism && global.Prism.languages[lang[1]]) {
                    global.Prism.highlightElement(code);
                }
            });
        }).catch(function () {
            codeLoading = null; // plain code remains readable; the next use retries
        });
    }

    /* KaTeX loads only on pages whose own body has math (audit X6). A
       transclusion can bring math to a page that has none: load it then,
       once. katex-bootstrap.js renders the whole document when it runs,
       skipping anything already typeset. Without the math switch there is
       no KaTeX to load. */
    var mathLoading = null;
    function ensureMath(container) {
        if (!switchOn('math')) return;
        if (mathLoading || typeof global.renderMath === 'function') return;
        if (!container.querySelector('.math') && !container.classList.contains('math')) return;
        ensureStyle(ASSETS + '/css/modules/katex.css', ['/katex/katex.min.css']);
        mathLoading = (global.katex ? Promise.resolve() : loadScript('/katex/katex.min.js'))
            .then(function () { return loadScript(ASSETS + '/js/katex-bootstrap.js'); })
            .catch(function () { mathLoading = null; });
    }

    global.ozyEnhance = enhance;
    global.ozyHighlightCode = highlightCode;
    global.ozyUtils = ozyUtils;
})(window);
