/* settings.js — Settings panel: theme, season, text size, the site's own
   reader settings, print.
   Text sizes are ozyUtils.TEXT_SIZE (utils.js), which theme.js reads too;
   the default among them is the stylesheet's (tokens.css).

   All localStorage access routes through window.ozyUtils.safeStorage so
   Safari private-mode SecurityErrors on writes do not throw uncaught. */
(function () {
    'use strict';

    var TEXT_SIZE_KEY = 'text-size';
    /* Without utils.js the text size controls do nothing. */
    var SIZES = (window.ozyUtils && window.ozyUtils.TEXT_SIZE) || null;
    var store = (window.ozyUtils && window.ozyUtils.safeStorage) || {
        get: function () { return null; },
        set: function () { return false; },
        remove: function () { return false; }
    };

    /* ------------------------------------------------------------------
       Init
    ------------------------------------------------------------------ */

    function init() {
        var toggle = document.querySelector('.settings-toggle');
        var panel  = document.querySelector('.settings-panel');
        if (!toggle || !panel) return;

        showOpenSeasons();
        syncThemeButtons();
        syncTextSizeButtons();
        syncToggleButton('focus-mode',    'data-focus-mode');
        syncToggleButton('reduce-motion', 'data-reduce-motion');
        syncReaderButtons();

        toggle.addEventListener('click', function (e) {
            e.stopPropagation();
            setOpen(toggle.getAttribute('aria-expanded') !== 'true');
        });

        document.addEventListener('click', function (e) {
            if (!panel.contains(e.target) && e.target !== toggle) setOpen(false);
        });

        document.addEventListener('keydown', function (e) {
            if (e.key === 'Escape') { setOpen(false); return; }
            if (e.key !== 'Tab' || !panel.classList.contains('is-open')) return;
            var focusable = Array.from(panel.querySelectorAll(
                'button:not([disabled]):not([hidden]), [tabindex]:not([tabindex="-1"])'
            ));
            if (focusable.length === 0) return;
            var first = focusable[0];
            var last  = focusable[focusable.length - 1];
            if (e.shiftKey && document.activeElement === first) {
                e.preventDefault();
                last.focus();
            } else if (!e.shiftKey && document.activeElement === last) {
                e.preventDefault();
                first.focus();
            }
        });

        panel.querySelectorAll('[data-action]').forEach(function (btn) {
            btn.addEventListener('click', function () {
                handle(btn.getAttribute('data-action'), btn);
            });
        });

        trackReading(toggle.parentNode);
    }

    /* ------------------------------------------------------------------
       Where the reader is, for a gear that is pinned to the window
       The gear's wrap carries `data-away` once the reader has scrolled
       down into the page, until they scroll back up or reach the top;
       and `data-near` while a mouse or pen is within reach of it. The
       engine draws nothing from either: a site that pins the gear may
       fade it by them. Both are written on the wrap, never on the root,
       and only when they change.
    ------------------------------------------------------------------ */

    function trackReading(wrap) {
        var REACH = 140;            /* px around the gear that count as near */
        var RETURN = 40;            /* px scrolled back up that bring it back */
        var lastY = window.scrollY, up = 0, away = false, near = false;
        var armed = false, queued = false, px = 0, py = 0;

        function setAway(v) {
            if (v === away) return;
            away = v;
            wrap.toggleAttribute('data-away', v);
        }

        /* A page that opens part-way down (a link to a section, a
           restored position) has scrolled without the reader: the gear
           is shown until they scroll themselves. */
        window.addEventListener('load', function () {
            setTimeout(function () { lastY = window.scrollY; armed = true; }, 300);
        });

        window.addEventListener('scroll', function () {
            var y = window.scrollY, dy = y - lastY;
            lastY = y;
            if (!armed) return;
            if (y < window.innerHeight * 0.4) { up = 0; setAway(false); }
            else if (dy > 0) { up = 0; setAway(true); }
            else if (dy < 0) { up -= dy; if (up > RETURN) setAway(false); }
        }, { passive: true });

        document.addEventListener('pointermove', function (e) {
            if (e.pointerType === 'touch') return;
            px = e.clientX; py = e.clientY;
            if (queued) return;
            queued = true;
            requestAnimationFrame(function () {
                queued = false;
                var r  = wrap.getBoundingClientRect();
                var dx = Math.max(r.left - px, 0, px - r.right);
                var dy = Math.max(r.top - py, 0, py - r.bottom);
                var n  = dx * dx + dy * dy < REACH * REACH;
                if (n === near) return;
                near = n;
                wrap.toggleAttribute('data-near', n);
            });
        }, { passive: true });
    }

    /* The site's Reduce Motion setting or the system's (nav.js has the
       same test for the footer's button). */
    function reducedMotion() {
        return document.documentElement.hasAttribute('data-reduce-motion') ||
            !!(window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches);
    }

    /* ------------------------------------------------------------------
       Panel open / close
    ------------------------------------------------------------------ */

    function setOpen(open) {
        var toggle  = document.querySelector('.settings-toggle');
        var panel   = document.querySelector('.settings-panel');
        var wasOpen = panel.classList.contains('is-open');
        toggle.setAttribute('aria-expanded', open ? 'true' : 'false');
        panel.setAttribute('aria-hidden',    open ? 'false' : 'true');
        panel.classList.toggle('is-open', open);
        if (open) {
            var first = panel.querySelector('button:not([disabled]):not([hidden]), [tabindex]:not([tabindex="-1"])');
            if (first) first.focus();
        } else if (wasOpen && (panel.contains(document.activeElement) || document.activeElement === toggle)) {
            /* Only return focus to toggle when the panel was open and focus
               was inside the settings area. Clicking outside the panel to
               dismiss it should not steal focus from wherever the user clicked. */
            toggle.focus();
        }
    }

    /* ------------------------------------------------------------------
       Actions
    ------------------------------------------------------------------ */

    function handle(action, btn) {
        if      (action === 'reader') setReader(btn.getAttribute('data-setting'), btn.getAttribute('data-option'));
        else if (action.indexOf('theme-') === 0)  setTheme(action.slice(6));
        else if (action.indexOf('season-') === 0) setSeason(action.slice(7));
        else if (action.indexOf('text-') === 0) {
            if (!SIZES) return;
            if      (action === 'text-smaller') shiftSize(-1);
            else if (action === 'text-larger')  shiftSize(+1);
            else if (action === 'text-reset')   setSize(SIZES.base);
        }
        else if (action === 'focus-mode')    toggleDataAttr('focus-mode',    'data-focus-mode');
        else if (action === 'reduce-motion') toggleDataAttr('reduce-motion', 'data-reduce-motion');
        else if (action === 'top') {
            setOpen(false);
            window.scrollTo({ top: 0, behavior: reducedMotion() ? 'auto' : 'smooth' });
        }
        else if (action === 'print')             { setOpen(false); window.print(); }
        else if (action === 'clear-annotations') { clearAnnotations(); }
    }

    function clearAnnotations() {
        if (!confirm('Remove all highlights and annotations across every page?')) return;
        if (window.Annotations) window.Annotations.clearAll();
        setOpen(false);
    }

    /* Theme ----------------------------------------------------------- */

    function setTheme(theme) {
        document.documentElement.removeAttribute('data-season');
        document.documentElement.setAttribute('data-theme', theme);
        store.set('theme', theme);
        syncThemeButtons();
    }

    /* A season (theme.js) is worn over its base theme and stored by its
       id. Its script waits for stage.js, which may not be on the page
       yet: theme.js loads it only for a season worn from the start. */
    function setSeason(id) {
        var meta = document.querySelector('meta[name="ozy:season"][content="' + id + '"]');
        if (!meta) return;
        var html = document.documentElement;
        html.setAttribute('data-theme', meta.getAttribute('data-base'));
        html.setAttribute('data-season', id);
        store.set('theme', id);
        syncThemeButtons();
        /* From this page's version: an old tab stays on its old set. */
        var stageSrc = (document.querySelector('meta[name="ozy:assets"]') || {content: ''}).content + '/js/stage.js';
        if (meta.hasAttribute('data-script') && !document.querySelector('script[src="' + stageSrc + '"]')) {
            var stage = document.createElement('script');
            stage.src = stageSrc;
            document.head.appendChild(stage);
        }
    }

    /* A season's button shows while its window is open (theme.js lists
       the open ones in data-seasons-open). */
    function showOpenSeasons() {
        var open = (document.documentElement.getAttribute('data-seasons-open') || '').split(' ');
        document.querySelectorAll('[data-action^="season-"]').forEach(function (btn) {
            btn.hidden = open.indexOf(btn.getAttribute('data-action').slice(7)) < 0;
        });
    }

    /* With none chosen, the theme the stylesheets give (ozy:themes'
       data-light, or data-dark while the system prefers dark). */
    function currentTheme() {
        var season = document.documentElement.getAttribute('data-season');
        if (season) return season;
        var attr = document.documentElement.getAttribute('data-theme');
        if (attr) return attr;
        var meta = document.querySelector('meta[name="ozy:themes"]');
        if (!meta) return null;
        var dark = meta.getAttribute('data-dark');
        return dark && window.matchMedia('(prefers-color-scheme: dark)').matches
            ? dark : meta.getAttribute('data-light');
    }

    function syncThemeButtons() {
        var active = currentTheme();
        /* aria-pressed says which: the class only showed it. */
        document.querySelectorAll('[data-action^="theme-"], [data-action^="season-"]').forEach(function (btn) {
            var action = btn.getAttribute('data-action');
            var on = action === 'theme-' + active || action === 'season-' + active;
            btn.classList.toggle('is-active', on);
            btn.setAttribute('aria-pressed', on ? 'true' : 'false');
        });
    }

    /* The site's reader settings -------------------------------------- */

    /* site.yaml's reader-settings (head.html's ozy:setting metas; theme.js
       restores them). The first option is the default: it is worn as no
       attribute and stored as nothing, so a site that changes its default
       changes it for every reader who never chose. */
    function readerOptions(id) {
        var found = [];
        document.querySelectorAll('meta[name="ozy:setting"]').forEach(function (meta) {
            if (meta.getAttribute('content') === id) found = (meta.getAttribute('data-options') || '').split(' ');
        });
        return found;
    }

    function setReader(id, option) {
        var options = readerOptions(id);
        if (options.indexOf(option) < 0) return;
        if (option === options[0]) {
            document.documentElement.removeAttribute('data-reader-' + id);
            store.remove('reader-' + id);
        } else {
            document.documentElement.setAttribute('data-reader-' + id, option);
            store.set('reader-' + id, option);
        }
        syncReaderButtons();
    }

    function syncReaderButtons() {
        document.querySelectorAll('[data-action="reader"]').forEach(function (btn) {
            var id = btn.getAttribute('data-setting');
            var worn = document.documentElement.getAttribute('data-reader-' + id) || readerOptions(id)[0];
            var on = btn.getAttribute('data-option') === worn;
            btn.classList.toggle('is-active', on);
            btn.setAttribute('aria-pressed', on ? 'true' : 'false');
        });
    }

    /* Text size ------------------------------------------------------- */

    /* Seven sizes, a step apart (ozyUtils.TEXT_SIZE). The size in force is
       the stored one, else the default; a size off the steps (the old
       scale's 20 or 26px) steps to the nearest one in that direction. */
    var lastSize = null;    /* the size this page last set, if storage cannot hold it */

    function currentSize() {
        var stored = window.ozyUtils && window.ozyUtils.storedTextSize
            ? window.ozyUtils.storedTextSize() : null;
        if (stored !== null) return stored;
        return lastSize !== null ? lastSize : SIZES.base;
    }

    function nextSize(px, delta) {
        var s = delta > 0 ? SIZES.min : SIZES.max;
        for (; s >= SIZES.min && s <= SIZES.max; s += delta > 0 ? SIZES.step : -SIZES.step) {
            if (delta > 0 ? s > px : s < px) return s;
        }
        return px;
    }

    function shiftSize(delta) {
        setSize(nextSize(currentSize(), delta));
    }

    /* The default is not stored, so the page follows base.css. */
    function setSize(px) {
        var html = document.documentElement;
        lastSize = px;
        if (px === SIZES.base) {
            store.remove(TEXT_SIZE_KEY);
            html.style.removeProperty('--text-size');
        } else {
            store.set(TEXT_SIZE_KEY, String(px));
            html.style.setProperty('--text-size', px + 'px');
        }
        syncTextSizeButtons(true);
    }

    /* Boolean toggles (focus-mode, reduce-motion) -------------------- */

    function toggleDataAttr(storageKey, attrName) {
        var html = document.documentElement;
        var on   = html.hasAttribute(attrName);
        if (on) {
            html.removeAttribute(attrName);
            store.remove(storageKey);
        } else {
            html.setAttribute(attrName, '');
            store.set(storageKey, '1');
        }
        syncToggleButton(storageKey, attrName);
    }

    function syncToggleButton(storageKey, attrName) {
        var btn = document.querySelector('[data-action="' + storageKey + '"]');
        if (!btn) return;
        var on = document.documentElement.hasAttribute(attrName);
        btn.classList.toggle('is-active', on);
        btn.setAttribute('aria-pressed', on ? 'true' : 'false');
    }

    /* At an end of the range a step button says it does nothing, but is
       not `disabled`: a disabled button gives up focus, and a reader
       pressing A+ to the largest size was left on the page behind the
       panel, outside its Tab loop. nextSize stops at the ends, so a press
       there is harmless. Between the buttons, the size as a share of the
       default, which resets it; a change is announced (`announce`), the
       size at load is not. */
    function syncTextSizeButtons(announce) {
        if (!SIZES) return;
        var px      = currentSize();
        var smaller = document.querySelector('[data-action="text-smaller"]');
        var larger  = document.querySelector('[data-action="text-larger"]');
        var value   = document.querySelector('[data-text-size]');
        var status  = document.querySelector('[data-text-size-status]');
        var pct     = Math.round(px / SIZES.base * 100) + '%';
        if (smaller) smaller.setAttribute('aria-disabled', nextSize(px, -1) === px ? 'true' : 'false');
        if (larger)  larger.setAttribute('aria-disabled', nextSize(px, +1) === px ? 'true' : 'false');
        if (value)   value.textContent = pct;
        if (status && announce) status.textContent = 'Text size ' + pct;
    }

    document.addEventListener('DOMContentLoaded', init);
}());
