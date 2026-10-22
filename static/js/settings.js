/* settings.js — Settings panel: theme, text size, print.
   Text sizes are lnUtils.TEXT_SIZE (utils.js), which theme.js reads too.

   All localStorage access routes through window.lnUtils.safeStorage so
   Safari private-mode SecurityErrors on writes do not throw uncaught. */
(function () {
    'use strict';

    var TEXT_SIZE_KEY = 'text-size';
    var SIZES = (window.lnUtils && window.lnUtils.TEXT_SIZE)
        || { min: 17, max: 29, step: 2, base: 23 };
    var store = (window.lnUtils && window.lnUtils.safeStorage) || {
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

        syncThemeButtons();
        syncTextSizeButtons();
        syncToggleButton('focus-mode',    'data-focus-mode');
        syncToggleButton('reduce-motion', 'data-reduce-motion');

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
                'button:not([disabled]), [tabindex]:not([tabindex="-1"])'
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
                handle(btn.getAttribute('data-action'));
            });
        });
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
            var first = panel.querySelector('button:not([disabled]), [tabindex]:not([tabindex="-1"])');
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

    function handle(action) {
        if      (action === 'theme-light')      setTheme('light');
        else if (action === 'theme-dark')       setTheme('dark');
        else if (action === 'theme-cappuccino') setTheme('cappuccino');
        else if (action === 'text-smaller')  shiftSize(-1);
        else if (action === 'text-larger')   shiftSize(+1);
        else if (action === 'text-reset')    setSize(SIZES.base);
        else if (action === 'focus-mode')    toggleDataAttr('focus-mode',    'data-focus-mode');
        else if (action === 'reduce-motion') toggleDataAttr('reduce-motion', 'data-reduce-motion');
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
        document.documentElement.setAttribute('data-theme', theme);
        store.set('theme', theme);
        syncThemeButtons();
    }

    function currentTheme() {
        var attr = document.documentElement.getAttribute('data-theme');
        if (attr) return attr;
        return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
    }

    function syncThemeButtons() {
        var active = currentTheme();
        /* aria-pressed says which: the class only showed it. */
        document.querySelectorAll('[data-action^="theme-"]').forEach(function (btn) {
            var on = btn.getAttribute('data-action') === 'theme-' + active;
            btn.classList.toggle('is-active', on);
            btn.setAttribute('aria-pressed', on ? 'true' : 'false');
        });
    }

    /* Text size ------------------------------------------------------- */

    /* Seven sizes, a step apart (lnUtils.TEXT_SIZE). The size in force is
       the stored one, else the default; a size off the steps (the old
       scale's 20 or 26px) steps to the nearest one in that direction. */
    var lastSize = null;    /* the size this page last set, if storage cannot hold it */

    function currentSize() {
        var stored = window.lnUtils && window.lnUtils.storedTextSize
            ? window.lnUtils.storedTextSize() : null;
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
