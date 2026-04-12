/* settings.js — Settings panel: theme, text size, print.
   Must stay in sync with TEXT_SIZES in theme.js.

   All localStorage access routes through window.lnUtils.safeStorage so
   Safari private-mode SecurityErrors on writes do not throw uncaught. */
(function () {
    'use strict';

    var TEXT_SIZES    = [20, 23, 26];
    var TEXT_SIZE_DEFAULT = 1;      /* index of 23px */
    var TEXT_SIZE_KEY = 'text-size';
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
        if      (action === 'theme-light')   setTheme('light');
        else if (action === 'theme-dark')    setTheme('dark');
        else if (action === 'text-smaller')  shiftSize(-1);
        else if (action === 'text-larger')   shiftSize(+1);
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
        document.querySelectorAll('[data-action^="theme-"]').forEach(function (btn) {
            btn.classList.toggle('is-active', btn.getAttribute('data-action') === 'theme-' + active);
        });
    }

    /* Text size ------------------------------------------------------- */

    function getSizeIndex() {
        var n = parseInt(store.get(TEXT_SIZE_KEY), 10);
        return (isNaN(n) || n < 0 || n >= TEXT_SIZES.length) ? TEXT_SIZE_DEFAULT : n;
    }

    function shiftSize(delta) {
        var idx = Math.max(0, Math.min(TEXT_SIZES.length - 1, getSizeIndex() + delta));
        store.set(TEXT_SIZE_KEY, idx);
        document.documentElement.style.setProperty('--text-size', TEXT_SIZES[idx] + 'px');
        syncTextSizeButtons();
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
        if (btn) btn.classList.toggle('is-active', document.documentElement.hasAttribute(attrName));
    }

    function syncTextSizeButtons() {
        var idx     = getSizeIndex();
        var smaller = document.querySelector('[data-action="text-smaller"]');
        var larger  = document.querySelector('[data-action="text-larger"]');
        if (smaller) smaller.disabled = (idx === 0);
        if (larger)  larger.disabled  = (idx === TEXT_SIZES.length - 1);
    }

    document.addEventListener('DOMContentLoaded', init);
}());
