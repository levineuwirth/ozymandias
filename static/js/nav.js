/* nav.js — Portal row expand/collapse with localStorage persistence.
   Loaded with defer.
*/
(function () {
    const STORAGE_KEY = 'portals-open';

    /* A09: one effective reduced-motion preference — the union of the
       site's own Reduce Motion setting ([data-reduce-motion] on <html>,
       stamped by theme.js and toggled by settings.js) and the operating
       system's prefers-reduced-motion. Read at the moment of use rather
       than cached, so a change to either input takes effect on the very
       next interaction with no listener to keep in sync. The same union
       is expressed in CSS in components.css. */
    const motionQuery = window.matchMedia
        ? window.matchMedia('(prefers-reduced-motion: reduce)')
        : null;

    function reducedMotion() {
        if (document.documentElement.hasAttribute('data-reduce-motion')) return true;
        return !!(motionQuery && motionQuery.matches);
    }

    /* --nav-height: the sticky header's real height, published to <html>
       as an inline style so base.css's static estimate is superseded.
       Four rules depend on it — scroll-padding-top (the landing offset
       for every in-page anchor), the TOC's sticky top and max-height,
       and the sticky-footer math — and a wrong value shows up as an
       anchor jump that lands the heading under the nav.

       An estimate cannot be right on its own: the header is one row on
       desktop and three on a phone, grows again when the Portals row is
       opened, changes when a web font swaps in, and collapses to nothing
       under focus mode. A ResizeObserver on the header catches all five
       without a listener per cause. */
    /* A deep link lands where the browser scrolled it, with whatever
       --nav-height was in force then: base.css's estimate, before this
       script has measured. And Firefox applies a new value to the scroll
       padding a frame or two late, so even an immediate re-aim (as
       collapse.js makes) can still use the estimate. A header taller than
       the estimate (a phone, the Portals row, a larger text size) covered
       the heading. While the page is landing, each new height aims at the
       target again two frames on; never once the reader has moved, nor
       later than a second after load. Returns the re-aim, or null. */
    function landing() {
        var id = '';
        try { id = decodeURIComponent(location.hash.slice(1)); }
        catch (_) { return null; }
        var target = id && document.getElementById(id);
        if (!target) return null;
        var open = true;
        function stop() { open = false; }
        ['wheel', 'touchstart', 'keydown', 'pointerdown'].forEach(function (ev) {
            window.addEventListener(ev, stop, { once: true, passive: true, capture: true });
        });
        window.addEventListener('load', function () { setTimeout(stop, 1000); });
        return function () {
            if (!open) return;
            requestAnimationFrame(function () {
                requestAnimationFrame(function () {
                    if (open) target.scrollIntoView({ block: 'start', behavior: 'instant' });
                });
            });
        };
    }

    function initNavHeight() {
        var header = document.querySelector('body > header:first-of-type');
        if (!header) return;

        var reaim = landing();
        var last = null;
        var shell = document.querySelector('body > .page-shell');
        var banner = null;
        /* A site's stylesheet may take the header off the top of the
           window: unstuck, so that it scrolls away, or out of the flow
           altogether (a masthead in the margin). What stays on screen is
           what an anchor must clear and the TOC must sit under
           (--nav-height); what stands in the flow is what the page shell
           gives up of the window's height (--nav-flow-height). Its
           height wherever it stands (--nav-box-height) is for the site
           that put it beside the page and must keep clear of it. */
        function sync() {
            var h = Math.round(header.getBoundingClientRect().height);
            var position = getComputedStyle(header).position;
            var stuck = position === 'sticky' || position === 'fixed';
            var flows = position !== 'absolute' && position !== 'fixed';
            /* What a site's banner (templates/partials/banner.html) takes of
               the flow between the header and the page shell, which gives
               that up of the window's height as well (--banner-height), so
               that a short page's footer still ends at the window's foot.
               From the header's own height, unrounded, and rounded up: a
               site without a banner gets none, and a fraction of a pixel
               too little would leave a short page a scrollbar. Unset until
               there is one. */
            var above = shell ? shell.getBoundingClientRect().top + window.scrollY : 0;
            var room = shell ? Math.max(0, Math.ceil(above - (flows ? header.getBoundingClientRect().height : 0) - 0.01)) : 0;
            var now = h + ' ' + stuck + ' ' + flows + ' ' + room;
            if (now === last) return;        /* no needless style invalidation */
            last = now;
            if (room || banner !== null) {
                banner = room;
                document.documentElement.style.setProperty('--banner-height', room + 'px');
            }
            document.documentElement.style.setProperty('--nav-height', (stuck ? h : 0) + 'px');
            document.documentElement.style.setProperty('--nav-flow-height', (flows ? h : 0) + 'px');
            document.documentElement.style.setProperty('--nav-box-height', h + 'px');
            if (reaim) reaim();
        }

        sync();

        if (window.ResizeObserver) {
            var watch = new ResizeObserver(sync);
            watch.observe(header);
            /* And a banner's elements, which a site's script may size. */
            for (var el = header.nextElementSibling; el && el !== shell; el = el.nextElementSibling) {
                if (el.tagName !== 'SCRIPT') watch.observe(el);
            }
        }
        /* The header's position can change with the window's width and
           leave its height as it was. */
        window.addEventListener('resize', sync, { passive: true });

        /* A font swapping in after first paint changes the nav's height
           without resizing anything the observer above is watching in
           browsers that lack ResizeObserver. */
        if (document.fonts && document.fonts.ready) {
            document.fonts.ready.then(sync).catch(function () {});
        }
    }

    document.addEventListener('DOMContentLoaded', function () {
        /* The Portals row first: the header's height includes it. */
        initPortals();
        initNavHeight();

        // Return-to-top button. Scripted scrolling is motion the CSS
        // override cannot reach — behavior is decided here instead.
        var totop = document.querySelector('.footer-totop');
        if (totop) {
            totop.addEventListener('click', function () {
                window.scrollTo({
                    top: 0,
                    behavior: reducedMotion() ? 'auto' : 'smooth'
                });
            });
        }

        // The site-wide build time, from the one file each build writes
        // (tools/stamp-build-time.py). Stamping it into every page made
        // every build rewrite, recompress, re-sign and re-upload ~500
        // unchanged pages; this way they stay byte-identical. Without
        // script, or before the file loads, the span is simply empty.
        var buildTime = document.querySelector('[data-build-time]');
        if (buildTime && window.fetch) {
            fetch('/build/time.txt', { cache: 'no-cache' })
                .then(function (r) { return r.ok ? r.text() : ''; })
                .then(function (t) { if (t.trim()) buildTime.textContent = t.trim(); })
                .catch(function () {});
        }

    });

    function initPortals() {
        const portals = document.querySelector('.nav-portals');
        const toggle  = document.querySelector('.nav-portal-toggle');
        if (!portals || !toggle) return;

        // safeStorage (utils.js, loaded synchronously before us) so a
        // storage-blocked context can't throw before the click listener
        // below binds; guarded like theme.js in case utils.js itself
        // failed to load.
        const store = window.ozyUtils && window.ozyUtils.safeStorage;

        function setOpen(open) {
            portals.classList.toggle('is-open', open);
            document.documentElement.toggleAttribute('data-portals-open', open);
            toggle.setAttribute('aria-expanded', String(open));
            // Rotate arrow indicator if present.
            const arrow = toggle.querySelector('.nav-portal-arrow');
            if (arrow) arrow.textContent = open ? '▲' : '▼';
            if (store) store.set(STORAGE_KEY, open ? '1' : '0');
        }

        // Restore persisted state; default is collapsed.
        const stored = store ? store.get(STORAGE_KEY) : null;
        setOpen(stored === '1');

        toggle.addEventListener('click', function () {
            setOpen(!portals.classList.contains('is-open'));
        });
    }
})();
