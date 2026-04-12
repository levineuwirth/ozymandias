/* nav.js — Portal row expand/collapse with localStorage persistence.
   Loaded with defer.
*/
(function () {
    const STORAGE_KEY = 'portals-open';

    document.addEventListener('DOMContentLoaded', function () {
        // Return-to-top button
        var totop = document.querySelector('.footer-totop');
        if (totop) {
            totop.addEventListener('click', function () {
                window.scrollTo({ top: 0, behavior: 'smooth' });
            });
        }

        const portals = document.querySelector('.nav-portals');
        const toggle  = document.querySelector('.nav-portal-toggle');
        if (!portals || !toggle) return;

        function setOpen(open) {
            portals.classList.toggle('is-open', open);
            toggle.setAttribute('aria-expanded', String(open));
            // Rotate arrow indicator if present.
            const arrow = toggle.querySelector('.nav-portal-arrow');
            if (arrow) arrow.textContent = open ? '▲' : '▼';
            localStorage.setItem(STORAGE_KEY, open ? '1' : '0');
        }

        // Restore persisted state; default is collapsed.
        const stored = localStorage.getItem(STORAGE_KEY);
        setOpen(stored === '1');

        toggle.addEventListener('click', function () {
            setOpen(!portals.classList.contains('is-open'));
        });
    });
})();
