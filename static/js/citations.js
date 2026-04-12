/* citations.js — hover tooltip for inline citation markers.
   On hover of a .cite-marker, reads the matching bibliography entry from
   the DOM and shows it in a floating tooltip.  On click, follows the href
   to jump to the bibliography section.  Phase 3 popups.js can supersede this. */

(function () {
    'use strict';

    let activeTooltip = null;
    let hideTimer     = null;

    function makeTooltip(html) {
        const el = document.createElement('div');
        el.className = 'cite-tooltip';
        el.innerHTML = html;
        el.addEventListener('mouseenter', () => clearTimeout(hideTimer));
        el.addEventListener('mouseleave', scheduleHide);
        return el;
    }

    function positionTooltip(tooltip, anchor) {
        document.body.appendChild(tooltip);
        const aRect = anchor.getBoundingClientRect();
        const tRect = tooltip.getBoundingClientRect();

        let left = aRect.left + window.scrollX;
        let top  = aRect.top  + window.scrollY - tRect.height - 10;

        // Keep horizontally within viewport with margin
        const maxLeft = window.innerWidth - tRect.width - 12;
        left = Math.max(8, Math.min(left, maxLeft));

        // Flip below anchor if not enough room above
        if (top < window.scrollY + 8) {
            top = aRect.bottom + window.scrollY + 10;
        }

        tooltip.style.left = left + 'px';
        tooltip.style.top  = top  + 'px';
    }

    function scheduleHide() {
        hideTimer = setTimeout(() => {
            if (activeTooltip) {
                activeTooltip.remove();
                activeTooltip = null;
            }
        }, 180);
    }

    function getRefHtml(refEl) {
        // Strip the [N] number span, return the remaining innerHTML
        const clone = refEl.cloneNode(true);
        const num   = clone.querySelector('.ref-num');
        if (num) num.remove();
        return clone.innerHTML.trim();
    }

    function init() {
        document.querySelectorAll('.cite-marker').forEach(marker => {
            const link = marker.querySelector('a.cite-link');
            if (!link) return;

            const href  = link.getAttribute('href');
            if (!href || !href.startsWith('#')) return;

            const refEl = document.getElementById(href.slice(1));
            if (!refEl) return;

            marker.addEventListener('mouseenter', () => {
                clearTimeout(hideTimer);
                if (activeTooltip) { activeTooltip.remove(); }
                activeTooltip = makeTooltip(getRefHtml(refEl));
                positionTooltip(activeTooltip, marker);
            });

            marker.addEventListener('mouseleave', scheduleHide);
        });
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', init);
    } else {
        init();
    }
})();
