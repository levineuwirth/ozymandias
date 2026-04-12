/* sidenotes.js — Collision avoidance and hover linking for sidenotes.
 *
 * HTML structure produced by Filters/Sidenotes.hs:
 *   <sup class="sidenote-ref" id="snref-N"><a href="#sn-N">N</a></sup>
 *   <span class="sidenote" id="sn-N">…</span>
 *
 * #markdownBody must be position: relative (layout.css guarantees this).
 * The .sidenote spans are position: absolute; left: calc(100% + 2.5rem).
 * Without an explicit top they sit at their "hypothetical static position",
 * which is fine for isolated notes but causes overlaps when notes are close.
 *
 * This script:
 *   1. Anchors each sidenote's top to its reference's offsetTop so the
 *      alignment is explicit and stable across all browsers.
 *   2. Walks notes top-to-bottom and pushes each one below the previous
 *      if they would overlap (with a small gap between them).
 *   3. Wires bidirectional hover highlights between each ref↔sidenote pair.
 */

(function () {
    'use strict';

    const GAP = 12; /* minimum px gap between successive sidenotes */

    function positionSidenotes() {
        const body = document.getElementById('markdownBody');
        if (!body) return;

        const notes = Array.from(body.querySelectorAll('.sidenote'));
        if (notes.length === 0) return;

        /* Only run on wide viewports where sidenotes are visible. */
        if (getComputedStyle(notes[0]).display === 'none') return;

        let prevBottom = 0;

        notes.forEach(function (sn) {
            /* Reset any prior JS-applied top so offsetTop reads correctly. */
            sn.style.top = '';

            const id    = sn.id;                          /* "sn-N"    */
            const refId = 'snref-' + id.slice(3);         /* "snref-N" */
            const ref   = document.getElementById(refId);

            /* Preferred top: align with the reference superscript. */
            const preferred = ref ? ref.offsetTop : sn.offsetTop;
            const top       = Math.max(preferred, prevBottom + GAP);

            sn.style.top = top + 'px';
            prevBottom   = top + sn.offsetHeight;
        });
    }

    /* ------------------------------------------------------------------ */
    /*  Hover + click + keyboard wiring                                    */
    /* ------------------------------------------------------------------ */

    /* At most one sidenote is "focused" (click-sticky) at a time. */
    let focusedPair = null;

    function wireHover(ref, sn) {
        /* Idempotent: skip pairs already wired so 'reinitSidenotes'
           after a transclusion injection cannot stack listeners. */
        if (ref.dataset.snBound === '1') return;
        ref.dataset.snBound = '1';
        sn.dataset.snBound  = '1';

        let hovering = false;
        let focused  = false;

        function update() {
            const active = focused || hovering;
            ref.classList.toggle('is-active', active);
            sn.classList.toggle('is-active', active);
        }

        function unfocus() { focused = false; update(); }

        function toggleFocus() {
            /* Sticky focus only makes sense on wide viewports where the
               sidenote is actually visible. On narrow screens there's
               nothing to pin. */
            if (getComputedStyle(sn).display === 'none') return;
            if (focused) {
                focused = false;
                focusedPair = null;
            } else {
                if (focusedPair) focusedPair.unfocus();
                focused = true;
                focusedPair = { ref: ref, sn: sn, unfocus: unfocus };
            }
            update();
        }

        ref.addEventListener('mouseenter', function () { hovering = true;  update(); });
        ref.addEventListener('mouseleave', function () { hovering = false; update(); });
        sn.addEventListener('mouseenter',  function () { hovering = true;  update(); });
        sn.addEventListener('mouseleave',  function () { hovering = false; update(); });

        /* Click on the superscript link: sticky focus on wide viewports,
           normal anchor scroll on narrow viewports (sidenote hidden). */
        const link = ref.querySelector('a');
        if (link) {
            link.addEventListener('click', function (e) {
                e.preventDefault();
                toggleFocus();
            });

            /* Keyboard activation: Enter follows the link by default (the
               browser synthesizes a click), but Space does not. Both are
               expected to toggle focus on a focus-activated element, so
               we normalize: Enter/Space toggle, Escape clears if focused.
               The <a href="#sn-N"> retains its native focusability so
               Tab reaches it; we only intercept the key. */
            link.addEventListener('keydown', function (e) {
                if (e.key === ' ') {
                    e.preventDefault();
                    toggleFocus();
                } else if (e.key === 'Escape' && focused) {
                    e.preventDefault();
                    focused = false;
                    focusedPair = null;
                    update();
                }
            });
        }
    }

    /* Click anywhere outside a focused pair dismisses it. */
    document.addEventListener('click', function (e) {
        if (focusedPair &&
            !focusedPair.ref.contains(e.target) &&
            !focusedPair.sn.contains(e.target)) {
            focusedPair.unfocus();
            focusedPair = null;
        }
    });

    /* Global Escape dismisses any sticky-focused sidenote, even if focus
       has moved away from the ref link. */
    document.addEventListener('keydown', function (e) {
        if (e.key === 'Escape' && focusedPair) {
            focusedPair.unfocus();
            focusedPair = null;
        }
    });

    function wireAll(root) {
        root.querySelectorAll('.sidenote').forEach(function (sn) {
            const refId = 'snref-' + sn.id.slice(3);
            const ref   = document.getElementById(refId);
            if (ref) wireHover(ref, sn);
        });
    }

    function init() {
        const body = document.getElementById('markdownBody');
        if (!body) return;

        wireAll(body);
        positionSidenotes();
    }

    /* Public re-init hook used by transclude.js after it injects new
       content. wireAll is idempotent so calling this multiple times is
       safe. */
    window.reinitSidenotes = function (container) {
        wireAll(container || document.getElementById('markdownBody') || document);
        positionSidenotes();
    };

    document.addEventListener('DOMContentLoaded', init);
    window.addEventListener('resize', positionSidenotes);
}());
