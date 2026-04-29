/* Photography section — view mode toggle and masonry row-span computation.
 *
 * Mode toggle:
 *   - Three modes: masonry (default), grid, chronological.
 *   - Selection persists to localStorage under "photography-mode".
 *   - Mode is applied as data-photography-mode on .photography-grid;
 *     CSS keys all per-mode rules off this attribute.
 *
 * Masonry row-spans:
 *   - In masonry mode, .photography-grid uses CSS grid with
 *     grid-auto-rows: 1px so each card can occupy a precise integer
 *     number of "row units" matching its image's natural aspect.
 *   - For each photo card we set inline grid-row: span N once the
 *     image's natural dimensions are known. Pre-load, the card uses
 *     orientation-derived defaults from data-orientation so initial
 *     paint is roughly the right shape.
 *
 * No-op gracefully:
 *   - If the page has no .photography-grid (i.e. we're not on the
 *     /photography/ landing) the script returns early.
 *   - If localStorage is unavailable, mode toggling still works for
 *     the duration of the visit; just no persistence.
 */
(function () {
    'use strict';

    var STORAGE_KEY = 'photography-mode';
    // Modes applied inline on /photography/ (CSS toggle). "map" is a
    // separate page and is excluded here so it follows normal navigation.
    var VALID_MODES = ['masonry', 'grid', 'chronological'];
    var DEFAULT_MODE = 'masonry';

    // Row unit (px) — must match grid-auto-rows in photography.css.
    // Smaller unit = finer granularity = better aspect-ratio matching
    // at a small cost in inline-style verbosity. 8px is a common sweet
    // spot for masonry.
    var ROW_UNIT = 8;

    // Vertical gap between rows in masonry mode (px). Must match
    // photography.css .photography-grid[data-photography-mode="masonry"] gap.
    var ROW_GAP = 8;

    // Approximate aspect ratios for each declared orientation; used as
    // a placeholder span before the image's natural dimensions arrive.
    var ORIENTATION_RATIO = {
        portrait:  3 / 2,   // height / width — taller than wide
        landscape: 2 / 3,
        square:    1
    };

    document.addEventListener('DOMContentLoaded', function () {
        var toggleButtons = document.querySelectorAll('.photography-mode-toggle .mode-btn');
        var grid = document.querySelector('.photography-grid');

        // The script is loaded site-wide on photography pages but only
        // does work when there's a toggle (and, separately, when there's
        // a grid for masonry row-spans). Bail early if neither is present.
        if (toggleButtons.length === 0 && !grid) return;

        // ----------------------------------------------------------------
        // localStorage helpers (shared by the toggle and any future
        // mode-aware behaviour on photography pages)
        // ----------------------------------------------------------------

        function readStoredMode() {
            try {
                var stored = window.localStorage.getItem(STORAGE_KEY);
                if (stored && VALID_MODES.indexOf(stored) !== -1) {
                    return stored;
                }
            } catch (e) { /* localStorage unavailable */ }
            return DEFAULT_MODE;
        }

        function writeStoredMode(mode) {
            try { window.localStorage.setItem(STORAGE_KEY, mode); }
            catch (e) { /* localStorage unavailable */ }
        }

        // ----------------------------------------------------------------
        // Mode toggle
        // ----------------------------------------------------------------

        function applyMode(mode) {
            if (!grid) return;
            grid.setAttribute('data-photography-mode', mode);
            toggleButtons.forEach(function (btn) {
                var match = btn.dataset.mode === mode;
                btn.classList.toggle('is-active', match);
                btn.setAttribute('aria-pressed', match ? 'true' : 'false');
            });
            if (mode === 'masonry') {
                applyAllRowSpans();
            } else {
                clearAllRowSpans();
            }
        }

        toggleButtons.forEach(function (btn) {
            btn.addEventListener('click', function (e) {
                var mode = btn.dataset.mode;
                if (!mode) return;

                // Persist the chosen mode regardless of whether we're
                // applying it inline or following a link. This is what
                // makes the round-trip through /photography/map/ remember
                // which grid view to land on.
                if (VALID_MODES.indexOf(mode) !== -1) {
                    writeStoredMode(mode);
                }

                // On the landing page (where the grid is in the DOM),
                // apply masonry/grid/chronological inline and suppress
                // the anchor's default navigation. The "map" link
                // ALWAYS navigates — there's no inline alternative.
                if (grid && VALID_MODES.indexOf(mode) !== -1) {
                    e.preventDefault();
                    applyMode(mode);
                }
            });
        });

        // ----------------------------------------------------------------
        // Masonry row-spans
        // ----------------------------------------------------------------

        function rowSpanForRatio(ratio, contentWidth) {
            // ratio = naturalHeight / naturalWidth; contentWidth = rendered width
            var imageHeight = ratio * contentWidth;
            // Allow ~1.4em for the meta strip (title + date below image).
            var metaHeight = 28;
            var totalHeight = imageHeight + metaHeight;
            return Math.max(1, Math.ceil((totalHeight + ROW_GAP) / (ROW_UNIT + ROW_GAP)));
        }

        function applyRowSpan(card) {
            var img = card.querySelector('.photo-card-img');
            if (!img) return;
            var width = card.clientWidth;
            if (!width) return; /* not visible yet — wait for resize */

            var ratio;
            if (img.naturalWidth && img.naturalHeight) {
                ratio = img.naturalHeight / img.naturalWidth;
            } else {
                var orient = card.dataset.orientation || 'landscape';
                ratio = ORIENTATION_RATIO[orient] || ORIENTATION_RATIO.landscape;
            }
            card.style.gridRowEnd = 'span ' + rowSpanForRatio(ratio, width);
        }

        function applyAllRowSpans() {
            grid.querySelectorAll('.photo-card').forEach(applyRowSpan);
        }

        function clearAllRowSpans() {
            grid.querySelectorAll('.photo-card').forEach(function (card) {
                card.style.gridRowEnd = '';
            });
        }

        // Refine spans once each image's natural dimensions are known.
        // No-op when the grid is absent (e.g. on the map page).
        if (grid) {
            grid.querySelectorAll('.photo-card-img').forEach(function (img) {
                if (img.complete && img.naturalWidth) {
                    applyRowSpan(img.closest('.photo-card'));
                } else {
                    img.addEventListener('load', function () {
                        if (grid.getAttribute('data-photography-mode') === 'masonry') {
                            applyRowSpan(img.closest('.photo-card'));
                        }
                    });
                }
            });

            // Re-flow on resize — cell width changes affect height.
            var resizeRaf = null;
            window.addEventListener('resize', function () {
                if (resizeRaf) cancelAnimationFrame(resizeRaf);
                resizeRaf = requestAnimationFrame(function () {
                    if (grid.getAttribute('data-photography-mode') === 'masonry') {
                        applyAllRowSpans();
                    }
                });
            });
        }

        // ----------------------------------------------------------------
        // Boot — only applies a mode when we're on a page that has the
        // grid. The map page has the toggle but no grid; for it,
        // localStorage is read/written by the click handler above and
        // by the grid page's boot when the user navigates back.
        // ----------------------------------------------------------------

        if (grid) applyMode(readStoredMode());
    });

}());
