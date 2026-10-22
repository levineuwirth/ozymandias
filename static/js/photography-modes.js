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
    // Grid is the default: it shows the most frames per screen and keeps
    // chrome off the pictures until one is pointed at. Masonry is the
    // deliberate second choice, for reading rather than scanning.
    var DEFAULT_MODE = 'grid';

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

    /* sizes=auto is not available in every browser. Its fallback must
       follow the actual grid, including the reader's text-size setting;
       viewport breakpoints cannot predict the auto-fill column count. */
    function updateImageSizes(container) {
        var image = container.querySelector('img[sizes]');
        if (!image) return;
        var width = Math.ceil(image.getBoundingClientRect().width);
        if (!width) return;
        var sizes = 'auto, ' + width + 'px';
        container.querySelectorAll('img[sizes], source[sizes]').forEach(function (el) {
            if (el.getAttribute('sizes') !== sizes) el.setAttribute('sizes', sizes);
        });
    }

    function watchImageSizes(container) {
        var image = container.querySelector('img[sizes]');
        if (!image) return;
        updateImageSizes(container);
        if (window.ResizeObserver) {
            // A font-size change may change column count without changing
            // the container's width, so observe a card's image instead.
            new ResizeObserver(function () { updateImageSizes(container); }).observe(image);
        } else {
            window.addEventListener('resize', function () { updateImageSizes(container); });
        }
    }

    document.addEventListener('DOMContentLoaded', function () {
        /* Only the view links live in .photography-mode-toggle now — the
           slideshow launch was moved out of the group (A06), which also
           keeps it out of this list, where it never belonged: it has no
           layout mode and must not be marked current. */
        var toggleButtons = document.querySelectorAll('.photography-mode-toggle .mode-btn');
        var grid = document.querySelector('.photography-grid');
        document.querySelectorAll('.photography-contact-sheet').forEach(watchImageSizes);
        if (grid) watchImageSizes(grid);

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
            updateImageSizes(grid);
            /* A06: the controls are a labelled navigation group, not a
               tablist and not a set of toggle buttons, so the selected
               view is stated with aria-current rather than aria-pressed
               (which role="tab" did not permit in the first place). The
               .is-active class is kept because the CSS still keys off it;
               aria-current is what assistive technology reads. */
            toggleButtons.forEach(function (btn) {
                var match = btn.dataset.mode === mode;
                btn.classList.toggle('is-active', match);
                btn.removeAttribute('aria-pressed');
                if (match) {
                    btn.setAttribute('aria-current', 'true');
                } else {
                    btn.removeAttribute('aria-current');
                }
            });
            if (mode === 'masonry') {
                // Measure after the browser has applied the new column
                // template, not before: applyRowSpan reads card.clientWidth,
                // and the track width it needs is the one the mode it is
                // switching *into* produces.
                window.requestAnimationFrame(applyAllRowSpans);
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

        function rowSpanForRatio(ratio, contentWidth, metaHeight) {
            // ratio = naturalHeight / naturalWidth; contentWidth = rendered width
            var imageHeight = ratio * contentWidth;
            var totalHeight = imageHeight + metaHeight;
            return Math.max(1, Math.ceil((totalHeight + ROW_GAP) / (ROW_UNIT + ROW_GAP)));
        }

        // The title-and-date strip under the photograph, margin included,
        // as laid out at the card's width. A fixed 28px allowance was short
        // of a line of 0.9em text and its margin, more so when the title
        // wraps, and the caption ran 10-18px into the card below.
        function metaHeightOf(card) {
            var meta = card.querySelector('.photo-card-meta');
            if (!meta) return 0;
            var cs = getComputedStyle(meta);
            return meta.getBoundingClientRect().height
                 + (parseFloat(cs.marginTop) || 0) + (parseFloat(cs.marginBottom) || 0);
        }

        function applyRowSpan(card) {
            var img = card.querySelector('.photo-card-img');
            var width = card.clientWidth;
            if (!width) return; /* not visible yet — wait for resize */

            /* A card can legitimately have no image: a series landing that has
               not been given a lead frame yet renders as title and date alone.
               Returning early left such a card with no span at all, so it
               occupied a single 8px auto-row and its title printed on top of
               whatever sat beside it. Measure the content instead — the card
               is short, and short is the correct answer. */
            if (!img) {
                var main = card.querySelector('.item-card-main') || card;
                var contentHeight = main.scrollHeight || 24;
                card.style.gridRowEnd = 'span ' + Math.max(
                    1,
                    Math.ceil((contentHeight + ROW_GAP) / (ROW_UNIT + ROW_GAP))
                );
                return;
            }

            var ratio;
            if (img.naturalWidth && img.naturalHeight) {
                ratio = img.naturalHeight / img.naturalWidth;
            } else {
                var orient = card.dataset.orientation || 'landscape';
                ratio = ORIENTATION_RATIO[orient] || ORIENTATION_RATIO.landscape;
            }
            card.style.gridRowEnd = 'span ' + rowSpanForRatio(ratio, width, metaHeightOf(card));
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
                    // Guard the mode here too. Without it, an image that was
                    // already cached applied an inline row span while the page
                    // was in grid or chronological mode, where 8px auto-rows
                    // do not exist and the span means something else entirely.
                    if (grid.getAttribute('data-photography-mode') === 'masonry') {
                        applyRowSpan(img.closest('.photo-card'));
                    }
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
