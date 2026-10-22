(function () {
    'use strict';

    document.addEventListener('DOMContentLoaded', function () {

        // ----------------------------------------------------------------
        // Build the overlay DOM
        // ----------------------------------------------------------------

        var overlay = document.createElement('div');
        overlay.className = 'lightbox-overlay';
        overlay.setAttribute('role', 'dialog');
        overlay.setAttribute('aria-modal', 'true');
        overlay.setAttribute('aria-label', 'Image lightbox');

        var img = document.createElement('img');
        img.className = 'lightbox-img';
        /* Default accessible name; overwritten in open() with the
           triggering image's alt when present. Avoids a nameless
           lightbox image when the source img has no alt text. */
        img.alt = 'Lightbox image';

        var caption = document.createElement('p');
        caption.className = 'lightbox-caption';

        var closeBtn = document.createElement('button');
        closeBtn.className = 'lightbox-close';
        closeBtn.setAttribute('aria-label', 'Close lightbox');
        closeBtn.textContent = '×';

        // Darkroom-mode: photography pages only. Three additional
        // elements layered behind / beneath the photo: a vignette
        // overlay, an info panel showing EXIF-style metadata, and an
        // "i" toggle button for revealing/hiding the panel.
        // All three sit dormant on non-photography pages — the
        // "darkroom" class on the overlay gates their visibility in CSS.
        var vignette = document.createElement('div');
        vignette.className = 'lightbox-vignette';
        vignette.setAttribute('aria-hidden', 'true');

        var infoPanel = document.createElement('div');
        infoPanel.className = 'lightbox-info-panel';
        infoPanel.setAttribute('aria-hidden', 'true');

        var infoBtn = document.createElement('button');
        infoBtn.className = 'lightbox-info-toggle';
        infoBtn.setAttribute('aria-label', 'Toggle photo metadata');
        infoBtn.setAttribute('aria-pressed', 'false');
        infoBtn.textContent = 'ℹ';  // ℹ — information source

        overlay.appendChild(vignette);
        overlay.appendChild(closeBtn);
        overlay.appendChild(infoBtn);
        overlay.appendChild(img);
        overlay.appendChild(caption);
        overlay.appendChild(infoPanel);
        document.body.appendChild(overlay);

        // ----------------------------------------------------------------
        // Darkroom helpers — populate / clear info panel
        // ----------------------------------------------------------------

        function isDarkroomPage() {
            return document.body.dataset.pageType === 'photography';
        }

        // Mapping from data-photo-* attribute to the human-readable
        // label shown in the panel. Order is the rendering order; only
        // attributes present on the trigger image produce panel rows.
        var PANEL_FIELDS = [
            ['photoCaptured', 'Captured'],
            ['photoLocation', 'Location'],
            ['photoCamera',   'Camera'],
            ['photoLens',     'Lens'],
            ['photoFilm',     'Film'],
            ['photoExposure', 'Exposure']
        ];

        function populateInfoPanel(triggerImg) {
            infoPanel.innerHTML = '';
            if (!triggerImg || !triggerImg.dataset) return false;
            var dl = document.createElement('dl');
            var any = false;
            PANEL_FIELDS.forEach(function (entry) {
                var key = entry[0];
                var label = entry[1];
                var value = triggerImg.dataset[key];
                if (!value) return;
                any = true;
                var dt = document.createElement('dt');
                dt.textContent = label;
                var dd = document.createElement('dd');
                dd.textContent = value;
                dl.appendChild(dt);
                dl.appendChild(dd);
            });
            if (any) infoPanel.appendChild(dl);
            return any;
        }

        function setInfoVisible(visible) {
            overlay.classList.toggle('is-info-visible', visible);
            infoPanel.setAttribute('aria-hidden', visible ? 'false' : 'true');
            infoBtn.setAttribute('aria-pressed', visible ? 'true' : 'false');
        }

        // ----------------------------------------------------------------
        // Open / close helpers
        // ----------------------------------------------------------------

        var triggerEl = null;

        function open(src, alt, captionText, trigger) {
            triggerEl = trigger || null;
            img.src = src;
            /* Prefer the source img's alt; fall back to the figure
               caption; fall back to a generic label so the lightbox
               image always has an accessible name. */
            img.alt = alt || captionText || 'Lightbox image';
            caption.textContent = captionText || '';
            caption.hidden = !captionText;

            // Darkroom mode is keyed off body data-page-type. The
            // class is set BEFORE is-open so the dark backdrop is in
            // place at the start of the transition rather than fading
            // in over the existing one.
            var darkroom = isDarkroomPage();
            overlay.classList.toggle('darkroom', darkroom);
            var hasInfo = darkroom ? populateInfoPanel(triggerEl) : false;
            infoBtn.hidden = !hasInfo;
            setInfoVisible(false);

            overlay.classList.add('is-open');
            document.documentElement.style.overflow = 'hidden';
            closeBtn.focus();
        }

        function close() {
            overlay.classList.remove('is-open');
            setInfoVisible(false);
            document.documentElement.style.overflow = '';
            if (triggerEl) {
                triggerEl.focus();
                triggerEl = null;
            }
            // Clear src after transition to stop background loading.
            // Removed, not set to '': an empty src is a failed image
            // load, with an error event. The darkroom class is also
            // cleared on the same delay so the page chrome doesn't
            // re-appear on top of a fading black backdrop.
            var delay = parseFloat(
                getComputedStyle(overlay).transitionDuration || '0'
            ) * 1000;
            setTimeout(function () {
                if (!overlay.classList.contains('is-open')) {
                    img.removeAttribute('src');
                    overlay.classList.remove('darkroom');
                }
            }, delay + 50);
        }

        // ----------------------------------------------------------------
        // Wire up lightbox-marked images
        // ----------------------------------------------------------------

        // Wiring is per-image and idempotent (a data flag), so it can run
        // again over content injected after load. Smaller finding 7:
        // transcluded photographs used to arrive with no lightbox at all.
        function wireImage(el) {
            if (el.dataset.lightboxWired === '1') return;
            el.dataset.lightboxWired = '1';

            // Keyboard activation: the trigger acts as a button, and the
            // tabindex also lets close() return focus to it.
            el.setAttribute('tabindex', '0');
            el.setAttribute('role', 'button');

            function activate() {
                // Look for a sibling figcaption in the parent figure
                var figcaptionText = '';
                var parent = el.parentElement;
                if (parent) {
                    var figcaption = parent.querySelector('figcaption');
                    if (figcaption) {
                        figcaptionText = figcaption.textContent.trim();
                    }
                }
                open(el.src, el.alt, figcaptionText, el);
            }

            el.addEventListener('click', activate);
            el.addEventListener('keydown', function (e) {
                if (e.key === 'Enter' || e.key === ' ') {
                    e.preventDefault();
                    activate();
                }
            });
        }

        function wireLightbox(root) {
            var scope = root && root.querySelectorAll ? root : document;
            if (scope.nodeType === 1 && scope.matches
                && scope.matches('img[data-lightbox]')) {
                wireImage(scope);
            }
            scope.querySelectorAll('img[data-lightbox]').forEach(wireImage);
        }

        window.reinitLightbox = wireLightbox;
        wireLightbox(document);

        document.addEventListener('ln:content-added', function (e) {
            wireLightbox(e.detail && e.detail.container);
        });

        // ----------------------------------------------------------------
        // Close handlers
        // ----------------------------------------------------------------

        // Close button
        closeBtn.addEventListener('click', close);

        // Click on overlay background (not the image itself)
        overlay.addEventListener('click', function (e) {
            if (e.target === overlay) {
                close();
            }
        });

        // Info-panel button (darkroom only — gated by .darkroom class
        // on overlay; CSS hides the button on non-photography pages).
        infoBtn.addEventListener('click', function () {
            setInfoVisible(!overlay.classList.contains('is-info-visible'));
        });

        /* Focus trap for the overlay: cycle Tab/Shift+Tab through the
           focusable controls inside the lightbox so keyboard users
           cannot tab out into the obscured page background. Same
           approach as gallery.js's trapTab; the [hidden] exclusion
           covers infoBtn, which is hidden outside darkroom mode. */
        function trapTab(e) {
            var focusable = Array.from(overlay.querySelectorAll(
                'button:not([disabled]):not([hidden]), [tabindex]:not([tabindex="-1"])'
            ));
            if (focusable.length === 0) {
                e.preventDefault();
                return;
            }
            var first = focusable[0];
            var last  = focusable[focusable.length - 1];
            var active = document.activeElement;
            if (e.shiftKey) {
                if (active === first || !overlay.contains(active)) {
                    e.preventDefault();
                    last.focus();
                }
            } else {
                if (active === last || !overlay.contains(active)) {
                    e.preventDefault();
                    first.focus();
                }
            }
        }

        // Escape closes; Tab is trapped; "i" toggles info panel (darkroom only).
        document.addEventListener('keydown', function (e) {
            if (!overlay.classList.contains('is-open')) return;
            if (e.key === 'Escape') {
                close();
            } else if (e.key === 'Tab') {
                trapTab(e);
            } else if ((e.key === 'i' || e.key === 'I')
                       && overlay.classList.contains('darkroom')
                       && !infoBtn.hidden) {
                setInfoVisible(!overlay.classList.contains('is-info-visible'));
            }
        });

    });

}());
