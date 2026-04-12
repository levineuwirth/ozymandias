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

        overlay.appendChild(closeBtn);
        overlay.appendChild(img);
        overlay.appendChild(caption);
        document.body.appendChild(overlay);

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
            overlay.classList.add('is-open');
            document.documentElement.style.overflow = 'hidden';
            closeBtn.focus();
        }

        function close() {
            overlay.classList.remove('is-open');
            document.documentElement.style.overflow = '';
            if (triggerEl) {
                triggerEl.focus();
                triggerEl = null;
            }
            // Clear src after transition to stop background loading
            var delay = parseFloat(
                getComputedStyle(overlay).transitionDuration || '0'
            ) * 1000;
            setTimeout(function () {
                if (!overlay.classList.contains('is-open')) {
                    img.src = '';
                }
            }, delay + 50);
        }

        // ----------------------------------------------------------------
        // Wire up lightbox-marked images
        // ----------------------------------------------------------------

        var images = document.querySelectorAll('img[data-lightbox]');

        images.forEach(function (el) {
            el.addEventListener('click', function () {
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
            });
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

        // Escape key
        document.addEventListener('keydown', function (e) {
            if (e.key === 'Escape' && overlay.classList.contains('is-open')) {
                close();
            }
        });

    });

}());
