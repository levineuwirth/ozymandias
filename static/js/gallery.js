/* gallery.js — Two orthogonal systems:
   1. Named exhibits (.exhibit[data-exhibit-name]) — TOC integration only.
   2. Math focusables — every .katex-display gets a hover expand button
      that opens a full-size overlay. Navigation is global (all focusables
      in document order). Group name in overlay comes from nearest exhibit
      or heading. */

(function () {
    'use strict';

    function slugify(name) {
        return name.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');
    }

    /* ============================================================
       NAMED EXHIBITS  (TOC integration)
       ============================================================ */

    var exhibits = [];

    function discoverExhibits() {
        document.querySelectorAll('.exhibit[data-exhibit-name]').forEach(function (el) {
            var name = el.dataset.exhibitName || '';
            var type = el.dataset.exhibitType || 'equation';
            var id   = 'exhibit-' + slugify(name);
            el.id    = id;
            exhibits.push({ el: el, type: type, name: name, id: id });
        });
    }

    function initProofExhibit(entry) {
        var body = entry.el.querySelector('.exhibit-body');
        if (!body) return;

        var header = document.createElement('div');
        header.className = 'exhibit-header';

        var label = document.createElement('span');
        label.className = 'exhibit-header-label';
        label.textContent = 'Proof.';

        var name = document.createElement('span');
        name.className = 'exhibit-header-name';
        name.textContent = entry.name;

        header.appendChild(label);
        header.appendChild(name);
        entry.el.insertBefore(header, body);
    }

    /* ============================================================
       MATH FOCUSABLES
       Auto-discover every .katex-display in #markdownBody.
       KaTeX must be called with output:'htmlAndMathml' so the
       original LaTeX survives in <annotation encoding="application/x-tex">.
       ============================================================ */

    var focusables = []; /* { katexEl, wrapperEl, source, groupName } */

    function getSource(katexEl) {
        var ann = katexEl.querySelector('annotation[encoding="application/x-tex"]');
        return ann ? ann.textContent.trim() : '';
    }

    function getGroupName(katexEl, markdownBody) {
        /* Named exhibit takes priority */
        var exhibit = katexEl.closest('.exhibit[data-exhibit-name]');
        if (exhibit) return exhibit.dataset.exhibitName || '';

        /* Otherwise: nearest preceding heading */
        var headings = Array.from(markdownBody.querySelectorAll(':is(h1,h2,h3,h4,h5,h6)'));
        var nearest  = null;
        headings.forEach(function (h) {
            if (h.compareDocumentPosition(katexEl) & Node.DOCUMENT_POSITION_FOLLOWING) {
                nearest = h;
            }
        });
        return nearest ? nearest.textContent.trim() : '';
    }

    function getCaption(katexEl) {
        var exhibit = katexEl.closest('.exhibit[data-exhibit-caption]');
        if (!exhibit) return '';
        /* A proof's caption belongs to the proof as a whole, not to each
           individual equation line within it. Only propagate for equation
           exhibits where the math IS the primary content. */
        if (exhibit.dataset.exhibitType === 'proof') return '';
        return exhibit.dataset.exhibitCaption || '';
    }

    function discoverFocusableMath(markdownBody) {
        markdownBody.querySelectorAll('.katex-display').forEach(function (katexEl) {
            var source    = getSource(katexEl);
            var groupName = getGroupName(katexEl, markdownBody);
            var caption   = getCaption(katexEl);

            /* Wrap in .math-focusable — the entire wrapper is the click target */
            var wrapper = document.createElement('div');
            wrapper.className = 'math-focusable';
            if (caption) wrapper.dataset.caption = caption; /* drives CSS ::after tooltip */
            katexEl.parentNode.insertBefore(wrapper, katexEl);
            wrapper.appendChild(katexEl);

            /* Decorative expand glyph (pointer-events: none in CSS) */
            var glyph = document.createElement('span');
            glyph.className = 'exhibit-expand';
            glyph.setAttribute('aria-hidden', 'true');
            glyph.textContent = '⤢';
            wrapper.appendChild(glyph);

            var entry = {
                type:      'math',
                katexEl:   katexEl,
                wrapperEl: wrapper,
                source:    source,
                groupName: groupName,
                caption:   caption
            };
            focusables.push(entry);

            /* Click anywhere on the wrapper opens the overlay */
            wrapper.addEventListener('click', function () {
                openOverlay(focusables.indexOf(entry));
            });
        });
    }

    function discoverFocusableScores(markdownBody) {
        markdownBody.querySelectorAll('.score-fragment').forEach(function (figEl) {
            var svgEl = figEl.querySelector('svg');
            if (!svgEl) return;

            var captionEl   = figEl.querySelector('.score-caption');
            var captionText = captionEl ? captionEl.textContent.trim() : '';
            var name        = figEl.dataset.exhibitName || '';
            var groupName   = name || getGroupName(figEl, markdownBody);

            /* Expand glyph — decorative affordance, same as math focusables */
            var glyph = document.createElement('span');
            glyph.className = 'exhibit-expand';
            glyph.setAttribute('aria-hidden', 'true');
            glyph.textContent = '⤢';
            figEl.appendChild(glyph);

            var entry = {
                type:      'score',
                wrapperEl: figEl,
                svgEl:     svgEl,
                groupName: groupName,
                caption:   captionText
            };
            focusables.push(entry);

            figEl.addEventListener('click', function () {
                openOverlay(focusables.indexOf(entry));
            });
        });
    }

    /* ============================================================
       OVERLAY
       ============================================================ */

    var overlay, overlayGroup, overlayBody, overlayCaption;
    var overlayPrev, overlayNext, overlayCounter, overlayClose;
    var currentIdx = -1;

    function buildOverlay() {
        overlay = document.createElement('div');
        overlay.id = 'gallery-overlay';
        overlay.setAttribute('hidden', '');
        overlay.setAttribute('role', 'dialog');
        overlay.setAttribute('aria-modal', 'true');

        /* All children are absolute or flex-centered — no panel wrapper */

        overlayClose = document.createElement('button');
        overlayClose.id = 'gallery-overlay-close';
        overlayClose.setAttribute('aria-label', 'Close');
        overlayClose.textContent = '✕';
        overlay.appendChild(overlayClose);

        overlayGroup = document.createElement('div');
        overlayGroup.id = 'gallery-overlay-name';
        overlay.appendChild(overlayGroup);

        overlayBody = document.createElement('div');
        overlayBody.id = 'gallery-overlay-body';
        overlay.appendChild(overlayBody);

        overlayCaption = document.createElement('div');
        overlayCaption.id = 'gallery-overlay-caption';
        overlay.appendChild(overlayCaption);

        overlayCounter = document.createElement('div');
        overlayCounter.id = 'gallery-overlay-counter';
        overlay.appendChild(overlayCounter);

        overlayPrev = document.createElement('button');
        overlayPrev.id = 'gallery-overlay-prev';
        overlayPrev.className = 'gallery-nav-btn';
        overlayPrev.setAttribute('aria-label', 'Previous equation');
        overlayPrev.textContent = '←';
        overlay.appendChild(overlayPrev);

        overlayNext = document.createElement('button');
        overlayNext.id = 'gallery-overlay-next';
        overlayNext.className = 'gallery-nav-btn';
        overlayNext.setAttribute('aria-label', 'Next equation');
        overlayNext.textContent = '→';
        overlay.appendChild(overlayNext);

        document.body.appendChild(overlay);

        /* Clicking the dark surround (not the content stage) closes */
        overlay.addEventListener('click', function (e) {
            if (e.target === overlay) closeOverlay();
        });
        overlayClose.addEventListener('click', closeOverlay);
        overlayPrev.addEventListener('click', function (e) { e.stopPropagation(); navigate(-1); });
        overlayNext.addEventListener('click', function (e) { e.stopPropagation(); navigate(+1); });

        document.addEventListener('keydown', function (e) {
            if (overlay.hasAttribute('hidden')) return;
            if (e.key === 'Escape')     { closeOverlay(); return; }
            if (e.key === 'ArrowLeft')  { navigate(-1);   return; }
            if (e.key === 'ArrowRight') { navigate(+1);   return; }
            if (e.key === 'Tab')        { trapTab(e);     return; }
        });
    }

    /* Focus trap for the overlay: cycle Tab/Shift+Tab through the
       focusable controls inside #gallery-overlay so keyboard users
       cannot tab out into the (currently inert) page background. */
    function trapTab(e) {
        var focusable = Array.from(overlay.querySelectorAll(
            'button:not([disabled]), [tabindex]:not([tabindex="-1"])'
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

    function openOverlay(idx) {
        currentIdx = idx;
        /* Show before rendering — measurements (scrollWidth etc.) return 0
           on elements inside display:none, so the fit loop needs the overlay
           to be visible before it runs. The browser will not repaint until
           JS yields, so the user sees only the final fitted size. */
        overlay.removeAttribute('hidden');
        renderOverlay();
        overlayClose.focus();
    }

    function closeOverlay() {
        var returnTo = currentIdx >= 0 ? focusables[currentIdx].wrapperEl : null;
        overlay.setAttribute('hidden', '');
        currentIdx = -1;
        if (returnTo) returnTo.focus();
    }

    function navigate(delta) {
        var next = currentIdx + delta;
        if (next < 0 || next >= focusables.length) return;
        currentIdx = next;
        renderOverlay();
        focusables[currentIdx].wrapperEl.scrollIntoView({
            behavior: 'instant',
            block:    'center'
        });
    }

    function renderOverlay() {
        var entry = focusables[currentIdx];

        overlayGroup.textContent = entry.groupName;

        if (entry.type === 'score') {
            overlayBody.className    = 'is-score';
            overlayBody.style.overflow = 'hidden';
            overlayBody.innerHTML    = '';
            overlayBody.appendChild(entry.svgEl.cloneNode(true));
        } else {
            overlayBody.className    = '';
            overlayBody.style.overflow = 'hidden';

            /* Re-render from source, or clone rendered HTML */
            if (entry.source && typeof katex !== 'undefined') {
                try {
                    overlayBody.innerHTML = katex.renderToString(entry.source, {
                        displayMode:  true,
                        throwOnError: false
                    });
                } catch (e) {
                    overlayBody.innerHTML = entry.katexEl.outerHTML;
                }
            } else {
                overlayBody.innerHTML = entry.katexEl.outerHTML;
            }

            /* Fit font size — set directly on .katex-display to avoid cascade.
               The overlay must already be visible (not display:none) for
               scrollWidth/clientWidth to return real values. */
            var katexEl = overlayBody.querySelector('.katex-display');
            if (katexEl) {
                var maxSize = 1.4;
                var minSize = 0.4;
                var step    = 0.05;
                var fitted  = false;
                for (var fs = maxSize; fs >= minSize; fs -= step) {
                    katexEl.style.fontSize = fs + 'em';
                    if (overlayBody.scrollWidth <= overlayBody.clientWidth &&
                        overlayBody.scrollHeight <= overlayBody.clientHeight) {
                        fitted = true;
                        break;
                    }
                }
                if (!fitted) overlayBody.style.overflow = 'auto'; /* absolute last resort */
            }
        }

        overlayCaption.textContent = entry.caption || '';
        overlayCaption.hidden = !entry.caption;

        var total = focusables.length;
        overlayCounter.textContent = (currentIdx + 1) + ' / ' + total;
        overlayPrev.disabled = (currentIdx === 0);
        overlayNext.disabled = (currentIdx === total - 1);
    }

    /* ============================================================
       TOC INTEGRATION
       ============================================================ */

    function patchTOC() {
        var toc = document.getElementById('toc');
        if (!toc || exhibits.length === 0) return;

        var headings = Array.from(
            document.querySelectorAll('#markdownBody :is(h1,h2,h3,h4,h5,h6)[id]')
        );

        var headingMap = new Map();
        exhibits.forEach(function (entry) {
            var nearest = null;
            headings.forEach(function (h) {
                if (h.compareDocumentPosition(entry.el) & Node.DOCUMENT_POSITION_FOLLOWING) {
                    nearest = h;
                }
            });
            if (nearest) {
                if (!headingMap.has(nearest.id)) headingMap.set(nearest.id, []);
                headingMap.get(nearest.id).push(entry);
            }
        });

        toc.querySelectorAll('a[data-target]').forEach(function (link) {
            var list = headingMap.get(link.dataset.target);
            if (!list || list.length === 0) return;

            var row = document.createElement('div');
            row.className = 'toc-exhibits-inline';

            list.forEach(function (entry) {
                var a = document.createElement('a');
                a.href = '#' + entry.id;

                var badge = document.createElement('span');
                badge.className = 'toc-exhibit-type-badge';
                badge.textContent = entry.type;
                a.appendChild(badge);
                a.appendChild(document.createTextNode(entry.name));
                row.appendChild(a);
            });

            var li = link.closest('li');
            if (li) li.appendChild(row);
        });

        /* Contained Herein */
        var tocNav = toc.querySelector('.toc-nav');
        if (!tocNav) return;

        var contained = document.createElement('div');
        contained.className = 'toc-contained';

        var toggleBtn = document.createElement('button');
        toggleBtn.className = 'toc-contained-toggle';
        toggleBtn.setAttribute('aria-expanded', 'false');

        var arrow = document.createElement('span');
        arrow.className = 'toc-contained-arrow';
        arrow.textContent = '▶';
        toggleBtn.appendChild(arrow);
        toggleBtn.appendChild(document.createTextNode(' Contained Herein'));
        contained.appendChild(toggleBtn);

        var ul = document.createElement('ul');
        ul.className = 'toc-contained-list';

        exhibits.forEach(function (entry) {
            var li = document.createElement('li');
            var a  = document.createElement('a');
            a.href = '#' + entry.id;

            var badge = document.createElement('span');
            badge.className = 'toc-exhibit-type-badge';
            badge.textContent = entry.type;
            a.appendChild(badge);
            a.appendChild(document.createTextNode(entry.name));
            li.appendChild(a);
            ul.appendChild(li);
        });

        contained.appendChild(ul);
        tocNav.appendChild(contained);

        toggleBtn.addEventListener('click', function () {
            var open = contained.classList.toggle('is-open');
            toggleBtn.setAttribute('aria-expanded', String(open));
        });
    }

    /* ============================================================
       ANNOTATIONS  (collapsible .annotation--collapsible boxes)
       ============================================================ */

    function initAnnotations() {
        document.querySelectorAll('.annotation--collapsible').forEach(function (el) {
            var toggle = el.querySelector('.annotation-toggle');
            var body   = el.querySelector('.annotation-body');
            if (!toggle || !body) return;

            function setOpen(open) {
                el.classList.toggle('is-open', open);
                toggle.setAttribute('aria-expanded', String(open));
                toggle.textContent = open ? '▾ collapse' : '▸ expand';
                body.style.maxHeight = open ? body.scrollHeight + 'px' : '0';
            }

            setOpen(false);
            toggle.addEventListener('click', function () {
                setOpen(!el.classList.contains('is-open'));
            });
        });
    }

    /* ============================================================
       INIT
       ============================================================ */

    document.addEventListener('DOMContentLoaded', function () {
        var markdownBody = document.getElementById('markdownBody');

        discoverExhibits();
        exhibits.forEach(function (entry) {
            if (entry.type === 'proof') initProofExhibit(entry);
        });

        if (markdownBody) {
            discoverFocusableMath(markdownBody);
            discoverFocusableScores(markdownBody);
            if (focusables.length > 0) buildOverlay();
        }

        if (exhibits.length > 0) patchTOC();

        initAnnotations();
    });

})();
