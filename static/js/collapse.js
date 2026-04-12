/* collapse.js — Collapsible h2/h3 sections in essay body.
   Self-guards via #markdownBody check; no-ops on non-essay pages.
   Persists collapsed state per heading in localStorage.
   Retriggered sidenote positioning after each transition via window resize.

   Exposes window.reinitCollapse(container) for use by transclude.js when
   newly injected content contains collapsible headings.
*/
(function () {
    'use strict';

    var PREFIX = 'section-collapsed:';

    function initHeading(heading) {
        var level   = parseInt(heading.tagName[1], 10);
        var content = [];
        var node    = heading.nextElementSibling;

        // Collect sibling elements until the next same-or-higher heading.
        while (node) {
            if (/^H[1-6]$/.test(node.tagName) &&
                parseInt(node.tagName[1], 10) <= level) break;
            content.push(node);
            node = node.nextElementSibling;
        }
        if (!content.length) return;

        // Wrap collected nodes in a .section-body div.
        var wrapper   = document.createElement('div');
        wrapper.className = 'section-body';
        wrapper.id        = 'section-body-' + heading.id;
        heading.parentNode.insertBefore(wrapper, content[0]);
        content.forEach(function (el) { wrapper.appendChild(el); });

        // Inject toggle button into the heading.
        var btn = document.createElement('button');
        btn.className = 'section-toggle';
        btn.setAttribute('aria-label', 'Toggle section');
        btn.setAttribute('aria-controls', wrapper.id);
        heading.appendChild(btn);

        // Restore persisted state without transition flash.
        var key       = PREFIX + heading.id;
        var collapsed = localStorage.getItem(key) === '1';

        function setCollapsed(c, animate) {
            if (!animate) wrapper.style.transition = 'none';
            if (c) {
                wrapper.style.maxHeight = '0';
                wrapper.classList.add('is-collapsed');
                btn.setAttribute('aria-expanded', 'false');
            } else {
                // Animate: transition 0 → scrollHeight, then release to 'none'
                // in transitionend so late-rendering content (e.g. KaTeX) is
                // never clipped. No animation: go straight to 'none'.
                wrapper.style.maxHeight = animate
                    ? wrapper.scrollHeight + 'px'
                    : 'none';
                wrapper.classList.remove('is-collapsed');
                btn.setAttribute('aria-expanded', 'true');
            }
            if (!animate) {
                // Re-enable transition after layout pass.
                requestAnimationFrame(function () {
                    requestAnimationFrame(function () {
                        wrapper.style.transition = '';
                    });
                });
            }
        }

        setCollapsed(collapsed, false);

        btn.addEventListener('click', function (e) {
            e.stopPropagation();
            var isCollapsed = wrapper.classList.contains('is-collapsed');
            if (!isCollapsed) {
                // Pin height before collapsing so CSS transition has a from-value.
                wrapper.style.maxHeight = wrapper.scrollHeight + 'px';
                void wrapper.offsetHeight; // force reflow
            }
            setCollapsed(!isCollapsed, true);
            localStorage.setItem(key, isCollapsed ? '0' : '1');
        });

        // After open animation: release the height cap so late-rendering
        // content (KaTeX, images) is never clipped.
        // After close animation: cap is already 0, nothing to do.
        // Also retrigger sidenote layout after each transition.
        wrapper.addEventListener('transitionend', function () {
            if (!wrapper.classList.contains('is-collapsed')) {
                wrapper.style.maxHeight = 'none';
            }
            window.dispatchEvent(new Event('resize'));
        });
    }

    document.addEventListener('DOMContentLoaded', function () {
        var body = document.getElementById('markdownBody');
        if (!body) return;
        if (body.hasAttribute('data-no-collapse')) return;

        var headings = Array.from(body.querySelectorAll('h2[id], h3[id]'));
        if (!headings.length) return;

        headings.forEach(initHeading);
    });

    // Public entry point for transclude.js: initialize collapse toggles on
    // headings inside a newly injected fragment.
    window.reinitCollapse = function (container) {
        Array.from(container.querySelectorAll('h2[id], h3[id]'))
            .forEach(initHeading);
    };
}());
