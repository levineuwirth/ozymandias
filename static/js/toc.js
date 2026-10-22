/* toc.js — Sticky TOC with IntersectionObserver scroll tracking,
   horizontal progress indicator, and expand/collapse.
   Loaded with defer.
*/
(function () {
    document.addEventListener('DOMContentLoaded', function () {
        const toc = document.getElementById('toc');
        if (!toc) return;

        const links = Array.from(toc.querySelectorAll('a[data-target]'));
        if (!links.length) return;

        // Map: heading ID → TOC anchor element.
        const linkMap = new Map(links.map(a => [a.dataset.target, a]));

        // All headings in the body that have a matching TOC entry.
        const headings = Array.from(
            document.querySelectorAll('#markdownBody :is(h1,h2,h3,h4,h5,h6)[id]')
        ).filter(h => linkMap.has(h.id));

        if (!headings.length) return;

        const label       = toc.querySelector('.toc-active-label');
        const toggleBtn   = toc.querySelector('.toc-toggle');
        const mobileBar   = document.getElementById('toc-mobile-bar');
        const mobileLabel = mobileBar && mobileBar.querySelector('.toc-mobile-label');

        const pageTitleEl = document.querySelector('#markdownBody .page-title');
        const pageTitle   = pageTitleEl ? pageTitleEl.textContent.trim() : 'Contents';

        /* The entry for the section being read is marked for assistive
           technology too (aria-current): the class alone was visible only. */
        function mark(id) {
            links.forEach(function (a) {
                const on = a.dataset.target === id;
                a.classList.toggle('is-active', on);
                if (on) a.setAttribute('aria-current', 'true');
                else a.removeAttribute('aria-current');
            });
        }

        function activateTitle() {
            mark(null);
            if (label) label.textContent = pageTitle;
            if (mobileLabel) mobileLabel.textContent = pageTitle;
        }

        function activate(id) {
            mark(id);
            const activeLink = linkMap.get(id);
            /* data-label is the heading as plain text: a link's own text
               would carry KaTeX's markup for any math in it. */
            const text = activeLink ? (activeLink.dataset.label || activeLink.textContent) : pageTitle;
            if (label) label.textContent = text;
            if (mobileLabel) mobileLabel.textContent = text;
        }

        // Collapse / expand. The collapsed state is hidden from
        // assistive technology (aria-hidden="true") and removed from
        // the keyboard tab order (tabindex=-1 on each link), so users
        // navigating with a screen reader or only the keyboard cannot
        // land on links inside a collapsed TOC.
        const tocNav = toc.querySelector('.toc-nav');
        function setExpanded(open) {
            if (!toggleBtn) return;
            toc.classList.toggle('is-collapsed', !open);
            toggleBtn.setAttribute('aria-expanded', String(open));
            if (tocNav) {
                tocNav.setAttribute('aria-hidden', String(!open));
            }
            links.forEach(function (a) {
                if (open) {
                    a.removeAttribute('tabindex');
                } else {
                    a.setAttribute('tabindex', '-1');
                }
            });
        }

        setExpanded(true);

        if (toggleBtn) {
            toggleBtn.addEventListener('click', function () {
                setExpanded(toc.classList.contains('is-collapsed'));
            });
        }


        // Progress indicator — drives the horizontal bar under .toc-header
        // and the mobile bar's progress line.
        function updateProgress() {
            const scrollable = document.documentElement.scrollHeight - window.innerHeight;
            const progress = scrollable > 0 ? Math.min(1, window.scrollY / scrollable) : 0;
            toc.style.setProperty('--toc-progress', progress);
            if (mobileBar) mobileBar.style.setProperty('--toc-progress', progress);
        }
        window.addEventListener('scroll', updateProgress, { passive: true });
        updateProgress();

        // The set of headings currently intersecting the trigger band.
        const visible = new Set();

        const observer = new IntersectionObserver(function (entries) {
            entries.forEach(function (e) {
                if (e.isIntersecting) visible.add(e.target);
                else visible.delete(e.target);
            });

            if (visible.size > 0) {
                // Activate the topmost visible heading in document order.
                const top = headings.find(h => visible.has(h));
                if (top) { activate(top.id); }
            } else {
                // Nothing in the trigger band: activate the last heading
                // whose top edge is above the sticky nav bar.
                const navHeight = (document.querySelector('header') || {}).offsetHeight || 0;
                let candidate = null;
                for (const h of headings) {
                    if (h.getBoundingClientRect().top < navHeight + 16) candidate = h;
                    else break;
                }
                if (candidate) { activate(candidate.id); }
                else activateTitle();
            }
        }, {
            // Trigger band: a strip from 10% to 15% down from the top of the
            // viewport. Headings become active when they enter this band.
            rootMargin: '-10% 0px -85% 0px',
            threshold: 0,
        });

        headings.forEach(h => observer.observe(h));

        // Set initial active state based on URL hash, else first heading.
        const hash = window.location.hash.slice(1);
        if (hash && linkMap.has(hash)) {
            activate(hash);
        } else {
            activateTitle();
        }
    });
})();
