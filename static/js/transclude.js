/* transclude.js — Client-side lazy transclusion.
 *
 * Authored in Markdown as a standalone line:
 *   {{slug}}              — embed full body of /slug.html
 *   {{slug#section-id}}   — embed one section by heading id
 *   {{path/to/page}}      — sub-path pages work the same way
 *
 * The Haskell preprocessor (Filters.Transclusion) converts these at build
 * time to placeholder divs:
 *   <div class="transclude" data-src="/slug.html"
 *        data-section="section-id"></div>
 *
 * This script finds those divs, fetches the target page, extracts the
 * requested content, rewrites cross-page fragment hrefs, injects the
 * content inline, and retriggers layout-dependent JS (sidenotes, collapse).
 */

(function () {
    'use strict';

    /* Shared fetch cache — one network request per URL regardless of how
     * many transclusions reference the same page. */
    var cache = {};

    function fetchPage(url) {
        if (!cache[url]) {
            cache[url] = fetch(url).then(function (r) {
                if (!r.ok) throw new Error('HTTP ' + r.status);
                return r.text();
            });
        }
        return cache[url];
    }

    function parseDoc(html) {
        return new DOMParser().parseFromString(html, 'text/html');
    }

    /* Extract a named section: the heading element with id=sectionId plus
     * all following siblings until the next heading at the same or higher
     * level (lower number), or end of parent. */
    function extractSection(doc, sectionId) {
        var anchor = doc.getElementById(sectionId);
        if (!anchor) return null;

        var level = parseInt(anchor.tagName[1], 10);
        if (!level) return null;

        var nodes = [anchor.cloneNode(true)];
        var el    = anchor.nextElementSibling;

        while (el) {
            if (/^H[1-6]$/.test(el.tagName) &&
                parseInt(el.tagName[1], 10) <= level) break;
            nodes.push(el.cloneNode(true));
            el = el.nextElementSibling;
        }

        return nodes.length ? nodes : null;
    }

    /* Extract the full contents of #markdownBody. */
    function extractBody(doc) {
        var body = doc.getElementById('markdownBody');
        if (!body) return null;
        var nodes = Array.from(body.children).map(function (el) {
            return el.cloneNode(true);
        });
        return nodes.length ? nodes : null;
    }

    /* Rewrite href="#fragment" → href="srcUrl#fragment" so in-page anchor
     * links from the source page remain valid when embedded elsewhere. */
    function rewriteFragmentHrefs(nodes, srcUrl) {
        nodes.forEach(function (node) {
            node.querySelectorAll('a[href^="#"]').forEach(function (a) {
                a.setAttribute('href', srcUrl + a.getAttribute('href'));
            });
        });
    }

    /* After injection, retrigger layout-dependent subsystems. */
    function reinitFragment(container) {
        /* sidenotes.js — wire newly injected sidenote refs/spans and
           reposition the column. Falls back to a manual resize event
           for older builds that haven't been redeployed yet. */
        if (typeof window.reinitSidenotes === 'function') {
            window.reinitSidenotes(container);
        } else {
            window.dispatchEvent(new Event('resize'));
        }

        /* popups.js — bind hover popups for newly injected links so
           transcluded content has the same preview behaviour as the
           host page. */
        if (typeof window.reinitPopups === 'function') {
            window.reinitPopups(container);
        }

        /* collapse.js exposes reinitCollapse for newly added headings. */
        if (typeof window.reinitCollapse === 'function') {
            window.reinitCollapse(container);
        }

        /* gallery.js can expose reinitGallery when needed. */
        if (typeof window.reinitGallery === 'function') {
            window.reinitGallery(container);
        }
    }

    function loadTransclusion(el) {
        var src     = el.dataset.src;
        var section = el.dataset.section || null;
        if (!src) return;

        el.classList.add('transclude--loading');

        fetchPage(src)
            .then(function (html) {
                var doc   = parseDoc(html);
                var nodes = section
                    ? extractSection(doc, section)
                    : extractBody(doc);

                if (!nodes) {
                    el.classList.replace('transclude--loading', 'transclude--error');
                    el.textContent = '[transclusion not found: '
                        + src + (section ? '#' + section : '') + ']';
                    return;
                }

                rewriteFragmentHrefs(nodes, src);

                var wrapper = document.createElement('div');
                wrapper.className = 'transclude--content';
                nodes.forEach(function (n) { wrapper.appendChild(n); });

                el.classList.replace('transclude--loading', 'transclude--loaded');
                el.appendChild(wrapper);

                reinitFragment(el);
            })
            .catch(function (err) {
                el.classList.replace('transclude--loading', 'transclude--error');
                console.warn('transclude: failed to load', src, err);
            });
    }

    document.addEventListener('DOMContentLoaded', function () {
        document.querySelectorAll('div.transclude').forEach(loadTransclusion);
    });
}());
