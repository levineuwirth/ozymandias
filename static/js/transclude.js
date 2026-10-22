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
 * requested content, rebases its URLs and identifiers onto the source
 * document, injects the content inline, and runs the shared enhancement
 * entry point over it.
 *
 * URL and identifier scope (F07)
 * ------------------------------
 * Transcluded content came from another document, so every URL in it is
 * relative to *that* document and every id in it already exists once on
 * the web. Before injection:
 *   · every URL-bearing attribute (href, src, srcset, poster, data-src,
 *     data-srcset, cite, action, formaction, object[data], SVG
 *     xlink:href, …) is resolved against the source document's URL;
 *   · every id in the fragment is namespaced with a per-transclusion
 *     prefix, and every local reference to those ids — fragment links,
 *     label[for], aria-*, headers, list, form, SVG url(#…) — is rewritten
 *     to match;
 *   · a fragment link whose target is *not* inside the extracted fragment
 *     still points at the source page (srcUrl#fragment), as before.
 *
 * Enhancement contract (F07, smaller finding 7)
 * ---------------------------------------------
 * `window.lnEnhance(container[, {force: true, source: url}])` is the one
 * idempotent entry point for content inserted into the page after load.
 * It calls the known reinitialisers (sidenotes, popups, collapse) and
 * then dispatches a bubbling `ln:content-added` CustomEvent
 * on the container, with `detail = {container, source}`. Any subsystem
 * that needs to see injected content — KaTeX rendering, code-copy
 * buttons, annotations, lightbox — should listen for that event rather
 * than growing its own ad hoc hook:
 *
 *     document.addEventListener('ln:content-added', function (e) {
 *         myInit(e.detail.container);
 *     });
 *
 * Calling lnEnhance twice on the same container is a no-op unless
 * `force` is passed, so listeners may assume one call per insertion.
 *
 * gallery.js does not listen yet: a transcluded display equation or score
 * fragment gets no expand button. No page transcludes either today.
 */

(function () {
    'use strict';

    /* Shared fetch cache — one network request per URL regardless of how
     * many transclusions reference the same page. A failure is not kept:
     * it was, for the life of the page, so one dropped request failed
     * every later transclusion of that URL. */
    var cache = {};

    function fetchPage(url) {
        if (!cache[url]) {
            cache[url] = fetch(url).then(function (r) {
                if (!r.ok) throw new Error('HTTP ' + r.status);
                return r.text();
            });
            cache[url].catch(function () { delete cache[url]; });
        }
        return cache[url];
    }

    function parseDoc(html) {
        return new DOMParser().parseFromString(html, 'text/html');
    }

    /* Extract a named section: the heading element with id=sectionId plus
     * all following siblings until the next heading at the same or higher
     * level (lower number), the page's notes list, or end of parent. */
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
            if (el.matches('section.footnotes')) break;
            nodes.push(el.cloneNode(true));
            el = el.nextElementSibling;
        }

        var notes = sectionNotes(doc, nodes);
        if (notes) nodes.push(notes);

        return nodes.length ? nodes : null;
    }

    /* A section's notes are not beside it: they are items in the list at
     * the end of the source page. Bring the ones its references name, in
     * a list of their own, before ids are scoped — the references and the
     * notes' back-links are then renamed together, and sidenotes.js pairs
     * them as on the source page. */
    function sectionNotes(doc, nodes) {
        var list = null;
        nodes.forEach(function (node) {
            node.querySelectorAll('.sidenote-ref a[href^="#"]').forEach(function (a) {
                var id;
                try { id = decodeURIComponent(a.getAttribute('href').slice(1)); }
                catch (e) { return; }
                var note = doc.getElementById(id);
                if (!note || !note.classList.contains('footnote-item')) return;
                if (!list) {
                    list = doc.createElement('ol');
                    list.className = 'footnotes-list';
                }
                list.appendChild(note.cloneNode(true));
            });
        });
        if (!list) return null;
        var section = doc.createElement('section');
        section.className = 'footnotes';
        section.setAttribute('role', 'doc-endnotes');
        section.appendChild(list);
        return section;
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

    /* ------------------------------------------------------------------
       URL rebasing
    ------------------------------------------------------------------ */

    /* Absolute URL, protocol-relative URL, or a non-http scheme
     * (mailto:, data:, tel:, javascript:) — leave untouched. */
    var HAS_SCHEME = /^(?:[a-z][a-z0-9+.\-]*:|\/\/)/i;

    /* Plain single-URL attributes, by attribute name. */
    var URL_ATTRS = ['href', 'src', 'poster', 'data-src', 'data-href',
                     'data-original', 'cite', 'action', 'formaction',
                     'longdesc', 'xlink:href'];
    /* Comma-separated candidate lists. */
    var SRCSET_ATTRS = ['srcset', 'data-srcset', 'imagesrcset'];
    /* <object data="…"> only — `data` on anything else is a stray. */
    var DATA_ATTR_TAGS = { OBJECT: true };

    function absolutise(value, base) {
        if (!value) return value;
        var v = value.trim();
        if (!v || v.charAt(0) === '#') return value;   /* fragments handled separately */
        if (HAS_SCHEME.test(v)) return value;
        try {
            var u = new URL(v, base);
            return (u.origin === window.location.origin)
                ? (u.pathname + u.search + u.hash)
                : u.href;
        } catch (e) {
            return value;
        }
    }

    function absolutiseSrcset(value, base) {
        /* data: URIs contain commas; do not attempt to split those. */
        if (!value || value.indexOf('data:') !== -1) return value;
        var parts = value.split(',');
        var out   = [];
        for (var i = 0; i < parts.length; i++) {
            var s = parts[i].trim();
            if (!s) continue;
            var bits = s.split(/\s+/);
            bits[0] = absolutise(bits[0], base);
            out.push(bits.join(' '));
        }
        return out.join(', ');
    }

    /* Every element in the fragment, roots included. */
    function eachElement(nodes, fn) {
        nodes.forEach(function (node) {
            if (node.nodeType !== 1) return;
            fn(node);
            node.querySelectorAll('*').forEach(fn);
        });
    }

    function rebaseUrls(nodes, base) {
        eachElement(nodes, function (el) {
            URL_ATTRS.forEach(function (name) {
                if (!el.hasAttribute(name)) return;
                var v = el.getAttribute(name);
                var next = absolutise(v, base);
                if (next !== v) el.setAttribute(name, next);
            });
            SRCSET_ATTRS.forEach(function (name) {
                if (!el.hasAttribute(name)) return;
                var v = el.getAttribute(name);
                var next = absolutiseSrcset(v, base);
                if (next !== v) el.setAttribute(name, next);
            });
            if (DATA_ATTR_TAGS[el.tagName] && el.hasAttribute('data')) {
                el.setAttribute('data', absolutise(el.getAttribute('data'), base));
            }
        });
    }

    /* ------------------------------------------------------------------
       Identifier scoping
    ------------------------------------------------------------------ */

    var instanceCounter = 0;

    /* Attributes whose value is one or more id references. */
    var IDREF_ATTRS = ['for', 'form', 'list', 'headers', 'aria-labelledby',
                       'aria-describedby', 'aria-controls', 'aria-owns',
                       'aria-activedescendant', 'aria-details',
                       'aria-errormessage', 'aria-flowto'];
    /* Attributes that may hold a local url(#id) functional reference. */
    var FUNCIRI_ATTRS = ['fill', 'stroke', 'filter', 'clip-path', 'mask',
                         'marker-start', 'marker-mid', 'marker-end', 'style'];

    /* Rename every id in the fragment; return oldId → newId. */
    function namespaceIds(nodes, prefix) {
        var map = {};
        eachElement(nodes, function (el) {
            var id = el.getAttribute('id');
            if (!id) return;
            var next = prefix + id;
            map[id] = next;
            el.setAttribute('id', next);
        });
        return map;
    }

    /* Rewrite local references: fragment links to renamed targets point at
     * the copy in this page; fragment links to anything else keep pointing
     * at the source document. */
    function rewriteReferences(nodes, map, srcUrl) {
        eachElement(nodes, function (el) {
            ['href', 'xlink:href'].forEach(function (name) {
                if (!el.hasAttribute(name)) return;
                var v = el.getAttribute(name);
                if (!v || v.charAt(0) !== '#') return;
                var target = v.slice(1);
                el.setAttribute(name, map.hasOwnProperty(target)
                    ? '#' + map[target]
                    : srcUrl + v);
            });

            IDREF_ATTRS.forEach(function (name) {
                if (!el.hasAttribute(name)) return;
                var tokens = el.getAttribute(name).split(/\s+/).filter(Boolean);
                if (!tokens.length) return;
                var changed = false;
                var next = tokens.map(function (t) {
                    if (!map.hasOwnProperty(t)) return t;
                    changed = true;
                    return map[t];
                });
                if (changed) el.setAttribute(name, next.join(' '));
            });

            FUNCIRI_ATTRS.forEach(function (name) {
                if (!el.hasAttribute(name)) return;
                var v = el.getAttribute(name);
                if (v.indexOf('url(') === -1) return;
                var next = v.replace(/url\(\s*(['"]?)#([^)'"\s]+)\1\s*\)/g,
                    function (whole, quote, id) {
                        return map.hasOwnProperty(id)
                            ? 'url(' + quote + '#' + map[id] + quote + ')'
                            : whole;
                    });
                if (next !== v) el.setAttribute(name, next);
            });
        });
    }

    /* One pass over an extracted fragment: rebase URLs, scope ids, fix
     * every reference to them. Order matters — ids must be renamed before
     * references to them are resolved. */
    function scopeFragment(nodes, srcUrl) {
        var base = srcUrl;
        try { base = new URL(srcUrl, document.baseURI).href; } catch (e) {}
        rebaseUrls(nodes, base);
        var map = namespaceIds(nodes, 'tx' + (++instanceCounter) + '-');
        rewriteReferences(nodes, map, srcUrl);
    }

    /* ------------------------------------------------------------------
       Shared enhancement entry point
    ------------------------------------------------------------------ */

    var CONTENT_EVENT = 'ln:content-added';

    function enhance(container, opts) {
        if (!container || container.nodeType !== 1) return;
        opts = opts || {};
        if (container.dataset.lnEnhanced === '1' && !opts.force) return;
        container.dataset.lnEnhanced = '1';

        /* sidenotes.js — pair newly injected note references with their
           notes and reposition the margin. A page that transcludes always loads it
           (has-sidenotes counts transclusion markers). */
        if (typeof window.reinitSidenotes === 'function') {
            window.reinitSidenotes(container);
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

        /* Everything else (math, code-copy, annotations, lightbox) joins
           in by listening for this event. */
        container.dispatchEvent(new CustomEvent(CONTENT_EVENT, {
            bubbles: true,
            detail: { container: container, source: opts.source || null }
        }));

        ensureMath(container);
        highlightCode(container);
    }

    function ensureStyle(href) {
        var present = Array.from(document.querySelectorAll('link[rel="stylesheet"]'))
            .some(function (el) {
                return new URL(el.href, document.baseURI).pathname === href;
            });
        if (present) return;
        var css = document.createElement('link');
        css.rel = 'stylesheet';
        css.href = href;
        document.head.appendChild(css);
    }

    function loadScript(src) {
        return new Promise(function (resolve, reject) {
            var script = document.createElement('script');
            script.src = src;
            script.onload = resolve;
            script.onerror = function () {
                script.remove();
                reject(new Error('Could not load ' + src));
            };
            document.head.appendChild(script);
        });
    }

    /* Code may arrive in a source popup or a transclusion on a page that
       did not need Prism at first. Share one download, then enhance each
       fragment (including a detached popup before it is shown). */
    var codeLoading = null;
    function highlightCode(container) {
        var selector = 'code[class*="language-"], code[class*="lang-"]';
        var codes = Array.from(container.querySelectorAll(selector));
        if (container.matches && container.matches(selector)) codes.unshift(container);
        if (!codes.length) return Promise.resolve();
        ensureStyle('/css/syntax.css');
        if (!codeLoading) codeLoading = window.Prism
            ? Promise.resolve() : loadScript('/js/prism.min.js');
        return codeLoading.then(function () {
            codes.forEach(function (code) {
                var lang = /(?:^|\s)lang(?:uage)?-([\w-]+)/i.exec(code.className);
                if (lang && window.Prism && Prism.languages[lang[1]]) {
                    Prism.highlightElement(code);
                }
            });
        }).catch(function () {
            codeLoading = null; // plain code remains readable; the next use retries
        });
    }
    window.lnHighlightCode = highlightCode;

    /* KaTeX loads only on pages whose own body has math (audit X6). A
       transclusion can bring math to a page that has none: load it then,
       once. katex-bootstrap.js renders the whole document when it runs,
       skipping anything already typeset. */
    var mathLoading = null;
    function ensureMath(container) {
        if (mathLoading || typeof window.renderMath === 'function') return;
        if (!container.querySelector('.math') && !container.classList.contains('math')) return;
        ensureStyle('/katex/katex.min.css');
        mathLoading = (window.katex ? Promise.resolve() : loadScript('/katex/katex.min.js'))
            .then(function () { return loadScript('/js/katex-bootstrap.js'); })
            .catch(function () { mathLoading = null; });
    }

    window.lnEnhance = enhance;

    /* ------------------------------------------------------------------
       Failure states
    ------------------------------------------------------------------ */

    /* A failed transclusion used to leave an empty element, so the reader
     * got a silent hole (F06). Say what happened and link to the source. */
    function showError(el, message, src, section) {
        el.classList.remove('transclude--loading');
        el.classList.add('transclude--error');
        el.textContent = '';

        var p = document.createElement('p');
        p.className = 'transclude-error-note';
        p.appendChild(document.createTextNode(message + ' '));

        if (src) {
            var a = document.createElement('a');
            a.className = 'transclude-error-link';
            a.href = src + (section ? '#' + section : '');
            a.textContent = 'Read it on its own page';
            p.appendChild(a);
            p.appendChild(document.createTextNode('.'));
        }
        el.appendChild(p);
    }

    /* ------------------------------------------------------------------
       Loading
    ------------------------------------------------------------------ */

    /* Nested transclusion limits: ancestors carries the chain of srcs
     * currently being expanded (cycle guard — a self-transcluding page
     * must not loop), and MAX_DEPTH caps pathological nesting. */
    var MAX_DEPTH = 3;

    function loadTransclusion(el, depth, ancestors) {
        depth     = depth     || 0;
        ancestors = ancestors || [];

        var src     = el.dataset.src;
        var section = el.dataset.section || null;
        if (!src) return;

        if (depth >= MAX_DEPTH || ancestors.indexOf(src) !== -1) {
            showError(el, 'This section is not expanded here (it would repeat, '
                + 'or nest too deeply).', src, section);
            return;
        }

        el.classList.add('transclude--loading');

        fetchPage(src)
            .then(function (html) {
                var doc   = parseDoc(html);
                var nodes = section
                    ? extractSection(doc, section)
                    : extractBody(doc);

                if (!nodes) {
                    showError(el, 'That section could not be found on the source page.',
                        src, section);
                    return;
                }

                scopeFragment(nodes, src);

                var wrapper = document.createElement('div');
                wrapper.className = 'transclude--content';
                nodes.forEach(function (n) { wrapper.appendChild(n); });

                el.classList.replace('transclude--loading', 'transclude--loaded');
                el.appendChild(wrapper);

                /* The fetched page may itself contain transclusion
                   placeholders — process them too, extending the
                   ancestor chain for cycle/depth guarding. */
                var chain = ancestors.concat(src);
                wrapper.querySelectorAll('div.transclude').forEach(function (nested) {
                    loadTransclusion(nested, depth + 1, chain);
                });

                enhance(el, { source: src });
            })
            .catch(function (err) {
                showError(el, 'This passage could not be loaded.', src, section);
                console.warn('transclude: failed to load', src, err);
            });
    }

    document.addEventListener('DOMContentLoaded', function () {
        document.querySelectorAll('div.transclude').forEach(function (el) {
            loadTransclusion(el);
        });
    });
}());
