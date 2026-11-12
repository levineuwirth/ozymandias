/* popups.js — Hover preview popups.
   Content providers (in dispatch priority order):
     1.  Local annotations  — /data/annotations.json (any URL, author-defined)
     2.  Citations          — DOM lookup, cite-link[href^="#ref-"]
     3.  Internal pages     — same-origin fetch, title + authors + tags + abstract + stats
     4.  Wikipedia          — MediaWiki action API, full lead section
     5.  arXiv              — export.arxiv.org Atom API
     6.  DOI / CrossRef     — api.crossref.org, title/authors/abstract
     7.  GitHub             — code links (blob/tree/commit): build-time
                              snapshots under /code-refs/ (codeRefContent);
                              bare repo links: api.github.com description + stars
     8.  Open Library       — openlibrary.org JSON API, book description
     9.  bioRxiv / medRxiv  — api.biorxiv.org, abstract
     10. YouTube            — oEmbed, title + channel (no key required)
     11. Internet Archive   — archive.org/metadata, title + description
     12. PubMed             — NCBI esummary, title + authors + journal
     13. MTG Wiki           — MediaWiki action API, article introduction
     14. Scryfall           — card image, type and Oracle rules text

   A site asks only the providers its site.yaml lists in popup-providers
   (ozySite.popupProviders, 4 to 14 above); local annotations, citations and
   internal pages need no listing. What other switches bring (the epistemic
   block and its terms, source files, GitHub snapshots, PDFs, the PGP
   signature) are modules, static/js/popups/*.js, served after this file
   in the same one by the site's switches (EngineFiles.scriptModules); they
   bind through window.ozyPopups, as a site's own script may ("Binding",
   below).

   For those it lists, production nginx CSP must add to connect-src:
     https://*.wikipedia.org  https://api.crossref.org
     https://api.github.com   https://openlibrary.org
     https://api.biorxiv.org  https://www.youtube.com
     https://mtg.wiki         https://api.scryfall.com
   Scryfall card images also need https://cards.scryfall.io in img-src.
   (The wildcard covers per-language Wikipedias: en, es, fr, simple,
    zh-yue, be-tarask, …  — the popup picks the host from the link URL.)

   Production nginx must also reverse-proxy three CORS-broken upstreams
   (immutable metadata — long cache TTL is safe). See nginx/popup-proxy.conf.
     /proxy/arxiv/    -> https://export.arxiv.org/
     /proxy/archive/  -> https://archive.org/
     /proxy/pubmed/   -> https://eutils.ncbi.nlm.nih.gov/
*/
(function () {
    'use strict';

    var SHOW_DELAY = 250;
    var HIDE_DELAY = 150;

    var popup        = null;
    var showTimer    = null;
    var hideTimer    = null;
    var activeTarget = null;
    var returning    = false;   /* focus going back to a link on Escape: not a new visit */
    var cache        = Object.create(null);   /* url → html; only successful results stored */
    var annotations  = null;                  /* null = not yet loaded */

    /* ------------------------------------------------------------------
       Init — load annotations first, then bind all targets
    ------------------------------------------------------------------ */

    function init() {
        // Hover popups are meaningless on touch-primary devices and interfere
        // with tap navigation (first tap = hover, second tap = follow link).
        if (window.matchMedia('(hover: none) and (pointer: coarse)').matches) return;

        popup = document.createElement('div');
        popup.className = 'link-popup';
        popup.setAttribute('aria-live', 'polite');
        popup.setAttribute('aria-hidden', 'true');
        /* Inert while hidden. The popup sits last in <body>, just after
           the footer, so Tab from the footer's last link (the signature,
           whose focus opens its popup) went into it, and Firefox makes its
           scrollable <pre> a stop: the popup then hid around the focus. */
        popup.inert = true;
        document.body.appendChild(popup);

        popup.addEventListener('mouseenter', cancelHide);
        popup.addEventListener('mouseleave', scheduleHide);
        /* Focus inside keeps it open, as the pointer does. */
        popup.addEventListener('focusin', cancelHide);
        popup.addEventListener('focusout', function (e) {
            if (!popup.contains(e.relatedTarget)) scheduleHide();
        });

        /* Escape dismisses the popup at once, and a pending one with it;
           it stays away until the pointer or focus reaches a target anew
           (WCAG 1.4.13, audit A03). Sidenotes and the selection popup
           already did this. Other Escape handlers still see the key. */
        document.addEventListener('keydown', function (e) {
            if (e.key !== 'Escape' || !activeTarget) return;
            /* From inside the popup, back to the link that opened it, without
               that focus opening it again (it reappeared after the show
               delay). */
            var back = popup.contains(document.activeElement) ? activeTarget : null;
            hideNow();
            if (back) {
                returning = true;
                back.focus();
                returning = false;
            }
        });

        loadAnnotations().then(function () {
            bindTargets(document.body);
        });
    }

    /* ------------------------------------------------------------------
       Binding: which provider an element gets

       Each binder is a selector and what to show for an element it finds,
       at an order. bindTargets runs them lowest first, and an element
       keeps the first provider bound to it (bind()), so a specific kind
       claims its links before the generic internal and external ones. A
       source reference with a same-origin href (../source/data/x.json)
       used to be claimed as an internal page, which showed nothing (audit
       J08). The engine's own binders are here; the rest come with their
       switches, served after this file in the same one
       (static/js/popups/*.js, EngineFiles.jsModules), and a site's script
       may add its own through window.ozyPopups before the page has loaded
       (contract.yaml's scripts):

          10  epistemic term definitions  [data-ep-term]        epistemic
          20  citation markers            a.cite-link           here
          30  the epistemic jump link     a[href="#epistemic"]  epistemic
          40  source-file references      a.source-ref          source-refs
          50  GitHub code references      a[data-code-ref]      code-refs
          60  PDF links                   a.pdf-link            pdf-viewer
          70  the PGP signature link      a.footer-sig-link     signature-link
          80  internal links              same-origin hrefs     here
          90  external links              PROVIDERS             here
         100  dates                       [data-date-start]     here
         110  item-card revision markers  .item-card-revised    here
    ------------------------------------------------------------------ */

    var BINDERS  = [];   /* {order, selector, pick(el): a provider, or null} */
    var EXCLUDED = [];   /* containers whose internal links are their own preview */
    var SHOWN    = [];   /* each called with the popup once it is shown and placed */

    function addBinder(order, selector, pick) {
        BINDERS.push({order: order, selector: selector, pick: pick});
        /* Stable: binders of one order keep the order they came in. */
        BINDERS.sort(function (a, b) { return a.order - b.order; });
    }

    window.ozyPopups = {
        /* `content(target)`, a Promise of HTML, a Node, or null for no
           popup, for the elements `selector` finds, at `order`. */
        bind: function (order, selector, content) {
            addBinder(order, selector, function () { return content; });
        },
        /* Internal links inside `selector` get no preview: what is there
           is its own (a photograph's card, a score's spine). */
        exclude: function (selector) { EXCLUDED.push(selector); },
        /* `fn(popup)` once a popup is shown and placed, for what needs its
           layout (the source popup's line band). */
        onShow: function (fn) { SHOWN.push(fn); },
        truncate: function (s, limit) { return truncate(s, limit); }
    };

    /* Citation markers */
    addBinder(20, 'a.cite-link[href^="#ref-"]', function () { return citationContent; });

    /* Internal links — absolute (/foo) and relative (../../foo) same-origin hrefs.
       relativizeUrls in Hakyll makes index-page links relative, so we must match both. */
    addBinder(80, 'a[href^="/"], a[href^="./"], a[href^="../"]', function (el) {
        for (var i = 0; i < EXCLUDED.length; i++) {
            if (el.closest(EXCLUDED[i])) return null;
        }

        /* Author links, backlink source links, and Related items always get popups */
        var inAuthors  = el.closest('.meta-authors');
        var isBacklink = el.classList.contains('backlink-source');
        var isSimilar  = el.classList.contains('similar-link');
        /* PDF-typed Related items are handled below by the pdf-link binder */
        if (isSimilar && el.classList.contains('pdf-link')) return null;
        if (!inAuthors && !isBacklink && !isSimilar) {
            if (el.closest('nav, #toc, footer, .page-meta-footer, .metadata')) return null;
            if (el.classList.contains('cite-link') || el.classList.contains('meta-tag')) return null;
            if (el.classList.contains('pdf-link')) return null;
            if (el.classList.contains('content-divider-logo') || el.classList.contains('aftermatter-logo')) return null;
        }
        return internalContent;
    });

    /* External links — single dispatcher handles all providers */
    addBinder(90, 'a[href^="http"]', function (el) {
        if (el.closest('nav, #toc, footer, .page-meta-footer')) return null;
        return getProvider(el.getAttribute('href') || '');
    });

    /* Date hover popups — any element tagged with data-date-start.
       Handles the frontmatter range link, version-history list items,
       last-reviewed in the epistemic block, blog post dates, etc. */
    addBinder(100, '[data-date-start]', function () { return dateContent; });

    /* Item-card revision markers (/new, library, tag pages). The card
       gutter says only "Revised"; the dates and the note live here. */
    addBinder(110, '.item-card-revised', function () { return revisionContent; });

    function bindTargets(root) {
        BINDERS.forEach(function (b) {
            root.querySelectorAll(b.selector).forEach(function (el) {
                var provider = b.pick(el);
                if (provider) bind(el, provider);
            });
        });
    }

    /* Public re-init hook used by window.ozyEnhance (utils.js) after new
       content enters the DOM. Idempotent — bind() marks each element so
       repeated calls don't stack listeners. */
    window.reinitPopups = function (container) {
        if (!popup) return;          /* init() not yet run / touch device */
        if (annotations === null) {
            loadAnnotations().then(function () { bindTargets(container || document.body); });
        } else {
            bindTargets(container || document.body);
        }
    };

    /* The link's hostname, lowercased by URL itself; '' for an href that
       does not parse. */
    function hostOf(href) {
        try { return new URL(href, location.href).hostname; }
        catch (e) { return ''; }
    }

    /* Returns the appropriate provider function for a given URL, or null.
       Local annotations win over the table; otherwise the first entry in
       PROVIDERS whose `host` regex matches the link's hostname and whose
       `match` regex hits the href is selected, among those the site lists.
       The host comes first, as for the link icons (build/Filters/Links.hs):
       `match` alone is unanchored, and took gist.github.com/user/id for a
       repository and a path containing doi.org/10.… for a DOI. */
    function getProvider(href) {
        if (!href) return null;
        if (annotations && annotations[href]) return annotationContent;
        var host = hostOf(href);
        for (var i = 0; i < PROVIDERS.length; i++) {
            if (window.ozySite.popupProviders.indexOf(PROVIDERS[i].name) !== -1
                && PROVIDERS[i].host.test(host) && PROVIDERS[i].match.test(href)) {
                var entry = PROVIDERS[i];
                return function (target) { return providerContent(target, entry); };
            }
        }
        return null;
    }

    function bind(el, provider) {
        /* Idempotent: skip elements that already have a popup binding,
           so reinitPopups() called from ozyEnhance cannot stack
           listeners on already-bound nodes. */
        if (el.dataset.popupBound === '1') return;
        el.dataset.popupBound = '1';
        el.addEventListener('mouseenter', function () {
            /* Pointing elsewhere does not replace a popup the keyboard is in. */
            if (!popup.contains(document.activeElement)) scheduleShow(el, provider);
        });
        el.addEventListener('mouseleave', scheduleHide);
        el.addEventListener('focus',      function () { if (!returning) scheduleShow(el, provider); });
        el.addEventListener('blur',       scheduleHide);
    }

    /* ------------------------------------------------------------------
       Lifecycle
    ------------------------------------------------------------------ */

    function scheduleShow(target, provider) {
        cancelHide();
        clearTimeout(showTimer);
        activeTarget = target;
        showTimer = setTimeout(function () {
            provider(target).then(function (content) {
                if (!content || activeTarget !== target) return;
                /* Providers may return either an HTML string or a DOM
                   Node — the latter is used by epistemicContent so the
                   popup receives cloned nodes instead of a re-parsed
                   HTML round-trip. Handle both forms. */
                popup.innerHTML = '';
                if (typeof content === 'string') {
                    popup.innerHTML = content;
                } else if (content instanceof Node) {
                    popup.appendChild(content);
                } else {
                    return;
                }
                /* Reference popups (provider-rendered: arXiv, Wikipedia,
                   …) get the larger glanceable layout; internal page
                   previews keep the compact one. Toggled per show since
                   the popup element is shared. */
                popup.classList.toggle('link-popup--rich',
                    !!popup.querySelector('.popup-provider'));
                positionPopup(target);
                SHOWN.forEach(function (fn) { fn(popup); });
                popup.classList.add('is-visible');
                popup.setAttribute('aria-hidden', 'false');
                popup.inert = false;
                /* Images with width/height attrs reserve their space
                   before load; one without them grows the popup after
                   positioning and can push it past the viewport edge.
                   Re-clamp when such an image arrives. */
                popup.querySelectorAll('img:not([height])').forEach(function (im) {
                    if (im.complete) return;
                    im.addEventListener('load', function () {
                        if (activeTarget === target &&
                            popup.classList.contains('is-visible')) {
                            positionPopup(target);
                        }
                    }, { once: true });
                });
            }).catch(function () { /* silently fail */ });
        }, SHOW_DELAY);
    }

    /* Shown until hover *and* focus are gone (WCAG 1.4.13): not hidden
       while focus is inside the popup, nor while the keyboard is on the
       link that opened it. The pointer leaving used to hide a popup the
       keyboard was reading, inert around its focus. A link focused by a
       click is not :focus-visible, and lets its popup go. */
    function stillWanted() {
        var a = document.activeElement;
        if (!a || !activeTarget) return false;
        if (popup.contains(a)) return true;
        try { return a === activeTarget && a.matches(':focus-visible'); }
        catch (_) { return false; }
    }

    function scheduleHide() {
        clearTimeout(showTimer);
        clearTimeout(hideTimer);
        hideTimer = setTimeout(function () {
            if (!stillWanted()) hideNow();
        }, HIDE_DELAY);
    }

    /* A provider still loading finds activeTarget changed and shows nothing. */
    function hideNow() {
        clearTimeout(showTimer);
        clearTimeout(hideTimer);
        popup.classList.remove('is-visible');
        popup.setAttribute('aria-hidden', 'true');
        popup.inert = true;
        activeTarget = null;
    }

    function cancelHide() { clearTimeout(hideTimer); }

    /* ------------------------------------------------------------------
       Positioning — centres below target, flips above if clipped
    ------------------------------------------------------------------ */

    function positionPopup(target) {
        var rect = target.getBoundingClientRect();
        var pw   = popup.offsetWidth;
        var ph   = popup.offsetHeight;
        var vw   = window.innerWidth;
        var vh   = window.innerHeight;
        var sy   = window.scrollY;
        var sx   = window.scrollX;
        var GAP  = 10;

        var left = rect.left + sx + rect.width / 2 - pw / 2;
        left = Math.max(sx + GAP, Math.min(left, sx + vw - pw - GAP));

        /* Below if it fits, else above if THAT fits, else whichever
           side has more room. The final clamp guarantees the popup
           never extends past either viewport edge — without it, the
           flip-above branch positions tall popups (rich layouts, lead
           figures) above the visible region for targets near the top
           of the screen. CSS caps .link-popup at viewport height
           (same 10px gap), so the clamp always has room to work. */
        var fitsBelow = rect.bottom + GAP + ph <= vh;
        var fitsAbove = rect.top - GAP - ph >= 0;
        var top;
        if (fitsBelow) {
            top = rect.bottom + sy + GAP;
        } else if (fitsAbove) {
            top = rect.top + sy - ph - GAP;
        } else {
            top = (vh - rect.bottom >= rect.top)
                ? rect.bottom + sy + GAP
                : rect.top    + sy - ph - GAP;
        }
        top = Math.max(sy + GAP, Math.min(top, sy + vh - ph - GAP));

        popup.style.left = left + 'px';
        popup.style.top  = top  + 'px';
    }

    /* ------------------------------------------------------------------
       Content providers
    ------------------------------------------------------------------ */

    /* Cross-origin JSON fetch helper.
       Validates Content-Type before parsing so a CORS-enabled endpoint
       cannot return text/html and have it interpreted as JSON. The
       caller's `.catch` still applies if the JSON parse itself fails.

       Mirror helpers exist for text/* (XML/Atom) and HTML responses. */
    function fetchJson(url, init) {
        return fetch(url, init).then(function (r) {
            if (!r.ok) return null;
            var ct = (r.headers.get('content-type') || '').toLowerCase();
            if (ct && !/(?:^|[\s;,])(?:application\/[a-z+.-]*json|text\/json)\b/.test(ct)) {
                return null;
            }
            return r.json();
        });
    }

    function fetchXml(url, init) {
        return fetch(url, init).then(function (r) {
            if (!r.ok) return null;
            var ct = (r.headers.get('content-type') || '').toLowerCase();
            if (ct && !/(?:xml|atom)/.test(ct)) {
                return null;
            }
            return r.text();
        });
    }

    /* 0. Local annotations — synchronous map lookup after eager load */
    function loadAnnotations() {
        if (annotations !== null) return Promise.resolve(annotations);
        return fetch('/data/annotations.json', { credentials: 'same-origin' })
            .then(function (r) { return r.ok ? r.json() : {}; })
            .then(function (data) { annotations = data; return data; })
            .catch(function ()    { annotations = {}; return {}; });
    }

    function annotationContent(target) {
        var href = target.getAttribute('href');
        var ann  = href && annotations && annotations[href];
        if (!ann) return Promise.resolve(null);
        return Promise.resolve(
            '<div class="popup-annotation">'
            + (ann.title      ? '<div class="popup-title">'    + esc(ann.title)      + '</div>' : '')
            + (ann.annotation ? '<div class="popup-abstract">' + esc(ann.annotation) + '</div>' : '')
            + '</div>'
        );
    }

    /* Item-card revision markers — a synchronous read of the card the
       marker sits in. Every piece is already in the markup (the note as a
       visually-hidden <p>, the original date inside the marker itself), so
       nothing is fetched and the popup cannot drift from what the page
       rendered. Text is extracted with textContent and re-escaped, never
       lifted as innerHTML. */
    function revisionContent(target) {
        var card = target.closest('.item-card');
        if (!card) return Promise.resolve(null);

        var dateEl = card.querySelector('.item-card-date');
        var fromEl = target.querySelector('.item-card-revised-from');
        var noteEl = card.querySelector('.item-card-revision-note');

        var on   = dateEl ? dateEl.textContent.trim() : '';
        var from = fromEl ? fromEl.textContent.replace(/^\s*from\s+/i, '').trim() : '';
        var note = noteEl ? noteEl.textContent.trim() : '';

        var meta = on ? 'Revised ' + on + (from ? ' \u00b7 from ' + from : '') : '';
        if (!meta && !note) return Promise.resolve(null);

        return Promise.resolve(
            '<div class="popup-revision">'
            + (meta ? '<div class="popup-meta">'     + esc(meta) + '</div>' : '')
            + (note ? '<div class="popup-abstract">' + esc(note) + '</div>' : '')
            + '</div>'
        );
    }

    /* 1. Citations — synchronous DOM lookup; supports multi-citation groups
          via data-cite-keys (space-separated list of ref-* IDs).
          Returns a DocumentFragment of cloned bibliography entries instead
          of stringifying innerHTML, so a malicious or malformed cite target
          cannot smuggle markup through the popup's innerHTML setter. */
    function citationContent(target) {
        return new Promise(function (resolve) {
            var keysAttr = target.getAttribute('data-cite-keys');
            var ids = keysAttr
                ? keysAttr.trim().split(/\s+/)
                : [(target.getAttribute('href') || '').slice(1)];
            var entries = ids
                .map(function (id) { return document.getElementById(id); })
                .filter(Boolean);
            if (!entries.length) { resolve(null); return; }

            var wrapper = document.createElement('div');
            wrapper.className = 'popup-citation';
            entries.forEach(function (entry) {
                var item = document.createElement('div');
                item.className = 'popup-citation-entry';
                Array.prototype.forEach.call(entry.childNodes, function (n) {
                    item.appendChild(n.cloneNode(true));
                });
                wrapper.appendChild(item);
            });
            resolve(wrapper);
        });
    }

    /* 2. Internal pages — same-origin fetch, rich preview */
    function internalContent(target) {
        /* Resolve relative hrefs (../../foo) to canonical path (/foo) for fetch + cache. */
        var raw  = target.getAttribute('href');
        if (!raw) return Promise.resolve(null);
        var href = new URL(raw, window.location.href).pathname;
        if (cache[href])  return Promise.resolve(cache[href]);

        return fetch(href, { credentials: 'same-origin' })
            .then(function (r) { return r.ok ? r.text() : null; })
            .then(function (text) {
                if (!text) return null;
                var doc     = new DOMParser().parseFromString(text, 'text/html');
                var titleEl = doc.querySelector('h1.page-title');
                if (!titleEl) return null;

                /* Monogram \u2014 only when the source page renders a real
                   authored mark.svg (not the placeholder roundel that
                   the empty-frontmatter slot uses for symmetric layout).
                   Serialised as an outerHTML string and trusted as
                   already-sanitised SVG produced by our own build. */
                var monoEl = doc.querySelector(
                    'figure.frontmatter-mark--monogram:not(.frontmatter-mark--placeholder) svg'
                );
                var mono   = monoEl ? monoEl.outerHTML : '';

                /* Abstract */
                var abstrEl  = doc.querySelector('.meta-description');
                var abstract = abstrEl ? abstrEl.textContent.trim() : '';
                if (abstract.length > 300)
                    abstract = abstract.slice(0, 300).replace(/\s\S+$/, '') + '\u2026';

                /* Authors: linked, or named alone on a site without author pages */
                var authorEls = doc.querySelectorAll('.meta-authors a, .meta-authors .meta-author');
                var authors   = Array.from(authorEls).map(function (a) {
                    return a.textContent.trim();
                }).join(', ');

                /* Tags */
                var tagEls = doc.querySelectorAll('.meta-tags a');
                var tags   = Array.from(tagEls).map(function (a) {
                    return a.textContent.trim();
                }).join(' · ');

                /* Reading stats — word count and reading time from meta block */
                var wcEl  = doc.querySelector('.meta-word-count');
                var rtEl  = doc.querySelector('.meta-reading-time');
                var stats = [
                    wcEl ? wcEl.textContent.trim() : '',
                    rtEl ? rtEl.textContent.trim() : ''
                ].filter(Boolean).join(' · ');

                return store(href,
                    '<div class="popup-internal' + (mono ? ' has-monogram' : '') + '">'
                    + srcHtml('internal', window.ozySite.host)
                    + (mono    ? '<div class="popup-monogram" aria-hidden="true">' + mono + '</div>' : '')
                    + '<div class="popup-internal-body">'
                    + (tags    ? '<div class="popup-tags">'     + esc(tags)     + '</div>' : '')
                    + '<div class="popup-title">'               + esc(titleEl.textContent.trim()) + '</div>'
                    + (authors ? '<div class="popup-authors">'  + esc(authors)  + '</div>' : '')
                    + (abstract ? '<div class="popup-abstract">' + esc(abstract) + '</div>' : '')
                    + (stats   ? '<div class="popup-meta">'     + esc(stats)    + '</div>' : '')
                    + '</div>'
                    + '</div>');
            })
            .catch(function () { return null; });
    }

    /* ------------------------------------------------------------------
       External providers — declarative table.

       Each entry drives a generic fetch + render pipeline. Adding a new
       source means: write a URL regex, a URL builder, and a parser that
       maps the upstream response to the normalized field shape below.

         { title, authors?, meta?, abstract? | extract?, tags? }

       providerContent() handles cache/fetch/error-swallow; renderPopup()
       handles HTML composition and truncation. Per-provider quirks live
       inside each parse() — e.g. CrossRef + Internet Archive strip
       upstream HTML before returning. The shared truncate step only
       normalizes whitespace and applies the length cap.

       Render order is fixed: tags → title → authors → meta → body →
       stats. `meta` and `stats` share the `.popup-meta` CSS class but
       differ in position: `meta` reads as a subtitle (journal, year),
       while `stats` reads as a footer line (language, star count).

       Quirk fields on a provider entry:
         icon       — override for data-popup-source (CSS icon key) when
                      it differs from the provider name.
         fetchInit  — options passed to fetch() (e.g. GitHub's Accept).
         bodyLimit  — char cap for abstract/extract (default 500). */

    function truncate(s, limit) {
        if (!s) return '';
        s = s.replace(/\s+/g, ' ').trim();
        if (s.length > limit) s = s.slice(0, limit).replace(/\s\S+$/, '') + '\u2026';
        return s;
    }

    /* Authors: array → "a, b, c et al." (3 max); string → trimmed
       passthrough (some parsers pre-join their own bylines, e.g. Internet
       Archive composes "creator, year"). */
    function formatAuthors(a) {
        if (!a) return '';
        if (typeof a === 'string') return a.trim();
        if (!a.length) return '';
        var head = a.slice(0, 3).join(', ');
        return a.length > 3 ? head + ' et\u00a0al.' : head;
    }

    function renderPopup(p, fields) {
        if (!fields || !fields.title) return null;
        var iconKey = p.icon || p.name;
        var authors = formatAuthors(fields.authors);
        var bodyKey = fields.extract !== undefined ? 'extract' : 'abstract';
        var body    = truncate(fields[bodyKey], p.bodyLimit || 500);

        /* The shared popup-provider class is what scheduleShow keys the
           larger .link-popup--rich layout on — reference popups (arXiv,
           Wikipedia, …) get the roomier glanceable treatment; internal
           page previews and item cards keep the compact one. */
        var html = '<div class="popup-' + p.name + ' popup-provider">'
                 + srcHtml(iconKey, p.label);
        /* Optional lead image (Wikipedia pageimages thumbnail, arXiv
           lead figure) as a full-width banner under the source label.
           Accepted srcs: https://… (API-supplied) or root-relative
           /proxy/… (same-origin proxied figures); anything else
           (protocol-relative, data:, …) is dropped rather than guessed
           at. width/height attrs reserve the aspect ratio so the popup
           is positioned correctly before the image arrives. Photos
           cover-crop; figures (plots, diagrams) must never be cropped,
           so imageKind 'figure' letterboxes with object-fit: contain. */
        var img = fields.image;
        if (img && img.src &&
            (/^https:\/\//.test(img.src) || /^\/(?!\/)/.test(img.src))) {
            html += '<img class="popup-image-banner'
                  + (fields.imageKind === 'figure' ? ' is-figure' : '')
                  + (fields.imageKind === 'card' ? ' is-card' : '')
                  + '" src="' + esc(img.src) + '"'
                  + (img.width && img.height
                        ? ' width="' + (+img.width) + '" height="' + (+img.height) + '"'
                        : '')
                  + ' alt="" loading="lazy">';
        }
        if (fields.tags)    html += '<div class="popup-tags">'    + esc(fields.tags)  + '</div>';
        html               += '<div class="popup-title">'         + esc(fields.title) + '</div>';
        if (authors)        html += '<div class="popup-authors">' + esc(authors)      + '</div>';
        if (fields.meta)    html += '<div class="popup-meta">'    + esc(fields.meta)  + '</div>';
        if (body)           html += '<div class="popup-' + bodyKey + '">' + esc(body) + '</div>';
        if (fields.stats)   html += '<div class="popup-meta">'    + esc(fields.stats) + '</div>';
        html               += '</div>';
        return html;
    }

    function providerContent(target, p) {
        var href = target.getAttribute('href');
        if (!href) return Promise.resolve(null);
        if (cache[href]) return Promise.resolve(cache[href]);

        var match = href.match(p.match);
        if (!match) return Promise.resolve(null);

        var ctx     = { match: match, href: href };
        /* p.url runs synchronously (before the .catch below attaches) and
           can throw — e.g. decodeURIComponent on a malformed percent
           sequence in the link path. Treat a throw as "no popup". */
        var url;
        try { url = p.url(ctx); }
        catch (e) { return Promise.resolve(null); }
        var fetcher = p.fetchType === 'xml' ? fetchXml : fetchJson;

        return fetcher(url, p.fetchInit).then(function (data) {
            if (!data) return null;
            var fields = p.parse(data, ctx);
            if (!fields) return null;
            var finish = function (f) {
                var html = renderPopup(p, f);
                return html ? store(href, html) : null;
            };
            if (!p.enrich) return finish(fields);
            /* enrich() is best-effort decoration (e.g. the arXiv lead
               figure needs a second, potentially large fetch). Time-box
               it so the metadata popup is never held hostage; if the
               enrichment lands late, refresh the cache so the NEXT
               hover gets the decorated version. enrich must return a
               new fields object, leaving the original untouched for
               the timeout path. */
            var enriched = Promise.resolve()
                .then(function () { return p.enrich(fields, ctx); })
                .catch(function () { return null; });
            var timeout = new Promise(function (resolve) {
                setTimeout(resolve, 1800, undefined);
            });
            return Promise.race([enriched, timeout]).then(function (f) {
                if (f !== undefined) return finish(f || fields);
                enriched.then(function (late) { if (late) finish(late); });
                return finish(fields);
            });
        }).catch(function () { return null; });
    }

    /* bioRxiv + medRxiv share the response schema, so both entries below
       use this parser — only the upstream path differs. */
    function biorxivParse(data) {
        var paper = data && data.collection && data.collection[0];
        if (!paper || !paper.title) return null;
        var authors = paper.authors
            ? paper.authors.split(';').map(function (s) { return s.trim(); }).filter(Boolean)
            : [];
        return { title: paper.title, authors: authors, abstract: paper.abstract || '' };
    }

    var PROVIDERS = [
        /* Wikipedia — MediaWiki action API, full lead section, text-only.
           Uses .popup-extract rather than .popup-abstract; the parser
           signals this by returning `extract` instead of `abstract`.

           The API host matches the link's own subdomain, so es.wikipedia.org
           links fetch the Spanish extract, de.wikipedia.org fetches German,
           etc. Bare wikipedia.org and www. fall through to en. */
        {
            name: 'wikipedia', label: 'Wikipedia',
            host:  /^(?:[a-z0-9-]+\.)*wikipedia\.org$/,
            match: /wikipedia\.org\/wiki\/([^#?]+)/,
            fetchType: 'json',
            bodyLimit: 600,
            url: function (ctx) {
                var hostMatch = ctx.href.match(/\/\/([a-z0-9-]+)\.wikipedia\.org\//i);
                var sub       = hostMatch ? hostMatch[1].toLowerCase() : 'en';
                if (sub === 'www') sub = 'en';
                /* pageimages|extracts in one call: the article's lead
                   image thumbnail rides along with the intro text.
                   Thumbnails come from thumb.wikimedia.org (older ones
                   from upload.wikimedia.org) — both hosts must stay in
                   the CSP's img-src. 480px because the
                   banner spans the rich popup's full width. */
                return 'https://' + sub + '.wikipedia.org/w/api.php'
                     + '?action=query&prop=extracts%7Cpageimages&exintro=1'
                     + '&piprop=thumbnail&pithumbsize=480'
                     + '&format=json&redirects=1'
                     + '&titles=' + encodeURIComponent(decodeURIComponent(ctx.match[1]))
                     + '&origin=*';
            },
            parse: function (data) {
                var pages = data && data.query && data.query.pages;
                if (!pages) return null;
                var page = Object.values(pages)[0];
                if (!page || page.missing !== undefined) return null;
                var doc = new DOMParser().parseFromString(page.extract || '', 'text/html');
                /* Math elements blend display chars with raw LaTeX source
                   in the DOM — strip them before textContent extraction. */
                doc.querySelectorAll('.mwe-math-element').forEach(function (el) {
                    el.parentNode.removeChild(el);
                });
                var text = (doc.body.textContent || '').replace(/\s+/g, ' ').trim();
                if (!text) return null;
                var thumb = page.thumbnail;
                return {
                    title:     page.title,
                    extract:   text,
                    image:     thumb && thumb.source
                                   ? { src: thumb.source,
                                       width: thumb.width,
                                       height: thumb.height }
                                   : null,
                    imageKind: 'photo'
                };
            }
        },

        /* arXiv — Atom API (CORS-broken upstream, proxied).
           ID forms: new-style 2403.12345(v2), and old-style
           archive/0211159 or archive.SC/0211159 (pre-2007); /pdf/ URLs
           may carry a trailing .pdf, which stays outside the capture. */
        {
            name: 'arxiv', label: 'arXiv',
            host:  /^(?:www\.)?arxiv\.org$/,
            match: /arxiv\.org\/(?:abs|pdf)\/((?:\d{4}\.\d{4,5}|[a-z-]+(?:\.[A-Z]{2})?\/\d{7})(?:v\d+)?)/,
            fetchType: 'xml',
            url: function (ctx) {
                return '/proxy/arxiv/api/query?id_list='
                     + encodeURIComponent(ctx.match[1].replace(/v\d+$/, ''));
            },
            parse: function (xml) {
                var doc = new DOMParser().parseFromString(xml, 'application/xml');
                var titleEl   = doc.querySelector('entry > title');
                var summaryEl = doc.querySelector('entry > summary');
                if (!titleEl || !summaryEl) return null;
                return {
                    title:    titleEl.textContent.trim().replace(/\s+/g, ' '),
                    authors:  Array.from(doc.querySelectorAll('entry > author > name'))
                                   .map(function (el) { return el.textContent.trim(); }),
                    abstract: summaryEl.textContent.trim().replace(/\s+/g, ' ')
                };
            },
            /* Lead figure, best-effort: arXiv's LaTeXML HTML rendition
               (when one exists — roughly 2024+ papers with convertible
               sources) carries the paper's figures. The first
               figure.ltx_figure img is almost always the teaser /
               architecture figure. Page and image both ride through
               /proxy/arxiv-html/ (arxiv.org upstream — export.arxiv.org
               rate-limits the /html/ asset tree), so img-src 'self'
               covers them and nginx caches the heavy page fetch. */
            enrich: function (fields, ctx) {
                var id = ctx.match[1].replace(/v\d+$/, '');
                return fetch('/proxy/arxiv-html/' + encodeURIComponent(id))
                    .then(function (r) {
                        if (!r.ok) return fields;
                        return r.text().then(function (text) {
                            var doc = new DOMParser().parseFromString(text, 'text/html');
                            var img = doc.querySelector('figure.ltx_figure img.ltx_graphics');
                            var src = img && img.getAttribute('src');
                            if (!src || /^(?:data|javascript):/i.test(src)) return fields;
                            /* Resolve against the page URL (r.url keeps any
                               redirect) — relative srcs like
                               "2410.21276v1/assets/x.png" become siblings
                               under /proxy/arxiv-html/. An upstream-absolute
                               "/html/…" src maps back into proxy space; any
                               other host is dropped. */
                            var resolved = new URL(src, r.url);
                            if (resolved.origin !== location.origin) return fields;
                            var path = resolved.pathname;
                            if (path.indexOf('/html/') === 0) {
                                path = '/proxy/arxiv-html/' + path.slice('/html/'.length);
                            }
                            if (path.indexOf('/proxy/arxiv-html/') !== 0) return fields;
                            return Object.assign({}, fields, {
                                image: { src: path,
                                         width: img.getAttribute('width'),
                                         height: img.getAttribute('height') },
                                imageKind: 'figure'
                            });
                        });
                    })
                    .catch(function () { return fields; });
            }
        },

        /* DOI → CrossRef — strips upstream JATS-HTML from abstract. */
        {
            name: 'doi', label: 'CrossRef',
            host:  /^(?:dx\.|www\.)?doi\.org$/,
            match: /(?:dx\.)?doi\.org\/(10\.[^?#\s]+)/,
            fetchType: 'json',
            url: function (ctx) {
                return 'https://api.crossref.org/works/' + encodeURIComponent(ctx.match[1]);
            },
            parse: function (data) {
                var msg = data && data.message;
                if (!msg) return null;
                var title = (msg.title && msg.title[0]) || '';
                if (!title) return null;
                var journal = (msg['container-title'] && msg['container-title'][0]) || '';
                var parts   = msg.issued && msg.issued['date-parts'];
                var year    = parts && parts[0] && parts[0][0];
                return {
                    title:    title,
                    authors:  (msg.author || []).map(function (a) {
                                  return (a.given ? a.given + ' ' : '') + (a.family || '');
                              }),
                    meta:     [journal, year].filter(Boolean).join(', '),
                    abstract: markupText(msg.abstract)
                };
            }
        },

        /* GitHub — repo description + language + stars. */
        {
            name: 'github', label: 'GitHub',
            host:  /^(?:www\.)?github\.com$/,
            match: /github\.com\/([^/]+)\/([^/?#]+)/,
            fetchType: 'json',
            fetchInit: { headers: { 'Accept': 'application/vnd.github.v3+json' } },
            url: function (ctx) {
                return 'https://api.github.com/repos/' + ctx.match[1] + '/' + ctx.match[2];
            },
            parse: function (data) {
                if (!data || !data.full_name) return null;
                var stars = data.stargazers_count;
                return {
                    title:    data.full_name,
                    abstract: data.description || '',
                    stats:    [data.language,
                               stars != null ? '\u2605\u00a0' + stars : null]
                              .filter(Boolean).join(' \u00b7 ')
                };
            }
        },

        /* Open Library — works/books JSON appended to href. */
        {
            name: 'openlibrary', label: 'Open Library',
            host:  /^(?:www\.)?openlibrary\.org$/,
            match: /openlibrary\.org\/(?:works|books)\//,
            fetchType: 'json',
            bodyLimit: 300,
            url: function (ctx) { return ctx.href.replace(/[?#].*$/, '') + '.json'; },
            parse: function (data) {
                if (!data || !data.title) return null;
                var desc = data.description;
                if (desc && typeof desc === 'object') desc = desc.value;
                return { title: data.title, abstract: desc || '' };
            }
        },

        /* bioRxiv — shares schema with medRxiv via biorxivParse. */
        {
            name: 'biorxiv', label: 'bioRxiv',
            host:  /^(?:www\.)?biorxiv\.org$/,
            match: /biorxiv\.org\/content\/(10\.\d{4,}\/[^?#\s]+)/,
            fetchType: 'json',
            url: function (ctx) {
                return 'https://api.biorxiv.org/details/biorxiv/'
                     + encodeURIComponent(ctx.match[1].replace(/v\d+$/, '')) + '/json';
            },
            parse: biorxivParse
        },

        /* medRxiv — identical shape as bioRxiv; different upstream path. */
        {
            name: 'medrxiv', label: 'medRxiv',
            host:  /^(?:www\.)?medrxiv\.org$/,
            match: /medrxiv\.org\/content\/(10\.\d{4,}\/[^?#\s]+)/,
            fetchType: 'json',
            url: function (ctx) {
                return 'https://api.biorxiv.org/details/medrxiv/'
                     + encodeURIComponent(ctx.match[1].replace(/v\d+$/, '')) + '/json';
            },
            parse: biorxivParse
        },

        /* YouTube — oEmbed (no API key required). */
        {
            name: 'youtube', label: 'YouTube',
            host:  /^(?:[a-z0-9-]+\.)?youtube\.com$|^youtu\.be$/,
            match: /youtube\.com\/watch|youtu\.be\//,
            fetchType: 'json',
            url: function (ctx) {
                return 'https://www.youtube.com/oembed?url='
                     + encodeURIComponent(ctx.href) + '&format=json';
            },
            parse: function (data) {
                if (!data || !data.title) return null;
                return { title: data.title, authors: data.author_name || '' };
            }
        },

        /* Internet Archive — item metadata (CORS-broken upstream, proxied).
           CSS icon key is `internet-archive` (hyphenated), but the
           provider/class name stays short — hence the `icon` override. */
        {
            name: 'archive', label: 'Internet Archive', icon: 'internet-archive',
            host:  /^(?:www\.)?archive\.org$/,
            match: /archive\.org\/details\/([^/?#]+)/,
            fetchType: 'json',
            bodyLimit: 280,
            url: function (ctx) {
                return '/proxy/archive/metadata/' + encodeURIComponent(ctx.match[1]);
            },
            parse: function (data) {
                var meta = data && data.metadata;
                if (!meta) return null;
                var first = function (v) { return Array.isArray(v) ? v[0] : (v || ''); };
                var title = first(meta.title);
                if (!title) return null;
                var creator = first(meta.creator);
                var year    = first(meta.year);
                return {
                    title:    title,
                    authors:  [creator, year].filter(Boolean).join(', '),
                    abstract: markupText(first(meta.description))
                };
            }
        },

        /* PubMed — NCBI esummary (CORS-broken upstream, proxied). */
        {
            name: 'pubmed', label: 'PubMed',
            host:  /^pubmed\.ncbi\.nlm\.nih\.gov$/,
            match: /pubmed\.ncbi\.nlm\.nih\.gov\/(\d+)/,
            fetchType: 'json',
            url: function (ctx) {
                return '/proxy/pubmed/entrez/eutils/esummary.fcgi'
                     + '?db=pubmed&id=' + ctx.match[1] + '&retmode=json';
            },
            parse: function (data, ctx) {
                var paper = data && data.result && data.result[ctx.match[1]];
                if (!paper || !paper.title) return null;
                return {
                    title:   paper.title,
                    authors: (paper.authors || []).map(function (a) { return a.name; }),
                    meta:    [paper.fulljournalname || paper.source || '',
                              (paper.pubdate || '').slice(0, 4)].filter(Boolean).join(', ')
                };
            }
        },

        /* MTG Wiki exposes TextExtracts, but not Wikipedia's PageImages
           extension. Its article paths start with /page/, not /wiki/. */
        {
            name: 'mtgwiki', label: 'MTG Wiki',
            host:  /^(?:www\.)?mtg\.wiki$/,
            match: /^https?:\/\/(?:www\.)?mtg\.wiki\/page\/([^#?]+)/,
            fetchType: 'json',
            bodyLimit: 600,
            url: function (ctx) {
                return 'https://mtg.wiki/api.php?action=query&prop=extracts'
                     + '&exintro=1&explaintext=1&format=json&redirects=1'
                     + '&titles=' + encodeURIComponent(decodeURIComponent(ctx.match[1]))
                     + '&origin=*';
            },
            parse: function (data) {
                var pages = data && data.query && data.query.pages;
                var page = pages && Object.values(pages)[0];
                if (!page || page.missing !== undefined || !page.extract) return null;
                return { title: page.title, extract: page.extract };
            }
        },

        /* Resolve the exact printing by set and collector number; the
           optional slug is for readers, not a card-name search. */
        {
            name: 'scryfall', label: 'Scryfall',
            host:  /^(?:www\.)?scryfall\.com$/,
            match: /^https?:\/\/(?:www\.)?scryfall\.com\/card\/([^/?#]+)\/([^/?#]+)(?:\/([^/?#]+))?(?:[/?#]|$)/,
            fetchType: 'json',
            fetchInit: { headers: { 'Accept': 'application/json' } },
            bodyLimit: 1200,
            url: function (ctx) {
                var lang = ctx.match[3] || '';
                /* A slug such as "fog" is not a language code. */
                var localized = /^(?:en|es|fr|de|it|pt|ja|ko|ru|zhs|zht|he|la|grc|ar|sa|ph)$/.test(lang);
                return 'https://api.scryfall.com/cards/'
                     + encodeURIComponent(decodeURIComponent(ctx.match[1])) + '/'
                     + encodeURIComponent(decodeURIComponent(ctx.match[2]))
                     + (localized ? '/' + lang : '');
            },
            parse: function (card) {
                if (!card || card.object !== 'card' || !card.name) return null;
                var faces = card.card_faces || [];
                var front = faces[0] || card;
                var images = card.image_uris || front.image_uris;
                var rules = card.oracle_text;
                if (!rules && faces.length) {
                    rules = faces.map(function (face) {
                        return face.name + ': ' + (face.oracle_text || '');
                    }).join(' // ');
                }
                var stats = card.power !== undefined && card.toughness !== undefined
                    ? card.power + '/' + card.toughness
                    : card.loyalty !== undefined ? 'Loyalty ' + card.loyalty : '';
                return {
                    title: card.name,
                    meta: [card.mana_cost || front.mana_cost, card.type_line, stats]
                              .filter(Boolean).join(' · '),
                    extract: rules || '',
                    stats: [card.set_name, card.collector_number ? '#' + card.collector_number : '']
                               .filter(Boolean).join(' · '),
                    image: images && images.normal
                        ? { src: images.normal, width: 488, height: 680 } : null,
                    imageKind: 'card'
                };
            }
        }
    ];

    /* ------------------------------------------------------------------
       Helpers
    ------------------------------------------------------------------ */

    function store(href, html) {
        cache[href] = html;
        return html;
    }

    /* ------------------------------------------------------------------
       Date popups — explain a date or date range in human terms.
       Single date:  "6 months ago"
       Range:        "~4 weeks · started 6 months ago"
       Frontmatter:  range + "17 revisions" cadence line (only when
                     data-date-commits is present on the trigger).
    ------------------------------------------------------------------ */

    function dateContent(target) {
        /* Calendar days, as now.js counts them: the reader's own date
           against the attribute's (ozyUtils.isoDay/localDay, audit J09). */
        var isoDay  = window.ozyUtils.isoDay;
        var start   = isoDay(target.getAttribute('data-date-start'));
        if (start === null) return Promise.resolve(null);

        var endAttr = target.getAttribute('data-date-end');
        var end     = endAttr ? isoDay(endAttr) : null;
        var commits = target.getAttribute('data-date-commits');

        var today   = window.ozyUtils.localDay();
        var lines   = [];

        if (end !== null) {
            var spanDays = end - start;
            var agoDays  = today - start;
            /* "~" prefix when we've rounded to a unit larger than days. */
            var span = humanDuration(spanDays, true);
            var ago  = humanAgo(agoDays);   /* '' when start is in the future */
            lines.push(
                '<div class="popup-date-primary">'
                + esc(span) + (ago ? ' · started ' + esc(ago) : '')
                + '</div>');
            if (commits && /^\d+$/.test(commits)) {
                var n = parseInt(commits, 10);
                lines.push(
                    '<div class="popup-date-cadence">'
                    + n + ' revision' + (n === 1 ? '' : 's')
                    + '</div>');
            }
        } else {
            var ago2 = humanAgo(today - start);  /* '' when the date is in the future */
            if (ago2) {
                lines.push(
                    '<div class="popup-date-primary">'
                    + esc(ago2) + '</div>');
            }
        }

        /* Nothing renderable (e.g. a lone future date): no popup. */
        if (!lines.length) return Promise.resolve(null);

        return Promise.resolve('<div class="popup-date">' + lines.join('') + '</div>');
    }

    /* "5 days" / "3 weeks" / "4 months" / "2 years" — the unit is chosen
       to match the magnitude so the number stays small and readable.
       `approx` prefixes "~" when the returned unit is coarser than days. */
    function humanDuration(days, approx) {
        if (days <= 1) return '1 day';
        if (days < 14) return days + ' days';
        if (days < 60) {
            var w = Math.round(days / 7);
            return (approx ? '~' : '') + w + ' week' + (w === 1 ? '' : 's');
        }
        /* From 345 days the months round to twelve: say "~1 year". */
        var mo = Math.round(days / 30);
        if (mo < 12) {
            return (approx ? '~' : '') + mo + ' month' + (mo === 1 ? '' : 's');
        }
        var y = Math.round(days / 365);
        return (approx ? '~' : '') + y + ' year' + (y === 1 ? '' : 's');
    }

    /* Past-tense phrasing for a date N days in the past. Returns '' for
       future dates (negative N) — mirror now.js — so callers render
       nothing rather than a false "N days ago". */
    function humanAgo(days) {
        if (days < 0)   return '';      /* future / clock skew */
        if (days === 0) return 'today';
        if (days === 1) return 'yesterday';
        if (days < 14)  return days + ' days ago';
        return humanDuration(days, true) + ' ago';
    }

    /* The text of an abstract that arrives as markup (CrossRef's JATS, an
       Internet Archive description), parsed rather than stripped with a
       regex: its entities are decoded once, so esc() does not show them
       escaped ("p &lt; 0.05"), and its blocks are kept apart ("firms.
       Second paragraph" used to run together). A heading that only says
       "Abstract" is dropped. DOMParser's document runs no scripts and
       loads nothing. */
    var MARKUP_BLOCK = /^(?:p|div|sec|title|br|li|ul|ol|h[1-6]|blockquote|table|tr|td|th|list-item|label|caption)$/;

    function markupText(markup) {
        if (!markup || typeof markup !== 'string') return '';
        var doc = new DOMParser().parseFromString(markup, 'text/html');
        Array.prototype.slice.call(doc.body.querySelectorAll('*')).forEach(function (el) {
            var name = el.localName.replace(/^.*:/, '');
            if (name === 'script' || name === 'style'
                    || (name === 'title' && /^\s*abstract\s*$/i.test(el.textContent))) {
                el.remove();
            } else if (MARKUP_BLOCK.test(name)) {
                el.before(' ');
                el.after(' ');
            }
        });
        return (doc.body.textContent || '').replace(/\s+/g, ' ').trim();
    }

    /* Defer to the shared utility (loaded synchronously from
       templates/partials/head.html) so this file cannot drift from
       annotations.js, semantic-search.js, or build/Utils.hs. */
    function esc(s) {
        return window.ozyUtils.escapeHtml(s);
    }

    /* Emit a .popup-source label with a data-popup-source attribute so CSS
       can prepend the matching icon via ::before + mask-image. */
    function srcHtml(key, label) {
        return '<div class="popup-source" data-popup-source="' + esc(key) + '">' + esc(label) + '</div>';
    }

    document.addEventListener('DOMContentLoaded', init);
}());
