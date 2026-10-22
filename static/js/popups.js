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

   Production nginx CSP must add to connect-src:
     https://*.wikipedia.org  https://api.crossref.org
     https://api.github.com   https://openlibrary.org
     https://api.biorxiv.org  https://www.youtube.com
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

    /* Epistemic term definitions — concise summaries from the colophon,
       shown on hover for filter labels, metadata strip items, etc. */
    var EP_DEFS = {
        status:       'A controlled vocabulary describing where the work stands: Draft, Working model, Durable, Refined, Superseded, or Deprecated.',
        /* templates/partials/metadata-header.html tags this one too; it
           had no entry, so hovering it showed nothing. After the colophon. */
        'peer-status': 'The external review state: unreviewed, under review, peer reviewed, published, or retracted. Distinct from status, the author\u2019s own position: a piece can be Durable and unreviewed at the same time.',
        confidence:   'An integer from 0\u2013100, representing the author\u2019s credence in the central thesis.',
        importance:   'How much the author thinks this matters, on a 1\u20135 dot scale. Useful for orienting a reader who has limited time.',
        evidence:     'How well-evidenced the claims are, on a 1\u20135 scale. High importance and low evidence indicates a speculative position.',
        trust:        'A 0\u2013100 score derived automatically from confidence (60%) and evidence quality (40%). Answers \u201chow much should you trust the central claim?\u201d and nothing else.',
        scope:        'An orientation from personal to civilizational. Not a rating\u2009\u2014\u2009deliberately not folded into the trust score.',
        novelty:      'An orientation from conventional to innovative. Not a rating\u2009\u2014\u2009deliberately not folded into the trust score.',
        practicality: 'An orientation from abstract to exceptional. Not a rating\u2009\u2014\u2009deliberately not folded into the trust score.',
        stability:    'Auto-computed from git history. Very new or barely-touched documents are volatile; actively-revised are revising; older settled documents are fairly stable, stable, or established.'
    };

    function bindTargets(root) {
        /* Epistemic term definitions — filter labels, metadata strip, footer */
        root.querySelectorAll('[data-ep-term]').forEach(function (el) {
            bind(el, epistemicTermContent);
        });

        /* Citation markers */
        root.querySelectorAll('a.cite-link[href^="#ref-"]').forEach(function (el) {
            bind(el, citationContent);
        });

        /* Epistemic jump link — preview of the status/confidence/dot block */
        root.querySelectorAll('a[href="#epistemic"]').forEach(function (el) {
            bind(el, epistemicContent);
        });

        /* Source-file references — wrapped at build time by
           build/Filters/SourceRefs.hs around inline `path` and Forgejo
           links. These specific kinds are bound before the generic
           internal- and external-link loops below: bind() keeps an
           element's first binding, and a source reference with a
           same-origin href (../source/data/x.json) used to be claimed as
           an internal page, which showed nothing (audit J08). Other
           git.levineuwirth.org links get no popup: the forge sits behind
           Anubis, which refuses anonymous API requests, and Forgejo sends
           no CORS headers in any case. */
        root.querySelectorAll('a.source-ref[data-source-path]').forEach(function (el) {
            bind(el, sourceContent);
        });

        /* GitHub code references — blob / tree / commit links that
           tools/code-refs.py snapshotted at build time and
           build/Filters/CodeRefs.hs tagged. Bound before the external
           dispatcher so the repository-card provider does not claim
           them; untagged GitHub links still fall through to it. */
        root.querySelectorAll('a[data-code-ref][data-code-src]').forEach(function (el) {
            bind(el, codeRefContent);
        });

        /* PDF links — rewritten to viewer URL by Links.hs; thumbnail on hover */
        root.querySelectorAll('a.pdf-link[data-pdf-src]').forEach(function (el) {
            bind(el, pdfContent);
        });

        /* PGP signature links in footer */
        root.querySelectorAll('a.footer-sig-link').forEach(function (el) {
            bind(el, sigContent);
        });

        /* Internal links — absolute (/foo) and relative (../../foo) same-origin hrefs.
           relativizeUrls in Hakyll makes index-page links relative, so we must match both. */
        root.querySelectorAll('a[href^="/"], a[href^="./"], a[href^="../"]').forEach(function (el) {
            /* Photography cards are their own preview.
               A card already shows the photograph, and in grid mode reveals
               the title over it on hover — so the generic internal-link
               popup fires at the same moment, in the same gesture, saying
               the same thing in a floating panel that covers neighbouring
               frames. The in-picture caption supersedes it here. This is
               scoped to the grid, so a photograph linked from prose still
               gets the normal popup. */
            if (el.closest('.photography-grid')) return;

            /* The music shelf is its own preview, for the same reason:
               pointing at a spine turns that score face-out beside it,
               and a popup would cover the page it has just shown. */
            if (el.closest('.shelf')) return;

            /* Author links, backlink source links, and Related items always get popups */
            var inAuthors  = el.closest('.meta-authors');
            var isBacklink = el.classList.contains('backlink-source');
            var isSimilar  = el.classList.contains('similar-link');
            /* PDF-typed Related items are handled below by the pdf-link binder */
            if (isSimilar && el.classList.contains('pdf-link')) return;
            if (!inAuthors && !isBacklink && !isSimilar) {
                if (el.closest('nav, #toc, footer, .page-meta-footer, .metadata')) return;
                if (el.classList.contains('cite-link') || el.classList.contains('meta-tag')) return;
                if (el.classList.contains('pdf-link')) return;
                if (el.classList.contains('content-divider-logo') || el.classList.contains('aftermatter-logo')) return;
            }
            bind(el, internalContent);
        });

        /* External links — single dispatcher handles all providers */
        root.querySelectorAll('a[href^="http"]').forEach(function (el) {
            if (el.closest('nav, #toc, footer, .page-meta-footer')) return;
            var provider = getProvider(el.getAttribute('href') || '');
            if (provider) bind(el, provider);
        });

        /* Date hover popups — any element tagged with data-date-start.
           Handles the frontmatter range link, version-history list items,
           last-reviewed in the epistemic block, blog post dates, etc. */
        root.querySelectorAll('[data-date-start]').forEach(function (el) {
            bind(el, dateContent);
        });

        /* Item-card revision markers (/new, library, tag pages). The card
           gutter says only "Revised"; the dates and the note live here. */
        root.querySelectorAll('.item-card-revised').forEach(function (el) {
            bind(el, revisionContent);
        });
    }

    /* Public re-init hook used by transclude.js after it injects new
       content into the DOM. Idempotent — bind() marks each element so
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
       `match` regex hits the href is selected. The host comes first, as
       for the link icons (build/Filters/Links.hs): `match` alone is
       unanchored, and took gist.github.com/user/id for a repository and
       a path containing doi.org/10.… for a DOI. */
    function getProvider(href) {
        if (!href) return null;
        if (annotations && annotations[href]) return annotationContent;
        var host = hostOf(href);
        for (var i = 0; i < PROVIDERS.length; i++) {
            if (PROVIDERS[i].host.test(host) && PROVIDERS[i].match.test(href)) {
                var entry = PROVIDERS[i];
                return function (target) { return providerContent(target, entry); };
            }
        }
        return null;
    }

    function bind(el, provider) {
        /* Idempotent: skip elements that already have a popup binding,
           so reinitPopups() called from transclude.js cannot stack
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
                placeSourceMark(popup);
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

                /* Authors */
                var authorEls = doc.querySelectorAll('.meta-authors a');
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
                    + srcHtml('internal', 'levineuwirth.org')
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
        }
    ];

    /* ------------------------------------------------------------------
       Helpers
    ------------------------------------------------------------------ */

    function store(href, html) {
        cache[href] = html;
        return html;
    }

    /* Epistemic jump link — pulls the parallel-tag strip from the top of
       the page (which holds the author-declared orientation tags) and
       the expanded DL from the #epistemic footer section (which holds
       the git-derived stability/last-reviewed/trend). The popup combines
       both so a reader hovering the link mid-page sees the full profile
       without scrolling.

       Returns a DocumentFragment instead of an HTML string so the popup
       receives cloned nodes (defense in depth — if a future change ever
       allowed user-authored HTML into the source section, the popup
       would still see exactly the same already-rendered DOM rather than
       a re-parsed string). */
    function epistemicContent() {
        var wrap = document.createElement('div');
        wrap.className = 'popup-epistemic';

        var strip = document.querySelector('.meta-epistemic-strip');
        if (strip) {
            wrap.appendChild(strip.cloneNode(true));
        }

        var section = document.getElementById('epistemic');
        var expanded = section ? section.querySelector('.ep-expanded') : null;
        if (expanded) {
            wrap.appendChild(expanded.cloneNode(true));
        }

        if (!strip && !expanded) return Promise.resolve(null);
        return Promise.resolve(wrap);
    }

    /* Epistemic term definition — shows a concise description from the
       colophon for any element tagged with data-ep-term="<field>". */
    function epistemicTermContent(target) {
        var term = target.dataset.epTerm;
        var def  = term && EP_DEFS[term];
        if (!def) return Promise.resolve(null);
        var words = term.replace(/-/g, ' ');           /* peer-status: "Peer status" */
        var label = words.charAt(0).toUpperCase() + words.slice(1);
        return Promise.resolve(
            '<div class="popup-ep-term">'
            + '<div class="popup-source" data-popup-source="colophon">'
            +   '<a href="/colophon.html#living-documents">Colophon</a>'
            + '</div>'
            + '<div class="popup-title">' + esc(label) + '</div>'
            + '<div class="popup-abstract">' + esc(def) + '</div>'
            + '</div>'
        );
    }

    /* Local PDF — shows the build-time first-page thumbnail (.thumb.png).
       Returns null (no popup) if the thumbnail file does not exist. */
    function pdfContent(target) {
        var src   = target.dataset.pdfSrc;
        if (!src) return Promise.resolve(null);
        var thumb = src.replace(/\.pdf$/i, '.thumb.png');
        if (cache[thumb]) return Promise.resolve(cache[thumb]);
        /* HEAD request: verify thumbnail exists before committing to a popup. */
        return fetch(thumb, { method: 'HEAD', credentials: 'same-origin' })
            .then(function (r) {
                if (!r.ok) return null;
                return store(thumb,
                    '<div class="popup-pdf">'
                    + '<img class="popup-pdf-thumb" src="' + esc(thumb) + '" alt="PDF first page">'
                    + '</div>');
            })
            .catch(function () { return null; });
    }

    /* Source-file preview — fetches /source/<path> (a same-origin copy
       emitted by the source-preview Hakyll rule) and renders it with
       renderSourcePopup; a Markdown file instead gets its sectioned
       prose rendering (/source/<path>.sections.json, MarkdownSections.hs)
       unless the link asks for a line range.

       Raw responses are cached (cachedFetch); rendering is repeated per
       hover so a cached entry never gets re-parented (a Node can only
       live in one place at a time). */
    function sourceContent(target) {
        var path = target.dataset.sourcePath;
        if (!path) return Promise.resolve(null);
        var opts = sourceOpts(target, 'view full file');
        var raw  = '/source/' + path;
        return sourceOrProse(path, raw, raw + '.sections.json', null, opts);
    }

    /* Shared by source-ref and GitHub blob popups: prose for Markdown
       (falling back to the raw text if its rendering is missing), code
       for everything else. */
    function sourceOrProse(path, rawUrl, sectionsUrl, meta, opts) {
        var asCode = function () {
            return cachedFetch(rawUrl, 'text').then(function (text) {
                return text == null ? null : renderSourcePopup(path, text, meta, opts);
            });
        };
        if (!isMarkdown(path) || opts.range) return asCode();
        return cachedFetch(sectionsUrl, 'json').then(function (doc) {
            if (doc && doc.sections && doc.sections.length) {
                return renderMarkdownPopup(path, doc, meta, opts);
            }
            return asCode();
        });
    }

    function isMarkdown(path) { return /\.(md|markdown)$/i.test(path); }

    /* What the link's fragment asks for — a line range (#L12, #L12-L30,
       as GitHub and Forgejo write them) or a heading id — plus the
       click-through target for the popup's footer. */
    function sourceOpts(target, footerLabel) {
        var hash = '';
        try { hash = decodeURIComponent((target.hash || '').slice(1)); }
        catch (_) { /* malformed escape: treat as no fragment */ }
        var m = /^L(\d+)(?:-L?(\d+))?$/.exec(hash);
        var range = null;
        if (m) {
            var a = +m[1], b = m[2] ? +m[2] : a;
            range = [Math.min(a, b), Math.max(a, b)];
        }
        return {
            range:    range,
            fragment: m ? '' : hash,
            href:     target.href,
            footer:   footerLabel
        };
    }

    /* Code body for source and GitHub blob popups: a line-number gutter,
       Prism highlighting, and — when the link names a line range — a
       band over those lines, scrolled into view by scheduleShow. Files
       longer than WINDOW lines show a window (around the range, when
       there is one); the footer says which lines, and links through. */
    function renderSourcePopup(path, text, meta, opts) {
        var WINDOW = 400;
        opts = opts || {};
        var lines = text.split('\n');
        if (lines.length > 1 && lines[lines.length - 1] === '') lines.pop();
        var n     = lines.length;
        var range = opts.range && opts.range[0] <= n
            ? [opts.range[0], Math.min(opts.range[1], n)] : null;

        var start = 1;
        if (n > WINDOW && range) {
            start = Math.max(1, Math.min(range[0] - 20, n - WINDOW + 1));
        }
        var end  = Math.min(n, start + WINDOW - 1);
        var lang = languageFromPath(path);

        var wrap = document.createElement('div');
        wrap.className = 'popup-source-code';
        wrap.appendChild(sourceHeader(path, meta, range
            ? 'L' + range[0] + (range[1] > range[0] ? '–' + range[1] : '') : ''));

        var scroll = document.createElement('div');
        scroll.className = 'popup-source-scroll';
        var body = document.createElement('div');
        body.className = 'popup-source-lines';

        var gutter = document.createElement('pre');
        gutter.className = 'popup-source-gutter';
        gutter.setAttribute('aria-hidden', 'true');
        var nums = [];
        for (var i = start; i <= end; i++) nums.push(i);
        gutter.textContent = nums.join('\n');
        body.appendChild(gutter);

        var pre  = document.createElement('pre');
        pre.className = lang ? 'popup-source-pre language-' + lang : 'popup-source-pre';
        var code = document.createElement('code');
        if (lang) code.className = 'language-' + lang;
        code.textContent = lines.slice(start - 1, end).join('\n');
        pre.appendChild(code);
        body.appendChild(pre);

        if (range && range[0] >= start && range[0] <= end) {
            var mark = document.createElement('div');
            mark.className = 'popup-source-mark';
            mark.style.setProperty('--mark-from', range[0] - start);
            mark.style.setProperty('--mark-lines', Math.min(range[1], end) - range[0] + 1);
            body.appendChild(mark);
        }

        scroll.appendChild(body);
        wrap.appendChild(scroll);

        var partial = start > 1 || end < n;
        wrap.appendChild(sourceFooter(opts,
            partial ? 'lines ' + start + '–' + end + ' of ' + n : ''));
        /* A page without inline code does not preload Prism. The shared
           loader highlights this detached preview before it is shown. */
        if (lang && typeof window.lnHighlightCode === 'function') {
            return window.lnHighlightCode(wrap).then(function () { return wrap; });
        }
        return wrap;
    }

    /* Place the line-range band from the gutter's used line-height, not
       from CSS calc() on --line: that is rem-based, the root size varies
       by viewport, and engines snap line boxes to whole pixels, so the
       calc drifts a fraction of a pixel per line and is visibly off by
       line 20. Needs layout, so it runs once the popup is in the
       document; then scrolls the band into view with two lines of
       lead-in. */
    function placeSourceMark(root) {
        var mark   = root.querySelector('.popup-source-mark');
        var gutter = mark && mark.parentNode.querySelector('.popup-source-gutter');
        if (!gutter) return;
        var cs    = getComputedStyle(gutter);
        var padT  = parseFloat(cs.paddingTop) || 0;
        /* The used line-height, in px. (The gutter's box height is no
           use: flex stretches it to the code column, which carries an
           extra strut of descent below its last line.) */
        var lineH = parseFloat(cs.lineHeight);
        if (!(lineH > 0)) return;
        var from  = +mark.style.getPropertyValue('--mark-from') || 0;
        var lines = +mark.style.getPropertyValue('--mark-lines') || 1;
        mark.style.left   = gutter.offsetWidth + 'px';
        mark.style.top    = (padT + from * lineH) + 'px';
        mark.style.height = (lines * lineH) + 'px';
        var scroller = mark.closest('.popup-source-scroll');
        if (scroller) scroller.scrollTop = Math.max(0, padT + (from - 2) * lineH);
    }

    /* Prose body for Markdown: the section the fragment names (with its
       subsections), or the whole document when there is none or it
       names no heading. The HTML comes from MarkdownSections.hs, which
       renders it at build time from a sanitised AST — raw HTML off, only
       absolute http(s) links, no images, no ids — so it is inserted
       as-is. */
    function renderMarkdownPopup(path, doc, meta, opts) {
        var secs = doc.sections;
        var from = 0, to = secs.length, excerpt = null;
        if (opts.fragment) {
            for (var i = 0; i < secs.length; i++) {
                if (secs[i].id === opts.fragment) { from = i; break; }
            }
            if (i < secs.length) {
                excerpt = secs[from];
                for (to = from + 1; to < secs.length; to++) {
                    if (secs[to].level <= excerpt.level) break;
                }
            }
        }

        var html = '';
        for (var k = from; k < to; k++) {
            var s = secs[k];
            /* The excerpt's own heading is already in the header strip. */
            if (s.level > 0 && !(excerpt && k === from)) {
                html += '<div class="popup-md-heading popup-md-h' + Math.min(s.level, 4) + '">'
                      + s.heading + '</div>';
            }
            html += s.html;
        }

        var wrap = document.createElement('div');
        wrap.className = 'popup-source-code popup-source-prose';
        wrap.appendChild(sourceHeader(path, meta,
            excerpt ? '§ ' + stripTags(excerpt.heading) : ''));
        var body = document.createElement('div');
        body.className = 'popup-md';
        body.innerHTML = html;
        wrap.appendChild(body);
        wrap.appendChild(sourceFooter(opts,
            excerpt ? 'section ' + (from + 1) + ' of ' + secs.length : ''));
        return wrap;
    }

    function stripTags(html) {
        var t = document.createElement('template');
        t.innerHTML = html;
        return t.content.textContent.trim();
    }

    /* Footer strip: an optional note ("lines 1–400 of 912") and the
       click-through, which is the link itself — fragment included, so
       GitHub or Forgejo opens at the same place. */
    function sourceFooter(opts, note) {
        var foot = document.createElement('div');
        foot.className = 'popup-source-truncated';
        if (note) foot.appendChild(document.createTextNode(note + ' · '));
        if (opts.href) {
            var a = document.createElement('a');
            a.href = opts.href;
            a.target = '_blank';
            a.rel = 'noopener noreferrer';
            a.textContent = (opts.footer || 'view full file') + ' →';
            foot.appendChild(a);
        }
        return foot;
    }

    /* Header strip for source and code-reference popups: the path, an
       optional locator after it (a line range, or "§ Results"), and
       optionally a quieter line naming the repository and revision. */
    function sourceHeader(path, meta, locator) {
        var label = document.createElement('div');
        label.className = 'popup-source-path';
        label.textContent = path;
        if (locator) {
            var loc = document.createElement('span');
            loc.className = 'popup-source-locator';
            loc.textContent = locator;
            label.appendChild(loc);
        }
        if (meta) {
            var m = document.createElement('div');
            m.className = 'popup-source-rev';
            m.textContent = meta;
            label.appendChild(m);
        }
        return label;
    }

    /* ------------------------------------------------------------------
       Code references — GitHub blob / tree / commit snapshots.

       tools/code-refs.py stores each linked object under /code-refs/
       at build time; build/Filters/CodeRefs.hs puts its location and
       revision on the link as data-code-* attributes. Everything here
       is same-origin: no GitHub API call, no rate limit, and the popup
       shows exactly the revision the link is pinned to.
    ------------------------------------------------------------------ */

    function codeRefContent(target) {
        var d    = target.dataset;
        var kind = d.codeRef;
        var src  = d.codeSrc;
        if (!src || !/^\/code-refs\//.test(src)) return Promise.resolve(null);

        var info = {
            repo:   d.codeRepo || '',
            sha:    d.codeSha || '',
            path:   d.codePath || '',
            date:   d.codeDate || '',
            branch: d.codeBranch || ''
        };

        if (kind === 'blob') {
            return sourceOrProse(info.path, src,
                src.replace(/\.txt$/, '.sections.json'),
                revisionLine(info), sourceOpts(target, 'view on GitHub'));
        }
        if (kind === 'tree') {
            return cachedFetch(src, 'json').then(function (tree) {
                return tree ? renderTreePopup(info, tree) : null;
            });
        }
        if (kind === 'commit') {
            return cachedFetch(src, 'json').then(function (c) {
                return c ? renderCommitPopup(info, c) : null;
            });
        }
        return Promise.resolve(null);
    }

    /* Same-origin fetch of a snapshot, memoised by URL. The raw text or
       parsed JSON is cached, never a rendered node: a Node can only have
       one parent, so each hover renders afresh. */
    function cachedFetch(url, as) {
        var key = as + ':' + url;
        if (cache[key] !== undefined) return Promise.resolve(cache[key]);
        return fetch(url, { credentials: 'same-origin' })
            .then(function (r) { return r.ok ? (as === 'json' ? r.json() : r.text()) : null; })
            .then(function (v) { if (v != null) cache[key] = v; return v; })
            .catch(function () { return null; });
    }

    /* "JamesPetrie/VerInf @ 3508ef1 · 20 Sep 2026" — or, for a link to
       a branch, which snapshot of that branch the popup is showing. */
    function revisionLine(info) {
        var short = info.sha.slice(0, 7);
        var date  = formatDay(info.date);
        if (info.branch) {
            return info.repo + ' · ' + info.branch + ' as of ' + short
                 + (date ? ', ' + date : '');
        }
        return info.repo + ' @ ' + short + (date ? ' · ' + date : '');
    }

    function formatDay(iso) {
        var d = iso ? new Date(iso) : null;
        if (!d || isNaN(d.getTime())) return '';
        return d.toLocaleDateString('en-GB',
            { day: 'numeric', month: 'short', year: 'numeric', timeZone: 'UTC' });
    }

    /* First paragraph of a README that is prose rather than a heading,
       badge row, or HTML block — markup stripped to plain text. */
    function readmeLead(text) {
        var paras = String(text).split(/\n\s*\n/);
        for (var i = 0; i < paras.length; i++) {
            var p = paras[i].trim();
            if (!p || /^(#|!\[|\[!\[|<|```|---|\|)/.test(p)) continue;
            return p.replace(/!\[[^\]]*\]\([^)]*\)/g, '')
                    .replace(/\[([^\]]*)\]\([^)]*\)/g, '$1')
                    .replace(/[`*_]/g, '')
                    .replace(/\s+/g, ' ');
        }
        return '';
    }

    /* The snapshot commit's own message is deliberately not shown: it
       describes whatever change happened to land last, which is rarely
       about the directory linked. The revision line is what matters. */
    function renderTreePopup(info, tree) {
        var MAX_ENTRIES = 24;
        var entries = tree.entries || [];
        var html = '<div class="popup-source-code popup-code-ref">';
        var path = info.path ? info.path.replace(/\/?$/, '/') : info.repo + '/';
        var rev  = revisionLine(info);
        html += '<div class="popup-source-path">' + esc(path)
              + '<div class="popup-source-rev">' + esc(rev) + '</div></div>';
        html += '<div class="popup-code-body">';
        var lead = tree.readme ? readmeLead(tree.readme.text) : '';
        if (lead) html += '<div class="popup-abstract">' + esc(truncate(lead, 320)) + '</div>';
        html += '<ul class="popup-code-list">';
        entries.slice(0, MAX_ENTRIES).forEach(function (e) {
            var dir = e.type === 'dir';
            html += '<li class="' + (dir ? 'is-dir' : 'is-file') + '">'
                  + esc(e.name) + (dir ? '/' : '') + '</li>';
        });
        html += '</ul></div>';
        if (entries.length > MAX_ENTRIES) {
            var n = entries.length - MAX_ENTRIES;
            html += '<div class="popup-source-truncated">' + n + ' more entr'
                  + (n === 1 ? 'y' : 'ies') + '</div>';
        }
        return html + '</div>';
    }

    function renderCommitPopup(info, c) {
        var MAX_FILES = 12;
        var lines   = String(c.message || '').split('\n');
        var subject = lines[0] || '';
        var body    = lines.slice(1).join('\n').trim();
        var files   = c.files || [];
        var total   = +(c.files_total != null ? c.files_total : files.length) || 0;
        var st      = c.stats || {};

        var html = '<div class="popup-source-code popup-code-ref">';
        html += '<div class="popup-source-path">' + esc(info.repo) + ' @ '
              + esc(String(c.sha || info.sha).slice(0, 7))
              + '<div class="popup-source-rev">'
              + esc([c.author, formatDay(c.date)].filter(Boolean).join(' · '))
              + '</div></div>';
        html += '<div class="popup-code-body">';
        html += '<div class="popup-code-subject">' + esc(subject) + '</div>';
        if (body) html += '<div class="popup-abstract">' + esc(truncate(body, 420)) + '</div>';
        html += '<div class="popup-code-stat">' + total + ' file' + (total === 1 ? '' : 's')
              + (st.additions != null ? ' · <ins>+' + (+st.additions) + '</ins>' : '')
              + (st.deletions != null ? ' <del>−' + (+st.deletions) + '</del>' : '')
              + '</div>';
        html += '<ul class="popup-code-list popup-code-files">';
        files.slice(0, MAX_FILES).forEach(function (f) {
            html += '<li><span class="popup-code-file">' + esc(f.filename) + '</span>'
                  + '<span class="popup-code-delta"><ins>+' + (+f.additions || 0) + '</ins> '
                  + '<del>−' + (+f.deletions || 0) + '</del></span></li>';
        });
        html += '</ul></div>';
        if (total > MAX_FILES) {
            var n = total - MAX_FILES;
            html += '<div class="popup-source-truncated">' + n + ' more file'
                  + (n === 1 ? '' : 's') + '</div>';
        }
        return html + '</div>';
    }

    /* Map a path's extension (or basename, for Makefile) onto the set
       of Prism languages bundled in /js/prism.min.js: bash, haskell,
       javascript, css, markup, yaml, python, makefile. Returns null
       when no mapping applies; the caller falls back to plain text. */
    function languageFromPath(path) {
        var basename = path.split('/').pop();
        if (basename === 'Makefile') return 'makefile';
        var m = path.match(/\.([a-z0-9]+)$/i);
        if (!m) return null;
        switch (m[1].toLowerCase()) {
            case 'hs':
            case 'cabal':   return 'haskell';
            case 'js':
            case 'mjs':     return 'javascript';
            case 'css':     return 'css';
            case 'html':
            case 'svg':     return 'markup';
            case 'py':      return 'python';
            case 'sh':
            case 'bash':
            case 'conf':    return 'bash';
            case 'yaml':
            case 'yml':     return 'yaml';
            case 'md':      return 'markup';
            default:        return null;
        }
    }

    /* PGP signature — fetch the .sig file and display ASCII armor */
    function sigContent(target) {
        var href = target.getAttribute('href');
        if (!href) return Promise.resolve(null);
        if (cache[href]) return Promise.resolve(cache[href]);
        return fetch(href, { credentials: 'same-origin' })
            .then(function (r) { return r.ok ? r.text() : null; })
            .then(function (text) {
                if (!text) return null;
                var html = '<div class="popup-sig"><pre>' + esc(text.trim()) + '</pre></div>';
                cache[href] = html;
                return html;
            });
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
           against the attribute's (lnUtils.isoDay/localDay, audit J09). */
        var isoDay  = window.lnUtils.isoDay;
        var start   = isoDay(target.getAttribute('data-date-start'));
        if (start === null) return Promise.resolve(null);

        var endAttr = target.getAttribute('data-date-end');
        var end     = endAttr ? isoDay(endAttr) : null;
        var commits = target.getAttribute('data-date-commits');

        var today   = window.lnUtils.localDay();
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
        return window.lnUtils.escapeHtml(s);
    }

    /* Emit a .popup-source label with a data-popup-source attribute so CSS
       can prepend the matching icon via ::before + mask-image. */
    function srcHtml(key, label) {
        return '<div class="popup-source" data-popup-source="' + esc(key) + '">' + esc(label) + '</div>';
    }

    document.addEventListener('DOMContentLoaded', init);
}());
