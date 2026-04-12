/* popups.js — Hover preview popups.
   Content providers (in dispatch priority order):
     1.  Local annotations  — /data/annotations.json (any URL, author-defined)
     2.  Citations          — DOM lookup, cite-link[href^="#ref-"]
     3.  Internal pages     — same-origin fetch, title + authors + tags + abstract + stats
     4.  Wikipedia          — MediaWiki action API, full lead section
     5.  arXiv              — export.arxiv.org Atom API
     6.  DOI / CrossRef     — api.crossref.org, title/authors/abstract
     7.  GitHub             — api.github.com, repo description + stars
     8.  Open Library       — openlibrary.org JSON API, book description
     9.  bioRxiv / medRxiv  — api.biorxiv.org, abstract
     10. YouTube            — oEmbed, title + channel (no key required)
     11. Internet Archive   — archive.org/metadata, title + description
     12. PubMed             — NCBI esummary, title + authors + journal

   Production nginx CSP must add:
     connect-src https://en.wikipedia.org https://export.arxiv.org
                 https://api.crossref.org https://api.github.com
                 https://openlibrary.org https://api.biorxiv.org
                 https://www.youtube.com https://archive.org
                 https://eutils.ncbi.nlm.nih.gov
*/
(function () {
    'use strict';

    var SHOW_DELAY = 250;
    var HIDE_DELAY = 150;

    var popup        = null;
    var showTimer    = null;
    var hideTimer    = null;
    var activeTarget = null;
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
        document.body.appendChild(popup);

        popup.addEventListener('mouseenter', cancelHide);
        popup.addEventListener('mouseleave', scheduleHide);

        loadAnnotations().then(function () {
            bindTargets(document.body);
        });
    }

    function bindTargets(root) {
        /* Citation markers */
        root.querySelectorAll('a.cite-link[href^="#ref-"]').forEach(function (el) {
            bind(el, citationContent);
        });

        /* Epistemic jump link — preview of the status/confidence/dot block */
        root.querySelectorAll('a[href="#epistemic"]').forEach(function (el) {
            bind(el, epistemicContent);
        });

        /* Internal links — absolute (/foo) and relative (../../foo) same-origin hrefs.
           relativizeUrls in Hakyll makes index-page links relative, so we must match both. */
        root.querySelectorAll('a[href^="/"], a[href^="./"], a[href^="../"]').forEach(function (el) {
            /* Author links in .meta-authors and backlink source links always get popups */
            var inAuthors  = el.closest('.meta-authors');
            var isBacklink = el.classList.contains('backlink-source');
            if (!inAuthors && !isBacklink) {
                if (el.closest('nav, #toc, footer, .page-meta-footer, .metadata')) return;
                if (el.classList.contains('cite-link') || el.classList.contains('meta-tag')) return;
                if (el.classList.contains('pdf-link')) return;
            }
            bind(el, internalContent);
        });

        /* PDF links — rewritten to viewer URL by Links.hs; thumbnail on hover */
        root.querySelectorAll('a.pdf-link[data-pdf-src]').forEach(function (el) {
            bind(el, pdfContent);
        });

        /* PGP signature links in footer */
        root.querySelectorAll('a.footer-sig-link').forEach(function (el) {
            bind(el, sigContent);
        });

        /* External links — single dispatcher handles all providers */
        root.querySelectorAll('a[href^="http"]').forEach(function (el) {
            if (el.closest('nav, #toc, footer, .page-meta-footer')) return;
            var provider = getProvider(el.getAttribute('href') || '');
            if (provider) bind(el, provider);
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

    /* Returns the appropriate provider function for a given URL, or null. */
    function getProvider(href) {
        if (!href) return null;
        /* Local annotation takes priority over everything */
        if (annotations && annotations[href])                              return annotationContent;
        if (/wikipedia\.org\/wiki\//.test(href))                           return wikipediaContent;
        if (/arxiv\.org\/(?:abs|pdf)\/\d{4}\.\d{4,5}/.test(href))         return arxivContent;
        if (/(?:dx\.)?doi\.org\/10\./.test(href))                          return doiContent;
        if (/github\.com\/[^/]+\/[^/?#]+/.test(href))                     return githubContent;
        if (/openlibrary\.org\/(?:works|books)\//.test(href))              return openlibraryContent;
        if (/(?:bio|med)rxiv\.org\/content\/10\./.test(href))              return biorxivContent;
        if (/(?:youtube\.com\/watch|youtu\.be\/)/.test(href))              return youtubeContent;
        if (/archive\.org\/details\//.test(href))                          return archiveContent;
        if (/pubmed\.ncbi\.nlm\.nih\.gov\/\d/.test(href))                  return pubmedContent;
        return null;
    }

    function bind(el, provider) {
        /* Idempotent: skip elements that already have a popup binding,
           so reinitPopups() called from transclude.js cannot stack
           listeners on already-bound nodes. */
        if (el.dataset.popupBound === '1') return;
        el.dataset.popupBound = '1';
        el.addEventListener('mouseenter', function () { scheduleShow(el, provider); });
        el.addEventListener('mouseleave', scheduleHide);
        el.addEventListener('focus',      function () { scheduleShow(el, provider); });
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
                positionPopup(target);
                popup.classList.add('is-visible');
                popup.setAttribute('aria-hidden', 'false');
            }).catch(function () { /* silently fail */ });
        }, SHOW_DELAY);
    }

    function scheduleHide() {
        clearTimeout(showTimer);
        hideTimer = setTimeout(function () {
            popup.classList.remove('is-visible');
            popup.setAttribute('aria-hidden', 'true');
            activeTarget = null;
        }, HIDE_DELAY);
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

        var top = (rect.bottom + GAP + ph <= vh)
            ? rect.bottom + sy + GAP
            : rect.top    + sy - ph - GAP;

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

    /* 1. Citations — synchronous DOM lookup; supports multi-citation groups
          via data-cite-keys (space-separated list of ref-* IDs). */
    function citationContent(target) {
        return new Promise(function (resolve) {
            var keysAttr = target.getAttribute('data-cite-keys');
            var ids = keysAttr
                ? keysAttr.trim().split(/\s+/)
                : [(target.getAttribute('href') || '').slice(1)];
            var parts = ids.map(function (id) {
                var entry = document.getElementById(id);
                return entry ? '<div class="popup-citation-entry">' + entry.innerHTML + '</div>' : null;
            }).filter(Boolean);
            resolve(parts.length
                ? '<div class="popup-citation">' + parts.join('') + '</div>'
                : null);
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
                    '<div class="popup-internal">'
                    + (tags    ? '<div class="popup-source">'   + esc(tags)     + '</div>' : '')
                    + '<div class="popup-title">'               + esc(titleEl.textContent.trim()) + '</div>'
                    + (authors ? '<div class="popup-authors">'  + esc(authors)  + '</div>' : '')
                    + (abstract ? '<div class="popup-abstract">' + esc(abstract) + '</div>' : '')
                    + (stats   ? '<div class="popup-meta">'     + esc(stats)    + '</div>' : '')
                    + '</div>');
            })
            .catch(function () { return null; });
    }

    /* 3. Wikipedia — MediaWiki action API, full lead section, text-only */
    function wikipediaContent(target) {
        var href = target.getAttribute('href');
        if (!href || cache[href]) return Promise.resolve(cache[href] || null);

        var m = href.match(/wikipedia\.org\/wiki\/([^#?]+)/);
        if (!m) return Promise.resolve(null);

        var apiUrl = 'https://en.wikipedia.org/w/api.php'
            + '?action=query&prop=extracts&exintro=1&format=json&redirects=1'
            + '&titles=' + encodeURIComponent(decodeURIComponent(m[1])) + '&origin=*';

        return fetchJson(apiUrl)
            .then(function (data) {
                var pages = data && data.query && data.query.pages;
                if (!pages) return null;
                var page = Object.values(pages)[0];
                if (!page || page.missing !== undefined) return null;
                var doc  = new DOMParser().parseFromString(page.extract || '', 'text/html');
                /* Remove math elements before extracting text — their DOM includes both
                   display characters and raw LaTeX source, producing garbled output. */
                doc.querySelectorAll('.mwe-math-element').forEach(function (el) {
                    el.parentNode.removeChild(el);
                });
                var text = (doc.body.textContent || '').replace(/\s+/g, ' ').trim();
                if (!text) return null;
                if (text.length > 600) text = text.slice(0, 600).replace(/\s\S+$/, '') + '\u2026';
                return store(href,
                    '<div class="popup-wikipedia">'
                    + srcHtml('wikipedia', 'Wikipedia')
                    + '<div class="popup-title">'   + esc(page.title) + '</div>'
                    + '<div class="popup-extract">' + esc(text)       + '</div>'
                    + '</div>');
            })
            .catch(function () { return null; });
    }

    /* 4. arXiv — Atom API, title + authors + abstract */
    function arxivContent(target) {
        var href = target.getAttribute('href');
        if (!href || cache[href]) return Promise.resolve(cache[href] || null);

        var m = href.match(/arxiv\.org\/(?:abs|pdf)\/(\d{4}\.\d{4,5}(?:v\d+)?)/);
        if (!m) return Promise.resolve(null);

        var id = m[1].replace(/v\d+$/, '');
        return fetchXml('https://export.arxiv.org/api/query?id_list=' + encodeURIComponent(id))
            .then(function (xml) {
                if (!xml) return null;
                var doc       = new DOMParser().parseFromString(xml, 'application/xml');
                var titleEl   = doc.querySelector('entry > title');
                var summaryEl = doc.querySelector('entry > summary');
                if (!titleEl || !summaryEl) return null;
                var title   = titleEl.textContent.trim().replace(/\s+/g, ' ');
                var summary = summaryEl.textContent.trim().replace(/\s+/g, ' ');
                if (summary.length > 500) summary = summary.slice(0, 500).replace(/\s\S+$/, '') + '\u2026';
                var authors = Array.from(doc.querySelectorAll('entry > author > name'))
                    .map(function (el) { return el.textContent.trim(); });
                var authorStr = authors.slice(0, 3).join(', ');
                if (authors.length > 3) authorStr += ' et\u00a0al.';
                return store(href,
                    '<div class="popup-arxiv">'
                    + srcHtml('arxiv', 'arXiv')
                    + '<div class="popup-title">'    + esc(title)     + '</div>'
                    + (authorStr ? '<div class="popup-authors">'  + esc(authorStr) + '</div>' : '')
                    + '<div class="popup-abstract">' + esc(summary)   + '</div>'
                    + '</div>');
            })
            .catch(function () { return null; });
    }

    /* 5. DOI / CrossRef — title, authors, journal, year, abstract */
    function doiContent(target) {
        var href = target.getAttribute('href');
        if (!href || cache[href]) return Promise.resolve(cache[href] || null);

        var m = href.match(/(?:dx\.)?doi\.org\/(10\.[^?#\s]+)/);
        if (!m) return Promise.resolve(null);

        return fetchJson('https://api.crossref.org/works/' + encodeURIComponent(m[1]))
            .then(function (data) {
                var msg = data && data.message;
                if (!msg) return null;
                var title = (msg.title && msg.title[0]) || '';
                if (!title) return null;
                var authors = (msg.author || []).slice(0, 3)
                    .map(function (a) { return (a.given ? a.given + ' ' : '') + (a.family || ''); })
                    .join(', ');
                if ((msg.author || []).length > 3) authors += ' et\u00a0al.';
                var journal = (msg['container-title'] && msg['container-title'][0]) || '';
                var parts   = msg.issued && msg.issued['date-parts'];
                var year    = parts && parts[0] && parts[0][0];
                var abstract = (msg.abstract || '').replace(/<[^>]+>/g, '').trim();
                if (abstract.length > 500) abstract = abstract.slice(0, 500).replace(/\s\S+$/, '') + '\u2026';
                var meta = [journal, year].filter(Boolean).join(', ');
                return store(href,
                    '<div class="popup-doi">'
                    + srcHtml('doi', 'CrossRef')
                    + '<div class="popup-title">'    + esc(title)    + '</div>'
                    + (authors  ? '<div class="popup-authors">'  + esc(authors)  + '</div>' : '')
                    + (meta     ? '<div class="popup-meta">'     + esc(meta)     + '</div>' : '')
                    + (abstract ? '<div class="popup-abstract">' + esc(abstract) + '</div>' : '')
                    + '</div>');
            })
            .catch(function () { return null; });
    }

    /* 6. GitHub — repo description, language, stars */
    function githubContent(target) {
        var href = target.getAttribute('href');
        if (!href || cache[href]) return Promise.resolve(cache[href] || null);

        var m = href.match(/github\.com\/([^/]+)\/([^/?#]+)/);
        if (!m) return Promise.resolve(null);

        return fetchJson('https://api.github.com/repos/' + m[1] + '/' + m[2],
                         { headers: { 'Accept': 'application/vnd.github.v3+json' } })
            .then(function (data) {
                if (!data || !data.full_name) return null;
                var meta = [data.language, data.stargazers_count != null ? '\u2605\u00a0' + data.stargazers_count : null]
                    .filter(Boolean).join(' \u00b7 ');
                return store(href,
                    '<div class="popup-github">'
                    + srcHtml('github', 'GitHub')
                    + '<div class="popup-title">'    + esc(data.full_name)   + '</div>'
                    + (data.description ? '<div class="popup-abstract">' + esc(data.description) + '</div>' : '')
                    + (meta ? '<div class="popup-meta">' + esc(meta) + '</div>' : '')
                    + '</div>');
            })
            .catch(function () { return null; });
    }

    /* 7. Open Library — book title + description */
    function openlibraryContent(target) {
        var href = target.getAttribute('href');
        if (!href || cache[href]) return Promise.resolve(cache[href] || null);

        var base   = href.replace(/[?#].*$/, '');
        var apiUrl = base + '.json';

        return fetchJson(apiUrl)
            .then(function (data) {
                if (!data || !data.title) return null;
                var desc = data.description;
                if (desc && typeof desc === 'object') desc = desc.value;
                desc = (desc || '').replace(/\s+/g, ' ').trim();
                if (desc.length > 300) desc = desc.slice(0, 300).replace(/\s\S+$/, '') + '\u2026';
                return store(href,
                    '<div class="popup-openlibrary">'
                    + srcHtml('openlibrary', 'Open Library')
                    + '<div class="popup-title">'    + esc(data.title) + '</div>'
                    + (desc ? '<div class="popup-abstract">' + esc(desc) + '</div>' : '')
                    + '</div>');
            })
            .catch(function () { return null; });
    }

    /* 8. bioRxiv / medRxiv — abstract via biorxiv content server API */
    function biorxivContent(target) {
        var href = target.getAttribute('href');
        if (!href || cache[href]) return Promise.resolve(cache[href] || null);

        var m = href.match(/(?:bio|med)rxiv\.org\/content\/(10\.\d{4,}\/[^?#\s]+)/);
        if (!m) return Promise.resolve(null);

        var doi    = m[1].replace(/v\d+$/, '');
        var server = /medrxiv/.test(href) ? 'medrxiv' : 'biorxiv';
        var label  = server === 'medrxiv' ? 'medRxiv' : 'bioRxiv';

        return fetchJson('https://api.biorxiv.org/details/' + server + '/' + encodeURIComponent(doi) + '/json')
            .then(function (data) {
                var paper = data && data.collection && data.collection[0];
                if (!paper || !paper.title) return null;
                var abstract = (paper.abstract || '').replace(/\s+/g, ' ').trim();
                if (abstract.length > 500) abstract = abstract.slice(0, 500).replace(/\s\S+$/, '') + '\u2026';
                var authorStr = '';
                if (paper.authors) {
                    var list = paper.authors.split(';').map(function (s) { return s.trim(); }).filter(Boolean);
                    authorStr = list.slice(0, 3).join(', ');
                    if (list.length > 3) authorStr += ' et\u00a0al.';
                }
                return store(href,
                    '<div class="popup-biorxiv">'
                    + srcHtml(server, label)
                    + '<div class="popup-title">'    + esc(paper.title) + '</div>'
                    + (authorStr ? '<div class="popup-authors">'  + esc(authorStr)  + '</div>' : '')
                    + (abstract  ? '<div class="popup-abstract">' + esc(abstract)   + '</div>' : '')
                    + '</div>');
            })
            .catch(function () { return null; });
    }

    /* 9. YouTube — oEmbed, title + channel name */
    function youtubeContent(target) {
        var href = target.getAttribute('href');
        if (!href || cache[href]) return Promise.resolve(cache[href] || null);

        return fetchJson('https://www.youtube.com/oembed?url=' + encodeURIComponent(href) + '&format=json')
            .then(function (data) {
                if (!data || !data.title) return null;
                return store(href,
                    '<div class="popup-youtube">'
                    + srcHtml('youtube', 'YouTube')
                    + '<div class="popup-title">'   + esc(data.title)       + '</div>'
                    + (data.author_name ? '<div class="popup-authors">' + esc(data.author_name) + '</div>' : '')
                    + '</div>');
            })
            .catch(function () { return null; });
    }

    /* 10. Internet Archive — title, creator, description */
    function archiveContent(target) {
        var href = target.getAttribute('href');
        if (!href || cache[href]) return Promise.resolve(cache[href] || null);

        var m = href.match(/archive\.org\/details\/([^/?#]+)/);
        if (!m) return Promise.resolve(null);

        return fetchJson('https://archive.org/metadata/' + encodeURIComponent(m[1]))
            .then(function (data) {
                var meta = data && data.metadata;
                if (!meta) return null;
                var first = function (v) { return Array.isArray(v) ? v[0] : (v || ''); };
                var title   = first(meta.title);
                var creator = first(meta.creator);
                var year    = first(meta.year);
                var desc    = first(meta.description);
                if (!title) return null;
                desc = desc.replace(/<[^>]+>/g, '').replace(/\s+/g, ' ').trim();
                if (desc.length > 280) desc = desc.slice(0, 280).replace(/\s\S+$/, '') + '\u2026';
                var byline = [creator, year].filter(Boolean).join(', ');
                return store(href,
                    '<div class="popup-archive">'
                    + srcHtml('internet-archive', 'Internet Archive')
                    + '<div class="popup-title">'    + esc(title) + '</div>'
                    + (byline ? '<div class="popup-authors">' + esc(byline) + '</div>' : '')
                    + (desc   ? '<div class="popup-abstract">' + esc(desc) + '</div>' : '')
                    + '</div>');
            })
            .catch(function () { return null; });
    }

    /* 11. PubMed — NCBI esummary, title + authors + journal */
    function pubmedContent(target) {
        var href = target.getAttribute('href');
        if (!href || cache[href]) return Promise.resolve(cache[href] || null);

        var m = href.match(/pubmed\.ncbi\.nlm\.nih\.gov\/(\d+)/);
        if (!m) return Promise.resolve(null);

        var pmid   = m[1];
        var apiUrl = 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi'
                   + '?db=pubmed&id=' + pmid + '&retmode=json';

        return fetchJson(apiUrl)
            .then(function (data) {
                var paper = data && data.result && data.result[pmid];
                if (!paper || !paper.title) return null;
                var authors = (paper.authors || []).slice(0, 3)
                    .map(function (a) { return a.name; }).join(', ');
                if ((paper.authors || []).length > 3) authors += ' et\u00a0al.';
                var journal = paper.fulljournalname || paper.source || '';
                var year    = (paper.pubdate || '').slice(0, 4);
                var meta    = [journal, year].filter(Boolean).join(', ');
                return store(href,
                    '<div class="popup-pubmed">'
                    + srcHtml('pubmed', 'PubMed')
                    + '<div class="popup-title">'    + esc(paper.title) + '</div>'
                    + (authors ? '<div class="popup-authors">' + esc(authors) + '</div>' : '')
                    + (meta    ? '<div class="popup-meta">'    + esc(meta)    + '</div>' : '')
                    + '</div>');
            })
            .catch(function () { return null; });
    }

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
