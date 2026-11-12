/* popups/source.js — what source-file and GitHub code popups share: a
   file shown as code with a line band, or as prose when it is Markdown.
   With the source-refs or code-refs switch: served after popups.js, in
   the same file (EngineFiles.jsModules), before popups/source-refs.js
   and popups/code-refs.js, which use it as window.ozyPopups.source. */
(function () {
    'use strict';

    var P = window.ozyPopups;
    if (!P) return;

    var cache = Object.create(null);   /* kind:url → raw text or parsed JSON */

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
        if (lang && typeof window.ozyHighlightCode === 'function') {
            return window.ozyHighlightCode(wrap).then(function () { return wrap; });
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

    P.source = {
        sourceOrProse: sourceOrProse,
        sourceOpts:    sourceOpts,
        cachedFetch:   cachedFetch
    };
    P.onShow(placeSourceMark);
}());
