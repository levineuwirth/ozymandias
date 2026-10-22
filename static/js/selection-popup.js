/* selection-popup.js — Custom text-selection toolbar.
   Appears automatically after a short delay on any non-empty selection.
   Adapts its buttons based on the context of the selection:

     code  (known lang) → Copy · [MDN / Hoogle / Docs…]
     code  (unknown)    → Copy
     inline code email → Copy · Email
     math               → Copy · nLab · OEIS · Wolfram
     prose (multi-word) → Annotate · BibTeX · Copy · DuckDuckGo · Here · Wikipedia
     prose (one word)   → Annotate · BibTeX · Copy · Define · DuckDuckGo · Here · Wikipedia

   When the selection sits inside a [lang] subtree whose primary subtag
   differs from the document root (e.g. the ::: {lang="es"} blocks in
   library.md), a Translate button is inserted in alphabetical position
   that opens DeepL. Languages DeepL does not support fall back to
   DeepL's auto-detect.
*/
(function () {
    'use strict';

    var SHOW_DELAY = 450;

    var popup     = null;
    var showTimer = null;
    var picker    = null;
    var pickerColor = 'amber';

    /* ------------------------------------------------------------------
       Documentation providers keyed by Prism language identifier.
       Label: short button text. url: base search URL (query appended).
    ------------------------------------------------------------------ */

    /* DeepL-supported source language codes (primary subtags, lowercase).
       Selections whose resolved lang is not in this set are sent with
       source='auto' so DeepL can attempt its own detection. */
    var DEEPL_SOURCES = {
        ar: 1, bg: 1, cs: 1, da: 1, de: 1, el: 1, en: 1, es: 1, et: 1,
        fi: 1, fr: 1, he: 1, hu: 1, id: 1, it: 1, ja: 1, ko: 1, lt: 1,
        lv: 1, nb: 1, nl: 1, no: 1, pl: 1, pt: 1, ro: 1, ru: 1, sk: 1,
        sl: 1, sv: 1, th: 1, tr: 1, uk: 1, vi: 1, zh: 1,
    };

    var DOC_PROVIDERS = {
        'javascript': { label: 'MDN',    url: 'https://developer.mozilla.org/en-US/search?q=' },
        'typescript': { label: 'MDN',    url: 'https://developer.mozilla.org/en-US/search?q=' },
        'jsx':        { label: 'MDN',    url: 'https://developer.mozilla.org/en-US/search?q=' },
        'tsx':        { label: 'MDN',    url: 'https://developer.mozilla.org/en-US/search?q=' },
        'html':       { label: 'MDN',    url: 'https://developer.mozilla.org/en-US/search?q=' },
        'css':        { label: 'MDN',    url: 'https://developer.mozilla.org/en-US/search?q=' },
        'haskell':    { label: 'Hoogle', url: 'https://hoogle.haskell.org/?hoogle=' },
        'python':     { label: 'Docs',   url: 'https://docs.python.org/3/search.html?q=' },
        'rust':       { label: 'Docs',   url: 'https://doc.rust-lang.org/std/?search=' },
        'c':          { label: 'Docs',   url: 'https://en.cppreference.com/mwiki/index.php?search=' },
        'cpp':        { label: 'Docs',   url: 'https://en.cppreference.com/mwiki/index.php?search=' },
        'java':       { label: 'Docs',   url: 'https://docs.oracle.com/en/java/javase/21/docs/api/search.html?q=' },
        'go':         { label: 'Docs',   url: 'https://pkg.go.dev/search?q=' },
        'ruby':       { label: 'Docs',   url: 'https://ruby-doc.org/core/search?q=' },
        'r':          { label: 'Docs',   url: 'https://www.rdocumentation.org/search?q=' },
        'lua':        { label: 'Docs',   url: 'https://www.lua.org/search.html?q=' },
        'scala':      { label: 'Docs',   url: 'https://docs.scala-lang.org/search/?q=' },
    };

    /* ------------------------------------------------------------------
       Init
    ------------------------------------------------------------------ */

    function init() {
        popup = document.createElement('div');
        popup.className = 'selection-popup';
        popup.setAttribute('role', 'toolbar');
        popup.setAttribute('aria-label', 'Text selection options');
        document.body.appendChild(popup);

        document.addEventListener('mouseup',   onMouseUp);
        document.addEventListener('keyup',     onKeyUp);
        document.addEventListener('mousedown', onMouseDown);
        document.addEventListener('keydown',   onKeyDown);
        /* Scrolling hides the toolbar, not the Annotate picker: it is
           placed on the page and scrolls with the selection, and a reader
           may scroll to see what they are annotating. Closing it on
           scroll also closed it when its own note field took focus and
           the browser scrolled that into view. */
        window.addEventListener('scroll', hide, { passive: true });
    }

    /* ------------------------------------------------------------------
       Event handlers
    ------------------------------------------------------------------ */

    function onMouseUp(e) {
        if (popup.contains(e.target)) return;
        clearTimeout(showTimer);
        showTimer = setTimeout(tryShow, SHOW_DELAY);
    }

    function onKeyUp(e) {
        /* Typing capitals in the annotation picker's note input (or any
           other editable field) releases Shift — don't re-summon the
           toolbar over the UI the user is typing into. */
        var t = e.target;
        if (t && t.nodeType === Node.ELEMENT_NODE) {
            if (popup.contains(t)) return;
            if (picker && picker.contains(t)) return;
            if (t.isContentEditable || t.closest('input, textarea')) return;
        }
        if (e.shiftKey || e.key === 'End' || e.key === 'Home') {
            clearTimeout(showTimer);
            showTimer = setTimeout(tryShow, SHOW_DELAY);
        }
    }

    function onMouseDown(e) {
        if (popup.contains(e.target)) return;
        if (picker && picker.classList.contains('is-visible') && picker.contains(e.target)) return;
        hide();
        hidePicker();
    }

    function onKeyDown(e) {
        if (e.key === 'Escape') { hide(); hidePicker(); }
    }

    /* ------------------------------------------------------------------
       Context detection
    ------------------------------------------------------------------ */

    function getContext(sel) {
        if (!sel.rangeCount) return 'prose';
        var range = sel.getRangeAt(0);
        var node  = range.commonAncestorContainer;
        var el    = (node.nodeType === Node.TEXT_NODE) ? node.parentElement : node;
        if (!el) return 'prose';

        if (el.closest('pre, code, .sourceCode, .highlight')) return 'code';
        if (el.closest('.math, .katex, .katex-html, .katex-display')) return 'math';

        /* Fallback: commonAncestorContainer can land at <p> when selection
           starts/ends just outside a KaTeX span — check via intersectsNode. */
        var mathEls = document.querySelectorAll('.math');
        for (var i = 0; i < mathEls.length; i++) {
            if (range.intersectsNode(mathEls[i])) return 'math';
        }

        return 'prose';
    }

    /* Returns the primary subtag of the nearest [lang] ancestor of the
       selection (e.g. "es-MX" → "es"), or null when that lang matches the
       document root — in which case the text is in the page's default
       language and no Translate button is warranted. */
    function getSelectionLang(sel) {
        if (!sel.rangeCount) return null;
        var node = sel.getRangeAt(0).commonAncestorContainer;
        var el   = (node.nodeType === Node.TEXT_NODE) ? node.parentElement : node;
        if (!el) return null;

        var langEl = el.closest('[lang]');
        if (!langEl) return null;

        /* A primary language subtag (ISO 639: two or three letters) or
           nothing: it goes into the Translate button's markup and on to
           DeepL (audit J14). */
        var lang = (langEl.getAttribute('lang') || '').toLowerCase().split('-')[0];
        if (!/^[a-z]{2,3}$/.test(lang)) return null;

        var rootLang = (document.documentElement.getAttribute('lang') || 'en')
            .toLowerCase().split('-')[0];
        if (lang === rootLang) return null;

        return lang;
    }

    /* Returns the Prism language identifier for the code block containing
       the current selection, or null if the language is not annotated. */
    function getCodeLanguage(sel) {
        if (!sel.rangeCount) return null;
        var node = sel.getRangeAt(0).commonAncestorContainer;
        var el   = (node.nodeType === Node.TEXT_NODE) ? node.parentElement : node;
        if (!el) return null;
        /* Prism puts language-* on the <code> element; our Code.hs filter
           ensures the class is always present when a language is specified. */
        var code = el.closest('code[class*="language-"]');
        if (!code) return null;
        var m = code.className.match(/language-(\w+)/);
        return m ? m[1].toLowerCase() : null;
    }

    /* Recognize one complete, unquoted ASCII address selected in inline
       code. Encode the mailbox so ?, &, # and % remain address characters
       rather than mailto headers or fragments (RFC 6068). */
    function getInlineEmailHref(sel, text) {
        var node = sel.getRangeAt(0).commonAncestorContainer;
        var el = node.nodeType === Node.TEXT_NODE ? node.parentElement : node;
        var code = el && el.closest('code');
        if (!code || code.closest('pre') || code.textContent.trim() !== text || text.length > 254) return null;
        var parts = text.split('@');
        if (parts.length !== 2 || parts[0].length > 64) return null;
        if (!/^[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*$/i.test(parts[0])) return null;
        var labels = parts[1].split('.');
        if (labels.length < 2 || labels.some(function (label) {
            return !/^[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?$/i.test(label);
        })) return null;
        return 'mailto:' + encodeURIComponent(parts[0]) + '@' + parts[1];
    }

    /* ------------------------------------------------------------------
       Core logic
    ------------------------------------------------------------------ */

    function tryShow() {
        var sel  = window.getSelection();
        var text = sel ? sel.toString().trim() : '';

        if (!text || text.length < 2 || !sel.rangeCount) { hide(); return; }

        var range = sel.getRangeAt(0);
        var rect  = range.getBoundingClientRect();
        if (!rect.width && !rect.height) { hide(); return; }
        var context  = getContext(sel);
        var oneWord  = isSingleWord(text);
        var codeLang = (context === 'code') ? getCodeLanguage(sel) : null;
        var emailHref = (context === 'code') ? getInlineEmailHref(sel, text) : null;
        var selLang  = (context === 'prose') ? getSelectionLang(sel) : null;
        /* Highlights belong to the page's text: a selection in a popup,
           the contents or the footer is not offered one (it used to
           highlight the same words in the body instead). */
        var body = document.getElementById('markdownBody');
        var annotatable = !!body && body.contains(range.commonAncestorContainer);

        popup.innerHTML = buildHTML(context, oneWord, codeLang, selLang, annotatable, emailHref);
        popup.style.visibility = 'hidden';
        popup.classList.add('is-visible');

        position(rect);
        popup.style.visibility = '';
        bindActions(text, rect, range.cloneRange());
    }

    function hide() {
        clearTimeout(showTimer);
        if (popup) popup.classList.remove('is-visible');
    }

    /* ------------------------------------------------------------------
       Positioning — centred above selection, flip below if needed
    ------------------------------------------------------------------ */

    function position(rect) {
        var GAP = 10;
        var sy  = window.scrollY;
        var sx  = window.scrollX;
        var vw  = document.documentElement.clientWidth;   /* without a scrollbar */
        var vh  = window.innerHeight;
        /* Measured at no more than the window's width, from the left edge:
           where it was last left would narrow it further (an absolutely
           placed box shrinks to the room on its right), and wider it ran
           off a phone's screen. CSS wraps the buttons into rows. */
        popup.style.maxWidth = (vw - 2 * GAP) + 'px';
        popup.style.left = sx + 'px';
        var pw  = popup.offsetWidth;
        var ph  = popup.offsetHeight;

        var left = rect.left + sx + rect.width / 2 - pw / 2;
        left = Math.max(sx + GAP, Math.min(left, sx + vw - pw - GAP));

        var top = rect.top + sy - ph - GAP;
        if (top < sy + GAP) {
            top = rect.bottom + sy + GAP;
            popup.classList.add('is-below');
        } else {
            popup.classList.remove('is-below');
        }
        /* In the window either way, as the picker is (positionPicker). */
        top = Math.max(sy + GAP, Math.min(top, sy + vh - ph - GAP));

        popup.style.left = left + 'px';
        popup.style.top  = top  + 'px';
    }

    /* ------------------------------------------------------------------
       Helpers
    ------------------------------------------------------------------ */

    function isSingleWord(text) {
        return !/\s/.test(text);
    }

    /* ------------------------------------------------------------------
       HTML builder — context-aware button sets
    ------------------------------------------------------------------ */

    function buildHTML(context, oneWord, codeLang, selLang, annotatable, emailHref) {
        if (context === 'code') {
            var provider = codeLang ? DOC_PROVIDERS[codeLang] : null;
            return btn('copy', 'Copy')
                 + (emailHref ? emailBtn(emailHref) : (provider ? docsBtn(provider) : ''));
        }

        if (context === 'math') {
            /* Alphabetical: Copy · nLab · OEIS · Wolfram */
            return btn('copy',    'Copy')
                 + btn('nlab',    'nLab')
                 + btn('oeis',    'OEIS')
                 + btn('wolfram', 'Wolfram');
        }

        /* Prose: [Annotate] · BibTeX · Copy · [Define] · DuckDuckGo · Here · [Translate] · Wikipedia */
        return (annotatable ? btn('annotate', 'Annotate') : '')
             + btn('cite',      'BibTeX')
             + btn('copy',      'Copy')
             + (oneWord ? btn('define', 'Define') : '')
             + btn('search',    'DuckDuckGo')
             + btn('here',      'Here')
             + (selLang ? translateBtn(selLang) : '')
             + btn('wikipedia', 'Wikipedia');
    }

    function btn(action, label) {
        return '<button class="selection-popup-btn" data-action="' + action + '">'
             + label + '</button>';
    }

    /* The builders' one reference to the shared namespace (renamed in A9). */
    function escapeHtml(s) {
        return window.lnUtils.escapeHtml(s);
    }

    function emailBtn(href) {
        return '<a class="selection-popup-btn" data-action="email" href="'
             + escapeHtml(href) + '">Email</a>';
    }

    /* Docs button embeds the base URL so dispatch can read it without a lookup. */
    function docsBtn(provider) {
        return '<button class="selection-popup-btn" data-action="docs"'
             + ' data-docs-url="' + provider.url + '">'
             + provider.label + '</button>';
    }

    /* Translate button carries the resolved source lang so dispatch can pass
       it to DeepL (or fall through to auto-detect). */
    function translateBtn(lang) {
        return '<button class="selection-popup-btn" data-action="translate"'
             + ' data-lang="' + escapeHtml(lang) + '">Translate</button>';
    }


    /* ------------------------------------------------------------------
       Action bindings
    ------------------------------------------------------------------ */

    function bindActions(text, rect, range) {
        popup.querySelectorAll('[data-action]').forEach(function (el) {
            if (el.getAttribute('aria-disabled') === 'true') return;
            el.addEventListener('click', function () {
                /* Email is a native link: let the browser hand it to the
                   configured mail app, without opening an empty tab. */
                if (el.tagName !== 'A') dispatch(el.getAttribute('data-action'), text, el, rect, range);
                hide();
            });
        });
    }

    /* ------------------------------------------------------------------
       BibTeX/BibLaTeX builder for the Cite action
    ------------------------------------------------------------------ */

    /* Escape LaTeX special characters in a BibTeX field value. */
    function escBib(s) {
        return String(s)
            .replace(/\\/g,  '\\textbackslash{}')
            .replace(/[#$%&_{}]/g, function (c) { return '\\' + c; })
            .replace(/~/g,   '\\textasciitilde{}')
            .replace(/\^/g,  '\\textasciicircum{}');
    }

    /* "Levi Neuwirth" → "Neuwirth, Levi" */
    function toBibAuthor(name) {
        var parts = name.trim().split(/\s+/);
        if (parts.length < 2) return name;
        return parts[parts.length - 1] + ', ' + parts.slice(0, -1).join(' ');
    }

    function buildBibTeX(selectedText) {
        /* Title — h1.page-title is most reliable; fall back to document.title */
        var titleEl = document.querySelector('h1.page-title');
        var title   = titleEl
            ? titleEl.textContent.trim()
            : document.title.split(' \u2014 ')[0].trim();

        /* Author(s) — read from .meta-authors, default to site owner */
        var authorEls = document.querySelectorAll('.meta-authors a');
        var authors   = authorEls.length
            ? Array.from(authorEls).map(function (a) {
                return toBibAuthor(a.textContent.trim());
              }).join(' and ')
            : 'Neuwirth, Levi';

        /* Year — scrape from the first version-history entry ("14 March 2026 · Created"),
           fall back to current year. */
        var year = String(new Date().getFullYear());
        var vhEl = document.querySelector('#version-history li');
        if (vhEl) {
            var ym = vhEl.textContent.match(/\b(\d{4})\b/);
            if (ym) year = ym[1];
        }

        /* Access date */
        var now     = new Date();
        var urldate = now.getFullYear() + '-'
                    + String(now.getMonth() + 1).padStart(2, '0') + '-'
                    + String(now.getDate()).padStart(2, '0');

        /* Citation key: lastname + year + first_content_word_of_title */
        var lastName  = authors.split(',')[0].toLowerCase().replace(/[^a-z]/g, '');
        var stopwords = /^(the|and|for|with|from|that|this|into|about|over)$/i;
        var keyWord   = title.split(/\s+/).filter(function (w) {
            return w.length > 2 && !stopwords.test(w);
        })[0] || 'untitled';
        keyWord = keyWord.toLowerCase().replace(/[^a-z0-9]/g, '');
        var key = lastName + year + keyWord;

        return [
            '@online{' + key + ',',
            '  author  = {' + escBib(authors)       + '},',
            '  title   = {' + escBib(title)          + '},',
            '  year    = {' + year                   + '},',
            '  url     = {' + window.location.href   + '},',
            '  urldate = {' + urldate                + '},',
            '  note    = {\\enquote{' + escBib(selectedText) + '}},',
            '}',
        ].join('\n');
    }

    /* ------------------------------------------------------------------
       Action dispatch
    ------------------------------------------------------------------ */

    function dispatch(action, text, el, rect, range) {
        var q = encodeURIComponent(text);
        if (action === 'search') {
            window.open('https://duckduckgo.com/?q=' + q, '_blank', 'noopener,noreferrer');

        } else if (action === 'copy') {
            if (navigator.clipboard) navigator.clipboard.writeText(text).catch(function () {});

        } else if (action === 'docs') {
            var base = el.getAttribute('data-docs-url');
            if (base) window.open(base + q, '_blank', 'noopener,noreferrer');

        } else if (action === 'wolfram') {
            window.open('https://www.wolframalpha.com/input?i=' + q, '_blank', 'noopener,noreferrer');

        } else if (action === 'oeis') {
            window.open('https://oeis.org/search?q=' + q, '_blank', 'noopener,noreferrer');

        } else if (action === 'nlab') {
            window.open('https://ncatlab.org/nlab/search?query=' + q, '_blank', 'noopener,noreferrer');

        } else if (action === 'wikipedia') {
            /* Always use Special:Search — never jumps to an article directly,
               so phrases and ambiguous terms always show the results page. */
            window.open('https://en.wikipedia.org/wiki/Special:Search?search=' + q, '_blank', 'noopener,noreferrer');

        } else if (action === 'define') {
            /* English Wiktionary — only rendered for single-word selections. */
            window.open('https://en.wiktionary.org/wiki/' + q, '_blank', 'noopener,noreferrer');

        } else if (action === 'cite') {
            var citation = buildBibTeX(text);
            if (navigator.clipboard) navigator.clipboard.writeText(citation).catch(function () {});

        } else if (action === 'here') {
            /* Site search via Pagefind — opens search page with query pre-filled. */
            window.open('/search.html?q=' + q, '_blank', 'noopener,noreferrer');

        } else if (action === 'translate') {
            /* DeepL web translator. Hash-routed source/target/text triple;
               'auto' is used when the resolved lang is not one DeepL supports. */
            var src     = el.getAttribute('data-lang') || '';
            var srcCode = DEEPL_SOURCES[src] ? src : 'auto';
            window.open('https://www.deepl.com/translator#' + srcCode + '/en/' + q,
                        '_blank', 'noopener,noreferrer');

        } else if (action === 'annotate') {
            showAnnotatePicker(text, rect, range);
        }
    }

    /* ------------------------------------------------------------------
       Annotate picker — color swatches + optional note input
    ------------------------------------------------------------------ */

    /* `range` is the selection the toolbar was shown for, which says
       which occurrence of the text to highlight; the selection itself is
       gone by the time the reader commits. */
    function showAnnotatePicker(text, selRect, range) {
        if (!picker) {
            picker = document.createElement('div');
            picker.className = 'ann-picker';
            picker.setAttribute('role', 'dialog');
            picker.setAttribute('aria-label', 'Annotate selection');
            document.body.appendChild(picker);
        }

        pickerColor = 'amber';
        picker.innerHTML =
            '<div class="ann-picker-swatches">'
            + swatchBtn('amber') + swatchBtn('sage') + swatchBtn('steel') + swatchBtn('rose')
            + '</div>'
            + '<input class="ann-picker-note" type="text" placeholder="Note (optional)" maxlength="200">'
            + '<div class="ann-picker-actions">'
            + '<button class="ann-picker-submit">Highlight</button>'
            + '</div>';

        picker.style.visibility = 'hidden';
        picker.classList.add('is-visible');
        positionPicker(selRect);
        picker.style.visibility = '';

        var note      = picker.querySelector('.ann-picker-note');
        var submitBtn = picker.querySelector('.ann-picker-submit');

        picker.querySelector('.ann-picker-swatch[data-color="amber"]').classList.add('is-selected');

        picker.querySelectorAll('.ann-picker-swatch').forEach(function (sw) {
            sw.addEventListener('click', function () {
                picker.querySelectorAll('.ann-picker-swatch').forEach(function (s) {
                    s.classList.remove('is-selected');
                });
                sw.classList.add('is-selected');
                pickerColor = sw.getAttribute('data-color');
            });
        });

        function commit() {
            if (window.Annotations &&
                !window.Annotations.add(text, pickerColor, note.value.trim(), range)) {
                /* Say so rather than close as if it had worked. */
                var msg = picker.querySelector('.ann-picker-error');
                if (!msg) {
                    msg = document.createElement('p');
                    msg.className = 'ann-picker-error';
                    msg.setAttribute('role', 'alert');
                    picker.appendChild(msg);
                }
                msg.textContent = 'This passage could not be highlighted.';
                return;
            }
            hidePicker();
        }

        submitBtn.addEventListener('click', commit);
        note.addEventListener('keydown', function (e) {
            if (e.key === 'Enter')  { e.preventDefault(); commit(); }
            if (e.key === 'Escape') { hidePicker(); }
        });

        /* At once: the picker is visible as soon as it opens
           (annotations.css). A deferred focus could run after the picker
           had closed, and took focus from wherever the reader had moved,
           the new highlight included. */
        note.focus({ preventScroll: true });
    }

    function hidePicker() {
        if (picker) picker.classList.remove('is-visible');
    }

    /* Above the selection, else below it, and in either case inside the
       window: below a selection that reached the foot of the window it
       used to open out of sight. */
    function positionPicker(rect) {
        var pw  = picker.offsetWidth;
        var ph  = picker.offsetHeight;
        var GAP = 8;
        var sy  = window.scrollY;
        var sx  = window.scrollX;
        var vw  = document.documentElement.clientWidth;   /* without a scrollbar */
        var vh  = window.innerHeight;

        var left = rect.left + sx + rect.width / 2 - pw / 2;
        left = Math.max(sx + GAP, Math.min(left, sx + vw - pw - GAP));

        var top = rect.top + sy - ph - GAP;
        if (top < sy + GAP) top = rect.bottom + sy + GAP;
        top = Math.max(sy + GAP, Math.min(top, sy + vh - ph - GAP));

        picker.style.left = left + 'px';
        picker.style.top  = top  + 'px';
    }

    function swatchBtn(color) {
        return '<button class="ann-picker-swatch ann-picker-swatch--' + color
            + '" data-color="' + color + '" aria-label="' + color + '"></button>';
    }

    document.addEventListener('DOMContentLoaded', init);
}());
