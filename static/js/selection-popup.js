/* selection-popup.js — Custom text-selection toolbar.
   Appears automatically after a short delay on any non-empty selection.
   Adapts its buttons based on the context of the selection:

     code  (known lang) → Copy · [MDN / Hoogle / Docs…]
     code  (unknown)    → Copy
     math               → Copy · nLab · OEIS · Wolfram
     prose (multi-word) → Annotate · BibTeX · Copy · DuckDuckGo · Here · Wikipedia
     prose (one word)   → Annotate · BibTeX · Copy · Define · DuckDuckGo · Here · Wikipedia
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
        window.addEventListener('scroll', function () { hide(); hidePicker(); }, { passive: true });
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

        popup.innerHTML = buildHTML(context, oneWord, codeLang);
        popup.style.visibility = 'hidden';
        popup.classList.add('is-visible');

        position(rect);
        popup.style.visibility = '';
        bindActions(text, rect);
    }

    function hide() {
        clearTimeout(showTimer);
        if (popup) popup.classList.remove('is-visible');
    }

    /* ------------------------------------------------------------------
       Positioning — centred above selection, flip below if needed
    ------------------------------------------------------------------ */

    function position(rect) {
        var pw  = popup.offsetWidth;
        var ph  = popup.offsetHeight;
        var GAP = 10;
        var sy  = window.scrollY;
        var sx  = window.scrollX;
        var vw  = window.innerWidth;

        var left = rect.left + sx + rect.width / 2 - pw / 2;
        left = Math.max(sx + GAP, Math.min(left, sx + vw - pw - GAP));

        var top = rect.top + sy - ph - GAP;
        if (top < sy + GAP) {
            top = rect.bottom + sy + GAP;
            popup.classList.add('is-below');
        } else {
            popup.classList.remove('is-below');
        }

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

    function buildHTML(context, oneWord, codeLang) {
        if (context === 'code') {
            var provider = codeLang ? DOC_PROVIDERS[codeLang] : null;
            return btn('copy', 'Copy')
                 + (provider ? docsBtn(provider) : '');
        }

        if (context === 'math') {
            /* Alphabetical: Copy · nLab · OEIS · Wolfram */
            return btn('copy',    'Copy')
                 + btn('nlab',    'nLab')
                 + btn('oeis',    'OEIS')
                 + btn('wolfram', 'Wolfram');
        }

        /* Prose: Annotate · BibTeX · Copy · [Define] · DuckDuckGo · Here · Wikipedia */
        return btn('annotate',  'Annotate')
             + btn('cite',      'BibTeX')
             + btn('copy',      'Copy')
             + (oneWord ? btn('define', 'Define') : '')
             + btn('search',    'DuckDuckGo')
             + btn('here',      'Here')
             + btn('wikipedia', 'Wikipedia');
    }

    function btn(action, label, placeholder) {
        var cls   = 'selection-popup-btn' + (placeholder ? ' selection-popup-btn--placeholder' : '');
        var extra = placeholder ? ' aria-disabled="true" title="Coming soon"' : '';
        return '<button class="' + cls + '" data-action="' + action + '"' + extra + '>'
             + label + '</button>';
    }


    /* Docs button embeds the base URL so dispatch can read it without a lookup. */
    function docsBtn(provider) {
        return '<button class="selection-popup-btn" data-action="docs"'
             + ' data-docs-url="' + provider.url + '">'
             + provider.label + '</button>';
    }


    /* ------------------------------------------------------------------
       Action bindings
    ------------------------------------------------------------------ */

    function bindActions(text, rect) {
        popup.querySelectorAll('[data-action]').forEach(function (el) {
            if (el.getAttribute('aria-disabled') === 'true') return;
            el.addEventListener('click', function () {
                dispatch(el.getAttribute('data-action'), text, el, rect);
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

    /* "Jane Doe" → "Doe, Jane" */
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

    function dispatch(action, text, el, rect) {
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

        } else if (action === 'annotate') {
            showAnnotatePicker(text, rect);
        }
    }

    /* ------------------------------------------------------------------
       Annotate picker — color swatches + optional note input
    ------------------------------------------------------------------ */

    function showAnnotatePicker(text, selRect) {
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
            if (window.Annotations) {
                window.Annotations.add(text, pickerColor, note.value.trim());
            }
            hidePicker();
        }

        submitBtn.addEventListener('click', commit);
        note.addEventListener('keydown', function (e) {
            if (e.key === 'Enter')  { e.preventDefault(); commit(); }
            if (e.key === 'Escape') { hidePicker(); }
        });

        setTimeout(function () { note.focus(); }, 0);
    }

    function hidePicker() {
        if (picker) picker.classList.remove('is-visible');
    }

    function positionPicker(rect) {
        var pw  = picker.offsetWidth;
        var ph  = picker.offsetHeight;
        var GAP = 8;
        var sy  = window.scrollY;
        var sx  = window.scrollX;
        var vw  = window.innerWidth;

        var left = rect.left + sx + rect.width / 2 - pw / 2;
        left = Math.max(sx + GAP, Math.min(left, sx + vw - pw - GAP));

        var top = rect.top + sy - ph - GAP;
        if (top < sy + GAP) top = rect.bottom + sy + GAP;

        picker.style.left = left + 'px';
        picker.style.top  = top  + 'px';
    }

    function swatchBtn(color) {
        return '<button class="ann-picker-swatch ann-picker-swatch--' + color
            + '" data-color="' + color + '" aria-label="' + color + '"></button>';
    }

    document.addEventListener('DOMContentLoaded', init);
}());
