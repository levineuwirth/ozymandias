/* selection-popup/code.js — with the code switch: a documentation search
   for selected code, by its language (Prism's, which Code.hs always puts
   on a block that names one). Served after selection-popup.js, in the
   same file (EngineFiles.scriptModules). */
(function () {
    'use strict';

    var S = window.ozySelection;
    if (!S) return;

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

    /* Docs button embeds the base URL so dispatch can read it without a lookup. */
    function docsBtn(provider) {
        return '<button class="selection-popup-btn" data-action="docs"'
             + ' data-docs-url="' + provider.url + '">'
             + provider.label + '</button>';
    }

    S.codeExtra(function (sel) {
        var lang = getCodeLanguage(sel);
        var provider = lang ? DOC_PROVIDERS[lang] : null;
        return provider ? docsBtn(provider) : '';
    });

    S.action('docs', function (text, el) {
        var base = el.getAttribute('data-docs-url');
        if (base) window.open(base + encodeURIComponent(text), '_blank', 'noopener,noreferrer');
    });
}());
