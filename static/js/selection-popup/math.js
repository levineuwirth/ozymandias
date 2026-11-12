/* selection-popup/math.js — with the math switch: a selection in math gets
   Copy · nLab · OEIS · Wolfram in place of the prose buttons. Served after
   selection-popup.js, in the same file (EngineFiles.scriptModules). */
(function () {
    'use strict';

    var S = window.ozySelection;
    if (!S) return;

    S.context('math', function (el, range) {
        if (el.closest('.math, .katex, .katex-html, .katex-display')) return true;

        /* Fallback: commonAncestorContainer can land at <p> when selection
           starts/ends just outside a KaTeX span — check via intersectsNode. */
        var mathEls = document.querySelectorAll('.math');
        for (var i = 0; i < mathEls.length; i++) {
            if (range.intersectsNode(mathEls[i])) return true;
        }
        return false;
    }, function () {
        /* Alphabetical: Copy · nLab · OEIS · Wolfram */
        return S.btn('copy',    'Copy')
             + S.btn('nlab',    'nLab')
             + S.btn('oeis',    'OEIS')
             + S.btn('wolfram', 'Wolfram');
    });

    function open(url) {
        return function (text) {
            window.open(url + encodeURIComponent(text), '_blank', 'noopener,noreferrer');
        };
    }
    S.action('wolfram', open('https://www.wolframalpha.com/input?i='));
    S.action('oeis',    open('https://oeis.org/search?q='));
    S.action('nlab',    open('https://ncatlab.org/nlab/search?query='));
}());
