/* popups/epistemic.js — the epistemic block's previews and its terms' definitions.
   With the epistemic switch: served after popups.js, in the same file
   (EngineFiles.jsModules), and bound through window.ozyPopups. */
(function () {
    'use strict';

    var P = window.ozyPopups;
    if (!P) return;   /* popups.js did not run (a touch-only device returns before binding) */

    function esc(s) {
        return window.ozyUtils.escapeHtml(s);
    }

    /* Epistemic term definitions — concise summaries,
       shown on hover for filter labels, metadata strip items, etc. */
    var EP_DEFS = {
        status:       'A controlled vocabulary describing where the work stands: Draft, Working model, Durable, Refined, Superseded, or Deprecated.',
        /* templates/partials/metadata-header.html tags this one too; it
           had no entry, so hovering it showed nothing. */
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

    /* Epistemic term definition — shows a concise description for any
       element tagged with data-ep-term="<field>", and a link to the page that
       explains them all, if the site names one. */
    function epistemicTermContent(target) {
        var term = target.dataset.epTerm;
        var def  = term && EP_DEFS[term];
        if (!def) return Promise.resolve(null);
        var words = term.replace(/-/g, ' ');           /* peer-status: "Peer status" */
        var label = words.charAt(0).toUpperCase() + words.slice(1);
        var site  = window.ozySite;
        return Promise.resolve(
            '<div class="popup-ep-term">'
            + (site.epistemicAbout
                ? '<div class="popup-source" data-popup-source="about">'
                  + '<a href="' + esc(site.epistemicAbout) + '">' + esc(site.epistemicAboutLabel) + '</a>'
                  + '</div>'
                : '')
            + '<div class="popup-title">' + esc(label) + '</div>'
            + '<div class="popup-abstract">' + esc(def) + '</div>'
            + '</div>'
        );
    }

    /* Epistemic term definitions — filter labels, metadata strip, footer */
    P.bind(10, '[data-ep-term]', epistemicTermContent);

    /* Epistemic jump link — preview of the status/confidence/dot block */
    P.bind(30, 'a[href="#epistemic"]', epistemicContent);
}());
