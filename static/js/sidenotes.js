/* sidenotes.js — Notes beside the text, and a reading sheet on narrow screens.
 *
 * HTML structure produced by Filters/Sidenotes.hs:
 *   <sup class="sidenote-ref" id="snref-a"><a href="#fn-a">a</a></sup>
 *   …
 *   <section class="footnotes"><ol class="footnotes-list">
 *     <li id="fn-a" class="footnote-item">…<a href="#snref-a" class="footnote-back">↩︎</a></li>
 *   </ol></section>
 *
 * Each note exists once, as that list item. Without this script, and on
 * paper, the list is the notes. With it:
 *   1. Where the room beside the text holds a readable measure, the script
 *      sets data-sidenotes="margin" on #markdownBody and each item goes in
 *      the right margin, level with its reference, pushed below the note
 *      before it if they would overlap. A note whose reference is hidden
 *      (in a closed section) is hidden with it. The body is kept at least
 *      as tall as its notes, so the last one cannot run over the footer.
 *      A note pushed far below its reference rises beside it, over its
 *      neighbors, while it is the active one.
 *   2. Elsewhere (data-sidenotes="list"): activating a reference lends its
 *      item to a modal sheet; the item goes back when the sheet closes.
 *   3. A reference is paired with its note through its href, and the note
 *      with its reference through the back-link. transclude.js renames
 *      ids and the links to them together, so the pair survives.
 *
 * #markdownBody must be position: relative (layout.css guarantees this).
 */

(function () {
    'use strict';

    const GAP = 12; /* minimum px gap between successive sidenotes */

    /* The margin is used when the room beside the text holds a readable
       measure: about thirty characters of note text, 14em at the notes'
       0.82rem (sidenotes.css). In rem, so it follows the reader's text
       size, which a media query cannot see (it reads the browser's
       default). Once in the margin the notes stay there until the room
       is HYSTERESIS_PX narrower: the layout changes the page's height, a
       scrollbar can come or go with it, and that moves the centered body
       by half the scrollbar's width (at most about 9px). Without the
       band the layout could flip back and forth. */
    const MIN_ROOM_REM  = 11.5;
    const HYSTERESIS_PX = 12;
    /* The note's gap from the text (left: 100% + 1.5rem) and the page's
       padding (--page-padding) come off the room. */
    const OUTSIDE_REM    = 3;

    /* A note pushed more than this many of its own lines below its
       reference has come loose from it: while it is the active note it
       rises to its reference's line, over the notes around it. */
    const DETACHED_LINES = 3;

    /* Reference <sup> → pair, and note <li> → the same pair. A WeakMap,
       not a data attribute: an attribute travels with copied markup and
       claims listeners the copy never had. */
    const pairs = new WeakMap();

    /* Watches the body and every note; any change of size repositions. */
    const noteObserver = window.ResizeObserver
        ? new ResizeObserver(function () { schedulePosition(); })
        : null;

    function marginLayout(body) {
        return body.dataset.sidenotes === 'margin';
    }

    /* Margin or list, from the room the text leaves to its right. */
    function chooseLayout(body) {
        const rem  = parseFloat(getComputedStyle(document.documentElement).fontSize) || 16;
        const room = document.documentElement.clientWidth
                     - body.getBoundingClientRect().right - OUTSIDE_REM * rem;
        const need = MIN_ROOM_REM * rem - (marginLayout(body) ? HYSTERESIS_PX : 0);
        const margin = room >= need;

        if (marginLayout(body) && !margin) unpin();
        body.dataset.sidenotes = margin ? 'margin' : 'list';
        if (margin) body.style.setProperty('--sidenote-room', Math.floor(room) + 'px');
        return margin;
    }

    function targetOf(link) {
        const href = link && link.getAttribute('href');
        if (!href || href.charAt(0) !== '#') return null;
        try { return document.getElementById(decodeURIComponent(href.slice(1))); }
        catch (_) { return null; }
    }

    /* Baselines. A character's box runs from its font's ascent above the
       baseline to its descent below, so the baseline is the box's top
       plus the ascent, which canvas text metrics give for any font.
       Measured, not probed: nothing is inserted into the text, which
       would disturb a reader's selection. Ascents are read afresh each
       pass, since a web font arriving changes them. */
    const metrics = document.createElement('canvas').getContext('2d');

    function baselineOf(textNode, offset, ascents) {
        const range = document.createRange();
        range.setStart(textNode, offset);
        range.setEnd(textNode, offset + 1);
        const r = range.getBoundingClientRect();
        if (!r.height) return null;
        const cs = getComputedStyle(textNode.parentElement);
        const font = cs.fontStyle + ' ' + cs.fontWeight + ' ' + cs.fontSize + ' ' + cs.fontFamily;
        if (!ascents.has(font)) {
            metrics.font = font;
            const a = metrics.measureText('x').fontBoundingBoxAscent;
            ascents.set(font, a === undefined ? 0.8 * parseFloat(cs.fontSize) : a);
        }
        return r.top + ascents.get(font);
    }

    /* The baseline of the line a reference sits on, in viewport
       coordinates, from the last character of text before it that sits
       on the line: the reference itself is raised, and so is a citation
       marker or any other superscript just before it. Nothing breaks a
       line between a word and what follows it, so that character is on
       the same line. */
    function refBaseline(ref, ascents) {
        const block = ref.closest('p, li, dd, dt, td, th, blockquote, figcaption') || ref.parentNode;
        const walker = document.createTreeWalker(block, NodeFilter.SHOW_TEXT);
        walker.currentNode = ref;
        let node;
        while ((node = walker.previousNode())) {
            const end = node.data.trimEnd().length;
            if (!end || node.parentElement.closest('sup, sub')) continue;
            const y = baselineOf(node, end - 1, ascents);
            if (y !== null) return y;
        }
        return ref.getBoundingClientRect().bottom;
    }

    /* How far below a note's top its first line of text has its
       baseline; the hanging letter is not the text. */
    function firstBaseline(note, ascents) {
        const walker = document.createTreeWalker(note, NodeFilter.SHOW_TEXT, {
            acceptNode: function (n) {
                return n.parentElement.closest('.footnote-label') || !n.data.trim()
                    ? NodeFilter.FILTER_SKIP : NodeFilter.FILTER_ACCEPT;
            }
        });
        const node = walker.nextNode();
        const y = node ? baselineOf(node, node.data.search(/\S/), ascents) : null;
        return y === null ? 0 : y - note.getBoundingClientRect().top;
    }

    function inView(el) {
        const r = el.getBoundingClientRect();
        return r.top >= 0 && r.bottom <= window.innerHeight;
    }

    /* ------------------------------------------------------------------ */
    /*  Margin layout                                                      */
    /* ------------------------------------------------------------------ */

    function positionSidenotes() {
        const body = document.getElementById('markdownBody');
        if (!body || !chooseLayout(body)) return;

        /* A note is in one place at a time: widening the window past the
           breakpoint takes it out of the sheet and back to the margin. */
        if (sheetPair) closeSheet();

        const list = [];
        body.querySelectorAll('.sidenote-ref').forEach(function (ref) {
            const pair = pairs.get(ref);
            if (pair) list.push(pair);
        });
        if (list.length === 0) return;

        /* Read everything, then write everything: one layout per pass,
           not one per note. A reference in a closed section has no boxes. */
        const shown = list.map(function (pair) { return pair.ref.getClientRects().length > 0; });
        list.forEach(function (pair, i) { pair.note.classList.toggle('is-unplaced', !shown[i]); });

        const placed = list.filter(function (_, i) { return shown[i]; });
        /* A note's first line shares its reference line's baseline, so
           the two read as one line across the gutter. */
        const ascents = new Map();
        const sizes = placed.map(function (pair) {
            const box = pair.note.offsetParent || body;
            return {
                want:   refBaseline(pair.ref, ascents) - firstBaseline(pair.note, ascents)
                        - box.getBoundingClientRect().top - box.clientTop,
                height: pair.note.offsetHeight,
                line:   parseFloat(getComputedStyle(pair.note).lineHeight) || 0
            };
        });

        /* Each note's place in the column (slot) is where it would be if
           none were raised, so raising one moves no other. */
        let prevBottom = 0;
        let extent = 0;
        placed.forEach(function (pair, i) {
            const size = sizes[i];
            pair.want = size.want;
            pair.slot = Math.max(size.want, prevBottom + GAP);
            pair.detached = pair.slot - size.want > DETACHED_LINES * size.line;
            prevBottom = pair.slot + size.height;
            const top = pair.raised ? pair.want : pair.slot;
            pair.note.style.top = top + 'px';
            extent = Math.max(extent, top + size.height);
        });

        /* Absolutely positioned, the notes add nothing to the body's
           height: a long note near the end ran on over the footer. The
           body is made at least as tall as its notes (sidenotes.css). */
        body.style.setProperty('--sidenote-extent', Math.ceil(extent + GAP) + 'px');
    }

    /* The positions follow the laid-out text, which keeps moving after
       DOMContentLoaded: web fonts swap in (on a cold load notes ended up to
       725 px from their marks, some overlapping; audit J03), images and
       KaTeX arrive, sections collapse, transclusions land. Whatever asks,
       one pass per frame. */
    let frame = 0;
    function schedulePosition() {
        if (frame) return;
        frame = requestAnimationFrame(function () {
            frame = 0;
            positionSidenotes();
        });
    }

    /* ------------------------------------------------------------------ */
    /*  Hover, activation, keyboard                                        */
    /* ------------------------------------------------------------------ */

    /* At most one pair is pinned (click-sticky) at a time. */
    let pinned = null;

    function update(pair) {
        const active = pair.hover || pinned === pair;
        pair.ref.classList.toggle('is-active', active);
        pair.note.classList.toggle('is-active', active);
    }

    function pin(pair) {
        const was = pinned;
        pinned = pair;
        if (was && was !== pair) {
            lower(was);
            update(was);
        }
        if (pair.detached) raise(pair);
        update(pair);
    }

    function unpin() {
        const was = pinned;
        pinned = null;
        if (was) {
            lower(was);
            update(was);
        }
    }

    /* A detached note, while active, sits on its reference's line. */
    function raise(pair) {
        pair.raised = true;
        pair.note.classList.add('is-raised');
        pair.note.style.top = pair.want + 'px';
    }

    function lower(pair) {
        if (!pair.raised) return;
        pair.raised = false;
        pair.note.classList.remove('is-raised');
        pair.note.style.top = pair.slot + 'px';
    }

    /* A reference leads to its note. In the margin the note is pinned
       (raised, if it had come loose) and takes focus, so a keyboard or
       screen-reader user continues from it; its back-link or Escape
       returns. Without the margin it opens in the sheet. A second
       activation in the margin lets go. */
    function activate(pair) {
        if (!marginLayout(document.getElementById('markdownBody'))) {
            openSheet(pair);
            return;
        }
        if (pinned === pair) {
            unpin();
            return;
        }
        pin(pair);
        pair.note.focus({ preventScroll: true });
        if (!inView(pair.note)) pair.note.scrollIntoView({ block: 'nearest' });
    }

    function wire(pair) {
        [pair.ref, pair.note].forEach(function (el) {
            el.addEventListener('mouseenter', function () { pair.hover = true;  update(pair); });
            el.addEventListener('mouseleave', function () { pair.hover = false; update(pair); });
        });

        pair.link.addEventListener('click', function (e) {
            if (e.metaKey || e.ctrlKey || e.shiftKey || e.altKey) return;
            e.preventDefault();
            activate(pair);
        });

        /* Enter follows a link, and so arrives above as a click; Space does
           not, and a reference should answer to both. */
        pair.link.addEventListener('keydown', function (e) {
            if (e.key === ' ') {
                e.preventDefault();
                activate(pair);
            }
        });

        /* In the list the back-link is an ordinary jump. In the sheet it
           closes the sheet; in the margin, where the reference is beside
           the note, it returns focus without moving the page. */
        if (pair.back) {
            pair.back.addEventListener('click', function (e) {
                if (sheetPair === pair) {
                    e.preventDefault();
                    closeSheet();
                    return;
                }
                if (!marginLayout(document.getElementById('markdownBody'))) return;
                e.preventDefault();
                unpin();
                pair.link.focus({ preventScroll: true });
                if (!inView(pair.ref)) pair.ref.scrollIntoView({ block: 'center' });
            });
        }
    }

    /* Idempotent: a reference already paired is skipped, so
       reinitSidenotes after a transclusion cannot stack listeners. */
    function wireAll(root) {
        root.querySelectorAll('.sidenote-ref').forEach(function (ref) {
            if (pairs.has(ref)) return;
            const link = ref.querySelector('a');
            const note = targetOf(link);
            if (!note || !note.classList.contains('footnote-item')) return;
            const pair = {
                ref:   ref,
                link:  link,
                note:  note,
                back:  note.querySelector('.footnote-back'),
                hover: false
            };
            pairs.set(ref, pair);
            pairs.set(note, pair);
            note.setAttribute('tabindex', '-1');
            wire(pair);
            /* A note that grows after it was placed (math typeset, an
               image arriving, a font swapping in) would run into the one
               below it. */
            if (noteObserver) noteObserver.observe(note);
        });
    }

    /* Click anywhere outside a pinned pair lets it go. */
    document.addEventListener('click', function (e) {
        if (pinned &&
            !pinned.ref.contains(e.target) &&
            !pinned.note.contains(e.target)) {
            unpin();
        }
    });

    /* Escape lets go of a pinned note, and returns focus to its reference
       if it was inside the note. */
    document.addEventListener('keydown', function (e) {
        if (e.key !== 'Escape' || !pinned) return;
        const pair = pinned;
        unpin();
        if (pair.note.contains(document.activeElement)) pair.link.focus({ preventScroll: true });
    });

    /* ------------------------------------------------------------------ */
    /*  Reading sheet — narrow screens, where the notes are a list at the  */
    /*  end. A modal <dialog>: the page behind is inert, so focus and the  */
    /*  screen reader stay in the sheet. Created lazily on first use.      */
    /* ------------------------------------------------------------------ */

    let sheet     = null;
    let sheetPair = null;   /* the pair whose note the sheet holds */
    let sheetHome = null;   /* marks the note's place in the list */

    function ensureSheet() {
        if (sheet) return;

        sheet = document.createElement('dialog');
        sheet.className = 'sidenote-popup';

        const bar = document.createElement('div');
        bar.className = 'sidenote-popup-bar';

        /* Names the sheet: "Note a". The note's own letter is hidden in
           the sheet (sidenotes.css). */
        const title = document.createElement('span');
        title.className = 'sidenote-popup-title';
        title.id = 'sidenote-popup-title';
        sheet.setAttribute('aria-labelledby', title.id);

        const closeBtn = document.createElement('button');
        closeBtn.type = 'button';
        closeBtn.className = 'sidenote-popup-close';
        closeBtn.setAttribute('aria-label', 'Close note');
        closeBtn.textContent = '×'; /* × */

        const body = document.createElement('ol');
        body.className = 'sidenote-popup-body';

        bar.appendChild(title);
        bar.appendChild(closeBtn);
        sheet.appendChild(bar);
        sheet.appendChild(body);
        document.body.appendChild(sheet);

        closeBtn.addEventListener('click', closeSheet);

        /* A click on the backdrop lands on the dialog itself, outside its box. */
        sheet.addEventListener('click', function (e) {
            if (e.target !== sheet) return;
            const r = sheet.getBoundingClientRect();
            if (e.clientX < r.left || e.clientX > r.right ||
                e.clientY < r.top  || e.clientY > r.bottom) closeSheet();
        });

        /* Escape closes a modal dialog by itself; however it closed, the
           note goes home. */
        sheet.addEventListener('close', returnNote);
    }

    function openSheet(pair) {
        ensureSheet();
        if (sheetPair) returnNote();

        sheetPair = pair;
        sheetHome = document.createComment(' note ');
        pair.note.parentNode.insertBefore(sheetHome, pair.note);
        sheet.querySelector('.sidenote-popup-body').appendChild(pair.note);
        sheet.querySelector('.sidenote-popup-title').textContent = 'Note ' + pair.link.textContent.trim();

        /* The page keeps its place underneath. */
        document.documentElement.style.overflow = 'hidden';
        sheet.showModal();
        /* Each note opens at its beginning, not where the last one was left. */
        sheet.scrollTop = 0;
        pair.note.focus({ preventScroll: true });
    }

    function closeSheet() {
        if (!sheet || !sheet.open) return;
        sheet.close();
        returnNote();
    }

    /* Focus goes back to the reference that opened the note; it used to
       stay on the closed sheet, and a keyboard or screen-reader user
       started again from the top of the page. */
    function returnNote() {
        if (!sheetPair) return;
        const pair = sheetPair;
        sheetPair = null;
        sheetHome.parentNode.replaceChild(pair.note, sheetHome);
        sheetHome = null;
        document.documentElement.style.overflow = '';
        pair.link.focus({ preventScroll: true });
    }

    /* ------------------------------------------------------------------ */
    /*  Links to a note                                                    */
    /* ------------------------------------------------------------------ */

    /* A link to a note (#fn-a, shared or bookmarked) names its list item.
       In the margin the item is set beside its reference only after the
       browser has scrolled to where the list was: aim again, and mark the
       note that was meant. A reference in a closed section hides its
       note, so that section opens first (collapse.js). In the list the
       browser's own scroll is right, and nothing needs opening. */
    function revealTarget(initial) {
        let id = '';
        try { id = decodeURIComponent(location.hash.slice(1)); }
        catch (_) { return; }
        const note = id ? document.getElementById(id) : null;
        const pair = note ? pairs.get(note) : null;
        if (!pair || pair.note !== note) return;
        if (!marginLayout(document.getElementById('markdownBody'))) return;

        let section = pair.ref.closest('.section-body');
        while (section) {
            if (section.expandSection) section.expandSection();
            section = section.parentElement && section.parentElement.closest('.section-body');
        }

        requestAnimationFrame(function () {
            positionSidenotes();
            if (!marginLayout(document.getElementById('markdownBody'))) return;
            pin(pair);
            note.scrollIntoView({ block: 'start', behavior: initial ? 'instant' : 'auto' });
        });
    }

    function init() {
        const body = document.getElementById('markdownBody');
        if (!body) return;

        wireAll(body);
        positionSidenotes();
        revealTarget(true);

        if (document.fonts && document.fonts.ready) document.fonts.ready.then(schedulePosition);
        window.addEventListener('load', schedulePosition);
        /* Text moving, the reader's text size, and the notes' extent all
           change the body's size. Moving notes does not, and the extent
           settles in one more pass: no loop. */
        if (noteObserver) noteObserver.observe(body);
    }

    /* Public re-init hook used by transclude.js after it injects new
       content. wireAll is idempotent so calling this multiple times is
       safe. */
    window.reinitSidenotes = function (container) {
        wireAll(container || document.getElementById('markdownBody') || document);
        positionSidenotes();
    };

    document.addEventListener('DOMContentLoaded', init);
    window.addEventListener('resize', schedulePosition);
    window.addEventListener('hashchange', function () { revealTarget(false); });
    /* Paper gets the list, whole. */
    window.addEventListener('beforeprint', closeSheet);
}());
