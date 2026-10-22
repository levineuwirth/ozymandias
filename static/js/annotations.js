/* annotations.js — localStorage-based personal highlights and annotations.
   Persists across sessions via localStorage. Re-anchors on page load by
   finding the highlighted text again in a TreeWalker text stream, at the
   place whose surrounding text matches what was stored with it.

   Public API (window.Annotations):
     .add(text, color, note, range) → ann object, or null
     .remove(id)
     .clearAll()
*/
(function () {
    'use strict';

    var STORAGE_KEY = 'site-annotations';
    var COLORS      = ['amber', 'sage', 'steel', 'rose'];
    var CONTEXT     = 32;   /* characters of surrounding text kept each side */
    var tooltip     = null;
    var tooltipTimer = null;
    var tooltipPinned = false; /* keyboard-opened: blur must not dismiss */
    var tooltipMark   = null;  /* mark that opened the tooltip, for focus return */
    var returningFocus = false; /* focus going back to that mark: do not reopen */

    /* ------------------------------------------------------------------
       Storage
    ------------------------------------------------------------------ */

    /* A stored highlight is used only in the shape written here; any other
       entry is dropped, and gone from storage at the next write. A value
       that was not a list used to throw at every load and every
       ln:content-added, so no highlight showed on any page, and Annotate
       failed without its message. */
    function wellFormed(a) {
        return a !== null && typeof a === 'object'
            && typeof a.id === 'string' && a.id !== ''
            && typeof a.url === 'string' && typeof a.text === 'string';
    }

    function loadAll() {
        var list;
        try { list = JSON.parse(localStorage.getItem(STORAGE_KEY)); }
        catch (e) { return []; }
        return Array.isArray(list) ? list.filter(wellFormed) : [];
    }

    function str(v) { return typeof v === 'string' ? v : ''; }

    /* Every <mark> of one highlight: a highlight across elements is
       several. Ids are escaped: one is only as safe as storage. */
    function marksOf(id) {
        return document.querySelectorAll(
            'mark.user-annotation[data-ann-id="' + CSS.escape(id) + '"]');
    }

    function saveAll(list) {
        try { localStorage.setItem(STORAGE_KEY, JSON.stringify(list)); }
        catch (e) {}
    }

    /* C03: an annotation is keyed by the page's URL, and a directory-routed
       page has two spellings of it — /essays/x/ and /essays/x/index.html.
       New annotations are written under the canonical one; both are read,
       so highlights made before this fix are still found. */
    function pagePaths() {
        return window.lnUtils && window.lnUtils.pagePaths
            ? window.lnUtils.pagePaths()
            : { current: location.pathname, legacy: null };
    }

    function forPage() {
        var paths = pagePaths();
        return loadAll().filter(function (a) {
            return a.url === paths.current
                || (paths.legacy !== null && a.url === paths.legacy);
        });
    }

    function uid() {
        return Date.now().toString(36) + Math.random().toString(36).slice(2, 8);
    }

    /* ------------------------------------------------------------------
       CRUD
    ------------------------------------------------------------------ */

    function addRaw(ann) {
        var list = loadAll();
        list.push(ann);
        saveAll(list);
    }

    function removeById(id) {
        saveAll(loadAll().filter(function (a) { return a.id !== id; }));
        marksOf(id).forEach(function (mark) {
            var parent = mark.parentNode;
            if (!parent) return;
            while (mark.firstChild) parent.insertBefore(mark.firstChild, mark);
            parent.removeChild(mark);
            parent.normalize();
        });
        hideTooltip(true);
    }

    /* ------------------------------------------------------------------
       Text stream — the visible text of root as one string, with every
       run of whitespace one space, and where each character came from.

       Both sides are compared with every run of whitespace as one space.
       Pandoc keeps a paragraph's source line breaks as newlines inside its
       text, and Selection.toString() gives them back as spaces, so a
       selection across a line break never matched: the highlight was
       stored but never shown (audit J02; 258 of 1,958 prose paragraphs).
       at[i] is the [node, offset] of full[i]; held[i] is true where that
       character is already highlighted. Highlighted text stays in the
       stream, so the text around a highlight reads the same whichever
       others are on the page.
    ------------------------------------------------------------------ */

    function collapse(text) {
        return text.replace(/\s+/g, ' ').trim();
    }

    function textStream(root) {
        var walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT, null);
        var full = '';
        var at   = [];
        var held = [];
        var lastWasSpace = false;
        var node;
        while ((node = walker.nextNode())) {
            var inMark = !!(node.parentElement && node.parentElement.closest('mark.user-annotation'));
            var v = node.nodeValue;
            for (var j = 0; j < v.length; j++) {
                var white = /\s/.test(v[j]);
                /* Reading the growing string with charAt here repeatedly
                   flattens it in V8: a long page can freeze for seconds.
                   Track this one bit instead, across text-node boundaries. */
                if (white && lastWasSpace) continue;
                lastWasSpace = white;
                full += white ? ' ' : v[j];
                at.push([node, j]);
                held.push(inMark);
            }
        }
        return { full: full, at: at, held: held };
    }

    /* The stream index of the first character at or after the start of
       range (a reader's selection): the characters are in document order,
       so a binary search over Range.comparePoint. */
    function streamIndex(stream, range) {
        var lo = 0, hi = stream.at.length;
        while (lo < hi) {
            var mid = (lo + hi) >> 1;
            if (range.comparePoint(stream.at[mid][0], stream.at[mid][1]) < 0) lo = mid + 1;
            else hi = mid;
        }
        return lo;
    }

    function commonTail(a, b) {
        var n = 0;
        while (n < a.length && n < b.length && a[a.length - 1 - n] === b[b.length - 1 - n]) n++;
        return n;
    }

    function commonHead(a, b) {
        var n = 0;
        while (n < a.length && n < b.length && a[n] === b[n]) n++;
        return n;
    }

    /* Where needle (collapsed) occurs clear of existing highlights. With
       `near`, the reader's selection says where: the occurrence starting
       there, or one character on, past a space the selection began with.
       Without it, the occurrence whose surroundings best match the stored
       prefix and suffix; the first among equals, which for a highlight
       stored with neither is the first. Highlights used to take the first
       occurrence always: a phrase selected in the fourth paragraph was
       highlighted in the first, and found there again on every load.
       -1 if there is none. */
    function locate(stream, needle, near, prefix, suffix) {
        var full = stream.full;
        var best = -1, bestScore = -1;
        for (var idx = full.indexOf(needle); idx !== -1; idx = full.indexOf(needle, idx + 1)) {
            if (stream.held.slice(idx, idx + needle.length).indexOf(true) !== -1) continue;
            if (near !== null) {
                if (idx === near || idx === near + 1) return idx;
                continue;
            }
            var end   = idx + needle.length;
            var score = commonTail(full.slice(Math.max(0, idx - prefix.length), idx), prefix)
                      + commonHead(full.slice(end, end + suffix.length), suffix);
            if (score > bestScore) { best = idx; bestScore = score; }
        }
        return best;
    }

    function rangeAt(stream, idx, length) {
        var first = stream.at[idx];
        var last  = stream.at[idx + length - 1];
        var range = document.createRange();
        range.setStart(first[0], first[1]);
        range.setEnd(last[0], last[1] + 1);
        return range;
    }

    /* ------------------------------------------------------------------
       Apply a single annotation to the DOM
    ------------------------------------------------------------------ */

    function annotationRoot() {
        return document.getElementById('markdownBody') || document.body;
    }

    function isBlock(node) {
        return !!node && node.nodeType === 1 && !/^inline/.test(getComputedStyle(node).display);
    }

    /* Text outside HTML (SVG, MathML) cannot hold a <mark>, and the
       whitespace between two blocks is not part of either. */
    function wrappable(node, from, to) {
        var parent = node.parentNode;
        if (!parent || parent.namespaceURI !== 'http://www.w3.org/1999/xhtml') return false;
        if (/\S/.test(node.nodeValue.slice(from, to))) return true;
        return !isBlock(node.previousSibling) && !isBlock(node.nextSibling);
    }

    /* Wrap the text range covers in marks, one for each text node it
       touches, leaving elements where they are. A range across elements
       used to be extracted and put back inside one inline <mark>: across
       a paragraph break that left cloned halves of both paragraphs inside
       it, between the originals, and two paragraphs became four on every
       load. Deleting unwraps each mark, and the text nodes join again.
       The range starts and ends on text (rangeAt). Returns the marks. */
    function wrap(range, ann) {
        var sc = range.startContainer, so = range.startOffset;
        var ec = range.endContainer,   eo = range.endOffset;
        var nodes = [sc];
        if (sc !== ec) {
            var walker = document.createTreeWalker(range.commonAncestorContainer,
                                                   NodeFilter.SHOW_TEXT, null);
            walker.currentNode = sc;
            var n;
            while ((n = walker.nextNode())) {
                nodes.push(n);
                if (n === ec) break;
            }
        }
        var pieces = [];
        nodes.forEach(function (node) {
            var from = node === sc ? so : 0;
            var to   = node === ec ? eo : node.nodeValue.length;
            if (from < to && wrappable(node, from, to)) pieces.push([node, from, to]);
        });

        var color = COLORS.indexOf(ann.color) !== -1 ? ann.color : COLORS[0];
        return pieces.map(function (piece, i) {
            var node = piece[0];
            if (piece[2] < node.nodeValue.length) node.splitText(piece[2]);
            if (piece[1] > 0) node = node.splitText(piece[1]);
            var mark = document.createElement('mark');
            mark.className = 'user-annotation user-annotation--' + color
                + (i > 0 ? ' ann-joins-prev' : '')
                + (i < pieces.length - 1 ? ' ann-joins-next' : '');
            mark.setAttribute('data-ann-id', ann.id);
            if (ann.note) mark.setAttribute('data-note', ann.note);
            mark.setAttribute('data-created', ann.created || '');
            node.parentNode.insertBefore(mark, node);
            mark.appendChild(node);
            /* One tab stop for the highlight, on its first mark. */
            bindMarkEvents(mark, ann, i === 0);
            return mark;
        });
    }

    function applyAnnotation(ann) {
        var needle = collapse(ann.text);
        if (!needle) return false;
        var stream = textStream(annotationRoot());
        var idx = locate(stream, needle, null, str(ann.prefix), str(ann.suffix));
        if (idx === -1) return false;
        return wrap(rangeAt(stream, idx, needle.length), ann).length > 0;
    }

    /* Re-anchor every stored annotation for this page that is not already
       on screen. Idempotent, so it is safe to call again after content is
       injected: an annotation whose <mark> already exists is skipped, and
       one whose text only just arrived (inside a transclusion) is anchored
       now. Smaller finding 7. */
    function applyAll() {
        forPage().forEach(function (ann) {
            if (marksOf(ann.id).length) return;
            applyAnnotation({
                id: ann.id, text: ann.text, color: ann.color,
                note: str(ann.note), created: str(ann.created),
                prefix: ann.prefix, suffix: ann.suffix,
            });
        });
    }

    /* ------------------------------------------------------------------
       Tooltip
    ------------------------------------------------------------------ */

    function initTooltip() {
        tooltip = document.createElement('div');
        tooltip.className = 'ann-tooltip';
        tooltip.setAttribute('role', 'tooltip');
        document.body.appendChild(tooltip);

        tooltip.addEventListener('mouseenter', function () { clearTimeout(tooltipTimer); });
        tooltip.addEventListener('mouseleave', function () { hideTooltip(false); });

        /* Keyboard flow: Escape closes a pinned tooltip and returns focus
           to its mark; tabbing out of the tooltip dismisses it. */
        tooltip.addEventListener('keydown', function (e) {
            if (e.key === 'Escape') {
                hideTooltip(true);
                /* The mark's focus handler shows the tooltip; returning
                   focus to it used to reopen what Escape had just closed. */
                if (tooltipMark) {
                    returningFocus = true;
                    tooltipMark.focus();
                    returningFocus = false;
                }
            }
        });
        tooltip.addEventListener('focusout', function (e) {
            if (!tooltip.contains(e.relatedTarget)) hideTooltip(false);
        });
    }

    /* Defer to the shared utility (loaded synchronously from
       templates/partials/head.html) so this file cannot drift from
       popups.js, semantic-search.js, or build/Utils.hs. */
    function escHtml(s) {
        return window.lnUtils.escapeHtml(s);
    }

    function showTooltip(mark, ann) {
        clearTimeout(tooltipTimer);
        tooltipPinned = false;
        tooltipMark   = mark;

        var note    = ann.note    || '';
        var created = ann.created ? new Date(ann.created).toLocaleDateString() : '';

        tooltip.innerHTML =
            (note ? '<div class="ann-tooltip-note">' + escHtml(note) + '</div>' : '') +
            '<div class="ann-tooltip-meta">' +
            (created ? '<span class="ann-tooltip-date">' + escHtml(created) + '</span>' : '') +
            '<button class="ann-tooltip-delete" data-ann-id="' + escHtml(ann.id) + '">Delete</button>' +
            '</div>';

        tooltip.querySelector('.ann-tooltip-delete').addEventListener('click', function () {
            removeById(ann.id);
        });

        /* Measure then position */
        tooltip.style.visibility = 'hidden';
        tooltip.classList.add('is-visible');

        var rect = mark.getBoundingClientRect();
        var tw   = tooltip.offsetWidth;
        var th   = tooltip.offsetHeight;
        var sx   = window.scrollX, sy = window.scrollY;
        var vw   = window.innerWidth;

        var left = rect.left + sx + rect.width / 2 - tw / 2;
        left = Math.max(sx + 8, Math.min(left, sx + vw - tw - 8));

        var top = rect.top + sy - th - 8;
        if (top < sy + 8) top = rect.bottom + sy + 8;

        tooltip.style.left = left + 'px';
        tooltip.style.top  = top  + 'px';
        tooltip.style.visibility = '';
    }

    function hideTooltip(immediate) {
        clearTimeout(tooltipTimer);
        tooltipPinned = false;
        if (immediate) {
            if (tooltip) tooltip.classList.remove('is-visible');
        } else {
            tooltipTimer = setTimeout(function () {
                if (tooltip) tooltip.classList.remove('is-visible');
            }, 120);
        }
    }

    function bindMarkEvents(mark, ann, focusable) {
        mark.addEventListener('mouseenter', function () {
            clearTimeout(tooltipTimer);
            showTooltip(mark, ann);
        });
        mark.addEventListener('mouseleave', function () { hideTooltip(false); });
        if (!focusable) return;

        /* Keyboard: focus mirrors hover; Enter/Space pins the tooltip and
           moves focus to its Delete button; Escape dismisses. */
        mark.setAttribute('tabindex', '0');
        mark.addEventListener('focus', function () {
            if (returningFocus) return;
            clearTimeout(tooltipTimer);
            showTooltip(mark, ann);
        });
        mark.addEventListener('blur', function () {
            if (!tooltipPinned) hideTooltip(false);
        });
        mark.addEventListener('keydown', function (e) {
            if (e.key === 'Enter' || e.key === ' ') {
                e.preventDefault();
                showTooltip(mark, ann);
                tooltipPinned = true;
                var del = tooltip.querySelector('.ann-tooltip-delete');
                if (del) del.focus();
            } else if (e.key === 'Escape') {
                hideTooltip(true);
            }
        });
    }

    /* ------------------------------------------------------------------
       Public API
    ------------------------------------------------------------------ */

    window.Annotations = {
        /* `range`, the reader's selection, says which occurrence of text
           is meant; one outside the page's text is not highlighted. The
           text either side is stored with it, to find the same place
           again on the next load. */
        add: function (text, color, note, range) {
            var root   = annotationRoot();
            var needle = collapse(text || '');
            if (!needle) return null;
            if (range && !root.contains(range.commonAncestorContainer)) return null;
            var stream = textStream(root);
            var idx = locate(stream, needle, range ? streamIndex(stream, range) : null, '', '');
            if (idx === -1) return null;
            var end = idx + needle.length;
            var ann = {
                id:      uid(),
                url:     pagePaths().current,
                text:    text,
                color:   color || 'amber',
                note:    note  || '',
                created: new Date().toISOString(),
                prefix:  stream.full.slice(Math.max(0, idx - CONTEXT), idx),
                suffix:  stream.full.slice(end, end + CONTEXT),
            };
            /* Stored only once it is on the page: an annotation that could
               not be anchored used to be saved anyway, invisible, and
               removable only by clearing them all. */
            if (!wrap(rangeAt(stream, idx, needle.length), ann).length) return null;
            addRaw(ann);
            return ann;
        },
        remove: removeById,
        clearAll: function () {
            saveAll([]);
            document.querySelectorAll('mark.user-annotation').forEach(function (mark) {
                var parent = mark.parentNode;
                if (!parent) return;
                while (mark.firstChild) parent.insertBefore(mark.firstChild, mark);
                parent.removeChild(mark);
                parent.normalize();
            });
            hideTooltip(true);
        },
    };

    document.addEventListener('DOMContentLoaded', function () {
        initTooltip();
        applyAll();
    });

    /* Content injected after load can contain the text a stored annotation
       was anchored to. applyAll() skips annotations already on the page,
       so this only ever adds. */
    document.addEventListener('ln:content-added', function () {
        if (!tooltip) initTooltip();
        applyAll();
    });
}());
