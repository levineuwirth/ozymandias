/* annotations.js — localStorage-based personal highlights and annotations.
   Persists across sessions via localStorage. Re-anchors on page load via
   exact text match using a TreeWalker text-stream search.

   Public API (window.Annotations):
     .add(text, color, note) → ann object
     .remove(id)
*/
(function () {
    'use strict';

    var STORAGE_KEY = 'site-annotations';
    var tooltip     = null;
    var tooltipTimer = null;

    /* ------------------------------------------------------------------
       Storage
    ------------------------------------------------------------------ */

    function loadAll() {
        try { return JSON.parse(localStorage.getItem(STORAGE_KEY)) || []; }
        catch (e) { return []; }
    }

    function saveAll(list) {
        try { localStorage.setItem(STORAGE_KEY, JSON.stringify(list)); }
        catch (e) {}
    }

    function forPage() {
        var path = location.pathname;
        return loadAll().filter(function (a) { return a.url === path; });
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
        document.querySelectorAll('mark[data-ann-id="' + id + '"]').forEach(function (mark) {
            var parent = mark.parentNode;
            if (!parent) return;
            while (mark.firstChild) parent.insertBefore(mark.firstChild, mark);
            parent.removeChild(mark);
            parent.normalize();
        });
        hideTooltip(true);
    }

    /* ------------------------------------------------------------------
       Text-stream search — finds the first occurrence of searchText in
       the visible text of root, skipping existing annotation marks.
    ------------------------------------------------------------------ */

    function findTextRange(searchText, root) {
        var walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT, null);
        var nodes  = [];
        var node;
        while ((node = walker.nextNode())) {
            /* Skip text already inside an annotation mark */
            if (node.parentElement && node.parentElement.closest('mark.user-annotation')) continue;
            nodes.push(node);
        }

        /* Build one long string, tracking each node's span */
        var full  = '';
        var spans = [];
        nodes.forEach(function (n) {
            spans.push({ node: n, start: full.length, end: full.length + n.nodeValue.length });
            full += n.nodeValue;
        });

        var idx = full.indexOf(searchText);
        if (idx === -1) return null;
        var end = idx + searchText.length;

        var startNode, startOff, endNode, endOff;
        for (var i = 0; i < spans.length; i++) {
            var s = spans[i];
            if (startNode === undefined && idx >= s.start && idx < s.end) {
                startNode = s.node; startOff = idx - s.start;
            }
            if (endNode === undefined && end > s.start && end <= s.end) {
                endNode = s.node; endOff = end - s.start;
            }
            if (startNode && endNode) break;
        }

        if (!startNode || !endNode) return null;
        var range = document.createRange();
        range.setStart(startNode, startOff);
        range.setEnd(endNode, endOff);
        return range;
    }

    /* ------------------------------------------------------------------
       Apply a single annotation to the DOM
    ------------------------------------------------------------------ */

    function applyAnnotation(ann) {
        var root  = document.getElementById('markdownBody') || document.body;
        var range = findTextRange(ann.text, root);
        if (!range) return false;

        var mark = document.createElement('mark');
        mark.className = 'user-annotation user-annotation--' + ann.color;
        mark.setAttribute('data-ann-id', ann.id);
        if (ann.note) mark.setAttribute('data-note', ann.note);
        mark.setAttribute('data-created', ann.created || '');

        try {
            range.surroundContents(mark);
        } catch (e) {
            /* Range crosses element boundaries — extract and re-insert */
            var frag = range.extractContents();
            mark.appendChild(frag);
            range.insertNode(mark);
        }

        bindMarkEvents(mark, ann);
        return true;
    }

    function applyAll() {
        forPage().forEach(function (ann) { applyAnnotation(ann); });
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
    }

    /* Defer to the shared utility (loaded synchronously from
       templates/partials/head.html) so this file cannot drift from
       popups.js, semantic-search.js, or build/Utils.hs. */
    function escHtml(s) {
        return window.lnUtils.escapeHtml(s);
    }

    function showTooltip(mark, ann) {
        clearTimeout(tooltipTimer);

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
        if (immediate) {
            if (tooltip) tooltip.classList.remove('is-visible');
        } else {
            tooltipTimer = setTimeout(function () {
                if (tooltip) tooltip.classList.remove('is-visible');
            }, 120);
        }
    }

    function bindMarkEvents(mark, ann) {
        mark.addEventListener('mouseenter', function () {
            clearTimeout(tooltipTimer);
            showTooltip(mark, ann);
        });
        mark.addEventListener('mouseleave', function () { hideTooltip(false); });
    }

    /* ------------------------------------------------------------------
       Public API
    ------------------------------------------------------------------ */

    window.Annotations = {
        add: function (text, color, note) {
            var ann = {
                id:      uid(),
                url:     location.pathname,
                text:    text,
                color:   color || 'amber',
                note:    note  || '',
                created: new Date().toISOString(),
            };
            addRaw(ann);
            applyAnnotation(ann);
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
}());
