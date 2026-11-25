/* portal-shelf.js — the shelf of portals on the home page
   (templates/partials/portal-shelf.html, css/portal-shelf.css).

   Pointing at a spine (a mouse or pen over it, or keyboard focus) draws it
   off the shelf and turns the map open beneath to it: a portal the map
   draws has its region lit, as its tag page's map is (tag-map.js does the
   lighting); one it does not draw shows its cover, if its sidecar names
   one, else the whole map. The caption under the map becomes the portal's
   name over its gloss, or its facts. Leaving the shelf turns it back, and
   so does tabbing on to the caption, which is then the way to /map/.

   A touch goes straight to the portal, as any link does: there is no
   pointing without a pointer. Without this script, or without a map, the
   spines are links and the caption names the map. */
(function () {
    'use strict';

    var shelf = document.querySelector('[data-portal-shelf]');
    if (!shelf) return;
    var stage = shelf.querySelector('.portal-shelf-stage');
    var svg   = stage && stage.querySelector('svg.tag-map');
    var cover = stage && stage.querySelector('.portal-shelf-cover');
    var title = shelf.querySelector('.portal-shelf-caption-title');
    var meta  = shelf.querySelector('.portal-shelf-caption-meta');
    if (!svg || !cover || !title || !meta) return;

    var rest  = { href: title.getAttribute('href'), title: title.textContent, meta: meta.textContent };
    var shown = null;

    function light(tag) {
        svg.dispatchEvent(new CustomEvent('tagmap:light', { detail: tag }));
    }

    function onMap(tag) {
        return svg.querySelector('.tag-map-tag[data-tag="' + CSS.escape(tag) + '"]') !== null;
    }

    function showCover(on) {
        cover.hidden = !on;
        stage.classList.toggle('is-cover', on);
    }

    function caption(href, name, line) {
        title.setAttribute('href', href);
        title.textContent = name;
        meta.textContent = line;
    }

    function show(spine) {
        if (spine === shown) return;
        if (shown) shown.classList.remove('is-out');
        shown = spine;
        if (!spine) {
            showCover(false);
            light(null);
            caption(rest.href, rest.title, rest.meta);
            return;
        }
        spine.classList.add('is-out');
        var tag = spine.getAttribute('data-tag');
        var src = spine.getAttribute('data-cover');
        /* The whole map first, whatever stood before: a cover is loaded
           before it is shown, and one that fails to load leaves the map. */
        showCover(false);
        light(onMap(tag) ? tag : null);
        if (!onMap(tag) && src) {
            var img = new Image();
            img.onload = function () {
                if (shown !== spine) return;
                cover.src = src;
                showCover(true);
            };
            img.src = src;
        }
        caption(spine.getAttribute('href'),
                spine.querySelector('.portal-spine-title').textContent,
                spine.getAttribute('data-caption') || '');
    }

    shelf.querySelectorAll('.portal-spine').forEach(function (spine) {
        spine.addEventListener('pointerenter', function (e) {
            if (e.pointerType !== 'touch') show(spine);
        });
        spine.addEventListener('focus', function () { show(spine); });
    });
    /* Tabbing on from the last spine reaches the caption: it is the way
       to the map for a keyboard (the map's own links are out of the tab
       order), so it names the map again. A click on it is not a keyboard's
       focus, and goes to the portal it names. */
    title.addEventListener('focus', function () {
        if (title.matches(':focus-visible')) show(null);
    });
    /* The pointer leaving gives the shelf back to the keyboard: to the
       spine that has focus, if one has, else to the map. */
    shelf.addEventListener('pointerleave', function () {
        var focused = document.activeElement;
        show(shelf.contains(focused) && focused.classList.contains('portal-spine') ? focused : null);
    });
    shelf.addEventListener('focusout', function (e) {
        if (!shelf.contains(e.relatedTarget)) show(null);
    });
    /* tag-map.js clears its own hover when the pointer leaves the map; the
       shelf's portal is lit again. */
    svg.addEventListener('pointerleave', function () {
        if (shown && onMap(shown.getAttribute('data-tag'))) light(shown.getAttribute('data-tag'));
    });
})();
