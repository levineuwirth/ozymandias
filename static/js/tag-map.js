/* tag-map.js — hover for the tag map (build/TagMap.hs, css/tag-map.css).
   Pointing at a tag lights its region as a tag page's map is lit: the tag
   and its subtags, the pieces filed under them and the lines among them,
   and one notch less the other tags those pieces carry and the tag's
   ancestors. Pointing at a piece lights it, its tags and its lines. The
   classes are the hover's own (is-hover-lit, is-hover-near), so a page's
   lit region comes back when the pointer leaves.

   On a narrow screen, where the map pans sideways, a lit map is scrolled
   so that its tag is in view.

   The map reads fully without this script: it is an enhancement only. */
(function () {
    'use strict';

    function init(svg) {
        var tags = Array.prototype.slice.call(svg.querySelectorAll('.tag-map-tag'));
        var pieces = Array.prototype.slice.call(svg.querySelectorAll('.tag-map-piece'));
        var edges = Array.prototype.slice.call(svg.querySelectorAll('.tag-map-edge'));
        var marked = [];

        function piecesTags(p) {
            var t = p.getAttribute('data-tags');
            return t ? t.split(' ') : [];
        }

        function ancestors(tag) {
            var out = [];
            for (var i = tag.indexOf('/'); i !== -1; i = tag.indexOf('/', i + 1)) {
                out.push(tag.slice(0, i));
            }
            return out;
        }

        function mark(el, cls) {
            el.classList.add(cls);
            marked.push([el, cls]);
        }

        function clear() {
            marked.forEach(function (m) { m[0].classList.remove(m[1]); });
            marked = [];
            svg.classList.remove('tag-map--hover');
        }

        /* The region of a tag: core tags, the pieces under them, and the
           tags near them. As TagMap.renderMap computes it. */
        function lightTag(tag) {
            var core = {}, near = {}, lit = {};
            tags.forEach(function (t) {
                var g = t.getAttribute('data-tag');
                if (g === tag || g.indexOf(tag + '/') === 0) core[g] = true;
            });
            pieces.forEach(function (p) {
                var ts = piecesTags(p);
                if (ts.some(function (g) { return core[g]; })) {
                    lit[p.getAttribute('data-piece')] = true;
                    ts.forEach(function (g) { if (!core[g]) near[g] = true; });
                }
            });
            ancestors(tag).forEach(function (g) { if (!core[g]) near[g] = true; });
            apply(core, near, lit);
        }

        function lightPiece(piece) {
            var core = {}, lit = {};
            piecesTags(piece).forEach(function (g) { core[g] = true; });
            lit[piece.getAttribute('data-piece')] = true;
            apply(core, {}, lit);
        }

        function level(tag, core, near) {
            return core[tag] ? 2 : near[tag] ? 1 : 0;
        }

        function apply(core, near, lit) {
            clear();
            svg.classList.add('tag-map--hover');
            var cls = ['', 'is-hover-near', 'is-hover-lit'];
            tags.forEach(function (t) {
                var l = level(t.getAttribute('data-tag'), core, near);
                if (l) mark(t, cls[l]);
            });
            pieces.forEach(function (p) {
                if (lit[p.getAttribute('data-piece')]) mark(p, cls[2]);
            });
            edges.forEach(function (e) {
                var l;
                if (e.hasAttribute('data-piece')) {
                    l = lit[e.getAttribute('data-piece')] ? level(e.getAttribute('data-tag'), core, near) : 0;
                } else {
                    l = Math.min(level(e.getAttribute('data-tag'), core, near),
                                 level(e.getAttribute('data-parent'), core, near));
                }
                if (l) mark(e, cls[l]);
            });
        }

        svg.addEventListener('pointerover', function (e) {
            var a = e.target.closest && e.target.closest('a');
            if (!a || !svg.contains(a)) return;
            if (a.classList.contains('tag-map-tag')) lightTag(a.getAttribute('data-tag'));
            else if (a.classList.contains('tag-map-piece')) lightPiece(a);
        });
        svg.addEventListener('pointerleave', clear);

        /* Another script may light a tag's region, or clear it (null):
           portal-shelf.js, for the spine pointed at. */
        svg.addEventListener('tagmap:light', function (e) {
            if (e.detail) lightTag(e.detail);
            else clear();
        });

        /* Bring a lit map's tag into view where the map pans. */
        var focus = svg.getAttribute('data-focus');
        var scroller = svg.parentElement;
        if (focus && scroller && scroller.scrollWidth > scroller.clientWidth) {
            var here = svg.querySelector('.tag-map-tag.is-here circle');
            if (here) {
                var box = here.getBoundingClientRect();
                var frame = scroller.getBoundingClientRect();
                scroller.scrollLeft += (box.left + box.width / 2) - (frame.left + frame.width / 2);
            }
        }
    }

    function start() {
        document.querySelectorAll('svg.tag-map').forEach(init);
    }

    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
