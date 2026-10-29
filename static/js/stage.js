/* stage.js — A layer for a season's decorations (site.yaml's seasons;
   theme.js decides which is worn). What is drawn here keeps to the page's
   margins and header, never covers its text, never takes a click, is never
   read aloud, and is gone in focus mode and print.

   theme.js loads this file while a season with a script is worn, and
   settings.js when a reader picks one; it loads that script (the season's
   ozy:season meta names it), which registers the season's actors:

       stage.register({
           season: 'halloween',
           mount:   function (ctx) {},  // ctx.layer: the layer to draw in
           layout:  function () {},     // after mount, and on every resize
           frame:   function (dt, t, moved) { return stage.REST; },
           unmount: function () {}
       });

   frame runs after the page moves (moved: a scroll or resize since the
   last frame) and for as long as some actor asks: BUSY, every frame;
   IDLE, a few frames a second; REST, not until the page moves again.
   Under reduce motion (stage.still) nothing is asked for: actors draw a
   still picture, and frame runs only when the page moves.

   What actors read:
     stage.geo      vw, vh; top, the header's foot; bodyL and bodyR, the
                    text column; margins, whether there is room beside it;
                    progress, how far down the page (0 to 1), and scrollable;
                    dpr, the device pixel ratio to draw at (at most 2)
     stage.pointer  {x, y}, or far off the screen
     stage.flung    seconds left of the startle after a fast scroll
     stage.blocked(x, y), stage.rectBlocked(l, t, r, b)
                    whether something of the page's is there
     stage.headerSlot()
                    a free spot in the header, {cx, cy, size, header}, or null

   The stage starts once the page has loaded and the browser is idle, so it
   never competes with the page's own first work, and fades in. */
(function () {
    'use strict';

    var root = document.documentElement;
    var reducedQuery = window.matchMedia('(prefers-reduced-motion: reduce)');
    var BUSY = 2, IDLE = 1, REST = 0;

    function clamp(v, a, b) { return v < a ? a : v > b ? b : v; }
    function stillMode() { return root.hasAttribute('data-reduce-motion') || reducedQuery.matches; }

    var geo = { vw: 0, vh: 0, top: 0, bodyL: 0, bodyR: 0, margins: false,
                progress: 0, scrollable: false, dpr: 1, toc: null };
    var pointer = { x: -1e4, y: -1e4 };

    var stage = {
        BUSY: BUSY, IDLE: IDLE, REST: REST,
        geo: geo, pointer: pointer, flung: 0, still: stillMode(),
        register: register, blocked: blocked, rectBlocked: rectBlocked,
        headerSlot: headerSlot
    };

    /* ------------------------------------------------------------------
       Geometry
    ------------------------------------------------------------------ */

    function measure() {
        geo.vw = window.innerWidth;
        geo.vh = window.innerHeight;
        var header = document.querySelector('body > header');
        geo.top = header ? Math.max(0, header.getBoundingClientRect().bottom) : 0;
        var body = document.getElementById('markdownBody');
        var r = body && body.getBoundingClientRect();
        if (r && r.width) { geo.bodyL = r.left; geo.bodyR = r.right; }
        else {
            var w = Math.min(800, geo.vw);
            geo.bodyL = (geo.vw - w) / 2; geo.bodyR = geo.bodyL + w;
        }
        geo.margins = geo.bodyL > 90 && geo.vw - geo.bodyR > 90;
        var max = root.scrollHeight - geo.vh;
        geo.scrollable = max > 40;
        geo.progress = geo.scrollable ? clamp(window.scrollY / max, 0, 1) : 0;
        /* The TOC is sticky: a live rectangle, outside the grid below. */
        var toc = document.getElementById('toc');
        var t = toc && toc.getBoundingClientRect();
        geo.toc = t && t.width && t.height
            ? { l: t.left - 16, t: t.top - 16, r: t.right + 16, b: t.bottom + 16 } : null;
    }

    /* ------------------------------------------------------------------
       Occupancy: which cells of the margins have something in them. Hit
       testing finds whatever is there (margin notes, an essay's figures,
       a wide backlinks block, the footer) without a list of names; the
       stage takes no hits itself.

       Cells are kept by page position, so a row is tested once, when it
       first comes into view, a few rows a frame: scrolling costs a
       fraction of a millisecond, where testing the whole screen again took
       10 to 17 ms at 1920 px. Anything fixed or sticky (the TOC, popups)
       stays out of the grid. It is dropped on a resize, when the page
       changes height, and every few seconds, and refills the same way.
    ------------------------------------------------------------------ */

    var CELL = 40, ROWS_PER_FRAME = 3;
    var grid = { cols: 0, rows: new Map(), at: 0 }, kinds = new WeakMap();

    function resetGrid(all) {
        grid = { cols: Math.ceil(geo.vw / CELL), rows: new Map(), at: performance.now() };
        if (all) kinds = new WeakMap();
    }

    /* 'free': the page's wrappers, and page-wide blocks with no text of
       their own; 'float': fixed or sticky, or inside such; 'solid': the
       rest. */
    function kindOf(el) {
        if (!el || el === root || el === document.body) return 'free';
        var k = kinds.get(el);
        if (k) return k;
        k = 'solid';
        for (var a = el; a && a !== document.body; a = a.parentElement) {
            var pos = getComputedStyle(a).position;
            if (pos === 'fixed' || pos === 'sticky') { k = 'float'; break; }
        }
        if (k === 'solid') {
            if (el.id === 'content' || el.id === 'markdownBody' || el.classList.contains('page-shell')) {
                k = 'free';
            } else if (!(el instanceof SVGElement) && el.getBoundingClientRect().width > geo.vw * 0.85 &&
                       !Array.prototype.some.call(el.childNodes, function (n) {
                           return n.nodeType === 3 && n.textContent.trim();
                       })) {
                k = 'free';
            }
        }
        kinds.set(el, k);
        return k;
    }

    function fillGrid() {
        if (!geo.margins) return;
        var sy = window.scrollY, budget = ROWS_PER_FRAME;
        var first = Math.floor((sy + geo.top) / CELL), last = Math.floor((sy + geo.vh - 1) / CELL);
        for (var pr = first; pr <= last && budget > 0; pr++) {
            if (grid.rows.has(pr)) continue;
            var y = pr * CELL + CELL / 2 - sy;
            if (y < geo.top + 2) continue;   /* under the header: a later frame */
            var row = new Uint8Array(grid.cols);
            for (var c = 0; c < grid.cols; c++) {
                var x = c * CELL + CELL / 2;
                if (x > geo.bodyL - 8 && x < geo.bodyR + 8) { row[c] = 1; continue; }
                row[c] = kindOf(document.elementFromPoint(x, y)) === 'solid' ? 1 : 0;
            }
            grid.rows.set(pr, row);
            budget--;
        }
    }

    function filling() {
        return geo.margins && grid.rows.size < Math.ceil((geo.vh - geo.top) / CELL) - 1;
    }

    function blocked(x, y) {
        if (!geo.margins) return false;
        var t = geo.toc;
        if (t && x > t.l && x < t.r && y > t.t && y < t.b) return true;
        var c = Math.floor(x / CELL);
        if (c < 0 || c >= grid.cols) return false;
        var row = grid.rows.get(Math.floor((y + window.scrollY) / CELL));
        return row ? row[c] === 1 : false;   /* not yet tested: free */
    }

    function rectBlocked(l, t, r, b) {
        for (var y = t; y <= b; y += CELL / 2)
            for (var x = l; x <= r; x += CELL / 2)
                if (blocked(x, y)) return true;
        return false;
    }

    /* A spot in the header with nothing in it, for something small where
       the margins have no room: right of the settings button (on a phone
       the controls row is centered), else between the logo and the links.
       Checked by hit testing; viewport coordinates. */
    function headerSlot() {
        var header = document.querySelector('body > header');
        if (!header) return null;
        var hr = header.getBoundingClientRect(), cands = [];
        var gear = header.querySelector('.settings-wrap');
        var logo = header.querySelector('.nav-logo'), links = header.querySelector('.nav-primary');
        if (gear) {
            var g = gear.getBoundingClientRect(), size = clamp(g.height * 0.8, 26, 34);
            cands.push({ cx: geo.vw - 18 - size / 2, cy: g.top + g.height / 2, size: size,
                         lo: g.right + 10, hi: geo.vw - 4 });
        }
        if (logo && links) {
            var a = logo.getBoundingClientRect(), b = links.getBoundingClientRect();
            if (a.width) cands.push({ cx: (a.right + b.left) / 2, cy: hr.top + hr.height / 2, size: 34,
                                      lo: a.right + 6, hi: b.left - 6 });
        }
        for (var i = 0; i < cands.length; i++) {
            var s = cands[i], h = s.size / 2;
            if (s.cx - h < s.lo || s.cx + h > s.hi) continue;
            var clear = [[0, 0], [-h, -h], [h, -h], [-h, h], [h, h]].every(function (p) {
                var el = document.elementFromPoint(s.cx + p[0], s.cy + p[1]);
                return el && header.contains(el) && !el.closest('a, button, svg, input, [role="button"]');
            });
            if (clear) return { cx: s.cx, cy: s.cy, size: s.size, header: header };
        }
        return null;
    }

    /* ------------------------------------------------------------------
       Actors, and the seasons' scripts that bring them
    ------------------------------------------------------------------ */

    var actors = [], loaded = {};

    function register(actor) {
        actors.push(actor);
        if (running && actor.season === running.season) {
            mountActor(actor);
            relayout = dirty = true;
            wake();
        }
    }

    function mountActor(actor) {
        running.actors.push(actor);
        actor.mount({ layer: running.layer, still: stage.still });
    }

    function loadSeasonScript(season) {
        var meta = document.querySelector('meta[name="ozy:season"][content="' + season + '"]');
        var src = meta && meta.getAttribute('data-script');
        if (!src || loaded[src]) return;
        loaded[src] = true;
        var s = document.createElement('script');
        s.src = src;
        document.head.appendChild(s);
    }

    /* ------------------------------------------------------------------
       Pointer and scroll
    ------------------------------------------------------------------ */

    function onPointer(e) { pointer.x = e.clientX; pointer.y = e.clientY; wake(); }
    function onPointerOut(e) { if (!e.relatedTarget) pointer.x = pointer.y = -1e4; }
    /* A touch is where the finger was, briefly. */
    function onTouch(e) {
        if (e.pointerType === 'mouse') return;
        onPointer(e);
        setTimeout(function () { pointer.x = pointer.y = -1e4; }, 350);
    }

    var lastScrollY = 0, lastScrollT = 0;
    function onScroll() {
        var now = performance.now(), dy = Math.abs(window.scrollY - lastScrollY);
        if (now > lastScrollT && dy / ((now - lastScrollT) / 1000) > 3500) stage.flung = 0.4;
        lastScrollY = window.scrollY; lastScrollT = now;
        dirty = true; wake();
    }
    function onResize() { dirty = relayout = true; wake(); }

    /* ------------------------------------------------------------------
       The loop
    ------------------------------------------------------------------ */

    var running = null, raf = 0, timer = 0, last = 0, dirty = true, relayout = true, sizeWatch = null;

    function wake() {
        if (!running || raf) return;
        if (timer) { clearTimeout(timer); timer = 0; }
        raf = requestAnimationFrame(frame);
    }
    function later(ms) {
        if (!timer && !raf) timer = setTimeout(function () { timer = 0; wake(); }, ms);
    }

    function frame(now) {
        raf = 0;
        if (!running) return;
        var dt = Math.min(0.05, (now - last) / 1000 || 0), moved = dirty;
        last = now;
        stage.flung = Math.max(0, stage.flung - dt);
        if (dirty) measure();
        if (relayout) {
            geo.dpr = Math.min(window.devicePixelRatio || 1, 2);
            resetGrid(true);
        } else if (now - grid.at > 6000) {
            resetGrid(false);
        }
        fillGrid();
        var want = REST;
        running.actors.forEach(function (a) {
            if (relayout && a.layout) a.layout();
            var w = a.frame ? a.frame(dt, now / 1000, moved || relayout) : REST;
            if (w > want) want = w;
        });
        dirty = relayout = false;
        if (document.hidden) return;
        if (want === BUSY && !stage.still) raf = requestAnimationFrame(frame);
        else if (want === IDLE && !stage.still) later(100);
        else if (filling()) raf = requestAnimationFrame(frame);
    }

    function start(season) {
        if (running) return;
        stage.still = stillMode();
        var layer = document.createElement('div');
        layer.className = 'stage-layer';
        layer.setAttribute('aria-hidden', 'true');
        document.body.appendChild(layer);
        running = { season: season, layer: layer, actors: [], still: stage.still };
        actors.forEach(function (a) { if (a.season === season) mountActor(a); });
        window.addEventListener('pointermove', onPointer, { passive: true });
        window.addEventListener('pointerdown', onTouch, { passive: true });
        document.addEventListener('pointerout', onPointerOut, { passive: true });
        window.addEventListener('scroll', onScroll, { passive: true });
        window.addEventListener('resize', onResize, { passive: true });
        document.addEventListener('visibilitychange', wake);
        sizeWatch = new ResizeObserver(function () { resetGrid(false); dirty = true; wake(); });
        sizeWatch.observe(document.body);
        lastScrollY = window.scrollY;
        dirty = relayout = true;
        last = performance.now();
        wake();
        requestAnimationFrame(function () { if (running) layer.classList.add('is-on'); });
    }

    function stop() {
        if (!running) return;
        running.actors.forEach(function (a) { if (a.unmount) a.unmount(); });
        running.layer.remove();
        running = null;
        if (raf) cancelAnimationFrame(raf);
        if (timer) clearTimeout(timer);
        raf = timer = 0;
        if (sizeWatch) { sizeWatch.disconnect(); sizeWatch = null; }
        window.removeEventListener('pointermove', onPointer);
        window.removeEventListener('pointerdown', onTouch);
        document.removeEventListener('pointerout', onPointerOut);
        window.removeEventListener('scroll', onScroll);
        window.removeEventListener('resize', onResize);
        document.removeEventListener('visibilitychange', wake);
    }

    /* Once the page has loaded and the browser is idle. */
    var pending = false;
    function startWhenSettled() {
        if (pending || running) return;
        pending = true;
        function go() {
            var idle = window.requestIdleCallback || function (f) { return setTimeout(f, 200); };
            idle(function () {
                pending = false;
                var season = wanted();
                if (season) start(season);
            }, { timeout: 1500 });
        }
        if (document.readyState === 'complete') go();
        else window.addEventListener('load', go, { once: true });
    }

    function wanted() {
        return root.hasAttribute('data-focus-mode') ? null : root.getAttribute('data-season');
    }

    function sync() {
        var season = wanted();
        if (running && (running.season !== season || running.still !== stillMode())) stop();
        if (season) {
            loadSeasonScript(season);
            startWhenSettled();
        }
    }

    window.stage = stage;
    new MutationObserver(sync).observe(root, {
        attributes: true,
        attributeFilter: ['data-season', 'data-focus-mode', 'data-reduce-motion']
    });
    if (reducedQuery.addEventListener) reducedQuery.addEventListener('change', sync);
    sync();
})();
