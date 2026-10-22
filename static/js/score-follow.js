/* score-follow.js — play a realization and follow it in the score.

   Loaded only when a composition has a realization (templates/
   score-reader-default.html). The stage's data-timing names timing.json,
   written by tools/music-import.py --audio from the same MuseScore layout
   as the pages:

     measures   one box per bar: [page, x, y, width, height], the box as
                fractions of its page, so it scales with any zoom
     events     [milliseconds, bar] in the order the bars sound, repeats
                written out
     movements  the bar each movement opens with
     audio      the realization's filename, beside timing.json

   While the music plays the reader turns its own pages, keeps the bar in
   view, and marks it in the style chosen under Settings — a band over the
   bar, a cursor moving through it, or page turns alone. Turning a page by
   hand steps out of following until the reader asks to return; clicking a
   bar plays from it. Nothing plays until asked: the audio is not even
   requested before the first press. */
(function () {
    'use strict';

    var reader = window.scoreReader;
    var stage  = document.getElementById('score-reader-stage');
    if (!reader || !stage || !stage.dataset.timing) return;

    var STYLE_KEY = 'score-follow-style';
    var STYLES = [
        { key: 'bar',    label: 'Bar' },
        { key: 'cursor', label: 'Cursor' },
        { key: 'none',   label: 'Turns only' }
    ];
    var storage = window.lnUtils && window.lnUtils.safeStorage;
    var style   = (storage && storage.get(STYLE_KEY)) || 'bar';
    if (!STYLES.some(function (s) { return s.key === style; })) style = 'bar';

    /* The site's Reduce Motion setting and the system's, unioned, and read
       when used, as nav.js and collapse.js do. Only the system's was read,
       once at load: a reader who had turned on the site's setting still
       had the view glide to every new system. */
    function reducedMotion() {
        if (document.documentElement.hasAttribute('data-reduce-motion')) return true;
        return !!(window.matchMedia
            && window.matchMedia('(prefers-reduced-motion: reduce)').matches);
    }

    var timing     = null;
    var audioUrl   = null;
    var audio      = null;
    var firstTime  = {};     /* bar -> ms of its first sounding */
    var following  = true;
    var frame      = 0;
    var lastBar    = -1;
    var lastSystem = null;   /* page and top of the system last scrolled to */
    var layer      = null;
    var mark       = null;
    var dragging   = false;   /* the seek slider is under the pointer */
    var dragTo     = null;    /* where a drag will seek to when it ends (s) */
    var startAt    = null;    /* where the first Play starts (ms), if not page 1 */
    var wakeTap    = false;   /* the pointer went down on a sleeping reader */
    var ui         = {};

    fetch(stage.dataset.timing, { credentials: 'same-origin' })
        .then(function (r) {
            if (!r.ok) throw new Error('HTTP ' + r.status);
            return r.json();
        })
        .then(init)
        .catch(function (err) {
            if (window.console) console.error('[score-follow]', err);
        });

    /* ------------------------------------------------------------------
       Timing lookups
    ------------------------------------------------------------------ */

    /* Index of the last event at or before ms: binary search, since a
       symphony has a thousand bars and this runs every frame. */
    function eventAt(ms) {
        var ev = timing.events, lo = 0, hi = ev.length - 1;
        if (hi < 0 || ms < ev[0][0]) return 0;
        while (lo < hi) {
            var mid = (lo + hi + 1) >> 1;
            if (ev[mid][0] <= ms) lo = mid; else hi = mid - 1;
        }
        return lo;
    }

    function clock(seconds) {
        seconds = Math.max(0, Math.floor(seconds || 0));
        var h = Math.floor(seconds / 3600);
        var m = Math.floor(seconds % 3600 / 60);
        var s = seconds % 60;
        var mm = h ? (m < 10 ? '0' + m : m) : m;
        return (h ? h + ':' : '') + mm + ':' + (s < 10 ? '0' + s : s);
    }

    /* ------------------------------------------------------------------
       Audio
    ------------------------------------------------------------------ */

    function ensureAudio() {
        if (audio) return audio;
        audio = new Audio();
        audio.preload = 'auto';
        audio.src = audioUrl;
        audio.addEventListener('play',  onPlayState);
        audio.addEventListener('pause', onPlayState);
        audio.addEventListener('ended', onPlayState);
        audio.addEventListener('seeked', function () { lastBar = -1; tick(); });
        audio.addEventListener('timeupdate', updateTime);
        audio.addEventListener('error', function () {
            ui.play.disabled = true;
            ui.play.title = 'The realization could not be loaded.';
            ui.group.classList.add('is-failed');
        });
        return audio;
    }

    /* When the music on a page begins: the first sounding of its earliest
       bar. */
    function pageStart(page) {
        var best = null;
        for (var b = 0; b < timing.measures.length; b++) {
            var m = timing.measures[b];
            if (!m || m[0] !== page || firstTime[b] === undefined) continue;
            if (best === null || firstTime[b] < best) best = firstTime[b];
        }
        return best;
    }

    /* The first Play starts where the reader is: at the movement chosen
       before it, or else at the music of the page on screen. Starting at
       0:00 pulled a reader who had arrived through a movement link back to
       page 1 on the first frame. Later presses resume. */
    function play() {
        if (!audio) {
            var from = startAt !== null ? startAt : pageStart(reader.page());
            startAt = null;
            ensureAudio();
            if (from) audio.currentTime = from / 1000;
        }
        following = true;
        hideReturn();
        var p = audio.play();
        if (p && p.catch) p.catch(function () {});
    }

    function toggle() {
        if (audio && !audio.paused) audio.pause(); else play();
    }

    /* keepView: the jump was made by clicking a bar the reader can see, so
       the view stays where it is; any other jump brings the music into view. */
    function seekTo(ms, andPlay, keepView) {
        startAt = null;
        ensureAudio();
        following = true;
        hideReturn();
        audio.currentTime = ms / 1000;
        if (andPlay && audio.paused) play();
        lastBar = -1;
        if (!keepView) lastSystem = null;
        tick();
    }

    function onPlayState() {
        var playing = audio && !audio.paused && !audio.ended;
        ui.play.setAttribute('aria-pressed', playing ? 'true' : 'false');
        ui.play.setAttribute('aria-label', playing ? 'Pause' : 'Play the realization');
        ui.play.classList.toggle('is-playing', playing);
        document.body.classList.toggle('is-following', !!playing);
        if ('mediaSession' in navigator) {
            navigator.mediaSession.playbackState = playing ? 'playing' : 'paused';
        }
        if (playing) schedule();
        if (audio && audio.ended) hideReturn();
    }

    /* ------------------------------------------------------------------
       Following
    ------------------------------------------------------------------ */

    /* One frame loop while the music plays. tick() is also called directly
       — on a seek, a page render, a style change — and used to reschedule
       itself as well, which started a second loop each time: after 150
       page turns, 150 ticks a frame. */
    function schedule() {
        if (!frame) frame = requestAnimationFrame(loop);
    }

    function loop() {
        frame = 0;
        tick();
        if (audio && !audio.paused) schedule();
    }

    function tick() {
        if (!timing || !audio) return;
        var ms  = audio.currentTime * 1000;
        var i   = eventAt(ms);
        var ev  = timing.events[i];
        if (!ev) return;
        var bar  = ev[1];
        var box  = timing.measures[bar];
        var next = timing.events[i + 1];
        var end  = next ? next[0] : timing.duration * 1000;
        var frac = Math.min(1, Math.max(0, (ms - ev[0]) / Math.max(1, end - ev[0])));

        if (box) {
            if (following && box[0] !== reader.page()) reader.turnTo(box[0]);
            place(box, frac, bar !== lastBar);
        }
        lastBar = bar;
    }

    function place(box, frac, newBar) {
        var onPage = box[0] === reader.page();
        if (mark) {
            mark.hidden = !onPage || style === 'none';
            if (onPage && style !== 'none') {
                var x = style === 'cursor' ? box[1] + box[3] * frac : box[1];
                mark.style.left   = (x * 100) + '%';
                mark.style.top    = (box[2] * 100) + '%';
                mark.style.height = (box[4] * 100) + '%';
                mark.style.width  = style === 'cursor' ? '' : (box[3] * 100) + '%';
            }
        }
        var system = box[0] + ':' + box[2];
        if (onPage && following && system !== lastSystem) {
            lastSystem = system;
            keepInView(box);
        }
    }

    /* A portrait page at fit-width is taller than the window, so the music
       can move below the fold. The view follows it one system at a time:
       when the music reaches a new system whose top is out of view, the
       view scrolls to it — and never within a system. An orchestral system
       is itself taller than the window, and a reader scrolling down to the
       strings must be left there until the music moves on. (Following each
       bar instead pulled the view back to the system's top every bar.) */
    function keepInView(box) {
        var sheet = reader.sheet.getBoundingClientRect();
        var view  = reader.viewport.getBoundingClientRect();
        var top    = sheet.top + box[2] * sheet.height;
        var margin = Math.min(80, view.height * 0.12);
        if (top >= view.top && top <= view.top + view.height * 0.6) return;
        reader.viewport.scrollBy({
            top: top - view.top - margin,
            behavior: reducedMotion() ? 'auto' : 'smooth'
        });
    }

    function attachLayer() {
        layer = document.createElement('div');
        layer.className = 'score-follow-layer';
        layer.setAttribute('aria-hidden', 'true');
        mark = document.createElement('div');
        mark.className = 'score-follow-mark';
        mark.hidden = true;
        layer.appendChild(mark);
        layer.dataset.style = style;
        reader.sheet.appendChild(layer);
        lastBar = -1;
        if (audio) tick();
    }

    /* ------------------------------------------------------------------
       Stepping out of following, and back
    ------------------------------------------------------------------ */

    function showReturn() {
        ui.back.hidden = false;
    }

    function hideReturn() {
        ui.back.hidden = true;
    }

    /* ------------------------------------------------------------------
       Clicking a bar plays from it
    ------------------------------------------------------------------ */

    function barAt(e) {
        var r = reader.sheet.getBoundingClientRect();
        if (!r.width || !r.height) return -1;
        var fx = (e.clientX - r.left) / r.width;
        var fy = (e.clientY - r.top) / r.height;
        var page = reader.page();
        for (var b = 0; b < timing.measures.length; b++) {
            var m = timing.measures[b];
            if (!m || m[0] !== page) continue;
            if (fx >= m[1] && fx <= m[1] + m[3] && fy >= m[2] && fy <= m[2] + m[4]) return b;
        }
        return -1;
    }

    /* ------------------------------------------------------------------
       Controls
    ------------------------------------------------------------------ */

    function el(tag, cls, attrs) {
        var n = document.createElement(tag);
        if (cls) n.className = cls;
        Object.keys(attrs || {}).forEach(function (k) { n.setAttribute(k, attrs[k]); });
        return n;
    }

    function buildControls() {
        ui.group = el('div', 'score-follow', { role: 'group', 'aria-label': timing.label || 'Realization' });
        ui.play  = el('button', 'score-follow-play', {
            type: 'button', 'aria-pressed': 'false', 'aria-label': 'Play the realization',
            title: (timing.label || 'Realization') + ' — play or pause (K)'
        });
        ui.play.innerHTML =
            '<svg class="score-follow-icon-play" viewBox="0 0 16 16" aria-hidden="true"><path d="M4.5 2.8v10.4L13 8z"/></svg>' +
            '<svg class="score-follow-icon-pause" viewBox="0 0 16 16" aria-hidden="true"><path d="M4 2.8h2.7v10.4H4zM9.3 2.8H12v10.4H9.3z"/></svg>';
        ui.seek = el('input', 'score-follow-seek', {
            type: 'range', min: '0', max: String(timing.duration || 0), step: '0.1', value: '0',
            'aria-label': 'Position in the realization'
        });
        ui.time  = el('span', 'score-follow-time', { 'aria-hidden': 'true' });
        ui.label = el('span', 'score-follow-label');
        ui.label.textContent = timing.label || 'Realization';

        ui.group.appendChild(ui.play);
        ui.group.appendChild(ui.seek);
        ui.group.appendChild(ui.time);
        ui.group.appendChild(ui.label);

        var right = reader.bar && reader.bar.querySelector('.score-reader-bar-right');
        if (right) right.insertBefore(ui.group, right.firstChild);

        ui.back = el('button', 'score-follow-return', { type: 'button' });
        ui.back.textContent = 'Follow the music';
        ui.back.hidden = true;
        stage.appendChild(ui.back);

        ui.play.addEventListener('click', toggle);
        /* A drag seeks once, where it ends. Seeking on every step turned,
           fetched and rendered every page the thumb passed — 85 of a
           symphony's 151 in one sweep. Keyboard steps seek at once. */
        ui.seek.addEventListener('input', function () {
            if (dragging) {
                dragTo = parseFloat(ui.seek.value);
                updateTime();
                return;
            }
            seekTo(parseFloat(ui.seek.value) * 1000, false);
        });
        /* An arrow moves five seconds and Page Up or Down a minute. The
           native step is the slider's resolution, 0.1 s, which made seeking
           by keyboard a matter of hundreds of presses. */
        ui.seek.addEventListener('keydown', function (e) {
            var by = { ArrowRight: 5, ArrowUp: 5, ArrowLeft: -5, ArrowDown: -5,
                       PageUp: 60, PageDown: -60 }[e.key];
            if (!by || e.metaKey || e.ctrlKey || e.altKey) return;
            e.preventDefault();
            var now = audio ? audio.currentTime : parseFloat(ui.seek.value) || 0;
            var to  = Math.min(parseFloat(ui.seek.max) || 0, Math.max(0, now + by));
            ui.seek.value = String(to);
            seekTo(to * 1000, false);
        });
        /* The slider follows the music except while it is being dragged.
           Not "while focused": a slider keeps focus after a drag, and would
           freeze where it was let go. */
        ui.seek.addEventListener('pointerdown', function () { dragging = true; });
        ['pointerup', 'pointercancel', 'change'].forEach(function (evt) {
            ui.seek.addEventListener(evt, function () {
                dragging = false;
                if (dragTo === null) return;
                var to = dragTo;
                dragTo = null;
                seekTo(to * 1000, false);
            });
        });
        ui.back.addEventListener('click', function () {
            following = true;
            hideReturn();
            lastBar = -1;
            lastSystem = null;
            tick();
        });
        updateTime();
    }

    function updateTime() {
        var now = dragTo !== null ? dragTo : audio ? audio.currentTime : 0;
        var total = timing.duration || (audio && audio.duration) || 0;
        ui.time.textContent = clock(now) + ' / ' + clock(total);
        ui.seek.setAttribute('aria-valuetext', clock(now) + ' of ' + clock(total));
        if (!dragging) ui.seek.value = String(now);
    }

    /* The follow style lives beside the theme in the settings panel. */
    function buildSettings() {
        var panel = document.querySelector('.settings-panel');
        if (!panel) return;
        var section = el('div', 'settings-section score-follow-settings');
        var label = el('div', 'settings-label');
        label.textContent = 'Following';
        var row = el('div', 'settings-row');
        STYLES.forEach(function (s) {
            var b = el('button', 'settings-btn', { type: 'button', 'data-follow-style': s.key });
            b.textContent = s.label;
            b.classList.toggle('is-active', s.key === style);
            b.addEventListener('click', function () { setStyle(s.key, row); });
            row.appendChild(b);
        });
        section.appendChild(label);
        section.appendChild(row);
        panel.appendChild(section);
    }

    function setStyle(key, row) {
        style = key;
        if (storage) storage.set(STYLE_KEY, key);
        Array.prototype.forEach.call(row.children, function (b) {
            b.classList.toggle('is-active', b.getAttribute('data-follow-style') === key);
        });
        if (layer) layer.dataset.style = key;
        lastBar = -1;
        if (audio) tick();
    }

    /* ------------------------------------------------------------------
       Init
    ------------------------------------------------------------------ */

    function init(t) {
        if (!t || !t.events || !t.events.length || !t.measures) return;
        timing   = t;
        audioUrl = new URL(t.audio || 'realization.mp3',
                           new URL(stage.dataset.timing, window.location.href)).href;
        t.events.forEach(function (ev) {
            if (!(ev[1] in firstTime)) firstTime[ev[1]] = ev[0];
        });

        buildControls();
        buildSettings();
        attachLayer();
        reader.onRender(attachLayer);

        /* A turn the reader makes steps out of following; the music keeps
           its place, and one press returns (or Play, once paused). Paused
           too: a paused reader still following was pulled back to the
           music's page as soon as the turned page rendered. Before the
           first Play, the turn replaces any movement chosen earlier. */
        reader.onTurn(function () {
            startAt = null;
            if (!audio) return;
            following = false;
            if (!audio.paused) showReturn();
        });

        /* A movement button also moves the music, when the realization's
           movements line up with the buttons one for one. Registered after
           the reader's own handler, so the page has already turned. */
        var buttons = document.querySelectorAll('.score-reader-mvt');
        if (t.movements && t.movements.length === buttons.length) {
            Array.prototype.forEach.call(buttons, function (btn, i) {
                btn.addEventListener('click', function () {
                    var ms = firstTime[t.movements[i]];
                    if (ms === undefined) return;
                    /* Before the first Play, just turn, and start there. */
                    if (!audio) { startAt = ms; return; }
                    seekTo(ms, !audio.paused);
                });
            });
        }

        /* On a touch screen the toolbar sleeps during playback, and the
           only way to wake it is to touch the page — which is also a bar.
           A touch that lands on a sleeping reader only wakes it; otherwise
           reaching Pause jumped the music to wherever the finger fell. */
        reader.sheet.addEventListener('pointerdown', function (e) {
            wakeTap = e.pointerType !== 'mouse' &&
                document.body.classList.contains('is-idle');
        }, true);

        reader.sheet.addEventListener('click', function (e) {
            if (wakeTap) { wakeTap = false; return; }
            if (e.button !== 0 || e.metaKey || e.ctrlKey || e.shiftKey || e.altKey) return;
            if (window.getSelection && String(window.getSelection())) return;
            var b = barAt(e);
            if (b < 0 || firstTime[b] === undefined) return;
            var box = timing.measures[b];
            lastSystem = box[0] + ':' + box[2];
            seekTo(firstTime[b], true, true);
        });

        document.addEventListener('keydown', function (e) {
            if (e.metaKey || e.ctrlKey || e.altKey) return;
            if (e.target && /^(INPUT|TEXTAREA|SELECT)$/.test(e.target.tagName)) return;
            if (e.key === 'k' || e.key === 'K') { toggle(); e.preventDefault(); }
        });

        if ('mediaSession' in navigator) {
            var back = document.querySelector('.score-reader-back');
            navigator.mediaSession.metadata = new window.MediaMetadata({
                title:  back ? back.textContent.replace(/^\s*←\s*/, '') : document.title,
                artist: 'Levi Neuwirth',
                album:  t.label || 'Realization'
            });
            navigator.mediaSession.setActionHandler('play',  play);
            navigator.mediaSession.setActionHandler('pause', function () { if (audio) audio.pause(); });
            navigator.mediaSession.setActionHandler('seekto', function (d) {
                if (d && typeof d.seekTime === 'number') seekTo(d.seekTime * 1000, false);
            });
        }
    }
}());
