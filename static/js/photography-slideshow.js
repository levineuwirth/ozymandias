/* photography-slideshow.js — sequential full-screen viewing of a grid.
 *
 * Slideshow is not a fourth grid layout, so it is deliberately not one of
 * photography-modes.js's VALID_MODES: it does not rearrange the page, it
 * replaces it for a while and then gives it back. That is also why it is not
 * persisted to localStorage — nobody wants to land on a page and find it
 * already playing.
 *
 * Frames come from whatever .photography-grid is on the page, in DOM order,
 * so the same control works on the section landing, a series, and a by-year
 * index without knowing anything about them.
 *
 * The overlay borrows .lightbox-overlay.darkroom styling from lightbox.js so
 * the two full-screen experiences look like one idea, but it does not drive
 * that module: lightbox.js builds its own DOM around a single image and
 * exposes no API for sequences.
 */
(function () {
    'use strict';

    var ADVANCE_MS = 5000;

    document.addEventListener('DOMContentLoaded', function () {
        var trigger = document.querySelector('[data-mode="slideshow"]');
        var grid    = document.querySelector('.photography-grid');
        if (!trigger || !grid) return;

        var frames = [];
        grid.querySelectorAll('.photo-card').forEach(function (card) {
            var im = card.querySelector('img');
            if (!im) return;
            var titleEl = card.querySelector('.photo-card-title');
            var dateEl  = card.querySelector('.photo-card-date');
            var link    = card.querySelector('.photo-card-link');
            frames.push({
                /* P01: `src` deliberately, not `currentSrc`. Card images
                   now carry a srcset ladder, so currentSrc is whichever
                   rung the grid slot needed — 480px wide, in a full-screen
                   slideshow. The `src` attribute stays the full delivery
                   source, which is also what lightbox.js opens. */
                src:   im.src,
                alt:   im.alt || '',
                title: titleEl ? titleEl.textContent.trim() : '',
                date:  dateEl ? dateEl.textContent.trim() : '',
                href:  link ? link.getAttribute('href') : null
            });
        });
        if (frames.length === 0) {
            trigger.hidden = true;
            return;
        }

        /* A09: autoplay is opt-in for anyone who has asked for less
           motion — and "asked" means EITHER input. This used to consult
           only the OS media query, so turning on the site's own Reduce
           Motion setting and opening a slideshow still started the timer
           and showed "Pause slideshow".

           Evaluated per call rather than cached at load, and watched for
           changes, so switching the setting mid-visit takes effect at
           once. Manual Play stays available in every case: this suppresses
           automatic playback, it does not remove the control. */
        var motionQuery = window.matchMedia
            ? window.matchMedia('(prefers-reduced-motion: reduce)')
            : null;

        function reducedMotion() {
            if (document.documentElement.hasAttribute('data-reduce-motion')) return true;
            return !!(motionQuery && motionQuery.matches);
        }

        var index = 0;
        var timer = null;
        var lastFocus = null;

        // ------------------------------------------------------------------
        // DOM
        // ------------------------------------------------------------------
        var overlay = document.createElement('div');
        overlay.className = 'lightbox-overlay darkroom slideshow-overlay';
        overlay.setAttribute('role', 'dialog');
        overlay.setAttribute('aria-modal', 'true');
        overlay.setAttribute('aria-label', 'Slideshow');
        // Visibility is carried by .is-open, exactly as lightbox.js does it.
        // .lightbox-overlay hides itself with opacity/visibility/pointer-events
        // rather than display, so toggling the `hidden` attribute did nothing
        // to it — the overlay stayed invisible while open() had already locked
        // body scrolling, which read as the page freezing for no reason.

        var vignette = document.createElement('div');
        vignette.className = 'lightbox-vignette';
        vignette.setAttribute('aria-hidden', 'true');

        var figure = document.createElement('figure');
        figure.className = 'slideshow-figure';

        var img = document.createElement('img');
        img.className = 'slideshow-img';
        img.decoding = 'async';

        var caption = document.createElement('figcaption');
        caption.className = 'slideshow-caption';

        figure.appendChild(img);
        figure.appendChild(caption);

        function control(cls, label, glyph) {
            var b = document.createElement('button');
            b.type = 'button';
            b.className = 'slideshow-btn ' + cls;
            b.setAttribute('aria-label', label);
            b.textContent = glyph;
            return b;
        }

        var prevBtn  = control('slideshow-prev',  'Previous photograph', '‹');
        var nextBtn  = control('slideshow-next',  'Next photograph',     '›');
        var playBtn  = control('slideshow-play',  'Play slideshow',      '▶');
        var closeBtn = control('slideshow-close', 'Close slideshow',     '×');

        var counter = document.createElement('p');
        counter.className = 'slideshow-counter';
        // Position changes are announced, but politely — a screen reader
        // should not interrupt itself every five seconds.
        counter.setAttribute('aria-live', 'polite');

        var bar = document.createElement('div');
        bar.className = 'slideshow-bar';
        bar.appendChild(prevBtn);
        bar.appendChild(playBtn);
        bar.appendChild(nextBtn);
        bar.appendChild(counter);

        overlay.appendChild(vignette);
        overlay.appendChild(closeBtn);
        overlay.appendChild(figure);
        overlay.appendChild(bar);
        document.body.appendChild(overlay);

        // ------------------------------------------------------------------
        // Behaviour
        // ------------------------------------------------------------------
        function preload(i) {
            if (i < 0 || i >= frames.length) return;
            var p = new Image();
            p.src = frames[i].src;
        }

        function show(i) {
            index = (i + frames.length) % frames.length;
            var f = frames[index];
            img.src = f.src;
            img.alt = f.alt;
            caption.innerHTML = '';
            if (f.title) {
                var t = document.createElement('span');
                t.className = 'slideshow-title';
                t.textContent = f.title;
                caption.appendChild(t);
            }
            if (f.date) {
                var d = document.createElement('span');
                d.className = 'slideshow-date';
                d.textContent = f.date;
                caption.appendChild(d);
            }
            counter.textContent = (index + 1) + ' of ' + frames.length;
            preload(index + 1);
        }

        function playing() { return timer !== null; }

        function play() {
            if (playing()) return;
            timer = window.setInterval(function () { show(index + 1); }, ADVANCE_MS);
            playBtn.textContent = '❚❚';
            playBtn.setAttribute('aria-label', 'Pause slideshow');
        }

        function pause() {
            if (!playing()) return;
            window.clearInterval(timer);
            timer = null;
            playBtn.textContent = '▶';
            playBtn.setAttribute('aria-label', 'Play slideshow');
        }

        function open(startAt) {
            lastFocus = document.activeElement;
            show(typeof startAt === 'number' ? startAt : 0);
            overlay.classList.add('is-open');
            document.body.classList.add('slideshow-open');
            /* Playback first, so the control that takes focus already has
               the name it will keep ("Pause slideshow" when playing); a
               screen reader announces it on focus. Play/Pause takes focus,
               not Close: Space presses the focused control (below), so
               Space on opening still pauses. */
            if (!reducedMotion()) play();
            playBtn.focus();
        }

        function close() {
            pause();
            overlay.classList.remove('is-open');
            document.body.classList.remove('slideshow-open');
            if (lastFocus && lastFocus.focus) lastFocus.focus();
        }

        // Manual navigation stops the clock: someone steering does not want
        // the timer yanking the frame out from under them mid-look.
        function step(delta) { pause(); show(index + delta); }

        trigger.addEventListener('click', function (e) {
            e.preventDefault();
            open(0);
        });

        prevBtn.addEventListener('click', function () { step(-1); });
        nextBtn.addEventListener('click', function () { step(1); });
        closeBtn.addEventListener('click', close);
        playBtn.addEventListener('click', function () {
            playing() ? pause() : play();
        });

        // Clicking the photograph opens its own page — the slideshow is for
        // looking, and this is the way out of it into reading.
        img.addEventListener('click', function () {
            var f = frames[index];
            if (f.href) { close(); window.location.href = f.href; }
        });

        overlay.addEventListener('click', function (e) {
            if (e.target === overlay || e.target === vignette) close();
        });

        document.addEventListener('keydown', function (e) {
            if (!overlay.classList.contains('is-open')) return;
            switch (e.key) {
                case 'Escape':     close(); break;
                case 'ArrowRight': e.preventDefault(); step(1); break;
                case 'ArrowLeft':  e.preventDefault(); step(-1); break;
                case 'Home':       e.preventDefault(); step(-index); break;
                case 'End':        e.preventDefault(); step(frames.length - 1 - index); break;
                case ' ':
                    /* A focused control takes Space as a press of itself,
                       as buttons do (audit J10): Space on Next used to
                       start playback and leave the frame where it was. */
                    if (e.target instanceof HTMLButtonElement && overlay.contains(e.target)) break;
                    e.preventDefault();
                    playing() ? pause() : play();
                    break;
                case 'Tab': trapTab(e); break;
            }
        });

        function trapTab(e) {
            var f = [closeBtn, prevBtn, playBtn, nextBtn];
            var first = f[0], last = f[f.length - 1];
            if (e.shiftKey && document.activeElement === first) {
                e.preventDefault(); last.focus();
            } else if (!e.shiftKey && document.activeElement === last) {
                e.preventDefault(); first.focus();
            }
        }

        // A tab switched away should not silently burn through the set.
        document.addEventListener('visibilitychange', function () {
            if (document.hidden) pause();
        });

        /* Respond to changes in either input while the overlay is open:
           asking for less motion should stop the frames advancing now, not
           at the next visit. Turning the preference back off does not
           auto-start — resuming is the reader's call. */
        function onMotionPreferenceChange() {
            if (reducedMotion()) pause();
        }

        if (motionQuery) {
            if (motionQuery.addEventListener) {
                motionQuery.addEventListener('change', onMotionPreferenceChange);
            } else if (motionQuery.addListener) {
                motionQuery.addListener(onMotionPreferenceChange);
            }
        }

        if (window.MutationObserver) {
            new MutationObserver(onMotionPreferenceChange).observe(
                document.documentElement,
                { attributes: true, attributeFilter: ['data-reduce-motion'] }
            );
        }
    });
}());
