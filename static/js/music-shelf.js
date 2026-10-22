/* music-shelf.js — the shelf on /music/.

   Every work is a spine; one stands face-out, showing its first page.
   Pointing at a spine (hover or keyboard focus) turns that work face-out
   in its place, the way a reader pulls a score off the shelf to look at
   its cover. The spines are ordinary links, so without this script the
   face-out simply stays on the featured work and nothing is lost.

   The new first page is loaded off-screen before anything changes: an
   orchestral first page is hundreds of kilobytes of paths, and swapping
   the src directly flashes an empty sheet while it loads. If it fails to
   load, nothing changes at all — the working cover stays, and so does
   the empty slot that marks it. */
(function () {
    'use strict';

    var shelf = document.querySelector('[data-shelf]');
    if (!shelf) return;

    var face     = shelf.querySelector('.shelf-faceout');
    var img      = face && face.querySelector('img');
    var capTitle = shelf.querySelector('.shelf-caption-title');
    var capMeta  = shelf.querySelector('.shelf-caption-meta');
    var spines   = shelf.querySelectorAll('.shelf-spine');
    if (!face || !img || spines.length === 0) return;

    var wanted = null;   /* the spine most recently pointed at */
    var shown  = null;   /* the spine whose score is face-out now */

    function mark(spine) {
        Array.prototype.forEach.call(spines, function (s) {
            s.classList.toggle('is-faceout', s === spine);
        });
    }

    function turnFaceOut(spine) {
        face.setAttribute('href', spine.getAttribute('href'));
        face.dataset.stack = spine.dataset.stack || '0';
        if (spine.dataset.aspect) face.style.setProperty('--aspect', spine.dataset.aspect);
        if (img.getAttribute('src') !== spine.dataset.page) img.setAttribute('src', spine.dataset.page);
        if (capTitle) capTitle.innerHTML = spine.querySelector('.shelf-spine-title').innerHTML;
        if (capMeta)  capMeta.textContent = spine.dataset.meta || '';
    }

    /* Resolves once the image has loaded (decoded, where the browser can
       say), rejects if it cannot load. A decode that fails on an image
       that did load is not a failure: it only means no head start. */
    function load(src) {
        return new Promise(function (resolve, reject) {
            var next = new Image();
            next.onload = function () {
                (next.decode ? next.decode() : Promise.resolve()).then(resolve, resolve);
            };
            next.onerror = reject;
            next.src = src;
        });
    }

    function show(spine) {
        if (!spine.dataset.page || wanted === spine) return;
        wanted = spine;
        load(spine.dataset.page).then(function () {
            if (wanted !== spine) return;   /* a later point won */
            shown = spine;
            mark(spine);
            turnFaceOut(spine);
        }, function () {
            if (wanted === spine) wanted = shown;
        });
    }

    Array.prototype.forEach.call(spines, function (spine) {
        spine.addEventListener('pointerenter', function () { show(spine); });
        spine.addEventListener('focus', function () { show(spine); });
        if (spine.getAttribute('href') === face.getAttribute('href')) {
            wanted = shown = spine;
            mark(spine);
        }
    });
})();
