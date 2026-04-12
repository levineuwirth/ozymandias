/* score-reader.js — Page-turn navigation for the full score reader.
   Configuration is read from #score-reader-stage data attributes:
     data-page-count  — total number of SVG pages
     data-pages       — comma-separated list of absolute page image URLs
*/
(function () {
    'use strict';

    var stage   = document.getElementById('score-reader-stage');
    var img     = document.getElementById('score-page-img');
    var counter = document.getElementById('score-page-counter');
    var prevBtn = document.getElementById('score-prev');
    var nextBtn = document.getElementById('score-next');

    if (!stage || !img || !counter || !prevBtn || !nextBtn) return;

    var rawPages   = stage.dataset.pages || '';
    var pages      = rawPages.split(',').filter(function (p) { return p.length > 0; });
    var pageCount  = pages.length;
    var currentPage = 1;

    if (pageCount === 0) return;   /* nothing to display */

    /* Read ?p= from the query string for deep linking. */
    var qs = new URLSearchParams(window.location.search);
    var initPage = parseInt(qs.get('p'), 10);
    if (!isNaN(initPage) && initPage >= 1 && initPage <= pageCount) {
        currentPage = initPage;
    }

    /* ------------------------------------------------------------------
       Navigation
    ------------------------------------------------------------------ */

    function navigate(page) {
        if (page < 1 || page > pageCount) return;
        currentPage = page;

        img.src = pages[currentPage - 1];
        img.alt = 'Score page ' + currentPage;
        counter.textContent = 'p. ' + currentPage + ' / ' + pageCount;

        prevBtn.disabled = (currentPage === 1);
        nextBtn.disabled = (currentPage === pageCount);

        updateActiveMovement();

        /* Replace URL so the page is bookmarkable at the current position.
           The back button still returns to the landing page. */
        history.replaceState(null, '', '?p=' + currentPage);

        /* Preload the adjacent pages for smooth turning. */
        if (currentPage > 1)         new Image().src = pages[currentPage - 2];
        if (currentPage < pageCount) new Image().src = pages[currentPage];
    }

    /* ------------------------------------------------------------------
       Movement buttons — highlight the movement that contains currentPage
    ------------------------------------------------------------------ */

    var mvtButtons = Array.from(document.querySelectorAll('.score-reader-mvt'));

    function updateActiveMovement() {
        /* Find the last movement whose start page ≤ currentPage. */
        var active = null;
        mvtButtons.forEach(function (btn) {
            var p = parseInt(btn.dataset.page, 10);
            if (!isNaN(p) && p <= currentPage) active = btn;
        });
        mvtButtons.forEach(function (btn) {
            btn.classList.toggle('is-active', btn === active);
        });
    }

    mvtButtons.forEach(function (btn) {
        btn.addEventListener('click', function () {
            var p = parseInt(btn.dataset.page, 10);
            if (!isNaN(p)) navigate(p);
        });
    });

    /* ------------------------------------------------------------------
       Prev / next buttons
    ------------------------------------------------------------------ */

    prevBtn.addEventListener('click', function () { navigate(currentPage - 1); });
    nextBtn.addEventListener('click', function () { navigate(currentPage + 1); });

    /* ------------------------------------------------------------------
       Keyboard
    ------------------------------------------------------------------ */

    document.addEventListener('keydown', function (e) {
        /* Ignore keypresses while focus is in the settings panel. */
        var panel = document.querySelector('.settings-panel');
        if (panel && panel.classList.contains('is-open')) return;

        if (e.key === 'ArrowRight' || e.key === 'ArrowDown') {
            navigate(currentPage + 1);
            e.preventDefault();
        } else if (e.key === 'ArrowLeft' || e.key === 'ArrowUp') {
            navigate(currentPage - 1);
            e.preventDefault();
        } else if (e.key === 'Escape') {
            history.back();
        }
    });

    /* ------------------------------------------------------------------
       Touch swipe — left/right swipe to turn pages
       Threshold: ≥50px horizontal, <30px vertical drift
    ------------------------------------------------------------------ */

    var touchStartX = 0;
    var touchStartY = 0;

    stage.addEventListener('touchstart', function (e) {
        touchStartX = e.changedTouches[0].clientX;
        touchStartY = e.changedTouches[0].clientY;
    }, { passive: true });

    stage.addEventListener('touchend', function (e) {
        var dx = e.changedTouches[0].clientX - touchStartX;
        var dy = e.changedTouches[0].clientY - touchStartY;
        if (Math.abs(dx) < 50 || Math.abs(dy) > 30) return;
        if (dx < 0) navigate(currentPage + 1);
        else        navigate(currentPage - 1);
    }, { passive: true });

    /* ------------------------------------------------------------------
       Init — load the starting page
    ------------------------------------------------------------------ */

    navigate(currentPage);
}());
