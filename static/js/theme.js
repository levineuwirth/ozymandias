/* theme.js — Restores theme, season and text size from localStorage before first paint.
   Loaded synchronously (no defer/async) to prevent flash of wrong appearance.
   DOM interaction (button wiring) is handled by settings.js (deferred).

   All storage access routes through window.lnUtils.safeStorage (from
   utils.js, loaded immediately before this script) so Safari private-mode
   SecurityErrors degrade to default appearance rather than blowing up
   the synchronous bootstrap.
*/
(function () {
    var store = window.lnUtils && window.lnUtils.safeStorage;
    function safeGet(key) { return store ? store.get(key) : null; }

    /* Theme */
    var storedTheme = safeGet('theme');
    if (storedTheme === 'dark' || storedTheme === 'light' || storedTheme === 'cappuccino') {
        document.documentElement.setAttribute('data-theme', storedTheme);
    }

    /* Text size: the reader's choice, read as settings.js reads it. */
    var size = window.lnUtils && window.lnUtils.storedTextSize
        ? window.lnUtils.storedTextSize() : null;
    if (size !== null) {
        document.documentElement.style.setProperty('--text-size', size + 'px');
    }

    /* Focus mode */
    if (safeGet('focus-mode')) {
        document.documentElement.setAttribute('data-focus-mode', '');
    }

    /* Reduce motion */
    if (safeGet('reduce-motion')) {
        document.documentElement.setAttribute('data-reduce-motion', '');
    }

    /* The Portals row, if the reader left it open (nav.js keeps this in
       step). Restored by nav.js alone, it opened after the first paint:
       the header grew under the reader, after nav.js had measured it, and
       a deep link's heading landed under the taller header. */
    if (safeGet('portals-open') === '1') {
        document.documentElement.setAttribute('data-portals-open', '');
    }

    /* Seasons: site.yaml's, as head.html's ozy:season metas. A season
       dresses a base theme: it is worn as data-season beside data-theme.
       While its window is open (month-day, inclusive; a window whose start
       is later than its end runs through the new year) a reader with no
       stored theme wears it, and one who chose it in Settings keeps it;
       once the window closes, one who chose it gets its base theme. The
       open seasons go in data-seasons-open, for settings.js to show their
       buttons. A worn season's script waits for stage.js, which loads it
       (static/js/stage.js); settings.js loads stage.js when a reader picks
       a season later. Malformed metas are skipped. */
    var root = document.documentElement;
    var today = new Date(), day = (today.getMonth() + 1) * 100 + today.getDate();
    var themes = ['dark', 'light', 'cappuccino'];
    var open = [], worn = null;
    function monthDay(value) {
        var m = /^(\d\d)-(\d\d)$/.exec(value || '');
        return m ? parseInt(m[1], 10) * 100 + parseInt(m[2], 10) : null;
    }
    Array.prototype.forEach.call(document.querySelectorAll('meta[name="ozy:season"]'), function (meta) {
        var id = meta.getAttribute('content'), base = meta.getAttribute('data-base');
        var from = monthDay(meta.getAttribute('data-from')), to = monthDay(meta.getAttribute('data-to'));
        if (!id || themes.indexOf(base) < 0 || from === null || to === null) return;
        var isOpen = from <= to ? day >= from && day <= to : day >= from || day <= to;
        if (isOpen) open.push(id);
        if (storedTheme === id) {
            root.setAttribute('data-theme', base);
            if (isOpen) worn = meta;
        } else if (isOpen && !worn && themes.indexOf(storedTheme) < 0 && !storedSeason()) {
            worn = meta;
        }
    });
    /* A stored theme that names a declared season counts as a choice
       even out of its window: no other season is put on over it. */
    function storedSeason() {
        return !!storedTheme && !!document.querySelector(
            'meta[name="ozy:season"][content="' + storedTheme.replace(/[^a-z0-9-]/g, '') + '"]');
    }
    if (worn) {
        root.setAttribute('data-theme', worn.getAttribute('data-base'));
        root.setAttribute('data-season', worn.getAttribute('content'));
        if (worn.hasAttribute('data-script')) {
            var stage = document.createElement('script');
            stage.src = '/js/stage.js';
            document.head.appendChild(stage);
        }
    }
    if (open.length) root.setAttribute('data-seasons-open', open.join(' '));
})();
