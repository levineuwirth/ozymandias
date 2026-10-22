/* theme.js — Restores theme and text size from localStorage before first paint.
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
})();
