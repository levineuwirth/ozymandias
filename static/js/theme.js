/* theme.js — Restores theme and text size from localStorage before first paint.
   Loaded synchronously (no defer/async) to prevent flash of wrong appearance.
   DOM interaction (button wiring) is handled by settings.js (deferred).

   All storage access routes through window.lnUtils.safeStorage (from
   utils.js, loaded immediately before this script) so Safari private-mode
   SecurityErrors degrade to default appearance rather than blowing up
   the synchronous bootstrap.
*/
(function () {
    var TEXT_SIZES = [20, 23, 26];
    var store = window.lnUtils && window.lnUtils.safeStorage;
    function safeGet(key) { return store ? store.get(key) : null; }

    /* Theme */
    var storedTheme = safeGet('theme');
    if (storedTheme === 'dark' || storedTheme === 'light' || storedTheme === 'cappuccino') {
        document.documentElement.setAttribute('data-theme', storedTheme);
    }

    /* Text size */
    var storedSize = parseInt(safeGet('text-size'), 10);
    if (!isNaN(storedSize) && storedSize >= 0 && storedSize < TEXT_SIZES.length) {
        document.documentElement.style.setProperty('--text-size', TEXT_SIZES[storedSize] + 'px');
    }

    /* Focus mode */
    if (safeGet('focus-mode')) {
        document.documentElement.setAttribute('data-focus-mode', '');
    }

    /* Reduce motion */
    if (safeGet('reduce-motion')) {
        document.documentElement.setAttribute('data-reduce-motion', '');
    }
})();
