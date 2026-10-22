/* list-pagination.js — "show N items" control for card lists.
 *
 * The chosen count is persisted in localStorage and shared across every
 * list page. Persisted values are untrusted input (another page, an older
 * build, or a hand-edited storage entry can put anything there), so the
 * saved string is validated against the options actually offered by the
 * page's own buttons before it is used or re-saved. Anything else falls
 * back to DEFAULT.
 */
(function () {
    var STORAGE_KEY = 'list-page-count';
    var DEFAULT     = '25';

    /* Used only if the page renders no count buttons at all — keeps the
       validator meaningful rather than accept-everything. Mirrors
       templates/partials/list-controls.html. */
    var FALLBACK_OPTIONS = ['25', '50', '100', 'all'];

    function supportedCounts() {
        var opts = [];
        document.querySelectorAll('.list-count-btn').forEach(function (btn) {
            var v = btn.dataset.count;
            if (v && opts.indexOf(String(v)) === -1) opts.push(String(v));
        });
        return opts.length ? opts : FALLBACK_OPTIONS;
    }

    /* Coerce any candidate to one of the supported options. */
    function validate(n) {
        var opts = supportedCounts();
        var v    = (n === null || n === undefined) ? '' : String(n);
        if (opts.indexOf(v) !== -1) return v;
        /* DEFAULT itself must be offered by this page; otherwise take the
           first option so the list is never left with an unusable limit. */
        return opts.indexOf(DEFAULT) !== -1 ? DEFAULT : opts[0];
    }

    function applyCount(n) {
        var value   = validate(n);
        var entries = document.querySelectorAll('.item-card');
        var limit   = (value === 'all') ? Infinity : parseInt(value, 10);
        if (isNaN(limit)) limit = Infinity;
        entries.forEach(function (el, i) {
            el.hidden = i >= limit;
        });
        document.querySelectorAll('.list-count-btn').forEach(function (btn) {
            var active = String(btn.dataset.count) === value;
            btn.classList.toggle('is-active', active);
            /* The template ships aria-pressed defaults for the 25 button;
               a restored preference must move the pressed state with the
               class, or the accessibility tree disagrees with the styling. */
            btn.setAttribute('aria-pressed', String(active));
        });
        /* Only ever persist a validated value, so a bad entry is repaired
           rather than propagated to the next page. */
        try { localStorage.setItem(STORAGE_KEY, value); } catch (e) {}
    }

    document.addEventListener('DOMContentLoaded', function () {
        var saved = null;
        try { saved = localStorage.getItem(STORAGE_KEY); } catch (e) {}
        applyCount(saved);

        document.querySelectorAll('.list-count-btn').forEach(function (btn) {
            btn.addEventListener('click', function () { applyCount(btn.dataset.count); });
        });
    });
}());
