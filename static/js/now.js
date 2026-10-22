/* now.js — Keep the Current page's "Last updated" relative phrase
   honest.

   The static page carries only the absolute `<time datetime>` date.
   Add the relative phrase in the browser against the visitor's clock;
   without JavaScript the absolute date remains accurate between builds.

   Bucket table (days = whole calendar days elapsed):

     d <   0            ""                       future / clock skew
     d ==  0            "today"
     d ==  1            "yesterday"
     d <   7            "<d> days ago"           2–6
     d <  30            "<floor(d/7)> weeks"     7–29  → 1–4 weeks
     d < 365            "<floor(d/30)> months"   30–364 → 1–11 months,
                                                 clamped at 11 so the last
                                                 few days before a year do
                                                 not read "12 months ago"
     otherwise          "<floor(d/365)> years"

   The week bucket runs to day 29 (not day 27) so that a month is only
   ever named once one can actually be expressed: the old boundary at 28
   produced "0 months ago" for 28- and 29-day-old stamps. */
(function () {
    'use strict';

    function relative(days) {
        if (days < 0)   return '';            // future / clock skew
        if (days === 0) return 'today';
        if (days === 1) return 'yesterday';
        if (days < 7)   return days + ' days ago';

        var n, unit;
        if (days < 30)       { n = Math.floor(days / 7);                unit = 'week';  }
        else if (days < 365) { n = Math.min(11, Math.floor(days / 30)); unit = 'month'; }
        else                 { n = Math.floor(days / 365);              unit = 'year';  }
        return n === 1 ? ('1 ' + unit + ' ago')
                       : (n + ' ' + unit + 's ago');
    }

    function update() {
        var stamp = document.querySelector('.now-stamp');
        if (!stamp) return;

        var timeEl = stamp.querySelector('.now-stamp-date');
        if (!timeEl) return;

        // Calendar days between the stamp's date and the visitor's own
        // (lnUtils.isoDay/localDay, shared with the date popups). Without
        // utils.js, or with an unparseable date, only the absolute date stays.
        var u = window.lnUtils;
        if (!u || !u.isoDay) return;
        var then = u.isoDay(timeEl.getAttribute('datetime'));
        if (then === null) return;
        var text = relative(u.localDay() - then);

        var rel = stamp.querySelector('.now-stamp-relative');
        if (!text) {
            // No meaningful relative phrase (e.g. dated in the future):
            // drop any stale server-rendered one rather than keep a lie.
            if (rel) rel.remove();
            return;
        }
        if (!rel) {
            rel = document.createElement('span');
            rel.className = 'now-stamp-relative';
            stamp.appendChild(rel);
        }
        rel.textContent = text;
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', update);
    } else {
        update();
    }
})();
