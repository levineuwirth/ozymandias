"""Relative dates in the reader's own calendar, in Chromium and Firefox
(audit J09).

A fixture page carries a date popup, a date-range popup (popups.js) and a
Current-page stamp (now.js), all dated 1 October 2026. Each case sets the
browser's time zone and freezes its clock, then reads what the page says.
Both scripts count calendar days the same way (lnUtils.isoDay/localDay,
tested alone in tests/test_js_dates.py); this checks that they use it,
and the popups' phrasing: past 345 days they say "~1 year", not
"~12 months".

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_dates -v
"""

from __future__ import annotations

import tempfile
import unittest
from datetime import datetime, timedelta
from pathlib import Path

from tests._browser import (BROWSERS, check_site, enforcing_csp, require_playwright,
                            requires_browser, site_server)

PAGE = """<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Dates fixture</title>
<script src="/js/utils.js"></script>
<script src="/js/popups.js" defer></script>
<script src="/js/now.js" defer></script>
</head><body><main id="markdownBody">
<p class="now-stamp">Updated <time class="now-stamp-date" datetime="2026-10-01">1 October 2026</time>
</p>
<p><span id="single" data-date-start="2026-10-01">1 October 2026</span></p>
<p><span id="range" data-date-start="2026-09-01" data-date-end="2026-09-29">September</span></p>
</main></body></html>
"""

# (time zone, the reader's wall clock) -> what the single-date popup, the
# range popup and the Current stamp say; None where nothing is shown.
CASES = [
    # The audit's two: an evening in Los Angeles, a morning in Tokyo,
    # both on the date itself.
    ("America/Los_Angeles", "2026-10-01T20:00:00-07:00",
     "today", "~4 weeks · started ~4 weeks ago", "today"),
    ("Asia/Tokyo", "2026-10-01T08:00:00+09:00",
     "today", "~4 weeks · started ~4 weeks ago", "today"),
    # The day before, anywhere: a future date has no popup and no stamp.
    ("Europe/Berlin", "2026-09-30T23:30:00+02:00",
     None, "~4 weeks · started ~4 weeks ago", None),
    # 350 days on: the popup rounds to a year; now.js clamps at 11 months.
    ("Europe/Berlin", "2027-09-16T12:00:00+02:00",
     "~1 year ago", "~4 weeks · started ~1 year ago", "11 months ago"),
]


@requires_browser
class RelativeDates(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-dates-")))
        (tmp / "fixtures").mkdir()
        (tmp / "fixtures" / "dates.html").write_text(PAGE, encoding="utf-8")
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp(), tmp / "fixtures"))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def page_at(self, browser: str, tz: str, clock: str):
        context = self.browsers[browser].new_context(timezone_id=tz)
        self.addCleanup(context.close)
        context.clock.set_fixed_time(datetime.fromisoformat(clock))
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        page.goto(self.base + "/__fixture/dates.html", wait_until="networkidle")
        return page, errors

    def popup_text(self, page, target: str) -> str | None:
        page.mouse.move(0, 0)
        page.wait_for_timeout(500)  # the previous popup's hide delay
        page.hover(target)
        shown = page.locator(".link-popup.is-visible .popup-date-primary")
        try:
            shown.wait_for(state="visible", timeout=2000)
        except Exception:
            return None
        return shown.inner_text().strip()

    def test_current_age_between_builds(self) -> None:
        for name, browser in self.browsers.items():
            with self.subTest(browser=name):
                context = browser.new_context(java_script_enabled=False)
                try:
                    page = context.new_page()
                    page.goto(self.base + "/current.html")
                    stamp = page.locator(".now-stamp-date").get_attribute("datetime")
                    self.assertTrue(stamp)
                    self.expect(page.locator(".now-stamp-relative")).to_have_count(0)
                finally:
                    context.close()
                # The same built HTML supplies a fresh age on later visits.
                for days, phrase in [(1, "yesterday"), (8, "1 week ago")]:
                    context = browser.new_context(timezone_id="Europe/Copenhagen")
                    try:
                        context.clock.set_fixed_time(
                            datetime.fromisoformat(stamp + "T12:00:00+02:00")
                            + timedelta(days=days))
                        page = context.new_page()
                        page.goto(self.base + "/current.html")
                        self.expect(page.locator(".now-stamp-relative")).to_have_text(phrase)
                    finally:
                        context.close()

    def test_phrases_in_each_time_zone(self) -> None:
        for browser in BROWSERS:
            for tz, clock, single, span, stamp in CASES:
                with self.subTest(browser=browser, tz=tz, clock=clock):
                    page, errors = self.page_at(browser, tz, clock)
                    # The zone and the clock took: the page's wall clock is the case's.
                    wall = datetime.fromisoformat(clock)
                    self.assertEqual(
                        page.evaluate("(d => [d.getFullYear(), d.getMonth() + 1, d.getDate(),"
                                      " d.getHours(), d.getMinutes()])(new Date())"),
                        [wall.year, wall.month, wall.day, wall.hour, wall.minute])
                    relative = page.locator(".now-stamp-relative")
                    if stamp is None:
                        self.expect(relative).to_have_count(0)
                    else:
                        self.expect(relative).to_have_text(stamp)
                    self.assertEqual(self.popup_text(page, "#single"), single)
                    self.assertEqual(self.popup_text(page, "#range"), span)
                    self.assertEqual(errors, [])


if __name__ == "__main__":
    unittest.main()
