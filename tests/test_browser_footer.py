"""The footer's build time, in Chromium and Firefox.

Every page ships an empty <span data-build-time> (templates/partials/
footer.html); static/js/nav.js fills it from /build/time.txt, the one file
each build writes (tools/stamp-build-time.py), so unchanged pages stay
byte-identical between builds. Without JavaScript, or when the file is
missing, blank or unreachable, the span stays empty and nothing throws.
Served under the enforcing CSP, whose connect-src the fetch must pass.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_footer -v
"""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, SITE, check_site, enforcing_csp, require_playwright,
                            requires_browser, site_server)

# Three templates, and the 404 page, which nginx serves for any missing path.
PAGES = ("/", "/essays/proof-broker/", "/photography/denmark/", "/does-not-exist")
TIME_TXT = "**/build/time.txt"


@requires_browser
class FooterBuildTime(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import sync_playwright
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-footer-")))
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp()))
        cls.want = (SITE / "build" / "time.txt").read_text(encoding="utf-8").strip()
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def visit(self, browser: str, path: str, *, javascript: bool = True, time_txt=None):
        """The footer's text on `path` once the page is quiet, the page
        errors, and how many times the page asked for time.txt. `time_txt`,
        if given, answers that request in place of the server."""
        context = self.browsers[browser].new_context(java_script_enabled=javascript)
        self.addCleanup(context.close)
        page = context.new_page()
        errors, asked = [], []
        page.on("pageerror", lambda e: errors.append(str(e)))
        page.on("request", lambda r: asked.append(r.url) if r.url.endswith("/build/time.txt") else None)
        if time_txt:
            page.route(TIME_TXT, time_txt)
        page.goto(self.base + path, wait_until="networkidle")
        page.wait_for_timeout(200)  # the fetch's promise chain settles
        return page.locator("[data-build-time]").inner_text().strip(), errors, len(asked)

    def test_filled_from_time_txt(self) -> None:
        self.assertRegex(self.want, r"^\w+day, \w+ \d+(st|nd|rd|th), \d{4} \d\d:\d\d:\d\d$")
        for browser in BROWSERS:
            for path in PAGES:
                with self.subTest(browser=browser, path=path):
                    text, errors, asked = self.visit(browser, path)
                    self.assertEqual(text, self.want)
                    self.assertEqual(errors, [])
                    self.assertEqual(asked, 1)

    def test_empty_without_javascript(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                text, _, asked = self.visit(browser, "/", javascript=False)
                self.assertEqual(text, "")
                self.assertEqual(asked, 0)

    def test_empty_when_the_file_fails(self) -> None:
        answers = {
            "missing": lambda route: route.fulfill(status=404, body="not found"),
            "blank": lambda route: route.fulfill(status=200, body=" \n",
                                                 content_type="text/plain"),
            "unreachable": lambda route: route.abort(),
        }
        for browser in BROWSERS:
            for case, answer in answers.items():
                with self.subTest(browser=browser, case=case):
                    text, errors, asked = self.visit(browser, "/", time_txt=answer)
                    self.assertEqual(asked, 1)
                    self.assertEqual(text, "")
                    self.assertEqual(errors, [])


if __name__ == "__main__":
    unittest.main()
