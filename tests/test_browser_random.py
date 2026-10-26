"""The homepage's Random link, in Chromium and Firefox (audit J13).

static/js/random.js sends a click to a random page from /random-pages.json.
The link's own href (/new.html, content/index.md) is where it goes without
JavaScript, when the list cannot be had, and on a click that asks for a new
tab. It used to be href="#": without the list, or without script, the link
did nothing. Only site paths in the list are followed; a list with none
falls back too.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_random -v
"""

from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path
from urllib.parse import urlsplit

from tests._browser import (BROWSERS, SITE, check_site, enforcing_csp, require_playwright,
                            requires_browser, site_server)

LINK = "[data-random]"
FALLBACK = "/new.html"


@requires_browser
class RandomLink(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import sync_playwright
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-random-")))
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp()))
        cls.pages = json.loads((SITE / "random-pages.json").read_text(encoding="utf-8"))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def home(self, browser: str, *, javascript: bool = True, pages_json=None):
        """The homepage, with /random-pages.json answered by `pages_json`
        (a route handler) when given."""
        context = self.browsers[browser].new_context(java_script_enabled=javascript)
        self.addCleanup(context.close)
        if pages_json:
            context.route("**/random-pages.json", pages_json)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        self.addCleanup(lambda: self.assertEqual(errors, []))
        page.goto(self.base + "/", wait_until="networkidle")
        return context, page

    def click_and_land(self, page) -> str:
        with page.expect_navigation(timeout=10000):
            page.click(LINK)
        return urlsplit(page.url).path

    def test_the_fallback_exists(self) -> None:
        # Landing on FALLBACK below proves the path; a missing page would
        # land there too, as the 404 page.
        self.assertTrue((SITE / FALLBACK.lstrip("/")).is_file())

    def test_goes_to_a_listed_page(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                _, page = self.home(browser)
                self.assertIn(self.click_and_land(page), self.pages)

    def test_falls_back_when_the_list_cannot_be_had(self) -> None:
        answers = {
            "missing": lambda route: route.fulfill(status=404, body="not found"),
            "unreachable": lambda route: route.abort(),
            "not json": lambda route: route.fulfill(status=200, body="<html>",
                                                    content_type="application/json"),
            "empty": lambda route: route.fulfill(status=200, body="[]",
                                                 content_type="application/json"),
            "not a list": lambda route: route.fulfill(status=200, body='{"length": 3}',
                                                      content_type="application/json"),
        }
        # Lists with no site path in them.
        for case, entries in {
            "null entry": [None],
            "number": [42],
            "empty string": [""],
            "other origin": ["https://example.org/"],
            "protocol-relative": ["//example.org/essays/"],
            "javascript: URL": ["javascript:alert(1)"],
            "relative path": ["essays/"],
        }.items():
            answers[case] = (lambda body: lambda route: route.fulfill(
                status=200, body=body, content_type="application/json"))(json.dumps(entries))
        for browser in BROWSERS:
            for case, answer in answers.items():
                with self.subTest(browser=browser, case=case):
                    _, page = self.home(browser, pages_json=answer)
                    self.assertEqual(self.click_and_land(page), FALLBACK)

    def test_only_site_paths_are_chosen(self) -> None:
        mixed = json.dumps([None, "", "//example.org/", "javascript:alert(1)", self.pages[0]])
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                _, page = self.home(browser, pages_json=lambda route: route.fulfill(
                    status=200, body=mixed, content_type="application/json"))
                self.assertEqual(self.click_and_land(page), self.pages[0])

    def test_without_javascript(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                _, page = self.home(browser, javascript=False)
                self.assertEqual(self.click_and_land(page), FALLBACK)

    def test_a_new_tab_is_left_to_the_browser(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                context, page = self.home(browser)
                with context.expect_page(timeout=10000) as opened:
                    page.click(LINK, modifiers=["ControlOrMeta"])
                tab = opened.value
                # A new tab starts at about:blank: its load state can be that
                # page's, before it navigates.
                tab.wait_for_url(f"**{FALLBACK}", timeout=10000)
                self.assertEqual(urlsplit(tab.url).path, FALLBACK)
                self.assertEqual(urlsplit(page.url).path, "/")


if __name__ == "__main__":
    unittest.main()
