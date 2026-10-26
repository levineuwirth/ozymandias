"""The "Show 25 / 50 / 100 / All" control on card lists, in Chromium and
Firefox, on built pages.

static/js/list-pagination.js hides the cards past the chosen count, marks
the chosen button pressed, and keeps the choice for every list page. A
stored count is the reader's own storage, so anything there is checked
against the buttons the page offers before it is used or saved again.

Checked: 25 by default; each choice shows that many cards and presses its
button alone; the choice is kept across a reload and on another list
page; a stored count that is not one of the buttons falls back to 25 and
is repaired in storage; storage that throws breaks nothing.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_lists -v
"""

from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, check_site, enforcing_csp, require_playwright,
                            requires_browser, site_server)

LIST = "/new.html"                     # more cards than the first choice
OTHER = "/research/graph-theory/"      # another list page, sharing the choice
KEY = "list-page-count"

STATE = """() => ({
    shown: [...document.querySelectorAll('.item-card')].filter(c => !c.hidden).length,
    total: document.querySelectorAll('.item-card').length,
    pressed: [...document.querySelectorAll('.list-count-btn')]
        .filter(b => b.getAttribute('aria-pressed') === 'true').map(b => b.dataset.count),
    active: [...document.querySelectorAll('.list-count-btn.is-active')].map(b => b.dataset.count),
})"""

NO_STORAGE = """
Object.defineProperty(window, 'localStorage', {configurable: true, get() {
    throw new DOMException('The operation is insecure.', 'SecurityError'); }});
"""


@requires_browser
class ListCounts(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import sync_playwright
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-lists-")))
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp()))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def open(self, browser: str, stored: str | None = None, init: str = ""):
        context = self.browsers[browser].new_context()
        self.addCleanup(context.close)
        if stored is not None:
            context.add_init_script(f"if (!sessionStorage.s) {{ localStorage.setItem({json.dumps(KEY)},"
                                    f" {json.dumps(stored)}); sessionStorage.s = 1; }}")
        if init:
            context.add_init_script(init)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        self.addCleanup(lambda: self.assertEqual(errors, [], "page errors"))
        page.goto(self.base + LIST, wait_until="load")
        return page

    def state(self, page) -> dict:
        return page.evaluate(STATE)

    def test_choices(self) -> None:
        for browser in BROWSERS:
            page = self.open(browser)
            total = self.state(page)["total"]
            self.assertGreater(total, 25, "the list page needs more cards than the first choice")
            with self.subTest(browser=browser, choice="the default"):
                self.assertEqual(self.state(page), {"shown": 25, "total": total,
                                                    "pressed": ["25"], "active": ["25"]})
            for count, shown in (("50", min(50, total)), ("all", total), ("100", min(100, total)),
                                 ("25", 25)):
                with self.subTest(browser=browser, choice=count):
                    page.click(f'.list-count-btn[data-count="{count}"]')
                    self.assertEqual(self.state(page), {"shown": shown, "total": total,
                                                        "pressed": [count], "active": [count]})
                    self.assertEqual(page.evaluate(f"localStorage.getItem({json.dumps(KEY)})"), count)
            with self.subTest(browser=browser, choice="kept: a reload, another list page"):
                page.click('.list-count-btn[data-count="50"]')
                page.reload(wait_until="load")
                self.assertEqual(self.state(page)["pressed"], ["50"])
                page.goto(self.base + OTHER, wait_until="load")
                other = self.state(page)
                self.assertEqual(other["pressed"], ["50"])
                self.assertEqual(other["shown"], min(50, other["total"]))

    def test_stored_counts_that_are_not_choices(self) -> None:
        for browser in BROWSERS:
            for stored in ("7", "ALL", "", "Infinity", "25; drop", "-1", "1e9"):
                with self.subTest(browser=browser, stored=stored):
                    page = self.open(browser, stored)
                    state = self.state(page)
                    self.assertEqual((state["shown"], state["pressed"]), (25, ["25"]))
                    self.assertEqual(page.evaluate(f"localStorage.getItem({json.dumps(KEY)})"), "25")

    def test_storage_that_throws(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser, init=NO_STORAGE)
                self.assertEqual(self.state(page)["shown"], 25)
                page.click('.list-count-btn[data-count="all"]')
                state = self.state(page)
                self.assertEqual((state["shown"], state["pressed"]), (state["total"], ["all"]))


if __name__ == "__main__":
    unittest.main()
