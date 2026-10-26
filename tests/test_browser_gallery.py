"""The equation gallery, in Chromium and Firefox.

gallery.js wraps each display equation of an essay and gives it an
expand control; the overlay shows one equation at a time, large, steps
through them all, and returns focus where it came from. Run on a built
essay with many display equations (rendered by KaTeX in the page).

Checked: one named button per equation and the equation itself left as
it is (audit J04: each was an unnamed role="button" tab stop); the
overlay by keyboard and by pointer, its counter, arrows, Tab trap,
Escape and focus return.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_gallery -v
"""

from __future__ import annotations

import re
import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, FakeNetwork, check_site, enforcing_csp,
                            require_playwright, requires_browser, site_server)

ESSAY = "/essays/growing-radius-domination.html"
OVERLAY = "#gallery-overlay"


@requires_browser
class Gallery(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-gallery-")))
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp()))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def open(self, browser: str):
        context = self.browsers[browser].new_context(viewport={"width": 1280, "height": 900})
        self.addCleanup(context.close)
        net = FakeNetwork(context, self.base, {}, {})   # nothing may leave serve.py
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        self.addCleanup(lambda: self.assertEqual(errors, [], "page errors"))
        self.addCleanup(lambda: self.assertEqual(net.unexpected, [], "requests leaving serve.py"))
        page.goto(self.base + ESSAY, wait_until="load")
        self.expect(page.locator(".math-focusable .exhibit-expand-btn").first).to_be_attached()
        return page

    def test_each_equation_has_a_named_button(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                n = page.locator(".katex-display").count()
                self.assertGreater(n, 5)
                buttons = page.locator(".math-focusable > button.exhibit-expand-btn")
                self.expect(buttons).to_have_count(n)
                labels = [b.get_attribute("aria-label") for b in buttons.all()]
                self.assertTrue(all(re.fullmatch(r"Expand equation \d+( in “.+”)?", l) for l in labels), labels)
                # Numbered within each section: no two buttons share a name.
                self.assertEqual(len(set(labels)), n)
                # The equation itself is not a control.
                self.expect(page.locator(".math-focusable[role], .math-focusable[tabindex], "
                                         ".katex-display[role=button]")).to_have_count(0)

    def test_overlay_by_keyboard(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                n = page.locator(".katex-display").count()
                first = page.locator(".math-focusable > button.exhibit-expand-btn").first
                first.focus()
                page.keyboard.press("Enter")
                overlay = page.locator(OVERLAY)
                self.expect(overlay).to_be_visible()
                self.expect(overlay).to_have_attribute("aria-modal", "true")
                self.expect(page.locator(f"{OVERLAY} .katex").first).to_be_visible()
                counter = page.locator("#gallery-overlay-counter")
                self.expect(counter).to_have_text(f"1 / {n}")
                page.keyboard.press("ArrowRight")
                self.expect(counter).to_have_text(f"2 / {n}")
                page.keyboard.press("ArrowLeft")
                page.keyboard.press("ArrowLeft")
                self.expect(counter).to_have_text(f"1 / {n}")   # it stops at the ends
                for _ in range(10):   # more than the overlay has stops
                    page.keyboard.press("Tab")
                    self.assertTrue(page.evaluate(
                        "document.getElementById('gallery-overlay').contains(document.activeElement)"))
                page.keyboard.press("Escape")
                self.expect(overlay).to_be_hidden()
                self.expect(first).to_be_focused()

    def test_overlay_by_pointer(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                second = page.locator(".math-focusable").nth(1)
                second.scroll_into_view_if_needed()
                second.locator(".katex-display").click()
                self.expect(page.locator(OVERLAY)).to_be_visible()
                self.expect(page.locator("#gallery-overlay-counter")).to_have_text(re.compile(r"^2 / \d+$"))
                page.click(f"{OVERLAY} [aria-label=Close]")
                self.expect(page.locator(OVERLAY)).to_be_hidden()


if __name__ == "__main__":
    unittest.main()
