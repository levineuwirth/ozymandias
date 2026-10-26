"""The table of contents, in Chromium and Firefox, on a built essay.

static/js/toc.js marks the section being read in the contents and names
it in the contents' header (and in the narrow screens' bar), drives the
reading-progress line, and folds the contents away and back.

Checked: at the top the header names the page (its title in the body, or
"Contents"); scrolling a section into
reading position marks its entry, alone, for sight and for assistive
technology (aria-current), and names it; a deep link marks its section
from the start; folding the contents takes its links out of the tab order
and the accessibility tree, and unfolding restores them; progress runs
from 0 at the top to 1 at the foot; on a phone the bar names the section.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_toc -v
"""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, check_site, enforcing_csp, require_playwright,
                            requires_browser, site_server)

ESSAY = "/essays/proof-broker/"

STATE = """() => {
    const toc = document.getElementById('toc');
    const active = [...toc.querySelectorAll('a[data-target].is-active')].map(a => a.dataset.target);
    const current = [...toc.querySelectorAll('a[data-target][aria-current]')]
        .filter(a => a.getAttribute('aria-current') !== 'false').map(a => a.dataset.target);
    return {active, current,
            label: toc.querySelector('.toc-active-label').textContent.trim(),
            mobile: (document.querySelector('#toc-mobile-bar .toc-mobile-label') || {}).textContent,
            progress: parseFloat(toc.style.getPropertyValue('--toc-progress'))};
}"""

FRAMES = "new Promise(r => requestAnimationFrame(() => requestAnimationFrame(r)))"


@requires_browser
class TableOfContents(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-toc-")))
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp()))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def open(self, browser: str, path: str = ESSAY, width: int = 1440):
        context = self.browsers[browser].new_context(
            viewport={"width": width, "height": 900 if width > 500 else 812}, reduced_motion="reduce")
        self.addCleanup(context.close)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        self.addCleanup(lambda: self.assertEqual(errors, [], "page errors"))
        page.goto(self.base + path, wait_until="load")
        page.evaluate(FRAMES)
        return page

    def state(self, page) -> dict:
        page.wait_for_timeout(150)      # IntersectionObserver reports a frame or two on
        return page.evaluate(STATE)

    def entries(self, page) -> list[tuple[str, str]]:
        return page.eval_on_selector_all("#toc a[data-target]",
                                         "as => as.map(a => [a.dataset.target, a.dataset.label])")

    def read(self, page, target: str) -> None:
        """Scroll `target` into reading position: just inside the band
        toc.js watches, 10-15% down the window."""
        page.evaluate(f"""() => {{ const h = document.getElementById({target!r});
            window.scrollTo({{top: h.getBoundingClientRect().top + scrollY - innerHeight * 0.12,
                              behavior: 'instant'}}); }}""")

    def test_the_section_being_read(self) -> None:
        for browser in BROWSERS:
            page = self.open(browser)
            # toc.js names the page's title found in the body, else "Contents"
            # (an essay's title is in the header above the body).
            title = page.evaluate("(document.querySelector('#markdownBody .page-title') || "
                                  "{textContent: 'Contents'}).textContent.trim()")
            with self.subTest(browser=browser, at="the top"):
                self.assertEqual(self.state(page)["active"], [])
                self.assertEqual(self.state(page)["label"], title)
            for target, label in self.entries(page)[1:6:2]:
                with self.subTest(browser=browser, at=target):
                    self.read(page, target)
                    state = self.state(page)
                    self.assertEqual(state["active"], [target])
                    self.assertEqual(state["current"], [target], "aria-current")
                    self.assertEqual(state["label"], label)
            with self.subTest(browser=browser, at="back at the top"):
                page.evaluate("window.scrollTo({top: 0, behavior: 'instant'})")
                state = self.state(page)
                self.assertEqual((state["active"], state["current"], state["label"]), ([], [], title))

    def test_a_deep_link_marks_its_section(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                target, label = self.entries(self.open(browser))[4]
                page = self.open(browser, f"{ESSAY}#{target}")
                state = self.state(page)
                self.assertEqual(state["active"], [target])
                self.assertEqual(state["label"], label)

    def test_folding(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                toggle, nav = page.locator(".toc-toggle"), page.locator("#toc .toc-nav")
                self.expect(toggle).to_have_attribute("aria-expanded", "true")
                toggle.click()
                self.expect(toggle).to_have_attribute("aria-expanded", "false")
                self.expect(nav).to_have_attribute("aria-hidden", "true")
                self.assertEqual(page.eval_on_selector_all(
                    "#toc a[data-target]", "as => as.filter(a => a.tabIndex !== -1).length"), 0)
                # Tab from the toggle leaves the contents.
                toggle.focus()
                page.keyboard.press("Tab")
                self.assertFalse(page.evaluate("document.getElementById('toc').contains(document.activeElement)"
                                               " && document.activeElement.matches('a[data-target]')"))
                toggle.click()
                self.expect(nav).to_have_attribute("aria-hidden", "false")
                # A frame on, as any reader's next key is: in the same frame
                # as the unfolding, Firefox could not yet place the links.
                page.evaluate("new Promise(r => requestAnimationFrame(() => requestAnimationFrame(r)))")
                toggle.focus()
                page.keyboard.press("Tab")
                self.assertTrue(page.evaluate("document.activeElement.matches('#toc a[data-target]')"))

    def test_progress(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                self.assertEqual(self.state(page)["progress"], 0)
                page.evaluate("window.scrollTo({top: document.documentElement.scrollHeight, behavior: 'instant'})")
                self.assertAlmostEqual(self.state(page)["progress"], 1, places=2)
                page.evaluate("window.scrollTo({top: (document.documentElement.scrollHeight - innerHeight) / 2,"
                              " behavior: 'instant'})")
                self.assertAlmostEqual(self.state(page)["progress"], 0.5, delta=0.02)

    def test_the_phone_bar_names_the_section(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser, width=375)
                target, label = self.entries(page)[3]
                self.read(page, target)
                self.assertEqual(self.state(page)["mobile"], label)


if __name__ == "__main__":
    unittest.main()
