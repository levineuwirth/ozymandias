"""The site header, in Chromium and Firefox, on a built essay.

The header is sticky, and its height is not fixed: one row on a desktop,
three on a phone, taller with the Portals row open or a larger text size,
and almost nothing in Focus Mode. static/js/nav.js measures it into
--nav-height, from which base.css sets scroll-padding-top: where every
in-page anchor lands. A wrong value lands a heading under the header.

Checked: --nav-height is the header's height at 1440 and 375px, at the
default and the largest text size, with Portals open and closed, and in
Focus Mode; a deep link and an in-page link put their heading just below
the header in each of those; the Portals toggle (state, arrow, the choice
kept, storage that throws); and Return to top, at once under Reduce
Motion and smoothly otherwise.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_nav -v
"""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, check_site, enforcing_csp, require_playwright,
                            requires_browser, site_server)

ESSAY = "/essays/proof-broker/"
HEADER = "body > header:not(.essay-frontmatter)"

MEASURE = f"""() => {{
    const h = document.querySelector('{HEADER}').getBoundingClientRect();
    return {{header: Math.round(h.height), bottom: h.bottom,
             nav: parseFloat(getComputedStyle(document.documentElement).getPropertyValue('--nav-height')),
             rem: parseFloat(getComputedStyle(document.documentElement).fontSize)}};
}}"""

# Two frames: ResizeObserver's callback, then the style it set.
FRAMES = "new Promise(r => requestAnimationFrame(() => requestAnimationFrame(r)))"

NO_STORAGE = """
Object.defineProperty(window, 'localStorage', {configurable: true, get() {
    throw new DOMException('The operation is insecure.', 'SecurityError'); }});
"""


@requires_browser
class SiteHeader(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-nav-")))
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp()))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def open(self, browser: str, path: str = ESSAY, width: int = 1440, *, text_size: int | None = None,
             portals: bool = False, focus: bool = False, reduce: bool = True, init: str = ""):
        """`path` loaded; reduced motion by default, so jumps are not animated."""
        context = self.browsers[browser].new_context(
            viewport={"width": width, "height": 900 if width > 500 else 812},
            reduced_motion="reduce" if reduce else "no-preference")
        self.addCleanup(context.close)
        seed = ""
        if text_size:
            seed += f"localStorage.setItem('text-size', '{text_size}');"
        if portals:
            seed += "localStorage.setItem('portals-open', '1');"
        if focus:
            seed += "localStorage.setItem('focus-mode', '1');"
        if seed:
            context.add_init_script(f"try {{ {seed} }} catch (e) {{}}")
        if init:
            context.add_init_script(init)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        self.addCleanup(lambda: self.assertEqual(errors, [], "page errors"))
        page.goto(self.base + path, wait_until="load")
        page.evaluate("document.fonts.ready.then(() => true)")
        page.evaluate(FRAMES)
        return page

    def measure(self, page) -> dict:
        page.evaluate(FRAMES)
        return page.evaluate(MEASURE)

    # Every layout the header takes: (label, open() arguments).
    LAYOUTS = [
        ("desktop", {"width": 1440}),
        ("desktop, Portals open", {"width": 1440, "portals": True}),
        ("desktop, largest text", {"width": 1440, "text_size": 29}),
        ("phone", {"width": 375}),
        ("phone, Portals open", {"width": 375, "portals": True}),
        ("phone, largest text", {"width": 375, "text_size": 29}),
        ("Focus Mode", {"width": 1440, "focus": True}),
    ]

    def test_nav_height_is_the_headers(self) -> None:
        for browser in BROWSERS:
            for label, args in self.LAYOUTS:
                with self.subTest(browser=browser, layout=label):
                    page = self.open(browser, **args)
                    m = self.measure(page)
                    self.assertGreater(m["header"], 0)
                    self.assertEqual(m["nav"], m["header"], m)
            with self.subTest(browser=browser, layout="Portals opened and closed on the page"):
                page = self.open(browser, width=375)
                closed = self.measure(page)
                page.click(".nav-portal-toggle")
                opened = self.measure(page)
                self.assertGreater(opened["header"], closed["header"])
                self.assertEqual(opened["nav"], opened["header"])
                page.click(".nav-portal-toggle")
                self.assertEqual(self.measure(page)["nav"], closed["header"])

    def heading(self, page) -> str:
        """The id of a heading well down the essay."""
        ids = page.eval_on_selector_all("#markdownBody h2[id]", "hs => hs.map(h => h.id)")
        self.assertGreater(len(ids), 3)
        return ids[3]

    def landing(self, page, target: str) -> tuple[float, float, float]:
        page.evaluate(FRAMES)
        top = page.evaluate(f"document.getElementById({target!r}).getBoundingClientRect().top")
        m = page.evaluate(MEASURE)
        return top, m["bottom"], m["rem"]

    def assert_below_the_header(self, top: float, bottom: float, rem: float) -> None:
        # base.css: scroll-padding-top is the header plus 1.25rem of air.
        self.assertGreaterEqual(top, bottom - 1, "under the header")
        self.assertLessEqual(top, bottom + 1.25 * rem + 4, "well below the header")

    def test_anchors_land_below_the_header(self) -> None:
        for browser in BROWSERS:
            target = self.heading(self.open(browser))
            for label, args in self.LAYOUTS:
                with self.subTest(browser=browser, layout=label, how="a deep link"):
                    page = self.open(browser, f"{ESSAY}#{target}", **args)
                    self.assert_below_the_header(*self.landing(page, target))
                with self.subTest(browser=browser, layout=label, how="an in-page link"):
                    page = self.open(browser, **args)
                    page.evaluate(f"location.hash = {('#' + target)!r}")
                    self.assert_below_the_header(*self.landing(page, target))

    def test_portals_toggle(self) -> None:
        toggle, row = ".nav-portal-toggle", ".nav-portals"
        for browser in BROWSERS:
            with self.subTest(browser=browser, part="closed by default, opened, kept"):
                page = self.open(browser)
                self.expect(page.locator(toggle)).to_have_attribute("aria-expanded", "false")
                self.expect(page.locator(".nav-portal-arrow")).to_have_text("▼")
                self.expect(page.locator(f"{row} a").first).to_be_hidden()
                page.click(toggle)
                self.expect(page.locator(toggle)).to_have_attribute("aria-expanded", "true")
                self.expect(page.locator(".nav-portal-arrow")).to_have_text("▲")
                self.expect(page.locator(f"{row} a").first).to_be_visible()
                self.assertEqual(page.evaluate("localStorage.getItem('portals-open')"), "1")
                page.reload(wait_until="load")
                self.expect(page.locator(toggle)).to_have_attribute("aria-expanded", "true")
                self.expect(page.locator(f"{row} a").first).to_be_visible()
                page.click(toggle)
                self.expect(page.locator(f"{row} a").first).to_be_hidden()
                self.assertEqual(page.evaluate("localStorage.getItem('portals-open')"), "0")
            with self.subTest(browser=browser, part="storage that throws"):
                page = self.open(browser, init=NO_STORAGE)
                page.click(toggle)
                self.expect(page.locator(toggle)).to_have_attribute("aria-expanded", "true")
                self.expect(page.locator(f"{row} a").first).to_be_visible()

    def test_return_to_top(self) -> None:
        bottom = "window.scrollTo({top: document.body.scrollHeight, behavior: 'instant'})"
        for browser in BROWSERS:
            with self.subTest(browser=browser, motion="the site's Reduce Motion"):
                page = self.open(browser, reduce=False,
                                 init="try { localStorage.setItem('reduce-motion', '1'); } catch (e) {}")
                page.evaluate(bottom)
                page.evaluate(FRAMES)
                self.assertGreater(page.evaluate("scrollY"), 1000)
                page.locator(".footer-totop").evaluate("b => b.click()")
                page.evaluate(FRAMES)
                self.assertEqual(page.evaluate("scrollY"), 0, "not at once")
            with self.subTest(browser=browser, motion="none asked for"):
                page = self.open(browser, reduce=False)
                page.evaluate(bottom)
                page.evaluate(FRAMES)
                page.locator(".footer-totop").evaluate("b => b.click()")
                page.evaluate(FRAMES)
                self.assertGreater(page.evaluate("scrollY"), 0, "jumped: no smooth scroll")
                for _ in range(40):
                    if page.evaluate("scrollY") == 0:
                        break
                    page.wait_for_timeout(100)
                self.assertEqual(page.evaluate("scrollY"), 0)


if __name__ == "__main__":
    unittest.main()
