"""The settings panel, in Chromium and Firefox, on built pages.

static/js/settings.js opens the panel from the gear in the header and
acts on its buttons; static/js/theme.js restores what they chose before
the first paint of every page. The panel carries the reader's
accessibility settings, so it is tested whole: opening and closing (the
gear, Escape, a click elsewhere) and where focus goes; the Tab trap; each
theme, applied, shown as pressed, kept and restored, and the page's colour
following it; text size, seven sizes from the middle one the page starts
at (it started at the smallest of three, which the panel took for the
middle), stepped to either end and reset, shown and announced, the
button at an end keeping focus (a disabled button dropped it), and a
size stored by the old three-size scale read as that size; Focus Mode and
Reduce Motion, on, off and restored, Reduce Motion stopping smooth
scrolling; Print, and Clear Annotations, confirmed or not; malformed
stored values ignored; storage that throws, as in some private windows,
breaking nothing; and, on the score reader, Escape closing the panel
without leaving the reader.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_settings -v
"""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, check_site, enforcing_csp, require_playwright,
                            requires_browser, site_server)

PAGE = "/essays/proof-broker/"
READER = "/music/bassoon-concerto/score/"
TOGGLE = ".settings-toggle"
PANEL = ".settings-panel"

# window.print and confirm, recorded instead of used; confirm answers
# window.__confirm.
STUBS = """
window.__printed = 0;
window.print = () => { window.__printed++; };
window.__confirm = true; window.__asked = 0;
window.confirm = () => { window.__asked++; return window.__confirm; };
"""

# localStorage that throws on every use, as Safari's private windows did.
NO_STORAGE = """
Object.defineProperty(window, 'localStorage', {configurable: true, get() {
    throw new DOMException('The operation is insecure.', 'SecurityError'); }});
"""


@requires_browser
class SettingsPanel(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-settings-")))
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp()))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def open_page(self, browser: str, path: str = PAGE, init: str = "", width: int = 1280):
        context = self.browsers[browser].new_context(viewport={"width": width, "height": 900})
        self.addCleanup(context.close)
        context.add_init_script(STUBS + init)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        self.addCleanup(lambda: self.assertEqual(errors, [], "page errors"))
        page.goto(self.base + path, wait_until="load")
        return page

    def button(self, page, action: str):
        return page.locator(f'{PANEL} [data-action="{action}"]')

    def open_panel(self, page) -> None:
        page.click(TOGGLE)
        self.expect(page.locator(PANEL)).to_be_visible()

    def stored(self, page, key: str):
        return page.evaluate(f"localStorage.getItem('{key}')")

    def html(self, page, attr: str):
        return page.evaluate(f"document.documentElement.getAttribute('{attr}')")

    # -- opening and closing -------------------------------------------------

    def test_open_and_close(self) -> None:
        for browser in BROWSERS:
            page = self.open_page(browser)
            toggle, panel = page.locator(TOGGLE), page.locator(PANEL)
            with self.subTest(browser=browser, how="the gear"):
                self.expect(panel).to_be_hidden()
                self.expect(toggle).to_have_attribute("aria-expanded", "false")
                toggle.click()
                self.expect(panel).to_be_visible()
                self.expect(toggle).to_have_attribute("aria-expanded", "true")
                self.expect(panel).to_have_attribute("aria-hidden", "false")
                self.expect(self.button(page, "theme-light")).to_be_focused()
                toggle.click()
                self.expect(panel).to_be_hidden()
                self.expect(toggle).to_be_focused()
            with self.subTest(browser=browser, how="Escape"):
                toggle.click()
                page.keyboard.press("Escape")
                self.expect(panel).to_be_hidden()
                self.expect(toggle).to_have_attribute("aria-expanded", "false")
                self.expect(panel).to_have_attribute("aria-hidden", "true")
                self.expect(toggle).to_be_focused()
            with self.subTest(browser=browser, how="a click elsewhere"):
                toggle.click()
                page.locator("#markdownBody p").first.click()
                self.expect(panel).to_be_hidden()
                # Focus is not pulled back to the gear from where the reader clicked.
                self.assertNotEqual(page.evaluate("document.activeElement.className"), "settings-toggle")

    def test_tab_stays_in_the_open_panel(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser)
                self.open_panel(page)
                count = page.locator(f"{PANEL} button").count()
                seen = set()
                for _ in range(count + 2):
                    page.keyboard.press("Tab")
                    seen.add(page.evaluate("document.activeElement.dataset.action || ''"))
                    self.assertTrue(page.evaluate(f"document.querySelector('{PANEL}')"
                                                  ".contains(document.activeElement)"))
                self.assertEqual(len(seen), count)
                page.keyboard.press("Shift+Tab")
                self.assertTrue(page.evaluate(f"document.querySelector('{PANEL}')"
                                              ".contains(document.activeElement)"))

    # -- what the buttons do ---------------------------------------------------

    def test_themes(self) -> None:
        background = "getComputedStyle(document.body).backgroundColor"
        # A replaced transition rejects `finished` (AbortError); settled all the same.
        settled = "Promise.all(document.body.getAnimations().map(a => a.finished.catch(() => null)))"
        for browser in BROWSERS:
            page = self.open_page(browser)
            colours = {}
            for theme in ("dark", "cappuccino", "light"):
                with self.subTest(browser=browser, theme=theme):
                    self.open_panel(page)
                    self.button(page, f"theme-{theme}").click()
                    self.assertEqual(self.html(page, "data-theme"), theme)
                    self.assertEqual(self.stored(page, "theme"), theme)
                    for other in ("light", "dark", "cappuccino"):
                        self.expect(self.button(page, f"theme-{other}")).to_have_attribute(
                            "aria-pressed", "true" if other == theme else "false")
                    page.evaluate(settled)          # the background fades between themes
                    colours[theme] = page.evaluate(background)
                    page.reload(wait_until="load")
                    self.assertEqual(self.html(page, "data-theme"), theme)
                    self.assertEqual(page.evaluate(background), colours[theme])
            with self.subTest(browser=browser, part="each theme its own colour"):
                self.assertEqual(len(set(colours.values())), 3, colours)

    SIZE = "getComputedStyle(document.documentElement).fontSize"

    def size_state(self, page) -> tuple:
        """The root's font size, what is stored, and what the panel shows
        and announces."""
        return (page.evaluate(self.SIZE), self.stored(page, "text-size"),
                page.inner_text("[data-text-size]"),
                page.locator("[data-text-size-status]").text_content())

    def test_text_size(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser)
                # The page starts at the middle size, as the panel takes it
                # to (base.css had 20px, the old scale's smallest: A- did
                # nothing at first, and A+ went to 26).
                self.assertEqual(self.size_state(page), ("23px", None, "100%", ""))
                self.open_panel(page)
                larger, smaller = self.button(page, "text-larger"), self.button(page, "text-smaller")
                reset = self.button(page, "text-reset")
                self.expect(reset).to_have_accessible_name("100% text size; reset")
                larger.focus()
                for px, pct in ((25, "109%"), (27, "117%"), (29, "126%")):
                    page.keyboard.press("Enter")
                    self.assertEqual(self.size_state(page),
                                     (f"{px}px", str(px), pct, f"Text size {pct}"))
                # At an end of the range the button says so, and keeps focus
                # (a disabled button dropped it, out of the panel).
                self.expect(larger).to_have_attribute("aria-disabled", "true")
                self.expect(larger).to_be_focused()
                page.keyboard.press("Enter")
                self.assertEqual(self.size_state(page)[0], "29px")
                reset.click()
                self.assertEqual(self.size_state(page)[:3], ("23px", None, "100%"))
                smaller.focus()
                for _ in range(3):
                    page.keyboard.press("Enter")
                self.assertEqual(self.size_state(page)[:3], ("17px", "17", "74%"))
                self.expect(smaller).to_have_attribute("aria-disabled", "true")
                self.expect(smaller).to_be_focused()
                self.expect(larger).to_have_attribute("aria-disabled", "false")
                page.reload(wait_until="load")
                self.assertEqual(self.size_state(page), ("17px", "17", "74%", ""))

    def test_text_size_stored_by_the_old_scale(self) -> None:
        # The panel had three sizes and stored their place, 0 to 2.
        for browser in BROWSERS:
            for stored, px, step, after in (("0", 20, "text-larger", 21), ("2", 26, "text-smaller", 25),
                                            ("1", 23, "text-larger", 25)):
                with self.subTest(browser=browser, stored=stored):
                    page = self.open_page(browser, init=f"if (!sessionStorage.s) {{"
                                          f" localStorage.setItem('text-size', '{stored}');"
                                          f" sessionStorage.s = 1; }}")
                    self.assertEqual(page.evaluate(self.SIZE), f"{px}px")
                    self.open_panel(page)
                    self.button(page, step).click()
                    self.assertEqual(page.evaluate(self.SIZE), f"{after}px")
                    self.assertEqual(self.stored(page, "text-size"), str(after))

    def test_focus_mode_and_reduce_motion(self) -> None:
        smooth = "getComputedStyle(document.documentElement).scrollBehavior"
        for browser in BROWSERS:
            for action, attr in (("focus-mode", "data-focus-mode"),
                                 ("reduce-motion", "data-reduce-motion")):
                with self.subTest(browser=browser, setting=action):
                    page = self.open_page(browser)
                    if action == "reduce-motion":
                        self.assertEqual(page.evaluate(smooth), "smooth")
                    self.open_panel(page)
                    btn = self.button(page, action)
                    self.expect(btn).to_have_attribute("aria-pressed", "false")
                    btn.click()
                    self.assertEqual(self.html(page, attr), "")
                    self.assertEqual(self.stored(page, action), "1")
                    self.expect(btn).to_have_attribute("aria-pressed", "true")
                    if action == "reduce-motion":
                        self.assertEqual(page.evaluate(smooth), "auto")
                    page.reload(wait_until="load")
                    self.assertEqual(self.html(page, attr), "")
                    self.open_panel(page)
                    self.expect(btn).to_have_attribute("aria-pressed", "true")
                    btn.click()
                    self.assertIsNone(self.html(page, attr))
                    self.assertIsNone(self.stored(page, action))
                    self.expect(btn).to_have_attribute("aria-pressed", "false")

    def test_print_and_clear_annotations(self) -> None:
        for browser in BROWSERS:
            page = self.open_page(browser)
            with self.subTest(browser=browser, action="print"):
                self.open_panel(page)
                self.button(page, "print").click()
                self.assertEqual(page.evaluate("window.__printed"), 1)
                self.expect(page.locator(PANEL)).to_be_hidden()
            # A highlight of the page's first words, made as Annotate does.
            words = page.evaluate("document.querySelector('#markdownBody p')"
                                  ".textContent.trim().split(/\\s+/).slice(0, 3).join(' ')")
            self.assertIsNotNone(page.evaluate("w => window.Annotations.add(w, 'amber', '')", words))
            self.expect(page.locator("mark.user-annotation")).not_to_have_count(0)
            with self.subTest(browser=browser, action="clear, declined"):
                page.evaluate("window.__confirm = false")
                self.open_panel(page)
                self.button(page, "clear-annotations").click()
                self.assertEqual(page.evaluate("window.__asked"), 1)
                self.expect(page.locator("mark.user-annotation")).not_to_have_count(0)
                self.assertNotEqual(self.stored(page, "site-annotations"), "[]")
                page.keyboard.press("Escape")
            with self.subTest(browser=browser, action="clear, confirmed"):
                page.evaluate("window.__confirm = true")
                self.open_panel(page)
                self.button(page, "clear-annotations").click()
                self.expect(page.locator("mark.user-annotation")).to_have_count(0)
                self.assertEqual(self.stored(page, "site-annotations"), "[]")
                self.expect(page.locator(PANEL)).to_be_hidden()
                self.expect(page.locator(TOGGLE)).to_be_focused()

    # -- what is stored, or cannot be ------------------------------------------

    def test_malformed_stored_values(self) -> None:
        for browser in BROWSERS:
            for size in ("9", "40", "16", "30", "x", "23px", "-1"):
                with self.subTest(browser=browser, theme="neon", text_size=size):
                    seed = (f"localStorage.setItem('theme', 'neon');"
                            f" localStorage.setItem('text-size', '{size}');")
                    page = self.open_page(browser, init=f"if (!sessionStorage.s) {{ {seed} sessionStorage.s = 1; }}")
                    self.assertIsNone(self.html(page, "data-theme"))
                    self.assertEqual(page.evaluate(
                        "document.documentElement.style.getPropertyValue('--text-size')"), "")
                    self.assertEqual(page.evaluate(self.SIZE), "23px")
                    self.open_panel(page)
                    self.expect(page.locator("[data-text-size]")).to_have_text("100%")
                    # The default size: neither end, so both steps open.
                    for action in ("text-smaller", "text-larger"):
                        self.expect(self.button(page, action)).to_have_attribute("aria-disabled", "false")

    def test_storage_that_throws(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser, init=NO_STORAGE)
                self.open_panel(page)
                self.button(page, "theme-dark").click()
                self.assertEqual(self.html(page, "data-theme"), "dark")
                self.button(page, "text-larger").click()
                self.button(page, "text-larger").click()
                self.assertEqual(page.evaluate(
                    "document.documentElement.style.getPropertyValue('--text-size')"), "27px")
                self.expect(page.locator("[data-text-size]")).to_have_text("117%")
                self.button(page, "reduce-motion").click()
                self.assertEqual(self.html(page, "data-reduce-motion"), "")

    # -- the score reader ------------------------------------------------------

    def test_escape_in_the_reader_closes_only_the_panel(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser, READER)
                self.expect(page.locator("#score-page > svg")).to_have_count(1)
                folio = page.inner_text("#score-folio")
                self.open_panel(page)
                page.keyboard.press("ArrowRight")
                page.keyboard.press("Escape")
                self.expect(page.locator(PANEL)).to_be_hidden()
                page.wait_for_timeout(500)
                self.assertEqual(page.url, self.base + READER)
                self.assertEqual(page.inner_text("#score-folio"), folio)


if __name__ == "__main__":
    unittest.main()
