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
scrolling; Top of page, from down the page, smoothly or at once; the
gear's wrap marked while the reader reads on down and while a pointer is
near it, for the site that pins the gear and fades it, and not marked by
a page that opens part-way down; Print, and Clear Annotations, confirmed
or not; malformed
stored values ignored; storage that throws, as in some private windows,
breaking nothing; and, on the score reader, Escape closing the panel
without leaving the reader.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_settings -v
"""

from __future__ import annotations

import math
import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, check_site, enforcing_csp, off_season, require_playwright,
                            requires_browser, site_server, site_themes)
from tests.target import any_page, requires_switch, route

# Where the controls every page has are tried: the essay, where the site
# has one, or the home page. A test that needs a long page with headings,
# or the score reader, asks for that page itself.
PAGE = any_page()
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

    def open_page(self, browser: str, path: str = PAGE, init: str = "", width: int = 1280,
                  context_setup=None):
        context = self.browsers[browser].new_context(viewport={"width": width, "height": 900})
        self.addCleanup(context.close)
        # Out of every season (test_browser_seasons), so that nothing stored
        # means no theme, on any date.
        context.clock.set_fixed_time(off_season())
        context.add_init_script(STUBS + init)
        if context_setup:
            context_setup(context)
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
                self.expect(self.button(page, "top")).to_be_focused()
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

    def test_the_panel_fits_a_phone(self) -> None:
        # Where the header's controls are a centered row, the panel hung
        # from the gear, mid-row, and ran off the screen's left.
        for browser in BROWSERS:
            for width in (320, 390):
                for focus in (False, True):
                    with self.subTest(browser=browser, width=width, focus_mode=focus):
                        init = "try { localStorage.setItem('focus-mode', '1'); } catch (e) {}" if focus else ""
                        page = self.open_page(browser, init=init, width=width)
                        self.open_panel(page)
                        box = page.locator(PANEL).bounding_box()
                        self.assertGreaterEqual(box["x"], 0)
                        self.assertLessEqual(box["x"] + box["width"], width)

    def test_tab_stays_in_the_open_panel(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser)
                self.open_panel(page)
                # The buttons shown: a season's is hidden out of its window.
                count = page.locator(f"{PANEL} button:visible").count()
                seen = set()
                for _ in range(count + 2):
                    page.keyboard.press("Tab")
                    # A reader setting's buttons share an action.
                    seen.add(page.evaluate("(d => [d.action, d.setting, d.option].join(' '))"
                                           "(document.activeElement.dataset)"))
                    self.assertTrue(page.evaluate(f"document.querySelector('{PANEL}')"
                                                  ".contains(document.activeElement)"))
                self.assertEqual(len(seen), count)
                page.keyboard.press("Shift+Tab")
                self.assertTrue(page.evaluate(f"document.querySelector('{PANEL}')"
                                              ".contains(document.activeElement)"))

    # -- what the buttons do ---------------------------------------------------

    def themes_offered(self, page) -> bool:
        """Whether the open panel offers a choice of theme: a site with one
        look may take the row away (its stylesheets, not the engine's)."""
        return page.locator(f'{PANEL} [data-action^="theme-"]').first.is_visible()

    def test_themes(self) -> None:
        background = "getComputedStyle(document.body).backgroundColor"
        # A replaced transition rejects `finished` (AbortError); settled all the same.
        settled = "Promise.all(document.body.getAnimations().map(a => a.finished.catch(() => null)))"
        themes, light = site_themes()
        for browser in BROWSERS:
            page = self.open_page(browser)
            self.open_panel(page)
            if not self.themes_offered(page):
                self.skipTest("the site's settings panel offers no choice of theme")
            page.keyboard.press("Escape")
            colours = {}
            # Each a change from the last: the one worn with none chosen last.
            for theme in [t for t in themes if t != light] + [light]:
                with self.subTest(browser=browser, theme=theme):
                    self.open_panel(page)
                    self.button(page, f"theme-{theme}").click()
                    self.assertEqual(self.html(page, "data-theme"), theme)
                    self.assertEqual(self.stored(page, "theme"), theme)
                    for other in themes:
                        self.expect(self.button(page, f"theme-{other}")).to_have_attribute(
                            "aria-pressed", "true" if other == theme else "false")
                    page.evaluate(settled)          # the background fades between themes
                    colours[theme] = page.evaluate(background)
                    page.reload(wait_until="load")
                    self.assertEqual(self.html(page, "data-theme"), theme)
                    self.assertEqual(page.evaluate(background), colours[theme])
            with self.subTest(browser=browser, part="each theme its own colour"):
                self.assertEqual(len(set(colours.values())), len(themes), colours)

    SIZE = "getComputedStyle(document.documentElement).fontSize"
    # The panel's sizes (utils.js TEXT_SIZE): 17 to 29px in steps of 2,
    # around the skin's default (--text-size-default), which is 100%.
    SIZES = range(17, 30, 2)

    def default_size(self, page) -> float:
        return page.evaluate("parseFloat(getComputedStyle(document.documentElement)"
                             ".getPropertyValue('--text-size-default'))")

    @staticmethod
    def px(size: float) -> str:
        return f"{size:g}px"

    @staticmethod
    def pct(size: float, base: float) -> str:
        return f"{math.floor(size / base * 100 + 0.5)}%"     # settings.js's Math.round

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
                # The page starts at the skin's default, as the panel takes
                # it to (base.css had 20px, the old scale's smallest: A- did
                # nothing at first, and A+ went to 26). The engine's is 23px,
                # a site's theme.css may set another.
                base = self.default_size(page)
                up = [s for s in self.SIZES if s > base]
                down = [s for s in reversed(self.SIZES) if s < base]
                self.assertTrue(up and down, f"a default of {base}px leaves the panel no step one way")
                self.assertEqual(self.size_state(page), (self.px(base), None, "100%", ""))
                self.open_panel(page)
                larger, smaller = self.button(page, "text-larger"), self.button(page, "text-smaller")
                reset = self.button(page, "text-reset")
                self.expect(reset).to_have_accessible_name("100% text size; reset")
                larger.focus()
                for px in up:
                    page.keyboard.press("Enter")
                    pct = self.pct(px, base)
                    self.assertEqual(self.size_state(page),
                                     (self.px(px), str(px), pct, f"Text size {pct}"))
                # At an end of the range the button says so, and keeps focus
                # (a disabled button dropped it, out of the panel).
                self.expect(larger).to_have_attribute("aria-disabled", "true")
                self.expect(larger).to_be_focused()
                page.keyboard.press("Enter")
                self.assertEqual(self.size_state(page)[0], self.px(up[-1]))
                reset.click()
                self.assertEqual(self.size_state(page)[:3], (self.px(base), None, "100%"))
                smaller.focus()
                for _ in down:
                    page.keyboard.press("Enter")
                least = (self.px(down[-1]), str(down[-1]), self.pct(down[-1], base))
                self.assertEqual(self.size_state(page)[:3], least)
                self.expect(smaller).to_have_attribute("aria-disabled", "true")
                self.expect(smaller).to_be_focused()
                self.expect(larger).to_have_attribute("aria-disabled", "false")
                page.reload(wait_until="load")
                self.assertEqual(self.size_state(page), (*least, ""))

    def test_a_skin_default_with_fresh_and_saved_settings(self) -> None:
        def skin(context):
            def stylesheet(route):
                response = route.fetch()
                route.fulfill(response=response, body=response.text() +
                              "\n@layer site { :root { --text-size-default: 21px; } }")
            context.route("**/css/core.css", stylesheet)

        for browser in BROWSERS:
            for stored in (False, True):
                with self.subTest(browser=browser, stored=stored):
                    init = ("if (!sessionStorage.s) { localStorage.setItem('text-size', '27');"
                            " sessionStorage.s = 1; }") if stored else ""
                    page = self.open_page(browser, init=init, context_setup=skin)
                    self.assertEqual(self.size_state(page)[:3],
                                     ("27px", "27", "129%") if stored else ("21px", None, "100%"))
                    self.open_panel(page)
                    self.button(page, "text-larger").click()
                    self.assertEqual(self.size_state(page)[0], "29px" if stored else "23px")
                    self.button(page, "text-reset").click()
                    self.assertEqual(self.size_state(page)[:3], ("21px", None, "100%"))
                    page.reload(wait_until="load")
                    self.assertEqual(self.size_state(page)[:3], ("21px", None, "100%"))

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
                    # The default is not stored: a step onto it forgets the choice.
                    self.assertEqual(self.stored(page, "text-size"),
                                     None if after == self.default_size(page) else str(after))

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

    # scrollTo at once (the page scrolls smoothly), resolved by the
    # scroll's own event, after settings.js has had it: a frame or two
    # later is not always late enough.
    SCROLL = """y => new Promise(done => {
        const end = document.documentElement.scrollHeight - window.innerHeight;
        if (Math.abs(Math.min(y, end) - window.scrollY) < 1) return done(window.scrollY);
        window.addEventListener('scroll', () => done(window.scrollY), {once: true});
        window.scrollTo({top: y, behavior: 'instant'});
    })"""
    # settings.js heeds scrolling from 300 ms after load.
    ARMED = 450

    def test_top_of_page(self) -> None:
        reduce = "try { localStorage.setItem('reduce-motion', '1'); } catch (e) {}"
        for browser in BROWSERS:
            for init in ("", reduce):
                with self.subTest(browser=browser, reduce_motion=bool(init)):
                    page = self.open_page(browser, route("essay"), init=init)   # long enough to scroll
                    self.assertGreater(page.evaluate(self.SCROLL, 2000), 1000)
                    self.open_panel(page)
                    self.button(page, "top").click()
                    self.expect(page.locator(PANEL)).to_be_hidden()
                    if init:
                        self.assertEqual(page.evaluate("window.scrollY"), 0)
                    else:
                        # wait_for_function's string form is an eval the CSP refuses.
                        for _ in range(100):
                            if page.evaluate("window.scrollY") == 0:
                                break
                            page.wait_for_timeout(50)
                        self.assertEqual(page.evaluate("window.scrollY"), 0)

    def test_the_gear_s_wrap_says_where_the_reader_is(self) -> None:
        wrap = ".settings-wrap"
        essay = route("essay")      # long enough to scroll well past the first screen

        def away(page) -> bool:
            return page.evaluate(f"document.querySelector('{wrap}').hasAttribute('data-away')")

        for browser in BROWSERS:
            with self.subTest(browser=browser, what="scrolling"):
                page = self.open_page(browser, essay)
                page.wait_for_timeout(self.ARMED)
                self.assertFalse(away(page))
                page.evaluate(self.SCROLL, 3000)
                self.assertTrue(away(page))
                # Back up a little is the page settling under a thumb; 40px is a reader.
                page.evaluate(self.SCROLL, 2980)
                self.assertTrue(away(page))
                page.evaluate(self.SCROLL, 2950)
                self.assertFalse(away(page))
                page.evaluate(self.SCROLL, 3200)
                self.assertTrue(away(page))
                page.evaluate(self.SCROLL, 100)
                self.assertFalse(away(page))
                # Nothing of it on the root, whose every change restyles the page.
                self.assertEqual(page.evaluate(
                    "[...document.documentElement.attributes].map(a => a.name)"
                    ".filter(n => /away|near/.test(n))"), [])
            with self.subTest(browser=browser, what="a pointer"):
                page = self.open_page(browser, essay)
                box = page.locator(wrap).bounding_box()
                x, y = box["x"] + box["width"] / 2, box["y"] + box["height"] / 2
                page.mouse.move(x, y)
                self.expect(page.locator(f"{wrap}[data-near]")).to_have_count(1)
                # The far corner of the window from wherever the site put it.
                page.mouse.move(10 if x > 640 else 1270, 10 if y > 450 else 890)
                self.expect(page.locator(f"{wrap}[data-near]")).to_have_count(0)
            with self.subTest(browser=browser, what="a page opened part-way down"):
                page = self.open_page(browser, essay)
                # A heading with page below it to scroll into.
                middle = page.evaluate("(h => h[h.length >> 1].id)"
                                       "([...document.querySelectorAll('#markdownBody :is(h2, h3)[id]')])")
                page = self.open_page(browser, f"{essay}#{middle}")
                page.wait_for_timeout(self.ARMED)
                y = page.evaluate("window.scrollY")
                self.assertGreater(y, 900)
                self.assertFalse(away(page))
                page.evaluate(self.SCROLL, y + 200)
                self.assertTrue(away(page))

    def test_print(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser)
                self.open_panel(page)
                self.button(page, "print").click()
                self.assertEqual(page.evaluate("window.__printed"), 1)
                self.expect(page.locator(PANEL)).to_be_hidden()

    @requires_switch("annotations")
    def test_clear_annotations(self) -> None:
        for browser in BROWSERS:
            page = self.open_page(browser)
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

    def test_a_site_s_reader_settings(self) -> None:
        """site.yaml's reader-settings: each a row of buttons, the first
        the default (worn as no attribute, stored as nothing), another
        worn as data-reader-ID and kept across a load; a stored option the
        site no longer offers is the default."""
        for browser in BROWSERS:
            page = self.open_page(browser)
            declared = page.evaluate("""[...document.querySelectorAll('meta[name="ozy:setting"]')].map(
                m => [m.content, m.dataset.options.split(' ')])""")
            # A site with none has no such row.
            self.assertEqual(page.locator('[data-action="reader"]').count(),
                             sum(len(options) for _, options in declared))
            for setting, options in declared:
                attr, key = f"data-reader-{setting}", f"reader-{setting}"
                choice = lambda option: page.locator(
                    f'{PANEL} [data-action="reader"][data-setting="{setting}"][data-option="{option}"]')
                pressed = lambda: [o for o in options if choice(o).get_attribute("aria-pressed") == "true"]
                with self.subTest(browser=browser, setting=setting):
                    self.assertGreaterEqual(len(options), 2)
                    self.assertIsNone(self.html(page, attr))
                    self.open_panel(page)
                    self.assertEqual(pressed(), options[:1])
                    for option in options[1:]:
                        choice(option).click()
                        self.assertEqual(self.html(page, attr), option)
                        self.assertEqual(self.stored(page, key), option)
                        self.assertEqual(pressed(), [option])
                    page.reload(wait_until="load")
                    self.assertEqual(self.html(page, attr), options[-1])
                    self.open_panel(page)
                    self.assertEqual(pressed(), options[-1:])
                    choice(options[0]).click()
                    self.assertIsNone(self.html(page, attr))
                    self.assertIsNone(self.stored(page, key))
                    self.assertEqual(pressed(), options[:1])
                    page.evaluate(f"localStorage.setItem('{key}', 'no-such-option')")
                    page.reload(wait_until="load")
                    self.assertIsNone(self.html(page, attr))
                    self.open_panel(page)
                    self.assertEqual(pressed(), options[:1])
                    page.keyboard.press("Escape")

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
                    self.assertEqual(page.evaluate(self.SIZE), self.px(self.default_size(page)))
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
                if self.themes_offered(page):
                    self.button(page, "theme-dark").click()
                    self.assertEqual(self.html(page, "data-theme"), "dark")
                base = self.default_size(page)
                second = [s for s in self.SIZES if s > base][1]
                self.button(page, "text-larger").click()
                self.button(page, "text-larger").click()
                self.assertEqual(page.evaluate(
                    "document.documentElement.style.getPropertyValue('--text-size')"), self.px(second))
                self.expect(page.locator("[data-text-size]")).to_have_text(self.pct(second, base))
                self.button(page, "reduce-motion").click()
                self.assertEqual(self.html(page, "data-reduce-motion"), "")

    # -- the score reader ------------------------------------------------------

    def test_escape_in_the_reader_closes_only_the_panel(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                reader = route("score") + "score/"
                page = self.open_page(browser, reader)
                self.expect(page.locator("#score-page > svg")).to_have_count(1)
                folio = page.inner_text("#score-folio")
                self.open_panel(page)
                page.keyboard.press("ArrowRight")
                page.keyboard.press("Escape")
                self.expect(page.locator(PANEL)).to_be_hidden()
                page.wait_for_timeout(500)
                self.assertEqual(page.url, self.base + reader)
                self.assertEqual(page.inner_text("#score-folio"), folio)


if __name__ == "__main__":
    unittest.main()
