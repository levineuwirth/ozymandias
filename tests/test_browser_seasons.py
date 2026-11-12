"""Seasons (site.yaml's seasons: theme.js, settings.js, stage.js), in
Chromium and Firefox, on built pages, on dates the test sets.

In a season's window a reader with no stored theme wears it, its base
theme with data-season beside it; Settings shows its button, pressed;
the stage fades in with the season's actors, and the enforcing CSP lets
it. A stored theme keeps the season off, and its script never loads. Out
of the window nothing changes and the button is hidden, but a reader who
chose the season gets its base theme. The season's button puts it on and
stores it, and a theme's button takes it off. Focus mode takes the stage
away and gives it back, Reduce Motion keeps it still, print hides it; at
phone width it starts too, and Settings, with the season's button, still
fits the screen. A sprite is shown at the size it is drawn, on whole
device pixels, and clears whole whatever its last paint left set; no
actor clears part of a canvas, flying or hanging.
The score reader wears the base theme only, with nothing drawn over the
score.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_seasons -v
"""

from __future__ import annotations

import datetime
import json
import tempfile
import unittest
from pathlib import Path

from tests import target
from tests._browser import (BROWSERS, asset_root, check_site, enforcing_csp, off_season, require_playwright,
                            requires_browser, site_server, site_themes)
from tests.target import route

PAGE = route("essay")
READER = route("score") + "score/"
PANEL = ".settings-panel"
LAYER = ".stage-layer"
SPRITE = ".stage-layer .stage-sprite"


def noon(day: datetime.date) -> datetime.datetime:
    return datetime.datetime(day.year, day.month, day.day, 12)


@requires_browser
class Seasons(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        seasons = target.config()["seasons"]
        if not seasons:
            raise unittest.SkipTest("the site declares no seasons")
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        cls.season = season = seasons[0]
        # A day in its window (its first), and a day out of every window.
        year = datetime.date.today().year
        month, day = (int(n) for n in season["from"].split("-"))
        cls.inside = noon(datetime.date(year if (month, day) != (2, 29) else 2028, month, day))
        cls.outside = off_season()
        cls.tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-seasons-")))
        cls.base = cls.enterClassContext(site_server(cls.tmp, enforcing_csp()))
        cls.playwright = playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def open_page(self, browser: str, when: datetime.datetime, path: str = PAGE,
                  theme: str | None = None, width: int = 1280, reduced: bool = False):
        context = self.browsers[browser].new_context(
            viewport={"width": width, "height": 900},
            reduced_motion="reduce" if reduced else "no-preference")
        self.addCleanup(context.close)
        context.clock.set_fixed_time(when)
        if theme:
            context.add_init_script(
                f"if (!sessionStorage.seeded) {{ localStorage.setItem('theme', {json.dumps(theme)});"
                f" sessionStorage.seeded = 1; }}")
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        self.addCleanup(lambda: self.assertEqual(errors, [], "page errors"))
        page.goto(self.base + path, wait_until="load")
        return page

    def html(self, page, attr: str):
        return page.evaluate(f"document.documentElement.getAttribute('{attr}')")

    def stored(self, page):
        return page.evaluate("localStorage.getItem('theme')")

    def button(self, page, action: str):
        return page.locator(f'{PANEL} [data-action="{action}"]')

    def season_button(self, page):
        return self.button(page, "season-" + self.season["id"])

    def open_panel(self, page) -> None:
        page.click(".settings-toggle")
        self.expect(page.locator(PANEL)).to_be_visible()

    def loaded(self, page, path: str) -> bool:
        return page.evaluate(
            "p => performance.getEntriesByType('resource').some(e => new URL(e.name).pathname === p)", path)

    def expect_stage(self, page) -> None:
        """The stage running, faded in, with the season's actors in it."""
        if not self.season["script"]:
            return
        self.expect(page.locator(LAYER)).to_have_class("stage-layer is-on", timeout=10_000)
        # The layer can fade in before the season's script has arrived and
        # mounted its actors.
        self.expect(page.locator(f"{LAYER} > *").first).to_be_attached(timeout=10_000)

    def assert_worn(self, page) -> None:
        self.assertEqual(self.html(page, "data-theme"), self.season["base"])
        self.assertEqual(self.html(page, "data-season"), self.season["id"])

    def tearDown(self) -> None:
        reports = self.tmp / "csp-reports.jsonl"
        if reports.exists():
            self.assertEqual(reports.read_text(), "", "CSP violations")

    # -- in the window ---------------------------------------------------------

    def test_worn_in_its_window_with_nothing_stored(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser, self.inside)
                self.assert_worn(page)
                self.assertIn(self.season["id"], self.html(page, "data-seasons-open").split())
                self.assertIsNone(self.stored(page), "wearing a season by default stores nothing")
                self.open_panel(page)
                self.expect(self.season_button(page)).to_be_visible()
                self.expect(self.season_button(page)).to_have_attribute("aria-pressed", "true")
                for theme in site_themes()[0]:
                    self.expect(self.button(page, "theme-" + theme)).to_have_attribute("aria-pressed", "false")
                self.expect_stage(page)

    def test_a_stored_theme_keeps_it_off(self) -> None:
        for browser in BROWSERS:
            for theme in site_themes()[0]:
                with self.subTest(browser=browser, theme=theme):
                    page = self.open_page(browser, self.inside, theme=theme)
                    self.assertEqual(self.html(page, "data-theme"), theme)
                    self.assertIsNone(self.html(page, "data-season"))
                    self.open_panel(page)
                    self.expect(self.season_button(page)).to_be_visible()
                    self.expect(self.season_button(page)).to_have_attribute("aria-pressed", "false")
                    page.wait_for_timeout(500)
                    self.assertFalse(self.loaded(page, asset_root() + "/js/stage.js"))
                    self.assertEqual(page.locator(LAYER).count(), 0)

    def test_its_buttons_put_it_on_and_take_it_off(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser, self.inside, theme="light")
                self.open_panel(page)
                self.season_button(page).click()
                self.assert_worn(page)
                self.assertEqual(self.stored(page), self.season["id"])
                self.expect(self.season_button(page)).to_have_attribute("aria-pressed", "true")
                self.expect_stage(page)
                self.button(page, "theme-dark").click()
                self.assertEqual(self.html(page, "data-theme"), "dark")
                self.assertIsNone(self.html(page, "data-season"))
                self.assertEqual(self.stored(page), "dark")
                self.expect(page.locator(LAYER)).to_have_count(0)
                self.expect(self.season_button(page)).to_have_attribute("aria-pressed", "false")
                page.reload(wait_until="load")
                self.assertIsNone(self.html(page, "data-season"))

    def test_chosen_it_stays_on_across_pages(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser, self.inside, theme=self.season["id"])
                self.assert_worn(page)
                self.expect_stage(page)

    # -- out of the window -----------------------------------------------------

    def test_out_of_its_window(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser, stored=None):
                page = self.open_page(browser, self.outside)
                self.assertIsNone(self.html(page, "data-theme"))
                self.assertIsNone(self.html(page, "data-season"))
                self.assertIsNone(self.html(page, "data-seasons-open"))
                self.open_panel(page)
                self.expect(self.season_button(page)).to_be_hidden()
                page.wait_for_timeout(500)
                self.assertFalse(self.loaded(page, asset_root() + "/js/stage.js"))
            with self.subTest(browser=browser, stored=self.season["id"]):
                page = self.open_page(browser, self.outside, theme=self.season["id"])
                self.assertEqual(self.html(page, "data-theme"), self.season["base"])
                self.assertIsNone(self.html(page, "data-season"))
                self.open_panel(page)
                self.expect(self.button(page, "theme-" + self.season["base"])).to_have_attribute(
                    "aria-pressed", "true")

    # -- the guards ------------------------------------------------------------

    def test_focus_mode_reduce_motion_and_print(self) -> None:
        if not self.season["script"]:
            self.skipTest("the season has no script: no stage")
        for browser in BROWSERS:
            with self.subTest(browser=browser, guard="focus mode"):
                page = self.open_page(browser, self.inside)
                self.expect_stage(page)
                self.open_panel(page)
                self.button(page, "focus-mode").click()
                self.expect(page.locator(LAYER)).to_have_count(0)
                self.button(page, "focus-mode").click()
                self.expect_stage(page)
            with self.subTest(browser=browser, guard="print"):
                page.emulate_media(media="print")
                self.assertEqual(page.locator(LAYER).evaluate("e => getComputedStyle(e).display"), "none")
            with self.subTest(browser=browser, guard="reduce motion"):
                page = self.open_page(browser, self.inside, reduced=True)
                self.expect_stage(page)
                self.assertTrue(page.evaluate("window.stage.still"))
                page.evaluate("document.documentElement.setAttribute('data-reduce-motion', '')")
                self.expect_stage(page)
                self.assertTrue(page.evaluate("window.stage.still"))

    def test_phone_width(self) -> None:
        for browser in BROWSERS:
            for width in (320, 390):
                with self.subTest(browser=browser, width=width):
                    page = self.open_page(browser, self.inside, width=width)
                    self.assert_worn(page)
                    self.expect_stage(page)
                    # The season's button widens the theme row; the panel
                    # still fits.
                    self.open_panel(page)
                    self.expect(self.season_button(page)).to_be_visible()
                    box = page.locator(PANEL).bounding_box()
                    self.assertGreaterEqual(box["x"], 0)
                    self.assertLessEqual(box["x"] + box["width"], width)

    def test_sprites_drawn_at_their_own_size(self) -> None:
        """Each sprite (stage.sprite) is shown at the size it is drawn, its
        corner on a whole device pixel, so nothing is resampled; at a
        fractional device pixel ratio too, and beside a classic scrollbar,
        which once squeezed a canvas as wide as the stage. Chromium only;
        headless Firefox keeps overlay scrollbars."""
        if not self.season["script"]:
            self.skipTest("the season has no script: no stage")
        browser = self.playwright.chromium.launch(ignore_default_args=["--hide-scrollbars"])
        self.addCleanup(browser.close)
        for dpr in (1, 1.25):
            with self.subTest(device_scale_factor=dpr):
                context = browser.new_context(viewport={"width": 1440, "height": 900},
                                              device_scale_factor=dpr)
                self.addCleanup(context.close)
                context.clock.set_fixed_time(self.inside)
                page = context.new_page()
                page.goto(self.base + PAGE, wait_until="load")
                self.expect_stage(page)
                self.expect(page.locator(SPRITE).first).to_be_attached(timeout=10_000)
                self.assertGreater(page.evaluate("innerWidth - document.documentElement.clientWidth"), 0,
                                   "no classic scrollbar: this proves nothing")
                # In device pixels. Layout keeps lengths to 1/64 CSS px, so a
                # sprite 104 px wide at 1.25 is laid out 0.016 px short.
                sprites = page.evaluate(f"""() => [...document.querySelectorAll('{SPRITE}')].map(c => {{
                    const r = c.getBoundingClientRect(), k = devicePixelRatio;
                    return [c.width, r.width * k, c.height, r.height * k, r.left * k, r.top * k];
                }})""")
                for drawn_w, shown_w, drawn_h, shown_h, x, y in sprites:
                    self.assertAlmostEqual(drawn_w, shown_w, delta=0.05)
                    self.assertAlmostEqual(drawn_h, shown_h, delta=0.05)
                    self.assertAlmostEqual(x, round(x), delta=0.05, msg="left edge between device pixels")
                    self.assertAlmostEqual(y, round(y), delta=0.05, msg="top edge between device pixels")

    def test_a_sprite_clears_whole_whatever_its_paint_left_set(self) -> None:
        """A sprite's draw clears the whole canvas whatever the last paint
        left set on the context. A clip it left in place would keep the next
        clear inside the clip, though the clear names the whole canvas, so
        this reads the pixels back rather than the clears' rectangles."""
        if not self.season["script"]:
            self.skipTest("the season has no script: no stage")
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser, self.inside)
                self.expect_stage(page)
                painted, left = page.evaluate(f"""() => {{
                    const s = window.stage.sprite(document.querySelector('{LAYER}'));
                    const count = () => {{
                        const d = s.canvas.getContext('2d').getImageData(0, 0, s.canvas.width, s.canvas.height).data;
                        let n = 0;
                        for (let i = 3; i < d.length; i += 4) if (d[i]) n++;
                        return n;
                    }};
                    s.draw(10, 10, 40, 40, c => {{
                        c.fillStyle = '#fff';
                        c.fillRect(10, 10, 40, 40);
                        c.beginPath(); c.rect(20, 20, 5, 5); c.clip();   // and never restored
                        c.translate(7, 3); c.globalAlpha = 0.5;
                    }});
                    const painted = count();
                    s.draw(10, 10, 40, 40, () => {{}});
                    const left = count();
                    s.remove();
                    return [painted, left];
                }}""")
                self.assertGreater(painted, 0, "the first paint drew nothing: this proves nothing")
                self.assertEqual(left, 0, "pixels from the first paint survived an empty draw")

    def test_no_actor_clears_part_of_a_canvas(self) -> None:
        """Every clearRect an actor makes covers its whole canvas, as the
        bats hang and as they fly (the pointer passing under their roosts
        sends them off): clearing part of a canvas left specks of foreign
        color in Chromium on ANGLE's OpenGL backend (stage.js, Sprites)."""
        if not self.season["script"]:
            self.skipTest("the season has no script: no stage")
        record = """(() => {
            const P = CanvasRenderingContext2D.prototype, clear = P.clearRect;
            window.__clears = [];
            P.clearRect = function (x, y, w, h) {
                const m = this.getTransform(), xs = [], ys = [];
                for (const [px, py] of [[x, y], [x + w, y], [x, y + h], [x + w, y + h]]) {
                    xs.push(m.a * px + m.c * py + m.e); ys.push(m.b * px + m.d * py + m.f);
                }
                window.__clears.push([Math.min(...xs), Math.min(...ys), Math.max(...xs), Math.max(...ys),
                                      this.canvas.width, this.canvas.height, this.canvas.className]);
                return clear.apply(this, arguments);
            };
        })()"""
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                context = self.browsers[browser].new_context(viewport={"width": 1440, "height": 900})
                self.addCleanup(context.close)
                context.clock.set_fixed_time(self.inside)
                context.add_init_script(record)
                page = context.new_page()
                page.goto(self.base + PAGE, wait_until="load")
                self.expect_stage(page)
                self.expect(page.locator(SPRITE).first).to_be_attached(timeout=10_000)
                page.wait_for_function(
                    f"[...document.querySelectorAll('{SPRITE}')].every(c => c.style.transform)", timeout=10_000)
                spots = page.evaluate(f"[...document.querySelectorAll('{SPRITE}')].map(c => c.style.transform)")
                foot = page.evaluate("document.querySelector('body > header').getBoundingClientRect().bottom")
                page.mouse.move(2, foot + 12)
                page.mouse.move(1438, foot + 12, steps=60)
                page.wait_for_timeout(800)
                moved = page.evaluate(f"[...document.querySelectorAll('{SPRITE}')].map(c => c.style.transform)")
                self.assertNotEqual(spots, moved, "no bat took off: this proves nothing about flight")
                clears = page.evaluate("window.__clears")
                self.assertTrue(clears, "no actor cleared anything: this proves nothing")
                part = [c for c in clears
                        if c[0] > 0.01 or c[1] > 0.01 or c[2] < c[4] - 0.01 or c[3] < c[5] - 0.01]
                self.assertEqual(part[:5], [], f"{len(part)} of {len(clears)} clears cover part of a canvas")

    def test_the_score_reader_wears_only_the_base_theme(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser, self.inside, path=READER)
                self.assert_worn(page)
                page.wait_for_timeout(500)
                self.assertFalse(self.loaded(page, asset_root() + "/js/stage.js"))
                self.assertFalse(self.loaded(page, self.season["stylesheet"]))


if __name__ == "__main__":
    unittest.main()
