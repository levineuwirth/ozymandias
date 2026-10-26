"""The score reader and its following, in Chromium and Firefox.

score-reader.js shows a composition's score a page at a time; where a
realization exists, score-follow.js plays it and turns the pages, marking
the bar. These run on the built readers of two works with recordings,
whose timing.json gives every bar's page, box and first sounding, so the
expected start of any page or movement is computed here, not written in.

Checked: the first Play starts at the page on screen, or at a movement
chosen before it, and the reader stays there (audit M03); the seek slider
keeps its arrows and Page keys, and a focused button its Space (M04);
paging keys read down a page before turning, and a forward turn opens at
the top (M04, M07); one animation-frame loop however many seeks and turns
(M06); only the pages near the current one are kept, and a slider drag
seeks once, where it ends (M08); the toolbar recedes unless a mouse rests
on it or keyboard focus is in it (what a tap or click leaves behind kept
it up); on a touch screen the tap that wakes the sleeping toolbar does not
also move the music (M05); following moves the view without animation
when the site's own Reduce Motion is on (it read only the system's);
Escape goes back to the work's page, by history only when the reader came
from it (history.back() left the site, or did nothing in a new tab); a
page's SVG is inlined without scripts, handlers or javascript: links.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_score -v
"""

from __future__ import annotations

import json
import re
import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, SITE, FakeNetwork, check_site, enforcing_csp,
                            require_playwright, requires_browser, site_server)
from tests._browser import fake_answer as answer

SYMPHONY = "symphony-no-5"
CONCERTO = "bassoon-concerto"


def timing(work: str) -> dict:
    path = SITE / "music" / work / "scores" / "timing.json"
    if not path.is_file():
        raise AssertionError(f"{path.relative_to(SITE)} is missing: the score pages come from "
                             f"tools/music-import.py, not git")
    t = json.loads(path.read_text(encoding="utf-8"))
    first = {}
    for ms, bar in t["events"]:
        first.setdefault(bar, ms)
    t["first"] = first
    return t


def page_start(t: dict, page: int) -> float:
    """score-follow.js's pageStart: the first sounding of the page's
    earliest bar, in seconds."""
    return min(t["first"][b] for b, m in enumerate(t["measures"]) if m and m[0] == page
               and b in t["first"]) / 1000


@requires_browser
class ScoreReader(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        cls.symphony, cls.concerto = timing(SYMPHONY), timing(CONCERTO)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-score-")))
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp()))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            options = {"args": ["--autoplay-policy=no-user-gesture-required"]} if name == "chromium" \
                else {"firefox_user_prefs": {"media.autoplay.default": 0}}
            cls.browsers[name] = getattr(playwright, name).launch(**options)
            cls.addClassCleanup(cls.browsers[name].close)

    def reader(self, browser: str, work: str, query: str = "", *, touch: bool = False,
               init: str = "", routes: dict | None = None, answers: dict | None = None):
        context = self.browsers[browser].new_context(viewport={"width": 1280, "height": 800},
                                                     has_touch=touch)
        self.addCleanup(context.close)
        if init:
            context.add_init_script(init)
        net = FakeNetwork(context, self.base, routes or {}, answers or {},
                          local=r"music/.+\.svg$" if routes else None)
        requests = []
        context.on("request", lambda r: requests.append(r.url))
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))

        def clean():
            self.assertEqual(errors, [], "page errors")
            self.assertEqual(net.unexpected, [], "requests leaving serve.py")
        self.addCleanup(clean)
        page.goto(f"{self.base}/music/{work}/score/{query}", wait_until="load")
        self.rendered(page)
        if work != "liebe-ohne-zeit":
            self.expect(page.locator(".score-follow-play")).to_be_visible()
        return page, requests

    def rendered(self, page) -> None:
        self.expect(page.locator("#score-page > svg")).to_have_count(1)   # the gutter holds a copy
        self.expect(page.locator("#score-page.is-loading")).to_have_count(0)

    def folio(self, page) -> int:
        return int(page.inner_text("#score-folio").split("/")[0])

    def position(self, page) -> float:
        return float(page.input_value(".score-follow-seek"))

    def playing(self, page, on: bool = True) -> None:
        self.expect(page.locator(".score-follow-play")).to_have_attribute(
            "aria-pressed", "true" if on else "false", timeout=15000)

    def unfocus(self, page) -> None:
        page.evaluate("document.activeElement && document.activeElement.blur()")

    # -- M03 ----------------------------------------------------------------

    def test_first_play_starts_where_the_reader_is(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser, arrived="?p=73"):
                page, _ = self.reader(browser, SYMPHONY, "?p=73")
                self.assertEqual(self.folio(page), 73)
                page.click(".score-follow-play")
                self.playing(page)
                page.wait_for_timeout(1500)
                self.assertAlmostEqual(self.position(page), page_start(self.symphony, 73), delta=4)
                self.assertEqual(self.folio(page), 73)
                page.click(".score-follow-play")
                self.playing(page, False)
            with self.subTest(browser=browser, arrived="movement III, then Play"):
                page, _ = self.reader(browser, SYMPHONY)
                mvt = page.locator(".score-reader-mvt").nth(2)
                mvt.click()
                self.assertEqual(self.folio(page), int(mvt.get_attribute("data-page")))
                page.click(".score-follow-play")
                self.playing(page)
                page.wait_for_timeout(1500)
                start = self.symphony["first"][self.symphony["movements"][2]] / 1000
                self.assertAlmostEqual(self.position(page), start, delta=4)
                page.click(".score-follow-play")

    # -- M04, M07 -----------------------------------------------------------

    def test_controls_keep_their_keys(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser, control="seek slider"):
                page, _ = self.reader(browser, CONCERTO)
                seek = page.locator(".score-follow-seek")
                seek.focus()
                page.keyboard.press("ArrowRight")
                self.assertAlmostEqual(self.position(page), 5, delta=0.2)
                page.keyboard.press("PageUp")
                self.assertAlmostEqual(self.position(page), 65, delta=0.2)
                page.keyboard.press("ArrowLeft")
                self.assertAlmostEqual(self.position(page), 60, delta=0.2)
                # The page follows the music, not the key.
                bar = self.concerto["events"][max(i for i, e in enumerate(self.concerto["events"])
                                                  if e[0] <= 60000)][1]
                self.expect(page.locator("#score-folio")).to_have_text(
                    re.compile(rf"^{self.concerto['measures'][bar][0]} / "))
            with self.subTest(browser=browser, control="Play button"):
                folio = self.folio(page)
                page.focus(".score-follow-play")
                page.keyboard.press("Space")
                self.playing(page)
                page.keyboard.press("Space")
                self.playing(page, False)
                self.assertEqual(self.folio(page), folio)

    def test_paging_keys_read_down_then_turn(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, _ = self.reader(browser, CONCERTO)
                viewport = page.locator("#score-reader-viewport")
                tall = viewport.evaluate("v => v.scrollHeight > v.clientHeight + 10")
                self.assertTrue(tall, "a page fits the window: nothing to read down")
                self.unfocus(page)
                page.keyboard.press("PageDown")
                self.expect(viewport).not_to_have_js_property("scrollTop", 0)
                self.assertEqual(self.folio(page), 1)
                for _ in range(20):
                    if self.folio(page) == 2:
                        break
                    page.keyboard.press("PageDown")
                    page.wait_for_timeout(150)
                self.assertEqual(self.folio(page), 2)
                self.rendered(page)
                # M07: a forward turn opens at the top.
                self.assertEqual(viewport.evaluate("v => v.scrollTop"), 0)
                # Back from the top: the previous page, at its foot.
                page.keyboard.press("PageUp")
                self.expect(page.locator("#score-folio")).to_have_text(re.compile(r"^1 / "))
                self.rendered(page)
                page.wait_for_timeout(200)
                self.assertTrue(viewport.evaluate(
                    "v => v.scrollTop >= v.scrollHeight - v.clientHeight - 2"))
                # A turn from half way down still opens the next page at its top.
                viewport.evaluate("v => v.scrollTop = v.scrollHeight / 3")
                page.keyboard.press("ArrowRight")
                self.expect(page.locator("#score-folio")).to_have_text(re.compile(r"^2 / "))
                self.rendered(page)
                self.assertEqual(viewport.evaluate("v => v.scrollTop"), 0)

    # -- M06 ----------------------------------------------------------------

    def test_one_frame_loop(self) -> None:
        # Every callback score-follow.js's loop() hands requestAnimationFrame,
        # against the frames that pass.
        count = """window.__loops = 0;
            const raf = window.requestAnimationFrame.bind(window);
            window.requestAnimationFrame = function (cb) {
                return raf(function (t) { if (cb.name === 'loop') window.__loops++; cb(t); });
            };"""
        measure = """() => new Promise(done => {
            const loops = window.__loops; let frames = 0;
            const start = performance.now();
            (function frame(t) {
                frames++;
                if (performance.now() - start < 1500) requestAnimationFrame(frame);
                else done([window.__loops - loops, frames]);
            })();
        })"""
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, _ = self.reader(browser, CONCERTO, init=count)
                page.click(".score-follow-play")
                self.playing(page)
                # Seeks and turns, each of which used to start another loop.
                page.focus(".score-follow-seek")
                for key in ("PageUp", "PageUp", "ArrowRight", "PageUp", "ArrowLeft", "PageDown"):
                    page.keyboard.press(key)
                    page.wait_for_timeout(250)
                self.unfocus(page)
                page.keyboard.press("ArrowRight")
                page.wait_for_timeout(300)
                loops, frames = page.evaluate(measure)
                page.keyboard.press("k")
                self.assertGreater(frames, 20)
                self.assertLessEqual(loops, frames + 2, "more than one loop a frame")
                # And one: counting by the callback's name, a renamed or
                # stopped loop would pass the line above with none.
                self.assertGreaterEqual(loops, frames - 2, "no loop while the music plays")

    # -- M08 ----------------------------------------------------------------

    def test_only_nearby_pages_are_kept(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser, how="turning"):
                page, requests = self.reader(browser, CONCERTO)
                pages = page.evaluate("document.getElementById('score-reader-stage').dataset.pages.split(',')")
                self.unfocus(page)
                for n in range(2, 10):
                    page.keyboard.press("ArrowRight")
                    self.expect(page.locator("#score-folio")).to_have_text(re.compile(rf"^{n} / "))
                    self.rendered(page)
                fetched = lambda url: sum(1 for u in requests if u.endswith(url))
                before = {n: fetched(pages[n - 1]) for n in (1, 8)}
                page.keyboard.press("ArrowLeft")      # page 8: kept
                self.rendered(page)
                self.assertEqual(fetched(pages[7]), before[8], "a neighbouring page was fetched again")
                page.keyboard.press("Home")           # page 1: let go long ago
                self.expect(page.locator("#score-folio")).to_have_text(re.compile(r"^1 / "))
                self.rendered(page)
                self.assertEqual(fetched(pages[0]), before[1] + 1, "page 1 was still kept")
            with self.subTest(browser=browser, how="dragging the slider"):
                svgs = lambda: sum(1 for u in requests if u.endswith(".svg"))
                before = svgs()
                box = page.locator(".score-follow-seek").bounding_box()
                y = box["y"] + box["height"] / 2
                page.mouse.move(box["x"] + 3, y)
                page.mouse.down()
                page.mouse.move(box["x"] + box["width"] * 0.8, y, steps=25)
                page.mouse.up()
                self.rendered(page)
                page.wait_for_timeout(500)
                self.assertLessEqual(svgs() - before, 4, "the drag fetched the pages it passed")
                self.assertGreater(self.folio(page), 50)

    # -- M05 ----------------------------------------------------------------

    def test_a_waking_tap_does_not_move_the_music(self) -> None:
        # The toolbar sleeps 3 s after the last input, but not while it is
        # hovered or holds focus, and a tap leaves both on what it touched.
        # So the reader first taps the page above its first system, where
        # there is no bar (the page's side margins are the turn buttons), and
        # the toolbar sleeps. Paused, so no page turns under the taps; the
        # wake-only rule does not depend on playback.
        t = self.concerto
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, _ = self.reader(browser, CONCERTO, touch=True)
                boxes = [(b, m) for b, m in enumerate(t["measures"]) if m and m[0] == 1]
                top = min(m[2] for _, m in boxes)
                system = sorted((m[1], b) for b, m in boxes if abs(m[2] - top) < 0.01)
                # The system's last bar: far enough in to tell a seek from none.
                bar = system[-1][1]
                self.assertGreater(t["first"][bar], 1500)
                m = t["measures"][bar]
                sheet = page.locator("#score-page").bounding_box()
                at = lambda fx, fy: (sheet["x"] + fx * sheet["width"], sheet["y"] + fy * sheet["height"])
                # Near the bar's top: an orchestral system is taller than the window.
                x, y = at(m[1] + m[3] / 2, m[2] + min(m[4] / 2, 0.05))
                self.assertLess(y, 790, "the first system is below the window")
                page.touchscreen.tap(*at(0.5, top / 2))
                self.expect(page.locator("body.is-idle")).to_have_count(1, timeout=8000)
                page.touchscreen.tap(x, y)
                self.expect(page.locator("body.is-idle")).to_have_count(0)
                page.wait_for_timeout(500)
                self.playing(page, False)
                self.assertEqual(self.position(page), 0, "the waking tap moved the music")
                # Awake, the same tap plays from that bar (a fresh audio element
                # applies its start position a moment after Play).
                page.touchscreen.tap(x, y)
                self.playing(page)
                start = t["first"][bar] / 1000
                for _ in range(30):
                    if self.position(page) > 0.5:
                        break
                    page.wait_for_timeout(100)
                self.assertGreaterEqual(self.position(page), start - 0.2)
                self.assertLess(self.position(page), start + 3)
                page.keyboard.press("k")

    def test_the_toolbar_recedes_unless_in_use(self) -> None:
        # Three seconds after the last input the toolbar recedes, unless a
        # mouse rests on it or keyboard focus is in it; what a tap or click
        # leaves behind (:hover on touch, focus on the pressed button) does
        # not keep it, as it used to.
        idle = "body.is-idle"
        for browser in BROWSERS:
            with self.subTest(browser=browser, reader="touch"):
                page, _ = self.reader(browser, CONCERTO, touch=True)
                page.tap(".score-follow-play")
                self.playing(page)
                self.expect(page.locator(idle)).to_have_count(1, timeout=6000)
                page.keyboard.press("k")
            with self.subTest(browser=browser, reader="mouse"):
                page, _ = self.reader(browser, CONCERTO)
                page.click(".score-follow-play")
                self.playing(page)
                page.wait_for_timeout(4500)       # resting on the toolbar
                self.expect(page.locator(idle)).to_have_count(0)
                box = page.locator("#score-reader-viewport").bounding_box()
                page.mouse.move(box["x"] + box["width"] / 2, box["y"] + box["height"] / 2)
                self.expect(page.locator(idle)).to_have_count(1, timeout=6000)
                page.keyboard.press("k")
            with self.subTest(browser=browser, reader="keyboard"):
                page, _ = self.reader(browser, CONCERTO)
                self.unfocus(page)
                for _ in range(15):
                    page.keyboard.press("Tab")
                    if page.evaluate("document.getElementById('score-reader-bar')"
                                     ".contains(document.activeElement)"):
                        break
                self.assertTrue(page.evaluate("document.getElementById('score-reader-bar')"
                                              ".contains(document.activeElement)"))
                page.wait_for_timeout(4500)
                self.expect(page.locator(idle)).to_have_count(0)

    # -- sanitising ----------------------------------------------------------

    def test_following_honours_the_sites_reduce_motion(self) -> None:
        # Every scrollBy the view makes, by its behavior; then a seek to a
        # bar low on its page, so that following has to bring it into view
        # (the symphony's pages hold several systems; the concerto's one).
        record = """window.__scrolls = [];
            const by = Element.prototype.scrollBy;
            Element.prototype.scrollBy = function (o) {
                window.__scrolls.push(o && o.behavior); return by.apply(this, arguments); };"""
        site = "try { localStorage.setItem('reduce-motion', '1'); } catch (e) {}"
        t = self.symphony
        bar = next(b for b, m in enumerate(t["measures"]) if m and m[2] > 0.7 and b in t["first"])
        for browser in BROWSERS:
            for setting, want in (("the site's", "auto"), ("none", "smooth")):
                with self.subTest(browser=browser, reduce_motion=setting):
                    page, _ = self.reader(browser, SYMPHONY,
                                          init=record + (site if setting == "the site's" else ""))
                    page.click(".score-follow-play")
                    self.playing(page)
                    page.evaluate("""at => { const s = document.querySelector('.score-follow-seek');
                        s.value = String(at); s.dispatchEvent(new Event('input')); }""",
                                  t["first"][bar] / 1000)
                    self.expect(page.locator(".score-follow-play")).to_have_attribute(
                        "aria-pressed", "true")
                    self.until(page, "window.__scrolls.length > 0")
                    page.click(".score-follow-play")
                    self.assertEqual(set(page.evaluate("window.__scrolls")), {want})

    def until(self, page, condition: str, seconds: float = 10) -> None:
        """Poll `condition` (a JavaScript expression) until it holds;
        wait_for_function's string form is an eval the CSP refuses."""
        for _ in range(int(seconds * 10)):
            if page.evaluate(f"() => !!({condition})"):
                return
            page.wait_for_timeout(100)
        self.fail(f"never held: {condition}")

    def test_escape_goes_back_to_the_work(self) -> None:
        work = f"/music/{CONCERTO}/"
        for browser in BROWSERS:
            with self.subTest(browser=browser, arrived="a shared link, in a new tab"):
                page, _ = self.reader(browser, CONCERTO, "?p=5")
                self.unfocus(page)
                page.keyboard.press("Escape")
                page.wait_for_url(self.base + work)
            with self.subTest(browser=browser, arrived="from the work's page"):
                page.locator("a.comp-frontispiece").click()
                page.wait_for_url(f"{self.base}/music/{CONCERTO}/score/")
                self.rendered(page)
                self.unfocus(page)
                page.keyboard.press("Escape")
                page.wait_for_url(self.base + work)
                # By history: the reader is still ahead of us, not behind.
                page.go_forward()
                page.wait_for_url(f"{self.base}/music/{CONCERTO}/score/")
            with self.subTest(browser=browser, arrived="from elsewhere on the site"):
                page.goto(self.base + "/music/", wait_until="load")
                page.evaluate(f"location.href = '/music/{CONCERTO}/score/'")
                page.wait_for_url(f"{self.base}/music/{CONCERTO}/score/")
                self.rendered(page)
                self.unfocus(page)
                page.keyboard.press("Escape")
                page.wait_for_url(self.base + work)

    def test_pages_are_inlined_inert(self) -> None:
        hostile = ('<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
                   'width="210mm" height="297mm" viewBox="0 0 210 297" onload="window.__pwned=1">'
                   '<title>s6 (1)</title><script>window.__pwned=2</script>'
                   '<a xlink:href="javascript:window.__pwned=3"><rect id="r" width="50" height="50"/></a>'
                   '<a href="file:///home/someone/score.ly"><circle id="c" r="5"/></a>'
                   '<use href="#r" x="60"/><g onclick="window.__pwned=4"><text x="1" y="90">ok</text></g></svg>')
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, _ = self.reader(browser, "liebe-ohne-zeit", routes={"page": r"/music/.+\.svg$"},
                                      answers={"page": answer(hostile, "image/svg+xml")})
                svg = page.locator("#score-page svg")
                self.expect(svg.locator("script, title")).to_have_count(0)
                self.assertEqual(page.evaluate("""() => [...document.querySelectorAll('#score-page *')]
                    .flatMap(e => [...e.attributes].map(a => a.name))
                    .filter(n => /^on/i.test(n))"""), [])
                hrefs = page.evaluate("""() => [...document.querySelectorAll('#score-page [href], #score-page a')]
                    .map(e => e.getAttribute('href') || e.getAttributeNS('http://www.w3.org/1999/xlink', 'href'))""")
                self.assertEqual([h for h in hrefs if h], ["#r"])
                page.click("#score-page rect", force=True)
                self.assertIsNone(page.evaluate("window.__pwned"))


if __name__ == "__main__":
    unittest.main()
