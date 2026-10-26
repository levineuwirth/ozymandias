"""Photography: view modes, the map, and the lightbox, in Chromium and
Firefox.

photography-modes.js switches the /photography/ grid between grid,
masonry and chronological views, remembered across visits, and in
masonry sizes each card to its photograph. photography-map.js draws one
marker per place from /photography/map.json on CartoDB tiles. lightbox.js
opens a photograph full size, in "darkroom" on photography pages. The
pages are the built ones; the map's data and tiles are faked, so its
markers are known and no tile is fetched (the CSP sweep meets the live
tiles).

Checked: modes, aria-current and the remembered choice, the map link
storing nothing; masonry cards hold their photographs and captions; one
marker per place with its count, label and destination, an escaped
tooltip, unusable pins skipped; wheel zoom on every focus, not only the
first (audit J11), and the data fetched under the normal cache rules;
the map's error and empty states; the lightbox by pointer and keyboard,
its darkroom info panel, focus trap, Escape and focus return, and the
image released afterwards.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_photography -v
"""

from __future__ import annotations

import base64
import re
import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, FakeNetwork, check_site, enforcing_csp, require_playwright,
                            requires_browser, site_server)
from tests._browser import fake_answer as answer

PNG = base64.b64decode("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk"
                       "+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==")

PHOTO = "/photography/denmark/copenhagen-005/"
ESSAY_WITH_FIGURE = "/essays/where-does-simd-help-post-quantum-cryptography/"

# Four places far apart, so no two cluster at the zoom that frames them.
PINS = [
    # One series, two frames, one place: a marker counting 2, to the series.
    {"lat": 55.7, "lon": 12.6, "title": "Harbour", "location": "Copenhagen, Denmark",
     "series": "alpha", "url": "/photography/alpha/one/", "captured": "2026-08-01"},
    {"lat": 55.7, "lon": 12.6, "title": "Bridge", "location": "Copenhagen, Denmark",
     "series": "alpha", "url": "/photography/alpha/two/", "captured": "2026-08-03"},
    # A lone frame: a plain marker, to itself; its title is hostile.
    {"lat": -33.9, "lon": 151.2, "title": '<img src=x onerror="window.__pwned=1">Opera',
     "location": "Sydney, Australia", "series": "beta", "url": "/photography/beta/opera/"},
    # Two series at one place: counted, but nowhere single to go.
    {"lat": 40.7, "lon": -74.0, "title": "Street", "location": "New York, USA", "series": "gamma",
     "url": "/photography/gamma/street/"},
    {"lat": 40.7, "lon": -74.0, "title": "Park", "location": "New York, USA", "series": "delta",
     "url": "/photography/delta/park/"},
    # Unusable: no number for a coordinate.
    {"lat": "64.1", "lon": -21.9, "title": "Nowhere", "location": "Reykjavik, Iceland"},
]

ROUTES = {"map": r"/photography/map\.json$", "tiles": r"^https://[a-d]\.basemaps\.cartocdn\.com/",
          "leaflet": r"/leaflet/leaflet(?:\.markercluster)?\.js$"}
# Same-origin paths a test may answer itself, by route name.
LOCAL = {"map": r"photography/map\.json$", "leaflet": r"leaflet/leaflet(?:\.markercluster)?\.js$"}

# Every fetch() call's URL and options, before the page's scripts run.
RECORD_FETCH = """window.__fetches = [];
const realFetch = window.fetch;
window.fetch = function (url, init) {
    window.__fetches.push([String(url), init ? JSON.parse(JSON.stringify(init)) : null]);
    return realFetch.apply(this, arguments);
};"""


@requires_browser
class Photography(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-photography-")))
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp()))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def open(self, browser: str, path: str, *, width: int = 1280, height: int = 900,
             fakes: dict | None = None, init: str = ""):
        context = self.browsers[browser].new_context(viewport={"width": width, "height": height})
        self.addCleanup(context.close)
        if init:
            context.add_init_script(init)
        # Always on: without fakes, any request leaving serve.py is one no
        # route answers, and fails the test.
        fakes = fakes or {}
        local = "|".join(LOCAL[name] for name in fakes if name in LOCAL) or None
        net = FakeNetwork(context, self.base, {k: v for k, v in ROUTES.items() if k in fakes},
                          fakes, local=local)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))

        def clean():
            self.assertEqual(errors, [], "page errors")
            self.assertEqual(net.unexpected, [], "requests no route answers")
        self.addCleanup(clean)
        page.goto(self.base + path, wait_until="load")
        return page, net

    # -- view modes --------------------------------------------------------

    def test_view_modes(self) -> None:
        grid = ".photography-grid"
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                # Faked map data and tiles: the test ends by following the map link.
                page, _ = self.open(browser, "/photography/", fakes=self.map_answers())
                self.expect(page.locator(grid)).to_have_attribute("data-photography-mode", "grid")
                self.expect(page.locator(".mode-btn[aria-current]")).to_have_attribute("data-mode", "grid")
                # The page offers grid and masonry (photography-modes.js also
                # knows "chronological", which no page links to).
                for mode in ("masonry", "grid", "masonry"):
                    page.click(f".mode-btn[data-mode={mode}]")
                    self.expect(page.locator(grid)).to_have_attribute("data-photography-mode", mode)
                    self.expect(page.locator(".mode-btn[aria-current]")).to_have_attribute("data-mode", mode)
                    self.expect(page.locator(".mode-btn[aria-pressed]")).to_have_count(0)
                    self.assertEqual(urlpath(page.url), "/photography/")
                self.assertEqual(page.evaluate("localStorage.getItem('photography-mode')"), "masonry")
                page.reload(wait_until="load")
                self.expect(page.locator(grid)).to_have_attribute("data-photography-mode", "masonry")
                # The map is a page of its own: followed, and not stored.
                page.click(".mode-btn[data-mode=map]")
                page.wait_for_url(re.compile(r"/photography/map/$"))
                self.assertEqual(page.evaluate("localStorage.getItem('photography-mode')"), "masonry")

    def test_masonry_cards_hold_their_photographs(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, _ = self.open(browser, "/photography/",
                                    init="try { localStorage.setItem('photography-mode', 'masonry'); } catch (e) {}")
                self.expect(page.locator(".photography-grid")).to_have_attribute("data-photography-mode", "masonry")
                # Every image loaded (they are lazy), then one frame for the spans.
                page.evaluate("""async () => {
                    for (const img of document.querySelectorAll('.photo-card-img')) {
                        img.loading = 'eager';
                        if (!img.complete) await new Promise(r => { img.onload = img.onerror = r; });
                    }
                    await new Promise(r => requestAnimationFrame(() => requestAnimationFrame(r)));
                }""")
                # Each card's photograph and caption against the grid area
                # its span gives it (rows and gap as the grid computes them):
                # content past it runs into the card below.
                cards = page.evaluate("""() => {
                    const gs = getComputedStyle(document.querySelector('.photography-grid'));
                    const row = parseFloat(gs.gridAutoRows), gap = parseFloat(gs.rowGap);
                    return [...document.querySelectorAll('.photo-card')].map(c => {
                        const span = +((/span (\\d+)/.exec(c.style.gridRowEnd) || [])[1] || 0);
                        return [c.querySelector('a').getAttribute('href'), span,
                                span * row + (span - 1) * gap,
                                c.querySelector('.photo-card-link').getBoundingClientRect().height];
                    });
                }""")
                self.assertGreater(len(cards), 1)
                self.assertEqual([c[0] for c in cards if not c[1]], [], "cards without a span")
                self.assertEqual([c for c in cards if c[3] > c[2] + 0.5], [],
                                 "cards whose content runs past their area: [href, span, area, content]")

    # -- the map -----------------------------------------------------------

    def map_answers(self, pins=PINS) -> dict:
        return {"map": answer(pins), "tiles": answer(PNG, "image/png")}

    def map_page(self, browser: str, pins=PINS, **changed):
        answers = self.map_answers(pins)
        answers.update(changed)
        return self.open(browser, "/photography/map/", fakes=answers, init=RECORD_FETCH)

    def zoom(self, page) -> int:
        """The map's zoom, from the tiles it shows."""
        return page.evaluate("""() => Math.max(...[...document.querySelectorAll('.leaflet-tile-container')]
            .filter(c => c.querySelector('.leaflet-tile-loaded') && getComputedStyle(c).visibility !== 'hidden')
            .map(c => +c.querySelector('img').src.match(/cartocdn\\.com\\/light_all\\/(\\d+)\\//)[1]))""")

    def steady_zoom(self, page) -> int:
        """The zoom once it has held for 600 ms: the map opens with an
        animated zoom to fit its markers, and a wheel turn animates too."""
        last, since = None, 0
        for _ in range(40):
            z = self.zoom(page)
            since = since + 1 if z == last else 0
            if since >= 3:
                return z
            last = z
            page.wait_for_timeout(200)
        raise AssertionError("the map's zoom never settled")

    def wheel(self, page) -> None:
        box = page.locator("#photography-map").bounding_box()
        page.mouse.move(box["x"] + box["width"] / 3, box["y"] + box["height"] / 3)
        page.mouse.wheel(0, -480)
        page.wait_for_timeout(900)

    def test_markers(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, net = self.map_page(browser)
                pins = page.locator(".photography-map-pin")
                self.expect(pins).to_have_count(3)
                titles = sorted(m.get_attribute("title") for m in page.locator(".leaflet-marker-icon").all())
                self.assertEqual(titles, ["Copenhagen — 2 photographs", "New York — 2 photographs", "Sydney"])
                self.expect(page.locator(".photography-map-pin span", has_text="2")).to_have_count(2)
                page.hover(".leaflet-marker-icon[title=Sydney]")
                tip = page.locator(".photography-map-tooltip-title")
                self.expect(tip).to_have_text('<img src=x onerror="window.__pwned=1">Opera')
                self.assertIsNone(page.evaluate("window.__pwned"))
                page.hover(".leaflet-marker-icon[title='Copenhagen — 2 photographs']")
                self.expect(page.locator(".photography-map-tooltip-meta")).to_have_text(
                    "2 photographs · 2026-08-01 – 2026-08-03")
                # Mixed series: no single destination, so a click goes nowhere.
                page.click(".leaflet-marker-icon[title='New York — 2 photographs']")
                page.wait_for_timeout(300)
                self.assertEqual(urlpath(page.url), "/photography/map/")
                page.click(".leaflet-marker-icon[title='Copenhagen — 2 photographs']")
                page.wait_for_url(re.compile(r"/photography/alpha/$"))

    def test_wheel_zoom_on_every_focus(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, net = self.map_page(browser)
                self.expect(page.locator(".photography-map-pin")).to_have_count(3)
                self.expect(page.locator(".leaflet-tile-loaded").first).to_be_visible()
                start = self.steady_zoom(page)
                self.wheel(page)
                self.assertEqual(self.steady_zoom(page), start, "the wheel zoomed a map without focus")
                for round_ in (1, 2):
                    page.focus("#photography-map")
                    before = self.steady_zoom(page)
                    self.wheel(page)
                    self.assertGreater(self.steady_zoom(page), before, f"focus {round_}: the wheel did not zoom")
                    page.evaluate("document.activeElement.blur()")
                    after_blur = self.steady_zoom(page)
                    self.wheel(page)
                    self.assertEqual(self.steady_zoom(page), after_blur, f"blur {round_}: the wheel still zoomed")
                # J11: under the normal cache rules.
                [(url, init)] = [f for f in page.evaluate("window.__fetches") if f[0].endswith("map.json")]
                self.assertNotEqual((init or {}).get("cache"), "force-cache")

    def test_map_states(self) -> None:
        cases = {
            "data missing": ({"map": answer("", "text/plain", 404)},
                             ".photography-map-error", "Could not load map data: HTTP 404"),
            "no pins": ({"map": answer([])}, ".photography-map-empty",
                        "No geo-tagged photographs yet. Photos with a geo: frontmatter field will appear here."),
            # Leaflet and its cluster plugin come from one directory: they
            # fail together (the plugin alone throws without Leaflet).
            "no Leaflet": ({"leaflet": answer("", "text/plain", 404)}, ".photography-map-error",
                           "Map library failed to load."),
        }
        for browser in BROWSERS:
            for case, (changed, selector, text) in cases.items():
                with self.subTest(browser=browser, case=case):
                    answers = self.map_answers()
                    answers.update(changed)
                    page, _ = self.open(browser, "/photography/map/", fakes=answers)
                    self.expect(page.locator(selector)).to_have_text(text)

    # -- the lightbox ------------------------------------------------------

    def test_lightbox_on_a_photograph(self) -> None:
        overlay = ".lightbox-overlay"
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, _ = self.open(browser, PHOTO, height=1000)
                trigger = page.locator("img[data-lightbox]").first
                self.expect(trigger).to_have_attribute("role", "button")
                trigger.focus()
                page.keyboard.press("Enter")
                self.expect(page.locator(f"{overlay}.is-open")).to_be_visible()
                self.expect(page.locator(overlay)).to_have_class(re.compile(r"\bdarkroom\b"))
                big = page.locator(".lightbox-img")
                self.assertEqual(big.get_attribute("src"), trigger.evaluate("i => i.src"))
                self.assertEqual(big.get_attribute("alt"), trigger.get_attribute("alt"))
                self.expect(page.locator(".lightbox-close")).to_be_focused()
                # The darkroom's info panel: its button, and the I key.
                info = page.locator(".lightbox-info-toggle")
                self.expect(info).to_be_visible()
                page.keyboard.press("i")
                self.expect(page.locator(overlay)).to_have_class(re.compile(r"\bis-info-visible\b"))
                self.expect(info).to_have_attribute("aria-pressed", "true")
                page.keyboard.press("i")
                self.expect(info).to_have_attribute("aria-pressed", "false")
                # Tab stays inside, however many times it is pressed (more
                # than the overlay has stops; Firefox also stops at scrollers).
                for _ in range(10):
                    page.keyboard.press("Tab")
                    self.assertTrue(page.evaluate(
                        "document.querySelector('.lightbox-overlay').contains(document.activeElement)"))
                page.keyboard.press("Escape")
                self.expect(page.locator(f"{overlay}.is-open")).to_have_count(0)
                self.expect(trigger).to_be_focused()
                # Released once the fade is over.
                self.expect(big).not_to_have_attribute("src", re.compile(".*"))

    def test_slideshow_takes_focus(self) -> None:
        # The slideshow shares the lightbox's overlay styles: with them,
        # focus has to reach Play/Pause as it does on the bare fixture
        # (tests/test_browser_slideshow.py).
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, _ = self.open(browser, "/photography/europa-2024/")
                page.click("[data-mode=slideshow]")
                self.expect(page.locator(".slideshow-overlay.is-open")).to_be_visible()
                self.expect(page.locator(".slideshow-play")).to_be_focused()
                page.keyboard.press("Escape")
                self.expect(page.locator("[data-mode=slideshow]")).to_be_focused()

    def test_lightbox_on_an_essay_figure(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, _ = self.open(browser, ESSAY_WITH_FIGURE)
                trigger = page.locator("img[data-lightbox]").first
                trigger.scroll_into_view_if_needed()
                trigger.click()
                self.expect(page.locator(".lightbox-overlay.is-open")).to_be_visible()
                self.expect(page.locator(".lightbox-overlay")).not_to_have_class(re.compile(r"\bdarkroom\b"))
                self.expect(page.locator(".lightbox-info-toggle")).to_be_hidden()
                caption = trigger.evaluate(
                    "i => (i.parentElement.querySelector('figcaption') || {}).textContent || ''").strip()
                self.assertTrue(caption, "the essay's figure has no caption to check")
                self.expect(page.locator(".lightbox-caption")).to_have_text(caption)
                page.mouse.click(5, 5)   # the backdrop closes it
                self.expect(page.locator(".lightbox-overlay.is-open")).to_have_count(0)


def urlpath(url: str) -> str:
    from urllib.parse import urlsplit
    return urlsplit(url).path


if __name__ == "__main__":
    unittest.main()
