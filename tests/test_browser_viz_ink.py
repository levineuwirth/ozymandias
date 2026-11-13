"""Charts are drawn in the theme's ink, in Chromium and Firefox, using
current source files without a site build.

static/js/viz.js gives Vega its colors from the theme's tokens (--text,
--text-muted, --border, --border-muted), each turned into #rrggbb by the
browser's canvas whatever syntax the theme wrote it in, since Vega reads
only a few. A stand-in for Vega takes the config it is given.

    RUN_BROWSER_TESTS=1 .venv/bin/python -m unittest tests.test_browser_viz_ink -v
"""
from __future__ import annotations

import mimetypes
import unittest
from urllib.parse import urlsplit

from tests._browser import BROWSERS, require_playwright, requires_browser
from tests._helpers import engine_path

ORIGIN = "http://viz.test"
PAGE = """<!doctype html><html lang="en"><head><meta charset="utf-8">
<style>:root { color-scheme: light; --text: oklch(0.3 0.05 60); --text-muted: rgb(85 85 85);
  --border: hsl(0 0% 80%); --border-muted: #aaa; }</style>
<script>window.vegaEmbed = function (el, spec) { window.drawn = spec.config;
  return Promise.resolve({finalize: function () {}}); };</script>
<script src="/js/viz.js" defer></script>
</head><body><div class="vega-container"><script type="application/json" class="vega-spec">{"mark": "point"}</script></div>
</body></html>"""


@requires_browser
class ChartInk(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        require_playwright()
        from playwright.sync_api import sync_playwright
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def open_page(self, browser, extra_css=""):
        context = self.browsers[browser].new_context()
        self.addCleanup(context.close)

        def answer(route):
            url = urlsplit(route.request.url)
            if url.netloc != "viz.test":
                route.abort()
            elif url.path == "/":
                route.fulfill(content_type="text/html", body=PAGE.replace("</style>", extra_css + "</style>"))
            else:
                path = engine_path("static/" + url.path.lstrip("/"))
                if path.is_file():
                    route.fulfill(content_type=mimetypes.guess_type(path.name)[0] or "text/plain",
                                  body=path.read_bytes())
                else:
                    route.abort()

        context.route("**/*", answer)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        self.addCleanup(lambda: self.assertEqual(errors, []))
        page.goto(ORIGIN, wait_until="load")
        page.wait_for_function("window.drawn")
        return page

    def drawn(self, browser, extra_css=""):
        return self.open_page(browser, extra_css).evaluate("window.drawn")

    def test_the_tokens_are_the_chart_s_colors_in_any_syntax(self):
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                config = self.drawn(browser)
                text = config["mark"]["color"]
                self.assertRegex(text, r"^#[0-9a-f]{6}$")
                self.assertNotEqual(text, "#1a1a1a", "oklch() was not read")
                self.assertEqual([config["axis"]["labelColor"], config["axis"]["titleColor"],
                                  config["axis"]["gridColor"]], [text, "#555555", "#cccccc"])
                self.assertEqual(config["range"]["category"],
                                 [text, "#555555", "#808080", "#aaaaaa", "#cccccc"])

    def test_translucent_series_have_an_alpha_weighted_midpoint(self):
        cases = [
            ("rgb(255 0 0 / 50%)", "rgb(0 0 255 / 25%)", "rgba(170, 0, 85, 0.376)"),
            ("red", "rgb(0 0 255 / 50%)", "rgba(170, 0, 85, 0.751)"),
            ("transparent", "blue", "rgba(0, 0, 255, 0.500)"),
            ("transparent", "transparent", "rgba(0, 0, 0, 0.000)"),
        ]
        for browser in BROWSERS:
            for muted, border, expected in cases:
                with self.subTest(browser=browser, muted=muted, border=border):
                    config = self.drawn(browser, ':root { --text-muted: ' + muted
                                        + '; --border-muted: ' + border + '; }')
                    self.assertEqual(config['range']['category'][2], expected)

    def test_colors_use_the_page_context_and_follow_a_theme_change(self):
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser, '''
                  :root { color: #2468ac; --text: currentColor;
                    --text-muted: color-mix(in srgb, currentColor 50%, black);
                    --border: light-dark(#123456, #abcdef); }
                  :root[data-theme="night"] { color-scheme: dark; color: #804020; }
                ''')
                config = page.evaluate('window.drawn')
                self.assertEqual(config['mark']['color'], '#2468ac')
                self.assertEqual(config['axis']['titleColor'], '#123456')
                self.assertEqual(config['axis']['gridColor'], '#123456')
                page.evaluate("document.documentElement.dataset.theme = 'night'")
                page.wait_for_function("window.drawn.mark.color === '#804020'")
                config = page.evaluate('window.drawn')
                self.assertEqual(config['axis']['titleColor'], '#402010')
                self.assertEqual(config['axis']['gridColor'], '#abcdef')
                self.assertEqual(page.locator('html > span').count(), 0, 'color probes must be removed')


if __name__ == "__main__":
    unittest.main()
