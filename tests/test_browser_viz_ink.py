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

    def drawn(self, browser):
        context = self.browsers[browser].new_context()
        self.addCleanup(context.close)

        def answer(route):
            url = urlsplit(route.request.url)
            if url.netloc != "viz.test":
                route.abort()
            elif url.path == "/":
                route.fulfill(content_type="text/html", body=PAGE)
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
        return page.evaluate("window.drawn")

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


if __name__ == "__main__":
    unittest.main()
