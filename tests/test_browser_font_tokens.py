"""The epistemic figure is set in the site's faces, in Chromium and Firefox.

build/Marks.hs writes the figure's words (the trust score, its result
glyph, the TRUST label) with classes marks.css sets in the font tokens,
--font-serif and --font-sans, not with the names of the faces a site
happens to have now. A page whose tokens name other faces sets the
figure in those.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_font_tokens -v
"""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from tests._browser import BROWSERS, check_site, enforcing_csp, require_playwright, requires_browser, site_server
from tests.target import requires_switch, route

FACES = """() => [...document.querySelectorAll('.frontmatter-mark--epistemic svg text')]
    .map(t => [t.getAttribute('class'), getComputedStyle(t).fontFamily])"""


def unquoted(value: str) -> str:
    """A font-family as a browser computes it, whatever its quoting: one
    word goes unquoted ("Spectral" is Spectral)."""
    return value.replace('"', "").replace("'", "").replace(" ", "")


@requires_switch("epistemic")
@requires_browser
class EpistemicFigureFaces(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import sync_playwright
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-font-tokens-")))
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp()))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def test_the_figure_follows_the_font_tokens(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                context = self.browsers[browser].new_context()
                self.addCleanup(context.close)
                page = context.new_page()
                page.goto(self.base + route("essay-peer-reviewed"), wait_until="load")
                faces = page.evaluate(FACES)
                if not faces:
                    self.skipTest("the page has no epistemic figure")
                serif = page.evaluate("getComputedStyle(document.documentElement).getPropertyValue('--font-serif').trim()")
                sans = page.evaluate("getComputedStyle(document.documentElement).getPropertyValue('--font-sans').trim()")
                expected = {"ep-figure-serif": serif, "ep-figure-sans": sans}
                for cls, family in faces:
                    self.assertIn(cls, expected)
                    self.assertEqual(unquoted(family), unquoted(expected[cls]))
                # Another site's faces: the tokens alone change.
                page.add_style_tag(content=':root { --font-serif: "Courier New", monospace;'
                                           ' --font-sans: "Comic Neue", cursive; }')
                changed = dict(page.evaluate(FACES))
                self.assertEqual(unquoted(changed["ep-figure-serif"]), "CourierNew,monospace")
                self.assertEqual(unquoted(changed["ep-figure-sans"]), "ComicNeue,cursive")


if __name__ == "__main__":
    unittest.main()
