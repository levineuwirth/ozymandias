"""A site's banner between the menu and the page (templates/partials/
banner.html), in Chromium and Firefox, under the engine's own stylesheets
and nav.js as the site serves them:

  · a banner that is a <header> of its own is not taken for the menu's:
    it is not stuck to the window's top, and print keeps it while the
    menu's header goes;
  · the page shell gives up what the banner takes of the page's flow
    (--banner-height, from nav.js), so a short page's footer ends at the
    window's foot and the page does not scroll;
  · a page with no banner has no --banner-height at all.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_banner -v
"""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, asset_root, check_site, enforcing_csp, require_playwright,
                            requires_browser, site_server)

# The engine's sheets alone, so that the rules under test are the engine's
# and not a site's own for its header.
SHEETS = ("tokens", "reset", "layout", "nav", "print")
BANNER = '<header class="site-banner" role="presentation" data-pagefind-ignore="all"><h1>The Site</h1></header>\n'
PAGE = """<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Banner fixture</title>
{sheets}
<link rel="stylesheet" href="/__fixture/banner.css">
<script src="{assets}/js/utils.js"></script>
<script src="{assets}/js/nav.js" defer></script>
</head><body class="{body_class}">
<a class="skip-link" href="#markdownBody">Skip to content</a>
<header data-pagefind-ignore="all">
    <nav class="site-nav" aria-label="Site"><div class="nav-row-primary">
        <div class="nav-primary"><a href="/">Home</a></div>
    </div></nav>
</header>
{banner}<div class="page-shell">
<main id="markdownBody"><p>A short page.</p></main>
<footer data-pagefind-ignore>The footer.</footer>
</div>
</body></html>
"""
CSS = """
.padded { padding-top: 36px; }
.site-banner { height: 240px; margin: 0; }
.site-banner h1 { margin: 0; }
.site-banner.spaced { margin-block: 16px 32px; }
.site-banner.overlay { position: absolute; }
.site-banner.tall { height: 320px; }
"""

MEASURE = """() => {
    const menu = document.querySelector('body > header:first-of-type'), banner = document.querySelector('.site-banner');
    const style = el => el && { position: getComputedStyle(el).position, zIndex: getComputedStyle(el).zIndex,
                                display: getComputedStyle(el).display };
    return { menu: style(menu), banner: style(banner),
             room: document.documentElement.style.getPropertyValue('--banner-height'),
             footer: document.querySelector('.page-shell > footer').getBoundingClientRect().bottom,
             window: innerHeight, page: document.documentElement.scrollHeight };
}"""


def page(assets: str, banner: str = BANNER, body_class: str = "") -> str:
    sheets = "\n".join(f'<link rel="stylesheet" href="{assets}/css/{name}.css">' for name in SHEETS)
    slot = f'<div class="banner-slot">{banner}</div>' if banner else ""
    return PAGE.format(sheets=sheets, assets=assets, banner=slot, body_class=body_class)


@requires_browser
class SiteBanner(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import sync_playwright
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-banner-")))
        (tmp / "fixtures").mkdir()
        for path, banner in (("banner", BANNER), ("bare", "")):
            for body_class in ("", "padded"):
                name = f"{body_class}-{path}" if body_class else path
                (tmp / "fixtures" / f"{name}.html").write_text(page(asset_root(), banner, body_class), encoding="utf-8")
        (tmp / "fixtures" / "banner.css").write_text(CSS, encoding="utf-8")
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp(), tmp / "fixtures"))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def open_page(self, browser: str, path: str):
        context = self.browsers[browser].new_context(viewport={"width": 1440, "height": 900})
        self.addCleanup(context.close)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        page.goto(f"{self.base}/__fixture/{path}", wait_until="load")
        # nav.js measures at DOMContentLoaded and again as fonts settle.
        page.wait_for_function("document.fonts.status === 'loaded'")
        page.wait_for_function("document.documentElement.style.getPropertyValue('--nav-flow-height') !== ''")
        self.assertEqual(errors, [])
        return page

    def test_a_header_of_the_site_s_own_is_not_the_menu_s(self):
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser, "banner.html")
                seen = page.evaluate(MEASURE)
                self.assertEqual(page.get_by_role("banner").count(), 1)
                self.assertEqual(page.get_by_role("heading", name="The Site").count(), 1)
                self.assertEqual(seen["menu"]["position"], "sticky")
                self.assertEqual((seen["banner"]["position"], seen["banner"]["zIndex"]), ("static", "auto"))
                page.emulate_media(media="print")
                printed = page.evaluate(MEASURE)
                self.assertEqual(printed["menu"]["display"], "none")
                self.assertNotEqual(printed["banner"]["display"], "none")

    def test_a_short_page_s_footer_still_ends_at_the_window_s_foot(self):
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                seen = self.open_page(browser, "banner.html").evaluate(MEASURE)
                self.assertEqual(seen["room"], "240px")
                self.assertLessEqual(seen["footer"], seen["window"] + 0.5)
                self.assertGreater(seen["footer"], seen["window"] - 2)
                self.assertEqual(seen["page"], seen["window"])

    def test_a_page_with_no_banner_has_no_room_set_for_one(self):
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                seen = self.open_page(browser, "bare.html").evaluate(MEASURE)
                self.assertEqual(seen["room"], "")
                self.assertLessEqual(seen["footer"], seen["window"] + 0.5)
                self.assertEqual(seen["page"], seen["window"])

    def test_body_padding_is_outside_the_banner(self):
        for browser in BROWSERS:
            for path, room in (("padded-bare.html", ""), ("padded-banner.html", "240px")):
                with self.subTest(browser=browser, path=path):
                    seen = self.open_page(browser, path).evaluate(MEASURE)
                    self.assertEqual(seen["room"], room)
                    self.assertAlmostEqual(seen["footer"], seen["window"] + 36, delta=1)

    def test_banner_tracks_margin_position_and_replacement(self):
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser, "banner.html")
                banner = page.locator(".site-banner")

                def check_room(expected):
                    page.wait_for_function(
                        "room => document.documentElement.style.getPropertyValue('--banner-height') === room",
                        arg=expected, timeout=3000)
                    seen = page.evaluate(MEASURE)
                    self.assertAlmostEqual(seen["footer"], seen["window"], delta=1)
                    self.assertEqual(seen["page"], seen["window"])

                banner.evaluate("el => el.classList.add('spaced')")
                check_room("288px")
                banner.evaluate("el => el.classList.add('overlay')")
                check_room("")
                banner.evaluate("el => el.replaceWith(el.cloneNode(true))")
                banner.evaluate("el => el.classList.remove('overlay', 'spaced')")
                check_room("240px")
                banner.evaluate("el => el.classList.add('tall')")
                check_room("320px")
                banner.evaluate("el => el.remove()")
                check_room("")


if __name__ == "__main__":
    unittest.main()
