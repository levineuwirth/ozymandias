"""The map at a piece's foot (templates/partials/page-footer.html,
TagMap.renderPieceMap, css/aftermatter.css), in Chromium and Firefox, on
the site's built pages under its enforcing policy: one page of each shape
the footer's row takes (which of the record, the map, the history and
Rights it holds), at desktop, tablet and phone widths, at the default text
size and the largest:

  · no section of the row is narrower than what it holds: nothing in it
    overflows its box, so a section that does not fit goes to the next
    line rather than being squeezed;
  · each line of the row is centered under the page;
  · the map is drawn, and the page does not scroll sideways.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_page_map -v
"""

from __future__ import annotations

import re
import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, SITE, check_site, enforcing_csp, require_playwright,
                            requires_browser, site_server)
from tests.target import requires_switch

WIDTHS = (1440, 1100, 900, 390)
TEXT_SIZES = (None, 29)      # the default, and the settings panel's largest
TEXT_SIZE_INIT = "try { localStorage.setItem('text-size', '%d'); } catch (e) {}"

ROW = r'class="meta-footer-grid meta-footer-grid--map">(.*?)</section>'
SECTION_ID = r'<div class="meta-footer-section[^"]*" id="([^"]+)"'

MEASURE = """() => {
    const grid = document.querySelector('.meta-footer-grid--map');
    const box = grid.getBoundingClientRect();
    const flex = getComputedStyle(grid).display === 'flex';
    const sections = [...grid.children].map(s => {
        const r = s.getBoundingClientRect();
        return { id: s.id, left: r.left, right: r.right, top: Math.round(r.top),
                 width: r.width, scroll: s.scrollWidth, client: s.clientWidth };
    });
    const svg = grid.querySelector('svg.tag-map');
    return { flex, left: box.left, right: box.right, sections,
             map: svg ? svg.getBoundingClientRect().height : 0,
             sideways: document.documentElement.scrollWidth - innerWidth };
}"""


def footer_shapes() -> dict[tuple[str, ...], str]:
    """One page (its URL path) for each set of sections the row holds."""
    shapes: dict[tuple[str, ...], str] = {}
    for html in sorted(SITE.rglob("*.html")):
        text = html.read_text(encoding="utf-8", errors="replace")
        row = re.search(ROW, text, re.S)
        if not row:
            continue
        shape = tuple(re.findall(SECTION_ID, row.group(1)))
        rel = html.relative_to(SITE).as_posix()
        shapes.setdefault(shape, "/" + (rel[:-len("index.html")] if rel.endswith("index.html") else rel))
    return shapes


@requires_switch("tag-map")
@requires_browser
class PageMap(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import sync_playwright
        cls.shapes = footer_shapes()
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-page-map-")))
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp()))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def measure(self, browser: str, path: str, width: int, text_size: int | None) -> dict:
        context = self.browsers[browser].new_context(viewport={"width": width, "height": 900})
        self.addCleanup(context.close)
        if text_size:
            context.add_init_script(TEXT_SIZE_INIT % text_size)
        page = context.new_page()
        page.goto(self.base + path, wait_until="load")
        page.locator(".meta-footer-grid--map").scroll_into_view_if_needed()
        page.wait_for_timeout(150)
        return page.evaluate(MEASURE)

    def test_the_row_fits_what_it_holds_and_is_centered(self) -> None:
        self.assertTrue(self.shapes, "no page has the map at its foot")
        for shape, path in sorted(self.shapes.items()):
            for browser in BROWSERS:
                for width in WIDTHS:
                    for text_size in TEXT_SIZES:
                        with self.subTest(page=path, shape=shape, browser=browser,
                                          width=width, text_size=text_size or "default"):
                            m = self.measure(browser, path, width, text_size)
                            self.assertGreater(m["map"], 0, "the map is drawn")
                            self.assertLessEqual(m["sideways"], 0, "the page scrolls sideways")
                            squeezed = [(s["id"], s["client"], s["scroll"]) for s in m["sections"]
                                        if s["scroll"] > s["client"] + 1]
                            self.assertEqual(squeezed, [], "a section narrower than what it holds")
                            if not m["flex"]:
                                continue
                            lines: dict[int, list[dict]] = {}
                            for s in m["sections"]:
                                lines.setdefault(s["top"], []).append(s)
                            for line in lines.values():
                                before = min(s["left"] for s in line) - m["left"]
                                after = m["right"] - max(s["right"] for s in line)
                                self.assertAlmostEqual(before, after, delta=2,
                                                       msg=[s["id"] for s in line])


if __name__ == "__main__":
    unittest.main()
