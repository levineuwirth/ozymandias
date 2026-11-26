"""The map at a piece's foot (templates/partials/page-footer.html,
TagMap.renderPieceMap, css/aftermatter.css), in Chromium and Firefox, on
the site's built pages under its enforcing policy: one page of each shape
the footer's row takes (which of the record, the map, the history and
Rights it holds), at desktop, tablet and phone widths, at the default text
size and the largest:

  · no section of the row is narrower than what it holds: nothing in it
    overflows its box, so a section that does not fit goes to the next
    line rather than being squeezed;
  · each line of the row is centered under the page, whether the sections
    stand in a row (flex) or stack one to a line (the phone's grid);
  · the map is drawn, and the page does not scroll sideways;
  · the version history (js/version-history.js), on every page of each
    shape the footer's row takes with it and on the longest histories,
    lists every entry when it is alone on its line, and beside the other
    sections reaches no lower than the bottom of what they hold (its
    newest entry always shown), the rest under a "More" button that shows
    them all and folds them again; opened, and the window then narrowed
    until nothing can fold, it keeps no button, and widened again it is
    open as it was.

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
    const sections = [...grid.children].map(s => {
        const r = s.getBoundingClientRect();
        return { id: s.id, left: r.left, right: r.right, top: Math.round(r.top),
                 width: r.width, scroll: s.scrollWidth, client: s.clientWidth };
    });
    const svg = grid.querySelector('svg.tag-map');
    return { left: box.left, right: box.right, sections,
             map: svg ? svg.getBoundingClientRect().height : 0,
             sideways: document.documentElement.scrollWidth - innerWidth };
}"""


HISTORY = """() => {
    const section = document.getElementById('version-history');
    const top = section.getBoundingClientRect().top;
    const held = el => Math.max(el.getBoundingClientRect().top, ...[...el.children]
        .filter(c => c.tagName !== 'SCRIPT' && !c.hidden).map(c => c.getBoundingClientRect().bottom));
    const beside = [...section.parentElement.children]
        .filter(s => s !== section && Math.abs(s.getBoundingClientRect().top - top) <= 2);
    const items = [...section.querySelectorAll('.version-history-list > li')];
    const shown = items.filter(li => li.getClientRects().length);
    const button = section.querySelector('button.version-history-more');
    const visible = button && !button.hidden && button.getClientRects().length > 0;
    // Below the section's top, in whole pixels: folding entries may scroll
    // the page, by a fraction of a pixel too.
    return { total: items.length, shown: shown.length,
             bottom: Math.round(Math.max(...shown.map(li => li.getBoundingClientRect().bottom),
                                         visible ? button.getBoundingClientRect().bottom : -Infinity) - top),
             limit: beside.length ? Math.round(Math.max(...beside.map(held)) - top) : null,
             button: visible ? button.textContent : null,
             expanded: visible ? button.getAttribute('aria-expanded') : null };
}"""
GRID = r'class="(meta-footer-grid[^"]*)">(.*?)</section>'


def url_path(html: Path) -> str:
    rel = html.relative_to(SITE).as_posix()
    return "/" + (rel[:-len("index.html")] if rel.endswith("index.html") else rel)


def histories() -> tuple[dict[tuple, str], list[str]]:
    """One page for each shape of the footer's row that holds a version
    history (its layout and its sections), and every such page, the
    longest history first."""
    shapes: dict[tuple, str] = {}
    counted = []
    for html in sorted(SITE.rglob("*.html")):
        text = html.read_text(encoding="utf-8", errors="replace")
        row = re.search(GRID, text, re.S)
        if not row or 'id="version-history"' not in row.group(2):
            continue
        history = row.group(2)[row.group(2).index('id="version-history"'):]
        entries = history[:history.index("</ul>")].count("<li")
        shapes.setdefault((row.group(1), tuple(re.findall(SECTION_ID, row.group(2)))), url_path(html))
        counted.append((-entries, url_path(html)))
    return shapes, [path for _, path in sorted(counted)]


def history_pages(longest: int = 2) -> list[str]:
    """A page of each shape, and the longest histories."""
    shapes, by_length = histories()
    return sorted(set(shapes.values()) | set(by_length[:longest]))


def footer_shapes() -> dict[tuple[str, ...], str]:
    """One page (its URL path) for each set of sections the row holds."""
    shapes: dict[tuple[str, ...], str] = {}
    for html in sorted(SITE.rglob("*.html")):
        text = html.read_text(encoding="utf-8", errors="replace")
        row = re.search(ROW, text, re.S)
        if not row:
            continue
        shape = tuple(re.findall(SECTION_ID, row.group(1)))
        shapes.setdefault(shape, url_path(html))
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

    def measure(self, browser: str, path: str, width: int, text_size: int | None,
                then=None) -> dict:
        context = self.browsers[browser].new_context(viewport={"width": width, "height": 900})
        try:
            if text_size:
                context.add_init_script(TEXT_SIZE_INIT % text_size)
            page = context.new_page()
            page.goto(self.base + path, wait_until="load")
            page.locator(".meta-footer-grid").scroll_into_view_if_needed()
            page.wait_for_timeout(150)
            return then(page) if then else page.evaluate(MEASURE)
        finally:
            context.close()

    def test_the_history_runs_as_long_as_what_stands_beside_it(self) -> None:
        paths = history_pages()
        self.assertTrue(paths, "no page has a version history at its foot")
        folded = 0

        def states(page) -> list[dict]:
            seen = [page.evaluate(HISTORY)]
            if seen[0]["button"]:
                button = page.locator("#version-history button.version-history-more")
                button.click()
                seen.append(page.evaluate(HISTORY))
                button.click()
                seen.append(page.evaluate(HISTORY))
            return seen

        for path in paths:
            for browser in BROWSERS:
                for width in WIDTHS:
                    for text_size in TEXT_SIZES:
                        with self.subTest(page=path, browser=browser, width=width,
                                          text_size=text_size or "default"):
                            first, *clicked = self.measure(browser, path, width, text_size, states)
                            if first["limit"] is None:
                                self.assertEqual(first["shown"], first["total"], "alone on its line, folded")
                                self.assertIsNone(first["button"])
                                continue
                            if first["shown"] < first["total"]:
                                folded += 1
                                self.assertEqual((first["button"], first["expanded"]), ("More", "false"))
                                self.assertEqual(clicked[0]["shown"], clicked[0]["total"], "More shows them all")
                                self.assertEqual((clicked[0]["button"], clicked[0]["expanded"]), ("Fewer", "true"))
                                self.assertEqual(clicked[1], first, "Fewer folds them as before")
                            else:
                                self.assertIsNone(first["button"])
                            if first["shown"] > 1:
                                self.assertLessEqual(first["bottom"], first["limit"] + 2,
                                                     "the history runs past what stands beside it")
        self.assertGreater(folded, 0, "no width folds the longest history: the fitting went untested")

    def test_opened_and_narrowed_the_history_keeps_no_button(self) -> None:
        _, by_length = histories()
        self.assertTrue(by_length, "no page has a version history at its foot")
        path = by_length[0]

        def steps(page) -> list[dict]:
            seen = [page.evaluate(HISTORY)]
            button = page.locator("#version-history button.version-history-more")
            button.click()
            seen.append(page.evaluate(HISTORY))
            page.set_viewport_size({"width": 390, "height": 900})
            page.wait_for_timeout(300)
            seen.append(page.evaluate(HISTORY))
            page.set_viewport_size({"width": 1440, "height": 900})
            page.wait_for_timeout(300)
            seen.append(page.evaluate(HISTORY))
            button.click()
            seen.append(page.evaluate(HISTORY))
            return seen

        for browser in BROWSERS:
            with self.subTest(page=path, browser=browser):
                folded, opened, narrow, wide, refolded = self.measure(browser, path, 1440, None, steps)
                self.assertEqual(folded["button"], "More", "the longest history does not fold at 1440")
                self.assertEqual((opened["shown"], opened["button"]), (opened["total"], "Fewer"))
                self.assertIsNone(narrow["limit"], "at 390 the history is not alone on its line")
                self.assertEqual((narrow["shown"], narrow["button"]), (narrow["total"], None),
                                 "narrowed, nothing can fold, and a button stays")
                self.assertEqual((wide["shown"], wide["button"], wide["expanded"]),
                                 (wide["total"], "Fewer", "true"), "widened again, not open as it was")
                self.assertEqual(refolded, folded, "Fewer folds them as at first")

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
