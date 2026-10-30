"""Dropcaps, plain and drawn, in Chromium and Firefox, however many a page
has.

Every paragraph that drops (the opening one, directly in #markdownBody,
and each one in a ``::: dropcap`` block) has either the CSS dropcap, a
floated first letter 3.8em tall, or a drawn initial from the page's set
(Filters/Dropcaps.hs): a box from the capitals of the first line to the
baseline of the third, with the paragraph's own letter kept in the text at
no size. That holds on the site's pages with several dropcaps (routes.yaml's
dropcaps), and on
tests/golden/initials.md, rendered by the real pipeline (``site
render-fixture``): four drawn initials on one page from two sets, the first
set winning, each aligned and each with ids of its own, beside paragraphs
no set can draw, which keep the CSS dropcap. In print an initial gives way to the letter's own dropcap;
under Reduce Motion it does not animate.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_dropcaps -v
"""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, check_site, enforcing_csp, require_playwright,
                            requires_browser, site_server)
from tests._helpers import SITE_ROOT, engine_path, site_binary
from tests.target import routes

PAGES = routes().names("dropcaps")
FIXTURE = engine_path("tests/golden/initials.md")

# Each paragraph that drops, as the CSS chooses them, with how it drops.
PROBE = """async () => {
  await document.fonts.ready;
  const body = document.getElementById('markdownBody');
  const opening = [...body.children].find(e => e.tagName === 'P');
  const drops = [...new Set([opening, ...body.querySelectorAll('.dropcap p')].filter(Boolean))];
  return drops.map(p => {
    const cs = getComputedStyle(p), fs = parseFloat(cs.fontSize), lh = parseFloat(cs.lineHeight);
    const initial = p.firstElementChild && p.firstElementChild.classList.contains('initial')
                    ? p.firstElementChild : null;
    const letter = p.querySelector('.initial-letter');
    const fl = getComputedStyle(p, '::first-letter');
    const out = {text: p.textContent.trim().slice(0, 12), fs, lh,
                 float: fl.float, letterPx: parseFloat(fl.fontSize)};
    if (initial) {
      const b = initial.getBoundingClientRect(), pb = p.getBoundingClientRect();
      const r = document.createRange();
      if (letter) r.selectNodeContents(letter);
      const lb = letter ? r.getBoundingClientRect() : {width: -1, height: -1};
      out.initial = {top: b.top - pb.top, height: b.height, width: b.width,
                     letterBox: [lb.width, lb.height], hidden: initial.getAttribute('aria-hidden')};
    }
    return out;
  });
}"""


@requires_browser
class Dropcaps(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import sync_playwright
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-dropcaps-")))
        fixtures = tmp / "fixtures"
        fixtures.mkdir()
        (fixtures / "initials.html").write_text(cls.fixture_page(), encoding="utf-8")
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp(), fixtures=fixtures))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    @staticmethod
    def fixture_page() -> str:
        """The fixture's body, through the real pipeline, in a page with the
        site's stylesheets."""
        done = subprocess.run([str(site_binary()), "render-fixture", str(FIXTURE.relative_to(SITE_ROOT))],
                              cwd=SITE_ROOT, capture_output=True, text=True, check=True)
        body = done.stdout.split("<!-- render-fixture: body -->", 1)[1].split("<!-- render-fixture:", 1)[0]
        return ('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Initials</title>'
                '<link rel="stylesheet" href="/css/base.css"><link rel="stylesheet" href="/css/typography.css">'
                '<link rel="stylesheet" href="/css/print.css" media="print"></head>'
                f'<body><main id="markdownBody">{body}</main></body></html>')

    def open_page(self, browser: str, path: str, reduced: bool = False):
        context = self.browsers[browser].new_context(
            viewport={"width": 1280, "height": 900},
            reduced_motion="reduce" if reduced else "no-preference")
        self.addCleanup(context.close)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        self.addCleanup(lambda: self.assertEqual(errors, [], "page errors"))
        page.goto(self.base + path, wait_until="load")
        return page

    def assert_drops(self, drops: list[dict]) -> None:
        """Each paragraph has its dropcap, plain or drawn, and only one."""
        for d in drops:
            with self.subTest(paragraph=d["text"]):
                self.assertTrue(d["text"][:1].isalpha() or not d.get("initial"), "the letter left the text")
                if d.get("initial"):
                    i = d["initial"]
                    self.assertEqual(i["hidden"], "true")
                    # Its own letter, and any ::first-letter dropcap on it,
                    # take no room (the computed ::first-letter style is the
                    # paragraph's, so the box is what tells).
                    self.assertLessEqual(max(i["letterBox"]), 2, "the paragraph's own letter still shows")
                    self.assertGreaterEqual(min(i["letterBox"]), 0, "no .initial-letter")
                    # Capitals of line 1 to the baseline of line 3; Spectral's
                    # cap height and (asc - desc) / 2 are 0.66em and 0.298em.
                    fs, lh = d["fs"], d["lh"]
                    self.assertAlmostEqual(i["height"], 2 * lh + 0.66 * fs, delta=1)
                    self.assertAlmostEqual(i["top"], 0.5 * lh + 0.298 * fs - 0.66 * fs, delta=1.5)
                    self.assertGreater(i["width"], 0.5 * i["height"])
                else:
                    self.assertEqual(d["float"], "left")
                    self.assertAlmostEqual(d["letterPx"], 3.8 * d["fs"], delta=1)

    def test_the_site_pages_with_several_dropcaps(self) -> None:
        for browser in BROWSERS:
            for role in PAGES:
                with self.subTest(browser=browser, page=role):
                    page = self.open_page(browser, routes().path(role))
                    drops = page.evaluate(PROBE)
                    self.assertTrue(drops, "no dropcap at all")
                    self.assert_drops(drops)
                    if role == "home":
                        self.assertTrue(drops[0].get("initial"), "the home page's F is not drawn")

    def test_many_drawn_initials_on_one_page(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser, "/__fixture/initials.html")
                drops = page.evaluate(PROBE)
                self.assert_drops(drops)
                drawn = [d["text"].strip() for d in drops if d.get("initial")]
                plain = [d["text"].strip() for d in drops if not d.get("initial")]
                self.assertEqual(drawn, ["The opening", "A paragraph", "Through emph", "When the fir"])
                self.assertEqual(plain, ["“A quotation", "Zero sets ho"])
                # The first set's T, not the second's.
                self.assertNotIn("never drawn", page.content())
                ids = page.evaluate("[...document.querySelectorAll('[id]')].map(e => e.id)")
                self.assertEqual(len(ids), len(set(ids)), "an id is used twice")
                refs = page.evaluate("""[...document.querySelectorAll('.initial [fill^="url(#"]')]
                    .map(e => !!document.getElementById(e.getAttribute('fill').slice(5, -1)))""")
                self.assertTrue(refs and all(refs), "an initial's gradient is missing")
                # The paragraph after the heading is neither the opening one
                # nor in a dropcap block: it has no initial and no dropcap.
                after = page.evaluate("""(() => { const p = document.querySelector('h2 + p');
                    return [!!p.querySelector('.initial'), getComputedStyle(p, '::first-letter').float]; })()""")
                self.assertEqual(after, [False, "none"])

    def test_print_and_reduce_motion(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser, guard="print"):
                page = self.open_page(browser, "/__fixture/initials.html")
                page.emulate_media(media="print")
                state = page.evaluate("""[...document.querySelectorAll('p:has(> .initial)')].map(p => [
                    getComputedStyle(p.querySelector('.initial')).display,
                    parseFloat(getComputedStyle(p.querySelector('.initial-letter')).fontSize) > 0,
                    getComputedStyle(p, '::first-letter').float])""")
                self.assertEqual(len(state), 4)
                for display, letter_shows, float_ in state:
                    self.assertEqual(display, "none")
                    self.assertTrue(letter_shows)
                    self.assertEqual(float_, "left")
            with self.subTest(browser=browser, guard="reduce motion"):
                page = self.open_page(browser, "/__fixture/initials.html", reduced=True)
                running = page.evaluate("""document.getAnimations()
                    .filter(a => a.effect && a.effect.target && a.effect.target.closest('.initial')).length""")
                self.assertEqual(running, 0)
                hidden = page.evaluate("""[...document.querySelectorAll('.initial .initial-ink')]
                    .filter(e => getComputedStyle(e).opacity !== '1').length""")
                self.assertEqual(hidden, 0, "an initial's ink waits for an animation that never runs")


if __name__ == "__main__":
    unittest.main()
