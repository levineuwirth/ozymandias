"""The home page's shelf of portals (switch portal-shelf; portal-shelf.js,
css/portal-shelf.css), in Chromium and Firefox, on the site's built home
page under its enforcing policy:

  · pointing at a spine draws it out and turns the caption to the portal:
    its name, and the gloss or facts the spine carries; on a map the
    portal is drawn on, its region is lit; a portal with a cover shows the
    cover in the map's place; leaving the shelf turns it back to the map;
  · keyboard focus does what pointing does, and tabbing on from the last
    spine reaches the caption naming the map, the keyboard's way to /map/;
    a pointer that leaves gives the shelf back to the focused spine;
  · a cover that fails to load leaves the map, not the last portal's cover;
    from one cover to the next the map does not show between, and a cover
    stands in the map's box, moving nothing below it;
  · on a phone (375, 360 and 320px) every spine stands on one shelf, inside the
    window, the spines' centres 24px apart (target size), and the page
    does not scroll sideways;
  · with the map off, a spine is a link the stylesheet draws off the shelf.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_portal_shelf -v
"""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, check_site, enforcing_csp, require_playwright,
                            requires_browser, site_server)
from tests.target import requires_switch, switch_on

STATE = """() => {
    const shelf = document.querySelector('[data-portal-shelf]');
    const svg = shelf.querySelector('svg.tag-map');
    const cover = shelf.querySelector('.portal-shelf-cover');
    return {
        title: shelf.querySelector('.portal-shelf-caption-title')?.textContent,
        meta: shelf.querySelector('.portal-shelf-caption-meta')?.textContent,
        out: [...shelf.querySelectorAll('.portal-spine.is-out')].map(s => s.dataset.tag),
        lit: svg ? [...svg.querySelectorAll('.tag-map-tag.is-hover-lit')].map(t => t.dataset.tag) : [],
        hover: svg ? svg.classList.contains('tag-map--hover') : false,
        cover: cover ? !cover.hidden && cover.getBoundingClientRect().height > 0 : false,
        map: svg ? svg.getBoundingClientRect().height > 0 && getComputedStyle(svg).visibility !== 'hidden' : false,
    };
}"""

SPINES = """() => {
    const svg = document.querySelector('[data-portal-shelf] svg.tag-map');
    return [...document.querySelectorAll('.portal-spine')].map(s => ({
        tag: s.dataset.tag, caption: s.dataset.caption || '', cover: s.dataset.cover || null,
        name: s.querySelector('.portal-spine-title').textContent,
        onMap: !!(svg && svg.querySelector('.tag-map-tag[data-tag="' + CSS.escape(s.dataset.tag) + '"]')),
    }));
}"""


@requires_browser
@requires_switch("portal-shelf")
class PortalShelf(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import sync_playwright
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-shelf-")))
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp()))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def open(self, browser: str, **context):
        ctx = self.browsers[browser].new_context(**context)
        self.addCleanup(ctx.close)
        page = ctx.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        page.goto(self.base + "/")
        page.wait_for_load_state("load")
        return page, errors

    def test_pointing_turns_the_shelf_to_a_portal(self):
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, errors = self.open(browser, viewport={"width": 1440, "height": 1000})
                spines = page.evaluate(SPINES)
                self.assertTrue(spines)
                rest = page.evaluate(STATE)
                if rest["title"] is None:
                    # No map (tag-map off): the spines are links, drawn off
                    # the shelf by the stylesheet alone.
                    first = f'.portal-spine[data-tag="{spines[0]["tag"]}"]'
                    page.hover(first)
                    page.wait_for_timeout(400)
                    self.assertNotEqual(page.eval_on_selector(first, "s => getComputedStyle(s).transform"), "none")
                    self.assertTrue(page.eval_on_selector(first, "s => s.href.length > 0"))
                    continue
                for spine in spines:
                    page.hover(f'.portal-spine[data-tag="{spine["tag"]}"]')
                    page.wait_for_timeout(150 if not spine["cover"] else 600)
                    state = page.evaluate(STATE)
                    self.assertEqual(state["out"], [spine["tag"]], spine["tag"])
                    self.assertEqual((state["title"], state["meta"]), (spine["name"], spine["caption"]))
                    if spine["onMap"]:
                        self.assertTrue(state["hover"] and spine["tag"] in state["lit"], spine["tag"])
                        self.assertTrue(state["map"] and not state["cover"])
                    elif spine["cover"]:
                        self.assertTrue(state["cover"] and not state["map"], spine["tag"])
                        self.assertFalse(state["hover"])
                    else:
                        self.assertTrue(state["map"] and not state["hover"], spine["tag"])
                page.mouse.move(2, 2)
                page.wait_for_timeout(150)
                back = page.evaluate(STATE)
                self.assertEqual((back["title"], back["meta"], back["out"], back["hover"], back["cover"]),
                                 (rest["title"], rest["meta"], [], False, False))
                self.assertEqual(errors, [])

    @requires_switch("tag-map")
    def test_focus_does_what_pointing_does(self):
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, errors = self.open(browser, viewport={"width": 1440, "height": 1000})
                rest = page.evaluate(STATE)
                map_href = page.get_attribute(".portal-shelf-caption-title", "href")
                spines = page.evaluate(SPINES)
                spine = next((s for s in spines if s["onMap"]), None)
                if spine is None:
                    self.skipTest("no portal on the map")
                page.focus(f'.portal-spine[data-tag="{spine["tag"]}"]')
                state = page.evaluate(STATE)
                self.assertEqual(state["out"], [spine["tag"]])
                self.assertIn(spine["tag"], state["lit"])
                self.assertEqual(state["title"], spine["name"])
                # Tab on from the last spine: the caption is the keyboard's
                # way to the map, and names it.
                page.focus(f'.portal-spine[data-tag="{spines[-1]["tag"]}"]')
                page.keyboard.press("Tab")
                focused = page.evaluate("""() => ({cls: document.activeElement.className,
                    href: document.activeElement.getAttribute('href'), text: document.activeElement.textContent})""")
                self.assertEqual(focused["cls"], "portal-shelf-caption-title")
                self.assertEqual((focused["text"], focused["href"]), (rest["title"], map_href))
                self.assertRegex(map_href, r"(^|/)map/$")
                self.assertEqual(page.evaluate(STATE)["out"], [])
                page.evaluate("document.activeElement.blur()")
                self.assertEqual(page.evaluate(STATE)["out"], [])
                self.assertEqual(errors, [])

    @requires_switch("tag-map")
    def test_the_pointer_leaving_gives_the_shelf_back_to_focus(self):
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, errors = self.open(browser, viewport={"width": 1440, "height": 1000})
                spines = page.evaluate(SPINES)
                if len(spines) < 2:
                    self.skipTest("one portal")
                focused, pointed = spines[0], spines[-1]
                page.focus(f'.portal-spine[data-tag="{focused["tag"]}"]')
                page.hover(f'.portal-spine[data-tag="{pointed["tag"]}"]')
                page.wait_for_timeout(150)
                self.assertEqual(page.evaluate(STATE)["title"], pointed["name"])
                page.mouse.move(2, 2)
                page.wait_for_timeout(150)
                state = page.evaluate(STATE)
                self.assertEqual((state["title"], state["meta"], state["out"]),
                                 (focused["name"], focused["caption"], [focused["tag"]]))
                self.assertEqual(errors, [])

    @requires_switch("tag-map")
    def test_cover_to_cover_turns_at_once_in_one_box(self):
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, errors = self.open(browser, viewport={"width": 1440, "height": 1000})
                covered = [s for s in page.evaluate(SPINES) if s["cover"] and not s["onMap"]]
                if len(covered) < 2:
                    self.skipTest("fewer than two portals with covers")
                below = "() => document.querySelector('.portal-shelf-caption').getBoundingClientRect().top + scrollY"
                at_rest = page.evaluate(below)
                page.hover(f'.portal-spine[data-tag="{covered[0]["tag"]}"]')
                page.wait_for_load_state("networkidle")       # the covers, loaded on reaching the shelf
                page.wait_for_function("() => !document.querySelector('.portal-shelf-cover').hidden")
                self.assertEqual(page.evaluate(below), at_rest, "a cover moved what is below the map")
                # Every state the stage passes through on the way to the next cover.
                page.evaluate("""() => { window.__stage = [];
                    const stage = document.querySelector('.portal-shelf-stage');
                    new MutationObserver(() => window.__stage.push(stage.classList.contains('is-cover')))
                        .observe(stage, {attributes: true, subtree: true}); }""")
                page.hover(f'.portal-spine[data-tag="{covered[1]["tag"]}"]')
                page.wait_for_timeout(300)
                self.assertNotIn(False, page.evaluate("window.__stage"), "the map showed between two covers")
                state = page.evaluate(STATE)
                self.assertEqual(state["title"], covered[1]["name"])
                self.assertTrue(state["cover"])
                self.assertEqual(page.evaluate(below), at_rest)
                self.assertEqual(errors, [])

    @requires_switch("tag-map")
    def test_a_cover_that_fails_leaves_the_map(self):
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, errors = self.open(browser, viewport={"width": 1440, "height": 1000})
                covered = [s for s in page.evaluate(SPINES) if s["cover"] and not s["onMap"]]
                if len(covered) < 2:
                    self.skipTest("fewer than two portals with covers")
                first, second = covered[0], covered[1]
                # Blocked before the shelf is reached, when the covers load.
                page.route("**" + second["cover"], lambda route: route.abort())
                page.hover(f'.portal-spine[data-tag="{first["tag"]}"]')
                page.wait_for_function("() => !document.querySelector('.portal-shelf-cover').hidden")
                page.hover(f'.portal-spine[data-tag="{second["tag"]}"]')
                page.wait_for_timeout(600)
                state = page.evaluate(STATE)
                self.assertEqual(state["title"], second["name"])
                self.assertTrue(state["map"] and not state["cover"], "another portal's cover stayed")
                self.assertEqual(errors, [])

    def test_a_phone_holds_every_spine_on_one_shelf(self):
        for browser in BROWSERS:
            for width in (375, 360, 320):
                with self.subTest(browser=browser, width=width):
                    options = {"viewport": {"width": width, "height": 800}, "has_touch": True}
                    if browser == "chromium":
                        options["is_mobile"] = True
                    page, errors = self.open(browser, **options)
                    boxes = page.evaluate("""() => [...document.querySelectorAll('.portal-spine')]
                        .map(s => s.getBoundingClientRect()).map(r => [r.left, r.right, r.bottom, r.width])""")
                    self.assertTrue(boxes)
                    self.assertEqual(len({round(b[2]) for b in boxes}), 1, "the spines stand on more than one shelf")
                    self.assertGreaterEqual(min(b[0] for b in boxes), 0)
                    self.assertLessEqual(max(b[1] for b in boxes), width)
                    # WCAG 2.2 SC 2.5.8: a target 24px wide, or one whose
                    # 24px circle meets no other: centres 24px apart.
                    centres = sorted((b[0] + b[1]) / 2 for b in boxes)
                    for a, b in zip(centres, centres[1:]):
                        self.assertGreaterEqual(b - a, 24, "spines too close to tell apart by touch")
                    self.assertEqual(page.evaluate("document.documentElement.scrollWidth - innerWidth"), 0)
                    self.assertEqual(errors, [])


if __name__ == "__main__":
    unittest.main()
