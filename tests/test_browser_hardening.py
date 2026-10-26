"""Three robustness fixes, in Chromium and Firefox (audit J14).

- search-filters.js takes from localStorage only values shaped as its
  fields are; a non-array `status` used to throw at load, before the
  filter panel was wired.
- collapse.js treats a malformed fragment (#100%) as no target; it used to
  throw URIError out of load and out of every hashchange.
- selection-popup.js offers Translate only for a real language subtag, and
  escapes it; any `lang` value used to go into the button's markup as it
  was.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_hardening -v
"""

from __future__ import annotations

import json
import re
import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, check_site, enforcing_csp, require_playwright,
                            requires_browser, site_server)

FILTER_KEY = "search-filter-state-v2"
ESSAY = "/essays/proof-broker/"   # long enough to have collapsible sections

SELECTION_PAGE = """<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Selection fixture</title>
<script src="/js/utils.js"></script>
<script src="/js/selection-popup.js" defer></script>
</head><body><main id="markdownBody">
<p id="german" lang="de-AT">Ein ganz gewöhnlicher deutscher Satz.</p>
<p id="english" lang="en">An ordinary sentence in English.</p>
<p id="forged" lang='de" data-forged="1'>Noch ein Satz mit falschem Attribut.</p>
<p id="private" lang="x-klingon">A private-use language tag.</p>
</main></body></html>
"""


@requires_browser
class Hardening(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-hardening-")))
        (tmp / "fixtures").mkdir()
        (tmp / "fixtures" / "selection.html").write_text(SELECTION_PAGE, encoding="utf-8")
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp(), tmp / "fixtures"))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def page(self, browser: str, path: str, stored: str | None = None):
        """`path` loaded, with `stored` as the saved search filters; the
        page's errors are checked at cleanup."""
        context = self.browsers[browser].new_context()
        self.addCleanup(context.close)
        if stored is not None:
            context.add_init_script(
                f"try {{ localStorage.setItem({json.dumps(FILTER_KEY)}, {json.dumps(stored)}); }}"
                " catch (e) {}")
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        page.goto(self.base + path, wait_until="networkidle")
        return page, errors

    def test_search_filters_ignore_misshapen_storage(self) -> None:
        bad = {
            "status not a list": '{"status": "draft"}',
            "status of numbers": '{"status": [1, 2]}',
            "threshold a string": '{"importance": "high"}',
            "ordinal out of range": '{"scope": 99}',
            "archive mode unknown": '{"archiveMode": "everything"}',
            "not an object": '"draft"',
            "null": "null",
            "not json": "{status:",
        }
        for browser in BROWSERS:
            for case, stored in bad.items():
                with self.subTest(browser=browser, case=case):
                    page, errors = self.page(browser, "/search.html", stored)
                    self.assertEqual(errors, [])
                    # Wired: the panel's toggle opens it.
                    page.click(".library-filter-toggle")
                    self.expect(page.locator("#search-filters")).to_be_visible()
                    # Dropped, not kept unshown: the badge counts every
                    # filter in force, shown or not.
                    self.expect(page.locator("#search-filters .is-active")).to_have_count(0)
                    self.assertEqual(page.inner_text(".filter-toggle-badge").strip(), "")

    def test_search_filters_keep_a_well_formed_state(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, errors = self.page(browser, "/search.html",
                                         '{"status": ["draft"], "scope": 2, "importance": 3}')
                self.assertEqual(errors, [])
                self.expect(page.locator(".filter-status-btn.is-active")).to_have_attribute(
                    "data-value", "draft")
                self.expect(page.locator("#search-filters")).to_be_visible()
                self.assertEqual(page.inner_text(".filter-toggle-badge").strip(), "(3)")
                self.expect(page.locator(".filter-ordinal-btn.is-active")).to_have_attribute(
                    "data-field", "scope")
                self.expect(page.locator(".filter-threshold-btn.is-active")).to_have_attribute(
                    "data-field", "importance")

    def test_a_malformed_fragment_is_no_target(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser, at="load"):
                page, errors = self.page(browser, ESSAY + "#100%")
                self.assertGreater(page.locator(".section-body").count(), 0)
                self.assertEqual(errors, [])
            with self.subTest(browser=browser, at="hashchange"):
                page.evaluate("location.hash = '#50%25%'")
                page.wait_for_timeout(300)
                page.evaluate("location.hash = '#%E0%A4%A'")
                page.wait_for_timeout(300)
                self.assertEqual(errors, [])

    def select_within(self, page, selector: str) -> None:
        """Drag across the paragraph's text, as a reader selects. Not a
        triple click: Chromium's takes the line break too, and the
        selection's common ancestor becomes the paragraph's parent."""
        page.mouse.click(1, 1)
        box = page.locator(selector).bounding_box()
        y = box["y"] + box["height"] / 2
        page.mouse.move(box["x"] + 2, y)
        page.mouse.down()
        page.mouse.move(box["x"] + 150, y, steps=5)
        page.mouse.up()
        self.assertEqual(page.evaluate(
            "(n => (n.nodeType === 3 ? n.parentElement : n).id)"
            "(getSelection().getRangeAt(0).commonAncestorContainer)"), selector[1:])

    def test_translate_only_for_a_real_language(self) -> None:
        cases = {"german": "de", "english": None, "forged": None, "private": None}
        for browser in BROWSERS:
            page, errors = self.page(browser, "/__fixture/selection.html")
            for paragraph, lang in cases.items():
                with self.subTest(browser=browser, paragraph=paragraph):
                    self.select_within(page, f"#{paragraph}")
                    popup = page.locator(".selection-popup.is-visible")
                    self.expect(popup).to_be_visible()
                    translate = popup.locator("[data-action=translate]")
                    if lang is None:
                        self.expect(translate).to_have_count(0)
                    else:
                        self.expect(translate).to_have_attribute("data-lang", lang)
                    self.assertEqual(page.locator("[data-forged]").count(), 0)
            with self.subTest(browser=browser):
                self.assertEqual(errors, [])


if __name__ == "__main__":
    unittest.main()
