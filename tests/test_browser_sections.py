"""Collapsible sections, and printing them, in Chromium and Firefox.

collapse.js turns each h2/h3 of an essay into a section a reader can
close, remembered per page. A fixture page, styled as an essay, has
sections. (Sidenotes, which follow sections as they close, are
tests/test_browser_sidenotes.py.)

Checked: closing a section takes its content out of view, the tab order
and the accessibility tree, and reopening restores it; the state survives
a reload; a link into a closed section opens it, at load, on hashchange,
and when it names the fragment already in the address bar (no hashchange:
it did nothing); a closed section prints in full (audit J07: it was left off
the paper).

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_sections -v
"""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, check_site, enforcing_csp, page_styles, require_playwright,
                            requires_browser, site_server)

PAGE = """<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Sections fixture</title>
{styles}
<script src="/js/utils.js"></script>
<script src="/js/collapse.js" defer></script>
</head><body><div class="page-shell"><main id="markdownBody">
<h1 class="page-title">Sections Fixture</h1>
<p>An introduction that stays open, with <a id="to-deep" href="#deep">a link to the subsection</a>.</p>
<h2 id="first">First section</h2>
<p id="p1">The first section's text, with <a id="link1" href="#second">a link</a> in it.</p>
{filler}
<h3 id="sub">A subsection</h3>
<p id="deep">Text deep inside the first section.</p>
<h2 id="second">Second section</h2>
<p id="p2">The second section's text.</p>
<p>A closing paragraph.</p>
</main></div></body></html>
"""
FILLER = "\n".join(f"<p>Filler paragraph {i}, long enough to take a line or two of the measure "
                   f"so that closing this section moves what follows it.</p>" for i in range(12))

KEY = "section-collapsed:/__fixture/sections.html:"

@requires_browser
class Sections(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-sections-")))
        (tmp / "fixtures").mkdir()
        (tmp / "fixtures" / "sections.html").write_text(
            PAGE.format(styles=page_styles(), filler=FILLER), encoding="utf-8")
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp(), tmp / "fixtures"))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def open(self, browser: str, path: str = "/__fixture/sections.html", width: int = 1600,
             collapsed: tuple[str, ...] = (), wait: str = "load", context_setup=None):
        context = self.browsers[browser].new_context(viewport={"width": width, "height": 900})
        self.addCleanup(context.close)
        if collapsed:
            context.add_init_script("".join(
                f"try {{ localStorage.setItem('{KEY}{h}', '1'); }} catch (e) {{}}" for h in collapsed))
        if context_setup:
            context_setup(context)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        self.addCleanup(lambda: self.assertEqual(errors, [], "page errors"))
        page.goto(self.base + path, wait_until=wait)
        return page

    def body(self, page, heading: str):
        return page.locator(f"#section-body-{heading}")

    def toggle(self, page, heading: str):
        return page.locator(f"#{heading} .section-toggle")

    def close_section(self, page, heading: str) -> None:
        self.toggle(page, heading).click()
        self.expect(self.body(page, heading)).to_have_attribute("hidden", "")

    # -- sections ----------------------------------------------------------

    def test_closing_a_section(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                toggle = self.toggle(page, "first")
                self.expect(toggle).to_have_attribute("aria-label", "Toggle section: First section")
                self.expect(toggle).to_have_attribute("aria-expanded", "true")
                self.close_section(page, "first")
                self.expect(toggle).to_have_attribute("aria-expanded", "false")
                self.expect(self.body(page, "first")).to_have_attribute("inert", "")
                self.expect(page.locator("#p1")).to_be_hidden()
                # Out of the tab order: from this toggle, Tab skips the link
                # and the subsection's toggle inside the closed section.
                toggle.focus()
                page.keyboard.press("Tab")
                self.expect(self.toggle(page, "second")).to_be_focused()
                self.assertEqual(page.evaluate(f"localStorage.getItem('{KEY}first')"), "1")
                toggle.click()
                self.expect(self.body(page, "first")).not_to_have_attribute("hidden", "")
                self.expect(page.locator("#p1")).to_be_visible()
                toggle.focus()
                page.keyboard.press("Tab")
                self.expect(page.locator("#link1")).to_be_focused()
                self.assertEqual(page.evaluate(f"localStorage.getItem('{KEY}first')"), "0")

    def test_closed_state_survives_a_reload(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser, collapsed=("first",))
                self.expect(self.body(page, "first")).to_have_attribute("hidden", "")
                self.expect(self.toggle(page, "first")).to_have_attribute("aria-expanded", "false")
                self.expect(page.locator("#p2")).to_be_visible()

    def test_a_link_into_a_closed_section_opens_it(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser, at="load"):
                page = self.open(browser, path="/__fixture/sections.html#deep", collapsed=("first",))
                self.expect(page.locator("#deep")).to_be_visible()
                self.expect(self.toggle(page, "first")).to_have_attribute("aria-expanded", "true")
                self.expect(page.locator("#deep")).to_be_in_viewport()
            with self.subTest(browser=browser, at="hashchange"):
                page.evaluate("location.hash = ''")
                self.close_section(page, "first")
                page.evaluate("location.hash = '#link1'")
                self.expect(page.locator("#link1")).to_be_visible()
                self.expect(self.toggle(page, "first")).to_have_attribute("aria-expanded", "true")
            with self.subTest(browser=browser, at="the fragment already in the address bar"):
                # A link to the current fragment fires no hashchange: one to
                # a passage closed since did nothing, its target hidden.
                page = self.open(browser, path="/__fixture/sections.html#deep")
                self.close_section(page, "first")
                page.evaluate("window.scrollTo(0, 0)")
                page.click("#to-deep")
                self.assertTrue(page.url.endswith("#deep"))
                self.expect(page.locator("#deep")).to_be_visible()
                self.expect(self.toggle(page, "first")).to_have_attribute("aria-expanded", "true")
                self.expect(page.locator("#deep")).to_be_in_viewport()

    def test_a_closed_section_prints(self) -> None:
        # J07: printed, a closed section was display:none.
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser, collapsed=("first",))
                self.expect(page.locator("#p1")).to_be_hidden()
                page.emulate_media(media="print")
                self.expect(page.locator("#p1")).to_be_visible()
                self.expect(page.locator("#deep")).to_be_visible()
                self.assertGreater(self.body(page, "first").evaluate("b => b.offsetHeight"),
                                   page.locator("#p1").evaluate("p => p.offsetHeight"))
                self.expect(self.toggle(page, "second")).to_be_hidden()
                page.emulate_media(media="screen")
                # Printing changed nothing the reader chose.
                self.expect(page.locator("#p1")).to_be_hidden()
                self.assertEqual(page.evaluate(f"localStorage.getItem('{KEY}first')"), "1")


if __name__ == "__main__":
    unittest.main()
