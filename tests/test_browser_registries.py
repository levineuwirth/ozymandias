"""What a site's own script may extend, in Chromium and Firefox
(contract.yaml's scripts): it registers into the engine's registries from
a script of its own, loaded after the engine's, and changes no engine file.

A fixture page loads the engine's utils.js, popups.js and
photography-modes.js as the site serves them, then a site script
(/__fixture/site.js) that:
  · binds a popup provider for its own elements (window.ozyPopups.bind),
    which shows on hover;
  · listens for ozy:content-added, which window.ozyEnhance announces for
    content added after load, once per insertion;
  · registers a photography layout mode (window.ozyPhotography
    .registerMode), which the grid's toggle applies, entering it and
    leaving the one before.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_registries -v
"""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, check_site, enforcing_csp, require_playwright, requires_browser,
                            site_server)
from tests.target import requires_switch

PAGE = """<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Registries fixture</title>
<link rel="stylesheet" href="/css/popups.css">
<script src="/js/utils.js"></script>
<script src="/js/popups.js" defer></script>
<script src="/js/photography-modes.js" defer></script>
<script src="/__fixture/site.js" defer></script>
</head><body><main id="markdownBody">
<p><span id="mine" class="site-preview" tabindex="0">a term of the site's own</span></p>
<nav class="photography-mode-toggle">
  <a class="mode-btn" data-mode="grid" href="#">Grid</a>
  <a class="mode-btn" data-mode="strip" href="#">Strip</a>
</nav>
<div class="photography-grid"></div>
<div id="later"></div>
</main></body></html>
"""

SITE_JS = """
window.ozyPopups.bind(15, '.site-preview', function (target) {
    return Promise.resolve('<div class="from-site">The site\\u2019s own preview of ' + target.id + '.</div>');
});
window.siteAdded = [];
document.addEventListener('ozy:content-added', function (e) {
    window.siteAdded.push(e.detail.container.id);
});
window.ozyPhotography.registerMode('strip', {
    enter: function (grid) { grid.dataset.stripEntered = String(+(grid.dataset.stripEntered || 0) + 1); },
    leave: function (grid) { grid.dataset.stripLeft = '1'; }
});
"""


@requires_switch("popups")
@requires_switch("photography")
@requires_browser
class SiteRegistries(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-registries-")))
        (tmp / "fixtures").mkdir()
        (tmp / "fixtures" / "page.html").write_text(PAGE, encoding="utf-8")
        (tmp / "fixtures" / "site.js").write_text(SITE_JS, encoding="utf-8")
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp(), tmp / "fixtures"))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def open_page(self, browser: str):
        context = self.browsers[browser].new_context(viewport={"width": 1280, "height": 900})
        self.addCleanup(context.close)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        self.addCleanup(lambda: self.assertEqual(errors, [], "page errors"))
        page.goto(self.base + "/__fixture/page.html", wait_until="load")
        return page

    def test_a_site_s_popup_provider_shows(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser)
                page.hover("#mine")
                self.expect(page.locator(".link-popup.is-visible .from-site")).to_have_text(
                    "The site’s own preview of mine.")

    def test_content_added_after_load_is_announced_once(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser)
                added = page.evaluate("""() => {
                    const later = document.getElementById('later');
                    later.innerHTML = '<p>arrived</p>';
                    window.ozyEnhance(later);
                    window.ozyEnhance(later);
                    return window.siteAdded;
                }""")
                self.assertEqual(added, ["later"])

    def test_a_site_s_photography_mode_is_entered_and_left(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open_page(browser)
                grid = page.locator(".photography-grid")
                self.expect(grid).to_have_attribute("data-photography-mode", "grid")
                page.click(".mode-btn[data-mode=strip]")
                self.expect(grid).to_have_attribute("data-photography-mode", "strip")
                self.expect(grid).to_have_attribute("data-strip-entered", "1")
                self.expect(page.locator(".mode-btn[data-mode=strip]")).to_have_attribute("aria-current", "true")
                page.click(".mode-btn[data-mode=grid]")
                self.expect(grid).to_have_attribute("data-photography-mode", "grid")
                self.expect(grid).to_have_attribute("data-strip-left", "1")
                # The choice is kept, as the engine's own modes' are.
                page.click(".mode-btn[data-mode=strip]")
                page.reload(wait_until="load")
                self.expect(grid).to_have_attribute("data-photography-mode", "strip")


if __name__ == "__main__":
    unittest.main()
