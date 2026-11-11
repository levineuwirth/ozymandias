"""Vendor stylesheets in the engine's layer, in Chromium and Firefox.

A wrapper in static/css/modules/ (katex.css, leaflet.css, pagefind.css)
@imports its vendor's sheets into `@layer engine`, the only way to put a
stylesheet in a layer unchanged; alone, that request would start only once
the wrapper arrived. head.html preloads each imported sheet beside the
wrapper. For each wrapper, on the first built page that links it: held
unanswered, the wrapper must not hold its vendor sheets back; answered,
each is imported and was asked for once. (transclude.js's later load of
katex.css: tests/test_browser_transclude.py.)

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_vendor_css -v
"""

from __future__ import annotations

import re
import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, SITE, check_site, enforcing_csp, imported_beside, require_playwright,
                            requires_browser, site_server)

WRAPPERS = ("katex", "leaflet", "pagefind")
IMPORT = re.compile(r'@import url\("?([^")]+)"?\)\s*layer\(engine\)')


def first_page_linking(wrapper: str) -> tuple[str, list[str]] | None:
    """The first built page (by path) that links modules/WRAPPER.css, as a
    URL path, and the sheets the wrapper imports; None if no page does."""
    link = re.compile(rf'<link rel="stylesheet" href="[^"]*assets/([0-9a-f]+)/css/modules/{wrapper}\.css">')
    for page in sorted(SITE.rglob("*.html")):
        rel = page.relative_to(SITE)
        if rel.parts[0] == "source":
            continue
        found = link.search(page.read_text(encoding="utf-8", errors="replace"))
        if found:
            css = (SITE / "assets" / found.group(1) / "css/modules" / f"{wrapper}.css").read_text(encoding="utf-8")
            path = "/" + str(rel)
            return (path.removesuffix("index.html") if path.endswith("/index.html") else path), IMPORT.findall(css)
    return None


@requires_browser
class VendorStylesheets(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import sync_playwright
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-vendor-css-")))
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp()))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def test_a_wrapper_does_not_hold_back_what_it_imports(self) -> None:
        found = {wrapper: first_page_linking(wrapper) for wrapper in WRAPPERS}
        self.assertTrue(any(found.values()), "no built page links a vendor wrapper")
        for wrapper, page in found.items():
            if page is None:
                continue   # its feature is off in this site
            path, imported = page
            self.assertTrue(imported, f"modules/{wrapper}.css imports nothing into the engine's layer")
            for browser in BROWSERS:
                with self.subTest(wrapper=wrapper, page=path, browser=browser):
                    context = self.browsers[browser].new_context()
                    self.addCleanup(context.close)
                    # The map's tiles and the like: nothing leaves serve.py.
                    context.route(re.compile(rf"^(?!{re.escape(self.base)}/)"), lambda route: route.abort())
                    imported_beside(self, context, self.base + path, f"/css/modules/{wrapper}.css", imported)


if __name__ == "__main__":
    unittest.main()
