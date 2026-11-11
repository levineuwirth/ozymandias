"""A site skin overrides structural ID selectors in both browsers.

Exercise the engine's layer declaration and actual structure CSS,
then a low-specificity site rule. Also check the empty engine skin hooks:
they must leave the default palette usable without site files.
"""
import unittest

from tests._browser import BROWSERS, require_playwright, requires_browser
from tests._helpers import engine_path


@requires_browser
class SkinLayers(unittest.TestCase):
    def test_site_rules_win_without_matching_engine_specificity(self):
        require_playwright()
        from playwright.sync_api import sync_playwright
        with sync_playwright() as p:
            for name in BROWSERS:
                with self.subTest(browser=name):
                    browser = getattr(p, name).launch()
                    try:
                        page = browser.new_page()
                        page.set_content('<html><head></head><body><main id="markdownBody"><h2>Heading</h2></main></body></html>')
                        # The skin loads before structure, as theme.css does.
                        page.add_style_tag(content=engine_path('static/css/tokens.css').read_text())
                        page.add_style_tag(content='@layer site { h2 { color: rgb(12, 34, 56); font-size: 31px; } }')
                        for sheet in ('reset', 'typography'):
                            page.add_style_tag(content=engine_path(f'static/css/{sheet}.css').read_text())
                        self.assertEqual(page.locator('h2').evaluate('(el) => getComputedStyle(el).color'), 'rgb(12, 34, 56)')
                        self.assertEqual(page.locator('h2').evaluate('(el) => getComputedStyle(el).fontSize'), '31px')
                        self.assertNotEqual(page.locator('html').evaluate('(el) => getComputedStyle(el).backgroundColor'), 'rgba(0, 0, 0, 0)')
                    finally:
                        browser.close()
