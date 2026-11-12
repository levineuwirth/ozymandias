"""Generated system-dark rules retain a skin's conditional overrides."""

import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._browser import BROWSERS, require_playwright, requires_browser
from tests._helpers import requires_cabal, script_env, site_binary

TOKENS = """@layer engine, site;
@layer engine {
  :root, [data-theme="light"] { color-scheme: light; --bg: white; }
  [data-theme="dark"] { color-scheme: dark; --bg: black; }
  body { background: var(--bg); }
  section { container-type: inline-size; }
  .card { color: rgb(90, 90, 90); }
}
"""
SKIN = """@layer site {
  [ data-theme /*name*/ = /*value*/ 'dark' ] { --bg: rgb(17, 17, 17); }
  @supports (display: grid) {
    @media (max-width: 600px) {
      [ data-theme = "dark" ] { --bg: rgb(34, 34, 34); }
    }
    @container (width < 450px) {
      [ data-theme = "dark" ] .card { color: rgb(1, 2, 3); }
    }
  }
  @supports (color: not-a-color) {
    [data-theme="dark"] { --bg: red; }
  }
  @media print {
    [data-theme="dark"] { --bg: white; color-scheme: light; }
  }
}
"""


@requires_browser
@requires_cabal
class ThemeConditions(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        require_playwright()
        from playwright.sync_api import sync_playwright
        binary = site_binary()
        root = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix='browser-themes-')))
        (root / 'static/css').mkdir(parents=True)
        (root / 'static/css/tokens.css').write_text(TOKENS)
        (root / 'static/css/theme.css').write_text(SKIN)
        cls.css = subprocess.run([str(binary), 'system-dark'], cwd=root, env=script_env(),
                                 input=TOKENS + SKIN, text=True, capture_output=True,
                                 check=True, timeout=60).stdout
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def test_system_dark_matches_explicit_dark_inside_conditions(self):
        for name in BROWSERS:
            for width, media in ((375, 'screen'), (900, 'screen'), (900, 'print')):
                with self.subTest(browser=name, width=width, media=media):
                    context = self.browsers[name].new_context(
                        color_scheme='dark', viewport={'width': width, 'height': 800})
                    self.addCleanup(context.close)
                    page = context.new_page()
                    page.emulate_media(media=media)
                    page.set_content('<style>' + self.css + '</style><section><p class="card">Text</p></section>')
                    read = """() => [getComputedStyle(document.body).backgroundColor,
                        getComputedStyle(document.querySelector('.card')).color,
                        getComputedStyle(document.documentElement).colorScheme]"""
                    system = page.evaluate(read)
                    page.evaluate('document.documentElement.dataset.theme = "dark"')
                    explicit = page.evaluate(read)
                    self.assertEqual(system, explicit)
                    self.assertEqual(explicit, [
                        'rgb(255, 255, 255)' if media == 'print' else
                        'rgb(34, 34, 34)' if width == 375 else 'rgb(17, 17, 17)',
                        'rgb(1, 2, 3)' if width == 375 else 'rgb(90, 90, 90)',
                        'light' if media == 'print' else 'dark'])
                    # The copies must not leak into the system-light view.
                    page.evaluate('document.documentElement.removeAttribute("data-theme")')
                    page.emulate_media(color_scheme='light')
                    self.assertEqual(page.evaluate(read), ['rgb(255, 255, 255)', 'rgb(90, 90, 90)', 'light'])
