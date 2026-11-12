"""A site's themes are its stylesheets' (build/Themes.hs), and nothing of the
engine's names one, so a site's own themes need no change to it.

`site themes` reads them from a scratch site's tokens.css and theme.css:
each rule for [data-theme="ID"] alone, at the top level or in an @layer, in
the order they first appear; the one beside :root is worn with none
chosen, and the first whose color-scheme is dark when the system prefers
dark. What is wrong with them fails, and names it."""

import json
import re
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import ENGINE, engine_path, requires_cabal, script_env, site_binary

TOKENS = """@layer engine, site;
@layer engine {
:root { --text-size-default: 23px; }
:root, [data-theme="light"] { color-scheme: light; --bg: #fafafa; }
/* [data-theme="commented"] { color-scheme: dark; } */
[data-theme="dark"] { color-scheme: dark; --bg: #111111; }
[data-theme="dark"] .x { color: white; }
@media print { [data-theme="printed"] { color-scheme: light; } }
}
"""
THEME = """@layer site {
:root { --bg: #fffff0; }
[data-theme='sepia'] { --bg: #704214; color-scheme: light dark; color-scheme: dark; }
[data-theme="dark"] { --bg: #000000; }
}
"""


@requires_cabal
class SiteThemes(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.binary = site_binary()

    def themes(self, tokens: str | None, theme: str | None) -> subprocess.CompletedProcess:
        with tempfile.TemporaryDirectory(prefix="site-themes-") as tmp:
            root = Path(tmp)
            css = root / "static/css"
            css.mkdir(parents=True)
            for name, text in (("tokens.css", tokens), ("theme.css", theme)):
                if text is not None:
                    (css / name).write_text(text, encoding="utf-8")
            return subprocess.run([str(self.binary), "themes"], cwd=root, env=script_env(),
                                  capture_output=True, text=True, timeout=60)

    def read(self, tokens, theme) -> dict:
        done = self.themes(tokens, theme)
        self.assertEqual(done.returncode, 0, done.stderr)
        return json.loads(done.stdout)

    def test_the_themes_their_order_and_the_two_worn_with_none_chosen(self):
        found = self.read(TOKENS, THEME)
        self.assertEqual(found["themes"], [
            {"id": "light", "label": "Light", "scheme": "light", "bg": "#fffff0"},
            {"id": "dark", "label": "Dark", "scheme": "dark", "bg": "#000000"},
            {"id": "sepia", "label": "Sepia", "scheme": "dark", "bg": "#704214"},
        ])
        self.assertEqual((found["light"], found["dark"]), ("light", "dark"))

    def test_a_site_without_the_stylesheets_has_none(self):
        self.assertEqual(self.read(None, None), {"themes": [], "light": "", "dark": None})

    def test_selector_whitespace_and_comments_do_not_change_discovery_or_copies(self):
        for selector in ('[data-theme = "night"]', "[ data-theme\t=\t'night' ]",
                         '[data-theme/**/=/**/night/**/]',
                         '[/*attr*/ data-theme /*equals*/ = /*value*/ "night" /*end*/]'):
            with self.subTest(selector=selector):
                found = self.read(TOKENS, f'{selector} {{ color-scheme: dark; }}')
                self.assertIn('night', [t['id'] for t in found['themes']])
                # The same spelling must work in descendant rules, too.
                rule = f'{selector.replace("night", "dark")} .x {{ color: white; }}'
                out = self.system_dark(rule)
                self.assertIn(':root:not([data-theme]) .x { color: white; }', out)

    def test_without_a_dark_theme_none_is_worn_for_a_dark_system(self):
        found = self.read(":root, [data-theme=\"day-light\"] { color-scheme: light; }", None)
        self.assertEqual(found["themes"][0]["label"], "Day light")
        self.assertIsNone(found["dark"])

    def test_what_is_wrong_is_named(self):
        light = ':root, [data-theme="light"] { color-scheme: light; }\n'
        for theme, message in (
            ('[data-theme="dusk"] { --bg: #222; }', "theme dusk declares no color-scheme"),
            ('[data-theme="dusk"] { color-scheme: normal; }', "color-scheme must be light or dark, not normal"),
            ('[data-theme="Dusk"] { color-scheme: dark; }', "theme ids must be lowercase"),
            ('[data-theme="system-dark"] { color-scheme: dark; }', "system- are reserved"),
            ('[data-theme="system-custom"] { color-scheme: light; }', "system- are reserved"),
            (':root, [data-theme="dusk"] { color-scheme: dark; }', "one theme may be listed beside :root"),
        ):
            with self.subTest(theme=theme):
                done = self.themes(light, theme)
                self.assertNotEqual(done.returncode, 0)
                self.assertIn(message, done.stderr)
        done = self.themes('[data-theme="dusk"] { color-scheme: dark; }', None)
        self.assertIn("no theme is listed beside :root", done.stderr)

    def system_dark(self, css: str) -> str:
        with tempfile.TemporaryDirectory(prefix="site-system-dark-") as tmp:
            root = Path(tmp)
            (root / "static/css").mkdir(parents=True)
            (root / "static/css/tokens.css").write_text(TOKENS, encoding="utf-8")
            done = subprocess.run([str(self.binary), "system-dark"], cwd=root, env=script_env(),
                                  input=css, capture_output=True, text=True, timeout=60)
        self.assertEqual(done.returncode, 0, done.stderr)
        return done.stdout

    def test_the_dark_theme_s_rules_are_copied_for_a_dark_system(self):
        css = """@layer site {
[data-theme="dark"] { --bg: #000; /* a } in a comment */ }
[data-theme="dark"]  .x, [data-theme="sepia"] .x, [data-theme="dark"]:where(.y) { color: "}"; }
[data-theme="sepia"] { --bg: #704214; }
:root:not([data-theme]) .z, [data-theme="dark"] .z { color: red; }
@media print { [data-theme="dark"] { --bg: #fff; } }
}
"""
        out = self.system_dark(css)
        # Each copy right after its rule; only the dark theme's selectors,
        # but for one already there. Conditional overrides keep their context.
        self.assertEqual(out, """@layer site {
[data-theme="dark"] { --bg: #000; /* a } in a comment */ }
@media (prefers-color-scheme: dark) {
:root:not([data-theme]) { --bg: #000; /* a } in a comment */ }}

[data-theme="dark"]  .x, [data-theme="sepia"] .x, [data-theme="dark"]:where(.y) { color: "}"; }
@media (prefers-color-scheme: dark) {
:root:not([data-theme]) .x,
:root:not([data-theme]):where(.y) { color: "}"; }}

[data-theme="sepia"] { --bg: #704214; }
:root:not([data-theme]) .z, [data-theme="dark"] .z { color: red; }
@media print { [data-theme="dark"] { --bg: #fff; }
@media (prefers-color-scheme: dark) {
:root:not([data-theme]) { --bg: #fff; }}
 }
}
""")

    def test_copies_stay_inside_nested_conditional_rules(self):
        for at in ('@media (max-width: 600px)', '@supports (display: grid)',
                   '@container (width < 600px)', '@scope (.panel)',
                   '@MEDIA(max-width: 600px)'):
            with self.subTest(at=at):
                rule = '[ data-theme = "dark" ] .x { color: white; }'
                css = f'@layer site {{ {at} {{ @supports (color: red) {{ {rule} }} }} }}'
                expected = css.replace(rule, rule + '\n@media (prefers-color-scheme: dark) {\n'
                                       ':root:not([data-theme]) .x { color: white; }}\n')
                self.assertEqual(self.system_dark(css), expected)
        # Keyframes and unknown at-rules are not style-rule groups.
        for at in ('@keyframes pulse', '@unknown'):
            css = f'{at} {{ [data-theme="dark"] {{ color: white; }} }}'
            self.assertEqual(self.system_dark(css), css)

    def test_without_a_dark_theme_nothing_is_copied(self):
        with tempfile.TemporaryDirectory(prefix="site-system-dark-") as tmp:
            done = subprocess.run([str(self.binary), "system-dark"], cwd=tmp, env=script_env(),
                                  input='[data-theme="dark"] { --bg: #000; }', capture_output=True,
                                  text=True, timeout=60)
        self.assertEqual(done.stdout, '[data-theme="dark"] { --bg: #000; }')

    def test_the_engine_s_own_are_light_and_dark(self):
        found = self.read(engine_path("static/css/tokens.css").read_text(encoding="utf-8"), "")
        self.assertEqual([(t["id"], t["scheme"]) for t in found["themes"]],
                         [("light", "light"), ("dark", "dark")])
        self.assertEqual((found["light"], found["dark"]), ("light", "dark"))


def hand_copies(css: str) -> list[str]:
    """Each rule a stylesheet writes for :root:not([data-theme]) in
    @media (prefers-color-scheme: dark): the system-dark copies the build
    makes, by hand. tests/test_site_css_tokens.py asks of a site's."""
    css = re.sub(r"/\*.*?\*/", " ", css, flags=re.S)
    return re.findall(r"@media\s*\(\s*prefers-color-scheme\s*:\s*dark\s*\)\s*\{\s*(:root:not\(\[data-theme\]\)[^{]*)", css)


# Engine files that may name themes: the default palette, the fixture
# site's own stylesheets, the tests, and what is vendored.
OWN = {"static/css/tokens.css"}
SKIPPED = ("fixture-site/", "tests/", "static/pdfjs/", "static/leaflet/", "static/katex/",
           "static/js/vendor/", "data/", "dist-newstyle/")


class NoThemeNamed(unittest.TestCase):
    """Only stylesheets name themes; scripts and templates read the
    site's (head.html's ozy:themes meta, $for(themes)$)."""

    @staticmethod
    def sources(*suffixes: str) -> list[Path]:
        out = subprocess.run(["git", "ls-files", "-z"], cwd=ENGINE, capture_output=True,
                             check=True).stdout.decode().split("\0")
        return [ENGINE / rel for rel in out
                if rel.endswith(suffixes) and not rel.startswith(SKIPPED) and rel not in OWN]

    def fixture_themes(self) -> set[str]:
        text = engine_path("fixture-site/static/css/theme.css").read_text(encoding="utf-8")
        return set(re.findall(r'\[data-theme=["\']?([a-z0-9-]+)', text))

    def test_engine_stylesheets_but_the_palette_name_no_theme(self):
        found = [f"{p.relative_to(ENGINE)}: {m}" for p in self.sources(".css")
                 for m in re.findall(r'\[data-theme\s*=[^\]]*\]', p.read_text(encoding="utf-8"))]
        self.assertEqual(found, [])

    def test_scripts_and_templates_name_no_theme(self):
        ids = self.fixture_themes() | {"light", "dark"}
        # A quoted id, or a quoted settings action for one ("theme-dark").
        quoted = re.compile(r"""['"](?:theme-)?(%s)['"]""" % "|".join(ids))
        found = [f"{p.relative_to(ENGINE)}:{n}: {line.strip()[:100]}"
                 for p in self.sources(".js", ".html")
                 for n, line in enumerate(p.read_text(encoding="utf-8").splitlines(), 1)
                 if quoted.search(line)]
        self.assertEqual(found, [])

    def test_no_theme_file_copies_the_dark_theme_by_hand(self):
        # The build writes them (Themes.systemDark); a hand copy beside it
        # would be a second place to keep in step.
        for rel in ("static/css/tokens.css", "fixture-site/static/css/theme.css"):
            with self.subTest(rel=rel):
                self.assertEqual(hand_copies(engine_path(rel).read_text(encoding="utf-8")), [])

    def test_no_engine_file_names_a_site_s_theme(self):
        mine = self.fixture_themes() - {"light", "dark"}
        self.assertTrue(mine, "the fixture site should have a theme of its own")
        found = [f"{p.relative_to(ENGINE)}: {name}" for p in self.sources(".js", ".html", ".css", ".hs", ".py", ".md", ".yaml", ".mk")
                 for name in mine if re.search(rf"\b{name}\b", p.read_text(encoding="utf-8"))]
        self.assertEqual(found, [])


if __name__ == "__main__":
    unittest.main()
