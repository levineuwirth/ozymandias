"""Every stylesheet is wholly in its layer.

The engine's rules are in @layer engine and a site's in @layer site, which
wins whatever the order (head.html). A sheet left unlayered would beat both,
whatever its specificity: the season stylesheets did, in B1's first pass.
Here: each of the engine's stylesheets, and the fixture site's, is one
layer block (after its comments), or an @import into the engine's layer,
or no rule at all (a skin hook's default). A site checks its own with
check(root, "site")."""

import re
import unittest
from pathlib import Path

from tests._helpers import engine_path

COMMENT = re.compile(r"/\*.*?\*/", re.S)


def layered(text: str, layer: str) -> bool:
    """Whether the sheet's rules are all in `layer`: an optional
    `@layer a, b;` statement, then one `@layer <layer> { … }` block ending the
    file, or only imports into that layer, or nothing."""
    rest = COMMENT.sub("", text).strip()
    rest = re.sub(r"^@layer\s+[\w\s,-]+;\s*", "", rest)
    if not rest:
        return True
    if all(re.fullmatch(rf'@import\s+url\("?[^")]+"?\)\s+layer\({layer}\);', line.strip())
           for line in rest.splitlines() if line.strip()):
        return True
    opening = re.match(rf"@layer\s+{layer}\s*\{{", rest)
    if not opening:
        return False
    depth, quote = 0, None
    for i, ch in enumerate(rest):
        if quote:
            if ch == quote:
                quote = None
        elif ch in "\"'":
            quote = ch
        elif ch == "{":
            depth += 1
        elif ch == "}":
            depth -= 1
            if depth == 0:
                return i == len(rest) - 1
    return False


def unlayered(root: Path, layer: str) -> list[str]:
    return sorted(str(p.relative_to(root)) for p in (root / "static/css").rglob("*.css")
                  if not layered(p.read_text(encoding="utf-8"), layer))


class Layers(unittest.TestCase):
    def test_the_engine_s_stylesheets_are_in_its_layer(self):
        self.assertEqual(unlayered(engine_path(""), "engine"), [])

    def test_the_fixture_site_s_stylesheets_are_in_the_site_s(self):
        self.assertEqual(unlayered(engine_path("fixture-site"), "site"), [])

    def test_what_counts_as_layered(self):
        for text, layer, verdict in (
                ("/* hook */\n", "site", True),
                ("@layer site {\na { color: red; }\n}\n", "site", True),
                ("@layer engine, site;\n@layer engine {\n:root { --x: 1; }\n}\n", "engine", True),
                ('@import url("/k/k.css") layer(engine);\n', "engine", True),
                ("a { color: red; }\n", "site", False),
                ("@layer site {\na { color: red; }\n}\nb { color: blue; }\n", "site", False),
                ("@layer engine {\na { content: \"}\"; }\n}\n", "engine", True),
                ("@layer site {\na {}\n}\n", "engine", False)):
            with self.subTest(text=text, layer=layer):
                self.assertEqual(layered(text, layer), verdict)


if __name__ == "__main__":
    unittest.main()
