"""Web fonts: preloads, fallback faces and the stacks that use them (audit V20, A19).

The site's fonts.yaml names the families, the fallback sets, and the
stylesheet and markers engine/tools/subset-fonts.py writes the faces
between; a site without one has no web fonts to check.
"""

import re
import unittest

from tests._helpers import SITE_ROOT
from tests.target import NO_FONTS, engine_file, fonts

FONTS = fonts()


def faces(css):
    return re.findall(r"@font-face\s*\{(.*?)\}", css, re.S)


def descriptor(face, name):
    m = re.search(rf"{name}:\s*([^;]+);", face)
    return m.group(1).strip() if m else None


@unittest.skipIf(FONTS is None, NO_FONTS)
class FontTests(unittest.TestCase):
    def setUp(self):
        fallbacks = FONTS["fallbacks"]
        self.css = engine_file(fallbacks["css"]).read_text(encoding="utf-8")
        begin, end = self.css.index(fallbacks["begin"]), self.css.index(fallbacks["end"])
        self.generated = faces(self.css[begin:end])
        # The web family each set stands in for: "Spectral on Georgia" is Spectral's.
        self.web = sorted({s["family"].split(" on ")[0] for s in fallbacks["sets"]})
        self.stacks = {token: [f.strip().strip('"') for f in value.split(",")]
                       for token, value in re.findall(r"--([\w-]+):\s*([^;{}]+);", self.css)}

    def test_the_generated_block_is_not_empty(self):
        self.assertGreater(len(self.generated), 0, "run engine/tools/subset-fonts.py")

    def test_every_fallback_face_sits_right_after_its_web_family(self):
        for web in self.web:
            fallbacks = sorted({descriptor(f, "font-family").strip('"') for f in self.generated
                                if descriptor(f, "font-family").strip('"').startswith(web + " on ")})
            self.assertTrue(fallbacks, f"no fallback faces for {web}")
            stacks = {token: families for token, families in self.stacks.items() if families[0] == web}
            self.assertTrue(stacks, f"no stack starts with {web}")
            for token, families in stacks.items():
                self.assertEqual(sorted(families[1:1 + len(fallbacks)]), fallbacks,
                                 f"--{token} must name {web}'s fallback faces next")

    def test_every_family_a_stack_names_with_on_is_defined(self):
        defined = {descriptor(f, "font-family").strip('"') for f in self.generated}
        for families in self.stacks.values():
            for family in families:
                if " on " in family:
                    self.assertIn(family, defined)

    def test_fallback_faces_are_local_and_adjusted(self):
        for face in self.generated:
            self.assertTrue(descriptor(face, "src").startswith("local("), face)
            self.assertNotIn("url(", face)
            for name in ("size-adjust", "ascent-override", "descent-override", "line-gap-override"):
                self.assertRegex(descriptor(face, name) or "", r"^\d+(\.\d+)?%$", f"{name} in {face}")

    def test_preloads_name_fonts_the_stylesheet_declares(self):
        head = engine_file("templates/partials/head.html").read_text(encoding="utf-8")
        preloads = re.findall(r'<link rel="preload" href="(/fonts/[^"]+)" as="font" type="font/woff2" crossorigin>', head)
        self.assertTrue(preloads)
        declared = {"/fonts/" + p for p in re.findall(r'url\("\.\./fonts/([^"]+)"\)', self.css)}
        for href in preloads:
            self.assertIn(href, declared, f"{href} is preloaded but no @font-face uses it")
            self.assertTrue((SITE_ROOT / "static" / href.lstrip("/")).is_file(), href)


if __name__ == "__main__":
    unittest.main()
