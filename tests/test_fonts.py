"""Web fonts: preloads, fallback faces and the stacks that use them (audit V20, A19)."""

import re
import unittest

from tests._helpers import SITE_ROOT, engine_path

BASE_CSS = engine_path("static/css/base.css").read_text(encoding="utf-8")
HEAD = engine_path("templates/partials/head.html").read_text(encoding="utf-8")


def stack(token):
    m = re.search(rf"--{token}:\s*([^;]+);", BASE_CSS)
    return [f.strip().strip('"') for f in m.group(1).split(",")]


def faces(css):
    return re.findall(r"@font-face\s*\{(.*?)\}", css, re.S)


def descriptor(face, name):
    m = re.search(rf"{name}:\s*([^;]+);", face)
    return m.group(1).strip() if m else None


class FontTests(unittest.TestCase):
    def setUp(self):
        begin = BASE_CSS.index("/* BEGIN fallback faces")
        end = BASE_CSS.index("/* END fallback faces */")
        self.generated = faces(BASE_CSS[begin:end])

    def test_the_generated_block_is_not_empty(self):
        self.assertGreater(len(self.generated), 0, "run tools/subset-fonts.py")

    def test_every_fallback_face_sits_right_after_its_web_family(self):
        for token, web in (("font-serif", "Spectral"), ("font-sans", "Fira Sans")):
            families = stack(token)
            fallbacks = sorted({descriptor(f, "font-family").strip('"') for f in self.generated
                                if descriptor(f, "font-family").strip('"').startswith(web + " on ")})
            self.assertTrue(fallbacks, f"no fallback faces for {web}")
            self.assertEqual(families[0], web)
            self.assertEqual(sorted(families[1:1 + len(fallbacks)]), fallbacks,
                             f"--{token} must name {web}'s fallback faces next")

    def test_every_family_a_stack_names_with_on_is_defined(self):
        defined = {descriptor(f, "font-family").strip('"') for f in self.generated}
        for token in ("font-serif", "font-sans"):
            for family in stack(token):
                if " on " in family:
                    self.assertIn(family, defined)

    def test_fallback_faces_are_local_and_adjusted(self):
        for face in self.generated:
            self.assertTrue(descriptor(face, "src").startswith("local("), face)
            self.assertNotIn("url(", face)
            for name in ("size-adjust", "ascent-override", "descent-override", "line-gap-override"):
                self.assertRegex(descriptor(face, name) or "", r"^\d+(\.\d+)?%$", f"{name} in {face}")

    def test_preloads_name_fonts_the_stylesheet_declares(self):
        preloads = re.findall(r'<link rel="preload" href="(/fonts/[^"]+)" as="font" type="font/woff2" crossorigin>', HEAD)
        self.assertTrue(preloads)
        declared = {"/fonts/" + p for p in re.findall(r'url\("\.\./fonts/([^"]+)"\)', BASE_CSS)}
        for href in preloads:
            self.assertIn(href, declared, f"{href} is preloaded but no @font-face uses it")
            self.assertTrue((SITE_ROOT / "static" / href.lstrip("/")).is_file(), href)


if __name__ == "__main__":
    unittest.main()
