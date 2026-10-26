"""tools/photo_naming.py, the responsive-variant names the photo tools share,
against the widths the generator offers in srcset (build/Contexts.hs,
through `site shared-rules`). A rung one side lacks is a srcset candidate
that 404s, or a variant written for nothing; and before 2026-10-04 four
tools each carried their own copy of the pattern."""

import json
import subprocess
import unittest
from pathlib import Path
from tests._helpers import load_tool, requires_cabal, site_binary


photo_naming = load_tool("photo_naming.py")


class PhotoNamingTests(unittest.TestCase):
    def test_pattern_matches_exactly_the_ladder(self) -> None:
        for width in photo_naming.WIDTHS:
            for ext in (".jpg", ".JPEG", ".png"):
                name = Path(f"a/photo.w{width}{ext}")
                with self.subTest(name=name):
                    self.assertTrue(photo_naming.is_variant(name))
                    self.assertEqual(photo_naming.variant_width(name), width)
                    self.assertEqual(photo_naming.source_for_variant(name), Path(f"a/photo{ext}"))
                    self.assertEqual(photo_naming.variant_path(Path(f"a/photo{ext}"), width), name)
        for name in ("photo.jpg", "photo.w960.webp", "photo.w720.jpg", "photo.w960.jpg.bak"):
            with self.subTest(name=name):
                self.assertFalse(photo_naming.is_variant(Path(name)))

    @requires_cabal
    def test_widths_are_the_generators(self) -> None:
        out = subprocess.run([str(site_binary()), "shared-rules"],
                             capture_output=True, text=True, check=True)
        self.assertEqual(list(photo_naming.WIDTHS),
                         json.loads(out.stdout)["photo-variant-widths"])


if __name__ == "__main__":
    unittest.main()
