"""tools/scaffold-photos.py and tools/extract-palette.py (audit T14)."""

import contextlib
import io
import shutil
import tempfile
import unittest
from pathlib import Path
from tests._helpers import load_tool


try:
    import yaml
    scaffold = load_tool("scaffold-photos.py")
    fm = scaffold.front_matter
except ImportError:
    scaffold = None

try:
    from PIL import Image
    palette = load_tool("extract-palette.py")
except ImportError:              # Pillow or colorthief missing: no .venv
    palette = None


@unittest.skipIf(scaffold is None, "PyYAML not installed")
class ScaffoldTests(unittest.TestCase):
    def test_rendered_values_parse_back_unchanged(self):
        for value in ('Nikon "Z" 6', "back\\slash", "a: b", "København", "#hash", "2024-07-11"):
            with self.subTest(value=value):
                self.assertEqual(yaml.safe_load(fm.field("title", value)), {"title": value})

    def test_numbers_booleans_and_dates_stay_unquoted(self):
        self.assertEqual(fm.field("iso", 400), "iso: 400")
        self.assertEqual(fm.field("draft", False), "draft: false")
        self.assertEqual(fm.field("captured", "2024-07-11"), "captured: 2024-07-11")
        self.assertEqual(fm.field("date", "2024-07-11"), "date: 2024-07-11")
        # Only under a date key: a title that looks like a date is a string.
        self.assertEqual(fm.field("title", "2024-07-11"), 'title: "2024-07-11"')

    def test_tags_are_filed_under_photography(self):
        self.assertEqual(scaffold.build_tags("travel, denmark/copenhagen, ,travel"),
                         ["photography", "photography/travel", "denmark/copenhagen"])
        self.assertEqual(scaffold.build_tags(""), ["photography"])

    def test_tags_line_parses_back_whatever_the_tags_hold(self):
        tags = scaffold.build_tags("travel: denmark, «Nyhavn», [x]")
        self.assertEqual(yaml.safe_load(fm.tags_field(tags)), {"tags": tags})

    def test_a_tag_loses_its_inner_whitespace(self):
        # A tag is a URL path; import-photo.sh already did this.
        self.assertEqual(scaffold.build_tags("street art"), ["photography", "photography/streetart"])

    def test_series_landing_is_written_once(self):
        tmp = Path(tempfile.mkdtemp(prefix="scaffold-"))
        self.addCleanup(shutil.rmtree, tmp)
        landing = tmp / "index.md"
        with contextlib.redirect_stderr(io.StringIO()):
            scaffold.write_series_landing(landing, "germany-2026")
            first = landing.read_text()
            self.assertEqual(yaml.safe_load(first.split("---")[1])["title"], "Germany 2026")
            landing.write_text(first + "An edited landing.\n")
            scaffold.write_series_landing(landing, "germany-2026")
        self.assertTrue(landing.read_text().endswith("An edited landing.\n"))

    def test_title_from_slug(self):
        self.assertEqual(scaffold.title_from_slug("from-the-belt-bridge"), "From The Belt Bridge")

    def test_exif_lines_prefer_the_composed_exposure(self):
        tmp = Path(tempfile.mkdtemp(prefix="scaffold-"))
        self.addCleanup(shutil.rmtree, tmp)
        side = tmp / "a.jpg.exif.yaml"
        side.write_text(yaml.safe_dump({"captured": "2024-07-11", "camera": "X100V", "lens": "",
                                        "shutter": "1/125", "aperture": "f/8", "iso": 400,
                                        "exposure": "1/125 f/8 ISO 400"}))
        self.assertEqual(scaffold.photo_sidecars.exif_front_matter(side),
                         ["captured: 2024-07-11", 'camera: "X100V"', 'exposure: "1/125 f/8 ISO 400"'])
        side.write_text(yaml.safe_dump({"shutter": "1/60", "iso": 800}))
        self.assertEqual(scaffold.photo_sidecars.exif_front_matter(side), ['shutter: "1/60"', "iso: 800"])
        self.assertEqual(scaffold.photo_sidecars.exif_front_matter(tmp / "missing.yaml"), [])
        # A broken sidecar copies nothing, but says so instead of failing
        # silently.
        side.write_text("camera: [unclosed\n")
        err = io.StringIO()
        with contextlib.redirect_stderr(err):
            self.assertEqual(scaffold.photo_sidecars.exif_front_matter(side), [])
        self.assertIn("unreadable", err.getvalue())


@unittest.skipIf(palette is None, "Pillow or colorthief not installed")
class PaletteTests(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix="palette-"))
        self.addCleanup(shutil.rmtree, self.tmp)

    def test_the_frame_s_colors_are_found(self):
        img = Image.new("RGB", (400, 300), (200, 30, 30))
        img.paste((20, 60, 200), (0, 0, 100, 300))          # a quarter blue, the rest red
        path = self.tmp / "frame.jpg"
        img.save(path, quality=95)
        swatches = palette._extract_palette(path)
        self.assertTrue(1 <= len(swatches) <= palette.N_SWATCHES)
        rgb = [tuple(int(s[i:i + 2], 16) for i in (1, 3, 5)) for s in swatches]
        self.assertTrue(any(r > 150 and max(g, b) < 80 for r, g, b in rgb), swatches)
        self.assertTrue(any(b > 150 and r < 80 for r, g, b in rgb), swatches)

    def test_hex_and_staleness(self):
        self.assertEqual(palette._hex((255, 0, 16)), "#ff0010")
        img = self.tmp / "a.jpg"
        Image.new("RGB", (8, 8)).save(img)
        sidecars = palette.photo_sidecars
        side = sidecars.sidecar_path(img, ".palette.yaml")
        self.assertEqual(side.name, "a.jpg.palette.yaml")
        self.assertTrue(sidecars.is_stale(img, side))
        sidecars.write_yaml(side, {"palette": ["#000000"]})
        self.assertFalse(sidecars.is_stale(img, side))

    def test_run_writes_stale_sidecars_and_skips_the_rest(self):
        sidecars = palette.photo_sidecars
        for name in ("a.jpg", "a.w480.jpg", ".hidden.jpg", "b.png"):
            Image.new("RGB", (8, 8)).save(self.tmp / name, format="PNG" if name.endswith("png") else "JPEG")
        (self.tmp / "notes.txt").write_text("x")
        calls = []
        def read(image):
            calls.append(image.name)
            if image.name == "b.png":
                raise ValueError("unreadable")
            return {"z": 1, "a": 2}
        counts = sidecars.run("test", ".t.yaml", read, roots=[self.tmp],
                              image_exts={".jpg", ".png"}, argv=[], keys=("a", "z"))
        self.assertEqual(counts, {"written": 1, "skipped": 0, "failed": 1, "variants": 1})
        self.assertEqual(sorted(calls), ["a.jpg", "b.png"])
        self.assertEqual((self.tmp / "a.jpg.t.yaml").read_text(), "a: 2\nz: 1\n")
        again = sidecars.run("test", ".t.yaml", read, roots=[self.tmp],
                             image_exts={".jpg", ".png"}, argv=[])
        self.assertEqual(again["skipped"], 1)


if __name__ == "__main__":
    unittest.main()
