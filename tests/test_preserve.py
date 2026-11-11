"""Phase B's preservation harness compares what it should, and only that.

tools/browser/preserve.py records a built site's computed styles and
screenshots, and compares two records (its own browser test drives a real
browser). These build records by hand: a difference in one element's
property, a pseudo-element that appears, elements that differ (the bodies
did), a screenshot's pixels, a file only one side has; and nothing for
records that are the same, or screenshots whose pixels are."""

import asyncio
import gzip
import json
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from tests._helpers import engine_path, load_tool

preserve = load_tool("browser/preserve.py", "preserve")

PROPS = ["color", "display", "content"]


def record(root: Path, rel: str, styles: list, elements: list) -> None:
    path = root / "styles" / rel
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(gzip.compress(json.dumps({"props": PROPS, "styles": styles,
                                               "elements": elements}).encode()))


def shot(root: Path, rel: str, pixel=(0, 0, 0), metadata: str = "") -> None:
    from PIL import Image, PngImagePlugin
    path = root / "shots" / rel
    path.parent.mkdir(parents=True, exist_ok=True)
    image = Image.new("RGB", (8, 8), (255, 255, 255))
    image.putpixel((3, 4), pixel)
    info = PngImagePlugin.PngInfo()
    if metadata:
        info.add_text("note", metadata)
    image.save(path, pnginfo=info)


BLACK, RED, NORMAL = ["rgb(0, 0, 0)", "block", "normal"], ["rgb(255, 0, 0)", "block", "normal"], None


class Compare(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory(prefix="preserve-")
        self.addCleanup(tmp.cleanup)
        self.base, self.new = Path(tmp.name) / "base", Path(tmp.name) / "new"
        self.report = Path(tmp.name) / "report"
        # Independent, complete one-page recordings; omit a file only in tests
        # that deliberately damage coverage. Do not derive these from inventory().
        meta = {"format": preserve.FORMAT, "complete": True, "browser": "chromium test",
                "moment": 1, "volatile": [".leaflet-container"],
                "themes": ["light", "dark", "cappuccino"], "sizes": [17, 23, 29],
                "widths": [320, 768, 1280, 1600], "style_width": 1280,
                "height": 900, "shot_height": 6000, "properties": PROPS,
                "shots": True, "pages": {"home": "/"}, "season": None, "init": None}
        for root in (self.base, self.new):
            root.mkdir()
            (root / "meta.json").write_text(json.dumps(meta))
            for theme in ("light", "dark", "cappuccino"):
                for size in (17, 23, 29):
                    record(root, f"home/{theme}-{size}.json.gz", [BLACK], [["html", 0, None, None]])
                for width in (320, 768, 1280, 1600, "print"):
                    record(root, f"home/{theme}-view-{width}.json.gz", [BLACK], [["html", 0, None, None]])
                    shot(root, f"home/{theme}-{width}.png")

    def compare(self) -> tuple[int, str]:
        code = preserve.compare(self.base, self.new, self.report)
        return code, (self.report / "differences.txt").read_text()

    def both(self, styles, elements):
        for root in (self.base, self.new):
            record(root, "home/light-23.json.gz", styles, elements)

    def test_the_same_records_compare_equal(self):
        self.both([BLACK], [["html", 0, None, None], ["html>body:1", 0, None, None]])
        shot(self.base, "home/light-320.png")
        shot(self.new, "home/light-320.png", metadata="written again")
        self.assertEqual(self.compare(), (0, "\n"))

    def test_a_property_that_changed_is_named(self):
        record(self.base, "home/dark-29.json.gz", [BLACK, RED], [["html", 0, None, None], ["html>body:1>p:2", 0, None, None]])
        record(self.new, "home/dark-29.json.gz", [BLACK, RED], [["html", 0, None, None], ["html>body:1>p:2", 1, None, None]])
        code, text = self.compare()
        self.assertEqual(code, 1)
        self.assertIn("home/dark-29.json.gz: html>body:1>p:2 color: 'rgb(0, 0, 0)' -> 'rgb(255, 0, 0)'", text)
        self.assertNotIn("display", text)

    def test_a_pseudo_element_that_appears_or_changes_is_named(self):
        icon = ["rgb(0, 0, 0)", "inline-block", '""']
        wider = ["rgb(0, 0, 0)", "inline-block", '"x"']
        record(self.base, "home/light-23.json.gz", [BLACK, icon], [["html>body:1>a:1", 0, None, 1], ["html>body:1>a:2", 0, None, 1]])
        record(self.new, "home/light-23.json.gz", [BLACK, icon, wider], [["html>body:1>a:1", 0, 1, 1], ["html>body:1>a:2", 0, None, 2]])
        code, text = self.compare()
        self.assertEqual(code, 1)
        self.assertIn("html>body:1>a:1::before appeared", text)
        self.assertIn("""html>body:1>a:2::after content: '""' -> '"x"'""", text)

    def test_elements_that_differ_say_the_bodies_changed(self):
        record(self.base, "home/light-23.json.gz", [BLACK], [["html", 0, None, None], ["html>body:1", 0, None, None]])
        record(self.new, "home/light-23.json.gz", [BLACK], [["html", 0, None, None]])
        code, text = self.compare()
        self.assertEqual(code, 1)
        self.assertIn("the elements differ (1 gone, 0 added; first html>body:1)", text)

    def test_a_screenshot_whose_pixels_differ_is_named_and_shown(self):
        shot(self.base, "home/dark-print.png")
        shot(self.new, "home/dark-print.png", pixel=(0, 0, preserve.TOLERANCE + 1))
        code, text = self.compare()
        self.assertEqual(code, 1)
        self.assertIn("shots/home/dark-print.png: pixels differ in (3, 4, 4, 5)", text)
        self.assertTrue((self.report / "shots/home/dark-print.png").is_file())

    def test_anti_aliasing_s_faint_noise_is_no_difference_but_exactly(self):
        shot(self.base, "home/light-320.png")
        shot(self.new, "home/light-320.png", pixel=(preserve.TOLERANCE, 0, 0))
        self.assertEqual(self.compare(), (0, "\n"))
        self.assertEqual(preserve.compare(self.base, self.new, None, 0), 1)

    def test_a_file_one_side_lacks_is_named(self):
        self.both([BLACK], [["html", 0, None, None]])
        (self.base / "shots/home/light-1600.png").unlink()
        code, text = self.compare()
        self.assertEqual(code, 1)
        self.assertIn("base: missing shots/home/light-1600.png", text)

    def test_nonexistent_and_empty_directories_are_not_snapshots(self):
        for name in ("missing", "empty"):
            with self.subTest(name=name):
                directory = self.report.parent / name
                if name == "empty":
                    directory.mkdir()
                self.assertEqual(preserve.compare(directory, directory, None), 1)

    def test_missing_or_unfinished_manifest_is_rejected(self):
        path = self.base / "meta.json"
        original = path.read_text()
        for contents in (None, "{", "{}", original.replace('"complete": true', '"complete": false')):
            with self.subTest(contents=contents):
                if contents is None:
                    path.unlink()
                else:
                    path.write_text(contents)
                code, text = self.compare()
                self.assertEqual(code, 1)
                self.assertIn("base: invalid snapshot", text)

    def test_matching_holes_in_both_snapshots_are_rejected(self):
        # The old comparison accepted these because both file sets matched.
        for root in (self.base, self.new):
            (root / "styles/home/light-view-print.json.gz").unlink()
            (root / "shots/home/dark-320.png").unlink()
        code, text = self.compare()
        self.assertEqual(code, 1)
        for side in ("base", "new"):
            self.assertIn(f"{side}: missing styles/home/light-view-print.json.gz", text)
            self.assertIn(f"{side}: missing shots/home/dark-320.png", text)

    def test_stale_files_are_rejected_even_when_both_have_them(self):
        for root in (self.base, self.new):
            shot(root, "old-route/light-320.png")
        code, text = self.compare()
        self.assertEqual(code, 1)
        self.assertIn("unexpected shots/old-route/light-320.png", text)

    def test_recording_settings_must_match(self):
        path = self.new / "meta.json"
        original = json.loads(path.read_text())
        for key, value in {"browser": "firefox test", "moment": 2, "height": 1000,
                           "style_width": 1600, "shot_height": 9000, "volatile": [],
                           "pages": {"home": "/other"}, "properties": PROPS + ["width"]}.items():
            with self.subTest(key=key):
                path.write_text(json.dumps({**original, key: value}))
                code, text = self.compare()
                self.assertEqual(code, 1)
                self.assertIn(f"incompatible recording setting: {key}", text)

    def test_styles_only_is_explicit_and_cannot_match_a_full_snapshot(self):
        for root in (self.base, self.new):
            meta = json.loads((root / "meta.json").read_text())
            meta["shots"] = False
            (root / "meta.json").write_text(json.dumps(meta))
            for path in (root / "shots").rglob("*.png"):
                path.unlink()
            for path in (root / "styles").rglob("*-view-*.json.gz"):
                path.unlink()
        self.assertEqual(self.compare(), (0, "\n"))
        meta["shots"] = True
        (self.new / "meta.json").write_text(json.dumps(meta))
        code, text = self.compare()
        self.assertEqual(code, 1)
        self.assertIn("incompatible recording setting: shots", text)

    def test_empty_or_incompatible_style_payloads_cannot_compare_equal(self):
        for props, styles, elements in ((PROPS, [], []),
                                        (PROPS + ["width"], [BLACK], [["html", 0, None, None]])):
            for root in (self.base, self.new):
                path = root / "styles/home/light-23.json.gz"
                path.write_bytes(gzip.compress(json.dumps({"props": props, "styles": styles,
                                                          "elements": elements}).encode()))
            self.assertEqual(self.compare()[0], 1)

    def test_failed_snapshot_cannot_be_compared_or_reused(self):
        output = self.report.parent / "interrupted"
        with patch.object(preserve, "text_sizes", return_value=(17, 23, 29)), \
             patch.object(preserve, "off_season", return_value=1), \
             patch.object(preserve, "serve", side_effect=RuntimeError("interrupted")) as server:
            with self.assertRaisesRegex(RuntimeError, "interrupted"):
                asyncio.run(preserve.snapshot(output, self.base, {"home": "/"}, 1, "chromium", True))
            self.assertTrue(output.is_dir())
            self.assertFalse((output / "meta.json").exists())
            self.assertEqual(preserve.compare(output, output, None), 1)
            with self.assertRaisesRegex(SystemExit, "already exists"):
                asyncio.run(preserve.snapshot(output, self.base, {"home": "/"}, 1, "chromium", True))
            self.assertEqual(server.call_count, 1)

    def test_completed_snapshot_cannot_be_overwritten(self):
        before = {p: p.read_bytes() for p in self.base.rglob("*") if p.is_file()}
        with patch.object(preserve, "text_sizes", return_value=(17, 23, 29)), \
             patch.object(preserve, "off_season", return_value=1), \
             patch.object(preserve, "serve") as server:
            with self.assertRaisesRegex(SystemExit, "already exists"):
                asyncio.run(preserve.snapshot(self.base, self.base, {"home": "/"}, 1, "chromium", True))
            server.assert_not_called()
        self.assertEqual(before, {p: p.read_bytes() for p in self.base.rglob("*") if p.is_file()})


class Settings(unittest.TestCase):
    def test_the_text_sizes_are_utils_js_s(self):
        low, base, high = preserve.text_sizes(engine_path(""))
        self.assertTrue(low < base < high, (low, base, high))

    def test_the_clock_stands_on_a_day_no_season_dresses(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "site.yaml").write_text(json.dumps({"seasons": [
                {"id": "winter", "from": "12-01", "to": "01-10"},
                {"id": "spring", "from": "01-11", "to": "01-12"}]}))
            moment = preserve.off_season(root)
            import datetime
            day = datetime.datetime.fromtimestamp(moment / 1000, datetime.timezone.utc).date()
            self.assertEqual(day, datetime.date(2026, 1, 13))
            (root / "site.yaml").write_text("{}")
            self.assertEqual(preserve.off_season(root) % 86_400_000, 43_200_000)


if __name__ == "__main__":
    unittest.main()
