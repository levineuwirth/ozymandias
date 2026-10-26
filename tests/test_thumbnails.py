"""Tests for tools/generate-thumbnails.py — the responsive variant ladder.

The contract under test (see the module docstring and PHOTOGRAPHY.md):
`<name>.<ext>` yields `<name>.w480.<ext>` / `.w960.` / `.w1440.` in the
same directory, each only when the source is strictly wider; reruns
rewrite nothing; variants are never themselves resized.

Skips wholesale when Pillow is absent, like tests/test_viz.py does for
matplotlib — the generator itself degrades to a no-op in that case, so
there is nothing to assert.
"""

from __future__ import annotations

import contextlib
import io
import os
import tempfile
import unittest
from pathlib import Path
from tests._helpers import load_tool


generate_thumbnails = load_tool("generate-thumbnails.py", register=True)

try:
    from PIL import Image

    HAVE_PILLOW = True
except ImportError:  # pragma: no cover — exercised only on a bare machine
    HAVE_PILLOW = False


# Fixed, distinct mtimes so staleness assertions never depend on how
# fast the test ran or on filesystem timestamp granularity.
OLD = 1_600_000_000
NEWER = OLD + 100


def run(*argv: str) -> tuple[int, str]:
    """Invoke the generator, capturing its stdout."""
    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        code = generate_thumbnails.main(list(argv))
    return code, buf.getvalue()


@unittest.skipUnless(HAVE_PILLOW, "Pillow not installed")
class ThumbnailContractTests(unittest.TestCase):
    def setUp(self) -> None:
        self._tmp = tempfile.TemporaryDirectory()
        self.root = Path(self._tmp.name)
        self.addCleanup(self._tmp.cleanup)

    # -- fixtures ---------------------------------------------------------

    def write_jpeg(self, name: str, size: tuple[int, int]) -> Path:
        path = self.root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        # A gradient rather than flat colour: a flat image survives any
        # resampling filter, so it would not distinguish a working
        # resize from a broken one.
        image = Image.new("RGB", size)
        image.putdata(
            [
                (x * 255 // max(1, size[0] - 1), y * 255 // max(1, size[1] - 1), 128)
                for y in range(size[1])
                for x in range(size[0])
            ]
        )
        image.save(path, format="JPEG", quality=90)
        os.utime(path, (OLD, OLD))
        return path

    def write_png_with_alpha(self, name: str, size: tuple[int, int]) -> Path:
        path = self.root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        image = Image.new("RGBA", size, (10, 120, 200, 255))
        # Punch a fully transparent block in the top-left quadrant, big
        # enough that it survives the downscale to 480px.
        for y in range(size[1] // 2):
            for x in range(size[0] // 2):
                image.putpixel((x, y), (0, 0, 0, 0))
        image.save(path, format="PNG")
        os.utime(path, (OLD, OLD))
        return path

    def variants(self) -> list[Path]:
        return sorted(
            p for p in self.root.rglob("*") if generate_thumbnails.is_variant(p)
        )

    # -- the ladder -------------------------------------------------------

    def test_wide_source_gets_all_three_widths(self) -> None:
        self.write_jpeg("series/wide.jpg", (2400, 1600))

        code, _ = run(str(self.root))

        self.assertEqual(code, 0)
        self.assertEqual(
            [p.name for p in self.variants()],
            ["wide.w1440.jpg", "wide.w480.jpg", "wide.w960.jpg"],
        )
        for width, height in ((480, 320), (960, 640), (1440, 960)):
            with Image.open(self.root / "series" / f"wide.w{width}.jpg") as img:
                self.assertEqual(
                    img.size,
                    (width, height),
                    f"w{width} must be {width}x{height} (3:2 preserved)",
                )

    def test_each_variant_gets_a_webp_sibling_from_the_source(self) -> None:
        self.write_jpeg("series/wide.jpg", (2400, 1600))
        run(str(self.root))
        for width, height in ((480, 320), (960, 640), (1440, 960)):
            jpg = self.root / "series" / f"wide.w{width}.jpg"
            webp = self.root / "series" / f"wide.w{width}.webp"
            with Image.open(webp) as img:
                self.assertEqual((img.format, img.size), ("WEBP", (width, height)))
            # Never older than its JPEG, or convert-images.sh re-encodes it.
            self.assertGreaterEqual(webp.stat().st_mtime_ns, jpg.stat().st_mtime_ns)

    def test_a_missing_webp_is_written_without_rewriting_the_jpeg(self) -> None:
        self.write_jpeg("series/wide.jpg", (2400, 1600))
        run(str(self.root))
        jpg = self.root / "series" / "wide.w960.jpg"
        os.utime(jpg, (NEWER, NEWER))
        (self.root / "series" / "wide.w960.webp").unlink()

        run(str(self.root))

        self.assertTrue((self.root / "series" / "wide.w960.webp").exists())
        self.assertEqual(jpg.stat().st_mtime, NEWER, "the current JPEG was rewritten")

    def test_png_variants_get_lossless_webp_with_alpha(self) -> None:
        self.write_png_with_alpha("series/art.png", (1000, 800))
        run(str(self.root))
        with Image.open(self.root / "series" / "art.w480.webp") as img:
            img = img.convert("RGBA")
            self.assertEqual(img.getpixel((5, 5))[3], 0)
            self.assertEqual(img.getpixel((470, 370)), (10, 120, 200, 255))

    def test_small_source_gets_only_the_widths_below_it(self) -> None:
        self.write_jpeg("series/small.jpg", (800, 600))

        run(str(self.root))

        self.assertEqual([p.name for p in self.variants()], ["small.w480.jpg"])
        with Image.open(self.root / "series" / "small.w480.jpg") as img:
            self.assertEqual(img.size, (480, 360))

    def test_source_exactly_at_a_ladder_width_gets_no_variant(self) -> None:
        # "Strictly wider": a 480px source would gain a byte-identical
        # twin and the srcset would advertise two candidates for one size.
        self.write_jpeg("series/exact.jpg", (480, 320))

        run(str(self.root))

        self.assertEqual(self.variants(), [])

    def test_variants_are_never_themselves_resized(self) -> None:
        self.write_jpeg("series/wide.jpg", (2400, 1600))

        run(str(self.root))
        first = [p.name for p in self.variants()]
        # A second pass sees the w1440 file sitting in the same directory
        # and must not treat it as a source.
        run(str(self.root))

        self.assertEqual([p.name for p in self.variants()], first)
        self.assertFalse(
            list(self.root.rglob("*.w1440.w480.*")),
            "a variant was used as a source",
        )
        self.assertTrue(generate_thumbnails.is_variant(Path("wide.w960.jpg")))
        self.assertFalse(generate_thumbnails.is_variant(Path("wide.jpg")))
        self.assertFalse(
            generate_thumbnails.is_variant(Path("wide.w1200.jpg")),
            "only the three contract widths mark a file as a variant",
        )

    def test_variant_carries_no_exif(self) -> None:
        source = self.write_jpeg("series/wide.jpg", (2400, 1600))
        with Image.open(source) as img:
            exif = img.getexif()
            exif[274] = 1  # Orientation — something to be dropped
            img.save(source, format="JPEG", quality=90, exif=exif.tobytes())
        os.utime(source, (OLD, OLD))

        run(str(self.root))

        with Image.open(self.root / "series" / "wide.w480.jpg") as img:
            self.assertEqual(
                dict(img.getexif()), {}, "variants must ship no EXIF"
            )

    # -- determinism ------------------------------------------------------

    def test_rerun_rewrites_nothing(self) -> None:
        self.write_jpeg("series/wide.jpg", (2400, 1600))
        run(str(self.root))
        for variant in self.variants():
            os.utime(variant, (NEWER, NEWER))
        before = {p: p.stat().st_mtime_ns for p in self.variants()}

        code, out = run(str(self.root))

        self.assertEqual(code, 0)
        after = {p: p.stat().st_mtime_ns for p in self.variants()}
        self.assertEqual(before, after, "an unchanged source rewrote its variants")
        self.assertIn("3 current", out)

    def test_force_rewrites_current_variants(self) -> None:
        self.write_jpeg("series/wide.jpg", (2400, 1600))
        run(str(self.root))
        for variant in self.variants():
            os.utime(variant, (NEWER, NEWER))
        before = {p: p.stat().st_mtime_ns for p in self.variants()}

        run(str(self.root), "--force")

        after = {p: p.stat().st_mtime_ns for p in self.variants()}
        self.assertEqual(sorted(before), sorted(after))
        for path in before:
            self.assertNotEqual(
                before[path], after[path], f"--force did not rewrite {path.name}"
            )

    def test_a_touched_source_regenerates_its_variants(self) -> None:
        source = self.write_jpeg("series/wide.jpg", (2400, 1600))
        run(str(self.root))
        for variant in self.variants():
            os.utime(variant, (OLD, OLD))
        os.utime(source, (NEWER, NEWER))

        _, out = run(str(self.root))

        self.assertIn("3 written", out)

    def test_dry_run_writes_nothing(self) -> None:
        self.write_jpeg("series/wide.jpg", (2400, 1600))

        code, out = run(str(self.root), "--dry-run")

        self.assertEqual(code, 0)
        self.assertEqual(self.variants(), [])
        self.assertIn("would be written", out)

    # -- prune ------------------------------------------------------------

    def test_prune_removes_orphans_and_their_companions(self) -> None:
        self.write_jpeg("series/kept.jpg", (2400, 1600))
        self.write_jpeg("series/doomed.jpg", (2400, 1600))
        run(str(self.root))
        orphan = self.root / "series" / "doomed.w960.jpg"
        companion = self.root / "series" / "doomed.w960.webp"
        companion.write_bytes(b"stub")
        (self.root / "series" / "doomed.jpg").unlink()

        code, out = run(str(self.root), "--prune")

        self.assertEqual(code, 0)
        self.assertFalse(orphan.exists(), "orphaned variant survived --prune")
        self.assertFalse(companion.exists(), "orphaned .webp survived --prune")
        self.assertIn("source gone", out)
        self.assertTrue((self.root / "series" / "kept.w960.jpg").exists())

    def test_prune_removes_widths_a_shrunken_source_no_longer_permits(self) -> None:
        self.write_jpeg("series/wide.jpg", (2400, 1600))
        run(str(self.root))
        # Re-import at a smaller size: w960 and w1440 are no longer legal.
        self.write_jpeg("series/wide.jpg", (700, 467))

        run(str(self.root), "--prune")

        self.assertEqual([p.name for p in self.variants()], ["wide.w480.jpg"])

    def test_prune_dry_run_deletes_nothing(self) -> None:
        self.write_jpeg("series/doomed.jpg", (2400, 1600))
        run(str(self.root))
        (self.root / "series" / "doomed.jpg").unlink()

        run(str(self.root), "--prune", "--dry-run")

        self.assertEqual(len(self.variants()), 3)

    # -- PNG --------------------------------------------------------------

    def test_png_variants_preserve_alpha(self) -> None:
        self.write_png_with_alpha("series/logo.png", (2400, 1600))

        run(str(self.root))

        variant = self.root / "series" / "logo.w480.png"
        self.assertTrue(variant.exists())
        with Image.open(variant) as img:
            self.assertIn(img.mode, ("RGBA", "LA", "PA"))
            self.assertEqual(
                img.getpixel((5, 5))[3], 0, "transparent corner lost its alpha"
            )
            self.assertEqual(
                img.getpixel((img.width - 5, img.height - 5))[3],
                255,
                "opaque corner became transparent",
            )

    # -- path arithmetic --------------------------------------------------

    def test_variant_and_source_paths_round_trip(self) -> None:
        source = Path("/x/y/photo.name.jpeg")
        for width in generate_thumbnails.WIDTHS:
            variant = generate_thumbnails.variant_path(source, width)
            self.assertEqual(variant.name, f"photo.name.w{width}.jpeg")
            self.assertEqual(generate_thumbnails.source_for_variant(variant), source)

    def test_a_single_named_image_generates_only_its_own_ladder(self) -> None:
        # The form tools/import-photo.sh uses: name the file just written
        # rather than re-walking a series of two hundred frames.
        self.write_jpeg("series/one.jpg", (2400, 1600))
        self.write_jpeg("series/other.jpg", (2400, 1600))

        run(str(self.root / "series" / "one.jpg"))

        self.assertEqual(
            [p.name for p in self.variants()],
            ["one.w1440.jpg", "one.w480.jpg", "one.w960.jpg"],
        )

    def test_a_named_variant_is_not_treated_as_a_source(self) -> None:
        self.write_jpeg("series/one.jpg", (2400, 1600))
        run(str(self.root))
        before = [p.name for p in self.variants()]

        code, _ = run(str(self.root / "series" / "one.w1440.jpg"))

        self.assertEqual(code, 0)
        self.assertEqual([p.name for p in self.variants()], before)

    def test_missing_root_is_not_an_error(self) -> None:
        code, _ = run(str(self.root / "nope"))

        self.assertEqual(code, 0)


if __name__ == "__main__":
    unittest.main()
