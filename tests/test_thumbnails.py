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
import errno
import hashlib
import io
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock
from tests._helpers import engine_path, load_tool, script_env, stub_bin


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

    def test_a_source_saved_again_regenerates_its_variants(self) -> None:
        source = self.write_jpeg("series/wide.jpg", (2400, 1600))
        run(str(self.root))
        with Image.open(source) as image:
            image.transpose(Image.Transpose.FLIP_LEFT_RIGHT).save(source, format="JPEG", quality=90)

        _, out = run(str(self.root))

        self.assertIn("3 written", out)

    def test_a_source_whose_time_alone_changed_keeps_its_variants(self) -> None:
        # A checkout gives a file a new time and the same bytes: the
        # variants are still that photograph's, and the record is brought
        # up to the time, so the photograph is read once and not again.
        source = self.write_jpeg("series/wide.jpg", (2400, 1600))
        run(str(self.root))
        before = {p: p.stat().st_mtime_ns for p in self.variants()}
        os.utime(source, (NEWER, NEWER))

        _, out = run(str(self.root))

        self.assertIn("0 written, 3 current", out)
        self.assertEqual(before, {p: p.stat().st_mtime_ns for p in self.variants()})
        record = json.loads((self.root / "data/derived-images.json").read_text(encoding="utf-8"))["outputs"]
        self.assertEqual({tuple(entry["source-stamp"]) for entry in record.values()},
                         {(source.stat().st_mtime_ns, source.stat().st_size)})

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



@unittest.skipUnless(HAVE_PILLOW, "Pillow not installed")
class OwnershipTests(unittest.TestCase):
    """The script writes over, and prunes, only what its record says it
    made (tools/derived_images.py): a file of a variant's name that an
    author put there, or changed, stays byte for byte."""

    AUTHORED = b"made by its author"

    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory(prefix="thumbnails-own-")
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name).resolve()
        self.photos = self.root / "trip"
        self.photos.mkdir()
        # Each test asks the filesystem what it can do, as a run of the
        # script does, and so passes or fails the same run alone.
        generate_thumbnails.derived_images._PLACEABLE.clear()

    def photo(self, name: str, width: int = 1000) -> Path:
        path = self.photos / name
        Image.new("RGB", (width, 10), (10, 120, 200)).save(path, format="JPEG")
        os.utime(path, (NEWER, NEWER))
        return path

    def authored(self, name: str) -> Path:
        path = self.photos / name
        path.write_bytes(self.AUTHORED)
        # Older than the photograph: what a rule by the dates rewrites.
        os.utime(path, (OLD, OLD))
        return path

    def files(self) -> dict[str, bytes]:
        """Every file beside the photographs, and in what a run reserved
        there."""
        return {p.relative_to(self.photos).as_posix(): p.read_bytes()
                for p in sorted(self.photos.rglob("*")) if p.is_file()}

    def reserved(self) -> list[str]:
        """The directories a run made to take a file aside into."""
        return sorted(p.name for p in self.photos.iterdir() if p.is_dir())

    def record(self) -> dict[str, dict[str, str]]:
        path = self.root / "data/derived-images.json"
        return json.loads(path.read_text(encoding="utf-8"))["outputs"] if path.is_file() else {}

    def test_what_it_writes_is_recorded_with_its_photograph(self) -> None:
        self.photo("view.jpg")
        code, _ = run(str(self.root))
        self.assertEqual(code, 0)
        self.assertEqual(set(self.record()), {f"trip/view.w{w}.{ext}" for w in (480, 960) for ext in ("jpg", "webp")})
        for entry in self.record().values():
            self.assertEqual((entry["producer"], entry["source"]), ("generate-thumbnails", "trip/view.jpg"))
            self.assertRegex(entry["digest"], r"^sha256:[0-9a-f]{64}$")

    def test_a_file_of_a_variant_s_name_that_is_not_its_own_is_left(self) -> None:
        self.photo("view.jpg")
        mine = [self.authored("view.w480.jpg"), self.authored("view.w960.webp")]
        for argv in ((), ("--force",), ("--prune",), ("--force", "--prune")):
            with self.subTest(argv=argv):
                code, out = run(str(self.root), *argv)
                self.assertEqual(code, 0, out)
                for path in mine:
                    self.assertEqual(path.read_bytes(), self.AUTHORED)
                    self.assertRegex(out, rf"keep   \S*{path.name}  \(not recorded as made here")
                self.assertIn("not this script's and left as they are", out)
        # The places that were free hold its own, and only those are recorded.
        self.assertEqual(set(self.record()), {"trip/view.w480.webp", "trip/view.w960.jpg"})

    def test_its_own_variant_changed_by_the_author_is_the_author_s(self) -> None:
        source = self.photo("view.jpg")
        run(str(self.root))
        edited = self.photos / "view.w480.jpg"
        edited.write_bytes(self.AUTHORED)
        source.unlink()
        self.photo("view.jpg", 1100)
        for argv in ((), ("--force",)):
            code, out = run(str(self.root), *argv)
            self.assertEqual(code, 0, out)
            self.assertEqual(edited.read_bytes(), self.AUTHORED)
        # The others, still its own, were made again from the new photograph.
        with Image.open(self.photos / "view.w960.jpg") as variant:
            self.assertEqual(variant.size[0], 960)

    def test_prune_takes_only_what_the_record_says_it_made(self) -> None:
        # The photograph is gone. Its variants are the script's, but a
        # page shows a WebP of a variant's name that an author made, and
        # another variant was changed by hand; a sidecar is another tool's.
        source = self.photo("view.jpg")
        run(str(self.root))
        (self.photos / "view.w480.webp").write_bytes(self.AUTHORED)
        (self.photos / "view.w960.jpg").write_bytes(self.AUTHORED)
        (self.photos / "view.w480.jpg.dims.yaml").write_text("width: 480\n", encoding="utf-8")
        loose = self.authored("gone.w480.jpg")
        source.unlink()

        code, out = run(str(self.root), "--prune", "--dry-run")
        self.assertEqual(code, 0, out)
        self.assertEqual(len(self.files()), 6)

        code, out = run(str(self.root), "--prune")
        self.assertEqual(code, 0, out)
        self.assertEqual(self.files(), {
            "view.w480.webp": self.AUTHORED, "view.w960.jpg": self.AUTHORED, "gone.w480.jpg": self.AUTHORED,
            "view.w480.jpg.dims.yaml": b"width: 480\n"})
        self.assertEqual(loose.read_bytes(), self.AUTHORED)
        self.assertIn("not recorded as made here", out)
        self.assertIn("a sidecar this script did not write", out)
        # What it pruned it no longer records; what it kept it never owned.
        self.assertEqual(set(self.record()), {"trip/view.w480.webp", "trip/view.w960.jpg"})

    def test_prune_takes_a_webp_whose_variant_is_already_gone(self) -> None:
        # The photograph and its variant are gone, and the variant's WebP
        # is left: nothing of its name says which photograph it came of,
        # and the record does.
        source = self.photo("map.jpg")
        other = self.photo("coast.jpg")
        run(str(self.root))
        for name in ("map.w480.jpg", "map.w960.jpg", "coast.w480.jpg"):
            (self.photos / name).unlink()
        (self.photos / "map.w960.webp").write_bytes(self.AUTHORED)
        source.unlink()

        code, out = run(str(self.root), "--prune", "--dry-run")
        self.assertEqual(code, 0, out)
        self.assertRegex(out, r"prune  \S*map\.w480\.jpg  \(source gone, dry run\)")
        self.assertRegex(out, r"keep   \S*map\.w960\.webp  \(source gone; not recorded as made here, dry run\)")
        self.assertIn("map.w480.webp", self.files())

        code, out = run(str(self.root), "--prune")
        self.assertEqual(code, 0, out)
        self.assertNotIn("map.w480.webp", self.files())
        self.assertNotIn("trip/map.w480.webp", self.record())
        # Changed by its author: kept. And a WebP whose photograph is
        # there is no orphan: its variant is made again beside it.
        self.assertEqual(self.files()["map.w960.webp"], self.AUTHORED)
        self.assertTrue(other.exists())
        self.assertIn("coast.w480.webp", self.files())
        self.assertIn("coast.w480.jpg", self.files())

    def test_a_variant_is_made_again_when_its_photograph_is_another_file(self) -> None:
        # Replaced by an older file of the same name: by the dates the
        # variants are current, and they hold another picture.
        source = self.photo("view.jpg")
        run(str(self.root))
        Image.new("RGB", (1000, 10), (200, 30, 30)).save(source, format="JPEG")
        os.utime(source, (OLD, OLD))
        code, out = run(str(self.root))
        self.assertIn("2 written", out)
        with Image.open(self.photos / "view.w480.jpg") as variant:
            self.assertGreater(variant.getpixel((0, 0))[0], 150)
        code, out = run(str(self.root))
        self.assertIn("0 written, 2 current", out)

    def test_a_webp_saved_while_a_thumbnail_is_encoding_is_preserved(self):
        self.photo("view.jpg", 600)
        webp = self.photos / "view.w480.webp"
        save = Image.Image.save

        def save_with_author_edit(image, output, **kwargs):
            if kwargs["format"] == "WEBP":
                webp.write_bytes(self.AUTHORED)
            return save(image, output, **kwargs)

        with mock.patch.object(Image.Image, "save", save_with_author_edit):
            self.assertEqual(run(str(self.root))[0], 0)
        self.assertEqual(webp.read_bytes(), self.AUTHORED)
        self.assertEqual(set(self.record()), {"trip/view.w480.jpg"})

    def test_a_successful_raster_is_recorded_even_if_webp_encoding_fails(self):
        self.photo("view.jpg", 600)
        save = Image.Image.save

        def fail_webp(image, output, **kwargs):
            if kwargs["format"] == "WEBP":
                raise OSError("WebP encoder failed")
            return save(image, output, **kwargs)

        with mock.patch.object(Image.Image, "save", fail_webp), contextlib.redirect_stderr(io.StringIO()):
            self.assertEqual(run(str(self.root))[0], 1)
        self.assertEqual(run(str(self.root))[0], 0)
        self.assertEqual(set(self.record()), {"trip/view.w480.jpg", "trip/view.w480.webp"})

    def test_variant_widths_follow_the_bytes_consumed(self):
        source = self.photo("view.jpg", 1600)
        consume = generate_thumbnails.derived_images.consume

        def consume_latest(path):
            Image.new("RGB", (250, 10)).save(source)
            return consume(path)

        with mock.patch.object(generate_thumbnails.derived_images, "consume", consume_latest):
            self.assertEqual(run(str(self.root))[0], 0)
        self.assertFalse(self.record())
        self.assertEqual(set(self.files()), {"view.jpg"})

    def sha(self, name: str) -> str:
        return "sha256:" + hashlib.sha256((self.photos / name).read_bytes()).hexdigest()

    def aside(self, name: str, held: bytes | None, tail: str = "abcd1234", *, named: bool = True,
              at: Path | None = None) -> Path:
        """What a stopped run leaves beside `name`: the directory it
        reserved, the name in it, and (unless `held` is None) the file it
        took aside."""
        records = generate_thumbnails.derived_images
        reserved = (at or self.photos) / f"{records.ASIDE}{records.aside_key(Path(name))}-{tail}.tmp"
        reserved.mkdir()
        if named:
            (reserved / records.NAMED).write_bytes(os.fsencode(name))
        if held is not None:
            (reserved / records.DISPLACED).write_bytes(held)
        return reserved

    def run_with_errors(self, *argv: str) -> tuple[int, str, str]:
        errors = io.StringIO()
        with contextlib.redirect_stderr(errors):
            code, out = run(str(self.root), *argv)
        return code, out, errors.getvalue()

    def stopped_at_the_displacement(self, variant: Path, *, before: bool = False, save: bytes | None = None):
        """A run stopped as it takes `variant` aside: just after the file
        has moved, or (`before`) just before. `save` is what its author
        saves there in the moment before."""
        replace = os.replace

        def stopped(source, target, **kwargs):
            if Path(source) != variant:
                return replace(source, target, **kwargs)
            if save is not None:
                variant.write_bytes(save)
            if not before:
                replace(source, target, **kwargs)
            raise KeyboardInterrupt

        return mock.patch.object(os, "replace", stopped)

    def test_a_run_stopped_while_a_variant_is_replaced_leaves_only_a_tmp_name(self) -> None:
        # Stopped between taking its old variant aside and putting the new
        # one in: the name is empty, the file taken aside is in a directory
        # Git ignores and no build publishes, and the next run puts it
        # back, finds it the script's own, and makes the variant again.
        photo = self.photo("view.jpg", 600)
        self.assertEqual(run(str(self.root))[0], 0)
        before = self.files()
        Image.new("RGB", (600, 10), (200, 30, 30)).save(photo, format="JPEG")
        variant = self.photos / "view.w480.jpg"
        with self.stopped_at_the_displacement(variant), self.assertRaises(KeyboardInterrupt):
            run(str(self.root))
        self.assertFalse(variant.exists())
        [reserved] = self.reserved()
        self.assertRegex(reserved, r"^\.previous-image-[0-9a-f]{16}-[a-z0-9_]{8}\.tmp$")
        self.assertEqual({name: held for name, held in self.files().items() if name.startswith(reserved)},
                         {f"{reserved}/name.tmp": b"view.w480.jpg",
                          f"{reserved}/displaced.tmp": before["view.w480.jpg"]})
        code, out, errors = self.run_with_errors()
        self.assertEqual((code, errors), (0, ""), out)
        self.assertEqual(set(self.files()), {"view.jpg", "view.w480.jpg", "view.w480.webp"})
        self.assertEqual(self.reserved(), [])
        self.assertEqual(self.record()["trip/view.w480.jpg"]["digest"], self.sha("view.w480.jpg"))
        self.assertEqual(self.record()["trip/view.w480.jpg"]["source-digest"], self.sha("view.jpg"))

    def test_a_run_stopped_before_it_took_a_variant_aside_leaves_nothing_to_put_back(self) -> None:
        # The directory is reserved, and the name written in it, before
        # the file moves. A run stopped then took nothing aside: the next
        # clears what it left, and takes none of it for a file of anyone's.
        photo = self.photo("view.jpg", 600)
        self.assertEqual(run(str(self.root))[0], 0)
        before = self.files()
        Image.new("RGB", (600, 10), (200, 30, 30)).save(photo, format="JPEG")
        variant = self.photos / "view.w480.jpg"
        with self.stopped_at_the_displacement(variant, before=True), self.assertRaises(KeyboardInterrupt):
            run(str(self.root))
        [reserved] = self.reserved()
        self.assertEqual(self.files()["view.w480.jpg"], before["view.w480.jpg"])
        self.assertEqual([name for name in self.files() if name.startswith(reserved)], [f"{reserved}/name.tmp"])
        # So does one stopped before the name was written, or while it
        # asked what the filesystem can do.
        self.aside("view.w480.webp", None, named=False)
        probing = self.aside("view.w480.webp", None, "efgh5678")
        (probing / "probe.tmp").write_bytes(b"")

        code, out, errors = self.run_with_errors()
        self.assertEqual((code, errors), (0, ""), out)
        self.assertEqual(self.reserved(), [])
        self.assertEqual(set(self.files()), {"view.jpg", "view.w480.jpg", "view.w480.webp"})
        self.assertEqual(self.record()["trip/view.w480.jpg"]["source-digest"], self.sha("view.jpg"))

    def test_an_empty_file_taken_aside_is_a_file_all_the_same(self) -> None:
        # Its author saved nothing into the name (a save cut short) in the
        # moment before it was taken: it is theirs, and goes back.
        photo = self.photo("view.jpg", 600)
        self.assertEqual(run(str(self.root))[0], 0)
        Image.new("RGB", (600, 10), (200, 30, 30)).save(photo, format="JPEG")
        variant = self.photos / "view.w480.jpg"
        with self.stopped_at_the_displacement(variant, save=b""), self.assertRaises(KeyboardInterrupt):
            run(str(self.root))
        code, out, errors = self.run_with_errors()
        self.assertEqual(code, 0, out + errors)
        self.assertEqual(variant.read_bytes(), b"")
        self.assertRegex(errors, r"view\.w480\.jpg was set aside by a run that was stopped, and is back in its place")
        self.assertEqual(self.reserved(), [])

    def test_a_save_taken_aside_by_a_run_that_was_stopped_is_put_back(self) -> None:
        # The author saves into a variant's name after the script found
        # the file there its own, and the run is stopped just after it
        # took the file aside. The only copy of the save is then under a
        # name nobody looks at; the next run puts it back, says so, and
        # leaves it.
        photo = self.photo("view.jpg", 600)
        self.assertEqual(run(str(self.root))[0], 0)
        Image.new("RGB", (600, 10), (200, 30, 30)).save(photo, format="JPEG")
        variant = self.photos / "view.w480.jpg"
        with self.stopped_at_the_displacement(variant, save=self.AUTHORED), self.assertRaises(KeyboardInterrupt):
            run(str(self.root))
        self.assertNotIn("view.w480.jpg", self.files())
        self.assertIn(self.AUTHORED, self.files().values())

        # A preview moves nothing.
        taken = self.files()
        code, out, errors = self.run_with_errors("--dry-run")
        self.assertEqual(self.files(), taken)

        code, out, errors = self.run_with_errors()
        self.assertEqual(code, 0, out + errors)
        self.assertEqual(variant.read_bytes(), self.AUTHORED)
        self.assertRegex(errors, r"view\.w480\.jpg was set aside by a run that was stopped, and is back in its place")
        self.assertRegex(out, r"keep   \S*view\.w480\.jpg  \(not recorded as made here")
        self.assertEqual(set(self.files()), {"view.jpg", "view.w480.jpg", "view.w480.webp"})
        self.assertEqual(self.reserved(), [])
        # Its WebP, still the script's own, is of the new photograph.
        self.assertEqual(self.record()["trip/view.w480.webp"]["source-digest"], self.sha("view.jpg"))
        code, out, errors = self.run_with_errors()
        self.assertEqual((variant.read_bytes(), errors), (self.AUTHORED, ""))

    def test_a_save_taken_aside_is_put_back_in_a_directory_renamed_since(self) -> None:
        # The record says where the variants were. What a stopped run took
        # aside is looked for where the photographs are now: the save goes
        # back to its name there, and is named, though no entry is of
        # that path.
        photo = self.photo("view.jpg", 600)
        self.assertEqual(run(str(self.root))[0], 0)
        Image.new("RGB", (600, 10), (200, 30, 30)).save(photo, format="JPEG")
        with self.stopped_at_the_displacement(self.photos / "view.w480.jpg", save=self.AUTHORED), \
                self.assertRaises(KeyboardInterrupt):
            run(str(self.root))
        self.photos = self.photos.rename(self.root / "journey")

        code, out, errors = self.run_with_errors()
        self.assertEqual(code, 0, out + errors)
        self.assertEqual((self.photos / "view.w480.jpg").read_bytes(), self.AUTHORED)
        self.assertRegex(errors, r"journey/view\.w480\.jpg was set aside by a run that was stopped, and is back "
                                 "in its place")
        self.assertEqual(self.reserved(), [])
        self.assertNotIn(".tmp", "".join(self.files()))

    def test_a_single_named_photograph_looks_in_its_own_directory_alone(self) -> None:
        photo = self.photo("view.jpg", 600)
        self.assertEqual(run(str(photo))[0], 0)
        made = self.files()
        (self.photos / "view.w480.jpg").unlink()
        self.aside("view.w480.jpg", made["view.w480.jpg"])
        below = self.photos / "album"
        below.mkdir()
        deeper = self.aside("other.w480.jpg", self.AUTHORED, at=below)
        errors = io.StringIO()
        with contextlib.redirect_stderr(errors):
            code, out = run(str(photo))
        self.assertEqual((code, errors.getvalue()), (0, ""), out)
        self.assertEqual(self.files()["view.w480.jpg"], made["view.w480.jpg"])
        self.assertTrue((deeper / "displaced.tmp").is_file())

    def test_what_a_stopped_run_left_aside_never_takes_a_name_that_has_a_file_again(self) -> None:
        self.photo("view.jpg", 600)
        self.assertEqual(run(str(self.root))[0], 0)
        made = self.files()
        # Its own old file, aside from a name that has the variant again:
        # nothing anyone needs.
        own = self.aside("view.w480.jpg", made["view.w480.jpg"])
        # A save of the author's, aside from a name that has a file again:
        # kept beside it, under a name that is seen.
        saved = self.aside("view.w480.webp", self.AUTHORED)

        code, out, errors = self.run_with_errors()
        self.assertEqual(code, 0, out + errors)
        files = self.files()
        self.assertEqual({name: files[name] for name in made}, made)
        self.assertEqual(self.reserved(), [])
        self.assertEqual(files[saved.name[:-len(".tmp")]], self.AUTHORED)
        self.assertRegex(errors, rf"view\.w480\.webp was set aside by a run that was stopped, and another file has "
                                 rf"its name since\. The one set aside is at \S*{saved.name[:-len('.tmp')]}\n")
        self.assertEqual(set(files), set(made) | {saved.name[:-len(".tmp")]})
        self.assertNotIn(own.name[:-len(".tmp")], files)
        # And said once: nothing is left to say it of.
        code, out, errors = self.run_with_errors()
        self.assertEqual((code, errors, self.files()), (0, "", files), out)

    def test_a_kept_file_named_and_not_yet_unnamed_is_one_file(self) -> None:
        # Stopped between giving a file kept for its author the name the
        # author sees and taking the other name away: one file under both.
        # The next run finishes that, and the one after has nothing to say.
        self.photo("view.jpg", 600)
        self.assertEqual(run(str(self.root))[0], 0)
        made = self.files()
        saved = self.aside("view.w480.webp", self.AUTHORED)
        kept = saved.with_suffix("")
        os.link(saved / "displaced.tmp", kept)
        code, out, errors = self.run_with_errors()
        self.assertEqual(code, 0, out + errors)
        self.assertIn(f"The one set aside is at {kept}", errors)
        self.assertEqual(self.files(), {**made, kept.name: self.AUTHORED})
        self.assertEqual((self.reserved(), kept.stat().st_nlink), ([], 1))
        code, out, errors = self.run_with_errors()
        self.assertEqual((code, errors, self.files()), (0, "", {**made, kept.name: self.AUTHORED}), out)

    def test_a_kept_file_takes_another_name_where_the_first_is_taken(self) -> None:
        # The name a kept file would get has another file: it gets the
        # next, and is not left in the reserved directory, where every
        # run would find it waiting.
        self.photo("view.jpg", 600)
        self.assertEqual(run(str(self.root))[0], 0)
        made = self.files()
        saved = self.aside("view.w480.webp", self.AUTHORED)
        first = saved.with_suffix("")
        first.write_bytes(b"kept before")
        (self.photos / f"{first.name}-2").mkdir()
        code, out, errors = self.run_with_errors()
        self.assertEqual(code, 0, out + errors)
        kept = self.photos / f"{first.name}-3"
        self.assertIn(f"The one set aside is at {kept}", errors)
        self.assertEqual(self.files(), {**made, first.name: b"kept before", kept.name: self.AUTHORED})
        self.assertEqual(self.reserved(), [f"{first.name}-2"])
        code, out, errors = self.run_with_errors()
        self.assertEqual((code, errors), (0, ""), out)

    def test_no_photograph_is_looked_for_in_a_directory_reserved_to_take_a_file_aside(self) -> None:
        # One left for its author to look at may hold a picture of any
        # width: none of it is the site's, to make variants of or to prune.
        self.photo("view.jpg", 600)
        self.assertEqual(run(str(self.root))[0], 0)
        left = self.aside("view.w480.jpg", None)
        Image.new("RGB", (2000, 10), (10, 120, 200)).save(left / "sketch.jpg", format="JPEG")
        (left / "gone.w480.jpg").write_bytes(self.AUTHORED)
        before = (self.files(), self.record())
        for argv in ((), ("--force",), ("--prune",)):
            code, out, errors = self.run_with_errors(*argv)
            self.assertEqual((code, errors), (0, ""), out)
            self.assertNotIn("sketch", out)
            self.assertNotIn("gone.w480", out)
            self.assertEqual(sorted(p.name for p in left.iterdir()), ["gone.w480.jpg", "name.tmp", "sketch.jpg"])
        self.assertEqual(self.record().keys(), before[1].keys())

    def test_a_reserved_directory_that_is_not_as_a_run_leaves_one_is_left_whole(self) -> None:
        # A run leaves, in the directory it reserved: nothing; empty files
        # it asked the filesystem with; the name of the file it was about
        # to take; and then that file, with its name. Anything else there
        # is somebody's, and none of it is moved or deleted: not a file
        # of a probe's name that holds something, not a file beside the
        # tools' own, not a file aside with no name or another's.
        self.photo("view.jpg", 600)
        self.assertEqual(run(str(self.root))[0], 0)
        written = self.aside("view.w480.jpg", None, "aaaa1111")
        (written / "probe.tmp").write_bytes(b"written by hand")
        beside = self.aside("view.w480.jpg", None, "bbbb2222")
        (beside / "probe.tmp").write_bytes(b"")
        (beside / "sketch.txt").write_bytes(b"put here by hand")
        unnamed = self.aside("view.w480.webp", self.AUTHORED, "cccc3333", named=False)
        misnamed = self.aside("view.w480.webp", self.AUTHORED, "dddd4444", named=False)
        (misnamed / "name.tmp").write_bytes(b"view.w480.jpg")
        crowded = self.aside("view.w480.webp", self.AUTHORED, "eeee5555")
        (crowded / "sketch.txt").write_bytes(b"put here by hand")
        before = (self.files(), self.reserved())

        code, out, errors = self.run_with_errors()
        self.assertEqual(code, 0, out + errors)
        self.assertEqual((self.files(), self.reserved()), before)
        # The ones with a file aside are named, each run: a save may be in them.
        for reserved in (unnamed, misnamed, crowded):
            self.assertIn(f"{reserved / 'displaced.tmp'} looks like a file set aside by a run that was stopped, and "
                          "its directory is not as a run leaves one", errors)
        self.assertEqual(errors.count("looks like a file set aside"), 3, errors)
        self.assertNotIn("was set aside by", errors)

    def test_only_what_a_run_reserved_is_ever_put_back_or_cleared(self) -> None:
        # Names that begin as a reserved directory's does, and are not
        # one: the author's, whatever they hold.
        self.photo("view.jpg", 600)
        self.assertEqual(run(str(self.root))[0], 0)
        (self.photos / ".previous-image-notes.tmp").write_bytes(b"mine")
        (self.photos / ".previous-image-0123456789abcdef-abcd1234.tmp").write_bytes(b"a file, not a directory")
        (self.photos / ".previous-image-notes-2.tmp").mkdir()
        (self.photos / ".previous-image-notes-2.tmp/displaced.tmp").write_bytes(b"mine too")
        (self.photos / ".previous-image-0123456789ABCDEF-abcd1234.tmp").mkdir()
        before = (self.files(), self.reserved())
        code, out, errors = self.run_with_errors()
        self.assertEqual((code, errors), (0, ""), out)
        self.assertEqual((self.files(), self.reserved()), before)

    def test_a_directory_of_no_tree_this_run_holds_is_not_looked_into(self) -> None:
        # What is aside in another tree is that tree's tool's to put back,
        # by its record and under its lock, however this run comes to
        # the directory: a link, here, where its own directory was.
        self.photo("view.jpg", 600)
        self.assertEqual(run(str(self.root))[0], 0)
        elsewhere = tempfile.TemporaryDirectory(prefix="thumbnails-other-")
        self.addCleanup(elsewhere.cleanup)
        other = Path(elsewhere.name).resolve() / "trip"
        other.mkdir()
        (other / "view.w480.jpg").write_bytes(b"the other tree's")
        taken = [self.aside("view.w480.jpg", self.files()["view.w480.jpg"], at=other),
                 self.aside("view.w480.jpg", self.AUTHORED, "efgh5678", at=other)]
        before = {p.relative_to(other).as_posix(): p.read_bytes() for p in other.rglob("*") if p.is_file()}
        records = generate_thumbnails.derived_images
        link = self.root / "linked"
        link.symlink_to(other, target_is_directory=True)
        errors = io.StringIO()
        with contextlib.redirect_stderr(errors), records.Records.held(elsewhere=self.root) as held:
            held.restore_displaced([link, other])
        self.assertEqual(errors.getvalue(), "")
        self.assertEqual({p.relative_to(other).as_posix(): p.read_bytes() for p in other.rglob("*") if p.is_file()},
                         before)
        self.assertTrue(all(reserved.is_dir() for reserved in taken))

    def test_a_file_put_back_and_not_yet_unnamed_is_one_file(self) -> None:
        # Stopped between giving the file its name back and taking the
        # other name away: both names are one file, and one goes.
        self.photo("view.jpg", 600)
        self.assertEqual(run(str(self.root))[0], 0)
        variant = self.photos / "view.w480.jpg"
        variant.write_bytes(self.AUTHORED)
        reserved = self.aside("view.w480.jpg", None)
        os.link(variant, reserved / "displaced.tmp")
        code, out, errors = self.run_with_errors()
        self.assertEqual((code, errors), (0, ""), out)
        self.assertEqual(variant.read_bytes(), self.AUTHORED)
        self.assertEqual(set(self.files()), {"view.jpg", "view.w480.jpg", "view.w480.webp"})
        self.assertEqual(self.reserved(), [])

    def test_a_save_displaced_and_then_shut_out_is_kept_where_its_author_sees_it(self) -> None:
        # Two saves by the author into a variant's name, one just before
        # the script takes its own file aside and one just after: the
        # second keeps the name, and the first is kept beside it under a
        # name that is not a temporary file's.
        photo = self.photo("view.jpg", 600)
        self.assertEqual(run(str(self.root))[0], 0)
        Image.new("RGB", (600, 10), (200, 30, 30)).save(photo, format="JPEG")
        variant = self.photos / "view.w480.jpg"
        replace = os.replace

        def replace_between_two_saves(source, target, **kwargs):
            if Path(source) != variant:
                return replace(source, target, **kwargs)
            variant.unlink()
            variant.write_bytes(b"the first save")
            replace(source, target, **kwargs)
            variant.write_bytes(b"the second save")

        errors = io.StringIO()
        with mock.patch.object(os, "replace", replace_between_two_saves), contextlib.redirect_stderr(errors):
            self.assertEqual(run(str(self.root))[0], 0)
        files = self.files()
        self.assertEqual(files["view.w480.jpg"], b"the second save")
        kept = [name for name, held in files.items() if held == b"the first save"]
        self.assertEqual(len(kept), 1, sorted(files))
        self.assertRegex(kept[0], r"^\.previous-image-[^.]+$")
        self.assertIn(f"The displaced file is at {self.photos / kept[0]}", errors.getvalue())
        self.assertFalse([name for name in files if name.endswith(".tmp")])
        self.assertEqual(self.reserved(), [])

    NO_LINKS = OSError(errno.EPERM, "Operation not permitted")

    def test_a_filesystem_without_hard_links_is_written_to_by_a_rename_that_replaces_nothing(self) -> None:
        # link(2) refused, as exFAT refuses it. A variant is still put only
        # where no file is, or over the script's own, and in one step: no
        # file is opened under the public name, where a save into it
        # meanwhile would be of the same file.
        photo = self.photo("view.jpg")
        kept = self.authored("view.w960.webp")
        no_links = mock.patch.object(os, "link", side_effect=self.NO_LINKS)
        ours = ("view.w480.jpg", "view.w480.webp", "view.w960.jpg")

        def recorded_as_written() -> None:
            self.assertEqual(set(self.record()), {f"trip/{name}" for name in ours})
            for name in ours:
                self.assertEqual(self.record()[f"trip/{name}"]["digest"], self.sha(name), name)
                self.assertEqual((self.photos / name).stat().st_mode & 0o777, 0o644, name)
            self.assertEqual(kept.read_bytes(), self.AUTHORED)
            self.assertEqual(set(self.files()), {"view.jpg", "view.w960.webp", *ours})

        opened = []
        opener = os.open

        def watched(path, flags, *args, **kwargs):
            if flags & (os.O_WRONLY | os.O_RDWR) and Path(os.fsdecode(path)).name in (*ours, kept.name):
                opened.append(path)
            return opener(path, flags, *args, **kwargs)

        with no_links, mock.patch.object(os, "open", watched):
            self.assertEqual(run(str(self.root))[0], 0)
        recorded_as_written()
        first = self.files()
        # Another photograph: its own variants are replaced.
        Image.new("RGB", (1000, 10), (200, 30, 30)).save(photo, format="JPEG")
        with no_links, mock.patch.object(os, "open", watched):
            self.assertEqual(run(str(self.root))[0], 0)
        recorded_as_written()
        self.assertEqual(opened, [])
        for name in ours:
            self.assertNotEqual(self.files()[name], first[name], name)
        # And a save by the author while one is encoded keeps its place.
        Image.new("RGB", (1000, 10), (30, 200, 30)).save(photo, format="JPEG")
        save = Image.Image.save

        def save_with_author_edit(image, output, **kwargs):
            if kwargs["format"] == "WEBP":
                (self.photos / "view.w480.webp").write_bytes(self.AUTHORED)
            return save(image, output, **kwargs)

        with no_links, mock.patch.object(Image.Image, "save", save_with_author_edit):
            self.assertEqual(run(str(self.root))[0], 0)
        self.assertEqual((self.photos / "view.w480.webp").read_bytes(), self.AUTHORED)
        self.assertEqual(self.record()["trip/view.w480.jpg"]["digest"], self.sha("view.w480.jpg"))
        self.assertNotEqual(self.record()["trip/view.w480.webp"]["digest"], self.sha("view.w480.webp"))

    def test_a_file_is_given_a_name_only_if_no_file_has_it(self) -> None:
        records = generate_thumbnails.derived_images
        for links in (True, False):
            with self.subTest(links=links), contextlib.ExitStack() as stack:
                if not links:
                    stack.enter_context(mock.patch.object(os, "link", side_effect=self.NO_LINKS))
                held, name = self.photos / "held.tmp", self.photos / f"name-{links}.jpg"
                held.write_bytes(b"encoded")
                records.place(held, name)
                self.assertEqual((name.read_bytes(), held.exists()), (b"encoded", False))
                held.write_bytes(b"encoded again")
                name.write_bytes(self.AUTHORED)
                with self.assertRaises(FileExistsError):
                    records.place(held, name)
                self.assertEqual((name.read_bytes(), held.read_bytes()), (self.AUTHORED, b"encoded again"))
                held.unlink()

    def test_where_no_step_spares_a_file_already_there_nothing_is_written(self) -> None:
        # Neither hard links nor a rename that refuses an existing name:
        # a copy into the name would share the file with a save made
        # meanwhile. The script says so and writes no variant, and takes
        # none of its own aside, which it could not put back.
        records = generate_thumbnails.derived_images
        photo = self.photo("view.jpg", 600)

        @contextlib.contextmanager
        def neither():
            records._PLACEABLE.clear()
            with mock.patch.object(os, "link", side_effect=self.NO_LINKS), \
                    mock.patch.object(records.ctypes, "CDLL", side_effect=OSError("no such library")):
                yield

        self.addCleanup(records._PLACEABLE.clear)
        with neither():
            code, out, errors = self.run_with_errors()
            self.assertEqual(code, 1, out)
            self.assertIn("neither hard links nor a rename that leaves an existing file alone", errors)
            self.assertEqual(set(self.files()), {"view.jpg"})
            self.assertFalse(self.record())

        self.assertEqual(run(str(self.root))[0], 0)
        made = self.files()
        Image.new("RGB", (600, 10), (200, 30, 30)).save(photo, format="JPEG")
        with neither():
            code, out, errors = self.run_with_errors()
            self.assertEqual(code, 1, out)
            self.assertIn("neither hard links nor a rename that leaves an existing file alone", errors)
        after = self.files()
        self.assertEqual({name: after[name] for name in made if name != "view.jpg"},
                         {name: made[name] for name in made if name != "view.jpg"})
        self.assertEqual(set(after), set(made))
        self.assertEqual(self.reserved(), [])

    def test_a_photograph_saved_again_while_its_variants_are_made_is_recorded_as_it_was_read(self) -> None:
        # Another photograph is saved over this one after it was read and
        # before its variants are recorded. They are of the one before,
        # and their entries say so: the next run makes them again.
        photo = self.photo("view.jpg")
        first = photo.read_bytes()
        render = generate_thumbnails.render_variant

        def render_and_save_again(source, destination, width, **how):
            written = render(source, destination, width, **how)
            if destination.name == "view.w480.jpg":
                Image.new("RGB", (1000, 10), (200, 30, 30)).save(source, format="JPEG")
            return written

        errors = io.StringIO()
        with mock.patch.object(generate_thumbnails, "render_variant", render_and_save_again), \
                contextlib.redirect_stderr(errors):
            code, _ = run(str(self.root))
        self.assertEqual(code, 0)
        self.assertIn("trip/view.jpg was saved again while its variants were made", errors.getvalue())
        was = "sha256:" + hashlib.sha256(first).hexdigest()
        self.assertNotEqual(photo.read_bytes(), first)
        made_first = (self.photos / "view.w480.jpg").read_bytes()
        self.assertEqual(self.record()["trip/view.w480.jpg"]["source-digest"], was)
        self.assertEqual(self.record()["trip/view.w480.jpg"]["digest"],
                         "sha256:" + hashlib.sha256(made_first).hexdigest())
        # Both widths were made from the same snapshot.
        code, said = run(str(self.root))
        self.assertIn("2 written, 0 current", said)
        self.assertNotEqual((self.photos / "view.w480.jpg").read_bytes(), made_first)
        now = "sha256:" + hashlib.sha256(photo.read_bytes()).hexdigest()
        self.assertEqual({entry["source-digest"] for entry in self.record().values()}, {now})
        self.assertIn("0 written, 2 current", run(str(self.root))[1])

    def test_a_record_that_cannot_be_read_stops_the_script(self) -> None:
        self.photo("view.jpg")
        (self.root / "data").mkdir()
        (self.root / "data/derived-images.json").write_text("{", encoding="utf-8")
        err = io.StringIO()
        with contextlib.redirect_stderr(err):
            code, _ = run(str(self.root))
        self.assertEqual(code, 1)
        self.assertIn("nothing was written", err.getvalue())
        self.assertEqual(set(self.files()), {"view.jpg"})



# cwebp -quiet -q 85 <image> -o <companion>: a stand-in that writes what a
# conversion depends on, the image's name and bytes.
CWEBP = """#!/bin/sh
image=$4
while [ "$1" != "-o" ]; do shift; done
{ printf '%s:' "$(basename "$image")"; cat "$image"; } > "$2"
"""


@unittest.skipUnless(HAVE_PILLOW, "Pillow not installed")
class AdoptionTests(unittest.TestCase):
    """`make adopt-images` (tools/derived_images.py adopt): the files from
    before the record, each entered if making it again gives the same
    bytes. The variants first, then the companions, over one reading of
    the record, so a preview says what the adoption would do."""

    AUTHORED = b"made by its author"

    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory(prefix="adopt-images-")
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name).resolve()
        self.photos = self.root / "content/photography/trip"
        self.photos.mkdir(parents=True)
        self.stubs = stub_bin(self.root / "bin", ["cwebp"], CWEBP)
        Image.new("RGB", (1000, 10), (10, 120, 200)).save(self.photos / "view.jpg", format="JPEG")
        # A site from before the record: variants and companions, no entries.
        for tool in (["generate-thumbnails.py"], ["convert_images.py"]):
            self.tool(*tool)
        (self.root / "data/derived-images.json").unlink()
        (self.photos / "view.w960.webp").write_bytes(self.AUTHORED)

    def tool(self, *argv: str) -> subprocess.CompletedProcess[str]:
        done = subprocess.run([sys.executable, str(engine_path("tools")) + "/" + argv[0], *argv[1:]], cwd=self.root,
                              capture_output=True, text=True, timeout=120,
                              env=script_env(self.stubs, SITE_ROOT=str(self.root)))
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        return done

    def state(self) -> tuple[dict[str, bytes], bytes | None]:
        record = self.root / "data/derived-images.json"
        return ({p.name: p.read_bytes() for p in self.photos.iterdir()},
                record.read_bytes() if record.is_file() else None)

    def test_a_preview_says_what_the_adoption_does_and_writes_nothing(self) -> None:
        before = self.state()
        preview = self.tool("derived_images.py", "adopt", "--dry-run").stdout
        self.assertEqual(self.state(), before)
        real = self.tool("derived_images.py", "adopt").stdout
        self.assertEqual(preview.replace("would be recorded", "recorded"), real)
        # The variants are the first stage's, so the second does not take
        # view.w480.jpg for an image and its WebP for a failed conversion.
        self.assertIn("generate-thumbnails: 3 variant file(s) recorded as this script's; 1 differ", real)
        self.assertIn("convert-images: 1 companion(s) recorded as this script's; 0 differ", real)
        self.assertEqual(real.count("differs  "), 1)
        self.assertIn("view.w960.webp", real)
        record = json.loads((self.root / "data/derived-images.json").read_text(encoding="utf-8"))["outputs"]
        at = "content/photography/trip/"
        self.assertEqual({k: v["producer"] for k, v in record.items()}, {
            at + "view.w480.jpg": "generate-thumbnails", at + "view.w480.webp": "generate-thumbnails",
            at + "view.w960.jpg": "generate-thumbnails", at + "view.webp": "convert-images"})
        # Nothing but the record was written, and the author's file is no one else's.
        self.assertEqual(self.state()[0], before[0])
        said = self.tool("generate-thumbnails.py", "--force").stdout
        self.assertEqual((self.photos / "view.w960.webp").read_bytes(), self.AUTHORED)
        self.assertIn("1 not this script's", said)

    def test_an_entry_that_does_not_say_which_bytes_its_photograph_had_is_completed(self) -> None:
        # A record of an earlier form: adopted then, with the photograph's
        # path and nothing of its bytes or its time.
        self.tool("derived_images.py", "adopt")
        path = self.root / "data/derived-images.json"
        document = json.loads(path.read_text(encoding="utf-8"))
        self.assertEqual(len(document["outputs"]), 4)
        for entry in document["outputs"].values():
            del entry["source-digest"], entry["source-stamp"]
        path.write_text(json.dumps(document), encoding="utf-8")
        before = self.state()
        preview = self.tool("derived_images.py", "adopt", "--dry-run").stdout
        self.assertEqual(self.state(), before)
        real = self.tool("derived_images.py", "adopt").stdout
        self.assertEqual(preview.replace("would be recorded", "recorded"), real)
        self.assertIn("generate-thumbnails: 3 variant file(s) recorded as this script's", real)
        self.assertIn("convert-images: 1 companion(s) recorded as this script's", real)
        source = "sha256:" + hashlib.sha256((self.photos / "view.jpg").read_bytes()).hexdigest()
        record = json.loads(path.read_text(encoding="utf-8"))["outputs"]
        self.assertEqual({entry["source-digest"] for entry in record.values()}, {source})
        self.assertEqual({key: entry["digest"] for key, entry in record.items()},
                         {key: entry["digest"] for key, entry in document["outputs"].items()})
        self.assertEqual(self.state()[0], before[0])
        # And nothing is made again for it.
        self.assertIn("0 written, 2 current", self.tool("generate-thumbnails.py").stdout)

    def test_an_entry_with_other_bytes_than_the_file_is_brought_up_to_what_the_script_makes_now(self) -> None:
        # A run stopped after it put a new variant in place and before it
        # saved the record: the entry has the bytes of the variant before.
        # No run writes over such a file, which may as well be one its
        # author changed. An adoption renders it again and brings the
        # entry up to it if that gives the same bytes; not one its author
        # changed, and not one whose entry is of another photograph.
        self.tool("derived_images.py", "adopt")
        path = self.root / "data/derived-images.json"
        at = "content/photography/trip/"
        before = json.loads(path.read_text(encoding="utf-8"))
        Image.new("RGB", (1000, 10), (200, 30, 30)).save(self.photos / "view.jpg", format="JPEG")
        self.tool("generate-thumbnails.py")
        after = json.loads(path.read_text(encoding="utf-8"))
        self.assertNotEqual(after["outputs"][at + "view.w480.jpg"], before["outputs"][at + "view.w480.jpg"])
        # The new files, and the entries of the ones before.
        stale = json.loads(json.dumps(after))
        for name in ("view.w480.jpg", "view.w480.webp", "view.w960.jpg"):
            stale["outputs"][at + name] = dict(before["outputs"][at + name])
        stale["outputs"][at + "view.w960.jpg"]["source"] = at + "other.jpg"
        (self.photos / "view.w480.webp").write_bytes(self.AUTHORED)
        path.write_text(json.dumps(stale), encoding="utf-8")
        state = self.state()

        said = self.tool("generate-thumbnails.py").stdout
        self.assertRegex(said, r"keep   \S*view\.w480\.jpg  \(not recorded as made here")
        self.assertEqual(self.state()[0], state[0])
        path.write_text(json.dumps(stale), encoding="utf-8")

        preview = self.tool("derived_images.py", "adopt", "--dry-run").stdout
        self.assertEqual(self.state(), state)
        self.assertEqual(preview.count("  repair   "), 1)
        self.assertRegex(preview, r"  repair   \S*view\.w480\.jpg  \(what this script makes of \S*view\.jpg now")
        self.assertIn("their entries would be brought up to them", preview)
        self.assertIn("1 more have an entry of this script's and other bytes than it says", preview)

        real = self.tool("derived_images.py", "adopt").stdout
        self.assertEqual(real, preview.replace("would be recorded", "recorded").replace("would be", "were"))
        self.assertEqual(self.state()[0], state[0])
        record = json.loads(path.read_text(encoding="utf-8"))["outputs"]
        self.assertEqual(record[at + "view.w480.jpg"], after["outputs"][at + "view.w480.jpg"])
        for name in ("view.w480.webp", "view.w960.jpg"):
            self.assertEqual(record[at + name], stale["outputs"][at + name])
        # Its own again: nothing is made again for it, and it is not kept
        # from the script as another's file is.
        said = self.tool("generate-thumbnails.py").stdout
        self.assertNotRegex(said, r"keep   \S*view\.w480\.jpg ")

    def test_an_entry_is_dropped_only_where_a_run_looked_for_what_was_taken_aside(self) -> None:
        # A photograph prepared outside content/, by the script run over
        # that one file, and a replacement of its variant stopped with the
        # old one aside. A conversion and an adoption look into content/
        # and static/: the variant is missing for them, and its entry is
        # not theirs to drop, or it comes back as nobody's and is never
        # made again.
        prepared = self.root / "prepared"
        prepared.mkdir()
        photo = prepared / "photo.jpg"
        Image.new("RGB", (600, 10), (200, 30, 30)).save(photo, format="JPEG")
        self.tool("generate-thumbnails.py", "prepared/photo.jpg")
        path = self.root / "data/derived-images.json"
        entry = json.loads(path.read_text(encoding="utf-8"))["outputs"]["prepared/photo.w480.jpg"]
        variant = prepared / "photo.w480.jpg"
        reserved = prepared / f".previous-image-{hashlib.sha256(variant.name.encode()).hexdigest()[:16]}-abcd1234.tmp"
        reserved.mkdir()
        (reserved / "name.tmp").write_bytes(variant.name.encode())
        os.replace(variant, reserved / "displaced.tmp")
        Image.new("RGB", (600, 10), (30, 30, 200)).save(photo, format="JPEG")

        for tool in (["derived_images.py", "adopt"], ["convert_images.py"]):
            self.tool(*tool)
            self.assertEqual(json.loads(path.read_text(encoding="utf-8"))["outputs"]["prepared/photo.w480.jpg"], entry)
            self.assertTrue((reserved / "displaced.tmp").is_file())

        # A declaration there waits too, and says which command puts the
        # variant back: the script over the photograph's directory, which
        # is run here exactly as it is printed.
        waits = subprocess.run([sys.executable, str(engine_path("tools/derived_images.py")), "declare",
                                "prepared/photo.w480.webp"], cwd=self.root, capture_output=True, text=True,
                               timeout=120, env=script_env(self.stubs, SITE_ROOT=str(self.root)))
        self.assertEqual(waits.returncode, 1, waits.stdout + waits.stderr)
        [command] = [line.split() for line in waits.stderr.splitlines() if "generate-thumbnails.py" in line]
        self.assertEqual(command, ["python3", "engine/tools/generate-thumbnails.py", "prepared"])
        done = self.tool("generate-thumbnails.py", *command[2:])
        self.assertEqual(done.stderr, "")
        self.assertFalse(reserved.exists())
        with Image.open(variant) as made:
            red, _, blue = made.getpixel((0, 0))
        self.assertGreater(blue, red)
        record = json.loads(path.read_text(encoding="utf-8"))["outputs"]["prepared/photo.w480.jpg"]
        self.assertEqual(record["digest"], "sha256:" + hashlib.sha256(variant.read_bytes()).hexdigest())
        self.assertEqual(record["source-digest"], "sha256:" + hashlib.sha256(photo.read_bytes()).hexdigest())


if __name__ == "__main__":
    unittest.main()
