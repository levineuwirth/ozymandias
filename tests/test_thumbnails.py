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
        return {p.name: p.read_bytes() for p in self.photos.iterdir()}

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

    def aside(self, name: str, held: bytes, tail: str = "abcd1234") -> Path:
        """A file as a stopped run leaves it: taken aside from `name`."""
        records = generate_thumbnails.derived_images
        path = self.photos / f"{records.ASIDE}{records.aside_key(Path(name))}-{tail}.tmp"
        path.write_bytes(held)
        return path

    def run_with_errors(self, *argv: str) -> tuple[int, str, str]:
        errors = io.StringIO()
        with contextlib.redirect_stderr(errors):
            code, out = run(str(self.root), *argv)
        return code, out, errors.getvalue()

    def test_a_run_stopped_while_a_variant_is_replaced_leaves_only_a_tmp_name(self) -> None:
        # Stopped between taking its old variant aside and putting the new
        # one in: the name is empty, the file taken aside has a name Git
        # ignores and no build publishes, and the next run puts it back,
        # finds it the script's own, and makes the variant again.
        photo = self.photo("view.jpg", 600)
        self.assertEqual(run(str(self.root))[0], 0)
        Image.new("RGB", (600, 10), (200, 30, 30)).save(photo, format="JPEG")
        link, calls = os.link, []

        def stopped(*args, **kwargs):
            calls.append(args)
            if len(calls) == 1:
                return link(*args, **kwargs)        # the name is taken, and says so
            raise KeyboardInterrupt

        with mock.patch.object(os, "link", stopped), self.assertRaises(KeyboardInterrupt):
            run(str(self.root))
        names = set(self.files())
        self.assertNotIn("view.w480.jpg", names)
        aside = names - {"view.jpg", "view.w480.webp"}
        self.assertEqual(len(aside), 1, names)
        self.assertRegex(aside.pop(), r"^\.previous-image-[0-9a-f]{16}-[^.]+\.tmp$")
        code, out, errors = self.run_with_errors()
        self.assertEqual((code, errors), (0, ""), out)
        self.assertEqual(set(self.files()), {"view.jpg", "view.w480.jpg", "view.w480.webp"})
        self.assertEqual(self.record()["trip/view.w480.jpg"]["digest"], self.sha("view.w480.jpg"))
        self.assertEqual(self.record()["trip/view.w480.jpg"]["source-digest"], self.sha("view.jpg"))

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
        replace = os.replace

        def saved_then_stopped(source, target, **kwargs):
            if Path(source) != variant:
                return replace(source, target, **kwargs)
            variant.write_bytes(self.AUTHORED)
            replace(source, target, **kwargs)
            raise KeyboardInterrupt

        with mock.patch.object(os, "replace", saved_then_stopped), self.assertRaises(KeyboardInterrupt):
            run(str(self.root))
        self.assertNotIn("view.w480.jpg", self.files())
        self.assertIn(self.AUTHORED, self.files().values())

        # A preview moves nothing.
        code, out, errors = self.run_with_errors("--dry-run")
        self.assertNotIn("view.w480.jpg", self.files())

        code, out, errors = self.run_with_errors()
        self.assertEqual(code, 0, out + errors)
        self.assertEqual(variant.read_bytes(), self.AUTHORED)
        self.assertRegex(errors, r"view\.w480\.jpg was set aside by a run that was stopped, and is back in its place")
        self.assertRegex(out, r"keep   \S*view\.w480\.jpg  \(not recorded as made here")
        self.assertEqual(set(self.files()), {"view.jpg", "view.w480.jpg", "view.w480.webp"})
        # Its WebP, still the script's own, is of the new photograph.
        self.assertEqual(self.record()["trip/view.w480.webp"]["source-digest"], self.sha("view.jpg"))
        code, out, errors = self.run_with_errors()
        self.assertEqual((variant.read_bytes(), errors), (self.AUTHORED, ""))

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
        # And one whose name the record does not know (its entry is gone).
        unknown = self.aside("gone.w480.jpg", b"from an earlier run")

        code, out, errors = self.run_with_errors()
        self.assertEqual(code, 0, out + errors)
        files = self.files()
        self.assertEqual({name: files[name] for name in made}, made)
        self.assertNotIn(own.name, files)
        self.assertNotIn(saved.name, files)
        self.assertEqual(files[saved.name[:-len(".tmp")]], self.AUTHORED)
        self.assertRegex(errors, rf"view\.w480\.webp was set aside by a run that was stopped, and another file has "
                                 rf"its name since\. The one set aside is at \S*{saved.name[:-len('.tmp')]}\n")
        self.assertEqual(files[unknown.name[:-len(".tmp")]], b"from an earlier run")
        self.assertRegex(errors, rf"{unknown.name[:-len('.tmp')]} was set aside by a run that was stopped, and the "
                                 "record no longer says which file it was")
        self.assertFalse([name for name in files if name.endswith(".tmp")])

    def test_a_file_put_back_and_not_yet_unnamed_is_one_file(self) -> None:
        # Stopped between giving the file its name back and taking the
        # other name away: both names are one file, and one goes.
        self.photo("view.jpg", 600)
        self.assertEqual(run(str(self.root))[0], 0)
        variant = self.photos / "view.w480.jpg"
        variant.write_bytes(self.AUTHORED)
        records = generate_thumbnails.derived_images
        twin = self.photos / f"{records.ASIDE}{records.aside_key(variant)}-abcd1234.tmp"
        os.link(variant, twin)
        code, out, errors = self.run_with_errors()
        self.assertEqual((code, errors), (0, ""), out)
        self.assertEqual(variant.read_bytes(), self.AUTHORED)
        self.assertEqual(set(self.files()), {"view.jpg", "view.w480.jpg", "view.w480.webp"})

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


if __name__ == "__main__":
    unittest.main()
