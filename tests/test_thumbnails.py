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
        # The wider variant was made after, of the photograph that is there.
        code, said = run(str(self.root))
        self.assertIn("1 written, 1 current", said)
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
