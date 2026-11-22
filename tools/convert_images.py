"""convert_images.py — a WebP companion beside every local JPEG and PNG.

Run as tools/convert-images.sh (`make convert-images`, and inside `make
build`). Walks the site's static/ and content/ and the engine's static/,
and has cwebp make `lake.webp` beside `lake.jpg`. An extension counts in
either case (IMG_0042.JPG, as a camera writes it).

Whose file a WebP is. The script writes a companion where there is none,
and over one it made itself: the ownership record (tools/derived_images.py)
says which those are, by the image they were made from and the bytes last
written. Its own companion is made again when its image is not the one it
was made from: other bytes than the record noted (an older file counts as
much as a newer one; a file whose time alone changed is the same image),
or another file altogether, the first being gone. It is made of the image
as the script read it, and recorded as that: an image saved again while
cwebp ran is converted again, at once. Any other WebP of that
name is the author's (made by hand, shown by a page as an image of its
own, or simply older than the record) and is left exactly as it is,
however old. The generator shows an image with a
companion only when the record or the author's declaration pairs them
(build/DerivedImages.hs), so an unrecorded one is not shown in the image's
place either, and neither is the script's own while it has yet to be made
again (no cwebp here). What was left alone is counted at the end, with what to do.

One companion, one image. A page shows map.png, map.JPG or map.gif with
map.webp, so two of them beside each other would have one companion
between them. The same holds across static/ and the engine's static/,
which are served as one tree with the site's file over the engine's: the
site's images/color.png and the engine's images/color.JPG would both be
shown with /images/color.webp. The script refuses such images, naming
them and any companion already there, before it converts anything, with
or without cwebp. It deletes nothing.

`make adopt-images` (tools/derived_images.py adopt) records the
companions that were there before the record and that a fresh conversion
reproduces byte for byte.

Without cwebp the script warns and exits 0: build/Filters/Images.hs only
writes a <source type="image/webp"> for a companion that is there, so the
site is correct, and heavier. `tools/check-site.py --require-webp` is what
makes a missing companion fatal in a deploy.

Photography's .w480/.w960/.w1440 variants and their WebPs are
tools/generate-thumbnails.py's, which runs first in `make build`: a file
that tool recorded is not an image for this one to convert.

Quality: -q 85 is a good default for photographic content.
"""

from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import derived_images  # noqa: E402
import sitelib  # noqa: E402
from derived_images import show  # noqa: E402

PRODUCER = "convert-images"
THUMBNAILS = "generate-thumbnails"
CONVERTED = (".jpg", ".jpeg", ".png")
SHOWN_WITH_ONE = CONVERTED + (".gif",)     # a GIF is shown with a companion, and never given one here

NO_CWEBP = """
  ==============================================================
   WARNING: cwebp not found — full-size WebP conversion skipped.
  ==============================================================

   Existing WebPs and photo variants made by Pillow remain usable.
   Images without WebP companions fall back to JPEG/PNG. The site
   stays usable, but those images ship more bytes.

   Install the converter:

     Arch    pacman -S libwebp-utils     <- NOT libwebp; that
                                            package is the
                                            library only and
                                            has no cwebp binary
     Debian  apt install webp
     macOS   brew install webp

   Then re-run `make build`. To make this a hard failure in a
   production build, run:  engine/tools/check-site.py _site --require-webp
   (`make validate REQUIRE_WEBP=1`).
"""


def companion_of(image: Path) -> Path:
    """`lake.jpg` -> `lake.webp`: the name without its last extension."""
    return image.with_name(image.name[: -len(image.suffix)] + ".webp")


def images_under(roots: list[Path]) -> list[Path]:
    """Every image a page may show with a companion, in one order on every
    machine. The walk is whole or it raises: one that failed halfway would
    pass the checks for want of the files it did not reach. A directory
    may be named as an image is (album.JPG) and is not one; a link to an
    image is."""
    found = []

    def failed(error: OSError) -> None:
        raise error

    for root in roots:
        if not root.is_dir():
            continue
        for directory, _, names in os.walk(root, onerror=failed):
            for name in names:
                path = Path(directory) / name
                if path.suffix.lower() in SHOWN_WITH_ONE and path.is_file():
                    found.append(path)
    return sorted(found, key=os.fsencode)


class Slots:
    """Which images would be shown with each companion. A companion is
    known by where it is served: beside its image under content/, and at
    one address for both static trees, where a site file of an engine
    file's own name is served in its place."""

    def __init__(self, site_static: Path, engine_static: Path, images: list[Path]) -> None:
        self.site_static, self.engine_static = site_static, engine_static
        self.shown_with: dict[str, list[Path]] = {}
        self.slot_of: dict[Path, str] = {}
        for image in images:
            slot = self._slot(image)
            if slot is not None:
                self.slot_of[image] = slot
                self.shown_with.setdefault(slot, []).append(image)

    def _under(self, path: Path, root: Path) -> str | None:
        prefix = str(root) + os.sep
        return str(path)[len(prefix):] if str(path).startswith(prefix) else None

    def _slot(self, image: Path) -> str | None:
        companion = companion_of(image)
        rel = self._under(companion, self.engine_static)
        if rel is not None and self.engine_static != self.site_static:
            if (self.site_static / self._under(image, self.engine_static)).is_file():
                return None        # the site's file of this name is served in its place
            return "static:" + rel
        rel = self._under(companion, self.site_static)
        return "static:" + rel if rel is not None else str(companion)

    def files(self, slot: str) -> list[Path]:
        """The files that are served as the companion, or would be."""
        if not slot.startswith("static:"):
            return [Path(slot)]
        rel = slot[len("static:"):]
        both = [self.site_static / rel]
        if self.engine_static != self.site_static:
            both.append(self.engine_static / rel)
        return both

    def name(self, slot: str) -> str:
        if slot.startswith("static:"):
            return "the WebP companion served at /" + slot[len("static:"):]
        return "one WebP companion, " + show(Path(slot))

    def shared(self) -> list[str]:
        return [slot for slot, images in self.shown_with.items() if len(images) > 1]


def refuse_shared(slots: Slots, records: derived_images.Records) -> bool:
    """Say which images share a companion, and which file is in question.
    Nothing is deleted: the file may be the author's."""
    shared = slots.shared()
    for slot in shared:
        images = [show(image) for image in slots.shown_with[slot]]
        names = images[0] if len(images) == 1 else ", ".join(images[:-1]) + " and " + images[-1]
        print(f"convert-images: {names} share {slots.name(slot)}: a page showing one would be served "
              "another's pixels.\n  Rename all but one of the images.", file=sys.stderr)
        for file in slots.files(slot):
            if not os.path.lexists(file):
                continue
            entry = records.entry(file)
            if (entry is not None and entry["producer"] == PRODUCER and os.path.isfile(file)
                    and not os.path.islink(file) and entry["digest"] == derived_images.digest(file)):
                whose = (f"this script made it from {entry['source']}: delete it once the images are renamed, "
                         "and the next build makes the one that is wanted")
            else:
                whose = "it is not one this script made: if it is yours, move it aside until the images are renamed"
            print(f"  {show(file)} is there already; {whose}.", file=sys.stderr)
    return bool(shared)


def cwebp(image: Path, destination: Path) -> None:
    subprocess.run(["cwebp", "-quiet", "-q", "85", str(image), "-o", str(destination)], check=True)


def cwebp_of(taken: derived_images.Consumed, image: Path, scratch: Path, destination: Path) -> None:
    """Convert the image as it was read: cwebp is given those bytes, in a
    file of this run's own, and not the path, where another image may
    have been saved since."""
    given = scratch / image.name
    given.write_bytes(taken.data)
    cwebp(given, destination)


TRIES = 3


def image_roots() -> tuple[Path, Path, list[Path]]:
    """The site's static directory, the engine's, and every directory
    whose images this script converts."""
    site_static = Path(sitelib.SITE_ROOT) / "static"
    engine_static = Path(sitelib.engine_path("static"))
    roots = [site_static, Path(sitelib.SITE_ROOT) / "content"]
    if engine_static != site_static:
        roots.append(engine_static)
    return site_static, engine_static, roots


def plan(records: derived_images.Records) -> tuple[Slots, list[Path]]:
    site_static, engine_static, roots = image_roots()
    images = images_under(roots)
    slots = Slots(site_static, engine_static, images)
    # What cwebp reads: the JPEGs and PNGs, less the photographs' variants,
    # which are another tool's outputs and get their WebPs from it.
    convertible = [image for image in images
                   if image.suffix.lower() in CONVERTED and not records.made_by(image, THUMBNAILS)]
    return slots, convertible


def convert(records: derived_images.Records, convertible: list[Path]) -> int:
    converted = present = 0
    left: list[Path] = []
    unsettled: list[Path] = []
    with tempfile.TemporaryDirectory(prefix="convert-webp-") as scratch:
        for image in convertible:
            companion = companion_of(image)
            if records.declared_for(companion) is not None:
                present += 1
            elif not records.may_write(companion, PRODUCER, image):
                left.append(companion)
            elif records.fresh(companion, PRODUCER, image):
                present += 1
            else:
                print(f"  webp  {show(image)}")
                settled = convert_one(records, image, companion, Path(scratch))
                if settled is None:
                    left.append(companion)
                    continue
                if not settled:
                    unsettled.append(image)
                converted += 1
    print(f"convert-images: {converted} converted, {present} already present.")
    for image in unsettled:
        print(f"convert-images: {show(image)} was saved again each of the {TRIES} times it was converted; its "
              "companion is of an earlier save, recorded as that, and is not shown with it. The next build "
              "converts it again.", file=sys.stderr)
    report_left(left)
    return 0


def convert_one(records: derived_images.Records, image: Path, companion: Path, scratch: Path) -> bool | None:
    """Make the companion of the image as it is read, and say so in the
    record; again, when the image was saved anew meanwhile, since the
    companion is then of the image before. Is the companion, in the end,
    of the image that is there? None means the destination was claimed
    during encoding and was left alone."""
    for attempt in range(TRIES):
        try:
            taken = derived_images.consume(image)
        except FileNotFoundError:
            if attempt == 0:
                raise
            return True             # gone since: its companion is of an image no longer there
        # The encoded file and the public name must share a filesystem.
        with tempfile.TemporaryDirectory(dir=companion.parent, prefix=".convert-") as stage:
            encoded = Path(stage) / "companion.tmp"
            cwebp_of(taken, image, scratch, encoded)
            if not records.publish(encoded, companion, PRODUCER, image, taken, derived_images.digest(encoded)):
                return None
        if derived_images.still(image, taken):
            return True
    return False


def report_left(left: list[Path]) -> None:
    if not left:
        return
    print(f"convert-images: {len(left)} WebP file(s) beside an image are not recorded as made from it by this "
          "script, and are left as they are; the image is shown without one.", file=sys.stderr)
    for companion in left[:5]:
        print(f"  {show(companion)}", file=sys.stderr)
    if len(left) > 5:
        print(f"  … and {len(left) - 5} more", file=sys.stderr)
    print("  `make adopt-images` records those a fresh conversion reproduces byte for byte. One made by hand "
          "is shown\n  with its image once declared: python3 engine/tools/derived_images.py declare FILE.webp",
          file=sys.stderr)


def adopt_all(records: derived_images.Records, *, dry_run: bool) -> tuple[int, bool]:
    """Enter the companions already there that this script would make
    the same, byte for byte: only its own output is that. The rest are
    listed, and stay unrecorded. So is one whose entry is this script's,
    of this image, and has other bytes than the file: what a run leaves
    when it is stopped between putting the file there and saving the
    record. Saves nothing itself
    (tools/derived_images.py adopt does, unless it is a preview), and
    returns how many it entered and whether it could not do its work."""
    if shutil.which("cwebp") is None:
        print("convert-images: adoption compares fresh conversions, and there is no cwebp to make them.",
              file=sys.stderr)
        return 0, True
    slots, convertible = plan(records)
    adopted = repaired = edited = differ = partial = 0
    with tempfile.TemporaryDirectory(prefix="adopt-webp-") as scratch:
        fresh = Path(scratch) / "fresh.webp"
        for image in convertible:
            companion = companion_of(image)
            # One with no entry, or with an entry that does not say which
            # bytes its image had (an earlier form of the record), or with
            # an entry of this script's for this image that has other
            # bytes than the file.
            known = records.entry(companion) is not None
            stale = known and records.stale(companion, PRODUCER, image)
            if (not companion.is_file() or companion.is_symlink()
                    or (known and not stale and not records.incomplete(companion, PRODUCER, image))
                    or records.declared_for(companion) is not None
                    or len(slots.shown_with.get(slots.slot_of.get(image, ""), [])) > 1):
                continue
            taken = derived_images.consume(image)
            cwebp_of(taken, image, Path(scratch), fresh)
            there = companion.read_bytes()
            if fresh.read_bytes() == there:
                if stale:
                    repaired += 1
                    print(f"  repair   {show(companion)}  (what cwebp makes of {show(image)} now; its entry "
                          "said other bytes)")
                else:
                    adopted += 1
                records.wrote(companion, PRODUCER, image, taken, derived_images.digest_of(there))
            elif stale:
                edited += 1
            elif known:
                partial += 1
            else:
                differ += 1
                print(f"  differs  {show(companion)}  (not what cwebp makes of {show(image)} now)")
    verb = "would be recorded" if dry_run else "recorded"
    print(f"convert-images: {adopted} companion(s) {verb} as this script's; {differ} differ and are left "
          "unrecorded.")
    if repaired:
        print(f"  {repaired} more, whose entries said other bytes, are what cwebp makes of their images now (a "
              f"run was stopped before it\n  saved the record): their entries {'would be' if dry_run else 'were'} "
              "brought up to them.")
    if edited:
        print(f"  {edited} more have an entry of this script's and other bytes than it says, and are not what "
              "cwebp makes of their\n  images now: left as they are, as a file its author changed is.")
    if partial:
        print(f"  {partial} more are this script's by an entry that does not say which bytes their image had, and "
              "are not what cwebp\n  makes of it now: left as recorded, and converted again by the next build "
              "that cannot tell the image is the same.")
    if differ:
        print("  One that differs is yours, or was made by another cwebp. Look at it; delete it to have it made "
              "again,\n  or declare it yours: python3 engine/tools/derived_images.py declare FILE.webp")
    return adopted + repaired, False


def main(argv: list[str] | None = None) -> int:
    argparse.ArgumentParser(description="Make a WebP companion beside every local JPEG and PNG.").parse_args(argv)
    try:
        # Each tree is held from here until the records are saved.
        with derived_images.Records.held() as records:
            # First of all: a file a stopped run took aside is back in
            # its place before its entry is looked for, or its image
            # converted.
            records.restore_displaced(image_roots()[2])
            records.forget_missing()
            slots, convertible = plan(records)
            if refuse_shared(slots, records):
                return 1
            if shutil.which("cwebp") is None:
                print(NO_CWEBP, file=sys.stderr)
                return 0
            return convert(records, convertible)
    except derived_images.RecordError as error:
        print(f"convert-images: {error}; nothing was checked or converted.", file=sys.stderr)
        return 1
    except OSError as error:
        print(f"convert-images: could not list the images, or write one ({error}).", file=sys.stderr)
        return 1
    except subprocess.CalledProcessError as error:
        print(f"convert-images: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
