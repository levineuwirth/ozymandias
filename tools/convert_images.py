"""convert_images.py — a WebP companion beside every local JPEG and PNG.

Run as tools/convert-images.sh (`make convert-images`, and inside `make
build`). Walks the site's static/ and content/ and the engine's static/,
and has cwebp make `lake.webp` beside `lake.jpg`. An extension counts in
either case (IMG_0042.JPG, as a camera writes it).

Whose file a WebP is. The script writes a companion where there is none,
and over one it made itself: the ownership record (tools/derived_images.py)
says which those are, by the image they were made from and the bytes last
written. Any other WebP of that name is the author's (made by hand, shown
by a page as an image of its own, or simply older than the record) and is
left exactly as it is, however old. The generator shows an image with a
companion only when the record or the author's declaration pairs them
(build/DerivedImages.hs), so an unrecorded one is not shown in the image's
place either. What was left alone is counted at the end, with what to do.

One companion, one image. A page shows map.png, map.JPG or map.gif with
map.webp, so two of them beside each other would have one companion
between them. The same holds across static/ and the engine's static/,
which are served as one tree with the site's file over the engine's: the
site's images/color.png and the engine's images/color.JPG would both be
shown with /images/color.webp. The script refuses such images, naming
them and any companion already there, before it converts anything, with
or without cwebp. It deletes nothing.

    --adopt      record the companions that are already there and that a
                 fresh conversion reproduces byte for byte; list the rest
    --dry-run    with --adopt: say what would be recorded, record nothing

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


def plan(records: derived_images.Records) -> tuple[Slots, list[Path]]:
    site_static = Path(sitelib.SITE_ROOT) / "static"
    engine_static = Path(sitelib.engine_path("static"))
    roots = [site_static, Path(sitelib.SITE_ROOT) / "content"]
    if engine_static != site_static:
        roots.append(engine_static)
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
    for image in convertible:
        companion = companion_of(image)
        if records.declared_for(companion) is not None:
            present += 1
        elif not os.path.lexists(companion) or (
                records.owns(companion, PRODUCER, image)
                and image.stat().st_mtime > companion.stat().st_mtime):
            print(f"  webp  {show(image)}")
            # Written under a temporary name and moved: an interrupted
            # cwebp leaves nothing where the companion goes.
            with sitelib.atomic_path(companion, durable=False) as tmp:
                cwebp(image, tmp)
            records.wrote(companion, PRODUCER, image)
            converted += 1
        elif records.owns(companion, PRODUCER, image):
            present += 1
        else:
            left.append(companion)
    print(f"convert-images: {converted} converted, {present} already present.")
    report_left(left)
    return 0


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


def adopt(records: derived_images.Records, slots: Slots, convertible: list[Path], *, dry_run: bool) -> int:
    """Record the companions already there that this script would make
    the same, byte for byte: only its own output is that. The rest are
    listed, and stay unrecorded."""
    adopted = differ = 0
    with tempfile.TemporaryDirectory(prefix="adopt-webp-") as scratch:
        fresh = Path(scratch) / "fresh.webp"
        for image in convertible:
            companion = companion_of(image)
            if (not companion.is_file() or companion.is_symlink() or records.entry(companion) is not None
                    or records.declared_for(companion) is not None
                    or len(slots.shown_with.get(slots.slot_of.get(image, ""), [])) > 1):
                continue
            cwebp(image, fresh)
            if fresh.read_bytes() == companion.read_bytes():
                adopted += 1
                if not dry_run:
                    records.wrote(companion, PRODUCER, image, save=False)
            else:
                differ += 1
                print(f"  differs  {show(companion)}  (not what cwebp makes of {show(image)} now)")
    records.save_all()
    verb = "would be recorded" if dry_run else "recorded"
    print(f"convert-images: {adopted} companion(s) {verb} as this script's; {differ} differ and are left "
          "unrecorded.")
    if differ:
        print("  One that differs is yours, or was made by another cwebp. Look at it; delete it to have it made "
              "again,\n  or declare it yours: python3 engine/tools/derived_images.py declare FILE.webp")
    if adopted and not dry_run:
        derived_images.request_full_rebuild()
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Make a WebP companion beside every local JPEG and PNG.")
    parser.add_argument("--adopt", action="store_true",
                        help="record the companions already there that a fresh conversion reproduces")
    parser.add_argument("--dry-run", action="store_true", help="with --adopt: record nothing")
    args = parser.parse_args(argv)
    try:
        records = derived_images.Records()
        records.forget_missing()
        slots, convertible = plan(records)
    except derived_images.RecordError as error:
        print(f"convert-images: {error}; nothing was checked or converted.", file=sys.stderr)
        return 1
    except OSError as error:
        print(f"convert-images: could not list the images ({error}); nothing was checked or converted.",
              file=sys.stderr)
        return 1
    if refuse_shared(slots, records):
        return 1
    if shutil.which("cwebp") is None:
        if args.adopt:
            print("convert-images: --adopt compares fresh conversions, and there is no cwebp to make them.",
                  file=sys.stderr)
            return 1
        print(NO_CWEBP, file=sys.stderr)
        return 0
    try:
        if args.adopt:
            return adopt(records, slots, convertible, dry_run=args.dry_run)
        return convert(records, convertible)
    except (OSError, subprocess.CalledProcessError, derived_images.RecordError) as error:
        print(f"convert-images: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
