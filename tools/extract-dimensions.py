#!/usr/bin/env python3
"""
extract-dimensions.py — Build-time pixel-dimension sidecar generator.

Walks @static/images/@ and @content/**@ for raster image files
(JPEG / PNG / GIF) and writes a @{image}.dims.yaml@ sidecar alongside
each one containing the file's pixel width and height. Consumed by
@build/Filters/Images.hs@, which attaches matching @width@ and
@height@ attributes to every <img> tag at compile time — preventing
cumulative layout shift while images load.

This is the body-image counterpart to @extract-exif.py@, which writes
photography-specific @{image}.exif.yaml@ sidecars (containing
dimensions plus camera / lens / etc.). The two complement each other:
photography templates read width / height through the EXIF sidecar
via @photographyCtx@; everything else (essay figures, blog images,
inline images) gets dimensions through @{image}.dims.yaml@ via the
filter.

Strategy:
  * Pillow's @Image.size@ is independent of EXIF, so synthetic
    images (ImageMagick gradients, GIMP exports) and EXIF-stripped
    JPEGs both yield correct dimensions.
  * Staleness check: skip when sidecar mtime > image mtime.
  * Per-image failures are logged and the walk continues; the build
    never fails on a dimensions extraction error.

Called by `make build` when .venv exists. Failures on individual
images are logged and the rest of the walk continues.
"""

from __future__ import annotations

import sys
from pathlib import Path


sys.path.insert(0, str(Path(__file__).resolve().parent))
import photo_sidecars  # noqa: E402
import sitelib  # noqa: E402

SITE_ROOT = sitelib.SITE_ROOT

# Roots to walk. content/photography/ also gets visited (its photos
# become double-sidecared with both .exif.yaml and .dims.yaml) — that's
# harmless and keeps the contract uniform: "every raster file has a
# .dims.yaml". The few extra bytes of YAML are immaterial.
WALK_ROOTS = list(dict.fromkeys([
    sitelib.engine_path("static/images"),  # the engine's (link icons)
    SITE_ROOT / "static" / "images",       # the site's
    SITE_ROOT / "content",
]))

IMAGE_EXTS = {".jpg", ".jpeg", ".png", ".gif"}


def _read_dimensions(image: Path) -> dict[str, int]:
    from PIL import Image

    with Image.open(image) as img:
        width, height = img.size
        return {"width": int(width), "height": int(height)}


def main() -> int:
    counts = photo_sidecars.run(
        "extract-dimensions", ".dims.yaml", _read_dimensions,
        roots=WALK_ROOTS, image_exts=IMAGE_EXTS, argv=[],
        # Width before height, so a diff reads the same way every time.
        keys=("width", "height"))
    print(
        "extract-dimensions: "
        f"{counts['written']} written, "
        f"{counts['skipped']} skipped, "
        f"{counts['variants']} responsive variants ignored, "
        f"{counts['failed']} failed",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
