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
from typing import Any

import yaml

REPO_ROOT = Path(__file__).parent.parent

# Roots to walk. content/photography/ also gets visited (its photos
# become double-sidecared with both .exif.yaml and .dims.yaml) — that's
# harmless and keeps the contract uniform: "every raster file has a
# .dims.yaml". The few extra bytes of YAML are immaterial.
WALK_ROOTS = [
    REPO_ROOT / "static" / "images",
    REPO_ROOT / "content",
]

IMAGE_EXTS = {".jpg", ".jpeg", ".png", ".gif"}


def _sidecar_path(image: Path) -> Path:
    return image.with_suffix(image.suffix + ".dims.yaml")


def _is_stale(image: Path, sidecar: Path) -> bool:
    if not sidecar.exists():
        return True
    return image.stat().st_mtime > sidecar.stat().st_mtime


def _atomic_write_yaml(path: Path, data: dict[str, Any]) -> None:
    tmp = path.with_suffix(path.suffix + ".tmp")
    with tmp.open("w", encoding="utf-8") as f:
        # Preserve a stable key order (width before height) so a manual
        # diff stays easy to read across regenerations.
        ordered = {k: data[k] for k in ("width", "height") if k in data}
        yaml.safe_dump(ordered, f, sort_keys=False, allow_unicode=True)
    tmp.replace(path)


def _read_dimensions(image: Path) -> dict[str, int]:
    from PIL import Image

    with Image.open(image) as img:
        width, height = img.size
        return {"width": int(width), "height": int(height)}


def _walk_one_root(root: Path, counters: dict[str, int]) -> None:
    if not root.exists():
        return
    for image in sorted(root.rglob("*")):
        if image.suffix.lower() not in IMAGE_EXTS:
            continue
        # Skip dotfiles, tmp files, and the .webp companions produced
        # by tools/convert-images.sh (their extension is .webp so they
        # already wouldn't match IMAGE_EXTS, but be explicit).
        if image.name.startswith(".") or image.name.endswith(".tmp"):
            continue

        sidecar = _sidecar_path(image)
        if not _is_stale(image, sidecar):
            counters["skipped"] += 1
            continue

        try:
            data = _read_dimensions(image)
        except Exception as e:  # noqa: BLE001 — keep walking
            print(f"extract-dimensions: {image}: {e}", file=sys.stderr)
            counters["failed"] += 1
            continue

        _atomic_write_yaml(sidecar, data)
        counters["written"] += 1


def main() -> int:
    counters = {"written": 0, "skipped": 0, "failed": 0}

    for root in WALK_ROOTS:
        _walk_one_root(root, counters)

    print(
        "extract-dimensions: "
        f"{counters['written']} written, "
        f"{counters['skipped']} skipped, "
        f"{counters['failed']} failed",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
