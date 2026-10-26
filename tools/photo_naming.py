"""The responsive variants tools/generate-thumbnails.py writes beside each
photograph: `photo.jpg` -> `photo.w480.jpg`, `photo.w960.jpg`,
`photo.w1440.jpg`, each only when the source is wider.

The Python tools read the ladder here; build/Contexts.hs keeps its own
(photoVariantWidths) for the srcset it emits, and tests/test_photo_naming.py
holds the two together through `site shared-rules`. Adding a rung is a
change on both sides.
"""

from __future__ import annotations

import re
from pathlib import Path

WIDTHS: tuple[int, ...] = (480, 960, 1440)

# The variant marker, as an anchored suffix. Written out rather than built
# from WIDTHS so that a grep for `960` finds it; the test checks they agree.
VARIANT_RE = re.compile(r"\.w(480|960|1440)\.(jpe?g|png)$", re.IGNORECASE)


def is_variant(path: Path) -> bool:
    """True for a variant, which the sidecar extractors skip: it is a
    pixel reduction of a source that has sidecars of its own."""
    return VARIANT_RE.search(path.name) is not None


def variant_path(source: Path, width: int) -> Path:
    """`photo.jpg`, 960 -> `photo.w960.jpg` (same directory)."""
    stem = source.name[: -len(source.suffix)]
    return source.with_name(f"{stem}.w{width}{source.suffix}")


def variant_width(variant: Path) -> int:
    """`photo.w960.jpg` -> 960."""
    match = VARIANT_RE.search(variant.name)
    if match is None:  # pragma: no cover — callers check is_variant first
        raise ValueError(f"not a variant: {variant}")
    return int(match.group(1))


def source_for_variant(variant: Path) -> Path:
    """`photo.w960.jpg` -> `photo.jpg` (the file it was derived from)."""
    match = VARIANT_RE.search(variant.name)
    if match is None:  # pragma: no cover — callers check is_variant first
        raise ValueError(f"not a variant: {variant}")
    head = variant.name[: match.start()]
    return variant.with_name(f"{head}.{match.group(2)}")
