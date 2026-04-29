#!/usr/bin/env python3
"""
extract-palette.py — Build-time 5-color palette sidecar for photography.

Walks content/photography/**/*.{jpg,jpeg,png} and writes a
{photo}.palette.yaml sidecar alongside each image, containing five
hex colors derived from the photograph via colorthief's k-means-like
quantisation. The sidecar is consumed by photographyCtx in Hakyll
and rendered as the thin <div class="photo-palette"> strip beneath
each photo.

Frontmatter `palette:` always wins. Authors can override the auto
extraction for artistic reasons (e.g. exposing brand-aligned tones
that aren't statistically dominant in the pixels). The sidecar is
the fallback so authors don't need to write hex codes by hand.

Staleness check: skips an image whose sidecar mtime > image mtime.

Called by `make build` when .venv exists. Per-image failures are
logged and the rest of the walk continues; the build never fails on
a palette extraction error.
"""

from __future__ import annotations

import sys
from pathlib import Path
from typing import Any

import yaml
from colorthief import ColorThief

REPO_ROOT = Path(__file__).parent.parent
CONTENT_DIR = REPO_ROOT / "content" / "photography"

IMAGE_EXTS = {".jpg", ".jpeg", ".png"}

# Number of swatches in the rendered strip. Five matches the design in
# PHOTOGRAPHY.md and the existing `photo-palette` CSS, which sets
# `display: flex; height: 0.75rem;` and divides the bar evenly. Bumping
# this requires a CSS revisit — the bar reads as a unified strip up to
# about 7 swatches; beyond that the bands become too narrow to perceive.
N_SWATCHES = 5

# colorthief's quality knob: lower = better palette but slower. The
# default of 10 is a reasonable trade-off; 1 is exhaustive.
QUALITY = 10


def _hex(rgb: tuple[int, int, int]) -> str:
    return "#{:02x}{:02x}{:02x}".format(*rgb)


def _sidecar_path(image: Path) -> Path:
    return image.with_suffix(image.suffix + ".palette.yaml")


def _is_stale(image: Path, sidecar: Path) -> bool:
    if not sidecar.exists():
        return True
    return image.stat().st_mtime > sidecar.stat().st_mtime


def _atomic_write_yaml(path: Path, data: dict[str, Any]) -> None:
    tmp = path.with_suffix(path.suffix + ".tmp")
    with tmp.open("w", encoding="utf-8") as f:
        yaml.safe_dump(data, f, sort_keys=False, allow_unicode=True)
    tmp.replace(path)


def _extract_palette(image: Path) -> list[str]:
    """Return up to N_SWATCHES hex colors, in colorthief's dominance order."""
    ct = ColorThief(str(image))
    palette = ct.get_palette(color_count=N_SWATCHES, quality=QUALITY)
    # colorthief sometimes returns one fewer entry than requested for
    # very low-color images; just take what we got.
    return [_hex(rgb) for rgb in palette[:N_SWATCHES]]


def main() -> int:
    if not CONTENT_DIR.exists():
        print(
            f"extract-palette: {CONTENT_DIR} does not exist — skipping.",
            file=sys.stderr,
        )
        return 0

    written = 0
    skipped = 0
    failed = 0

    for image in sorted(CONTENT_DIR.rglob("*")):
        if image.suffix.lower() not in IMAGE_EXTS:
            continue
        if image.name.startswith(".") or image.name.endswith(".tmp"):
            continue

        sidecar = _sidecar_path(image)
        if not _is_stale(image, sidecar):
            skipped += 1
            continue

        try:
            palette = _extract_palette(image)
        except Exception as e:  # noqa: BLE001 — keep walking
            print(f"extract-palette: {image}: {e}", file=sys.stderr)
            failed += 1
            continue

        _atomic_write_yaml(sidecar, {"palette": palette})
        written += 1

    print(
        f"extract-palette: {written} written, {skipped} skipped, {failed} failed",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
