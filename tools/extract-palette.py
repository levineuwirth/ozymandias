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

import io
import sys
from pathlib import Path

from PIL import Image
from colorthief import ColorThief

sys.path.insert(0, str(Path(__file__).resolve().parent))
import photo_sidecars  # noqa: E402
import sitelib  # noqa: E402

SITE_ROOT = sitelib.SITE_ROOT
CONTENT_DIR = SITE_ROOT / "content" / "photography"
TOOL = "extract-palette"

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


# Longest edge, in pixels, that the palette is computed from.
#
# Clustering a 2400x1600 image to find five colours is almost entirely wasted
# work: the answer is a property of the distribution, and a faithful 1/8-scale
# sample has the same distribution. Pillow's draft() asks the JPEG decoder for
# a reduced-scale decode, which is a different and much cheaper operation than
# decoding fully and then resizing. Measured on a 2400px frame: 0.297 s before,
# 0.191 s after, with the dominant swatch moving from #465129 to #47512a — a
# difference no eye will find on a 40px strip.
PALETTE_SAMPLE_PX = 320


def _extract_palette(image: Path) -> list[str]:
    """Return up to N_SWATCHES hex colors, in colorthief's dominance order."""
    try:
        with Image.open(image) as im:
            im.draft("RGB", (PALETTE_SAMPLE_PX, PALETTE_SAMPLE_PX))
            sample = im.convert("RGB")
            buf = io.BytesIO()
            sample.save(buf, format="JPEG", quality=90)
        buf.seek(0)
        ct = ColorThief(buf)
        # quality=1 inspects every pixel of the sample. On an image this small
        # that is cheaper than the stride-sampling QUALITY exists to avoid, and
        # it removes sampling as a source of run-to-run variation.
        palette = ct.get_palette(color_count=N_SWATCHES, quality=1)
    except Exception:
        # Any decoder that will not co-operate with a reduced-scale read falls
        # back to the original path rather than losing its palette.
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
    counts = photo_sidecars.run(
        TOOL, ".palette.yaml", lambda image: {"palette": _extract_palette(image)},
        roots=[CONTENT_DIR], image_exts=IMAGE_EXTS, argv=sys.argv[1:])
    print(
        f"extract-palette: {counts['written']} written, {counts['skipped']} skipped, "
        f"{counts['failed']} failed",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
