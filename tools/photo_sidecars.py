"""The walk the sidecar extractors share (extract-exif, extract-palette,
extract-dimensions): which images get a sidecar, when one is stale, and how
it is written. Each extractor supplies its suffix, its reader and the key
order it writes.

Responsive variants (tools/photo_naming.py) get none: they are pixel
reductions of a source that already has its own sidecars, and nothing reads
a sidecar for a srcset candidate, so extracting for them would add ~1100
files of churn for no consumer.
"""

from __future__ import annotations

import sys
import traceback
from collections.abc import Callable, Iterable
from pathlib import Path
from typing import Any

import yaml

sys.path.insert(0, str(Path(__file__).resolve().parent))
import front_matter  # noqa: E402
import photo_naming  # noqa: E402
import sitelib  # noqa: E402

SITE_ROOT = sitelib.SITE_ROOT


def sidecar_path(image: Path, suffix: str) -> Path:
    """`photo.jpg`, `.exif.yaml` -> `photo.jpg.exif.yaml`."""
    return image.with_suffix(image.suffix + suffix)


def is_stale(image: Path, sidecar: Path) -> bool:
    return not sidecar.exists() or image.stat().st_mtime > sidecar.stat().st_mtime


def write_yaml(path: Path, data: dict[str, Any], keys: Iterable[str] | None = None) -> None:
    """Write a sidecar, its keys in `keys` order when given (so a diff reads
    the same way across regenerations). Not durable: sidecars are
    regenerated from the photo on the next build, so a lost rename costs
    one re-extraction, not data."""
    if keys is not None:
        data = {k: data[k] for k in keys if k in data}
    with sitelib.atomic_path(path, durable=False) as tmp, tmp.open("w", encoding="utf-8") as f:
        yaml.safe_dump(data, f, sort_keys=False, allow_unicode=True)


def candidates(tool: str, argv: list[str], roots: Iterable[Path]) -> list[Path]:
    """Files to consider: the ones named, or everything under `roots`.

    `make build` passes no arguments and wants the full walk; the photo
    importers name the files they just wrote, which keeps a bulk import
    linear instead of quadratic."""
    if not argv:
        return [p for root in roots if root.exists() for p in sorted(root.rglob("*"))]
    out = []
    for a in argv:
        p = Path(a)
        if not p.is_absolute():
            p = SITE_ROOT / p
        if p.exists():
            out.append(p)
        else:
            print(f"{tool}: no such file: {p}", file=sys.stderr)
    return out


def run(tool: str, suffix: str, read: Callable[[Path], dict[str, Any]], *,
        roots: Iterable[Path], image_exts: set[str], argv: list[str],
        keys: Iterable[str] | None = None,
        prefetch: Callable[[list[Path]], None] | None = None) -> dict[str, int]:
    """Write a fresh sidecar for every source image whose sidecar is missing
    or older than it, and count what happened. A failure to read one image
    is reported with its traceback and the walk goes on. `prefetch` is
    handed the stale images first (extract-exif batches its exiftool
    calls)."""
    counts = {"written": 0, "skipped": 0, "failed": 0, "variants": 0}
    images = []
    for image in candidates(tool, argv, roots):
        if (image.suffix.lower() not in image_exts
                or image.name.startswith(".") or image.name.endswith(".tmp")):
            continue
        if photo_naming.is_variant(image):
            counts["variants"] += 1
            continue
        images.append(image)
    if prefetch is not None:
        prefetch([i for i in images if is_stale(i, sidecar_path(i, suffix))])
    for image in images:
        sidecar = sidecar_path(image, suffix)
        if not is_stale(image, sidecar):
            counts["skipped"] += 1
            continue
        try:
            data = read(image)
        except Exception as e:  # noqa: BLE001 — keep walking
            print(f"{tool}: {image}: {e}", file=sys.stderr)
            traceback.print_exc(file=sys.stderr)
            counts["failed"] += 1
            continue
        write_yaml(sidecar, data, keys)
        counts["written"] += 1
    return counts


# The EXIF fields a photograph's front matter carries, in file order. The
# sidecar is gitignored and the front matter is tracked, so the importers
# copy these across once, where they stay editable.
EXIF_FRONT_MATTER_KEYS = ["captured", "camera", "lens", "focal-length"]


def exif_front_matter(sidecar: Path) -> list[str]:
    """The front-matter lines for a photograph's EXIF sidecar, for
    scaffold-photos.py and import-photo.sh. `geo` is deliberately not among
    them: the sidecar holds full-precision coordinates, and Hakyll rounds
    them at render time, which is the privacy gate."""
    if not sidecar.exists():
        return []
    try:
        data = yaml.safe_load(sidecar.read_text()) or {}
    except (OSError, yaml.YAMLError) as e:
        print(f"photo_sidecars: {sidecar}: unreadable, no camera fields copied: {e}",
              file=sys.stderr)
        return []
    keys = list(EXIF_FRONT_MATTER_KEYS)
    # extract-exif composes `exposure` only when shutter, aperture and ISO are
    # all present; prefer it, and fall back to whichever parts were readable.
    # Emitting both would say the same thing twice.
    keys += ["exposure"] if data.get("exposure") else ["shutter", "aperture", "iso"]
    return [front_matter.field(k, data[k]) for k in keys if data.get(k) not in (None, "")]
