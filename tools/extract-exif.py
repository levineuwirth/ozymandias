#!/usr/bin/env python3
"""
extract-exif.py — Build-time EXIF sidecar generator for photography.

Walks content/photography/**/*.{jpg,jpeg,png} and writes a
{photo}.exif.yaml sidecar alongside each image. The Hakyll context in
build/Contexts.hs reads these sidecars and merges their fields into
the photographyCtx so authors don't have to hand-write camera / lens /
exposure / captured-date in frontmatter for digital photos.

Frontmatter always wins. The sidecar is a strict fallback — present
to populate fields the author chose not to write. Film scans typically
have no EXIF; the sidecar is still written but with an empty body, and
the author hand-writes the relevant fields in frontmatter.

Strategy:

  1. Prefer exiftool when available — Perl-based, ships in distro
     repos, handles every camera vendor's tag dialect (incl. RAW).
  2. Fall back to Pillow's EXIF reader — pure Python, narrower
     coverage, but always available via the project's .venv.

Staleness check: skips an image whose sidecar mtime > image mtime.
This means re-running the tool is idempotent and cheap.

GPS coordinates are written to the sidecar at full precision; the
geo-precision rounding (`exact | km | city | hidden`) is applied in
Hakyll at the consuming end, against each photo's frontmatter
`geo-precision:` value. The sidecar is the source of truth; the
consumer is the privacy gate.

Called by `make build` when .venv exists. Failures on individual
images are logged and the rest of the walk continues.
"""

from __future__ import annotations

import json
import shutil
import subprocess
import sys
from fractions import Fraction
from pathlib import Path
from typing import Any

import yaml

REPO_ROOT = Path(__file__).parent.parent
CONTENT_DIR = REPO_ROOT / "content" / "photography"

IMAGE_EXTS = {".jpg", ".jpeg", ".png"}

# ---------------------------------------------------------------------------
# Field normalisation
# ---------------------------------------------------------------------------

# Mapping from EXIF field names (as exposed by exiftool / Pillow) to the
# sidecar keys consumed by Hakyll. Hakyll's fields are deliberately
# lowercase-with-hyphens, matching the photographyCtx convention.
SIDECAR_KEYS = [
    "camera",
    "lens",
    "exposure",
    "shutter",
    "aperture",
    "iso",
    "focal-length",
    "captured",
    "geo",
    # Pixel dimensions of the delivered (resized, EXIF-stripped) JPEG.
    # Threaded through to the Hakyll photographyCtx and emitted as
    # width / height attrs on every <img> tag — prevents cumulative
    # layout shift while photos load.
    "width",
    "height",
]


def _format_shutter(speed: float) -> str:
    """Render shutter speed as "1/125" or "0.5s" depending on magnitude."""
    if speed <= 0:
        return ""
    if speed >= 1.0:
        return f"{speed:g}s"
    denom = round(1.0 / speed)
    return f"1/{denom}"


def _format_aperture(value: float) -> str:
    if value <= 0:
        return ""
    # Common aperture values display with at most one decimal place.
    if abs(value - round(value)) < 0.05:
        return f"f/{int(round(value))}"
    return f"f/{value:.1f}"


def _format_focal(value: float) -> str:
    if value <= 0:
        return ""
    return f"{int(round(value))}mm"


def _build_exposure_string(
    shutter: str | None,
    aperture: str | None,
    iso: int | None,
) -> str | None:
    """Compose "1/125 f/8 ISO 400" from individual fields when present."""
    parts: list[str] = []
    if shutter:
        parts.append(shutter)
    if aperture:
        parts.append(aperture)
    if iso:
        parts.append(f"ISO {iso}")
    return " ".join(parts) if parts else None


# ---------------------------------------------------------------------------
# exiftool path
# ---------------------------------------------------------------------------


def _exiftool_available() -> bool:
    return shutil.which("exiftool") is not None


def _read_exif_via_exiftool(image: Path) -> dict[str, Any]:
    """Invoke exiftool and return a dict of normalised sidecar keys.

    exiftool's `-json` output is a list of objects; we parse the first
    entry. Numeric values come through as numbers; text values as
    strings. We accept missing keys silently.
    """
    result = subprocess.run(
        [
            "exiftool",
            "-json",
            "-Make",
            "-Model",
            "-LensModel",
            "-LensSpec",
            "-LensInfo",
            "-ExposureTime",
            "-FNumber",
            "-ISO",
            "-FocalLength",
            "-FocalLengthIn35mmFormat",
            "-DateTimeOriginal",
            "-CreateDate",
            "-GPSLatitude",
            "-GPSLongitude",
            "-GPSLatitudeRef",
            "-GPSLongitudeRef",
            "-ImageWidth",
            "-ImageHeight",
            "-n",  # numeric output for shutter/aperture/GPS/dimensions
            str(image),
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode != 0:
        return {}
    try:
        data = json.loads(result.stdout)
    except json.JSONDecodeError:
        return {}
    if not data:
        return {}
    raw = data[0]

    out: dict[str, Any] = {}

    make = (raw.get("Make") or "").strip()
    model = (raw.get("Model") or "").strip()
    if make and model and not model.lower().startswith(make.lower()):
        out["camera"] = f"{make} {model}".strip()
    elif model:
        out["camera"] = model
    elif make:
        out["camera"] = make

    lens = (
        raw.get("LensModel")
        or raw.get("LensSpec")
        or raw.get("LensInfo")
        or ""
    ).strip()
    if lens:
        out["lens"] = lens

    shutter_secs = raw.get("ExposureTime")
    if isinstance(shutter_secs, (int, float)) and shutter_secs > 0:
        out["shutter"] = _format_shutter(float(shutter_secs))

    aperture = raw.get("FNumber")
    if isinstance(aperture, (int, float)) and aperture > 0:
        out["aperture"] = _format_aperture(float(aperture))

    iso = raw.get("ISO")
    if isinstance(iso, int) and iso > 0:
        out["iso"] = iso

    focal = raw.get("FocalLength")
    if isinstance(focal, (int, float)) and focal > 0:
        out["focal-length"] = _format_focal(float(focal))

    captured_raw = raw.get("DateTimeOriginal") or raw.get("CreateDate")
    if isinstance(captured_raw, str) and captured_raw:
        # exiftool format is "YYYY:MM:DD HH:MM:SS"; we want ISO date only.
        date_part = captured_raw.split(" ", 1)[0].replace(":", "-")
        if len(date_part) == 10:
            out["captured"] = date_part

    lat = raw.get("GPSLatitude")
    lon = raw.get("GPSLongitude")
    if isinstance(lat, (int, float)) and isinstance(lon, (int, float)):
        # exiftool with -n returns signed decimals already.
        out["geo"] = [round(float(lat), 6), round(float(lon), 6)]

    width = raw.get("ImageWidth")
    height = raw.get("ImageHeight")
    if isinstance(width, int) and width > 0:
        out["width"] = width
    if isinstance(height, int) and height > 0:
        out["height"] = height

    exposure = _build_exposure_string(
        out.get("shutter"), out.get("aperture"), out.get("iso")
    )
    if exposure:
        out["exposure"] = exposure

    return out


# ---------------------------------------------------------------------------
# Pillow fallback path
# ---------------------------------------------------------------------------


def _pillow_rational(value: Any) -> float | None:
    """Pillow can return EXIF rationals as IFDRational, tuples, or floats."""
    if value is None:
        return None
    try:
        if isinstance(value, tuple) and len(value) == 2:
            num, den = value
            return float(num) / float(den) if den else None
        return float(Fraction(value).limit_denominator())
    except (TypeError, ValueError, ZeroDivisionError):
        try:
            return float(value)
        except (TypeError, ValueError):
            return None


def _gps_to_decimal(coord: Any, ref: Any) -> float | None:
    """Pillow GPS coords come as ((deg_n, deg_d), (min_n, min_d), (sec_n, sec_d))."""
    if not coord:
        return None
    try:
        deg = float(coord[0])
        minutes = float(coord[1])
        seconds = float(coord[2])
        decimal = deg + minutes / 60.0 + seconds / 3600.0
        if isinstance(ref, str) and ref in ("S", "W"):
            decimal = -decimal
        return decimal
    except (TypeError, ValueError, IndexError):
        return None


def _read_exif_via_pillow(image: Path) -> dict[str, Any]:
    from PIL import Image, ExifTags

    out: dict[str, Any] = {}

    # Pixel dimensions are extracted unconditionally (separate from
    # EXIF) — every readable raster file has them, even synthetic
    # placeholders or photos that have had their EXIF stripped.
    try:
        with Image.open(image) as img:
            width, height = img.size
            if isinstance(width, int) and width > 0:
                out["width"] = width
            if isinstance(height, int) and height > 0:
                out["height"] = height
            exif = img._getexif() or {}
    except Exception:  # noqa: BLE001 — corrupt EXIF should not abort the walk
        return out

    if not exif:
        return out

    tag_name = {v: k for k, v in ExifTags.TAGS.items()}
    gps_name = {v: k for k, v in ExifTags.GPSTAGS.items()}

    def _g(name: str) -> Any:
        return exif.get(tag_name.get(name, -1))

    make = (_g("Make") or "").strip()
    model = (_g("Model") or "").strip()
    if make and model and not model.lower().startswith(make.lower()):
        out["camera"] = f"{make} {model}".strip()
    elif model:
        out["camera"] = model
    elif make:
        out["camera"] = make

    lens = (_g("LensModel") or _g("LensMake") or "").strip()
    if lens:
        out["lens"] = lens

    shutter_secs = _pillow_rational(_g("ExposureTime"))
    if shutter_secs and shutter_secs > 0:
        out["shutter"] = _format_shutter(shutter_secs)

    aperture = _pillow_rational(_g("FNumber"))
    if aperture and aperture > 0:
        out["aperture"] = _format_aperture(aperture)

    iso_raw = _g("ISOSpeedRatings") or _g("PhotographicSensitivity")
    if isinstance(iso_raw, int) and iso_raw > 0:
        out["iso"] = iso_raw
    elif isinstance(iso_raw, tuple) and iso_raw and isinstance(iso_raw[0], int):
        out["iso"] = iso_raw[0]

    focal = _pillow_rational(_g("FocalLength"))
    if focal and focal > 0:
        out["focal-length"] = _format_focal(focal)

    captured_raw = _g("DateTimeOriginal") or _g("DateTime")
    if isinstance(captured_raw, str) and captured_raw:
        date_part = captured_raw.split(" ", 1)[0].replace(":", "-")
        if len(date_part) == 10:
            out["captured"] = date_part

    gps_idx = tag_name.get("GPSInfo", -1)
    gps_info = exif.get(gps_idx) or {}
    if isinstance(gps_info, dict) and gps_info:
        # Pillow exposes GPSInfo by integer-keyed dict; remap.
        named = {gps_name.get(k, str(k)): v for k, v in gps_info.items()}
        lat = _gps_to_decimal(named.get("GPSLatitude"), named.get("GPSLatitudeRef"))
        lon = _gps_to_decimal(named.get("GPSLongitude"), named.get("GPSLongitudeRef"))
        if lat is not None and lon is not None:
            out["geo"] = [round(lat, 6), round(lon, 6)]

    exposure = _build_exposure_string(
        out.get("shutter"), out.get("aperture"), out.get("iso")
    )
    if exposure:
        out["exposure"] = exposure

    return out


# ---------------------------------------------------------------------------
# Walk + write
# ---------------------------------------------------------------------------


def _sidecar_path(image: Path) -> Path:
    return image.with_suffix(image.suffix + ".exif.yaml")


def _is_stale(image: Path, sidecar: Path) -> bool:
    if not sidecar.exists():
        return True
    return image.stat().st_mtime > sidecar.stat().st_mtime


def _atomic_write_yaml(path: Path, data: dict[str, Any]) -> None:
    tmp = path.with_suffix(path.suffix + ".tmp")
    with tmp.open("w", encoding="utf-8") as f:
        # Preserve the SIDECAR_KEYS order so a manual diff is easy to read.
        ordered = {k: data[k] for k in SIDECAR_KEYS if k in data}
        yaml.safe_dump(ordered, f, sort_keys=False, allow_unicode=True)
    tmp.replace(path)


def _read_one(image: Path) -> dict[str, Any]:
    if _exiftool_available():
        data = _read_exif_via_exiftool(image)
        if data:
            return data
    return _read_exif_via_pillow(image)


def main() -> int:
    if not CONTENT_DIR.exists():
        print(f"extract-exif: {CONTENT_DIR} does not exist — skipping.", file=sys.stderr)
        return 0

    using_exiftool = _exiftool_available()
    print(
        "extract-exif: source ="
        f" {'exiftool' if using_exiftool else 'Pillow (exiftool not installed)'}",
        file=sys.stderr,
    )

    written = 0
    skipped = 0
    failed = 0

    for image in sorted(CONTENT_DIR.rglob("*")):
        if image.suffix.lower() not in IMAGE_EXTS:
            continue
        # Skip the WebP companions (extension wouldn't match anyway, but
        # be explicit) and any tmp / hidden files.
        if image.name.startswith(".") or image.name.endswith(".tmp"):
            continue

        sidecar = _sidecar_path(image)
        if not _is_stale(image, sidecar):
            skipped += 1
            continue

        try:
            data = _read_one(image)
        except Exception as e:  # noqa: BLE001 — keep walking
            print(f"extract-exif: {image}: {e}", file=sys.stderr)
            failed += 1
            continue

        # Always write a sidecar — even if it's empty — so the consumer
        # doesn't need to branch on existence. An empty sidecar is the
        # explicit signal that "we tried; nothing to extract" (typical
        # for film scans).
        _atomic_write_yaml(sidecar, data)
        written += 1

    print(
        f"extract-exif: {written} written, {skipped} skipped, {failed} failed",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
