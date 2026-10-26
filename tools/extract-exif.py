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
import os
import shutil
import subprocess
import sys
from fractions import Fraction
from pathlib import Path
from typing import Any


sys.path.insert(0, str(Path(__file__).resolve().parent))
import photo_sidecars  # noqa: E402
import sitelib  # noqa: E402

SITE_ROOT = sitelib.SITE_ROOT
CONTENT_DIR = SITE_ROOT / "content" / "photography"
TOOL = "extract-exif"

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


# The tag list, shared by the single-image and batch paths so they can never
# disagree about what is being read.
EXIFTOOL_ARGS = [
    "-Make",
    "-Model",
    "-LensModel",
    "-Lens",
    "-LensID",
    "-LensSpec",
    "-LensInfo",
    # Numeric form is requested per-tag with the `#` suffix rather
    # than globally with `-n`. A global -n also flattens the string
    # tags: Composite:Lens becomes the raw LensInfo quad, so an
    # 18–55 kit zoom arrived as "18 55 3.5 5.6" instead of
    # "18-55mm f/3.5-5.6". These are the only fields parsed as
    # numbers below, so they are the only ones forced.
    "-ExposureTime#",
    "-FNumber#",
    "-ISO#",
    "-FocalLength#",
    "-FocalLengthIn35mmFormat#",
    "-DateTimeOriginal",
    "-CreateDate",
    "-GPSLatitude#",
    "-GPSLongitude#",
    "-GPSLatitudeRef",
    "-GPSLongitudeRef",
    "-ImageWidth#",
    "-ImageHeight#",
]

# exiftool is Perl and pays roughly 0.3 s of startup per invocation — twenty
# images cost 1.47 s as twenty calls and 0.12 s as one. Importing a large
# batch therefore prefetches every file in a single call and answers from
# this cache. Keyed by resolved path, since exiftool echoes SourceFile back
# in whatever form it was handed.
_RAW_CACHE: dict[str, dict] = {}


def _exiftool_json(images: list[Path]) -> list[dict]:
    args = []
    for image in images:
        a = str(image)
        # exiftool does not reliably support `--` as an end-of-options
        # marker, so make the path argument non-option-shaped instead: a
        # relative path is prefixed with ./ so it can never start with `-`.
        if not os.path.isabs(a):
            a = os.path.join(os.curdir, a)
        args.append(a)
    result = subprocess.run(
        ["exiftool", "-json", *EXIFTOOL_ARGS, *args],
        capture_output=True, text=True, check=False,
    )
    if result.returncode != 0:
        return []
    try:
        data = json.loads(result.stdout)
    except json.JSONDecodeError:
        return []
    return data or []


def prefetch_exiftool(images: list[Path]) -> None:
    """Read every image's tags in one exiftool call, into _RAW_CACHE."""
    if not images:
        return
    for entry in _exiftool_json(images):
        src = entry.get("SourceFile")
        if src:
            _RAW_CACHE[str(Path(src).resolve())] = entry


def _read_exif_via_exiftool(image: Path) -> dict[str, Any]:
    """Invoke exiftool and return a dict of normalised sidecar keys.

    exiftool's `-json` output is a list of objects; we parse the first
    entry. Numeric values come through as numbers; text values as
    strings. We accept missing keys silently.
    """
    key = str(image.resolve())
    raw = _RAW_CACHE.get(key)
    if raw is None:
        found = _exiftool_json([image])
        raw = found[0] if found else {}
    if not raw:
        return {}

    out: dict[str, Any] = {}

    make = (raw.get("Make") or "").strip()
    model = (raw.get("Model") or "").strip()
    # Manufacturers repeat the brand in both fields, but rarely identically:
    # Nikon writes Make "NIKON CORPORATION" and Model "NIKON D3100", so a
    # startswith() test against the whole Make string fails and the result
    # reads "NIKON CORPORATION NIKON D3100". Comparing the leading word
    # instead collapses that to "NIKON D3100", which is how every photo tool
    # displays it, while leaving genuinely distinct pairs untouched.
    if make and model:
        if model.lower().split()[:1] == make.lower().split()[:1]:
            out["camera"] = model
        elif model.lower().startswith(make.lower()):
            out["camera"] = model
        else:
            out["camera"] = f"{make} {model}".strip()
    elif model:
        out["camera"] = model
    elif make:
        out["camera"] = make

    # Preference order is "most human-readable first". LensModel is the
    # standard EXIF field and is already clean when present. `Lens` and
    # `LensID` are exiftool compositions that many bodies (Nikon among them)
    # populate when LensModel is absent. LensSpec/LensInfo are last because
    # LensInfo is a raw numeric quad — it is what produced the unreadable
    # "18 55 3.5 5.6 14" before this preference existed.
    lens = (
        raw.get("LensModel")
        or raw.get("Lens")
        or raw.get("LensID")
        or raw.get("LensSpec")
        or raw.get("LensInfo")
        or ""
    ).strip()

    # A phone's "lens" is not a lens. An iPhone reports "iPhone 15 Pro back
    # triple camera 6.765mm f/1.78" — a product description that repeats the
    # body named on the line above and describes a module nobody chose or can
    # remove. Where the lens field only restates the camera, drop it: the
    # camera row already says what took the photograph, and the exposure row
    # already carries the aperture.
    names_the_body = bool(lens) and bool(model) and model.lower() in lens.lower()
    if lens and not names_the_body:
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

    # Focal length in the terms the photographer means.
    #
    # A camera that names its lens is describing a barrel with numbers printed
    # on it: an 18-55 at its wide end is 18mm, and rendering that as its 27mm
    # equivalent would contradict the object in the hand. A body that names no
    # lens distinct from itself has no such barrel — 6.8mm is a fact about a
    # sensor, not about a photograph — so the 35mm equivalent is the only
    # figure that means anything, and it is what every phone reports a photo
    # as having been taken at.
    focal = raw.get("FocalLength")
    focal35 = raw.get("FocalLengthIn35mmFormat")
    if "lens" not in out and isinstance(focal35, (int, float)) and focal35 > 0:
        out["focal-length"] = _format_focal(float(focal35))
    elif isinstance(focal, (int, float)) and focal > 0:
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
            exif = img.getexif() or {}
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
# Walk + write (tools/photo_sidecars.py)
# ---------------------------------------------------------------------------


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

    # A sidecar is written even when it is empty, so the consumer need not
    # branch on existence: an empty one says "we tried; nothing to
    # extract" (typical for film scans). Keys in SIDECAR_KEYS order, so a
    # diff reads the same way every time.
    counts = photo_sidecars.run(
        TOOL, ".exif.yaml", _read_one,
        roots=[CONTENT_DIR], image_exts=IMAGE_EXTS, argv=sys.argv[1:],
        keys=SIDECAR_KEYS, prefetch=prefetch_exiftool if using_exiftool else None)
    print(
        f"extract-exif: {counts['written']} written, {counts['skipped']} skipped, "
        f"{counts['failed']} failed",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
