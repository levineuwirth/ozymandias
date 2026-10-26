#!/usr/bin/env python3
"""audit-viz.py — inspect what figures actually look like, not how they are coded.

Every defect this audit has caught in the visualization pipeline was invisible
in the source and obvious in the render:

  * a Cliff's delta heatmap drawn from data where 26 of 27 cells were exactly
    1.000, so `imshow` produced a solid black rectangle carrying no
    information, described in the prose as if it showed something;
  * white cell labels in that same figure repainted near-black by an
    over-broad CSS rule, i.e. invisible against the cells they sat on;
  * tick labels rendering at 4.4px on a phone;
  * a y-axis reading "(imes)", because `"$\\times$"` in a non-raw Python
    string makes `\\t` a tab.

`tests/test_viz.py` guards the contract — colour, determinism, ids, captions,
accessibility. It cannot see any of the above, because all of them are
properties of the picture. This is the complement: it renders each figure and
complains about what a reader would notice.

Exit code is 0 unless --strict is passed; this is a report, like
`make audit-marks`, not a build gate.

Checks
------
constant-image   An <image> mark (imshow, pcolormesh) whose pixels barely
                 vary. A heatmap with no gradient is a bug in the data, the
                 normalisation, or the choice of figure.
text-contrast    Every text run against whatever is painted behind it, using
                 the WCAG relative-luminance ratio, in every site theme.
crowding         Text runs whose rendered size falls below the legible floor
                 once the figure is scaled to the body column.
metadata         Missing alt / <title> / <desc>, which tests also check but
                 which belong in a figure report.

Usage:  uv run python tools/audit-viz.py [--strict] [PATH ...]
"""

from __future__ import annotations

import argparse
import base64
import io
import os
import re
import subprocess
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import sitelib  # noqa: E402

SITE_ROOT = sitelib.SITE_ROOT
SVG_NS = "{http://www.w3.org/2000/svg}"
XLINK_HREF = "{http://www.w3.org/1999/xlink}href"

# Site theme backgrounds and foregrounds (--bg, --text), from
# static/css/base.css. currentColor resolves to these, so a figure has to
# stay legible against every one.
THEMES = {
    "light": ("#faf8f4", "#1a1a1a"),
    "dark": ("#121212", "#d4d0c8"),
    "cappuccino": ("#553a28", "#ead0a0"),
}

BODY_MAX_WIDTH = 800     # --body-max-width in base.css
MIN_LABEL_PX = 9.0       # below this, axis labels stop being readable
MIN_CONTRAST = 3.0       # WCAG AA for large text / graphical objects
MIN_IMAGE_SPREAD = 8     # 0-255; an <image> flatter than this says nothing


def find_scripts(paths: list[str]) -> list[Path]:
    """Figure scripts referenced by a `.figure` div, or the paths given."""
    if paths:
        return [Path(p).resolve() for p in paths]
    pattern = re.compile(r'\{\.figure[^}]*script="([^"]+)"')
    found: list[Path] = []
    for md in (SITE_ROOT / "content").rglob("*.md"):
        for rel in pattern.findall(md.read_text(encoding="utf-8")):
            script = (md.parent / rel).resolve()
            if script.is_file():
                found.append(script)
    return sorted(set(found))


def render(script: Path) -> str | None:
    venv = SITE_ROOT / ".venv" / "bin" / "python3"
    exe = str(venv) if venv.is_file() else sys.executable
    result = subprocess.run(
        [exe, str(script)], cwd=SITE_ROOT, capture_output=True, text=True, timeout=180
    )
    if result.returncode != 0:
        return None
    return result.stdout


# ---------------------------------------------------------------------------
# Colour
# ---------------------------------------------------------------------------

def parse_hex(value: str) -> tuple[int, int, int] | None:
    value = value.strip()
    if not value.startswith("#"):
        return None
    body = value[1:]
    if len(body) == 3:
        body = "".join(c * 2 for c in body)
    if len(body) < 6:
        return None
    try:
        return tuple(int(body[i:i + 2], 16) for i in (0, 2, 4))  # type: ignore[return-value]
    except ValueError:
        return None


def luminance(rgb: tuple[int, int, int]) -> float:
    def channel(c: float) -> float:
        c /= 255.0
        return c / 12.92 if c <= 0.04045 else ((c + 0.055) / 1.055) ** 2.4
    r, g, b = (channel(c) for c in rgb)
    return 0.2126 * r + 0.7152 * g + 0.0722 * b


def contrast(a: tuple[int, int, int], b: tuple[int, int, int]) -> float:
    la, lb = luminance(a), luminance(b)
    hi, lo = max(la, lb), min(la, lb)
    return (hi + 0.05) / (lo + 0.05)


# ---------------------------------------------------------------------------
# Checks
# ---------------------------------------------------------------------------

def check_metadata(svg: str) -> list[str]:
    issues = []
    root_tag = re.search(r"<svg\b[^>]*>", svg)
    if root_tag and 'role="img"' not in root_tag.group(0):
        issues.append("metadata: no role=\"img\" on the svg root")
    if root_tag and not re.search(r'aria-label="[^"]{20,}"', root_tag.group(0)):
        issues.append("metadata: no usable aria-label (pass alt= to save_svg)")
    if "<desc>" not in svg:
        issues.append("metadata: no <desc> (pass desc= to save_svg)")
    return issues


def check_size(svg: str) -> list[str]:
    m = re.search(r'viewBox="[\d.]+ [\d.]+ ([\d.]+)', svg)
    if not m:
        return ["crowding: no viewBox, cannot judge rendered size"]
    units = float(m.group(1))
    issues = []
    # The scroll wrapper floors the render at the natural width, so a label
    # is at worst 1px per unit. Flag figures whose own font size is small
    # enough to fall under the floor even then.
    for size in {float(s) for s in re.findall(r"font-size:\s*([\d.]+)px", svg)}:
        if size < MIN_LABEL_PX:
            issues.append(
                f"crowding: {size:g}px text — below the {MIN_LABEL_PX:g}px "
                f"floor even at natural width"
            )
    if units > BODY_MAX_WIDTH * 1.5:
        issues.append(
            f"crowding: {units:.0f} units wide against a {BODY_MAX_WIDTH}px "
            f"column — readers will scroll a long way"
        )
    return issues


def check_constant_image(svg: str) -> list[str]:
    """An <image> mark whose pixels barely vary carries no information."""
    try:
        from PIL import Image
    except ImportError:
        return []
    issues = []
    root = ET.fromstring(svg)
    for i, node in enumerate(root.iter(f"{SVG_NS}image"), 1):
        href = node.get(XLINK_HREF) or node.get("href") or ""
        if "base64," not in href:
            continue
        raw = base64.b64decode(href.split("base64,", 1)[1])
        img = Image.open(io.BytesIO(raw)).convert("L")
        pixels = list(img.getdata())
        if not pixels:
            continue
        spread = max(pixels) - min(pixels)
        if spread < MIN_IMAGE_SPREAD:
            issues.append(
                f"constant-image: image {i} spans only {spread}/255 grey levels "
                f"— the mark shows no variation, so it conveys nothing"
            )
    return issues


def check_text_contrast(svg: str) -> list[str]:
    """Text against whatever is painted behind it, in every theme.

    Only explicit fills are checked. Text with no fill inherits currentColor
    from the page, which is the themed path and correct by construction.
    """
    root = ET.fromstring(svg)
    parent = {c: p for p in root.iter() for c in p}

    # Backing fills, largest first: a text run is assumed to sit on the
    # darkest large area painted in the figure, which for a heatmap is a cell.
    backdrops: list[tuple[int, int, int]] = []
    for node in root.iter():
        style = node.get("style") or ""
        m = re.search(r"fill:\s*(#[0-9a-fA-F]{3,8})", style)
        if m and node.tag in (f"{SVG_NS}path", f"{SVG_NS}rect"):
            rgb = parse_hex(m.group(1))
            if rgb:
                backdrops.append(rgb)

    issues = []
    seen: set[tuple] = set()
    for node in root.iter():
        style = node.get("style") or ""
        m = re.search(r"fill:\s*(#[0-9a-fA-F]{3,8})", style)
        if not m:
            continue
        # Only groups that hold glyphs.
        anc_ids = []
        x = node
        while x in parent:
            x = parent[x]
            if x.get("id"):
                anc_ids.append(x.get("id"))
        if not any(i.startswith("text_") for i in anc_ids):
            continue
        fg = parse_hex(m.group(1))
        if fg is None:
            continue
        for theme, (page_bg, _) in THEMES.items():
            page = parse_hex(page_bg)
            assert page is not None
            candidates = backdrops + [page]
            worst = min(contrast(fg, bg) for bg in candidates)
            key = (m.group(1), theme)
            if worst < MIN_CONTRAST and key not in seen:
                seen.add(key)
                issues.append(
                    f"text-contrast: {m.group(1)} text reaches only "
                    f"{worst:.1f}:1 in {theme} mode against the worst thing "
                    f"it could sit on, the page itself included "
                    f"(want {MIN_CONTRAST:g}:1). Text with an explicit fill "
                    f"is legible only where it lands on a mark dark or light "
                    f"enough to carry it; text left at the default colour "
                    f"inherits currentColor and is themed for free."
                )
    return issues


# ---------------------------------------------------------------------------

def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("paths", nargs="*", help="figure scripts (default: all in content/)")
    ap.add_argument("--strict", action="store_true",
                    help="exit non-zero when any figure has an issue")
    args = ap.parse_args()

    scripts = find_scripts(args.paths)
    if not scripts:
        print("audit-viz: no figure scripts found")
        return 0

    total = 0
    for script in scripts:
        svg = render(script)
        rel = os.path.relpath(script, SITE_ROOT)
        if svg is None:
            print(f"{rel}\n  ERROR: script exited non-zero")
            total += 1
            continue
        issues = (check_metadata(svg) + check_size(svg)
                  + check_constant_image(svg) + check_text_contrast(svg))
        total += len(issues)
        if issues:
            print(rel)
            for issue in issues:
                print(f"  {issue}")
        else:
            print(f"{rel}\n  ok")

    print(f"\naudit-viz: {len(scripts)} figure(s), {total} issue(s)")
    return 1 if (args.strict and total) else 0


if __name__ == "__main__":
    sys.exit(main())
