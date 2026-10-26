#!/usr/bin/env python3
"""subset-fonts.py — the site's web fonts, from their upstream releases.

    UV_PROJECT_ENVIRONMENT="$PWD/.venv" uv run --frozen --project engine \
        --with fonttools --with brotli engine/tools/subset-fonts.py

The site's tools/fonts.yaml names each family's upstream sources, the
characters and OpenType features its subsets keep, and the license notice
written beside them; tools/font-sources.json pins each download's SHA-256.
Downloads are cached under ~/.cache/ozymandias-fonts/<host>, and the WOFF2
subsets and OFL-*.txt notices go to fonts.yaml's `output`. Every name
record is kept (`--name-IDs '*'`): pyftsubset's default keeps only 0–6,
which dropped the license text and URL (IDs 13 and 14) from every font
(audit C05). A family whose font reserves its name takes a primary name of
the site's own (`rename`), as the OFL asks of a Modified Version.

It then rewrites the fallback faces between fonts.yaml's markers (audit
V20, A19): for each system font a reader may see before or instead of a
web font, an @font-face that names it with local() and scales it
(size-adjust) so a line of the site's prose sets to the same width, with
the web font's ascent and descent. Text then barely moves when the web
font arrives. The metrics come from each set's reference font, a
metric-compatible open font or the system font itself, and the widths are
averaged over the characters of content/'s prose.

Tempo Notes is built separately by tools/build-notes-font.py.
"""

from __future__ import annotations

import collections
import hashlib
import io
import json
import re
import sys
import urllib.request
from pathlib import Path

import yaml
from fontTools import subset
from fontTools.ttLib import TTFont
from fontTools.varLib import instancer

sys.path.insert(0, str(Path(__file__).resolve().parent))
import sitelib  # noqa: E402

SITE_ROOT = sitelib.SITE_ROOT
CONFIG = yaml.safe_load((SITE_ROOT / "tools" / "fonts.yaml").read_text(encoding="utf-8"))
OUT = SITE_ROOT / CONFIG["output"]
CONTENT = SITE_ROOT / "content"
CACHE: Path | None = None   # where downloads are kept, if not the site's own cache
SOURCE_HASHES = json.loads((SITE_ROOT / "tools" / "font-sources.json").read_text())
BASE = CONFIG["unicodes"]


def source(spec: str) -> str:
    """A download, `<upstream>:<path>` in fonts.yaml: the path under that
    upstream's pinned revision."""
    upstream, path = spec.split(":", 1)
    return f"{CONFIG['upstreams'][upstream]}/{urllib.request.quote(path)}"


def site_or_engine(rel: str) -> Path:
    """The site's file, or the engine's it stands in for, as the build reads
    them (build/EngineFiles.hs)."""
    own = SITE_ROOT / rel
    return own if own.is_file() else sitelib.engine_path(rel)


FAMILIES = CONFIG["families"]
# (family, source URL, output, features, extra unicodes, wght instance)
FONTS = [(family, source(src), out, spec["features"], spec.get("unicodes", ""), spec.get("weight"))
         for family, spec in FAMILIES.items() for src, out in spec["faces"]]
# family -> (notice written beside the subsets, upstream licence text)
LICENCES = {family: (spec["license"][0], source(spec["license"][1]))
            for family, spec in FAMILIES.items()}

# The subset's primary name, in place of a reserved one: the family, full
# and PostScript names and their typographic forms. family -> (reserved
# name, the site's, the word no primary name record may keep)
RENAME = {family: (spec["rename"]["from"], spec["rename"]["to"], spec["rename"]["reserved"])
          for family, spec in FAMILIES.items() if "rename" in spec}
PRIMARY_NAME_IDS = {1, 3, 4, 6, 16, 17, 18, 21, 22}

# Fallback faces (fonts.yaml's `fallbacks`), named for local() by the
# schemes below.
STYLE_WORDS = {"Regular": "", "Italic": " Italic", "Bold": " Bold", "BoldItalic": " Bold Italic"}

def full(family: str, style: str, regular: str = "") -> str:
    return family + (STYLE_WORDS[style] or regular)

def ps(prefix: str, style: str, regular: str = "-Regular") -> str:
    return prefix + ("-" + style if style != "Regular" else regular)

def ms(prefix: str, style: str) -> str:          # Monotype's: TimesNewRomanPS-BoldMT
    return prefix + ("-" + style if style != "Regular" else "") + "MT"

def weight_of(style: str) -> int:
    return 700 if "Bold" in style else 400

NAME_SCHEMES = {"full": full, "ps": ps, "ms": ms}


def fallback_set(spec: dict) -> tuple:
    def names(style: str) -> list[str]:
        return [NAME_SCHEMES[scheme](name, style, *rest) for scheme, name, *rest in spec["local"]]

    def reference(style: str) -> tuple[str, int | None]:
        ref = spec.get("reference-italic", spec["reference"]) if "Italic" in style else spec["reference"]
        return source(ref.format(style=style)), weight_of(style) if spec.get("instance") else None

    faces = [tuple(face) for face in CONFIG["fallbacks"]["faces"][spec["faces"]]]
    return spec["family"], faces, names, reference


# (family, faces, local names for a style, reference font for a style: URL and wght)
FALLBACK_SETS = [fallback_set(spec) for spec in CONFIG["fallbacks"]["sets"]]
BASE_CSS = site_or_engine(CONFIG["fallbacks"]["css"])
FALLBACK_BEGIN = CONFIG["fallbacks"]["begin"] + "\n"
FALLBACK_END = CONFIG["fallbacks"]["end"] + "\n"

# What each family must cover after subsetting.
MUST_COVER = {family: spec["must-cover"] for family, spec in FAMILIES.items()}


def cache_dir() -> Path:
    """CACHE, else ~/.cache/ozymandias-fonts/<host>: one cache per site."""
    return CACHE or Path.home() / ".cache" / "ozymandias-fonts" / sitelib.site_config()["host"]


def fetch(url: str) -> bytes:
    expected = SOURCE_HASHES[url]
    cached = cache_dir() / hashlib.sha256(url.encode()).hexdigest()[:16] / url.rsplit("/", 1)[1]
    if cached.is_file():
        data = cached.read_bytes()
    else:
        with urllib.request.urlopen(url, timeout=60) as r:
            data = r.read()
    if hashlib.sha256(data).hexdigest() != expected:
        raise ValueError(f"font source checksum mismatch: {url}; cache: {cached}")
    if not cached.is_file():
        cached.parent.mkdir(parents=True, exist_ok=True)
        cached.write_bytes(data)
    print(f"  {url.rsplit('/', 1)[1]}  sha256 {hashlib.sha256(data).hexdigest()[:16]}…  ({len(data) // 1024} KB)")
    return data


def rename(font: TTFont, old: str, new: str, reserved: str) -> None:
    for rec in font["name"].names:
        if rec.nameID in PRIMARY_NAME_IDS:
            text = rec.toUnicode()
            rec.string = text.replace(old, new).replace(old.replace(" ", ""), new.replace(" ", ""))
    left = [r.nameID for r in font["name"].names if r.nameID in PRIMARY_NAME_IDS and reserved in r.toUnicode()]
    if left:
        sys.exit(f"name records {left} still carry the reserved name")


def build(family, url, out, features, extra, wght) -> str:
    """Write one subset; return the font's own copyright notice."""
    # recalcTimestamp off: the same sources give the same bytes.
    font = TTFont(io.BytesIO(fetch(url)), recalcTimestamp=False)
    notice = font["name"].getDebugName(0)
    if wght is not None:
        font = instancer.instantiateVariableFont(font, {"wght": wght})
    options = subset.Options()
    options.layout_features = features.split(",")
    options.name_IDs = ["*"]
    options.hinting = False
    options.desubroutinize = True
    unicodes = subset.parse_unicodes(BASE + ("," + extra if extra else ""))
    sub = subset.Subsetter(options)
    sub.populate(unicodes=unicodes)
    sub.subset(font)
    if family in RENAME:
        rename(font, *RENAME[family])
    dest = OUT / out
    before = dest.stat().st_size if dest.is_file() else 0
    font.flavor = "woff2"          # Options.flavor is the CLI's; the API saves the font's own
    font.save(str(dest))
    cmap = TTFont(str(dest)).getBestCmap()
    missing = [c for c in MUST_COVER[family] if ord(c) not in cmap]
    if missing:
        sys.exit(f"{out}: upstream lacks {''.join(missing)}")
    print(f"  → {out}: {before // 1024} → {dest.stat().st_size // 1024} KB")
    return notice


def write_licence(family: str, notice: str) -> None:
    """The upstream OFL.txt, headed by the copyright notice the font itself
    carries. They differ for Fira Sans: bBox's OFL.txt drops the Reserved
    Font Name that 4.301's name table still declares, and the font is what
    is shipped."""
    name, url = LICENCES[family]
    text = fetch(url).decode("utf-8").replace("\r\n", "\n")
    head, sep, body = text.partition("\n\n")
    if head.strip() != notice:
        print(f"  {name}: notice taken from the font, not the upstream text")
        text = notice + sep + body
    (OUT / name).write_text(text, encoding="utf-8")


def prose_frequencies() -> collections.Counter:
    """Characters of the site's prose: content/'s Markdown without front
    matter, code, maths, link targets or tags."""
    freq = collections.Counter()
    for md in CONTENT.rglob("*.md"):
        text = md.read_text(encoding="utf-8", errors="ignore")
        text = re.sub(r"\A---\n.*?\n---\n", "", text, flags=re.S)
        text = re.sub(r"```.*?```|\$\$.*?\$\$|\$[^$\n]+\$|`[^`\n]+`|\]\([^)]*\)|<[^>]+>", " ", text, flags=re.S)
        freq.update(re.sub(r"[#*_>\[\]|{}\\]", "", re.sub(r"\s+", " ", text)))
    return freq


def instance(font: TTFont, wght: int | None) -> TTFont:
    """A static instance: wght as asked, every other axis at its default."""
    if wght is None or "fvar" not in font:
        return font
    axes = {a.axisTag: (wght if a.axisTag == "wght" else a.defaultValue) for a in font["fvar"].axes}
    return instancer.instantiateVariableFont(font, axes)


def mean_advance(font: TTFont, freq: collections.Counter, chars: set[str]) -> float:
    cmap, hmtx, upm = font.getBestCmap(), font["hmtx"], font["head"].unitsPerEm
    total = sum(freq[c] for c in chars)
    return sum(freq[c] * hmtx[cmap[ord(c)]][0] for c in chars) / upm / total


def vertical_metrics(font: TTFont) -> tuple[int, int, int]:
    """Ascent, descent and line gap as browsers read them: OS/2's typo
    values when USE_TYPO_METRICS is set, else hhea's."""
    os2 = font["OS/2"]
    if os2.fsSelection & (1 << 7):
        return os2.sTypoAscender, -os2.sTypoDescender, os2.sTypoLineGap
    hhea = font["hhea"]
    return hhea.ascent, -hhea.descent, hhea.lineGap


def fallback_rule(family, weight, style, names, webfont, ref, freq) -> tuple[str, float]:
    common = {c for c in freq if ord(c) in webfont.getBestCmap() and ord(c) in ref.getBestCmap()}
    adjust = mean_advance(webfont, freq, common) / mean_advance(ref, freq, common)
    upm = webfont["head"].unitsPerEm
    # The overrides are scaled by size-adjust in turn, so divide it out.
    asc, desc, gap = (f"{100 * v / upm / adjust:.1f}%" for v in vertical_metrics(webfont))
    src = ", ".join(f'local("{n}")' for n in dict.fromkeys(names))
    return (f'@font-face {{\n'
            f'    font-family: "{family}";\n'
            f'    src: {src};\n'
            f'    font-weight: {weight};\n'
            f'    font-style: {style};\n'
            f'    size-adjust: {100 * adjust:.1f}%;\n'
            f'    ascent-override: {asc};\n'
            f'    descent-override: {desc};\n'
            f'    line-gap-override: {gap};\n'
            f'}}\n'), adjust


def write_fallbacks() -> None:
    freq = prose_frequencies()
    rules = []
    for family, faces, names_for, reference in FALLBACK_SETS:
        for style_name, weight, style, web in faces:
            ref_url, wght = reference(style_name)
            ref = instance(TTFont(io.BytesIO(fetch(ref_url))), wght)
            rule, adjust = fallback_rule(family, weight, style, names_for(style_name),
                                         TTFont(str(OUT / web)), ref, freq)
            rules.append(rule)
            print(f"  {family} {weight} {style}: size-adjust {100 * adjust:.1f}%")
    css = BASE_CSS.read_text(encoding="utf-8")
    start, end = css.find(FALLBACK_BEGIN), css.find(FALLBACK_END)
    if start < 0 or end < start:
        sys.exit(f"{BASE_CSS.name}: no fallback-face markers")
    BASE_CSS.write_text(css[:start + len(FALLBACK_BEGIN)] + "".join(rules) + css[end:], encoding="utf-8")


def main() -> None:
    OUT.mkdir(parents=True, exist_ok=True)
    notices = {}
    for spec in FONTS:
        notice = build(*spec)
        if notices.setdefault(spec[0], notice) != notice:
            sys.exit(f"{spec[2]}: copyright differs from the family's other faces")
    for family, notice in notices.items():
        write_licence(family, notice)
    write_fallbacks()


if __name__ == "__main__":
    main()
