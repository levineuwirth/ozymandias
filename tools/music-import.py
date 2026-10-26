#!/usr/bin/env python3
"""music-import.py — bring a MuseScore score onto the site, and keep it there.

Score pages are derived artifacts, like the photography delivery JPEGs:
git carries each composition's index.md and a small score-source.yaml
manifest, and the pages themselves are regenerated from the engraver's
source and deployed through _site/. (See the note in .gitignore.)

    tools/music-import.py import <score.mscz> <slug> [--pdf] [--audio]
    tools/music-import.py refresh [<slug> ...] [--force] [--audio]
    tools/music-import.py check

import   Exports one SVG per page into content/music/<slug>/scores/,
         optionally a PDF, and writes score-source.yaml. For a new piece
         it also scaffolds index.md — title, year, duration, category and
         the movements (numeral, tempo, reader page, duration), all read
         from the score. An existing index.md is never touched; the
         computed movements are compared against it instead.
refresh  Re-exports pieces from their manifests (all of them by default):
         the fresh-clone path, and the re-engrave path.
--movements 1-2
         Publishes only the leading movements — for a work whose later
         movements are in revision. The pages stop before the first withheld
         movement, which must therefore begin on a new page; the realization
         and its timing stop at its first downbeat. The choice is recorded in
         the manifest, and `--movements all` publishes the whole work again.
         List the withheld movements in index.md with `status: in revision`.
--engraver mscore3
         Engraves with MuseScore 3.6.2 (`mscore3`) instead of 4, for a score
         saved in MuseScore 3: 4 reflows such a score and drops what was
         placed by hand. Pages and bar positions come from 3; a realization
         still comes from 4 (Muse Sounds needs it), its timing matched to
         the MuseScore 3 layout bar by bar. MuseScore 3's bundled Qt has no
         headless platform, so it runs against the desktop's X display.
--audio  Also renders a MIDI realization with Muse Sounds, and writes
         scores/timing.json: when each bar sounds, and where it stands on
         its page. The reader uses the two to follow the score while it
         plays. Both come from one playback of one layout, so they agree
         by construction — repeats included, since the timing lists bars
         in the order they are played.
shrink   Rewrites the pages already on disk as an import now writes them
         (below), in place. For pages exported before shrinking existed;
         a page already shrunk is left alone.
thumbnail
         Writes scores/thumb.webp for pieces imported before thumbnails
         existed (below).
check    Reports every composition whose score is missing or stale, a page
         cut short, or an import that was interrupted.

Pages are shrunk on import, by about two thirds raw and a third over the
wire (audit MO1). MuseScore writes coordinates to two decimals of a 1/1200
inch unit and repeats every glyph's outline wherever it is drawn: the
coordinates are rounded to whole units, a twelve-hundredth of an inch, and
each outline drawn more than once on a page is written once in <defs> and
placed with <use>. Nothing the reader depends on changes: the root's size
and viewBox, every <line> and <polyline> (the instrument-name gutter is
measured from them), the barlines (the timing's scale), and the fill and
stroke attributes the stylesheet recolours by. The root <title> and <desc>,
which carry the source file's name, go.

Each import also writes page 1 as scores/thumb.webp, 720 px wide and grey,
which the composition page's frontispiece and the /music/ shelf show in
place of the SVG: a decoded image of about 26 KB, where the SVG is 2,000
outlines for the browser to rasterise at a fifth of their size (audit
MO4). It needs rsvg-convert and Pillow with WebP; without them the import
warns and the pages show the SVG, as before.

An import is assembled in .music-import/<slug>/ at the repository root and
swapped into place by two renames, the manifest written last: a crash
leaves the old pages or the new ones, never a mixture with the other
export's timing, and `check` names what an interrupted run left behind
(audit M09). Recovery files stay until the manifest and any new index are
written. An import refuses an existing staging directory; recover its
saved pages and manifest before removing it and trying again.

Why refresh refuses to change the page count without --force: unlike a
photo resize, a re-export is not reproducible. A newer MuseScore can
reflow the layout (the 3.6 → 4.x Violin Sonata moved whole systems), and
a reflow moves every movement's `page:` in index.md with it — a silent
wrong jump in the reader. The manifest records the page count and the
MuseScore version so that change is caught and named.

Source paths are stored relative to MUSIC_SCORES_DIR (default
~/Documents/Scores) so the manifest does not publish a home directory.

Requires MuseScore 4's `mscore` on PATH; runs headless.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import zipfile
import xml.etree.ElementTree as ET
from pathlib import Path

import yaml

sys.path.insert(0, str(Path(__file__).resolve().parent))
import front_matter  # noqa: E402
import sitelib  # noqa: E402

SITE_ROOT = sitelib.SITE_ROOT
MUSIC = SITE_ROOT / "content" / "music"
SCORES_DIR = Path(os.environ.get("MUSIC_SCORES_DIR", "~/Documents/Scores")).expanduser()
MANIFEST = "score-source.yaml"
PAGES = "scores"
# Imports are assembled here, on the same filesystem as content/ (a rename
# across filesystems is a copy) and outside it (the build must never see a
# half-assembled set). Gitignored; Hakyll skips dot-directories.
STAGING = SITE_ROOT / ".music-import"
AUDIO = "realization.mp3"
TIMING = "timing.json"
THUMB = "thumb.webp"
# The frontispiece is at most 17.5rem wide (280 px at the default text size,
# about 350 at the largest), the shelf's face-out narrower; twice that for a
# 2x screen.
THUMB_WIDTH = 720
AUDIO_LABEL = "MIDI realization (Muse Sounds)"

# The engravers the importer can drive. MuseScore 3's AppImage bundles a Qt
# with only the xcb platform, so it needs a display where 4 runs offscreen.
ENGRAVERS = {
    "mscore":  {"bin": "mscore",  "platform": "offscreen", "name": "MuseScore 4"},
    "mscore3": {"bin": "mscore3", "platform": "xcb",       "name": "MuseScore 3"},
}

# MuseScore writes note values into tempo text as SMuFL symbols, either as
# <sym> elements or as private-use characters. The frontmatter gets the
# readable Unicode forms; the page's Tempo Notes font draws both.
SMUFL = {
    "metNoteDoubleWhole": "𝅜", "metNoteWhole": "𝅝", "metNoteHalfUp": "𝅗𝅥",
    "metNoteQuarterUp": "♩", "metNote8thUp": "♪", "metNote16thUp": "𝅘𝅥𝅯",
    "metAugmentationDot": ".",
}
SMUFL_PUA = {
    "": "𝅜", "": "𝅝", "": "𝅗𝅥", "": "♩",
    "": "♪", "": "𝅘𝅥𝅯", "": ".",
}

# MuseScore instrument ids that are voices, not instruments.
VOICES = {"voice", "soprano", "mezzo-soprano", "alto", "contralto", "countertenor",
          "tenor", "baritone", "bass", "boy-soprano", "women", "men", "choir"}

# A printed subtitle that reads as a dedication. "for" is left out on
# purpose: "for string quartet" is a scoring, not a person.
DEDICATION = re.compile(r"^(to|für|pour|à|per|dedicated|in memoriam|in memory)\b", re.I)

ROMAN = re.compile(r"^\s*([IVXLC]+)\b\.?\s*(?:[-–—:]\s*)?(.*)$", re.S)


def die(msg: str) -> None:
    sys.exit(f"music-import: {msg}")


def warn(msg: str) -> None:
    print(f"music-import: {msg}", file=sys.stderr)


# ---------------------------------------------------------------------------
# MuseScore
# ---------------------------------------------------------------------------

def mscore(*args: str, timeout: int = 600, engraver: str = "mscore") -> subprocess.CompletedProcess:
    e = ENGRAVERS[engraver]
    env = dict(os.environ, QT_QPA_PLATFORM=e["platform"])
    if e["platform"] == "xcb" and not env.get("DISPLAY"):
        die(f"{e['name']} needs an X display (its Qt has no headless platform); "
            f"run this from the desktop session")
    try:
        return subprocess.run([e["bin"], *args], capture_output=True, text=True,
                              timeout=timeout, env=env, check=False)
    except FileNotFoundError:
        die(f"`{e['bin']}` ({e['name']}) is not on PATH")


def mscore_version(engraver: str = "mscore") -> str:
    out = mscore("--version", engraver=engraver).stdout.strip().splitlines()
    # "MuseScore4 4.7.5" → "MuseScore 4.7.5"; "MuseScore3 3.6.2" likewise
    ver = out[-1].split()[-1] if out else "unknown"
    return f"MuseScore {ver}"


def export_one(source: Path, target: Path, *extra: str, timeout: int = 600,
               engraver: str = "mscore") -> None:
    """One MuseScore export, checked on both counts: the exit status, and a
    non-empty file where it was asked for. Either alone can be fooled —
    an existing file satisfies a presence check after a failed export.
    Page exports are numbered page-1.svg by MuseScore 4, page-01.svg by 3."""
    r = mscore(*extra, "-o", str(target), str(source), timeout=timeout, engraver=engraver)
    made = [target] if target.suffix != ".svg" else \
        sorted(target.parent.glob(f"{target.stem}-*{target.suffix}"))
    if r.returncode != 0 or not made or made[0].stat().st_size == 0:
        die(f"MuseScore could not export {target.suffix} from {source} "
            f"(exit {r.returncode}):\n{r.stderr[-2000:]}")


def export(source: Path, outdir: Path, pdf_name: str | None, audio: bool,
           engraver: str = "mscore") -> dict:
    """Export everything a piece needs into outdir, a scratch directory.
    Nothing published is touched until all of it has succeeded.

    The layout — pages, PDF, bar boxes — comes from the engraver. The sound
    always comes from MuseScore 4, and so, for an older engraver, does a
    second .mpos whose event times match that sound."""
    export_one(source, outdir / "page.svg", engraver=engraver)
    pages = sorted(outdir.glob("page-*.svg"),
                   key=lambda p: int(p.stem.rsplit("-", 1)[1]))
    export_one(source, outdir / "score.mpos", engraver=engraver)
    pdf = None
    if pdf_name:
        pdf = outdir / pdf_name
        export_one(source, pdf, engraver=engraver)
    realization, playback = None, outdir / "score.mpos"
    if audio:
        # Muse Sounds renders at roughly six times real time: a fifty-minute
        # symphony takes the better part of ten minutes.
        realization = outdir / AUDIO
        export_one(source, realization, "--sound-profile", "MuseSounds",
                   "-b", "128", timeout=3 * 3600)
        if engraver != "mscore":
            playback = outdir / "playback.mpos"
            export_one(source, playback)
    return {"pages": pages, "mpos": outdir / "score.mpos", "pdf": pdf,
            "audio": realization, "playback": playback}


def score_meta(source: Path) -> dict:
    r = mscore("--score-meta", str(source))
    try:
        m = json.loads(r.stdout)
    except ValueError:
        die(f"could not read metadata from {source}")
    return m.get("metadata", m)


# ---------------------------------------------------------------------------
# Structure: movements from section breaks, pages and times from .mpos
# ---------------------------------------------------------------------------

def read_mscx(source: Path) -> ET.Element:
    with zipfile.ZipFile(source) as z:
        name = next(n for n in z.namelist() if n.endswith(".mscx"))
        return ET.fromstring(z.read(name))


def text_of(el: ET.Element | None) -> str:
    """Flatten a MuseScore <text>, turning SMuFL note symbols into Unicode."""
    if el is None:
        return ""
    out = []
    def walk(e: ET.Element) -> None:
        if e.tag == "sym":
            out.append(SMUFL.get((e.text or "").strip(), ""))
        else:
            out.append(e.text or "")
            for c in e:
                walk(c)
        if e is not el:
            out.append(e.tail or "")
    walk(el)
    s = "".join(out)
    for pua, uni in SMUFL_PUA.items():
        s = s.replace(pua, uni)
    return re.sub(r"\s+", " ", s).strip()


def movements(root: ET.Element, mpos: Path, total_seconds: float) -> list[dict]:
    """One entry per section of the score: its opening measure's tempo
    text, reader page, and duration."""
    staff = root.find("Score/Staff")
    starts, tempos, idx = [0], {}, 0
    for el in staff:
        breaks = [b.findtext("subtype") for b in el.iter("LayoutBreak")]
        if el.tag != "Measure":
            # A section break can sit on the frame between movements.
            if "section" in breaks:
                starts.append(idx)
            continue
        for t in el.iter("Tempo"):
            tempos.setdefault(idx, text_of(t.find("text")))
        if "section" in breaks:
            starts.append(idx + 1)
        idx += 1
    starts = sorted({s for s in starts if s < idx})

    x = mpos.read_text()
    page = {int(a): int(b) for a, b in
            re.findall(r'<element id="(\d+)"[^>]*page="(\d+)"', x)}
    first = {}
    for a, b in re.findall(r'<event elid="(\d+)" position="(\d+)"', x):
        first.setdefault(int(a), int(b) / 1000)

    out = []
    for i, s in enumerate(starts):
        t0 = first.get(s, 0.0)
        t1 = first.get(starts[i + 1], total_seconds) if i + 1 < len(starts) else total_seconds
        numeral, tempo = "", tempos.get(s, "")
        m = ROMAN.match(tempo)
        if m:
            numeral, tempo = m.group(1) + ".", m.group(2).strip()
        out.append({"numeral": numeral, "tempo": tempo, "measure": s,
                    "page": page.get(s, 0) + 1, "seconds": max(t1 - t0, 0)})
    return out


TAG = re.compile(r"<(polyline|line)\b([^>]*)>")
ATTR = re.compile(r'([\w:-]+)="([^"]*)"')


def barline_xs(svg_text: str) -> list[float]:
    """The x of every barline on a page, in viewBox units.

    Attributes are read by name, in any order: MuseScore writes class first,
    but a minifier need not (svgo puts it last), and a pattern that assumed
    the order found no barlines at all on such a page (audit M16). A
    polyline's points alternate x and y."""
    xs = []
    for m in TAG.finditer(svg_text):
        attrs = dict(ATTR.findall(m.group(2)))
        if "BarLine" not in attrs.get("class", "").split():
            continue
        if m.group(1) == "polyline":
            nums = re.findall(r"-?[\d.]+", attrs.get("points", ""))
            xs += [float(v) for v in nums[0::2]]
        else:
            xs += [float(attrs[k]) for k in ("x1", "x2") if k in attrs]
    return xs


def mpos_scale(mpos_text: str, pages: list[Path]) -> tuple[float, float, float]:
    """How .mpos units map onto the SVG pages: (units per SVG unit, viewBox
    width, viewBox height).

    Measured, not assumed. MuseScore 4.7 writes pages at 1200 units per
    inch and 3.6 at 360, and neither documents the .mpos unit; but on any
    page the rightmost bar box ends where the rightmost barline stands, so
    their ratio is the scale. Taken from the first page that has both."""
    boxes = {}
    for bx, bw, pg in re.findall(r'<element id="\d+" x="([\d.]+)" y="[\d.]+" '
                                 r'sx="([\d.]+)" sy="[\d.]+" page="(\d+)"', mpos_text):
        boxes[int(pg)] = max(boxes.get(int(pg), 0.0), float(bx) + float(bw))
    for i, svg in enumerate(pages):
        text = svg.read_text()
        vb = re.search(r'viewBox="[\d.\-]+ [\d.\-]+ ([\d.]+) ([\d.]+)"', text[:4096])
        xs = barline_xs(text)
        if vb and xs and boxes.get(i):
            return boxes[i] / max(xs), float(vb.group(1)), float(vb.group(2))
    die("could not relate the bar boxes to the pages: no page has both a "
        "barline and a bar box")


def timing(mpos: Path, playback: Path, pages: list[Path], mvts: list[dict],
           seconds: float) -> dict:
    """The follow-along map: every bar's box as fractions of its page (from
    the layout's .mpos), the bars in the order they sound (from the .mpos of
    the playback that made the audio), and where each movement begins."""
    x = mpos.read_text()
    k, vbw, vbh = mpos_scale(x, pages)
    w, h = k * vbw, k * vbh
    boxes = {}
    for i, bx, by, bw, bh, pg in re.findall(
            r'<element id="(\d+)" x="([\d.]+)" y="([\d.]+)" sx="([\d.]+)" '
            r'sy="([\d.]+)" page="(\d+)"', x):
        boxes[int(i)] = [int(pg) + 1, round(float(bx) / w, 4), round(float(by) / h, 4),
                         round(float(bw) / w, 4), round(float(bh) / h, 4)]
    stray = [i for i, b in boxes.items() if b[1] + b[3] > 1.02 or b[2] + b[4] > 1.02]
    if stray:
        warn(f"{len(stray)} bar boxes fall outside their page — the measured "
             f".mpos scale ({k:.3f}) looks wrong for this engraver")
    events = sorted((int(t), int(i)) for i, t in
                    re.findall(r'<event elid="(\d+)" position="(\d+)"', playback.read_text()))
    if boxes and any(i not in boxes for _, i in events):
        die("the playback's bars do not match the layout's — was the score "
            "changed between the two exports?")
    return {
        "version": 1,
        "label": AUDIO_LABEL,
        "audio": AUDIO,
        "duration": round(seconds, 2),
        "measures": [boxes.get(i) for i in range(max(boxes) + 1)] if boxes else [],
        "events": [[t, i] for t, i in events],
        "movements": [m["measure"] for m in mvts] if len(mvts) > 1 else [],
    }


def selection(spec: str | None, sections: int) -> int | None:
    """How many leading movements to publish: None for all of them.

    Only a leading run can be published: a gap would leave the reader's
    pages, the realization and the timing with a hole in the middle."""
    if spec is None or str(spec).strip().lower() == "all":
        return None
    nums = set()
    for part in str(spec).replace(" ", "").split(","):
        lo, _, hi = part.partition("-")
        if not lo.isdigit() or (hi and not hi.isdigit()):
            die(f"--movements {spec!r}: expected something like 1-2 or 1,2")
        nums.update(range(int(lo), int(hi or lo) + 1))
    k = len(nums)
    if nums != set(range(1, k + 1)):
        die(f"--movements {spec!r}: only the leading movements can be published (1-{k}, say)")
    if sections < 2:
        die("--movements needs a score whose movements are separated by section breaks")
    if k > sections:
        die(f"--movements {spec!r}: the score has only {sections} movements")
    return None if k == sections else k


def trim_audio(path: Path, seconds: float) -> None:
    """Cut the realization at `seconds` with a short fade, in place."""
    out = path.with_name("trimmed-" + path.name)
    fade = min(2.0, seconds / 4)
    r = subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-i", str(path),
                        "-t", f"{seconds:.3f}",
                        "-af", f"afade=t=out:st={max(0.0, seconds - fade):.3f}:d={fade:.3f}",
                        "-codec:a", "libmp3lame", "-b:a", "128k", str(out)],
                       capture_output=True, text=True)
    if r.returncode != 0 or not out.exists() or out.stat().st_size == 0:
        die(f"could not trim the realization (ffmpeg exit {r.returncode}):\n{r.stderr[-2000:]}")
    out.replace(path)


def roman(n: int) -> str:
    out = ""
    for value, sym in ((10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")):
        while n >= value:
            out, n = out + sym, n - value
    return out


def stale_properties(root: ET.Element, meta: dict) -> list[str]:
    """Project properties that disagree with the printed title page.

    MuseScore copies File → Project properties along with Save As, so a
    score begun from another project carries that project's work title —
    and the work title is what a PDF export writes as the document title,
    shown in every viewer's title bar. Nothing on the page reads these;
    a downloaded PDF does."""
    props = {t.get("name"): (t.text or "").strip() for t in root.iter("metaTag")}
    frames = meta.get("textFramesData", {})
    printed_title = " ".join(frames.get("titles") or [])
    printed_sub = " ".join(frames.get("subtitles") or [])
    out = []
    if props.get("workTitle") and props["workTitle"] != printed_title:
        out.append(f"work title is {props['workTitle']!r}, the score is titled "
                   f"{printed_title!r} — a PDF export takes the work title")
    if props.get("subtitle") and props["subtitle"] != printed_sub:
        out.append(f"subtitle is {props['subtitle']!r}, the score prints "
                   f"{printed_sub or 'none'!r}")
    if props.get("composer") in ("Composer / arranger", "Composer"):
        out.append(f"composer is the placeholder {props['composer']!r}")
    return out


def minutes(seconds: float) -> str:
    """Durations as a programme gives them: to the half minute, or in
    seconds under a minute. Typed with straight quotes; the site sets
    them as primes."""
    if seconds < 60:
        return f'{round(seconds)}"'
    half = round(seconds / 30) / 2
    whole = int(half)
    return f"{whole}½'" if half - whole else f"{whole}'"


# ---------------------------------------------------------------------------
# Manifest and frontmatter
# ---------------------------------------------------------------------------

def source_key(source: Path) -> str:
    try:
        return str(source.resolve().relative_to(SCORES_DIR.resolve()))
    except ValueError:
        home = Path.home()
        s = source.resolve()
        return "~/" + str(s.relative_to(home)) if s.is_relative_to(home) else str(s)


def source_path(key: str) -> Path:
    p = Path(key).expanduser()
    return p if p.is_absolute() else SCORES_DIR / p


def read_manifest(slug: str) -> dict | None:
    f = MUSIC / slug / MANIFEST
    return yaml.safe_load(f.read_text()) if f.exists() else None


def write_manifest(slug: str, data: dict) -> None:
    head = ("# Written by tools/music-import.py. The score pages in scores/ are\n"
            "# not versioned; this records how to regenerate them:\n"
            f"#   tools/music-import.py refresh {slug}\n")
    body = yaml.safe_dump(data, sort_keys=False, allow_unicode=True)
    sitelib.atomic_write_text(MUSIC / slug / MANIFEST, head + body)


def frontmatter(slug: str) -> dict | None:
    f = MUSIC / slug / "index.md"
    if not f.exists():
        return None
    try:
        return front_matter.load(f.read_text(encoding="utf-8"))
    except yaml.YAMLError as exc:
        sys.exit(f"music-import: {f}: front matter is not valid YAML: {exc}")


def scaffold(slug: str, meta: dict, mvts: list[dict], pdf: bool, created: str,
             published: int | None = None) -> None:
    title = (meta.get("textFramesData", {}).get("titles") or [meta.get("title")])[0] or slug
    composer = " ".join(meta.get("textFramesData", {}).get("composers") or [meta.get("composer", "")])
    # "L. Neuwirth (2025)" or "(2022-2023)" on the title page — possibly
    # followed by an opus line — else the file's creation date. A range
    # gives the finishing year and a `composed:` span.
    span = re.search(r"\((\d{4})(?:\s*[-–]\s*(\d{2,4}))?", composer)
    year, composed = None, None
    if span:
        start, end = span.group(1), span.group(2)
        if end:
            end = start[:4 - len(end)] + end          # "2023-24" → 2024
        year = end or start
        if end and end != start:
            composed = f"{start} – {end}"
    elif re.match(r"\d{4}", created):
        year = created[:4]
    opus = (re.findall(r"\bop\.\s*(\d+[a-z]?(?:\s*(?:no\.|/)\s*\d+)?)", composer, re.I) or [None])[0]
    subtitle = " ".join(meta.get("textFramesData", {}).get("subtitles") or []).strip()
    parts = [p["name"] for p in meta.get("parts", [])]
    voices = sum(1 for p in meta.get("parts", []) if p.get("instrumentId") in VOICES)
    category = ("orchestral" if len(parts) > 12 else
                "choral" if voices >= 3 else
                "vocal" if voices else
                "solo" if len(parts) == 1 else "chamber")
    # A concerto's soloist has a part named "Solo …": forces read
    # "bassoon and orchestra" rather than "orchestra".
    solo = [p[5:].strip() for p in parts if p.lower().startswith("solo ")]
    forces = ((f"{solo[0].lower()} and orchestra" if solo else "orchestra")
              if category == "orchestral" else
              " and ".join(re.sub(r"\s+\d+$", "", p).lower() for p in parts))
    # A partial work's duration is what is published, not the whole.
    heard = (sum(m["seconds"] for m in mvts[:published]) if published
             else float(meta.get("duration", 0)))
    fm = {"title": title}
    # `date` is the page's publication date — the feed, the New page and
    # the footer's version history all read it — so a new page is dated
    # today. When the work was finished goes in `completed:`, by hand.
    fm["date"] = dt.date.today()
    if year:
        fm["year"] = int(year)
    if composed:
        fm["composed"] = composed
    if opus:
        fm["opus"] = opus
    if subtitle and DEDICATION.match(subtitle):
        fm["dedication"] = subtitle
    fm |= {"tags": ["music"], "instrumentation": forces,
           "duration": f"ca. {max(round(heard / 60), 1)}'",
           "category": category, "score-dir": f"{PAGES}/"}
    if pdf:
        fm["pdf"] = f"{PAGES}/{slug}.pdf"
    if len(mvts) > 1:
        fm["movements"] = [
            {"name": m["numeral"] or f"{roman(i + 1)}.", "status": "in revision"}
            if published is not None and i >= published else
            {"name": m["numeral"] or f"{roman(i + 1)}.", **({"tempo": m["tempo"]} if m["tempo"] else {}),
             "page": m["page"], "duration": minutes(m["seconds"])}
            for i, m in enumerate(mvts)]
    text = "---\n" + yaml.safe_dump(fm, sort_keys=False, allow_unicode=True) + "---\n"
    (MUSIC / slug / "index.md").write_text(text)
    print("  scaffolded index.md — check the date, forces, and movement names,\n"
          "  and add `scoring:` by hand: divisi does not show in the part list.")
    if subtitle and "dedication" not in fm:
        print(f"  the title page's subtitle {subtitle!r} was not used — place it by hand")
    if str(meta.get("hasLyrics")).lower() == "true" and not meta.get("poet"):
        print("  the score sets a text but credits no poet — add `text:` for the author")


def compare_movements(slug: str, mvts: list[dict], published: int | None = None) -> None:
    """Check index.md's movements against the sections of this export.

    Counts are compared before anything else, and a score that has
    collapsed to a single section is a count mismatch like any other:
    five declared movements against one section means the reader's
    movement buttons all point into a score that no longer has them."""
    fm = frontmatter(slug) or {}
    declared = fm.get("movements") or []
    sections = len(mvts) if len(mvts) > 1 else 0   # one section = no movements
    if len(declared) != sections:
        have = f"{sections} sections" if sections else "a single section"
        warn(f"{slug}: index.md lists {len(declared)} movement(s), this export "
             f"has {have} — update `movements:`")
    if not declared or not sections:
        return
    for i, (d, m) in enumerate(zip(declared, mvts)):
        if published is not None and i >= published:
            if not d.get("status"):
                warn(f"{slug}: movement {d.get('name')!r} is withheld from this export — "
                     f"give it `status: in revision` in index.md")
            continue
        if d.get("page") != m["page"]:
            warn(f"{slug}: movement {d.get('name')!r} is at page {m['page']} "
                 f"in this export, index.md says {d.get('page')} — update `page:`")


# ---------------------------------------------------------------------------
# Shrinking the pages
# ---------------------------------------------------------------------------

SHRUNK = "<!-- shrunk by tools/music-import.py, v1 -->"
DECIMAL = re.compile(r"-?\d*\.\d+")
GLYPH = re.compile(r'<path class="([^"]*)" transform="([^"]*)" d="([^"]*)"\s*/>')


def whole(text: str) -> str:
    """Every decimal in text rounded to a whole number."""
    def one(m: re.Match) -> str:
        v = round(float(m.group()))
        return "0" if v == 0 else str(v)
    return DECIMAL.sub(one, text)


def shrink_page(svg: str, ids: str) -> str:
    """A page as the site publishes it; `ids` prefixes the ids of the
    outlines moved into <defs>, which must be unique across the site. Pure,
    and idempotent: a shrunk page comes back unchanged.

    MuseScore draws only in absolute coordinates (M, L, C; no relative
    commands, no exponents), so rounding a number moves only its own point.
    In a glyph's matrix only the translation is rounded: the scale is what
    turns font units into page units."""
    if SHRUNK in svg:
        return svg
    svg = re.sub(r"<\?xml[^>]*\?>\s*", "", svg, count=1)
    svg = re.sub(r"<title>.*?</title>\s*|<desc>.*?</desc>\s*", "", svg, flags=re.S)
    svg = re.sub(r'\b(d|points)="([^"]*)"', lambda m: f'{m.group(1)}="{whole(m.group(2))}"', svg)

    def matrix(m: re.Match) -> str:
        parts = m.group(1).split(",")
        return 'transform="matrix(' + ",".join(parts[:4] + [whole(v) for v in parts[4:]]) + ')"'
    svg = re.sub(r'transform="matrix\(([^)]*)\)"', matrix, svg)

    # Outlines drawn more than once: noteheads, rests, accidentals, clefs.
    # Only a bare <path class transform d> is moved; one with a fill or
    # stroke of its own keeps it on the element the stylesheet matches.
    counts: dict[str, int] = {}
    for m in GLYPH.finditer(svg):
        counts[m.group(3)] = counts.get(m.group(3), 0) + 1
    defs: dict[str, str] = {}

    def use(m: re.Match) -> str:
        cls, transform, d = m.groups()
        if counts[d] < 2:
            return m.group(0)
        if d not in defs:
            defs[d] = f"{ids}{len(defs)}"
        return f'<use class="{cls}" href="#{defs[d]}" transform="{transform}"/>'
    svg = GLYPH.sub(use, svg)
    block = "".join(f'<path id="{i}" d="{d}"/>' for d, i in defs.items())
    head = SHRUNK + (f"\n<defs>{block}</defs>" if block else "")
    return re.sub(r"(<svg\b[^>]*>)", lambda m: m.group(1) + "\n" + head, svg, count=1)


def page_index(page: Path) -> int:
    """page-7.svg (MuseScore 4) and page-07.svg (MuseScore 3) alike."""
    return int(page.stem.rsplit("-", 1)[1])


def shrink_pages(slug: str, pages: list[Path]) -> tuple[int, int]:
    """Shrink each page file in place; (bytes before, bytes after)."""
    before = after = 0
    for page in pages:
        text = page.read_text(encoding="utf-8")
        out = shrink_page(text, f"{slug}-{page_index(page)}-")
        before += len(text.encode()); after += len(out.encode())
        if out != text:
            sitelib.atomic_write_text(page, out)
    return before, after


def thumbnail(page: Path, out: Path) -> bool:
    """Write page as a greyscale WebP THUMB_WIDTH wide to out; False, with a
    warning, when the tools are missing or fail."""
    rsvg = shutil.which("rsvg-convert")
    try:
        from PIL import Image, features
        webp = features.check("webp")
    except ImportError:
        webp = False
    if not rsvg or not webp:
        warn(f"no thumbnail for {page.parent.parent.name}: needs rsvg-convert and Pillow "
             f"with WebP; the pages show the SVG instead")
        return False
    png = out.with_name(f".{out.stem}.png")
    r = subprocess.run([rsvg, "-w", str(THUMB_WIDTH), "-b", "white", "-o", str(png), str(page)],
                       capture_output=True, text=True)
    if r.returncode != 0:
        png.unlink(missing_ok=True)
        warn(f"rsvg-convert could not render {page}: {r.stderr.strip()[-500:]}")
        return False
    with sitelib.atomic_path(out) as tmp, Image.open(png) as im:
        im.convert("L").save(tmp, "WEBP", quality=80, method=6)
    png.unlink()
    return True


def page_problem(page: Path) -> str | None:
    """Why a page cannot be a whole SVG, or None. Reads its two ends only."""
    with page.open("rb") as f:
        head = f.read(256).lstrip()
        f.seek(max(0, page.stat().st_size - 64))
        tail = f.read().rstrip()
    if not (head.startswith(b"<svg") or head.startswith(b"<?xml")):
        return "does not begin as an SVG"
    if not tail.endswith(b"</svg>"):
        return "is cut short"
    return None


# ---------------------------------------------------------------------------
# Commands
# ---------------------------------------------------------------------------

def install(slug: str, source: Path, pdf: bool, audio: bool, force: bool, new: bool,
            movements_spec: str | None = None, engraver: str = "mscore") -> None:
    if not source.exists():
        die(f"no such score: {source}")
    stage = STAGING / slug
    if stage.exists():
        die(f"{slug}: an interrupted import left {stage}. Recover its saved files "
            "and remove the staging directory before importing again; run check for details.")
    dest = MUSIC / slug / PAGES
    old = read_manifest(slug)
    meta = score_meta(source)
    with tempfile.TemporaryDirectory(prefix="music-import-") as tmp:
        tmpdir = Path(tmp)
        got = export(source, tmpdir, f"{slug}.pdf" if pdf else None, audio, engraver)
        root = read_mscx(source)
        mvts = movements(root, got["mpos"], float(meta.get("duration", 0)))
        k = selection(movements_spec, len(mvts) if len(mvts) > 1 else 0)
        cut = None
        if k is not None:
            cut = cut_point(got["mpos"], mvts, k, got["playback"])
            got["pages"] = got["pages"][:cut["page"] - 1]
        n = len(got["pages"])
        version = mscore_version(engraver)
        if old and old.get("pages") != n and not force:
            die(f"{slug}: this export has {n} pages, the manifest records "
                f"{old.get('pages')} ({old.get('engraver')} → {version}). Every "
                f"movement `page:` may have moved. Re-run with --force, then fix index.md.")
        if old and old.get("engraver") != version:
            warn(f"{slug}: engraver changed, {old.get('engraver')} → {version}; "
                 f"page count unchanged, but look the pages over")
        created = next((t.text or "" for t in root.iter("metaTag")
                        if t.get("name") == "creationDate"), "")
        for problem in stale_properties(root, meta):
            warn(f"{slug}: {source.name} project properties: {problem} "
                 f"(fix in MuseScore: File → Project properties)")
        follow = None
        if got["audio"]:
            follow = timing(got["mpos"], got["playback"], got["pages"], mvts,
                            float(meta.get("duration", 0)))
            if cut:
                follow["measures"] = follow["measures"][:cut["measure"]]
                follow["events"] = [e for e in follow["events"] if e[0] < cut["ms"]]
                follow["movements"] = follow["movements"][:k]
                follow["duration"] = round(cut["ms"] / 1000, 2)
                trim_audio(got["audio"], cut["ms"] / 1000)

        # The timing was measured on the pages as exported; only now shrink.
        before, after = shrink_pages(slug, got["pages"])
        thumb = tmpdir / THUMB
        made_thumb = bool(got["pages"]) and thumbnail(got["pages"][0], thumb)

        # Every export has succeeded. Assemble the new score directory beside
        # the published one, then swap them.
        # Exclusive creation also protects a second importer that finished
        # exporting while this one was publishing. Never discard recovery
        # files left by an earlier run.
        stage.mkdir(parents=True)
        new_scores = stage / PAGES
        new_scores.mkdir()
        manifest = MUSIC / slug / MANIFEST
        if manifest.exists():
            shutil.copy2(manifest, stage / MANIFEST)
        # A thumbnail of an earlier page 1 is never carried over.
        produced = {AUDIO, TIMING, THUMB} | ({got["pdf"].name} if got["pdf"] else set())
        if dest.is_dir():
            # What this import does not write stays: a PDF from an earlier
            # --pdf import, say. A realization and its timing stand or fall
            # together; re-exported without --audio, both go, rather than
            # keep a timing map for a layout that may have moved.
            for f in dest.iterdir():
                if f.is_file() and not f.name.startswith("page-") and f.name not in produced:
                    shutil.copy2(f, new_scores / f.name)
        for f in got["pages"]:
            shutil.move(str(f), new_scores / f.name)
        if got["pdf"]:
            shutil.move(str(got["pdf"]), new_scores / got["pdf"].name)
        if made_thumb:
            shutil.move(str(thumb), new_scores / THUMB)
        if got["audio"]:
            shutil.move(str(got["audio"]), new_scores / AUDIO)
            (new_scores / TIMING).write_text(json.dumps(follow, separators=(",", ":")))
        dest.parent.mkdir(parents=True, exist_ok=True)
        if dest.exists():
            os.rename(dest, stage / f"{PAGES}.old")
        os.rename(new_scores, dest)

    write_manifest(slug, {
        "source": source_key(source),
        "engraver": version,
        **({"engraver-cli": engraver} if engraver != "mscore" else {}),
        "pages": n,
        "pdf": pdf,
        "audio": "Muse Sounds" if audio else False,
        **({"movements": f"1-{k}"} if k else {}),
        "exported": dt.datetime.now().isoformat(timespec="seconds"),
    })
    print(f"{slug}: {n} pages from {source.name} ({version})"
          + (", with a Muse Sounds realization" if audio else "")
          + f"; pages shrunk {before / 1e6:.1f} → {after / 1e6:.1f} MB")
    for i, m in enumerate(mvts if len(mvts) > 1 else []):
        held = k is not None and i >= k
        print(f"  {m['numeral'] or '·':5} "
              + ("withheld" if held else
                 f"page {m['page']:>4}  {minutes(m['seconds']):>5}  {m['tempo']}"))
    if new and not (MUSIC / slug / "index.md").exists():
        scaffold(slug, meta, mvts, pdf, created, k)
    else:
        compare_movements(slug, mvts, k)
    # Keep the recovery marker until the manifest and any new index have
    # been written too. Even a same-page-count import must fail the deploy
    # check if publication stops between the directory swap and metadata.
    shutil.rmtree(stage)


def cut_point(mpos: Path, mvts: list[dict], k: int, playback: Path | None = None) -> dict:
    """Where a partial work stops: the page, bar and moment at which its
    first withheld movement begins — which must be the top of a page, or
    the last published page would show the start of unpublished music."""
    x = mpos.read_text()
    page = {int(i): int(p) + 1 for i, p in
            re.findall(r'<element id="(\d+)"[^>]*page="(\d+)"', x)}
    first = {}
    for i, t in re.findall(r'<event elid="(\d+)" position="(\d+)"',
                           (playback or mpos).read_text()):
        first.setdefault(int(i), int(t))
    held = mvts[k]
    bar = held["measure"]
    name = (held["numeral"] or roman(k + 1)).rstrip(".")
    before = (mvts[k - 1]["numeral"] or roman(k)).rstrip(".")
    if page.get(bar - 1) == held["page"]:
        die(f"movement {name} begins partway down page {held['page']}, which would "
            f"publish its opening. In MuseScore, put a page break at the end of "
            f"movement {before} and try again.")
    return {"page": held["page"], "measure": bar, "ms": first.get(bar, 0)}


def cmd_import(a: argparse.Namespace) -> None:
    if not re.fullmatch(r"[a-z0-9-]+", a.slug):
        die(f"invalid slug {a.slug!r} (lowercase a-z, 0-9, hyphens)")
    install(a.slug, Path(a.score).expanduser(), a.pdf, a.audio, a.force, new=True,
            movements_spec=a.movements, engraver=a.engraver or "mscore")


def pieces() -> list[str]:
    return sorted(p.parent.name for p in MUSIC.glob(f"*/{MANIFEST}"))


def cmd_refresh(a: argparse.Namespace) -> None:
    for slug in a.slugs or pieces():
        m = read_manifest(slug)
        if not m:
            warn(f"{slug}: no {MANIFEST}; import it first")
            continue
        install(slug, source_path(m["source"]), bool(m.get("pdf")),
                a.audio or bool(m.get("audio")), a.force, new=False,
                movements_spec=a.movements if a.movements is not None else m.get("movements"),
                engraver=a.engraver or m.get("engraver-cli", "mscore"))


def cmd_shrink(a: argparse.Namespace) -> None:
    total_before = total_after = 0
    for slug in a.slugs or pieces():
        pages = sorted((MUSIC / slug / PAGES).glob("page-*.svg"), key=page_index)
        if not pages:
            warn(f"{slug}: no pages on disk")
            continue
        before, after = shrink_pages(slug, pages)
        total_before += before; total_after += after
        print(f"{slug}: {len(pages)} pages, {before / 1e6:.1f} → {after / 1e6:.1f} MB")
    if total_before:
        print(f"all: {total_before / 1e6:.1f} → {total_after / 1e6:.1f} MB")


def cmd_thumbnail(a: argparse.Namespace) -> None:
    for slug in a.slugs or pieces():
        pages = sorted((MUSIC / slug / PAGES).glob("page-*.svg"), key=page_index)
        if not pages:
            warn(f"{slug}: no pages on disk")
            continue
        out = MUSIC / slug / PAGES / THUMB
        if thumbnail(pages[0], out):
            print(f"{slug}: {out.relative_to(SITE_ROOT)} ({out.stat().st_size // 1024} KB)")


def cmd_check(_: argparse.Namespace) -> None:
    problems = 0
    for left in sorted(STAGING.glob("*/")) if STAGING.is_dir() else []:
        print(f"{left.name}: an interrupted import left {STAGING.name}/{left.name}/ — if "
              f"content/music/{left.name}/{PAGES} is missing, its previous pages are in "
              f"{PAGES}.old there. Recover that directory and the saved {MANIFEST}, "
              "if present; inspect the published files before removing staging and re-running")
        problems += 1
    for index in sorted(MUSIC.glob("*/index.md")):
        slug = index.parent.name
        fm = frontmatter(slug) or {}
        if not fm.get("score-dir"):
            continue
        m = read_manifest(slug)
        pages = list((MUSIC / slug / PAGES).glob("page-*.svg"))
        have = len(pages)
        broken = [(p.name, why) for p in pages if (why := page_problem(p))]
        for name, why in broken:
            print(f"{slug}: {name} {why} — tools/music-import.py refresh {slug}")
        problems += len(broken)
        unshrunk = sum(1 for p in pages if SHRUNK.encode() not in p.open("rb").read(512))
        if unshrunk:
            print(f"{slug}: {unshrunk} page(s) not shrunk — tools/music-import.py shrink {slug}")
        if pages and not (MUSIC / slug / PAGES / THUMB).exists():
            print(f"{slug}: no thumbnail — tools/music-import.py thumbnail {slug}")
        if not m:
            print(f"{slug}: declares a score but has no {MANIFEST}")
            problems += 1
        elif have != m.get("pages"):
            print(f"{slug}: {have} pages on disk, manifest records {m.get('pages')} "
                  f"— tools/music-import.py refresh {slug}")
            problems += 1
        elif m.get("audio") and not all((MUSIC / slug / PAGES / f).exists()
                                        for f in (AUDIO, TIMING)):
            print(f"{slug}: the manifest records a realization, but it is missing "
                  f"— tools/music-import.py refresh {slug}")
            problems += 1
        elif not source_path(m["source"]).exists():
            print(f"{slug}: source {m['source']} is missing (pages present)")
        elif source_path(m["source"]).stat().st_mtime > \
                dt.datetime.fromisoformat(str(m["exported"])).timestamp():
            print(f"{slug}: {m['source']} changed since the {m['exported']} export")
    if problems:
        sys.exit(1)


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    sub = ap.add_subparsers(dest="cmd", required=True)
    p = sub.add_parser("import", help="export a score and scaffold its page")
    p.add_argument("score"); p.add_argument("slug")
    p.add_argument("--pdf", action="store_true", help="also export a downloadable PDF")
    p.add_argument("--audio", action="store_true", help="also render a Muse Sounds realization")
    p.add_argument("--movements", help="publish only the leading movements, e.g. 1-2")
    p.add_argument("--engraver", choices=sorted(ENGRAVERS), help="mscore3 for a MuseScore 3 score")
    p.add_argument("--force", action="store_true", help="accept a changed page count")
    p.set_defaults(fn=cmd_import)
    p = sub.add_parser("refresh", help="re-export pieces from their manifests")
    p.add_argument("slugs", nargs="*")
    p.add_argument("--audio", action="store_true", help="add a Muse Sounds realization")
    p.add_argument("--movements", help="publish only the leading movements (1-2), or all")
    p.add_argument("--engraver", choices=sorted(ENGRAVERS), help="override the manifest's engraver")
    p.add_argument("--force", action="store_true", help="accept a changed page count")
    p.set_defaults(fn=cmd_refresh)
    p = sub.add_parser("shrink", help="shrink pages exported before shrinking existed")
    p.add_argument("slugs", nargs="*")
    p.set_defaults(fn=cmd_shrink)
    p = sub.add_parser("thumbnail", help="write page 1's thumbnail for pieces already imported")
    p.add_argument("slugs", nargs="*")
    p.set_defaults(fn=cmd_thumbnail)
    p = sub.add_parser("check", help="report missing or stale score pages")
    p.set_defaults(fn=cmd_check)
    a = ap.parse_args()
    a.fn(a)


if __name__ == "__main__":
    main()
