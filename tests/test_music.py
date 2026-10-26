"""Tests for the music section: the importer, the data it leaves, the pages.

A composition reaches the site through three hands that must agree:

  * ``tools/music-import.py``  — exports a MuseScore score into numbered page
    SVGs, a manifest, and (with --audio) a realization plus ``timing.json``
  * ``build/Contexts.hs`` and ``build/Catalog.hs`` — the composition page,
    the score reader and the /music/ index, built from those files
  * ``static/js/score-reader.js`` and ``score-follow.js`` — which turn pages
    and follow the music from ``timing.json``

None of it fails loudly. A wrong .mpos scale draws every bar mark in the
wrong place; a movement whose `page:` drifted after a reflow turns the
reader to the wrong page; a realization whose movement starts do not line up
one for one with the reader's movement buttons simply leaves the buttons
unwired (score-follow.js checks the counts and gives up). So:

  * ``ImporterTests`` drives the importer's pure logic on small synthetic
    scores — section breaks, the measured scale, the timing map, partial
    works, and what ``check`` refuses. It never runs MuseScore.
  * ``ScoreDataTests`` checks the committed and exported files of every
    published piece against each other, in the shapes the JavaScript reads.
    The pages are not versioned, so on a fresh clone these skip.
  * ``BuiltMusicPageTests`` reads ``_site``: the reader, the composition
    pages, and the shelf and catalogue on /music/.

Run with:  .venv/bin/python -m unittest tests.test_music
"""

from __future__ import annotations

import contextlib
import datetime as dt
import io
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import unittest
import xml.etree.ElementTree as ET
from pathlib import Path
from unittest import mock
from urllib.parse import urljoin, urlsplit
from tests._helpers import SITE_ROOT, load_tool

try:
    import yaml

    HAVE_YAML = True
except ImportError:  # pragma: no cover — exercised only on a bare machine
    HAVE_YAML = False

SITE_DIR = SITE_ROOT / "_site"
MUSIC_DIR = SITE_ROOT / "content" / "music"

if HAVE_YAML:
    music_import = load_tool("music-import.py", register=True)
    _front_matter = music_import.front_matter   # tools/front_matter.py

HAVE_THUMB_TOOLS = bool(__import__("shutil").which("rsvg-convert"))
if HAVE_THUMB_TOOLS:
    try:
        from PIL import features as _pil_features
        HAVE_THUMB_TOOLS = _pil_features.check("webp")
    except ImportError:
        HAVE_THUMB_TOOLS = False

# Pages whose engraving is known to run past the foot of the page. Each is a
# defect in the score, tracked by its own expected-failure test below; the
# general box check leaves them out so it still guards every other page.
KNOWN_OVERFLOW = {("bassoon-concerto", 68)}

# Keep in step with categoryOrder in build/Catalog.hs.
CATEGORY_ORDER = ["orchestral", "chamber", "solo", "vocal", "choral", "electronic", "other"]


# ---------------------------------------------------------------------------
# Synthetic MuseScore output
# ---------------------------------------------------------------------------

SECTION = "<LayoutBreak><subtype>section</subtype></LayoutBreak>"


def measure(tempo: str | None = None, section: bool = False) -> str:
    """One <Measure>, optionally opening with tempo text and/or closing a
    section. `tempo` is the inner XML of the tempo's <text>."""
    t = f"<voice><Tempo><text>{tempo}</text></Tempo></voice>" if tempo else ""
    return f"<Measure>{t}{SECTION if section else ''}</Measure>"


def frame(section: bool = True) -> str:
    """A vertical frame, where MuseScore often puts the break between movements."""
    return f"<VBox>{SECTION if section else ''}</VBox>"


def mscx(*items: str) -> ET.Element:
    return ET.fromstring('<museScore version="4.70"><Score><Staff id="1">'
                         + "".join(items) + "</Staff></Score></museScore>")


def mpos(boxes: dict[int, tuple[float, float, float, float, int]],
         events: list[tuple[int, int]]) -> str:
    """An .mpos: bar boxes (x, y, width, height, 0-based page) and the
    (bar, milliseconds) events, in the order given."""
    els = "".join(f'<element id="{i}" x="{x}" y="{y}" sx="{sx}" sy="{sy}" page="{p}"/>'
                  for i, (x, y, sx, sy, p) in boxes.items())
    evs = "".join(f'<event elid="{i}" position="{t}"/>' for i, t in events)
    return f'<score><elements>{els}</elements><events>{evs}</events></score>'


def page_svg(width: float, height: float, barlines: list[float]) -> str:
    lines = "".join(f'<polyline class="BarLine" fill="none" points="{x},500 {x},900"/>'
                    for x in barlines)
    return (f'<svg width="215.9mm" height="279.4mm" viewBox="0 0 {width} {height}" '
            f'xmlns="http://www.w3.org/2000/svg">{lines}</svg>')


# A MuseScore 4 page in miniature: the shapes shrink_page() rewrites and the
# ones it must leave alone.
GLYPH_D = "M0,13.9063 C0,28.1406 10.2656,44.0156 37.0781,44.0156 L107.578,-13.2344"
SAMPLE_PAGE = ('<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n'
        '<svg width="215.9mm" height="279.4mm" viewBox="0 0 10200 13200"\n'
        ' xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.2">\n'
        '<title>s5_condensed (3)</title>\n<desc>Generated by MuseScore Studio 4.7.5</desc>\n'
        '<path class="" fill="#ffffff" fill-rule="evenodd" d="M0,0 L10200,0 L10200,13200 L0,13200 L0,0 "/>\n'
        '<polyline class="StaffLines" fill="none" stroke="#000000" stroke-width="4.22" points="1360.93,1051.89 9491.34,1051.89" />\n'
        '<polyline class="BarLine" fill="none" stroke="#000000" stroke-width="6.91" points="4379.42,1049.77 4379.42,1391.07" />\n'
        + "".join(f'<path class="Note" transform="matrix(0.464457,0,0,0.464457,{x},1090.29)" d="{GLYPH_D}"/>\n'
                  for x in ("3299.75", "3873.73", "-0.4"))
        + '<path class="Clef" transform="matrix(0.464457,0,0,0.464457,1400.5,1100.5)" d="M1.5,2.5 L3.5,4.5"/>\n'
        '<path class="SlurSegment" stroke="#000000" stroke-width="2.5" fill-rule="evenodd" d="M10.4,20.6 C30.5,40.5 50.5,60.5 70.5,80.5"/>\n'
        '</svg>\n')


def quiet(fn, *args, **kwargs):
    """Call fn with stdout and stderr captured; return (result, out, err)."""
    out, err = io.StringIO(), io.StringIO()
    with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
        result = fn(*args, **kwargs)
    return result, out.getvalue(), err.getvalue()


def front_matter(index: Path) -> dict:
    return _front_matter.load(index.read_text(encoding="utf-8"))


def page_number(path: Path) -> int:
    """page-7.svg (MuseScore 4) and page-07.svg (MuseScore 3) alike."""
    m = re.fullmatch(r"page-0*(\d+)\.svg", path.name)
    return int(m.group(1)) if m else -1


# ---------------------------------------------------------------------------
# The importer
# ---------------------------------------------------------------------------

@unittest.skipUnless(HAVE_YAML, "PyYAML not installed")
class ImporterTests(unittest.TestCase):
    """tools/music-import.py on synthetic scores; MuseScore is never run."""

    def setUp(self) -> None:
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.tmp = Path(self._tmp.name)

    def write(self, name: str, text: str) -> Path:
        p = self.tmp / name
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(text, encoding="utf-8")
        return p

    def two_movements(self) -> tuple[ET.Element, Path]:
        """Movement I in bars 0–2 on page 1; II in bars 3–5 on page 2, each
        bar two seconds long."""
        root = mscx(measure("I. Allegro"), measure(), measure(section=True),
                    measure("II – <sym>metNoteQuarterUp</sym> = 60"), measure(),
                    measure(section=True))
        boxes = {i: (1000.0, 1000.0, 500.0, 800.0, 0 if i < 3 else 1) for i in range(6)}
        layout = self.write("score.mpos", mpos(boxes, [(i, i * 2000) for i in range(6)]))
        return root, layout

    # -- structure -----------------------------------------------------------

    def test_movements_split_at_section_breaks(self) -> None:
        """Each movement's page and duration come from its opening bar.
        A trailing section break must not make an empty last movement."""
        root, layout = self.two_movements()
        mvts = music_import.movements(root, layout, 14.0)
        self.assertEqual([(m["numeral"], m["measure"], m["page"], m["seconds"]) for m in mvts],
                         [("I.", 0, 1, 6.0), ("II.", 3, 2, 8.0)],
                         "the last movement runs to the end of the score")
        self.assertEqual(mvts[0]["tempo"], "Allegro")
        self.assertEqual(mvts[1]["tempo"], "♩ = 60", "SMuFL <sym> becomes Unicode")

    def test_a_section_break_on_a_frame_starts_a_movement(self) -> None:
        root = mscx(measure("I."), measure(), frame(), measure("II."), measure())
        layout = self.write("score.mpos", mpos(
            {i: (0.0, 0.0, 1.0, 1.0, i // 2) for i in range(4)},
            [(i, i * 1000) for i in range(4)]))
        self.assertEqual([m["measure"] for m in music_import.movements(root, layout, 4.0)], [0, 2])

    def test_tempo_text_in_private_use_characters_becomes_unicode(self) -> None:
        """MuseScore writes note values either as <sym> or as SMuFL
        private-use characters; the frontmatter gets readable text."""
        quarter, dot = chr(0xECA5), chr(0xECB7)   # metNoteQuarterUp, metAugmentationDot
        el = ET.fromstring(f"<text>{quarter}{dot} = 72</text>")
        self.assertEqual(music_import.text_of(el), "♩. = 72")

    # -- the bar map ----------------------------------------------------------

    def test_scale_is_measured_from_the_pages(self) -> None:
        """The .mpos unit is undocumented and differs by engraver, so the
        scale is measured: the last bar box on a page ends at its rightmost
        barline. Whatever the page resolution or .mpos scale, a bar's right
        edge must land on that barline as a fraction of the page."""
        for width, height, barline, k in ((10200, 13200, 9000, 12),   # MuseScore 4.7
                                          (3060, 3960, 2700, 12),     # MuseScore 3.6
                                          (3060, 3960, 2700, 40)):    # anything else
            with self.subTest(viewBox=(width, height), scale=k):
                svg = self.write(f"p{width}-{k}/page-1.svg", page_svg(width, height, [barline / 3, barline]))
                right = barline * k
                layout = self.write(f"p{width}-{k}/score.mpos", mpos(
                    {0: (right * 0.1, height * k * 0.2, right * 0.4, height * k * 0.3, 0),
                     1: (right * 0.5, height * k * 0.2, right * 0.5, height * k * 0.3, 0)},
                    [(0, 0), (1, 1000)]))
                t, _, err = quiet(music_import.timing, layout, layout, [svg],
                                  [{"measure": 0}], 2.0)
                self.assertEqual(err, "", "no bar should fall outside its page")
                page, x, y, w, h = t["measures"][1]
                self.assertEqual(page, 1)
                self.assertAlmostEqual(x + w, barline / width, places=3)
                self.assertAlmostEqual(y, 0.2, places=3)
                self.assertAlmostEqual(h, 0.3, places=3)

    def test_scale_comes_from_the_first_page_with_both_bars_and_barlines(self) -> None:
        blank = self.write("page-1.svg", page_svg(10200, 13200, []))
        music = self.write("page-2.svg", page_svg(10200, 13200, [9000]))
        text = mpos({0: (12000, 0, 96000, 1000, 1)}, [(0, 0)])
        k, w, h = music_import.mpos_scale(text, [blank, music])
        self.assertEqual((k, w, h), (12.0, 10200.0, 13200.0))
        with self.assertRaises(SystemExit):
            music_import.mpos_scale(text, [blank])

    def test_barlines_are_found_whatever_the_attribute_order(self) -> None:
        """A minifier may reorder attributes (svgo puts class last); the scale
        and the barline check must still see every barline (audit M16)."""
        svg = ('<svg viewBox="0 0 100 100">'
               '<polyline class="BarLine" fill="none" points="10.5,1 10.5,9"/>'
               '<polyline points="20,1 20,9" stroke="#000" class="BarLine"/>'
               '<polyline class="StaffLines" points="0,1 99,1"/>'
               '<line x1="30" y1="1" x2="30" y2="9" class="BarLine Final"/>'
               '<path class="BarLine" d="M40,1 L40,9"/></svg>')
        self.assertEqual(music_import.barline_xs(svg), [10.5, 10.5, 20.0, 20.0, 30.0, 30.0])

    def test_a_bar_beyond_its_page_is_reported(self) -> None:
        """What flags a score whose system overruns the page, or a scale
        that is wrong for the engraver."""
        svg = self.write("page-1.svg", page_svg(1000, 1000, [900]))
        layout = self.write("score.mpos", mpos(
            {0: (0, 0, 10800, 6000, 0), 1: (0, 6000, 10800, 6600, 0)}, [(0, 0), (1, 1)]))
        _, _, err = quiet(music_import.timing, layout, layout, [svg], [{"measure": 0}], 1.0)
        self.assertIn("1 bar boxes fall outside their page", err)

    def test_timing_lists_bars_in_the_order_they_sound(self) -> None:
        """Repeats are written out: a repeated bar sounds twice. The events
        come from the playback export, whatever order its file lists them."""
        svg = self.write("page-1.svg", page_svg(1000, 1000, [900]))
        boxes = {i: (i * 3600, 0, 3600, 1000, 0) for i in range(3)}
        layout = self.write("layout.mpos", mpos(boxes, [(i, 0) for i in range(3)]))
        playback = self.write("playback.mpos", mpos(boxes, [
            (2, 8000), (0, 0), (1, 2000), (0, 4000), (1, 6000)]))
        t = music_import.timing(layout, playback, [svg], [{"measure": 0}], 10.0)
        self.assertEqual(t["events"], [[0, 0], [2000, 1], [4000, 0], [6000, 1], [8000, 2]])
        self.assertEqual(t["movements"], [], "a single movement has no movement starts")
        self.assertEqual((t["version"], t["audio"], t["duration"]), (1, "realization.mp3", 10.0))

    def test_timing_refuses_a_playback_the_layout_does_not_have(self) -> None:
        svg = self.write("page-1.svg", page_svg(1000, 1000, [900]))
        layout = self.write("layout.mpos", mpos({0: (0, 0, 10800, 100, 0)}, [(0, 0)]))
        playback = self.write("playback.mpos", mpos({}, [(0, 0), (9, 1000)]))
        with self.assertRaises(SystemExit) as cm:
            music_import.timing(layout, playback, [svg], [{"measure": 0}], 2.0)
        self.assertIn("do not match", str(cm.exception.code))

    # -- partial works --------------------------------------------------------

    def test_selection_publishes_a_leading_run(self) -> None:
        for spec, sections, want in (("1-2", 3, 2), ("1,2", 3, 2), ("1", 4, 1),
                                     ("1-3", 3, None), ("all", 3, None), (None, 3, None)):
            with self.subTest(spec=spec, sections=sections):
                self.assertEqual(music_import.selection(spec, sections), want)

    def test_selection_refuses_gaps_and_overreach(self) -> None:
        """A gap would leave the pages, the realization and the timing with
        a hole in the middle; there is no way to publish one."""
        for spec, sections in (("2-3", 3), ("1,3", 3), ("1-4", 3), ("1-2", 1), ("one", 3)):
            with self.subTest(spec=spec, sections=sections):
                with self.assertRaises(SystemExit):
                    quiet(music_import.selection, spec, sections)

    def test_cut_point_stops_at_the_withheld_movements_page_and_downbeat(self) -> None:
        root, layout = self.two_movements()
        mvts = music_import.movements(root, layout, 12.0)
        playback = self.write("playback.mpos", mpos({}, [(i, i * 2500) for i in range(6)]))
        self.assertEqual(music_import.cut_point(layout, mvts, 1, playback),
                         {"page": 2, "measure": 3, "ms": 7500},
                         "the moment comes from the playback the audio was made from")

    def test_cut_point_refuses_a_movement_that_begins_mid_page(self) -> None:
        """Else the last published page would show the withheld movement's opening."""
        root, _ = self.two_movements()
        boxes = {i: (0.0, 0.0, 1.0, 1.0, 0 if i < 2 else 1) for i in range(6)}
        layout = self.write("score.mpos", mpos(boxes, [(i, i * 2000) for i in range(6)]))
        mvts = music_import.movements(root, layout, 12.0)
        with self.assertRaises(SystemExit) as cm:
            music_import.cut_point(layout, mvts, 1)
        self.assertIn("partway down page 2", str(cm.exception.code))

    def test_minutes_round_to_the_half_minute(self) -> None:
        for seconds, want in ((42, '42"'), (270, "4½'"), (1140, "19'"), (1170, "19½'")):
            with self.subTest(seconds=seconds):
                self.assertEqual(music_import.minutes(seconds), want)

    def test_scaffold_lists_withheld_movements_and_times_only_what_is_published(self) -> None:
        meta = {
            "textFramesData": {"titles": ["Violin Concerto"],
                               "composers": ["L. Neuwirth (2022-23)"], "subtitles": []},
            "parts": [{"name": "Solo Violin", "instrumentId": "violin"}]
                     + [{"name": f"Part {i}", "instrumentId": "x"} for i in range(13)],
            "duration": 1620,
        }
        mvts = [{"numeral": "I.", "tempo": "♩ = 80", "measure": 0, "page": 1, "seconds": 420},
                {"numeral": "II.", "tempo": "", "measure": 179, "page": 22, "seconds": 420},
                {"numeral": "", "tempo": "", "measure": 482, "page": 62, "seconds": 480}]
        (self.tmp / "vc").mkdir()
        with mock.patch.object(music_import, "MUSIC", self.tmp):
            quiet(music_import.scaffold, "vc", meta, mvts, False, "2022-01-01", 2)
        fm = front_matter(self.tmp / "vc" / "index.md")
        self.assertEqual((fm["year"], fm["composed"]), (2023, "2022 – 2023"))
        self.assertEqual((fm["category"], fm["instrumentation"]), ("orchestral", "violin and orchestra"))
        self.assertEqual(fm["duration"], "ca. 14'", "a partial work's duration is what is published")
        self.assertEqual(fm["movements"][2], {"name": "III.", "status": "in revision"})
        self.assertEqual([m["page"] for m in fm["movements"][:2]], [1, 22])

    # -- comparing a re-export against index.md -------------------------------

    def compare(self, declared: list[dict], mvts: list[dict], published: int | None = None) -> str:
        piece = self.tmp / "piece"
        piece.mkdir(exist_ok=True)
        (piece / "index.md").write_text(
            "---\n" + yaml.safe_dump({"title": "Piece", "movements": declared}) + "---\n")
        with mock.patch.object(music_import, "MUSIC", self.tmp):
            _, _, err = quiet(music_import.compare_movements, "piece", mvts, published)
        return err

    MVTS = [{"page": 1}, {"page": 40}, {"page": 90}]

    def test_compare_names_a_movement_whose_page_moved(self) -> None:
        """A reflow moves every movement's page; a stale `page:` is a silent
        wrong jump in the reader unless the re-export names it."""
        err = self.compare([{"name": "I.", "page": 1}, {"name": "II.", "page": 38},
                            {"name": "III.", "page": 90}], self.MVTS)
        self.assertIn("'II.' is at page 40", err)
        self.assertNotIn("'III.'", err)

    def test_compare_counts_movements_before_anything_else(self) -> None:
        err = self.compare([{"name": "I.", "page": 1}, {"name": "II.", "page": 40}],
                           [{"page": 1}])
        self.assertIn("lists 2 movement(s), this export has a single section", err)

    def test_compare_asks_for_a_status_on_a_withheld_movement(self) -> None:
        declared = [{"name": "I.", "page": 1}, {"name": "II.", "page": 40}, {"name": "III."}]
        self.assertIn("'III.' is withheld", self.compare(declared, self.MVTS, published=2))
        declared[2]["status"] = "in revision"
        self.assertEqual(self.compare(declared, self.MVTS, published=2), "")

    # -- check: what the deploy preflight refuses -----------------------------

    EXPORTED = "2026-09-29T22:00:00"

    def piece(self, slug: str, *, pages: int = 3, manifest_pages: int | None = 3,
              audio: bool = False, realization: bool = True, source_age: int = -3600,
              score_dir: bool = True) -> None:
        d = self.tmp / "music" / slug
        (d / "scores").mkdir(parents=True)
        fm = {"title": slug, **({"score-dir": "scores/"} if score_dir else {})}
        (d / "index.md").write_text("---\n" + yaml.safe_dump(fm) + "---\n")
        for i in range(1, pages + 1):
            (d / "scores" / f"page-{i}.svg").write_text(
                f'<svg viewBox="0 0 10 10">\n{music_import.SHRUNK}\n</svg>\n')
        if pages:
            (d / "scores" / "thumb.webp").write_bytes(b"RIFF")
        if audio and realization:
            for f in ("realization.mp3", "timing.json"):
                (d / "scores" / f).write_text("x")
        if manifest_pages is not None:
            (d / "score-source.yaml").write_text(yaml.safe_dump({
                "source": f"{slug}.mscz", "engraver": "MuseScore 4.7.5",
                "pages": manifest_pages, "pdf": False,
                "audio": "Muse Sounds" if audio else False, "exported": self.EXPORTED}))
            src = self.tmp / "Scores" / f"{slug}.mscz"
            src.parent.mkdir(exist_ok=True)
            src.write_text("x")
            t = dt.datetime.fromisoformat(self.EXPORTED).timestamp() + source_age
            os.utime(src, (t, t))

    def check(self) -> tuple[int, str]:
        with mock.patch.object(music_import, "MUSIC", self.tmp / "music"), \
             mock.patch.object(music_import, "SCORES_DIR", self.tmp / "Scores"):
            try:
                _, out, _ = quiet(music_import.cmd_check, None)
                return 0, out
            except SystemExit as e:
                return int(e.code or 0), ""

    def test_check_refuses_missing_or_partial_score_pages(self) -> None:
        """Deploying a checkout without its pages would delete the published
        scores (rsync --delete), so each of these must stop the deploy."""
        cases = {
            "no-manifest": dict(manifest_pages=None),
            "short": dict(pages=2, manifest_pages=3),
            "no-realization": dict(audio=True, realization=False),
        }
        for slug, kwargs in cases.items():
            with self.subTest(case=slug):
                self.setUp()
                self.piece(slug, **kwargs)
                self.assertEqual(self.check()[0], 1)

    def test_check_passes_a_complete_piece_and_only_reports_a_changed_source(self) -> None:
        self.piece("complete", audio=True)
        self.piece("unscored", score_dir=False, pages=0, manifest_pages=None)
        self.assertEqual(self.check(), (0, ""))
        self.setUp()
        self.piece("revised", source_age=+3600)
        code, out = self.check()
        self.assertEqual(code, 0, "a newer source is news, not a broken checkout")
        self.assertIn("revised: revised.mscz changed since", out)

    def test_check_refuses_a_page_cut_short_and_notes_an_unshrunk_one(self) -> None:
        self.piece("cut")
        page = self.tmp / "music" / "cut" / "scores" / "page-2.svg"
        page.write_text(page.read_text()[:-8])
        code, _ = self.check()
        self.assertEqual(code, 1)
        self.setUp()
        self.piece("old")
        (self.tmp / "music" / "old" / "scores" / "page-1.svg").write_text('<svg viewBox="0 0 1 1"></svg>')
        code, out = self.check()
        self.assertEqual(code, 0, "an unshrunk page is larger, not wrong")
        self.assertIn("old: 1 page(s) not shrunk", out)

    # -- shrinking the pages (audit MO1) --------------------------------------


    def test_shrink_rounds_coordinates_and_keeps_what_the_reader_reads(self) -> None:
        out = music_import.shrink_page(SAMPLE_PAGE, "piece-3-")
        self.assertNotIn("<?xml", out)
        self.assertNotIn("s5_condensed", out, "the source file's name is not published")
        self.assertIn(music_import.SHRUNK, out)
        self.assertIn('width="215.9mm" height="279.4mm" viewBox="0 0 10200 13200"', out)
        # every coordinate whole; a glyph's scale kept, its translation rounded
        for coords in re.findall(r'\b(?:d|points)="([^"]*)"', out):
            self.assertNotRegex(coords, r"\d\.\d", coords)
        self.assertIn('transform="matrix(0.464457,0,0,0.464457,3300,1090)"', out)
        self.assertIn('transform="matrix(0.464457,0,0,0.464457,0,1090)"', out, "-0 is written 0")
        # the outline drawn three times is defined once and used three times
        self.assertEqual(out.count('<use class="Note" href="#piece-3-0"'), 3)
        self.assertEqual(out.count('<path id="piece-3-0" d="M0,14 C0,28 10,44 37,44 L108,-13"/>'), 1)
        self.assertIn('<path class="Clef" transform=', out, "an outline drawn once stays put")
        # what the stylesheet recolours by, and the gutter and scale read
        self.assertIn('<path class="" fill="#ffffff"', out)
        self.assertIn('<path class="SlurSegment" stroke="#000000"', out)
        self.assertEqual(out.count("<polyline"), 2)
        self.assertEqual(music_import.barline_xs(out), [4379.0, 4379.0])

    @unittest.skipUnless(HAVE_THUMB_TOOLS, "rsvg-convert or Pillow's WebP support missing")
    def test_the_thumbnail_is_page_one_small_and_grey(self) -> None:
        from PIL import Image
        page = self.write("page-1.svg", music_import.shrink_page(SAMPLE_PAGE, "p-1-"))
        out = self.tmp / "thumb.webp"
        self.assertTrue(music_import.thumbnail(page, out))
        with Image.open(out) as im:
            self.assertEqual(im.format, "WEBP")
            self.assertEqual(im.size, (720, round(720 * 13200 / 10200)))
            # grey, give or take the encoder's chroma rounding
            spread = max(max(px) - min(px) for px in im.convert("RGB").getdata())
            self.assertLessEqual(spread, 8)
        self.assertEqual(sorted(p.name for p in self.tmp.iterdir()), ["page-1.svg", "thumb.webp"],
                         "no intermediate PNG left behind")

    def test_shrinking_a_shrunk_page_changes_nothing(self) -> None:
        once = music_import.shrink_page(SAMPLE_PAGE, "piece-3-")
        self.assertEqual(music_import.shrink_page(once, "piece-3-"), once)

    # -- installing an import (audit M09) -------------------------------------

    def fake_export(self, pages: int):
        page = SAMPLE_PAGE

        def export(source, outdir, pdf_name, audio, engraver="mscore"):
            files = []
            for i in range(1, pages + 1):
                f = outdir / f"page-{i}.svg"
                f.write_text(page.replace("(3)", f"({i})"))
                files.append(f)
            m = outdir / "score.mpos"
            m.write_text(mpos({0: (100.0, 100.0, 500.0, 800.0, 0)}, [(0, 0)]))
            return {"pages": files, "mpos": m, "pdf": None, "audio": None, "playback": m}
        return export

    def import_patches(self, pages: int) -> contextlib.ExitStack:
        stack = contextlib.ExitStack()
        for name, value in (("MUSIC", self.tmp / "music"), ("STAGING", self.tmp / ".music-import"),
                            ("SCORES_DIR", self.tmp / "Scores"),
                            ("export", self.fake_export(pages)),
                            ("score_meta", lambda source: {"duration": 4}),
                            ("read_mscx", lambda source: mscx(measure(), measure())),
                            ("mscore_version", lambda engraver="mscore": "MuseScore 4.7.5"),
                            ("stale_properties", lambda root, meta: [])):
            stack.enter_context(mock.patch.object(music_import, name, value))
        return stack

    def test_an_import_replaces_the_score_directory_whole(self) -> None:
        self.piece("p", pages=3, audio=True)
        scores = self.tmp / "music" / "p" / "scores"
        (scores / "p.pdf").write_text("an earlier PDF")
        with self.import_patches(pages=2):
            quiet(music_import.install, "p", self.tmp / "Scores" / "p.mscz",
                  False, False, True, False)
        thumbs = ["thumb.webp"] if HAVE_THUMB_TOOLS else []
        self.assertEqual(sorted(f.name for f in scores.iterdir()),
                         ["p.pdf", "page-1.svg", "page-2.svg"] + thumbs,
                         "page-3, the old realization, timing and thumbnail go; the PDF stays")
        self.assertIn(music_import.SHRUNK, (scores / "page-2.svg").read_text())
        self.assertIn('id="p-2-0"', (scores / "page-2.svg").read_text())
        self.assertFalse((self.tmp / ".music-import" / "p").exists())
        self.assertEqual(yaml.safe_load((self.tmp / "music" / "p" / "score-source.yaml").read_text())["pages"], 2)

    def test_a_first_import_creates_the_composition_directory(self) -> None:
        source = self.write("Scores/new-piece.mscz", "x")
        with self.import_patches(pages=2):
            quiet(music_import.install, "new-piece", source, False, False, False, True)
        piece = self.tmp / "music" / "new-piece"
        self.assertTrue((piece / "index.md").is_file())
        self.assertEqual(len(list((piece / "scores").glob("page-*.svg"))), 2)
        self.assertEqual(yaml.safe_load((piece / "score-source.yaml").read_text())["pages"], 2)

    def test_refresh_does_not_recreate_a_removed_index(self) -> None:
        self.piece("p")
        index = self.tmp / "music" / "p" / "index.md"
        index.unlink()
        with self.import_patches(pages=3):
            quiet(music_import.install, "p", self.tmp / "Scores" / "p.mscz",
                  False, False, False, False)
        self.assertFalse(index.exists(), "only an explicit import may scaffold a page")

    def test_a_failed_manifest_keeps_recovery_files_and_blocks_the_check(self) -> None:
        self.piece("p")
        manifest = self.tmp / "music" / "p" / "score-source.yaml"
        original = manifest.read_bytes()
        with self.import_patches(pages=3):
            with mock.patch.object(music_import, "write_manifest", side_effect=OSError("disk full")):
                with self.assertRaises(OSError):
                    quiet(music_import.install, "p", self.tmp / "Scores" / "p.mscz",
                          False, False, False, False)
            with self.assertRaises(SystemExit):
                quiet(music_import.cmd_check, None)
        stage = self.tmp / ".music-import" / "p"
        self.assertEqual(len(list((stage / "scores.old").glob("page-*.svg"))), 3)
        self.assertEqual((stage / "score-source.yaml").read_bytes(), original)
        self.assertEqual(manifest.read_bytes(), original)

    def test_retry_preserves_an_interrupted_imports_only_copy_of_the_old_files(self) -> None:
        self.piece("p")
        scores = self.tmp / "music" / "p" / "scores"
        (scores / "p.pdf").write_bytes(b"original PDF")
        stage = self.tmp / ".music-import" / "p"
        stage.mkdir(parents=True)
        scores.rename(stage / "scores.old")
        with self.import_patches(pages=3), \
             mock.patch.object(music_import, "export", wraps=self.fake_export(3)) as export:
            with self.assertRaises(SystemExit):
                quiet(music_import.install, "p", self.tmp / "Scores" / "p.mscz",
                      False, False, False, False)
            export.assert_not_called()
        self.assertEqual((stage / "scores.old" / "p.pdf").read_bytes(), b"original PDF")

    def test_a_failed_manifest_replace_preserves_the_previous_manifest(self) -> None:
        self.piece("p")
        manifest = self.tmp / "music" / "p" / "score-source.yaml"
        original = manifest.read_bytes()
        with mock.patch.object(music_import, "MUSIC", self.tmp / "music"), \
             mock.patch.object(music_import.os, "replace", side_effect=OSError("disk full")):
            with self.assertRaises(OSError):
                music_import.write_manifest("p", {"pages": 2})
        self.assertEqual(manifest.read_bytes(), original)

    def test_an_interrupted_swap_leaves_whole_pages_and_check_names_it(self) -> None:
        self.piece("p", pages=3)
        scores = self.tmp / "music" / "p" / "scores"
        real = os.rename

        def crash(src, dst):
            if Path(dst) == scores:
                raise OSError("power cut")
            return real(src, dst)
        with self.import_patches(pages=2), mock.patch.object(music_import.os, "rename", crash):
            with self.assertRaises(OSError):
                quiet(music_import.install, "p", self.tmp / "Scores" / "p.mscz",
                      False, False, True, False)
        old = self.tmp / ".music-import" / "p" / "scores.old"
        self.assertEqual(sorted(f.name for f in old.iterdir()),
                         ["page-1.svg", "page-2.svg", "page-3.svg", "thumb.webp"])
        out = io.StringIO()
        with mock.patch.object(music_import, "MUSIC", self.tmp / "music"), \
             mock.patch.object(music_import, "STAGING", self.tmp / ".music-import"), \
             contextlib.redirect_stdout(out), self.assertRaises(SystemExit):
            music_import.cmd_check(None)
        self.assertIn("p: an interrupted import left", out.getvalue())

    def test_source_paths_are_stored_relative_to_the_scores_directory(self) -> None:
        """So the committed manifest never publishes a home directory."""
        scores = self.tmp / "Scores"
        (scores / "sub").mkdir(parents=True)
        src = scores / "sub" / "piece.mscz"
        src.write_text("x")
        with mock.patch.object(music_import, "SCORES_DIR", scores):
            key = music_import.source_key(src)
            self.assertEqual(key, "sub/piece.mscz")
            self.assertEqual(music_import.source_path(key), src)


# ---------------------------------------------------------------------------
# What the importer left: committed manifests, exported pages and timing
# ---------------------------------------------------------------------------

def published_pieces() -> list[Path]:
    """Pieces with a manifest whose pages are on disk (they are not versioned)."""
    return [m.parent for m in sorted(MUSIC_DIR.glob("*/score-source.yaml"))
            if any((m.parent / "scores").glob("page-*.svg"))]


def scored_pieces() -> list[Path]:
    """Pieces that also have a realization to follow."""
    return [d for d in published_pieces() if (d / "scores" / "timing.json").is_file()]


@unittest.skipUnless(HAVE_YAML and published_pieces(),
                     "no score pages on disk — run `tools/music-import.py refresh`")
@unittest.skipUnless(HAVE_YAML, "PyYAML not installed")
class FrontMatterTests(unittest.TestCase):
    def setUp(self):
        import tempfile
        self.root = Path(tempfile.mkdtemp(prefix="music-fm-"))
        self.addCleanup(__import__("shutil").rmtree, self.root)
        patcher = mock.patch.object(music_import, "MUSIC", self.root)
        patcher.start()
        self.addCleanup(patcher.stop)

    def write(self, text):
        (self.root / "w").mkdir(exist_ok=True)
        (self.root / "w" / "index.md").write_text(text, encoding="utf-8")

    def test_an_em_dash_in_a_value_is_not_a_delimiter(self):
        self.write("---\ntitle: A --- B\nopus: 3\n---\nBody --- more.\n")
        self.assertEqual(music_import.frontmatter("w"), {"title": "A --- B", "opus": 3})

    def test_invalid_yaml_is_a_clear_error(self):
        self.write("---\ntitle: [unclosed\n---\n")
        with self.assertRaises(SystemExit) as cm:
            music_import.frontmatter("w")
        self.assertIn("not valid YAML", str(cm.exception))


class ScoreDataTests(unittest.TestCase):
    """The files score-reader.js and score-follow.js consume, checked
    against each other and against the manifest and index.md."""

    @classmethod
    def setUpClass(cls) -> None:
        cls.timing = {d.name: json.loads((d / "scores" / "timing.json").read_text())
                      for d in scored_pieces()}

    def published_movements(self, d: Path) -> tuple[list[dict], list[dict]]:
        """index.md's movements: (published, withheld)."""
        mvts = front_matter(d / "index.md").get("movements") or []
        return ([m for m in mvts if not m.get("status")], [m for m in mvts if m.get("status")])

    def test_every_svg_in_the_score_directory_is_a_numbered_page(self) -> None:
        """The build takes every *.svg in score-dir as a page, in natural
        order, so a stray SVG would become a page and a gap a missing one."""
        for d in published_pieces():
            with self.subTest(piece=d.name):
                manifest = yaml.safe_load((d / "score-source.yaml").read_text())
                numbers = sorted(page_number(p) for p in (d / "scores").glob("*.svg"))
                self.assertEqual(numbers, list(range(1, manifest["pages"] + 1)))

    def test_manifests_do_not_publish_a_home_directory(self) -> None:
        for d in published_pieces():
            with self.subTest(piece=d.name):
                source = str(yaml.safe_load((d / "score-source.yaml").read_text())["source"])
                self.assertFalse(source.startswith("/") or "/home/" in source, source)

    def test_a_recorded_realization_is_present(self) -> None:
        for d in published_pieces():
            with self.subTest(piece=d.name):
                manifest = yaml.safe_load((d / "score-source.yaml").read_text())
                timing = d / "scores" / "timing.json"
                self.assertEqual(bool(manifest.get("audio")), timing.is_file())
                if timing.is_file():
                    audio = d / "scores" / json.loads(timing.read_text())["audio"]
                    self.assertGreater(audio.stat().st_size, 0, audio.name)

    def test_every_event_names_a_bar_with_a_box_and_sounds_in_order(self) -> None:
        """eventAt() binary-searches the events by time, and tick() reads the
        bar's box: an unsorted list or a bar with no box breaks following."""
        for slug, t in self.timing.items():
            with self.subTest(piece=slug):
                self.assertEqual(t["version"], 1)
                times = [ms for ms, _ in t["events"]]
                self.assertEqual(times, sorted(times))
                self.assertLessEqual(times[-1], t["duration"] * 1000)
                bars = {bar for _, bar in t["events"]}
                self.assertTrue(all(0 <= b < len(t["measures"]) and t["measures"][b]
                                    for b in bars), "an event names a bar with no box")
                self.assertEqual(bars, set(range(len(t["measures"]))),
                                 "every bar should sound, or clicking it plays nothing")

    def boxes_off_their_page(self, slug: str) -> list[tuple[int, int]]:
        pages = len(list((MUSIC_DIR / slug / "scores").glob("page-*.svg")))
        off = []
        for bar, (page, x, y, w, h) in enumerate(self.timing[slug]["measures"]):
            if not (1 <= page <= pages and x >= 0 and y >= 0
                    and x + w <= 1.005 and y + h <= 1.005):
                off.append((bar, page))
        return off

    def test_bar_boxes_lie_on_their_pages(self) -> None:
        for slug in self.timing:
            with self.subTest(piece=slug):
                off = [(b, p) for b, p in self.boxes_off_their_page(slug)
                       if (slug, p) not in KNOWN_OVERFLOW]
                self.assertEqual(off, [], "(bar, page) drawn off the page")

    @unittest.expectedFailure
    def test_bassoon_concerto_page_68_fits_its_page(self) -> None:
        # Known defect: a system on page 68 of the Bassoon Concerto is
        # engraved below the foot of the page (staff lines to y=13479 on a
        # 13200 page; the contrabasses are cut off), so five bar boxes run
        # 2% past it. The fix belongs in the score. When it lands this test
        # passes unexpectedly: delete it and the KNOWN_OVERFLOW entry.
        if "bassoon-concerto" not in self.timing:
            self.skipTest("Bassoon Concerto not exported")
        self.assertEqual([p for _, p in self.boxes_off_their_page("bassoon-concerto")], [])

    def test_bar_boxes_end_at_the_barlines(self) -> None:
        """The scale was measured on one page; on every page the rightmost
        box must still end at the rightmost barline. A wrong scale (a 360-dpi
        assumption on a 1200-dpi page, say) misses by far more than this."""
        for slug, t in self.timing.items():
            right: dict[int, float] = {}
            for page, x, _, w, _ in t["measures"]:
                right[page] = max(right.get(page, 0.0), x + w)
            with self.subTest(piece=slug):
                for page, edge in right.items():
                    text = (MUSIC_DIR / slug / "scores" / f"page-{page}.svg").read_text()
                    vb = re.search(r'viewBox="[\d.\-]+ [\d.\-]+ ([\d.]+) ', text[:4096])
                    xs = music_import.barline_xs(text)
                    # A page with bars has barlines: finding none means the
                    # page format changed under the parser, and skipping it
                    # would make this test pass without checking anything.
                    self.assertTrue(vb and xs, f"page {page}: bar boxes but no barlines found")
                    self.assertAlmostEqual(edge, max(xs) / float(vb.group(1)), delta=0.03,
                                           msg=f"page {page}")

    def test_the_timing_ends_with_the_realization(self) -> None:
        """The slider's length is timing.duration and the clock counts to
        it; a realization that runs on past it (or stops short) leaves the
        last bars unreachable or the clock wrong. A trimmed partial work has
        its audio cut at the same moment."""
        if not shutil.which("ffprobe"):
            self.skipTest("ffprobe not installed")
        for slug, t in self.timing.items():
            with self.subTest(piece=slug):
                audio = MUSIC_DIR / slug / "scores" / t["audio"]
                out = subprocess.run(["ffprobe", "-v", "error", "-show_entries", "format=duration",
                                      "-of", "csv=p=0", str(audio)],
                                     capture_output=True, text=True, check=True).stdout
                self.assertAlmostEqual(float(out), t["duration"], delta=5.0)

    def test_movement_starts_agree_with_index_pages(self) -> None:
        """score-follow.js wires the reader's movement buttons only when the
        realization has exactly one movement start per button, and each
        button turns to its `page:` — so both have to match."""
        for slug, t in self.timing.items():
            published, _ = self.published_movements(MUSIC_DIR / slug)
            if len(published) < 2:
                continue
            with self.subTest(piece=slug):
                self.assertEqual([t["measures"][b][0] for b in t["movements"]],
                                 [m["page"] for m in published])
                self.assertEqual(t["movements"][0], 0)

    def test_a_partial_work_withholds_only_its_last_movements(self) -> None:
        """The manifest's `movements: 1-k` and index.md's statuses describe
        the same cut, and nothing published may follow a withheld movement."""
        for d in published_pieces():
            with self.subTest(piece=d.name):
                manifest = yaml.safe_load((d / "score-source.yaml").read_text())
                mvts = front_matter(d / "index.md").get("movements") or []
                held = [bool(m.get("status")) for m in mvts]
                self.assertEqual(held, sorted(held), "a published movement follows a withheld one")
                spec = manifest.get("movements")
                self.assertEqual(f"1-{held.count(False)}" if any(held) else None, spec)


# ---------------------------------------------------------------------------
# The built pages
# ---------------------------------------------------------------------------

def site_file(url: str, base: str = "/") -> Path:
    """The _site file a link resolves to: relative links from `base`, a
    query string dropped, a directory meaning its index.html."""
    path = urlsplit(urljoin(base, url)).path
    return SITE_DIR / (path.lstrip("/") + ("index.html" if path.endswith("/") else ""))


def sort_key(fm: dict) -> str:
    """Catalog.hs's ceSortKey: year, then a parseable `completed:`, then date."""
    completed = ""
    raw = fm.get("completed")
    for fmt in ("%d %B %Y", "%B %Y", "%Y-%m-%d"):
        try:
            completed = dt.datetime.strptime(str(raw), fmt).strftime("%Y-%m-%d")
            break
        except ValueError:
            continue
    return f"{fm.get('year', '0000')}|{completed}|{fm.get('date', '')}"


@unittest.skipUnless(HAVE_YAML and (SITE_DIR / "music" / "index.html").is_file(),
                     "no _site — run `make build`")
class BuiltMusicPageTests(unittest.TestCase):
    """The composition pages, the score reader and /music/, as built."""

    @classmethod
    def setUpClass(cls) -> None:
        cls.pieces = [d for d in published_pieces()
                      if (SITE_DIR / "music" / d.name / "index.html").is_file()]
        cls.fm = {d.name: front_matter(d / "index.md")
                  for d in sorted(MUSIC_DIR.glob("*/")) if (d / "index.md").is_file()}

    def html(self, rel: str) -> str:
        return (SITE_DIR / rel).read_text(encoding="utf-8")

    def test_reader_pages_are_in_reading_order_and_exist(self) -> None:
        """data-pages is the reader's whole page list; a lexicographic sort
        would interleave page-10 between page-1 and page-2."""
        for d in self.pieces:
            with self.subTest(piece=d.name):
                page = self.html(f"music/{d.name}/score/index.html")
                urls = re.search(r'data-pages="([^"]*)"', page).group(1).split(",")
                count = int(re.search(r'data-page-count="(\d+)"', page).group(1))
                manifest = yaml.safe_load((d / "score-source.yaml").read_text())
                self.assertEqual(count, manifest["pages"])
                self.assertEqual([page_number(Path(u)) for u in urls], list(range(1, count + 1)))
                missing = [u for u in urls if not site_file(u).is_file()]
                self.assertEqual(missing, [])

    def test_no_published_score_file_outlives_its_source(self) -> None:
        """Hakyll never deletes an output whose source is gone; a page left
        in _site after a re-export with fewer pages is still served, and
        still reachable from an old ?p= link (audit M02, M16)."""
        for d in sorted((SITE_DIR / "music").glob("*/scores")):
            with self.subTest(piece=d.parent.name):
                source = MUSIC_DIR / d.parent.name / "scores"
                orphans = sorted(f.name for f in d.iterdir()
                                 if f.suffix in (".svg", ".json", ".mp3", ".pdf", ".webp")
                                 and not (source / f.name).is_file())
                self.assertEqual(orphans, [])

    def test_frontispiece_and_shelf_show_the_thumbnail_when_there_is_one(self) -> None:
        """Page 1 as a decoded image rather than 2,000 outlines (audit MO4)."""
        shelf = self.html("music/index.html")
        for d in self.pieces:
            if not (d / "scores" / "page-1.svg").is_file():
                continue
            with self.subTest(piece=d.name):
                thumb = (d / "scores" / "thumb.webp").is_file()
                want = f"/music/{d.name}/scores/" + ("thumb.webp" if thumb else "page-1.svg")
                page = self.html(f"music/{d.name}/index.html")
                src = re.search(r'class="comp-frontispiece".*?<img src="([^"]+)"', page, re.S).group(1)
                self.assertEqual(urlsplit(urljoin(f"/music/{d.name}/", src)).path, want)
                self.assertTrue(site_file(src, f"/music/{d.name}/").is_file())
                self.assertIn(f'data-page="{want}"', shelf)

    def test_reader_movement_buttons_line_up_with_the_realization(self) -> None:
        for d in self.pieces:
            with self.subTest(piece=d.name):
                page = self.html(f"music/{d.name}/score/index.html")
                buttons = [(n.strip(), int(p)) for p, n in re.findall(
                    r'<button class="score-reader-mvt" data-page="(\d+)"[^>]*>([^<]*)</button>', page)]
                published = [m for m in self.fm[d.name].get("movements") or [] if not m.get("status")]
                self.assertEqual(buttons, [(m["name"], m["page"]) for m in published])
                timing = re.search(r'data-timing="([^"]+)"', page)
                self.assertEqual(bool(timing), (d / "scores" / "timing.json").is_file())
                if timing:
                    t = json.loads(site_file(timing.group(1)).read_text())
                    self.assertTrue(site_file(t["audio"], timing.group(1)).is_file(),
                                    "the realization is not beside timing.json")
                    if len(buttons) > 1:
                        self.assertEqual(len(t["movements"]), len(buttons),
                                         "score-follow.js leaves the buttons unwired")

    def test_composition_page_links_resolve(self) -> None:
        """Read the score, the frontispiece, and each published movement's
        row open the reader at that movement's page; a withheld movement is
        listed with its status and no link."""
        for d in self.pieces:
            with self.subTest(piece=d.name):
                base = f"/music/{d.name}/index.html"
                page = self.html(f"music/{d.name}/index.html")
                hrefs = re.findall(r'class="comp-(?:read|frontispiece)" href="([^"]+)"', page)
                hrefs += re.findall(r'<img src="([^"]+)"', page[page.find("comp-frontispiece"):][:600])
                self.assertGreaterEqual(len(hrefs), 3)
                self.assertEqual([h for h in hrefs if not site_file(h, base).is_file()], [])
                rows = re.findall(r'<li class="comp-mvt([^"]*)">(.*?)</li>', page, re.S)
                mvts = self.fm[d.name].get("movements") or []
                self.assertEqual(len(rows), len(mvts))
                for (cls, row), m in zip(rows, mvts):
                    if m.get("status"):
                        self.assertNotIn("<a ", row)
                        self.assertIn(f'comp-mvt-status">{m["status"]}<', row)
                    else:
                        link = re.search(r'<a class="comp-mvt-row" href="([^"]+)"', row).group(1)
                        self.assertTrue(site_file(link, base).is_file(), link)
                        self.assertEqual(urlsplit(link).query, f"p={m['page']}")

    def test_index_shelf_is_chronological_and_catalogue_newest_first(self) -> None:
        """Catalog.hs orders by year, then a parseable `completed:` date,
        then the page date; the shelf oldest first, each catalogue section
        newest first, the sections in a fixed order."""
        page = self.html("music/index.html")
        base = "/music/index.html"

        def slug(href: str) -> str:
            return site_file(href, base).parent.name

        spines = [slug(h) for h in re.findall(r'<a class="shelf-spine" href="([^"]+)"', page)]
        self.assertCountEqual(spines, self.fm.keys(), "every work stands on the shelf once")
        keys = [sort_key(self.fm[s]) for s in spines]
        self.assertEqual(keys, sorted(keys), "the shelf runs oldest to newest")

        sections = re.findall(r'<h2 class="cat-section-title">([^<]+)</h2>(.*?)</section>', page, re.S)
        order = [CATEGORY_ORDER.index(t.strip().lower()) for t, _ in sections]
        self.assertEqual(order, sorted(order))
        for title, body in sections:
            with self.subTest(section=title):
                keys = [sort_key(self.fm[slug(h)])
                        for h in re.findall(r'<a class="cat-work-row" href="([^"]+)"', body)]
                self.assertEqual(keys, sorted(keys, reverse=True))

    def test_index_says_what_a_partial_work_withholds(self) -> None:
        """Otherwise its duration reads as the whole work's."""
        page = self.html("music/index.html")
        for s, fm in self.fm.items():
            held = [m["name"].rstrip(".") for m in fm.get("movements") or [] if m.get("status")]
            if not held:
                continue
            with self.subTest(piece=s):
                row = re.search(rf'<a class="cat-work-row" href="[^"]*/{s}/[^"]*">(.*?)</a>',
                                page, re.S).group(1)
                self.assertIn(" and ".join(held) + " in revision", row)


if __name__ == "__main__":
    sys.exit(0 if unittest.main(exit=False).result.wasSuccessful() else 1)
