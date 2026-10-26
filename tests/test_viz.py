"""Regression tests for the static-figure visualization pipeline.

Covers the contract between three pieces that have to agree about colour:

  * ``content/**/figures/*.py``  — matplotlib scripts, which emit black
  * ``build/Filters/Viz.hs``     — ``processColors``, which rewrites it
  * ``static/css/viz.css``       — which themes what is left

That contract silently broke once already. ``processColors`` was written as a
literal mirror of ``Filters.Score``, whose SVG comes from Lilypond and uses
quoted attributes (``stroke="#000000"``). Matplotlib writes ``style=``
properties with a space instead (``stroke: #000000``), so every rule matched
zero times on every figure and the pass was inert — nothing failed loudly,
the CSS ``!important`` fallbacks just absorbed it, and those fallbacks in turn
repainted colours the scripts had chosen on purpose.

So the tests below check the two ends rather than the middle:

  * ``FigureOutputTests`` runs the real scripts and asserts every black
    declaration they emit is in a form ``processColors`` recognises. This is
    the guard that would have caught the original bug, and it fires again if
    a future matplotlib changes its serialization.
  * ``BuiltPageTests`` reads ``_site`` and asserts the whole pipeline landed:
    no black survives and captions are Markdown.
  * ``ColourPreservationTests`` closes the loop from the other side — every
    non-black colour a script emits has to arrive in the page unaltered.

Run with:  .venv/bin/python -m unittest discover -s tests
"""

from __future__ import annotations

import functools
import os
import re
import subprocess
import sys
import unittest
import xml.etree.ElementTree as ET
from pathlib import Path

from tests._helpers import SITE_ROOT, engine_path, script_env

SITE_DIR = SITE_ROOT / "_site"
VENV_PYTHON = SITE_ROOT / ".venv" / "bin" / "python3"

# Every colour declaration, in either syntax, with its value captured.
COLOUR_DECL = re.compile(
    r"""(fill|stroke)          # the property
        \s*(?::\s*|=")         # `: ` (style property) or `="` (attribute)
        (\#[0-9a-fA-F]{3,8}|[a-zA-Z]+)""",
    re.VERBOSE,
)

# The pure-black values processColors rewrites to currentColor. Keep this in
# step with `blackToken` in build/Filters/Viz.hs.
BLACK_VALUES = {"#000", "#000000", "black"}

# Any colour that is not one of these was chosen by a script on purpose, and
# must reach the page unaltered.
NEUTRAL_VALUES = BLACK_VALUES | {"none", "currentcolor"}


def figure_scripts() -> list[Path]:
    """Every script referenced by a `.figure` div anywhere in content/."""
    pattern = re.compile(r'\{\.figure[^}]*script="([^"]+)"')
    found: list[Path] = []
    for md in (SITE_ROOT / "content").rglob("*.md"):
        for rel in pattern.findall(md.read_text(encoding="utf-8")):
            script = (md.parent / rel).resolve()
            if script.is_file():
                found.append(script)
    return sorted(set(found))


def theme_dir() -> Path:
    """Where Filters.Viz finds tools/viz_theme.py: the site's, else the
    engine's (build/EngineFiles.hs)."""
    for root in (SITE_ROOT, SITE_ROOT / "engine"):
        if (root / "tools" / "viz_theme.py").is_file():
            return root / "tools"
    raise AssertionError("no tools/viz_theme.py in the site or its engine")


@functools.lru_cache(maxsize=None)
def render(script: Path) -> str:
    """Run a figure script the way Filters.Viz does: from the site, with
    the theme's directory first on PYTHONPATH."""
    env = script_env()
    env["PYTHONPATH"] = os.pathsep.join(filter(None, [str(theme_dir()), env.get("PYTHONPATH")]))
    result = subprocess.run(
        [str(VENV_PYTHON), str(script)],
        cwd=SITE_ROOT,
        env=env,
        capture_output=True,
        text=True,
        timeout=120,
    )
    if result.returncode != 0:
        raise AssertionError(f"{script.name} exited {result.returncode}: {result.stderr}")
    return result.stdout


@unittest.skipUnless(VENV_PYTHON.is_file(), "no .venv — run `make venv`")
class FigureOutputTests(unittest.TestCase):
    """The scripts must emit black in a syntax processColors can see."""

    @classmethod
    def setUpClass(cls) -> None:
        cls.scripts = figure_scripts()
        cls.svgs = {s: render(s) for s in cls.scripts}

    def test_corpus_is_not_empty(self) -> None:
        # A refactor that stops finding figures would make every other test
        # here vacuously pass.
        self.assertTrue(self.scripts, "no .figure scripts found under content/")

    def test_black_is_emitted_in_a_recognised_syntax(self) -> None:
        for script, svg in self.svgs.items():
            with self.subTest(script=script.name):
                # A black value reachable by processColors appears as either
                # `prop="value"` or `prop:<space?>value`. Anything else — a
                # `rgb(0,0,0)`, a presentation attribute the regex above
                # can't see — would slip past the filter into the page.
                for match in re.finditer(r"(?:fill|stroke)\s*[:=]\s*\"?([^;\"'>\s]+)", svg):
                    value = match.group(1).lower()
                    if value in {"none", "currentcolor"} or not self._is_black(value):
                        continue
                    self.assertRegex(
                        match.group(0),
                        r'(?:fill|stroke)(?:\s*:\s*|=")',
                        f"{script.name}: black in a syntax processColors cannot match",
                    )

    def test_label_text_has_no_control_characters(self) -> None:
        # Matplotlib records each text run's source string in an SVG comment,
        # which makes this checkable. A control character in one means a
        # Python escape ate part of a mathtext command: fig_decomp.py had
        # "$\times$" in a non-raw string, so \t became a TAB and the y-axis
        # rendered "(imes)". Every other script in the corpus doubled the
        # backslash, so nothing flagged it and it shipped.
        for script, svg in self.svgs.items():
            for comment in re.findall(r"<!--\s*(.*?)\s*-->", svg, re.S):
                bad = [c for c in comment if ord(c) < 32 and c != "\n"]
                with self.subTest(script=script.name, text=comment[:40]):
                    self.assertFalse(
                        bad,
                        f"control character {bad!r} in label text — a "
                        f"backslash escape was consumed by Python",
                    )

    def test_scripts_are_reproducible(self) -> None:
        # save_svg pins metadata Date=None so repeated builds produce
        # identical bytes; the embedding cache in tools/embed.py depends on
        # it. A script that reintroduces nondeterminism churns that cache.
        for script in self.scripts:
            with self.subTest(script=script.name):
                self.assertEqual(
                    self.svgs[script],
                    render.__wrapped__(script),  # bypass the cache: rerun it
                    f"{script.name} is not deterministic",
                )

    @staticmethod
    def _is_black(value: str) -> bool:
        return value in BLACK_VALUES or value in {"rgb(0,0,0)", "#000000ff"}


@unittest.skipUnless(SITE_DIR.is_dir(), "no _site — run `cabal run site -- build`")
class BuiltPageTests(unittest.TestCase):
    """End-to-end: what actually reaches a reader's browser."""

    @classmethod
    def setUpClass(cls) -> None:
        cls.pages = [
            (p, p.read_text(encoding="utf-8"))
            for p in SITE_DIR.rglob("*.html")
            if 'class="viz-figure"' in p.read_text(encoding="utf-8")
        ]

    def test_some_page_has_a_figure(self) -> None:
        self.assertTrue(self.pages, "no built page contains a .viz-figure")

    def test_no_black_survives_into_the_page(self) -> None:
        # The dark-mode contract. Every black the scripts emit must have been
        # rewritten to currentColor; anything left renders black-on-black.
        for path, html in self.pages:
            for figure in self._figures(html):
                for prop, value in COLOUR_DECL.findall(figure):
                    with self.subTest(page=path.name, decl=f"{prop}:{value}"):
                        self.assertNotIn(
                            value.lower(),
                            BLACK_VALUES,
                            "unconverted black — dark mode will render this invisible",
                        )

    def test_pages_with_figures_load_viz_css(self) -> None:
        # viz.css supplies the only theming for default-coloured text:
        # matplotlib writes no fill for it, so nothing else makes it follow
        # light/dark. The stylesheet used to sit inside head.html's
        # $if(viz)$ gate together with the Vega scripts, while WRITING.md
        # tells authors static .figure divs do not need viz: true — so the
        # one page with figures never loaded it, and every axis label, tick
        # label and legend entry rendered black on #121212 in dark mode.
        for path, html in self.pages:
            with self.subTest(page=path.name):
                self.assertIn(
                    "css/viz.css",
                    html,
                    "page has a .viz-figure but does not load viz.css — "
                    "figure text will not follow the theme",
                )

    def test_wide_figures_can_pan_instead_of_shrinking(self) -> None:
        # Matplotlib bakes label text at a fixed size in user units, so a
        # figure scaled down to a narrow column takes its text with it:
        # before the scroll wrapper, fig_decomp's 10-unit tick labels
        # rendered at ~4.4px on a 375px phone. Each figure carries its own
        # natural width so min-width can floor one user unit at one pixel.
        MIN_LEGIBLE_PX = 10.0
        PHONE_VIEWPORT = 375
        for path, html in self.pages:
            for block in self._figures(html):
                svg = re.search(r"<svg\b[^>]*>", block)
                if svg is None:
                    continue
                with self.subTest(page=path.name):
                    self.assertIn(
                        'class="viz-scroll"', block, "figure has no scroll wrapper"
                    )
                    natural = re.search(r"--viz-natural-width:(\d+)px", block)
                    self.assertIsNotNone(natural, "no natural width on the wrapper")

                    viewbox = re.search(r'viewBox="[\d.]+ [\d.]+ ([\d.]+)', svg.group(0))
                    self.assertIsNotNone(viewbox, "svg has no viewBox to size from")
                    units = float(viewbox.group(1))
                    rendered = max(PHONE_VIEWPORT, int(natural.group(1)))
                    label_px = 10.0 * rendered / units
                    self.assertGreaterEqual(
                        label_px,
                        MIN_LEGIBLE_PX,
                        f"a 10-unit label would render at {label_px:.1f}px on a "
                        f"{PHONE_VIEWPORT}px viewport",
                    )

    def test_scroll_wrapper_is_keyboard_reachable(self) -> None:
        # A scrollable region that only responds to touch or trackpad fails
        # WCAG 2.1.1.
        for path, html in self.pages:
            for block in self._figures(html):
                if 'class="viz-scroll"' not in block:
                    continue
                with self.subTest(page=path.name):
                    self.assertRegex(
                        block,
                        r'class="viz-scroll"[^>]*tabindex="0"',
                        "scroll wrapper is not focusable",
                    )

    def test_figures_are_announced_to_screen_readers(self) -> None:
        # Inline SVG is exposed as a graphic only if it says so. Without
        # role="img" plus a name, the whole chart is either skipped or read
        # out as a pile of <path> elements, and the caption is all a screen
        # reader user gets.
        for path, html in self.pages:
            for block in self._figures(html):
                svg = re.search(r"<svg\b[^>]*>", block)
                if svg is None:
                    continue
                with self.subTest(page=path.name):
                    self.assertIn('role="img"', svg.group(0), "svg has no role")
                    self.assertRegex(
                        svg.group(0), r'aria-label="[^"]{20,}"',
                        "svg has no usable accessible name",
                    )
                    self.assertRegex(block, r"<title>.{20,}?</title>", "no <title>")
                    self.assertRegex(block, r"<desc>.{40,}?</desc>", "no <desc>")

    def test_figure_svg_ids_are_unique_across_the_page(self) -> None:
        # Matplotlib restarts its counters per figure, so several inlined
        # SVGs collided on figure_1 / axes_1 / text_1 — 491 duplicate ids on
        # one page, 47 of them referenced by <use> and url(#…) pointers that
        # all resolved to whichever copy came first. Filters.Viz suffixes
        # each figure's ids with a token from its script name.
        for path, html in self.pages:
            ids: list[str] = []
            for block in self._figures(html):
                ids += re.findall(r'\bid="([^"]+)"', block)
            dupes = {i for i in ids if ids.count(i) > 1}
            with self.subTest(page=path.name):
                self.assertFalse(dupes, f"duplicate ids across figures: {sorted(dupes)[:5]}")

    def test_figure_references_all_resolve(self) -> None:
        # The counterpart: suffixing ids must not leave a <use> or url(#…)
        # pointing at an id that no longer exists.
        for path, html in self.pages:
            for block in self._figures(html):
                ids = set(re.findall(r'\bid="([^"]+)"', block))
                refs = set(re.findall(r'(?:xlink:href|href)="#([^"]+)"', block))
                refs |= set(re.findall(r"url\(#([^)]+)\)", block))
                with self.subTest(page=path.name):
                    self.assertFalse(
                        refs - ids, f"dangling references: {sorted(refs - ids)[:5]}"
                    )

    def test_captions_render_markdown(self) -> None:
        # Captions arrive as a flat attribute string and used to be HTML
        # escaped, so `code` spans and $math$ shipped as literal source.
        captions = [c for _, html in self.pages for c in self._captions(html)]
        self.assertTrue(captions, "no viz captions found")
        joined = " ".join(captions)
        self.assertNotRegex(joined, r"`[^`]+`", "backticks in a caption — Markdown not parsed")
        self.assertNotRegex(joined, r"(?<!\\)\$[^$]+\$", "raw $math$ in a caption")

    def test_caption_math_is_bare_tex_for_katex(self) -> None:
        # static/js/katex-bootstrap.js hands each .math element's textContent
        # straight to katex.render, which wants bare TeX. Pandoc's default
        # math method would wrap it in \(...\) delimiters instead.
        for path, html in self.pages:
            for caption in self._captions(html):
                for span in re.findall(r'<span class="math inline">(.*?)</span>', caption):
                    with self.subTest(page=path.name, math=span):
                        self.assertNotIn("\\(", span, "delimiters KaTeX would try to typeset")

    @staticmethod
    def _figures(html: str) -> list[str]:
        return re.findall(r'<figure class="viz-figure">.*?</figure>', html, re.S)

    @staticmethod
    def _captions(html: str) -> list[str]:
        return re.findall(r'<figcaption class="viz-caption">(.*?)</figcaption>', html, re.S)


@unittest.skipUnless(VENV_PYTHON.is_file(), "no .venv — run `make venv`")
@unittest.skipUnless(SITE_DIR.is_dir(), "no _site — run `cabal run site -- build`")
class ColourPreservationTests(unittest.TestCase):
    """The counterpart to "no black survives": everything else must.

    processColors and viz.css are only ever allowed to be about black. Both
    have overreached before — a blanket `!important` in viz.css repainted the
    white labels a heatmap script had chosen for its dark cells, leaving them
    invisible against those cells in light mode.

    Rather than name the colours to protect, this derives them: whatever a
    script emits that is not black, none, or currentColor was a deliberate
    choice and has to arrive intact. That keeps the test honest when the
    corpus changes — deleting the one figure that used white does not
    quietly reduce what is covered.
    """

    def test_deliberate_colours_survive_into_the_page(self) -> None:
        page_blob = "".join(
            p.read_text(encoding="utf-8")
            for p in SITE_DIR.rglob("*.html")
            if 'class="viz-figure"' in p.read_text(encoding="utf-8")
        )
        checked = 0
        for script in figure_scripts():
            for _prop, value in COLOUR_DECL.findall(render(script)):
                if value.lower() in NEUTRAL_VALUES:
                    continue
                checked += 1
                with self.subTest(script=script.name, colour=value):
                    self.assertIn(
                        value,
                        page_blob,
                        f"{value} from {script.name} was stripped — "
                        "a deliberate colour got overridden",
                    )
        self.assertGreater(checked, 0, "no non-black colours found to check")


@unittest.skipUnless(SITE_DIR.is_dir(), "no _site — run `cabal run site -- build`")
class FigureNumberingTests(unittest.TestCase):
    """Filters.FigureRefs — opt-in numbering and cross-references.

    The opt-in half matters as much as the numbering half: three essays
    number by hand in three different conventions, and this must not touch
    them.
    """

    @classmethod
    def setUpClass(cls) -> None:
        opted = {
            md.parent.name
            for md in (SITE_ROOT / "content").rglob("*.md")
            if re.search(r"^figure-numbering:\s*true\s*$", md.read_text(encoding="utf-8"), re.M)
        }
        cls.pages = {}
        for p in SITE_DIR.rglob("index.html"):
            if p.parent.name in opted:
                cls.pages[p.parent.name] = p.read_text(encoding="utf-8")
        cls.opted = opted

    def test_an_opted_in_page_exists(self) -> None:
        self.assertTrue(self.pages, "no built page opts in to figure numbering")

    def test_numbers_are_a_gapless_sequence_in_document_order(self) -> None:
        for name, html in self.pages.items():
            with self.subTest(page=name):
                nums = [
                    int(m) for m in re.findall(
                        r'class="figure-number">Figure (\d+)\.', html
                    )
                ]
                self.assertEqual(
                    nums,
                    list(range(1, len(nums) + 1)),
                    "figure numbers must run 1..N in the order they appear",
                )

    def test_every_numbered_figure_is_addressable(self) -> None:
        # A number with no anchor cannot be referenced, which defeats the
        # point of numbering it. Only labelled figures are in scope: a page
        # also carries uncaptioned <figure> elements (the frontmatter
        # monogram), which FigureRefs deliberately leaves alone.
        for name, html in self.pages.items():
            blocks = re.findall(r"<figure\b.*?</figure>", html, re.S)
            labelled = [b for b in blocks if 'class="figure-number"' in b]
            with self.subTest(page=name):
                self.assertTrue(labelled, "no labelled figures found")
                for block in labelled:
                    tag = re.match(r"<figure\b[^>]*>", block).group(0)
                    self.assertIn("id=", tag, f"numbered figure without an id: {tag}")

    def test_uncaptioned_figures_are_left_alone(self) -> None:
        # Numbering a decorative figure would put an invisible label in the
        # sequence and shift every real figure's number.
        for name, html in self.pages.items():
            for block in re.findall(r"<figure\b.*?</figure>", html, re.S):
                if "<figcaption" in block:
                    continue
                with self.subTest(page=name):
                    self.assertNotIn(
                        'class="figure-number"', block,
                        "an uncaptioned figure was numbered",
                    )

    def test_no_unresolved_cross_references(self) -> None:
        # FigureRefs renders a dangling anchor as "Figure ?" rather than
        # leaving an invisible empty link — so a typo is visible here.
        for name, html in self.pages.items():
            with self.subTest(page=name):
                self.assertNotIn(
                    "Figure ?", html, "a [](#anchor) reference did not resolve"
                )

    def test_pages_that_did_not_opt_in_are_untouched(self) -> None:
        for p in SITE_DIR.rglob("index.html"):
            if p.parent.name in self.opted or "/source/" in str(p):
                continue
            with self.subTest(page=p.parent.name):
                self.assertNotIn(
                    'class="figure-number"',
                    p.read_text(encoding="utf-8"),
                    "numbering leaked into a page that did not opt in",
                )


# ---------------------------------------------------------------------------
# Default fills
# ---------------------------------------------------------------------------
#
# Matplotlib and MuseScore write no fill at all for a black face: the text of
# a figure, a black bar or arrowhead, every MuseScore notehead. They lean on
# SVG's initial `fill: black`, so there is no declaration for processColors to
# rewrite, and on the dark and cappuccino pages those shapes draw black on
# near-black. Each surface that shows such SVG therefore sets an inherited
# `fill: currentColor` on its container, and these tests keep it that way.

SVG_NS = "{http://www.w3.org/2000/svg}"
XLINK_HREF = "{http://www.w3.org/1999/xlink}href"
CSS_DIR = engine_path("static/css")

# Surface: the selector whose rule must carry an inherited fill.
THEMED_SURFACES = {
    "viz-figure": ".viz-figure svg",
    "score-fragment-inner": ".score-fragment-inner svg",
    "score-page": ".score-page",
}

SHAPES = {"path", "rect", "circle", "ellipse", "polygon", "polyline", "use"}
UNRENDERED = {"defs", "clipPath", "mask", "marker", "pattern", "symbol"}


def css_fill_selectors() -> set[str]:
    """Selectors whose rule sets `fill: currentColor`, without !important."""
    found: set[str] = set()
    for css in CSS_DIR.glob("*.css"):
        text = re.sub(r"/\*.*?\*/", "", css.read_text(encoding="utf-8"), flags=re.S)
        for selectors, body in re.findall(r"([^{}]+)\{([^{}]*)\}", text):
            if re.search(r"(?<![\w-])fill\s*:\s*currentColor\s*(;|$)", body, re.I):
                found.update(" ".join(s.split()) for s in selectors.split(","))
    return found


def own_fill(node) -> str | None:
    m = re.search(r"(?<![\w-])fill\s*:\s*([^;]+)", node.get("style") or "")
    return m.group(1).strip() if m else node.get("fill")


def paints_area(node, ids) -> bool:
    """Whether a fill on this element could paint anything at all."""
    tag = node.tag.replace(SVG_NS, "")
    if tag == "use":
        ref = ids.get((node.get(XLINK_HREF) or node.get("href") or "").lstrip("#"))
        return ref is not None and paints_area(ref, ids)
    if tag == "path":
        # A tick mark or a single segment encloses nothing.
        return len(re.findall(r"-?[\d.]+(?:e-?\d+)?", node.get("d") or "")) >= 6
    return True


def unthemed_shapes(svg: str, container_fill: str | None) -> int:
    """Shapes that would paint in SVG's initial black.

    `container_fill` is what the page's CSS hands the <svg> root by
    inheritance: "currentColor" when the surface's rule is in place.
    """
    root = ET.fromstring(svg)
    parent = {c: p for p in root.iter() for c in p}
    ids = {n.get("id"): n for n in root.iter() if n.get("id")}
    count = 0
    for node in root.iter():
        if node.tag.replace(SVG_NS, "") not in SHAPES or not paints_area(node, ids):
            continue
        chain, x = [node], node
        while x in parent:
            x = parent[x]
            chain.append(x)
        if any(a.tag.replace(SVG_NS, "") in UNRENDERED for a in chain[1:]):
            continue
        if node.tag == f"{SVG_NS}use":
            ref = ids.get((node.get(XLINK_HREF) or node.get("href") or "").lstrip("#"))
            if ref is not None and own_fill(ref) is not None:
                continue
        fill = next((f for f in map(own_fill, chain) if f is not None), container_fill)
        if fill is None or fill.lower() in BLACK_VALUES:
            count += 1
    return count


class DefaultFillRuleTests(unittest.TestCase):
    """Each SVG surface inherits the theme's ink for undeclared fills."""

    def test_every_surface_sets_an_inherited_fill(self) -> None:
        selectors = css_fill_selectors()
        for surface, selector in THEMED_SURFACES.items():
            with self.subTest(surface=surface):
                self.assertIn(
                    selector, selectors,
                    f"no `{selector} {{ fill: currentColor }}` rule in "
                    f"static/css/ — shapes with no fill of their own draw in "
                    f"SVG's initial black, invisible on the dark themes",
                )


@unittest.skipUnless(SITE_DIR.is_dir(), "no _site — run `cabal run site -- build`")
class DefaultFillPageTests(unittest.TestCase):
    """What a browser paints: no shape on a built page falls back to black."""

    SURFACE_SVG = re.compile(
        r'class="(viz-figure|score-fragment-inner)".*?(<svg\b.*?</svg>)', re.S
    )

    @classmethod
    def setUpClass(cls) -> None:
        selectors = css_fill_selectors()
        cls.inherited = {
            surface: "currentColor" if selector in selectors else None
            for surface, selector in THEMED_SURFACES.items()
        }

    def assert_themed(self, svg: str, surface: str, where: str) -> None:
        try:
            bad = unthemed_shapes(svg, self.inherited[surface])
        except ET.ParseError as e:
            self.fail(f"{where}: SVG does not parse ({e})")
        self.assertEqual(
            bad, 0,
            f"{where}: {bad} shape(s) paint in SVG's initial black; the "
            f"{THEMED_SURFACES[surface]} rule must set fill: currentColor",
        )

    def test_figures_and_fragments(self) -> None:
        checked = 0
        for page in SITE_DIR.rglob("*.html"):
            html = page.read_text(encoding="utf-8")
            if "viz-figure" not in html and "score-fragment-inner" not in html:
                continue
            for i, (surface, svg) in enumerate(self.SURFACE_SVG.findall(html)):
                checked += 1
                where = f"{page.relative_to(SITE_DIR)} ({surface} {i + 1})"
                with self.subTest(where=where):
                    self.assert_themed(svg, surface, where)
        self.assertGreater(checked, 0, "no figure or score fragment found in _site")

    def test_score_reader_pages(self) -> None:
        # The reader inlines each page into .score-page; one page per work
        # shows what its engraver writes.
        for work in sorted({p.parent for p in SITE_DIR.glob("music/*/scores/page-1.svg")}):
            svg = (work / "page-1.svg").read_text(encoding="utf-8")
            svg = re.sub(r"^<\?xml[^>]*\?>\s*", "", svg)
            where = str((work / "page-1.svg").relative_to(SITE_DIR))
            with self.subTest(where=where):
                self.assert_themed(svg, "score-page", where)


if __name__ == "__main__":
    sys.exit(0 if unittest.main(exit=False).result.wasSuccessful() else 1)
