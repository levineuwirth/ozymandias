"""What each switch makes of the fixture site, both ways (plan §7.3): for
every switch the engine registers, a check that its feature is there in a
build with it on, and one that nothing of it is left in a build with it
off. test_fixture_site runs them on each of the fixture's builds, as the
build's switches say, and its coverage test fails unless every switch has
both checks and both have run.

Each check takes the copy the build ran in (its _site/ inside) and raises
AssertionError saying what is wrong.
"""

from __future__ import annotations

import re
from pathlib import Path

ARCHIVED = "example-cited-page"   # the fixture's archive entry (archive/manifest.yaml)
PIECE = "a-piece"                 # the fixture's composition (content/music/a-piece/)
CAPABILITIES = "essays/capabilities/index.html"   # the full profile's essay of switched features
EULER = r"$e^{i\pi} + 1 = 0$"     # its inline math, as written
CAPTION_MATH = "$y = x$"          # its static figure's caption's math, as written
# What music puts on a page: the score filter's fragment, the composition
# page's stylesheet, the catalog's shelf script, the reader's page list.
MUSIC_MARKS = ('data-exhibit-type="score"', "/css/composition.css", "/css/catalog.css", "/js/music-shelf.js",
               'id="score-reader-stage"')


def published(site: Path) -> list[Path]:
    """Every file the built site serves but /source/'s copies and the
    search index's fragments."""
    return sorted(p for p in (site / "_site").rglob("*") if p.is_file()
                  and p.relative_to(site / "_site").parts[0] not in ("source", "pagefind"))


def captions_of(page: str) -> list[str]:
    """A page's figure captions (viz's)."""
    return re.findall(r'<figcaption class="viz-caption">.*?</figcaption>', page, re.S)


def pages(site: Path) -> list[Path]:
    """The built site's pages, not /source/'s copies of the files that made
    them (whose templates say what every switch would put on a page)."""
    return sorted(p for p in (site / "_site").rglob("*.html")
                  if p.relative_to(site / "_site").parts[0] != "source")


def text(site: Path, rel: str) -> str:
    return (site / "_site" / rel).read_text(encoding="utf-8")


def archive_on(site: Path) -> None:
    assert ARCHIVED in text(site, "archive/index.html"), "the /archive/ index lists no entry"
    assert (site / "_site/archive" / ARCHIVED / "index.html").is_file(), "no page for the archived entry"
    assert "archive-affordance" in text(site, "essays/long/index.html"), \
        "the essay's link to the archived page has no archive mark"
    assert 'id="archive"' in text(site, "build/index.html"), "the build page has no link-archive section"


def archive_off(site: Path) -> None:
    assert not (site / "_site/archive").exists(), "_site/archive/ was built"
    marked = [p.name for p in pages(site) if "archive-affordance" in p.read_text(encoding="utf-8")]
    assert not marked, f"archive marks on {marked}"
    assert 'id="archive"' not in text(site, "build/index.html"), "the build page has a link-archive section"


def music_on(site: Path) -> None:
    assert f"/music/{PIECE}/" in text(site, "music/index.html"), "the /music/ catalog lists no work"
    work = text(site, f"music/{PIECE}/index.html")
    assert 'data-exhibit-type="score"' in work, "the work's score fragment was not inlined"
    assert 'data-exhibit-type="score"' in text(site, "essays/capabilities/index.html"), \
        "the essay's score fragment was not inlined"
    assert "/css/composition.css" in work, "the work's page has no composition stylesheet"
    reader = text(site, f"music/{PIECE}/score/index.html")
    assert f"/music/{PIECE}/scores/page-1.svg,/music/{PIECE}/scores/page-2.svg" in reader, \
        "the score reader does not list both pages"
    for rel in ("scores/page-1.svg", "scores/page-2.svg", "scores/timing.json", "scores/realization.mp3"):
        assert (site / "_site/music" / PIECE / rel).is_file(), f"{rel} was not published"
    for rel in ("new.html", "sitemap.xml", "data/backlinks.json"):
        assert f"/music/{PIECE}/" in text(site, rel), f"{rel} does not list the work"
    assert "Compositions" in text(site, "stats/index.html"), "/stats/ counts no compositions"


def music_off(site: Path) -> None:
    # /music/ may still be built: without the switch content/music/ is an
    # ordinary page collection. None of it is a work, a reader or a score.
    assert not list((site / "_site").glob("music/*/score")), "a score reader was built"
    assert not list((site / "_site").glob("music/*/scores")), "score files were published"
    marked = {mark: [p.name for p in pages(site) if mark in p.read_text(encoding="utf-8")]
              for mark in MUSIC_MARKS}
    assert not any(marked.values()), f"music on pages: {marked}"
    naming = [str(p.relative_to(site / "_site")) for p in published(site)
              if f"/music/{PIECE}/".encode() in p.read_bytes()]
    assert not naming, f"the work is named in {naming}"
    assert "Compositions" not in text(site, "stats/index.html"), "/stats/ counts compositions"


def math_on(site: Path) -> None:
    essay = text(site, CAPABILITIES)
    for mark in ('class="math inline"', 'class="math display"', "/katex/katex.min.css", "/katex/katex.min.js"):
        assert mark in essay, f"the essay's math has no {mark}"
    assert "<math" in essay, "the essay's abstract has no MathML"
    captions = "".join(captions_of(essay))
    if captions:   # the figures are viz's
        assert CAPTION_MATH not in captions and 'class="math inline"' in captions, \
            "the figure's caption has its math as text"


def math_off(site: Path) -> None:
    marked = {mark: [p.name for p in pages(site) if mark in p.read_text(encoding="utf-8")]
              for mark in ('class="math', "/katex/", "<math")}
    assert not any(marked.values()), f"math on pages: {marked}"
    if (site / "_site" / CAPABILITIES).is_file():
        essay = text(site, CAPABILITIES)
        assert EULER in essay, "the essay's TeX is not left as written"
        captions = "".join(captions_of(essay))
        assert not captions or CAPTION_MATH in captions, "the figure's caption has its TeX read"


# What code puts on a page: Prism's language classes and the highlighter,
# its stylesheet, the copy buttons' script.
CODE_MARKS = ('class="language-', "/js/prism.min.js", "/css/syntax.css", "/js/copy.js")


def code_on(site: Path) -> None:
    essay = text(site, CAPABILITIES)
    for mark in CODE_MARKS:
        assert mark in essay, f"the essay's code block has no {mark}"
    assert "/js/copy.js" in text(site, "index.html"), "the home page has no copy buttons' script"


def code_off(site: Path) -> None:
    marked = {mark: [p.name for p in pages(site) if mark in p.read_text(encoding="utf-8")]
              for mark in CODE_MARKS}
    assert not any(marked.values()), f"code on pages: {marked}"


# What viz puts on a page: its two kinds of figure, its stylesheet, and on a
# page that sets `viz: true` Vega (from its CDN) and viz.js.
VIZ_MARKS = ('class="viz-figure"', 'class="viz-interactive"', "/css/viz.css", "/js/viz.js",
             "cdn.jsdelivr.net/npm/vega")


def viz_on(site: Path) -> None:
    essay = text(site, CAPABILITIES)
    for mark in VIZ_MARKS:
        assert mark in essay, f"the essay's figures have no {mark}"
    assert "<polyline" in essay, "the static figure's SVG was not inlined"
    assert 'class="vega-spec"' in essay, "the interactive figure has no spec"


def viz_off(site: Path) -> None:
    marked = {mark: [p.name for p in pages(site) if mark in p.read_text(encoding="utf-8")]
              for mark in VIZ_MARKS}
    assert not any(marked.values()), f"viz on pages: {marked}"


# Each registered switch: (its on check, its off check).
CHECKS = {
    "archive": (archive_on, archive_off),
    "code": (code_on, code_off),
    "math": (math_on, math_off),
    "music": (music_on, music_off),
    "viz": (viz_on, viz_off),
}
