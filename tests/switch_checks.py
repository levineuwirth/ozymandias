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
    if (site / "_site/build/index.html").is_file():
        assert 'id="archive"' in text(site, "build/index.html"), "the build page has no link-archive section"


def archive_off(site: Path) -> None:
    assert not (site / "_site/archive").exists(), "_site/archive/ was built"
    marked = [p.name for p in pages(site) if "archive-affordance" in p.read_text(encoding="utf-8")]
    assert not marked, f"archive marks on {marked}"
    if (site / "_site/build/index.html").is_file():
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
    if (site / "_site/stats/index.html").is_file():
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
    if (site / "_site/stats/index.html").is_file():
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


PAPER = "/essays/capabilities/paper.pdf"   # the essay's PDF: a link, a citation's file, an embed


def pdf_viewer_on(site: Path) -> None:
    essay = text(site, CAPABILITIES)
    opened = essay.count(f"pdfjs/web/viewer.html?file={PAPER}")
    assert opened >= 3, f"{opened} of the link, the citation and the embed open the viewer"
    assert '<iframe class="pdf-embed"' in essay, "the embed is not the viewer's"


def pdf_viewer_off(site: Path) -> None:
    marked = {mark: [p.name for p in pages(site) if mark in p.read_text(encoding="utf-8")]
              for mark in ("pdfjs/", "pdf-embed")}
    assert not any(marked.values()), f"the viewer on pages: {marked}"
    if (site / "_site" / CAPABILITIES).is_file():
        essay = text(site, CAPABILITIES)
        links = re.findall(rf'<a href="[./]*{PAPER[1:]}(?:#page=1)?" class="pdf-link" data-pdf-src="{PAPER}">', essay)
        assert len(links) >= 3, f"{len(links)} of the link, the citation and the embed go to the PDF as one"


SOURCED = "data/bibliography.bib"   # the file the essay names, three ways


def source_refs_on(site: Path) -> None:
    essay = text(site, CAPABILITIES)
    marked = essay.count(f'class="source-ref" data-source-path="{SOURCED}"')
    assert marked >= 2, f"{marked} of the inline code and the links are source references"
    assert (site / "_site/source" / SOURCED).is_file(), "/source/ has no copy of the file"


def source_refs_off(site: Path) -> None:
    assert not (site / "_site/source").exists(), "_site/source/ was built"
    mark = re.compile(r'class="[^"]*\bsource-ref\b|data-source-path=')
    marked = [p.name for p in pages(site) if mark.search(p.read_text(encoding="utf-8"))]
    assert not marked, f"source references on {marked}"


SNAPSHOT = "code-refs/github/example/fixture/0123456789abcdef0123456789abcdef01234567"


def code_refs_on(site: Path) -> None:
    assert 'data-code-ref="blob"' in text(site, CAPABILITIES), "the essay's GitHub link is not tagged"
    for rel in ("blob/README.md.txt", "blob/README.md.sections.json", "commit.json"):
        assert (site / "_site" / SNAPSHOT / rel).is_file(), f"{rel} of the snapshot was not published"


def code_refs_off(site: Path) -> None:
    assert not (site / "_site/code-refs").exists(), "_site/code-refs/ was built"
    tagged = [p.name for p in pages(site) if "data-code-ref" in p.read_text(encoding="utf-8")]
    assert not tagged, f"code references on {tagged}"


# What the epistemic profile puts on a page: the strip, its terms, the
# figure, the footer's section.
EPISTEMIC_MARKS = ("meta-epistemic-strip", "data-ep-term=", "frontmatter-mark--epistemic", "ep-expanded")


def epistemic_on(site: Path) -> None:
    essay = text(site, CAPABILITIES)
    for mark in EPISTEMIC_MARKS:
        assert mark in essay, f"the essay's epistemic profile has no {mark}"
    assert 'data-ep-term="peer-status"' in essay, "the essay's peer status is not shown"
    assert f'"/{CAPABILITIES}"' in text(site, "data/epistemic-meta.json"), "epistemic-meta.json lacks the essay"
    if (site / "_site/build/index.html").is_file():
        assert '<h2 id="epistemic"' in text(site, "build/index.html"), "the build page has no epistemic coverage"


def epistemic_off(site: Path) -> None:
    marked = {mark: [p.name for p in pages(site) if mark in p.read_text(encoding="utf-8")]
              for mark in EPISTEMIC_MARKS}
    assert not any(marked.values()), f"epistemic profiles on pages: {marked}"
    assert not (site / "_site/data/epistemic-meta.json").exists(), "epistemic-meta.json was built"
    if (site / "_site/build/index.html").is_file():
        build = text(site, "build/index.html")
        assert '<h2 id="epistemic"' not in build and "Epistemic figure" not in build, \
            "the build page covers the epistemic profile"


def marks_on(site: Path) -> None:
    essay = text(site, CAPABILITIES)
    assert 'frontmatter-mark--monogram">' in essay and "M90 190" in essay, "the essay's monogram is not inlined"
    assert "frontmatter-mark--placeholder" in text(site, "essays/long/index.html"), \
        "an essay without a mark has no placeholder roundel"
    if (site / "_site/build/index.html").is_file():
        assert '<h2 id="marks"' in text(site, "build/index.html"), "the build page has no marks coverage"


def marks_off(site: Path) -> None:
    marked = {mark: [p.name for p in pages(site) if mark in p.read_text(encoding="utf-8")]
              for mark in ("frontmatter-mark--monogram", "item-card--has-monogram")}
    assert not any(marked.values()), f"monograms on {marked}"
    assert '<div class="frontmatter-mark-slot frontmatter-mark-slot--left"></div>' in \
        text(site, "essays/long/index.html"), "an essay's monogram slot is not left empty"
    if (site / "_site/build/index.html").is_file():
        assert '<h2 id="marks"' not in text(site, "build/index.html"), "the build page has marks coverage"


def vita_on(site: Path) -> None:
    cv = text(site, "cv.html")
    assert "Example University" in cv, "the CV page does not render yaml-source/data/education.yml"
    assert "/css/vita.css" in cv, "the CV page has no vita stylesheet"


def vita_off(site: Path) -> None:
    # Without the switch content/cv.md may still be an ordinary page.
    marked = {mark: [p.name for p in pages(site) if mark in p.read_text(encoding="utf-8")]
              for mark in ("/css/vita.css", "Example University")}
    assert not any(marked.values()), f"the CV on pages: {marked}"


COMMONPLACE_MARKS = ("/js/commonplace.js", "/css/commonplace.css", "cp-attribution")


def commonplace_on(site: Path) -> None:
    book = text(site, "commonplace.html")
    for mark in COMMONPLACE_MARKS:
        assert mark in book, f"the commonplace book has no {mark}"
    assert "An Example Writer" in book, "the commonplace book does not render data/commonplace.yaml"


def commonplace_off(site: Path) -> None:
    # Without the switch content/commonplace.md may still be an ordinary page.
    marked = {mark: [p.name for p in pages(site) if mark in p.read_text(encoding="utf-8")]
              for mark in COMMONPLACE_MARKS}
    assert not any(marked.values()), f"the commonplace book on pages: {marked}"


NOW_MARKS = ("/js/now.js", "/css/now.css", "now-stamp")


def now_on(site: Path) -> None:
    current = text(site, "current.html")
    for mark in NOW_MARKS:
        assert mark in current, f"the now page has no {mark}"
    assert "An example project" in current, "the now page does not render data/now.yaml"
    assert "/current.html</loc><lastmod>2026-02-01" in text(site, "sitemap.xml").replace("\n", "").replace(" ", ""), \
        "the sitemap does not date the now page by data/now.yaml"


def now_off(site: Path) -> None:
    # Without the switch content/current.md may still be an ordinary page.
    marked = {mark: [p.name for p in pages(site) if mark in p.read_text(encoding="utf-8")]
              for mark in NOW_MARKS}
    assert not any(marked.values()), f"the now page on pages: {marked}"


def signing_on(site: Path) -> None:
    # make sign, which test_fixture_site runs for every build.
    assert (site / "_site/index.html.sig").is_file(), "make sign signed no page"
    assert (site / "data/sign-manifest.txt").is_file(), "make sign left no manifest"


def signing_off(site: Path) -> None:
    signed = [str(p.relative_to(site / "_site")) for p in (site / "_site").rglob("*.sig")]
    assert not signed, f"make sign signed {signed[:3]}"


def signature_link_on(site: Path) -> None:
    assert 'class="footer-sig-link"' in text(site, "essays/long/index.html"), "the footer has no signature link"


def signature_link_off(site: Path) -> None:
    linked = [p.name for p in pages(site) if "footer-sig-link" in p.read_text(encoding="utf-8")]
    assert not linked, f"signature links on {linked}"


def build_page_on(site: Path) -> None:
    assert "Build Telemetry" in text(site, "build/index.html"), "/build/ was not built"
    assert 'class="footer-build-link"' in text(site, "essays/long/index.html"), "the footer has no link to /build/"


def build_page_off(site: Path) -> None:
    assert not (site / "_site/build/index.html").exists(), "/build/ was built"
    linked = [p.name for p in pages(site) if "footer-build-link" in p.read_text(encoding="utf-8")]
    assert not linked, f"links to /build/ on {linked}"


def stats_page_on(site: Path) -> None:
    assert 'id="activity"' in text(site, "stats/index.html"), "/stats/ was not built"


def stats_page_off(site: Path) -> None:
    assert not (site / "_site/stats").exists(), "/stats/ was built"


def footer_build_time_on(site: Path) -> None:
    # make stamp-build-time, which test_fixture_site runs for every build.
    assert (site / "_site/build/time.txt").is_file(), "make stamp-build-time wrote no time"
    assert "data-build-time" in text(site, "essays/long/index.html"), "the footer has no build time"


def footer_build_time_off(site: Path) -> None:
    assert not (site / "_site/build/time.txt").exists(), "the build's time was published"
    stamped = [p.name for p in pages(site) if "data-build-time" in p.read_text(encoding="utf-8")]
    assert not stamped, f"build times on {stamped}"


# Each registered switch: (its on check, its off check).
CHECKS = {
    "archive": (archive_on, archive_off),
    "build-page": (build_page_on, build_page_off),
    "code": (code_on, code_off),
    "code-refs": (code_refs_on, code_refs_off),
    "commonplace": (commonplace_on, commonplace_off),
    "epistemic": (epistemic_on, epistemic_off),
    "footer-build-time": (footer_build_time_on, footer_build_time_off),
    "marks": (marks_on, marks_off),
    "math": (math_on, math_off),
    "music": (music_on, music_off),
    "now": (now_on, now_off),
    "pdf-viewer": (pdf_viewer_on, pdf_viewer_off),
    "signature-link": (signature_link_on, signature_link_off),
    "signing": (signing_on, signing_off),
    "source-refs": (source_refs_on, source_refs_off),
    "stats-page": (stats_page_on, stats_page_off),
    "vita": (vita_on, vita_off),
    "viz": (viz_on, viz_off),
}
