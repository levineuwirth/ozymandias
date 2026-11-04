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

import json
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
        if (site / "_site" / rel).is_file():
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


LIBRARY_LINK = re.compile(r'href="[./]*library\.html"')


def library_on(site: Path) -> None:
    library = text(site, "library.html")
    assert re.search(r'<section class="library-section">\s*<h2 id="notes">', library), \
        "the library has no shelf for the Notes portal"
    assert "one for each portal" in library, "the library does not show its intro (content/library.md)"
    assert "/library.html" in text(site, "sitemap.xml"), "the sitemap does not list /library.html"
    assert LIBRARY_LINK.search(text(site, "bibliography/index.html")), "the bibliography does not link the library"


def library_off(site: Path) -> None:
    assert not (site / "_site/library.html").exists(), "/library.html was built"
    assert "/library.html" not in text(site, "sitemap.xml"), "the sitemap lists /library.html"
    linked = [p.name for p in pages(site) if LIBRARY_LINK.search(p.read_text(encoding="utf-8"))]
    assert not linked, f"links to /library.html on {linked}"


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


def asset_tag(path: str) -> re.Pattern:
    """The tag that loads an engine asset, as a page's relative URLs have it."""
    return re.compile(rf'''(?:src|href)="[./]*{re.escape(path)}"''')


def loading(site: Path, *paths: str) -> list[str]:
    """Each page that loads one of `paths`, with its path under _site."""
    tags = [asset_tag(path) for path in paths]
    return [str(p.relative_to(site / "_site")) for p in pages(site)
            if any(tag.search(p.read_text(encoding="utf-8")) for tag in tags)]


def selection_toolbar_on(site: Path) -> None:
    essay = text(site, "essays/long/index.html")
    for path in ("js/selection-popup.js", "css/selection-popup.css"):
        assert asset_tag(path).search(essay), f"the essay does not load {path}"


def selection_toolbar_off(site: Path) -> None:
    pages_with = loading(site, "js/selection-popup.js", "css/selection-popup.css")
    assert not pages_with, f"the toolbar is loaded on {pages_with}"


def annotations_on(site: Path) -> None:
    essay = text(site, "essays/long/index.html")
    for path in ("js/annotations.js", "css/annotations.css"):
        assert asset_tag(path).search(essay), f"the essay does not load {path}"
    assert 'data-action="clear-annotations"' in essay, "the settings panel cannot clear annotations"


def annotations_off(site: Path) -> None:
    pages_with = loading(site, "js/annotations.js", "css/annotations.css")
    assert not pages_with, f"annotations are loaded on {pages_with}"
    offered = [p.name for p in pages(site) if 'data-action="clear-annotations"' in p.read_text(encoding="utf-8")]
    assert not offered, f"Clear Annotations on {offered}"


def popups_on(site: Path) -> None:
    essay = text(site, "essays/long/index.html")
    for path in ("js/popups.js", "css/popups.css"):
        assert asset_tag(path).search(essay), f"the essay does not load {path}"
    assert "An annotated link" in text(site, "data/annotations.json"), "the author's previews are not published"


def popups_off(site: Path) -> None:
    pages_with = loading(site, "js/popups.js", "css/popups.css")
    assert not pages_with, f"popups are loaded on {pages_with}"
    assert not (site / "_site/data/annotations.json").exists(), "the author's previews are published"


def search_filters_on(site: Path) -> None:
    assert asset_tag("js/search-filters.js").search(text(site, "search.html")), \
        "the search page does not load the filters"


def search_filters_off(site: Path) -> None:
    pages_with = loading(site, "js/search-filters.js")
    assert not pages_with, f"the search filters are loaded on {pages_with}"


SEMANTIC_INDEX = ("data/semantic-index.bin", "data/semantic-meta.json")


def semantic_search_on(site: Path) -> None:
    assert asset_tag("js/semantic-search.js").search(text(site, "search.html")), \
        "the search page does not load semantic search"
    for path in SEMANTIC_INDEX:
        assert (site / "_site" / path).is_file(), f"/{path} was not published"


def semantic_search_off(site: Path) -> None:
    pages_with = loading(site, "js/semantic-search.js")
    assert not pages_with, f"semantic search is loaded on {pages_with}"
    for path in SEMANTIC_INDEX:
        assert not (site / "_site" / path).exists(), f"/{path} was published"


READING_PAGES = ("poetry/a-poem.html", "fiction/a-story.html")


def reading_progress_on(site: Path) -> None:
    for page in READING_PAGES:
        html = text(site, page)
        assert 'id="reading-progress"' in html, f"{page} has no progress bar"
        assert asset_tag("js/reading.js").search(html), f"{page} does not load reading.js"


def reading_progress_off(site: Path) -> None:
    pages_with = loading(site, "js/reading.js")
    assert not pages_with, f"reading.js is loaded on {pages_with}"
    barred = [p.name for p in pages(site) if 'id="reading-progress"' in p.read_text(encoding="utf-8")]
    assert not barred, f"a progress bar on {barred}"
    assert 'class="reading-mode' in text(site, READING_PAGES[0]), "the poem is no longer in reading mode"


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


KEYWORD = "fixtures"   # the long essay's, and alpha2001's in the .bib


def keyword_link(cls: str) -> re.Pattern:
    """A link of class `cls` to the keyword's page."""
    return re.compile(rf'<a\b(?=[^>]*\bclass="{cls}")(?=[^>]*\bhref="[./]*bibliography/{KEYWORD}/")[^>]*>')


def bibliography_index_on(site: Path) -> None:
    assert 'id="ref-alpha2001"' in text(site, "bibliography/index.html"), "/bibliography/ does not list a cited work"
    keyword = text(site, f"bibliography/{KEYWORD}/index.html")
    assert "/essays/long/" in keyword and 'id="ref-alpha2001"' in keyword, \
        f"/bibliography/{KEYWORD}/ does not list the essay and the work"
    assert "/bibliography/" in text(site, "sitemap.xml"), "the sitemap does not list /bibliography/"
    essay = text(site, "essays/long/index.html")
    for cls in ("meta-keyword", "bib-keyword"):
        assert keyword_link(cls).search(essay), f"the essay's {cls} does not link its page"


def bibliography_index_off(site: Path) -> None:
    assert not (site / "_site/bibliography").exists(), "/bibliography/ was built"
    assert "/bibliography/" not in text(site, "sitemap.xml"), "the sitemap lists /bibliography/"
    linked = [p.name for p in pages(site) if re.search(r'href="[./]*bibliography/', p.read_text(encoding="utf-8"))]
    assert not linked, f"links to the bibliography's pages on {linked}"
    essay = text(site, "essays/long/index.html")
    for cls in ("meta-keyword", "bib-keyword"):
        assert f'<span class="{cls}">{KEYWORD}</span>' in essay, f"the essay's {cls} is not shown"


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


TAGGED = "notes"   # the fixture's portal, and its essays' tag


def tag_pages_on(site: Path) -> None:
    assert (site / "_site" / TAGGED / "index.html").is_file(), f"/{TAGGED}/ was not built"
    essay = text(site, "essays/long/index.html")
    assert re.search(rf'<a class="meta-tag" href="[./]*{TAGGED}/">{TAGGED}</a>', essay), "the essay's tag is not a link"
    assert 'class="nav-portals"' in essay, "the navigation has no portals"
    assert 'class="hp-portals"' in text(site, "index.html"), "the home page has no portals"


def tag_pages_off(site: Path) -> None:
    assert not (site / "_site" / TAGGED / "index.html").exists(), f"/{TAGGED}/ was built"
    linking = re.compile(rf'href="[./]*{TAGGED}/"|class="(?:nav-portals|nav-portal-toggle|hp-portals)"')
    linked = [p.name for p in pages(site) if linking.search(p.read_text(encoding="utf-8"))]
    assert not linked, f"links to tag pages, or portals, on {linked}"
    assert f'<span class="meta-tag">{TAGGED}</span>' in text(site, "essays/long/index.html"), \
        "the essay's tag is not shown"
    landing = re.compile(r'<a class="meta-tag" href="[./]*photography/">photography</a>')
    assert any(landing.search(p.read_text(encoding="utf-8")) for p in pages(site)), \
        "a tag naming a section no longer links its landing"


AUTHOR, AUTHOR_SLUG = "Example Author", "example-author"   # site.yaml's author


def authors_on(site: Path) -> None:
    listing = sorted((site / "_site/authors" / AUTHOR_SLUG).rglob("index.html"))   # and its later pages
    assert any("/essays/long/" in p.read_text(encoding="utf-8") for p in listing), \
        "the author's pages do not list the essay"
    assert re.search(rf'<a href="[./]*authors/{AUTHOR_SLUG}/">{AUTHOR}</a>', text(site, "essays/long/index.html")), \
        "the essay's byline does not link its author"


def authors_off(site: Path) -> None:
    assert not (site / "_site/authors").exists(), "/authors/ was built"
    linked = [p.name for p in pages(site) if re.search(r'href="[./]*authors/', p.read_text(encoding="utf-8"))]
    assert not linked, f"links to author pages on {linked}"
    assert re.search(rf'<span class="meta-label">by</span>{AUTHOR}\s', text(site, "essays/long/index.html")), \
        "the essay's byline does not name its author"


RANDOM_SCRIPT = re.compile(r'<script src="[./]*js/random\.js"')


def random_on(site: Path) -> None:
    listed = json.loads(text(site, "random-pages.json"))
    assert "/essays/long/index.html" in listed, "/random-pages.json does not list the essay"
    home = text(site, "index.html")
    assert RANDOM_SCRIPT.search(home), "the home page does not load random.js"
    assert re.search(r'<a href="[./]*short-page\.html" data-random>', home), "the home page has no Random link"


def random_off(site: Path) -> None:
    assert not (site / "_site/random-pages.json").exists(), "/random-pages.json was built"
    loading = [p.name for p in pages(site) if RANDOM_SCRIPT.search(p.read_text(encoding="utf-8"))]
    assert not loading, f"random.js is loaded on {loading}"


def new_on(site: Path) -> None:
    assert "/essays/long/" in text(site, "new.html"), "/new.html does not list the essay"
    assert "/new.html" in text(site, "sitemap.xml"), "the sitemap does not list /new.html"
    assert re.search(r'<a href="[./]*new\.html" class="content-divider-logo">', text(site, "essays/long/index.html")), \
        "the essay's mark does not link /new.html"
    assert re.search(r'<a href="[./]*new\.html" class="aftermatter-logo">', text(site, "essays/long/index.html")), \
        "the aftermatter's mark does not link /new.html"


def new_off(site: Path) -> None:
    assert not (site / "_site/new.html").exists(), "/new.html was built"
    assert "/new.html" not in text(site, "sitemap.xml"), "the sitemap lists /new.html"
    linked = [p.name for p in pages(site) if re.search(r'href="[./]*new\.html"', p.read_text(encoding="utf-8"))]
    assert not linked, f"links to /new.html on {linked}"
    assert '<span class="content-divider-logo" aria-hidden="true"></span>' in text(site, "essays/long/index.html"), \
        "the essay's mark is not drawn"
    assert '<span class="aftermatter-logo" aria-hidden="true"></span>' in text(site, "essays/long/index.html"), \
        "the aftermatter's mark is not drawn"


# Each registered switch: (its on check, its off check).
SERIES, FRAME, LONE = "fixture-series", "fixture-series/one", "lone-frame"
PHOTO_ASSETS = ("js/photography-modes.js", "js/photography-slideshow.js", "js/photography-map.js",
                "css/photography.css", "leaflet/leaflet.js")


def photography_on(site: Path) -> None:
    landing = text(site, "photography/index.html")
    assert f"/photography/{SERIES}/" in landing and f"/photography/{LONE}.html" in landing, \
        "the landing lists neither the series nor the flat frame"
    assert 'data-page-type="photography"' in text(site, f"photography/{FRAME}/index.html"), \
        "a frame in the series is not a photograph's page"
    assert 'data-page-type="photography"' in text(site, f"photography/{LONE}.html"), \
        "the flat frame is not a photograph's page"
    for rel in (f"{FRAME}.jpg", f"{LONE}.jpg", "by-year/index.html", "contact-sheet/index.html"):
        assert (site / "_site/photography" / rel).is_file(), f"/photography/{rel} was not published"
    pins = json.loads(text(site, "photography/map.json"))
    assert len(pins) == 4, f"map.json has {len(pins)} pins, not the three frames in the series and the flat one"
    assert loading(site, "leaflet/leaflet.js") == ["photography/map/index.html"], "the map page does not load Leaflet"
    assert f"/photography/{LONE}" in text(site, "sitemap.xml"), "the sitemap does not list the flat frame"
    if (site / "_site" / TAGGED / "index.html").is_file():
        assert LONE in text(site, f"{TAGGED}/index.html"), "the frame is not on its tag's page"
    if (site / "_site/library.html").is_file():
        assert LONE in text(site, "library.html"), "the library does not shelve the frame"
    if (site / "_site/stats/index.html").is_file():
        stats = text(site, "stats/index.html")
        assert 'href="#photography"' in stats and 'id="photography"' in stats, "/stats/ has no photography section"
        assert "Photographs" in stats, "/stats/ counts no photographs"


def photography_off(site: Path) -> None:
    # /photography/ may still be built, like /music/: without the switch a
    # flat file under content/photography/ is an ordinary page.
    for rel in ("map.json", "map", "by-year", "contact-sheet", "feed.xml", SERIES, f"{LONE}.jpg"):
        assert not (site / "_site/photography" / rel).exists(), f"/photography/{rel} was published"
    if (site / "_site/photography" / f"{LONE}.html").is_file():
        assert 'data-page-type="photography"' not in text(site, f"photography/{LONE}.html"), \
            "the flat file is a photograph's page"
    assert not loading(site, *PHOTO_ASSETS), f"photography's assets load on {loading(site, *PHOTO_ASSETS)}"
    framed = [p.name for p in pages(site) if 'data-page-type="photography"' in p.read_text(encoding="utf-8")]
    assert not framed, f"a photograph's page: {framed}"
    assert f"/photography/{SERIES}/" not in text(site, "sitemap.xml"), "the sitemap lists the series"
    if (site / "_site/stats/index.html").is_file():
        stats = text(site, "stats/index.html")
        for mark in ('href="#photography"', 'id="photography"', "Photographs", "photography, and"):
            assert mark not in stats, f"/stats/ still has {mark}"
    if (site / "_site/library.html").is_file():
        assert LONE not in text(site, "library.html"), "the library shelves the flat file as a photograph"
    if (site / "_site" / TAGGED / "index.html").is_file():
        assert LONE not in text(site, f"{TAGGED}/index.html"), "a tag's page lists the flat file as a photograph"


CHECKS = {
    "annotations": (annotations_on, annotations_off),
    "archive": (archive_on, archive_off),
    "authors": (authors_on, authors_off),
    "bibliography-index": (bibliography_index_on, bibliography_index_off),
    "build-page": (build_page_on, build_page_off),
    "code": (code_on, code_off),
    "code-refs": (code_refs_on, code_refs_off),
    "commonplace": (commonplace_on, commonplace_off),
    "epistemic": (epistemic_on, epistemic_off),
    "footer-build-time": (footer_build_time_on, footer_build_time_off),
    "library": (library_on, library_off),
    "marks": (marks_on, marks_off),
    "math": (math_on, math_off),
    "music": (music_on, music_off),
    "new": (new_on, new_off),
    "now": (now_on, now_off),
    "pdf-viewer": (pdf_viewer_on, pdf_viewer_off),
    "photography": (photography_on, photography_off),
    "popups": (popups_on, popups_off),
    "random": (random_on, random_off),
    "reading-progress": (reading_progress_on, reading_progress_off),
    "search-filters": (search_filters_on, search_filters_off),
    "selection-toolbar": (selection_toolbar_on, selection_toolbar_off),
    "semantic-search": (semantic_search_on, semantic_search_off),
    "signature-link": (signature_link_on, signature_link_off),
    "signing": (signing_on, signing_off),
    "source-refs": (source_refs_on, source_refs_off),
    "stats-page": (stats_page_on, stats_page_off),
    "tag-pages": (tag_pages_on, tag_pages_off),
    "vita": (vita_on, vita_off),
    "viz": (viz_on, viz_off),
}
