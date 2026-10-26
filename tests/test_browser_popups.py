"""Link popups, in Chromium and Firefox, with every outside answer faked.

static/js/popups.js previews a link on hover or focus. Ten providers ask
an outside API (Wikipedia, arXiv, CrossRef, GitHub, Open Library, bioRxiv,
medRxiv, YouTube, the Internet Archive, PubMed; arXiv, the Archive and
PubMed through the same-origin /proxy/), and same-origin kinds need no
one: internal pages, PDF thumbnails, source and code references,
citations, local annotations. A fixture page carries a link of each.

Every request that would leave serve.py is answered here (context.route)
with a response shaped as the real API's, or aborted and recorded, which
fails the test: the runs are deterministic, offline, and spend no one's
rate limit. The live origins are the CSP sweep's to meet. The enforcing
CSP (connect-src, img-src) is checked before a request reaches its route,
so a provider whose origin fell out of the policy fails here too. CORS is
not checked: the browser reads a route.fulfill() answer whatever its
headers say (a GitHub answer without Access-Control-Allow-Origin still
renders), and no test in the suite meets the real APIs' CORS headers.

Checked: each provider's request and popup; that a failed, misshapen or
mistyped answer shows nothing, throws nothing and is not cached; that
hostile text and image sources from an API stay inert; an abstract sent
as markup (CrossRef's JATS, an Archive description) reads as text, its
entities decoded once and its paragraphs apart; the arXiv lead
figure, on time and late; Escape (audit A03); Forgejo links, which get no
popup and make no request (J05); a same-origin source reference, which
gets its source preview (J08).

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_popups -v
"""

from __future__ import annotations

import base64
import html
import json
import re
import tempfile
import unittest
from pathlib import Path
from urllib.parse import parse_qs, urlsplit

from tests._browser import (BROWSERS, SITE, FakeNetwork, check_site, enforcing_csp,
                            require_playwright, requires_browser, site_server)
from tests._browser import fake_answer as answer

PNG = base64.b64decode("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk"
                       "+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==")


ATOM = """<?xml version="1.0" encoding="UTF-8"?>
<feed xmlns="http://www.w3.org/2005/Atom"><entry>
<title>A Fake Paper
  on Graphs</title>
<summary>We study graphs that do not exist.</summary>
<author><name>Ada Lovelace</name></author><author><name>Alan Turing</name></author>
</entry></feed>"""

ARXIV_HTML = """<!doctype html><html><body><article>
<figure class="ltx_figure"><img class="ltx_graphics" src="2401.01234v1/x1.png" width="400" height="300"></figure>
</article></body></html>"""

# name -> pattern of the requests it answers
ROUTES = {
    "wikipedia":       r"^https://en\.wikipedia\.org/w/api\.php\?",
    "wikipedia-de":    r"^https://de\.wikipedia\.org/w/api\.php\?",
    "wikimedia-image": r"^https://(?:upload|thumb)\.wikimedia\.org/",
    "arxiv":           r"/proxy/arxiv/api/query\?",
    "arxiv-figure":    r"/proxy/arxiv-html/[^/?]+/[^?]+\.png$",
    "arxiv-html":      r"/proxy/arxiv-html/[^/?]+$",
    "doi":             r"^https://api\.crossref\.org/works/",
    "github":          r"^https://api\.github\.com/repos/",
    "openlibrary":     r"^https://openlibrary\.org/(?:works|books)/[^?]+\.json$",
    "biorxiv":         r"^https://api\.biorxiv\.org/details/biorxiv/",
    "medrxiv":         r"^https://api\.biorxiv\.org/details/medrxiv/",
    "youtube":         r"^https://www\.youtube\.com/oembed\?",
    "archive":         r"/proxy/archive/metadata/",
    "pubmed":          r"/proxy/pubmed/entrez/eutils/esummary\.fcgi\?",
    "annotations":     r"/data/annotations\.json$",
}

# The real APIs' shapes, cut down to what the parsers read.
ANSWERS = {
    "wikipedia": answer({"query": {"pages": {"1": {
        "title": "Graph theory",
        "extract": "<p><b>Graph theory</b> is the study of <i>graphs</i>.</p>",
        "thumbnail": {"source": "https://upload.wikimedia.org/wikipedia/commons/x/Graph.png",
                      "width": 480, "height": 320}}}}}),
    "wikipedia-de": answer({"query": {"pages": {"2": {
        "title": "Graph (Graphentheorie)", "extract": "<p>Ein Graph ist eine Struktur.</p>"}}}}),
    "wikimedia-image": answer(PNG, "image/png"),
    "arxiv": answer(ATOM, "application/atom+xml; charset=utf-8"),
    "arxiv-html": answer(ARXIV_HTML, "text/html"),
    "arxiv-figure": answer(PNG, "image/png"),
    "doi": answer({"status": "ok", "message": {
        "title": ["A Fake DOI Title"], "container-title": ["Journal of Fakes"],
        "issued": {"date-parts": [[2021, 3]]},
        "author": [{"given": "Grace", "family": "Hopper"}],
        "abstract": "<jats:p>An abstract with <jats:italic>markup</jats:italic>.</jats:p>"}}),
    "github": answer({"full_name": "octo/repo", "description": "A repository.",
                      "language": "Haskell", "stargazers_count": 42}),
    "openlibrary": answer({"title": "A Book",
                           "description": {"type": "/type/text", "value": "About the book."}}),
    "biorxiv": answer({"collection": [{"title": "A Bio Paper", "authors": "Doe, J.; Roe, R.",
                                       "abstract": "Cells."}]}),
    "medrxiv": answer({"collection": [{"title": "A Med Paper", "authors": "Poe, E.",
                                       "abstract": "Patients."}]}),
    "youtube": answer({"title": "A Video", "author_name": "A Channel"}),
    "archive": answer({"metadata": {"title": "An Item", "creator": "Someone", "year": "1999",
                                    "description": "<p>An archived item.</p>"}}),
    "pubmed": answer({"result": {"uids": ["12345678"], "12345678": {
        "title": "A PubMed Paper", "authors": [{"name": "Smith J"}],
        "fulljournalname": "Journal of Medicine", "pubdate": "2020 Jan"}}}),
    "annotations": answer({"https://example.org/annotated": {
        "title": "Annotated link", "annotation": "The author's own note."}}),
}

# link id -> (provider answering it, popup class, title, (selector, text) more)
PROVIDERS = {
    "wikipedia":    ("wikipedia", "popup-wikipedia", "Graph theory",
                     (".popup-extract", "Graph theory is the study of graphs.")),
    "wikipedia-de": ("wikipedia-de", "popup-wikipedia", "Graph (Graphentheorie)",
                     (".popup-extract", "Ein Graph ist eine Struktur.")),
    "arxiv":        ("arxiv", "popup-arxiv", "A Fake Paper on Graphs",
                     (".popup-authors", "Ada Lovelace, Alan Turing")),
    "doi":          ("doi", "popup-doi", "A Fake DOI Title",
                     (".popup-abstract", "An abstract with markup.")),
    "github":       ("github", "popup-github", "octo/repo",
                     (".popup-meta", "Haskell · ★ 42")),
    "openlibrary":  ("openlibrary", "popup-openlibrary", "A Book",
                     (".popup-abstract", "About the book.")),
    "biorxiv":      ("biorxiv", "popup-biorxiv", "A Bio Paper",
                     (".popup-authors", "Doe, J., Roe, R.")),
    "medrxiv":      ("medrxiv", "popup-medrxiv", "A Med Paper",
                     (".popup-authors", "Poe, E.")),
    "youtube":      ("youtube", "popup-youtube", "A Video",
                     (".popup-authors", "A Channel")),
    "archive":      ("archive", "popup-archive", "An Item",
                     (".popup-authors", "Someone, 1999")),
    "pubmed":       ("pubmed", "popup-pubmed", "A PubMed Paper",
                     (".popup-meta", "Journal of Medicine, 2020")),
    "annotated":    ("annotations", "popup-annotation", "Annotated link",
                     (".popup-abstract", "The author's own note.")),
}

# link id -> what its request must say (beyond matching its route)
REQUESTS = {
    "wikipedia":    lambda r: query(r)["titles"] == ["Graph_theory"] and query(r)["origin"] == ["*"],
    "wikipedia-de": lambda r: query(r)["titles"] == ["Graph_(Graphentheorie)"],
    "arxiv":        lambda r: query(r)["id_list"] == ["2401.01234"],
    "doi":          lambda r: urlsplit(r.url).path == "/works/10.1000%2Fxyz123",
    "github":       lambda r: (urlsplit(r.url).path == "/repos/octo/repo" and
                               r.headers.get("accept") == "application/vnd.github.v3+json"),
    "openlibrary":  lambda r: r.url == "https://openlibrary.org/works/OL1W.json",
    "biorxiv":      lambda r: urlsplit(r.url).path == "/details/biorxiv/10.1101%2F2020.01.01.000001/json",
    "medrxiv":      lambda r: urlsplit(r.url).path == "/details/medrxiv/10.1101%2F2020.02.02.000002/json",
    "youtube":      lambda r: query(r)["url"] == ["https://www.youtube.com/watch?v=abc123"],
    "archive":      lambda r: urlsplit(r.url).path == "/proxy/archive/metadata/someitem",
    "pubmed":       lambda r: query(r)["id"] == ["12345678"],
}

PAGE = """<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Popups fixture</title>
<link rel="stylesheet" href="/css/base.css">
<link rel="stylesheet" href="/css/popups.css">
<script src="/js/utils.js"></script>
<script src="/js/popups.js" defer></script>
</head><body><main id="markdownBody">
<p><a id="wikipedia" href="https://en.wikipedia.org/wiki/Graph_theory">Wikipedia</a></p>
<p><a id="wikipedia-de" href="https://de.wikipedia.org/wiki/Graph_(Graphentheorie)">Wikipedia (de)</a></p>
<p><a id="arxiv" href="https://arxiv.org/abs/2401.01234v2">arXiv</a></p>
<p><a id="doi" href="https://doi.org/10.1000/xyz123">DOI</a></p>
<p><a id="github" href="https://github.com/octo/repo">GitHub</a></p>
<p><a id="openlibrary" href="https://openlibrary.org/works/OL1W">Open Library</a></p>
<p><a id="biorxiv" href="https://www.biorxiv.org/content/10.1101/2020.01.01.000001v1">bioRxiv</a></p>
<p><a id="medrxiv" href="https://www.medrxiv.org/content/10.1101/2020.02.02.000002v1">medRxiv</a></p>
<p><a id="youtube" href="https://www.youtube.com/watch?v=abc123">YouTube</a></p>
<p><a id="archive" href="https://archive.org/details/someitem">Internet Archive</a></p>
<p><a id="pubmed" href="https://pubmed.ncbi.nlm.nih.gov/12345678/">PubMed</a></p>
<p><a id="annotated" href="https://example.org/annotated">annotated</a></p>
<p><a id="forge" href="https://git.levineuwirth.org/neuwirth/levineuwirth.org">Forgejo</a></p>
<p><a id="internal" href="/essays/proof-broker/">an essay</a></p>
<p><a id="pdf" class="pdf-link" data-pdf-src="/cv.pdf" href="/pdfjs/web/viewer.html?file=/cv.pdf">CV</a></p>
<p><a id="source" class="source-ref" data-source-path="data/similar-links.json"
      href="../source/data/similar-links.json">similar-links.json</a></p>
<p>{code_ref}</p>
<p>A claim<a id="cite" class="cite-link" href="#ref-knuth">[1]</a>.</p>
<div id="refs"><div id="ref-knuth">Knuth, D. <em>The Art of Computer Programming</em>.</div></div>
</main></body></html>
"""


def query(request) -> dict:
    return parse_qs(urlsplit(request.url).query)


def code_ref() -> tuple[str, dict]:
    """A link to a GitHub blob snapshot the build made (tools/code-refs.py),
    as build/Filters/CodeRefs.hs tags one, and what its popup names."""
    snaps = sorted((SITE / "code-refs" / "github").glob("*/*/*/blob/**/*.txt"))
    if not snaps:
        raise AssertionError("no GitHub blob snapshot under _site/code-refs/")
    rel = snaps[0].relative_to(SITE / "code-refs" / "github").parts
    owner, repo, sha, path = rel[0], rel[1], rel[2], "/".join(rel[4:])[:-len(".txt")]
    link = (f'<a id="code" href="https://github.com/{owner}/{repo}/blob/{sha}/{path}"'
            f' data-code-ref="blob" data-code-src="/code-refs/github/{"/".join(rel)}"'
            f' data-code-repo="{owner}/{repo}" data-code-sha="{sha}" data-code-path="{path}"'
            f' data-code-date="2026-10-01T00:00:00Z">snapshot</a>')
    return link, {"path": path, "rev": f"{owner}/{repo} @ {sha[:7]}"}


@requires_browser
class LinkPopups(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-popups-")))
        (tmp / "fixtures").mkdir()
        link, cls.code = code_ref()
        (tmp / "fixtures" / "popups.html").write_text(PAGE.format(code_ref=link), encoding="utf-8")
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp(), tmp / "fixtures"))
        essay = (SITE / "essays" / "proof-broker" / "index.html").read_text(encoding="utf-8")
        title = re.search(r'<h1 class="page-title"[^>]*>(.*?)</h1>', essay, re.S)
        cls.essay_title = " ".join(html.unescape(re.sub(r"<[^>]+>", "", title.group(1))).split())
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def fixture(self, browser: str, **answers):
        """The fixture page and its FakeNetwork, with `answers` replacing the
        default ones. At cleanup: no page errors, no CSP violations, no
        request that no route answers."""
        context = self.browsers[browser].new_context(viewport={"width": 1280, "height": 1600})
        self.addCleanup(context.close)
        context.add_init_script("""window.__csp = [];
            document.addEventListener('securitypolicyviolation',
                e => window.__csp.push(e.effectiveDirective + ' ' + e.blockedURI));""")
        net = FakeNetwork(context, self.base, ROUTES, ANSWERS,
                          local=r"proxy/|data/annotations\.json")
        net.answers.update(answers)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        page.goto(self.base + "/__fixture/popups.html", wait_until="load")
        # popups.js binds its targets once annotations.json is in.
        self.expect(page.locator("#wikipedia[data-popup-bound]")).to_have_count(1)
        # Every link in view: base.css scrolls smoothly, and a hover that
        # has to scroll aims at where its link was, landing on another.
        self.assertLessEqual(page.evaluate("document.documentElement.scrollHeight"), 1600,
                             "the fixture outgrew its viewport")

        def clean():
            self.assertEqual(errors, [], "page errors")
            self.assertEqual(page.evaluate("window.__csp"), [], "CSP violations")
            self.assertEqual(net.unexpected, [], "requests no route answers")
        self.addCleanup(clean)
        return page, net

    # -- driving -----------------------------------------------------------

    def away(self, page) -> None:
        page.mouse.move(0, 0)
        self.expect(page.locator(".link-popup.is-visible")).to_have_count(0)

    def shown(self, page, link: str):
        self.away(page)
        page.hover(f"#{link}")
        popup = page.locator(".link-popup.is-visible")
        self.expect(popup).to_be_visible(timeout=5000)
        return popup

    def nothing_shown(self, page, net: FakeNetwork, link: str, route: str | None) -> None:
        """Hover `link`, wait for its request (if it makes one) to be
        answered and the popup to have had its chance, and see none."""
        self.away(page)
        before = net.hits[route] if route else 0
        page.hover(f"#{link}")
        if route:
            for _ in range(50):
                if net.hits[route] > before:
                    break
                page.wait_for_timeout(100)
            self.assertGreater(net.hits[route], before, f"{link} asked nothing of {route}")
        # A popup is drawn within a frame of its answer, and the answer is
        # given as soon as asked for: this is time to have shown one.
        page.wait_for_timeout(300)
        self.expect(page.locator(".link-popup.is-visible")).to_have_count(0)

    @staticmethod
    def text(locator) -> str:
        return " ".join(locator.inner_text().replace(" ", " ").split())

    # -- tests -------------------------------------------------------------

    def test_each_provider(self) -> None:
        for browser in BROWSERS:
            page, net = self.fixture(browser)
            for link, (route, cls, title, (sel, more)) in PROVIDERS.items():
                with self.subTest(browser=browser, link=link):
                    popup = self.shown(page, link)
                    body = popup.locator(f".{cls}")
                    self.expect(body).to_have_count(1)
                    self.assertEqual(self.text(body.locator(".popup-title")), title)
                    self.assertEqual(self.text(body.locator(sel)), more)
                    if link in REQUESTS:
                        self.assertTrue(REQUESTS[link](net.requests[route][-1]),
                                        net.requests[route][-1].url)
            with self.subTest(browser=browser, link="wikipedia", part="lead image"):
                banner = self.shown(page, "wikipedia").locator("img.popup-image-banner")
                self.expect(banner).to_have_attribute("src", re.compile(r"^https://upload\."))
                self.assertTrue(banner.evaluate("i => i.complete && i.naturalWidth > 0"))
            with self.subTest(browser=browser, part="annotations replace providers"):
                self.assertEqual(net.hits["annotations"], 1)
                self.assertEqual([u for u in net.unexpected if "example.org" in u], [])

    def test_same_origin_kinds(self) -> None:
        for browser in BROWSERS:
            page, _ = self.fixture(browser)
            with self.subTest(browser=browser, link="internal"):
                popup = self.shown(page, "internal")
                self.assertEqual(self.text(popup.locator(".popup-internal .popup-title")),
                                 self.essay_title)
            with self.subTest(browser=browser, link="pdf"):
                thumb = self.shown(page, "pdf").locator(".popup-pdf img.popup-pdf-thumb")
                self.expect(thumb).to_have_attribute("src", "/cv.thumb.png")
                self.expect(thumb).to_have_js_property("complete", True)
                self.assertGreater(thumb.evaluate("i => i.naturalWidth"), 0)
            with self.subTest(browser=browser, link="source (same origin, J08)"):
                popup = self.shown(page, "source")
                self.expect(popup.locator(".popup-source-code .popup-source-path")) \
                    .to_contain_text("data/similar-links.json")
                self.expect(popup.locator(".popup-internal")).to_have_count(0)
            with self.subTest(browser=browser, link="code reference"):
                popup = self.shown(page, "code")
                self.expect(popup.locator(".popup-source-path")).to_contain_text(self.code["path"])
                self.expect(popup.locator(".popup-source-rev")).to_contain_text(self.code["rev"])
            with self.subTest(browser=browser, link="citation"):
                entry = self.shown(page, "cite").locator(".popup-citation-entry")
                self.assertEqual(self.text(entry), "Knuth, D. The Art of Computer Programming.")

    def test_forge_links_ask_nothing(self) -> None:
        # J05: the forge sits behind Anubis and sends no CORS headers.
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, net = self.fixture(browser)
                self.nothing_shown(page, net, "forge", None)
                self.assertEqual(net.unexpected, [])

    def test_misshapen_answers_show_nothing(self) -> None:
        empty = {name: answer({}) for name in ANSWERS}
        empty["arxiv"] = answer("<feed xmlns='http://www.w3.org/2005/Atom'/>", "application/atom+xml")
        empty["annotations"] = ANSWERS["annotations"]
        for browser in BROWSERS:
            page, net = self.fixture(browser, **empty)
            for link, (route, *_) in PROVIDERS.items():
                if route == "annotations":
                    continue
                with self.subTest(browser=browser, link=link):
                    self.nothing_shown(page, net, link, route)

    def test_failed_answers_show_nothing_and_are_not_kept(self) -> None:
        # name -> the failing answer for one provider's route
        failures = {
            "server error": lambda name: answer({"error": "x"}, status=500),
            "html, not json or xml": lambda name: answer(ANSWERS[name]["body"], "text/html"),
            "malformed": lambda name: answer("<feed" if name == "arxiv" else "{",
                                             ANSWERS[name]["headers"]["content-type"]),
            "unreachable": lambda name: None,
        }
        links = {"wikipedia": "wikipedia", "arxiv": "arxiv", "archive": "archive",
                 "github": "github"}
        for browser in BROWSERS:
            for case, make in failures.items():
                page, net = self.fixture(browser, **{r: make(r) for r in links.values()})
                for link, route in links.items():
                    with self.subTest(browser=browser, case=case, link=link):
                        self.nothing_shown(page, net, link, route)
                        # Only successes are cached: the next hover asks again.
                        net.answers[route] = ANSWERS[route]
                        self.shown(page, link)

    def test_success_is_cached(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, net = self.fixture(browser)
                self.shown(page, "doi")
                self.shown(page, "doi")
                self.assertEqual(net.hits["doi"], 1)

    def test_abstracts_in_markup_read_as_text(self) -> None:
        # The markup was stripped with a regex and the rest escaped: its
        # entities showed as written and its paragraphs ran together.
        jats = ("<jats:title>Abstract</jats:title><jats:p>Effects of R&amp;D at p &lt; 0.05 on"
                " <jats:italic>firms</jats:italic>.</jats:p><jats:p>A second paragraph.</jats:p>")
        doi = answer({"status": "ok", "message": {"title": ["A Fake DOI Title"], "abstract": jats}})
        archive = answer({"metadata": {"title": "An Item", "creator": "Someone",
                                       "description": "<p>An item &amp; its <b>notes</b>.</p><p>More.</p>"}})
        for browser in BROWSERS:
            page, _ = self.fixture(browser, doi=doi, archive=archive)
            for link, want in (("doi", "Effects of R&D at p < 0.05 on firms. A second paragraph."),
                               ("archive", "An item & its notes. More.")):
                with self.subTest(browser=browser, link=link):
                    abstract = self.shown(page, link).locator(".popup-abstract")
                    self.assertEqual(self.text(abstract), want)

    def test_hostile_answers_stay_inert(self) -> None:
        evil = '<img src=x onerror="window.__pwned=1">Evil'
        hostile = {
            "wikipedia": answer({"query": {"pages": {"1": {
                "title": evil, "extract": "<p>Fine <script>window.__pwned=2</script>text</p>",
                "thumbnail": {"source": "javascript:window.__pwned=3", "width": 1, "height": 1}}}}}),
            "doi": answer({"message": {"title": ['"><svg onload="window.__pwned=4">'],
                                       "abstract": '<img src=x onerror="window.__pwned=5">Words'}}),
            "github": answer({"full_name": "o/r", "description": "</div><script>window.__pwned=6</script>"}),
            "arxiv-html": answer(ARXIV_HTML.replace('src="2401.01234v1/x1.png"',
                                                    'src="https://evil.example/x.png"'), "text/html"),
            "wikimedia-image": None,
        }
        for browser in BROWSERS:
            page, net = self.fixture(browser, **hostile)
            for link, title in (("wikipedia", evil), ("doi", '"><svg onload="window.__pwned=4">'),
                                ("github", "o/r"), ("arxiv", "A Fake Paper on Graphs")):
                with self.subTest(browser=browser, link=link):
                    popup = self.shown(page, link)
                    self.assertEqual(popup.locator(".popup-title").inner_text().strip(), title)
                    self.expect(popup.locator("img, svg, script, iframe")).to_have_count(0)
            with self.subTest(browser=browser, part="text kept, markup dropped"):
                self.assertEqual(self.text(self.shown(page, "doi").locator(".popup-abstract")), "Words")
                self.assertIsNone(page.evaluate("window.__pwned"))
                self.assertEqual(net.hits["wikimedia-image"], 0)

    def test_arxiv_lead_figure(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser, figure="on time"):
                page, net = self.fixture(browser)
                banner = self.shown(page, "arxiv").locator("img.popup-image-banner.is-figure")
                self.expect(banner).to_have_attribute("src", "/proxy/arxiv-html/2401.01234v1/x1.png")
                self.expect(banner).to_have_js_property("complete", True)
                self.assertGreater(banner.evaluate("i => i.naturalWidth"), 0)
            with self.subTest(browser=browser, figure="none"):
                page, net = self.fixture(browser, **{"arxiv-html": answer("gone", "text/html", 404)})
                popup = self.shown(page, "arxiv")
                self.expect(popup.locator(".popup-arxiv .popup-title")).to_have_text("A Fake Paper on Graphs")
                self.expect(popup.locator("img")).to_have_count(0)
            with self.subTest(browser=browser, figure="late"):
                held = []
                page, net = self.fixture(browser, **{"arxiv-html": held.append})
                popup = self.shown(page, "arxiv")
                # Past enrich's 1.8 s allowance the popup shows without it...
                self.expect(popup.locator(".popup-arxiv .popup-title")).to_have_text("A Fake Paper on Graphs")
                self.expect(popup.locator("img")).to_have_count(0)
                held[0].fulfill(**ANSWERS["arxiv-html"])
                page.wait_for_timeout(500)
                # ...and the next hover has it, from the cache.
                banner = self.shown(page, "arxiv").locator("img.popup-image-banner.is-figure")
                self.expect(banner).to_have_count(1)
                self.assertEqual((net.hits["arxiv"], net.hits["arxiv-html"]), (1, 1))

    def test_escape_dismisses(self) -> None:
        # Audit A03 (WCAG 1.4.13): Escape hides the popup at once and keeps
        # it away until the pointer or focus comes to a target anew.
        for browser in BROWSERS:
            page, _ = self.fixture(browser)
            visible = page.locator(".link-popup.is-visible")
            with self.subTest(browser=browser, how="hover"):
                self.shown(page, "github")
                page.keyboard.press("Escape")
                self.expect(visible).to_have_count(0)
                page.wait_for_timeout(700)   # still over the link: stays away
                self.expect(visible).to_have_count(0)
                self.shown(page, "github")
            with self.subTest(browser=browser, how="pending"):
                self.away(page)
                page.hover("#doi")
                page.keyboard.press("Escape")   # inside the 250 ms show delay
                page.wait_for_timeout(800)
                self.expect(visible).to_have_count(0)
            with self.subTest(browser=browser, how="focus"):
                self.away(page)
                page.focus("#youtube")
                self.expect(visible.locator(".popup-youtube")).to_have_count(1)
                page.keyboard.press("Escape")
                self.expect(visible).to_have_count(0)


if __name__ == "__main__":
    unittest.main()
