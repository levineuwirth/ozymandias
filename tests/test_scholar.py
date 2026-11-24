"""Citation tags for scholarly papers (build/Scholar.hs).

A page with `scholarly: true` carries Google Scholar's citation_* tags, or
the site does not build: `site build --dry-run` evaluates the rules, where
every flagged page is read, its PDF placed and checked, and compiles
nothing. Full builds of a scratch site check the tags, the PDF published
beside its page (the only place Scholar takes citation_pdf_url from), the
page's links, and the one reading of `noindex:` the head, the sitemap and
the check share.

    python -m unittest tests.test_scholar -v
"""

from __future__ import annotations

import json
import subprocess
import tempfile
import unittest
from pathlib import Path

from bs4 import BeautifulSoup

from tests._helpers import engine_path, requires_cabal, script_env, site_binary, write_site_config

PAPER = """---
title: "A <em>Sharp</em> Bound & Its Limits"
date: 2026-07-22
authors:
  - "Ada Example | /me.html"
  - "Bo Second"
abstract: >
  We prove a bound.
preprint: /papers/bound.pdf
scholarly: true
citation-institution: ["First University", ""]
citation-doi: "10.1234/example.5678"
citation-arxiv: "2611.01230v2"
---

Body.
"""

BESIDE = """---
title: Beside
date: 2026-11-17
authors: ["Ada Example"]
affiliation: ["Only University | https://example.edu"]
preprint: /essays/beside/main.pdf
scholarly: true
citation-date: 2026-11-13
---

Body.
"""

PLAIN = """---
title: Not a paper
date: 2026-01-02
authors: ["Ada Example"]
preprint: /papers/bound.pdf
---

Body.
"""


def page(title: str, pdf: str, extra: str = "") -> str:
    return (f"---\ntitle: {json.dumps(title)}\ndate: 2026-03-04\nauthors: [\"Ada Example\"]\n"
            f"preprint: {json.dumps(pdf)}\nscholarly: true\n{extra}---\n\nBody.\n")


def citation(html: str) -> list[tuple[str, str]]:
    head = BeautifulSoup(html, "html.parser").head
    return [(m["name"], m["content"]) for m in head.find_all("meta")
            if m.get("name", "").startswith("citation_")]


@requires_cabal
class ScholarlyPages(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory(prefix="scholar-")
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        (self.root / "engine").symlink_to(engine_path(""))
        for d in ("content/essays/beside", "content/blog", "static/papers", "data"):
            (self.root / d).mkdir(parents=True, exist_ok=True)
        (self.root / "content/index.md").write_text("---\ntitle: Home\n---\n\nHome.\n", encoding="utf-8")
        self.pdf("static/papers/bound.pdf", b"bound")
        self.pdf("content/essays/beside/main.pdf", b"beside")
        self.write("content/essays/paper.md", PAPER)
        self.write("content/essays/beside/index.md", BESIDE)
        self.write("content/essays/plain.md", PLAIN)
        self.write("content/essays/hidden.md", PLAIN.replace("title: Not a paper", "title: Hidden\nnoindex: true"))
        self.configure()

    def configure(self, **overrides) -> None:
        config = write_site_config(self.root, **overrides)
        (self.root / "site.yaml").write_text(json.dumps(config), encoding="utf-8")

    def write(self, path: str, text: str) -> None:
        (self.root / path).parent.mkdir(parents=True, exist_ok=True)
        (self.root / path).write_text(text, encoding="utf-8")

    def pdf(self, path: str, marker: bytes) -> None:
        (self.root / path).parent.mkdir(parents=True, exist_ok=True)
        (self.root / path).write_bytes(b"%PDF-1.4\n%" + marker + b"\n")

    def site(self, *args: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run([str(site_binary()), *args], cwd=self.root,
                              env=script_env(SITE_ENV="build", SITE_THREADS="1"),
                              capture_output=True, text=True, timeout=600)

    def refused(self, *needles: str) -> None:
        done = self.site("build", "--dry-run")
        out = done.stdout + done.stderr
        self.assertNotEqual(done.returncode, 0, out[-3000:])
        for needle in needles:
            self.assertIn(needle, out)

    def built(self) -> Path:
        done = self.site("build")
        self.assertEqual(done.returncode, 0, done.stdout[-3000:] + done.stderr)
        return self.root / "_site"

    # -- Refusals ------------------------------------------------------------

    def test_a_flagged_page_missing_a_field_is_refused(self):
        self.write("content/essays/paper.md", PAPER.replace('authors:\n  - "Ada Example | /me.html"\n  - "Bo Second"\n', "")
                   .replace('citation-institution: ["First University", ""]\n', ""))
        self.refused("content/essays/paper.md", "no authors")

    def test_a_pdf_that_is_not_there_is_refused(self):
        self.write("content/essays/paper.md", PAPER.replace("/papers/bound.pdf", "/papers/gone.pdf"))
        self.refused("content/essays/paper.md", "is no file", "static/papers/gone.pdf")

    def test_a_pdf_the_site_does_not_publish_is_refused(self):
        # A refused name: the file is there, and nothing serves it.
        self.pdf("content/essays/beside/main.draft.pdf", b"draft")
        self.write("content/essays/beside/index.md", BESIDE.replace("main.pdf", "main.draft.pdf"))
        self.refused("content/essays/beside/index.md", "main.draft.pdf is never published")

    def test_bad_values_are_refused(self):
        # (An unparseable date: fails the whole build before this check.)
        self.write("content/essays/paper.md", PAPER.replace("scholarly: true\n", "scholarly: true\ncitation-date: 2026-13-01\n")
                   .replace('"10.1234/example.5678"', '"https://doi.org/10.1234/x"')
                   .replace('"2611.01230v2"', '"arXiv:2611"'))
        self.refused("citation date", "not a bare DOI", "not an arXiv identifier")

    def test_an_unquoted_arxiv_identifier_is_refused(self):
        # YAML reads it as 2611.0123: the identifier's last zero is gone.
        self.write("content/essays/paper.md", PAPER.replace('"2611.01230v2"', "2611.01230"))
        self.refused("citation-arxiv is a number to YAML", "quote it")

    def test_an_empty_title_is_refused(self):
        self.write("content/essays/paper.md", PAPER.replace("scholarly: true\n", "scholarly: true\ncitation-title: \"<em></em>\"\n"))
        self.refused("the title is empty")

    def test_institutions_must_go_author_by_author(self):
        self.write("content/essays/paper.md", PAPER.replace('["First University", ""]', '["A", "B", "C"]'))
        self.refused("citation-institution: 3 for 2 authors")

    def test_a_flag_outside_the_essays_is_refused(self):
        self.write("content/blog/post.md", "---\ntitle: Post\ndate: 2026-01-02\nscholarly: true\n---\n\nBody.\n")
        self.refused("content/blog/post.md", "essays only")

    def test_a_flagged_draft_is_not_asked_for_anything(self):
        # Withheld: it is no page of the site's, essay or not.
        self.write("content/essays/unready.md", page("Unready", "/papers/gone.pdf", "draft: true\n"))
        site = self.built()
        self.assertFalse((site / "essays/unready.html").exists())

    # -- What a build publishes ----------------------------------------------

    def test_tags_pdf_and_links(self):
        site = self.built()
        html = (site / "essays/paper.html").read_text(encoding="utf-8")
        self.assertEqual(citation(html), [
            ("citation_title", "A Sharp Bound & Its Limits"),
            ("citation_author", "Ada Example"),
            ("citation_author_institution", "First University"),
            ("citation_author", "Bo Second"),
            ("citation_publication_date", "2026/07/22"),
            ("citation_doi", "10.1234/example.5678"),
            ("citation_pdf_url", "https://example.org/essays/paper.pdf"),
        ])
        # The PDF is published again beside its page, named for it, byte for byte.
        self.assertEqual((site / "essays/paper.pdf").read_bytes(), (site / "papers/bound.pdf").read_bytes())
        self.assertIn('paper.pdf">Preprint</a>', html)
        self.assertNotIn('papers/bound.pdf">Preprint', html)
        self.assertIn('href="https://arxiv.org/abs/2611.01230v2">arXiv</a>', html)
        # One set, and the other head tags kept.
        self.assertEqual(html.count('name="citation_title"'), 1)
        self.assertIn('property="og:title"', html)

        # A PDF already beside its page is named where it is; one author
        # takes the page's affiliation.
        tags = dict(citation((site / "essays/beside/index.html").read_text(encoding="utf-8")))
        self.assertEqual(tags["citation_pdf_url"], "https://example.org/essays/beside/main.pdf")
        self.assertEqual(tags["citation_publication_date"], "2026/11/13")
        self.assertEqual(tags["citation_author_institution"], "Only University")
        self.assertNotIn("arXiv</a>", (site / "essays/beside/index.html").read_text(encoding="utf-8"))

        # An unflagged page with a preprint carries none, and keeps its link.
        plain = (site / "essays/plain.html").read_text(encoding="utf-8")
        self.assertNotIn("citation_", plain)
        self.assertIn('bound.pdf">Preprint</a>', plain)

    def test_noindex_is_read_one_way(self):
        # noindex: false is no request: a paper keeps its tags, its sitemap
        # entry and no robots meta. noindex: true has the meta and no entry.
        self.write("content/essays/paper.md", PAPER.replace("scholarly: true\n", "scholarly: true\nnoindex: false\n"))
        site = self.built()
        paper = (site / "essays/paper.html").read_text(encoding="utf-8")
        hidden = (site / "essays/hidden.html").read_text(encoding="utf-8")
        sitemap = (site / "sitemap.xml").read_text(encoding="utf-8")
        self.assertTrue(citation(paper))
        self.assertNotIn('name="robots"', paper)
        self.assertIn("<loc>https://example.org/essays/paper.html</loc>", sitemap)
        self.assertIn('<meta name="robots" content="noindex">', hidden)
        self.assertNotIn("essays/hidden", sitemap)
        self.assertIn("<loc>https://example.org/essays/plain.html</loc>", sitemap)

    def test_pdfs_beside_and_copies_never_collide(self):
        # A static file already served beside the page is used where it is;
        # two pages with PDFs of one name, from two places, get two copies.
        self.pdf("static/essays/bound.pdf", b"static-beside")
        self.pdf("static/other/bound.pdf", b"other")
        self.write("content/essays/near.md", page("Near", "/essays/bound.pdf"))
        self.write("content/essays/first.md", page("First", "/papers/bound.pdf"))
        self.write("content/essays/second.md", page("Second", "/other/bound.pdf"))
        # A file of the name a copy would take: the copy takes another.
        self.pdf("content/essays/third.pdf", b"someone else's")
        self.write("content/essays/third.md", page("Third", "/papers/bound.pdf"))
        site = self.built()
        url = lambda p: dict(citation((site / p).read_text(encoding="utf-8")))["citation_pdf_url"]
        self.assertEqual(url("essays/near.html"), "https://example.org/essays/bound.pdf")
        self.assertIn(b"static-beside", (site / "essays/bound.pdf").read_bytes())
        self.assertEqual(url("essays/first.html"), "https://example.org/essays/first.pdf")
        self.assertIn(b"%bound", (site / "essays/first.pdf").read_bytes())
        self.assertEqual(url("essays/second.html"), "https://example.org/essays/second.pdf")
        self.assertIn(b"%other", (site / "essays/second.pdf").read_bytes())
        self.assertEqual(url("essays/third.html"), "https://example.org/essays/third-bound.pdf")
        self.assertIn(b"%bound", (site / "essays/third-bound.pdf").read_bytes())
        self.assertIn(b"someone else's", (site / "essays/third.pdf").read_bytes())

    def test_a_file_served_where_a_copy_went_moves_the_copy(self):
        # Built, then a different file comes to be served at the copy's
        # address: the next build, incremental, moves the copy, and the
        # page names where it went.
        site = self.built()
        self.assertEqual(dict(citation((site / "essays/paper.html").read_text(encoding="utf-8")))["citation_pdf_url"],
                         "https://example.org/essays/paper.pdf")
        self.pdf("static/essays/paper.pdf", b"unrelated")
        site = self.built()
        html = (site / "essays/paper.html").read_text(encoding="utf-8")
        self.assertEqual(dict(citation(html))["citation_pdf_url"], "https://example.org/essays/paper-bound.pdf")
        self.assertIn('paper-bound.pdf">Preprint</a>', html)
        self.assertIn(b"%bound", (site / "essays/paper-bound.pdf").read_bytes())
        self.assertIn(b"%unrelated", (site / "essays/paper.pdf").read_bytes())

        # Then a page comes whose own name that is. It takes it, and the
        # first paper's copy moves on, built incrementally as on a clean
        # build.
        self.pdf("static/papers/other.pdf", b"other")
        self.write("content/essays/paper-bound.md", page("Paper Bound", "/papers/other.pdf"))
        url = lambda p: dict(citation((site / p).read_text(encoding="utf-8")))["citation_pdf_url"]
        for clean in (False, True):
            with self.subTest(clean=clean):
                if clean:
                    self.assertEqual(self.site("clean").returncode, 0)
                site = self.built()
                self.assertEqual(url("essays/paper-bound.html"), "https://example.org/essays/paper-bound.pdf")
                self.assertIn(b"%other", (site / "essays/paper-bound.pdf").read_bytes())
                self.assertEqual(url("essays/paper.html"), "https://example.org/essays/paper-2.pdf")
                self.assertIn(b"%bound", (site / "essays/paper-2.pdf").read_bytes())

    def test_a_directory_page_and_a_page_beside_it_take_two_names(self):
        # The directory page's PDF is called what the page beside it is: the
        # page beside it keeps its own name, on a clean build and when it
        # comes after the directory page has built.
        self.pdf("static/papers/chapter.pdf", b"book")
        self.pdf("static/papers/other.pdf", b"chapter")
        self.write("content/essays/book/index.md", page("Book", "/papers/chapter.pdf"))
        site = self.built()
        url = lambda p: dict(citation((site / p).read_text(encoding="utf-8")))["citation_pdf_url"]
        self.assertEqual(url("essays/book/index.html"), "https://example.org/essays/book/chapter.pdf")
        self.write("content/essays/book/chapter.md", page("Chapter", "/papers/other.pdf"))
        for clean in (False, True):
            with self.subTest(clean=clean):
                if clean:
                    self.assertEqual(self.site("clean").returncode, 0)
                site = self.built()
                self.assertEqual(url("essays/book/chapter.html"), "https://example.org/essays/book/chapter.pdf")
                self.assertIn(b"%chapter", (site / "essays/book/chapter.pdf").read_bytes())
                self.assertEqual(url("essays/book/index.html"), "https://example.org/essays/book/index-chapter.pdf")
                self.assertIn(b"%book", (site / "essays/book/index-chapter.pdf").read_bytes())

    def test_the_numbered_names_do_not_run_out(self):
        for name in ["paper.pdf", "paper-bound.pdf"] + [f"paper-{n}.pdf" for n in range(2, 10)]:
            self.pdf(f"static/essays/{name}", b"taken")
        site = self.built()
        self.assertEqual(dict(citation((site / "essays/paper.html").read_text(encoding="utf-8")))["citation_pdf_url"],
                         "https://example.org/essays/paper-10.pdf")
        self.assertIn(b"%bound", (site / "essays/paper-10.pdf").read_bytes())

    def test_titles_names_and_dates_as_scholar_reads_them(self):
        self.pdf("content/essays/odd/bound#1.pdf", b"hash")
        self.write("content/essays/odd/index.md",
                   page("Is a < b? <em>Yes</em>, &lt;b&gt; &amp; more", "bound#1.pdf", "citation-date: 2026-11\n"))
        site = self.built()
        html = (site / "essays/odd/index.html").read_text(encoding="utf-8")
        tags = dict(citation(html))
        self.assertEqual(tags["citation_title"], "Is a < b? Yes, <b> & more")
        self.assertIn('content="Is a &lt; b? Yes, &lt;b&gt; &amp; more"', html)
        # A month alone: Scholar takes a full date or the year.
        self.assertEqual(tags["citation_publication_date"], "2026")
        self.assertEqual(tags["citation_pdf_url"], "https://example.org/essays/odd/bound%231.pdf")
        self.assertIn('bound%231.pdf">Preprint</a>', html)
        self.assertTrue((site / "essays/odd/bound#1.pdf").exists())

        # The Preprint link opens the file the tag names: &copy; in a name
        # is a path there, not a character reference.
        self.pdf("content/essays/amp/bound&copy;.pdf", b"amp")
        self.write("content/essays/amp/index.md", page("Amp", "bound&copy;.pdf"))
        site = self.built()
        soup = BeautifulSoup((site / "essays/amp/index.html").read_text(encoding="utf-8"), "html.parser")
        href = next(a["href"] for a in soup.find_all("a") if a.get_text() == "Preprint")
        self.assertTrue(href.endswith("essays/amp/bound&copy;.pdf"), href)
        self.assertEqual(dict(citation(str(soup)))["citation_pdf_url"], "https://example.org/essays/amp/bound&copy;.pdf")

        # Named references as a browser reads them, the visible title's.
        self.write("content/essays/odd/index.md",
                   page("Caf&eacute; &ndash; A &le; B &amp;lt; &bogus;", "bound#1.pdf"))
        site = self.built()
        tags = dict(citation((site / "essays/odd/index.html").read_text(encoding="utf-8")))
        self.assertEqual(tags["citation_title"], "Café – A ≤ B &lt; &bogus;")

    def test_an_http_site_is_taken(self):
        # Scholar's own example citation_pdf_url is an http:// one.
        self.configure(url="http://example.org")
        site = self.built()
        tags = dict(citation((site / "essays/paper.html").read_text(encoding="utf-8")))
        self.assertEqual(tags["citation_pdf_url"], "http://example.org/essays/paper.pdf")

    def test_the_scholar_icon_is_the_engine_s(self):
        # The filter gives every site's Scholar links the icon; its mask
        # must come with the engine, not with one site's stylesheet.
        css = engine_path("static/css/typography.css").read_text(encoding="utf-8")
        self.assertIn("a[data-link-icon=\"google-scholar\"]::after", css)
        self.assertTrue(engine_path("static/images/link-icons/google-scholar.svg").is_file())


if __name__ == "__main__":
    unittest.main()
