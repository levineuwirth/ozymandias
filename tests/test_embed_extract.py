"""tools/embed.py: what a built page contributes to Related links and
semantic search, and the vector cache's model pinning (audit T14).
No model is loaded."""

import json
import shutil
import tempfile
import unittest
from pathlib import Path
from unittest import mock
from tests._helpers import load_tool


try:
    embed = load_tool("embed.py", "embed_tool")
except ImportError as exc:          # faiss, numpy or bs4 missing: no .venv
    embed = None
    MISSING = str(exc)

PROSE = "A sentence about random regular graphs and their domination numbers. " * 3


def page(body, title="Essay — Levi Neuwirth", portal=False):
    attr = " data-portal" if portal else ""
    return (f"<html><head><title>{title}</title></head><body{attr}>"
            f"<nav>Home Library</nav><main id=\"markdownBody\">{body}</main>"
            f"<footer>footer text</footer></body></html>")


@unittest.skipIf(embed is None, "embed.py's dependencies are not installed")
class ExtractTests(unittest.TestCase):
    def setUp(self):
        self.site = Path(tempfile.mkdtemp(prefix="embed-"))
        self.addCleanup(shutil.rmtree, self.site)
        patcher = mock.patch.object(embed, "SITE_DIR", self.site)
        patcher.start()
        self.addCleanup(patcher.stop)

    def write(self, rel, html):
        p = self.site / rel
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(html, encoding="utf-8")
        return p

    def test_urls_from_paths(self):
        self.assertEqual(embed._url_from_path(self.site / "index.html"), "/")
        self.assertEqual(embed._url_from_path(self.site / "essays/x/index.html"), "/essays/x/")
        self.assertEqual(embed._url_from_path(self.site / "about.html"), "/about.html")

    def test_paragraphs_carry_their_section_heading(self):
        p = self.write("essays/a.html", page(f"<h1>Domination</h1><p>{PROSE}</p><h2>Proofs</h2><p>{PROSE}</p><p>Too short.</p>"))
        doc, paras = embed.extract_document(p)
        self.assertEqual(doc["title"], "Domination")
        self.assertEqual([x["heading"] for x in paras], ["Domination", "Proofs"])
        self.assertTrue(all(x["url"] == "/essays/a.html" for x in paras))

    def test_title_falls_back_to_the_title_tag_before_the_dash(self):
        doc, _ = embed.extract_document(self.write("b.html", page(f"<p>{PROSE}</p>")))
        self.assertEqual(doc["title"], "Essay")

    def test_chrome_and_duplicated_text_are_stripped(self):
        body = (f"<p>{PROSE}</p><nav id=\"toc\">Contents list</nav>"
                "<div class=\"page-meta-footer\">Related: other pages</div>")
        doc, paras = embed.extract_document(self.write("c.html", page(body)))
        for gone in ("Contents list", "Related: other"):
            self.assertNotIn(gone, doc["text"])
        self.assertEqual(len(paras), 1)

    def test_notes_are_passages_once(self):
        # The notes list is each note's only copy: its paragraphs are
        # indexed, once each, under "Notes" rather than the last section.
        note = "A note's own paragraph, long enough to count as a passage. " * 3
        body = (f"<h2>Last section</h2><p>{PROSE}</p>"
                "<section class=\"footnotes\"><ol class=\"footnotes-list\">"
                "<li id=\"fn-a\" class=\"footnote-item\"><span class=\"footnote-label\">a</span>"
                f"<p>{note}</p><ul><li>{note}</li></ul></li></ol></section>")
        doc, paras = embed.extract_document(self.write("n.html", page(body)))
        self.assertIn("A note's own paragraph", doc["text"])
        notes = [x for x in paras if x["text"].startswith("A note's")]
        self.assertEqual(len(notes), 2)
        self.assertEqual({x["heading"] for x in notes}, {"Notes"})

    def test_long_paragraphs_are_truncated_and_excerpted(self):
        long = "word " * 400
        _, paras = embed.extract_document(self.write("d.html", page(f"<p>{long}</p>")))
        self.assertEqual(len(paras[0]["text"]), embed.MAX_PARA_CHARS)
        self.assertTrue(paras[0]["excerpt"].endswith("…"))

    def test_excluded_pages_contribute_nothing(self):
        cases = {
            "search/index.html": page(f"<p>{PROSE}</p>"),
            "source/README.md.html": page(f"<p>{PROSE}</p>"),
            "drafts/x.html": page(f"<p>{PROSE}</p>"),
            "portal.html": page(f"<p>{PROSE}</p>", portal=True),
            "nobody.html": f"<html><body><p>{PROSE}</p></body></html>",
            "thin.html": page("<p>Hardly anything.</p>"),
        }
        for rel, html in cases.items():
            with self.subTest(rel=rel):
                self.assertEqual(embed.extract_document(self.write(rel, html)), (None, []))


@unittest.skipIf(embed is None, "embed.py's dependencies are not installed")
class VectorCacheTests(unittest.TestCase):
    def setUp(self):
        self.dir = Path(tempfile.mkdtemp(prefix="embed-cache-"))
        self.addCleanup(shutil.rmtree, self.dir)
        self.path = self.dir / "cache.npz"
        import numpy as np
        self.np = np
        self.vec = {"h1": np.ones(4, dtype=np.float32), "h2": np.zeros(4, dtype=np.float32)}

    def test_round_trip(self):
        embed.save_vec_cache(self.path, "m", "r1", 4, self.vec)
        back = embed.load_vec_cache(self.path, "m", "r1", 4)
        self.assertEqual(set(back), {"h1", "h2"})
        self.assertTrue(self.np.array_equal(back["h1"], self.vec["h1"]))

    def test_another_model_revision_or_dimension_discards_it(self):
        embed.save_vec_cache(self.path, "m", "r1", 4, self.vec)
        self.assertEqual(embed.load_vec_cache(self.path, "other", "r1", 4), {})
        self.assertEqual(embed.load_vec_cache(self.path, "m", "r2", 4), {})
        self.assertEqual(embed.load_vec_cache(self.path, "m", "r1", 8), {})

    def test_a_corrupt_or_missing_cache_is_empty(self):
        self.assertEqual(embed.load_vec_cache(self.path, "m", "r1", 4), {})
        self.path.write_bytes(b"not a zip")
        with mock.patch("sys.stderr"):
            self.assertEqual(embed.load_vec_cache(self.path, "m", "r1", 4), {})

    def test_an_empty_cache_round_trips(self):
        embed.save_vec_cache(self.path, "m", "r1", 4, {})
        self.assertEqual(embed.load_vec_cache(self.path, "m", "r1", 4), {})
        self.assertEqual([p.name for p in self.dir.iterdir()], ["cache.npz"], "no temp file left behind")


def stamped(body, when="Monday, October 6th, 2026 13:03:33"):
    """A page with a footer build time in it, as pages were stamped until 2026-10-06."""
    return page(body).replace(
        "<footer>footer text</footer>",
        f'<footer><span class="footer-build-time" data-build-time>{when}</span></footer>')


@unittest.skipIf(embed is None, "embed.py's dependencies are not installed")
class ExtractCacheTests(unittest.TestCase):
    """Parsing every page was nearly all of a no-change run; extract_site
    parses only pages whose bytes (footer build time aside) or extractor
    changed, and must return exactly what extract_document would."""

    def setUp(self):
        self.site = Path(tempfile.mkdtemp(prefix="embed-"))
        self.addCleanup(shutil.rmtree, self.site)
        patcher = mock.patch.object(embed, "SITE_DIR", self.site)
        patcher.start()
        self.addCleanup(patcher.stop)
        self.cache = self.site.parent / (self.site.name + "-extract.json")
        self.addCleanup(self.cache.unlink, missing_ok=True)
        self.write("essays/a.html", stamped(f"<h1>Alpha</h1><p>{PROSE}</p>"))
        self.write("essays/b.html", stamped(f"<h1>Beta</h1><p>{PROSE} Beta.</p>"))
        self.write("404.html", stamped(f"<p>{PROSE}</p>"))           # excluded by URL

    def write(self, rel, html):
        p = self.site / rel
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(html, encoding="utf-8")
        return p

    def direct(self):
        pages, paras = [], []
        for html in sorted(self.site.rglob("*.html")):
            p, q = embed.extract_document(html)
            if p is not None:
                pages.append(p)
                paras.extend(q)
        return pages, paras

    def run_site(self):
        pages, paras, parsed = embed.extract_site(self.cache)
        self.assertEqual((pages, paras), self.direct())
        return parsed

    def test_an_unchanged_site_is_not_parsed_again(self):
        self.assertEqual(self.run_site(), 2)
        with mock.patch.object(embed, "_extract", side_effect=AssertionError("parsed")):
            pages, paras, parsed = embed.extract_site(self.cache)
        self.assertEqual(parsed, 0)
        self.assertEqual((pages, paras), self.direct())

    def test_a_new_build_time_alone_is_not_a_change(self):
        self.run_site()
        self.write("essays/a.html", stamped(f"<h1>Alpha</h1><p>{PROSE}</p>", when="Tuesday"))
        self.assertEqual(self.run_site(), 0)

    def test_a_changed_page_is_parsed_and_a_deleted_one_dropped(self):
        self.run_site()
        self.write("essays/a.html", stamped(f"<h1>Alpha, revised</h1><p>{PROSE}</p>"))
        (self.site / "essays/b.html").unlink()
        self.assertEqual(self.run_site(), 1)
        kept = json.loads(self.cache.read_text())["pages"]
        self.assertEqual(sorted(kept), ["essays/a.html"])

    def test_another_extractor_parses_everything_again(self):
        self.run_site()
        with mock.patch.object(embed, "extractor_fingerprint", return_value="other"):
            self.assertEqual(self.run_site(), 2)

    def test_an_unreadable_cache_is_ignored(self):
        self.cache.write_text("{not json")
        self.assertEqual(self.run_site(), 2)


if __name__ == "__main__":
    unittest.main()
