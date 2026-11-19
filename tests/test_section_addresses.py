"""A site lists the sections of writing it has and says where each is
served (site.yaml's sections: dir, address, title).

Whole builds of small scratch sites against this engine:

  · a section the site does not list publishes nothing, and a file in its
    directory stops the build;
  · a section's pages, its collections, the files beside them and its
    landing are at its address, and the feed, the New page, wikilinks and
    tags follow;
  · the blog's landing is there before its first post.

    python -m unittest tests.test_section_addresses -v
"""

from __future__ import annotations

import json
import re
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import engine_path, requires_cabal, script_env, site_binary, write_site_config

NOTEBOOK = {"dir": "blog", "address": "notebook", "title": "Notebook", "kind": "Note", "dingbat": None}
WRITING = {"dir": "essays", "address": "writing", "title": "Writing & Such", "kind": "Piece", "dingbat": None}
FEED = {"path": "feed.xml", "title": "Notes", "description": "Notes.", "sections": ["blog"],
        "advertise": True, "limit": 30}


def page(path: Path, front: str, body: str = "Body.") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(f"---\n{front}\n---\n\n{body}\n", encoding="utf-8")


@requires_cabal
class SectionAddresses(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory(prefix="section-addresses-")
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        (self.root / "engine").symlink_to(engine_path(""))
        page(self.root / "content/index.md", "title: Home\nhome: true", "Home.")

    def configure(self, **settings) -> None:
        config = write_site_config(self.root, **settings)
        (self.root / "site.yaml").write_text(json.dumps(config), encoding="utf-8")

    def site(self, *args: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run([str(site_binary()), *args], cwd=self.root, env=script_env(),
                              capture_output=True, text=True, timeout=240)

    def built(self) -> set[str]:
        done = self.site("rebuild")
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        out = self.root / "_site"
        return {p.relative_to(out).as_posix() for p in out.rglob("*") if p.is_file()}

    def read(self, path: str) -> str:
        return (self.root / "_site" / path).read_text(encoding="utf-8")

    def test_a_site_with_one_section_publishes_that_one_and_its_landing_before_any_entry(self):
        self.configure(sections=[NOTEBOOK], feeds=[FEED])
        files = self.built()
        pages = {f for f in files if f.endswith(".html")}
        # No essays/, poetry/ or fiction/, and nothing at the directory's name.
        self.assertEqual({f for f in pages if "/" in f and not f.startswith(("authors/", "assets/"))},
                         {"notebook/index.html"})
        landing = self.read("notebook/index.html")
        self.assertIn("<title>Notebook — Example Site</title>", landing)
        self.assertRegex(landing, r"<h1[^>]*>Notebook</h1>")
        self.assertIn("<feed", self.read("feed.xml"))
        self.assertNotIn("<entry>", self.read("feed.xml"))

    def test_a_section_s_pages_are_at_its_address(self):
        root = self.root
        page(root / "content/blog/first.md", "title: First\ndate: 2026-01-02",
             "See [[Notebook]] and [[Writing]].")
        page(root / "content/blog/cycle/index.md", "title: Cycle")
        page(root / "content/blog/cycle/one.md", "title: One\ndate: 2026-01-03")
        page(root / "content/essays/long/index.md", "title: Long\ndate: 2026-01-04")
        (root / "content/essays/long/data.csv").write_text("a,b\n", encoding="utf-8")
        (root / "content/essays/long/page.js").write_text("// a page's own script\n", encoding="utf-8")
        page(root / "content/essays/flat.md", "title: Flat\ndate: 2026-01-05\ntags: [notebook, blog]")
        self.configure(sections=[NOTEBOOK, WRITING], feeds=[FEED], switches={"tag-pages": True, "new": True})
        files = self.built()
        for path in ("notebook/index.html", "notebook/first.html", "notebook/cycle/index.html",
                     "notebook/cycle/one.html", "writing/index.html", "writing/long/index.html",
                     "writing/long/data.csv", "writing/long/page.js", "writing/flat.html"):
            self.assertIn(path, files)
        # Nothing is left at the directories' names but a tag's page: an
        # essay's tag `blog` is a tag like any other here, and its tag
        # `notebook` has no page, since the landing is there.
        self.assertEqual(sorted(f for f in files if f.startswith(("blog/", "essays/"))), ["blog/index.html"])
        self.assertIn("Flat", self.read("blog/index.html"))
        landing = self.read("notebook/index.html")
        self.assertRegex(landing, r"<h1[^>]*>Notebook</h1>")
        self.assertNotIn("Flat", landing)
        self.assertIn('<a href="../notebook/first.html">First</a>', landing)
        self.assertIn('<a href="../notebook/cycle/one.html">One</a>', landing)
        writing = self.read("writing/index.html")
        self.assertIn("<title>Writing &amp; Such — Example Site</title>", writing)
        self.assertIn('<a href="../writing/long/">Long</a>', writing)
        # The feed, the New page's labels, canonical addresses and the
        # sitemap follow the routes.
        feed = self.read("feed.xml")
        self.assertIn("https://example.org/notebook/first.html", feed)
        self.assertNotIn("example.org/blog/", feed)
        new = self.read("new.html")
        self.assertRegex(new, r'(?s)<span class="item-card-kind">Note</span>')
        self.assertRegex(new, r'(?s)<span class="item-card-kind">Piece</span>')
        self.assertIn('<link rel="canonical" href="https://example.org/writing/long/">',
                      self.read("writing/long/index.html"))
        sitemap = self.read("sitemap.xml")
        self.assertIn("<loc>https://example.org/writing/</loc>", sitemap)
        self.assertIn("<loc>https://example.org/notebook/first.html</loc>", sitemap)
        self.assertNotRegex(sitemap, r"example\.org/(essays|blog)/")
        # A wikilink to a section is to its landing, by its address.
        first = self.read("notebook/first.html")
        self.assertRegex(first, r'<a href="\.\./notebook/"[^>]*>Notebook</a>')
        self.assertRegex(first, r'<a href="\.\./writing/"[^>]*>Writing</a>')

    def test_a_file_of_a_section_the_site_does_not_list_stops_the_build(self):
        page(self.root / "content/poetry/stray.md", "title: Stray\ndate: 2026-01-02")
        self.configure(sections=[NOTEBOOK], feeds=[])
        done = self.site("rebuild")
        self.assertNotEqual(done.returncode, 0)
        self.assertIn("sections lists no poetry, and content/ holds its files: content/poetry/stray.md",
                      done.stdout + done.stderr)
        # Nor is its directory a page collection's.
        rules = json.loads(self.site("shared-rules").stdout)
        self.assertLessEqual({"essays", "blog", "poetry", "fiction", "notebook"}, set(rules["reserved-sections"]))

    def test_a_page_collection_cannot_take_a_section_s_address(self):
        page(self.root / "content/notebook/a.md", "title: A")
        self.configure(sections=[NOTEBOOK], feeds=[])
        done = self.site("rebuild")
        self.assertNotEqual(done.returncode, 0)
        self.assertIn("/notebook/ is the blog section's address, and its pages are written in content/blog/: "
                      "content/notebook/a.md", done.stdout + done.stderr)


if __name__ == "__main__":
    unittest.main()
