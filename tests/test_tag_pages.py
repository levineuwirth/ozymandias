"""What the tag pages and the map file, in whole builds of a small scratch
site (Tags.hs, TagMap.hs):

  · a standalone page's tags file it beside the essays, unless it is a
    draft, whose tags make no page and whose title is on no map;
  · a tagged standalone page with neither a date nor a history is listed
    without a date, and the tag's date range is the dated pieces'; one
    with only a revision is listed at it; neither stops a tag too large
    for one page, whose pages keep a day's new pieces ahead of its
    revisions as each page shows them;
  · /map/'s list under the drawing names every tag at any depth and links
    every piece the drawing has;
  · a tag page's photographs keep the order of its sidecar's featured:,
    which leads the first page of a tag too large for one;
  · a tag page is a volume: its title page's ornament follows the portal's
    facet (smaller on a subtag), the tags within are named and numbered,
    and a sidecar may add a featured piece and a gallery.

    python -m unittest tests.test_tag_pages -v
"""

from __future__ import annotations

import json
import re
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import engine_path, requires_cabal, script_env, site_binary, write_site_config

PLAIN = {"template": "page.html", "context": "page", "scores": False, "dingbat": None, "flags": []}


def page(path: Path, front: str, body: str = "Body.") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(f"---\n{front}\n---\n\n{body}\n", encoding="utf-8")


@requires_cabal
class TagPages(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.binary = site_binary()
        tmp = tempfile.TemporaryDirectory(prefix="tag-pages-")
        cls.addClassCleanup(tmp.cleanup)
        cls.root = root = Path(tmp.name)
        (root / "engine").symlink_to(engine_path(""))
        page(root / "content/index.md", "title: Home\nhome: true", "Home.")
        page(root / "content/essays/dated.md",
             "title: A Dated Essay\ndate: 2026-03-02\ntags: [notes, test/nested/leaf]")
        page(root / "content/probe.md", "title: An Undated Page\ntags: [notes]")
        page(root / "content/secret.md",
             "title: Hidden Draft Title\ndate: 2026-04-01\ndraft: true\ntags: [notes, secret/inner]")
        page(root / "content/revised.md",
             "title: A Revised Page\nrevised: 2026-06-01\ntags: [solo]")
        # More than one page holds (Tags.tagPaginationThreshold), one undated.
        for n in range(150):
            page(root / f"content/essays/many-{n:03d}.md",
                 f"title: Many {n:03d}\ndate: 2025-{n % 12 + 1:02d}-{n % 28 + 1:02d}\ntags: [many]")
        page(root / "content/undated-many.md", "title: Undated Among Many\ntags: [many]")
        # The volume's optional parts, set in a sidecar: a featured piece and
        # a gallery (an SVG's proportions come from its viewBox).
        (root / "content/essays").mkdir(parents=True, exist_ok=True)
        (root / "content/essays/wide.svg").write_text(
            '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 300 100"></svg>', encoding="utf-8")
        page(root / "content/tag-meta/notes.md",
             "featured:\n  - content/essays/dated.md\n"
             "gallery:\n  - image: /essays/wide.svg\n    piece: content/essays/dated.md", "")
        # A tag of several pages whose pieces share one day: some new that
        # day, the rest revised then (Contexts.displaySortKey).
        for n in range(76):
            page(root / f"content/essays/day-new-{n:03d}.md",
                 f"title: Day New {n:03d}\ndate: 2026-08-01\ntags: [sameday]")
        for n in range(75):
            page(root / f"content/essays/day-rev-{n:03d}.md",
                 f"title: Day Rev {n:03d}\ndate: 2024-02-01\nrevised: 2026-08-01\ntags: [sameday]")
        config = write_site_config(
            root,
            portals=[{"name": "Notes", "tag": "notes", "facet": "subject"},
                     {"name": "Test", "tag": "test", "facet": "research"}],
            standalone=[PLAIN | {"source": "content/probe.md", "route": "probe.html"},
                        PLAIN | {"source": "content/secret.md", "route": "secret.html"},
                        PLAIN | {"source": "content/revised.md", "route": "revised.html"},
                        PLAIN | {"source": "content/undated-many.md", "route": "undated-many.html"}],
            switches={"tag-map": True})
        (root / "site.yaml").write_text(json.dumps(config), encoding="utf-8")
        # A production build: drafts are withheld (Drafts.hs).
        cls.done = subprocess.run([str(cls.binary), "rebuild"], cwd=root,
                                  env=script_env(SITE_ENV="production", SITE_THREADS="1"),
                                  capture_output=True, text=True, timeout=240)

    def read(self, rel: str) -> str:
        return (self.root / "_site" / rel).read_text(encoding="utf-8")

    def test_the_build_succeeds(self):
        self.assertEqual(self.done.returncode, 0, self.done.stdout[-3000:] + self.done.stderr)

    def test_a_draft_standalone_page_files_nothing(self):
        self.assertEqual(self.done.returncode, 0, self.done.stderr)
        self.assertFalse((self.root / "_site/secret").exists(), "a draft's tag made a page")
        leaked = [p.relative_to(self.root / "_site").as_posix()
                  for p in (self.root / "_site").rglob("*.html")
                  if "Hidden Draft Title" in p.read_text(encoding="utf-8")]
        self.assertEqual(leaked, [])

    def test_an_undated_standalone_page_is_listed_without_a_date(self):
        self.assertEqual(self.done.returncode, 0, self.done.stderr)
        notes = self.read("notes/index.html")
        self.assertIn("An Undated Page", notes)
        self.assertIn("A Dated Essay", notes)
        # Two pieces, of two kinds; the range is the dated one's month.
        self.assertRegex(notes, r'class="tag-facts">Two pieces · March 2026<')
        # The dated piece comes first, under its year; the undated one
        # under "Undated", with no month.
        self.assertLess(notes.index("A Dated Essay"), notes.index("An Undated Page"))
        self.assertEqual(re.findall(r'class="tag-contents-year"><span class="tag-spaced">([^<]*)<', notes), ["2026", "Undated"])
        row = re.search(r"<li>(?:(?!</li>).)*An Undated Page(?:(?!</li>).)*</li>", notes, re.S).group(0)
        self.assertNotIn("tag-contents-date", row)

    def test_a_page_with_only_a_revision_is_listed_at_it(self):
        self.assertEqual(self.done.returncode, 0, self.done.stderr)
        solo = self.read("solo/index.html")
        self.assertIn("A Revised Page", solo)
        row = re.search(r"<li>(?:(?!</li>).)*A Revised Page(?:(?!</li>).)*</li>", solo, re.S).group(0)
        self.assertIn('class="tag-contents-date">June<', row)
        self.assertRegex(solo, r'class="tag-facts">One page · June 2026<')

    def test_an_undated_page_goes_last_in_a_tag_of_several_pages(self):
        self.assertEqual(self.done.returncode, 0, self.done.stderr)
        def cards(rel: str) -> str:
            # The contents only: the page's map names every piece.
            html = self.read(rel)
            start = html.index('class="tag-contents"')
            return html[start:html.index("</section>", start)]
        self.assertNotIn("Undated Among Many", cards("many/index.html"))
        last = cards("many/page/2/index.html")
        self.assertIn("Undated Among Many", last)
        self.assertGreater(last.index("Undated Among Many"), last.rindex("Many "))

    def test_a_day_s_new_pieces_lead_its_revisions_across_pages(self):
        self.assertEqual(self.done.returncode, 0, self.done.stderr)
        titles = []
        for rel in ("sameday/index.html", "sameday/page/2/index.html"):
            html = self.read(rel)
            start = html.index('class="tag-contents"')
            titles += re.findall(r"Day (New|Rev) \d{3}", html[start:html.index("</section>", start)])
        self.assertEqual(titles, ["New"] * 76 + ["Rev"] * 75)

    def test_the_title_page_and_its_optional_parts(self):
        self.assertEqual(self.done.returncode, 0, self.done.stderr)
        notes = self.read("notes/index.html")
        # The ornament follows the portal's facet, and a subtag's is smaller.
        self.assertIn('<div class="tag-volume tag-volume--subject">', notes)
        self.assertIn('<div class="tag-volume tag-volume--research tag-volume--sub">',
                      self.read("test/nested/index.html"))
        # The tags within, by name and number.
        within = re.search(r'<ul class="tag-within-list".*?</ul>', self.read("test/index.html"), re.S).group(0)
        self.assertRegex(within, r'>Nested</a><span class="tag-within-count" aria-label="one essay">1</span>')
        # The sidecar's featured piece, under its heading, and its gallery.
        featured = re.search(r'<section class="tag-featured".*?</section>', notes, re.S).group(0)
        self.assertIn(">Featured<", featured)
        self.assertIn(">A Dated Essay</a>", featured)
        gallery = re.search(r'<section class="tag-gallery".*?</section>', notes, re.S).group(0)
        self.assertRegex(gallery, r'href="[./]*essays/dated\.html" title="A Dated Essay" style="--ar: 3\.0">'
                                  r'<img src="[./]*essays/wide\.svg" alt="A figure from A Dated Essay"')
        # A tag without a sidecar has neither.
        self.assertNotIn("tag-featured", self.read("solo/index.html"))
        self.assertNotIn("tag-gallery", self.read("solo/index.html"))

    def test_the_map_s_list_names_every_tag_and_links_every_piece(self):
        self.assertEqual(self.done.returncode, 0, self.done.stderr)
        whole = self.read("map/index.html")
        index = whole[whole.index('class="tag-map-index-list"'):]
        for name in ("Notes", "Test", "Nested", "Leaf"):
            self.assertRegex(index, rf">{name}</a>", name)
        self.assertIn("A Dated Essay</a>", index)
        self.assertIn("An Undated Page</a>", index)
        self.assertNotIn("Hidden Draft Title", whole)
        # No caption under the map's name: its description is for search.
        self.assertNotIn('class="tag-gloss"', whole)
        # The drawing holds links, so it is hidden from assistive technology
        # (an image's links are nested interactive controls); a hidden
        # paragraph says what it shows.
        self.assertRegex(whole, r'<p class="visually-hidden">Map of tags\. A map of the \d+ tags')
        self.assertRegex(whole, r'<svg [^>]*class="tag-map"[^>]* aria-hidden="true"')
        self.assertNotIn('role="img"', whole[whole.index("<svg"):whole.index("</svg>")])


@requires_cabal
class FeaturedPhotographs(unittest.TestCase):
    def test_the_sidecar_s_order_leads_the_grid(self):
        from tests._helpers import write_photo_site_config
        with tempfile.TemporaryDirectory(prefix="tag-photos-") as tmp:
            root = Path(tmp)
            (root / "engine").symlink_to(engine_path(""))
            page(root / "content/index.md", "title: Home\nhome: true", "Home.")
            for n, day in (("one", 1), ("two", 2), ("three", 3)):
                page(root / f"content/photography/{n}.md",
                     f"title: Photo {n}\ndate: 2026-05-0{day}\nphoto: /images/{n}.jpg\n"
                     "tags: [photography, photography/place]")
            page(root / "content/tag-meta/photography/place.md",
                 "featured:\n  - content/photography/one.md")
            # A place too large for one page, its oldest photograph featured.
            for n in range(151):
                page(root / f"content/photography/crowd-{n:03d}.md",
                     f"title: Crowd {n:03d}\ndate: 2026-{n % 12 + 1:02d}-{n % 28 + 1:02d}\n"
                     f"photo: /images/crowd-{n:03d}.jpg\ntags: [photography, photography/crowd]")
            page(root / "content/photography/oldest.md",
                 "title: The Oldest\ndate: 2020-01-01\nphoto: /images/oldest.jpg\n"
                 "tags: [photography, photography/crowd]")
            page(root / "content/tag-meta/photography/crowd.md",
                 "featured:\n  - content/photography/oldest.md")
            write_photo_site_config(root)
            done = subprocess.run([str(site_binary()), "rebuild"], cwd=root,
                                  env=script_env(SITE_THREADS="1"),
                                  capture_output=True, text=True, timeout=240)
            self.assertEqual(done.returncode, 0, done.stdout[-3000:] + done.stderr)
            place = (root / "_site/photography/place/index.html").read_text(encoding="utf-8")
            self.assertIn('class="photography-grid"', place)
            titles = re.findall(r'class="photo-card-title">Photo (\w+)<', place)
            # The featured photograph first, then the rest newest first.
            self.assertEqual(titles, ["one", "three", "two"])
            crowd = (root / "_site/photography/crowd/index.html").read_text(encoding="utf-8")
            first = re.search(r'class="photo-card-title">([^<]*)<', crowd)
            self.assertEqual(first.group(1), "The Oldest")


if __name__ == "__main__":
    unittest.main()
