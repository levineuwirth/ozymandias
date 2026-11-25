"""The home page's shelf of portals (switch portal-shelf; Tags.portalShelfContext,
templates/partials/portal-shelf.html), in a whole build of a small scratch
site:

  · the portals stand in site.yaml's order, a run of one facet together;
  · a spine's count is what its tag pages list (its subtags' with it,
    and not a blog post, which the tag index does not file), and what a
    section publishes at its address, an address of the section's own
    included; a portal with nothing has no count;
  · a spine's caption is its sidecar's gloss, else its facts in a phrase,
    and its cover, if its sidecar names one, rides on it for the script;
  · with tag-map on, the whole map stands open under the shelf, captioned
    with what it draws, and the home page loads the map's assets.

    python -m unittest tests.test_portal_shelf -v
"""

from __future__ import annotations

import re
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import engine_path, requires_cabal, script_env, site_binary, write_site_config


def page(path: Path, front: str, body: str = "Body.") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(f"---\n{front}\n---\n\n{body}\n", encoding="utf-8")


@requires_cabal
class PortalShelf(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        tmp = tempfile.TemporaryDirectory(prefix="portal-shelf-")
        cls.addClassCleanup(tmp.cleanup)
        root = Path(tmp.name)
        (root / "engine").symlink_to(engine_path(""))
        page(root / "content/index.md", "title: Home\nhome: true", "Home.")
        page(root / "content/essays/first.md", "title: First\ndate: 2026-03-02\ntags: [notes]")
        page(root / "content/essays/second.md", "title: Second\ndate: 2026-05-09\ntags: [notes/inner, research]")
        # A poem that carries its section's tag counts once.
        page(root / "content/poetry/one.md", "title: One\ndate: 2026-01-01\ntags: [poetry]")
        page(root / "content/poetry/two.md", "title: Two\ndate: 2026-02-01")
        # Filed under a subtag of a section's portal: /poetry/metre/ lists
        # it, though no page is /poetry/'s tag index.
        page(root / "content/essays/metre.md", "title: On Metre\ndate: 2026-06-01\ntags: [poetry/metre]")
        # Filed under the section's own tag, which has no key in the tag
        # index (the section has /poetry/).
        page(root / "content/essays/verse.md", "title: On Verse\ndate: 2026-07-01\ntags: [poetry]")
        page(root / "content/tag-meta/poetry.md", 'tooltip: "verse"\ncover: "/images/cover.webp"', "")
        # The blog is served at an address of its own; one post is untagged,
        # one tagged notes, which /notes/ does not list (the tag index files
        # no blog post), so neither does the shelf.
        page(root / "content/blog/untagged.md", "title: Untagged\ndate: 2026-04-01")
        page(root / "content/blog/tagged.md", "title: Tagged Post\ndate: 2026-04-02\ntags: [notes]")
        write_site_config(
            root,
            sections=[{"dir": d, "kind": k, "dingbat": None} for d, k in
                      (("essays", "Essay"), ("poetry", "Poem"), ("fiction", "Fiction"))]
                     + [{"dir": "blog", "address": "notebook", "kind": "Post", "dingbat": None}],
            portals=[{"name": "Research", "tag": "research", "facet": "research"},
                     {"name": "Poetry", "tag": "poetry", "facet": "form"},
                     {"name": "Fiction", "tag": "fiction", "facet": "form"},
                     {"name": "Notebook", "tag": "notebook", "facet": "form"},
                     {"name": "Notes", "tag": "notes", "facet": "subject"}],
            switches={"portal-shelf": True, "tag-map": True})
        cls.done = subprocess.run([str(site_binary()), "rebuild"], cwd=root,
                                  env=script_env(SITE_ENV="production", SITE_THREADS="1"),
                                  capture_output=True, text=True, timeout=240)
        home = root / "_site/index.html"
        cls.home = home.read_text(encoding="utf-8") if home.is_file() else ""
        notes = root / "_site/notes/index.html"
        cls.notes = notes.read_text(encoding="utf-8") if notes.is_file() else ""
        metre = root / "_site/poetry/metre/index.html"
        cls.metre = metre.read_text(encoding="utf-8") if metre.is_file() else ""

    def spine(self, tag: str) -> str:
        m = re.search(rf'<li style="--count: (\d+)"><a class="portal-spine[^"]*"[^>]*data-tag="{tag}"[^>]*>.*?</a>',
                      self.home, re.S)
        self.assertIsNotNone(m, f"no spine for {tag}")
        return m.group(0)

    def test_the_build_succeeds(self):
        self.assertEqual(self.done.returncode, 0, self.done.stdout[-3000:] + self.done.stderr)

    def test_the_portals_stand_in_order_a_facet_together(self):
        facets = re.findall(r'<ol class="portal-shelf-facet">(.*?)</ol>', self.home, re.S)
        self.assertEqual([re.findall(r'data-tag="([^"]+)"', f) for f in facets],
                         [["research"], ["poetry", "fiction", "notebook"], ["notes"]])
        self.assertNotIn('class="hp-portals"', self.home)

    def test_a_spine_counts_what_its_page_lists(self):
        # /notes/ lists the two essays and not the blog post tagged notes.
        self.assertIn("Second", self.notes)
        self.assertNotIn("Tagged Post", self.notes)
        self.assertIn('--count: 2"', self.spine("notes"))      # notes and notes/inner
        # A section at an address of its own: both posts are the Notebook's.
        self.assertIn('--count: 2"', self.spine("notebook"))
        self.assertIn("On Metre", self.metre)
        # Two poems (one tagged poetry, counted once), the essay on
        # poetry/metre, and the essay tagged poetry.
        self.assertIn('--count: 4"', self.spine("poetry"))
        self.assertIn('--count: 1"', self.spine("research"))
        self.assertIn('<span class="portal-spine-count">2</span>', self.spine("notes"))
        fiction = self.spine("fiction")
        self.assertIn('--count: 0"', fiction)
        self.assertNotIn("portal-spine-count", fiction)

    def test_a_spine_s_caption_and_cover(self):
        poetry = self.spine("poetry")
        self.assertIn('data-caption="verse"', poetry)
        self.assertIn('data-cover="/images/cover.webp"', poetry)
        notes = self.spine("notes")
        self.assertIn('data-caption="two essays, March to May 2026"', notes)
        self.assertNotIn("data-cover", notes)
        self.assertIn("portal-spine--research", self.spine("research"))

    def test_each_tag_on_the_map_has_a_caption(self):
        listed = re.findall(r'<li data-tag="([^"]+)" data-name="([^"]+)" data-url="[^"]+" data-caption="([^"]*)">',
                            self.home)
        by_tag = {t: (n, c) for t, n, c in listed}
        self.assertEqual(by_tag["notes/inner"], ("Inner", "one essay, May 2026"))
        self.assertEqual(by_tag["notes"][1], "two essays, March to May 2026")

    def test_the_map_stands_open_under_the_shelf(self):
        stage = self.home[self.home.index('class="portal-shelf-stage"'):]
        self.assertRegex(stage[:400], r'<svg [^>]*class="tag-map"')
        self.assertRegex(self.home, r'class="portal-shelf-caption-meta">\d+ tags and \d+ pieces<')
        for asset in ("css/tag-map.css", "js/tag-map.js", "css/portal-shelf.css", "js/portal-shelf.js"):
            self.assertIn(asset, self.home)


if __name__ == "__main__":
    unittest.main()
