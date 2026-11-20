"""A collection's landing: content/{blog,poetry,fiction}/<collection>/index.md.

One build of a small scratch site with a collection in each section:

  · the landing is a page of its own (templates/collection.html): a main
    region the skip link goes to, its title, its own prose, and the
    collection's entries under it, the newest first;
  · an entry that is a draft is not listed, and neither is an entry of
    another collection or of the section itself;
  · a collection with no entry says so.

The gate at the end of `make build` (tools/check-site.py) meets a blog
collection's landing in tests/test_blog_images.py.

    python -m unittest tests.test_collection_landings -v
"""

from __future__ import annotations

import re
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import engine_path, requires_cabal, script_env, site_binary, write_site_config

SECTIONS = ("blog", "poetry", "fiction")


def page(path: Path, front: str, body: str = "Body.") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(f"---\n{front}\n---\n\n{body}\n", encoding="utf-8")


@requires_cabal
class CollectionLandings(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        tmp = tempfile.TemporaryDirectory(prefix="collection-landings-")
        cls.addClassCleanup(tmp.cleanup)
        cls.root = root = Path(tmp.name)
        (root / "engine").symlink_to(engine_path(""))
        page(root / "content/index.md", "title: Home\nhome: true", "Home.")
        for section in SECTIONS:
            at = root / "content" / section
            page(at / "alone.md", "title: Alone\ndate: 2026-01-09")
            page(at / "trip/index.md", "title: The trip\nabstract: Three days out.", "Why we went.")
            page(at / "trip/day-one.md", "title: Day one\ndate: 2026-01-03\nabstract: Setting out.")
            page(at / "trip/day-two.md", "title: Day two\ndate: 2026-01-04")
            page(at / "trip/day-three.md", "title: Day three\ndate: 2026-01-05\ndraft: true")
            page(at / "other/index.md", "title: Other")
            page(at / "other/elsewhere.md", "title: Elsewhere\ndate: 2026-01-07")
            page(at / "empty/index.md", "title: Empty")
        write_site_config(root)
        # One worker: with several, a build of a site this small has failed
        # in Hakyll's metadata cache before any page was compiled.
        done = subprocess.run([str(site_binary()), "rebuild"], cwd=root,
                              env=script_env(SITE_ENV="build", SITE_THREADS="1"),
                              capture_output=True, text=True, timeout=300)
        assert done.returncode == 0, done.stdout[-3000:] + done.stderr[-3000:]

    def read(self, path: str) -> str:
        return (self.root / "_site" / path).read_text(encoding="utf-8")

    def test_a_landing_is_a_page_that_lists_its_collection_s_entries(self):
        for section in SECTIONS:
            with self.subTest(section=section):
                landing = self.read(f"{section}/trip/index.html")
                main = re.search(r'<main id="markdownBody"[^>]*>(.*?)</main>', landing, re.S)
                self.assertIsNotNone(main, "no main region for the skip link")
                body = main.group(1)
                self.assertIn('<h1 class="page-title">The trip</h1>', body)
                self.assertIn("Three days out.", body)
                self.assertIn("Why we went.", body)
                # Its own published entries, the newest first, each at its
                # address (written from the landing's own).
                self.assertEqual(re.findall(r'<a href="([^"]*)">([^<]*)</a>', body.split("collection-contents", 1)[1]),
                                 [(f"../../{section}/trip/day-two.html", "Day two"),
                                  (f"../../{section}/trip/day-one.html", "Day one")])
                self.assertRegex(body, r'<time class="essay-list-date" datetime="2026-01-04">')
                self.assertIn("Setting out.", body)
                self.assertNotIn("Nothing has been added", body)

    def test_a_collection_with_no_entry_says_so(self):
        for section in SECTIONS:
            with self.subTest(section=section):
                landing = self.read(f"{section}/empty/index.html")
                self.assertIn('<main id="markdownBody"', landing)
                self.assertIn("Nothing has been added to this collection yet.", landing)
                self.assertNotIn("essay-list-item", landing)


if __name__ == "__main__":
    unittest.main()
