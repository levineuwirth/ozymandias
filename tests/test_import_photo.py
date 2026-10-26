"""tools/import-photo.sh and tools/import-photos.sh, run end to end in a
scratch copy of the repository's layout (the scripts find the repository
from their own path), with a generated JPEG.

What a730bad broke and its review caught: a title passed through a
tab-separated plan lost everything after a tab and crashed on a newline;
and tags normalized after the namespace check (`street art` became
`streetart`) could claim an existing series' URL. The single importer had
no namespace check at all."""

import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import (SITE_ROOT, copy_engine_tree, requires_cabal, script_env, site_binary,
                            write_site_config)

try:
    import yaml
    from PIL import Image
except ImportError:  # pragma: no cover — no .venv
    yaml = None

READY = (yaml is not None and shutil.which("magick") and shutil.which("exiftool")
         and (SITE_ROOT / ".venv" / "bin" / "python").exists())


@requires_cabal
@unittest.skipUnless(READY, "needs magick, exiftool and the .venv")
class Importers(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.tmp = Path(tmp.name)
        self.repo = self.tmp / "repo"
        copy_engine_tree("tools", self.repo / "tools",
                         ignore=shutil.ignore_patterns("__pycache__", "bin"))
        (self.repo / ".venv").symlink_to(SITE_ROOT / ".venv")
        self.photos = self.repo / "content" / "photography"
        self.photos.mkdir(parents=True)
        # The namespace guard asks the generator what the tree routes
        # (`site list-routes`), which reads data/ as a build does.
        (self.repo / "data").mkdir()
        write_site_config(self.repo)
        self.env = script_env(SITE_BINARY=str(site_binary()))
        self.original = self.tmp / "original.jpg"
        Image.new("RGB", (300, 200), (200, 50, 50)).save(self.original, quality=90)

    def run_single(self, *args):
        return subprocess.run(["bash", str(self.repo / "tools" / "import-photo.sh"),
                               str(self.original), *args],
                              cwd=self.repo, env=self.env, capture_output=True, text=True,
                              timeout=120)

    def run_bulk(self, manifest_lines, *args):
        manifest = self.tmp / "manifest.tsv"
        manifest.write_text("".join(line + "\n" for line in manifest_lines))
        return subprocess.run(["bash", str(self.repo / "tools" / "import-photos.sh"),
                               "--manifest", str(manifest), *args, "--execute"],
                              cwd=self.repo, env=self.env, capture_output=True, text=True,
                              timeout=120)

    def front_matter(self, path):
        return yaml.safe_load(path.read_text().split("---")[1])

    def test_a_title_keeps_its_tabs_and_newlines(self):
        title = 'Tab\there, "quoted",\nand a second line'
        done = self.run_single("harbor", "--title", title)
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(self.front_matter(self.photos / "harbor" / "index.md")["title"], title)

    def test_a_normalized_tag_cannot_claim_an_existing_series(self):
        (self.photos / "streetart").mkdir()
        (self.photos / "streetart" / "index.md").write_text("---\ntitle: Street Art\n---\n")
        single = self.run_single("harbor", "--tags", "street art")
        self.assertNotEqual(single.returncode, 0)
        self.assertIn("/photography/streetart/", single.stderr)
        self.assertFalse((self.photos / "harbor").exists())
        bulk = self.run_bulk([f"{self.original}\tquay"], "--series", "copenhagen",
                             "--tags", "street art")
        self.assertNotEqual(bulk.returncode, 0)
        self.assertIn("/photography/streetart/", bulk.stderr)
        self.assertFalse((self.photos / "copenhagen").exists())

    def test_a_series_cannot_claim_an_existing_tag(self):
        (self.photos / "old").mkdir()
        (self.photos / "old" / "index.md").write_text(
            '---\ntitle: Old\ndate: 2026-01-01\ntags: ["photography", "photography/night"]\n---\n')
        done = self.run_single("harbor", "--series", "night")
        self.assertNotEqual(done.returncode, 0)
        self.assertIn("/photography/night/", done.stderr)
        self.assertFalse((self.photos / "night").exists())

    def test_a_nested_tag_cannot_claim_a_photograph_or_its_series(self):
        # photography/denmark/harbor pages /photography/denmark/harbor/ and,
        # as an ancestor, /photography/denmark/: the series' entry and its
        # landing. The old guard looked only at one-level tags.
        (self.photos / "denmark").mkdir()
        (self.photos / "denmark" / "index.md").write_text(
            "---\ntitle: Denmark\ndate: 2026-01-01\ntags: [photography]\n---\n")
        (self.photos / "denmark" / "harbor.md").write_text(
            "---\ntitle: Harbor\ndate: 2026-01-02\nseries: denmark\ntags: [photography]\n---\n")
        done = self.run_single("quay", "--tags", "photography/denmark/harbor")
        self.assertNotEqual(done.returncode, 0)
        self.assertIn("tag 'photography/denmark' collides with content/photography/denmark/index.md",
                      done.stderr)
        self.assertIn("tag 'photography/denmark/harbor' collides with "
                      "content/photography/denmark/harbor.md", done.stderr)
        self.assertFalse((self.photos / "quay").exists())

    def test_empty_tag_segments_are_judged_as_the_build_expands_them(self):
        # The build drops empty path segments: photography//denmark/harbor
        # pages photography/denmark and photography/denmark/harbor. A Python
        # copy of the expansion kept them, so both importers copied the
        # photos and the next build refused the collision.
        (self.photos / "denmark").mkdir()
        (self.photos / "denmark" / "index.md").write_text(
            "---\ntitle: Denmark\ndate: 2026-01-01\ntags: [photography]\n---\n")
        (self.photos / "denmark" / "harbor.md").write_text(
            "---\ntitle: Harbor\ndate: 2026-01-02\nseries: denmark\ntags: [photography]\n---\n")
        single = self.run_single("quay", "--tags", "photography//denmark/harbor")
        bulk = self.run_bulk([f"{self.original}\tpier"], "--tags", "photography//denmark/harbor")
        for done in (single, bulk):
            self.assertNotEqual(done.returncode, 0)
            self.assertIn("tag 'photography/denmark' collides with "
                          "content/photography/denmark/index.md", done.stderr)
            self.assertIn("tag 'photography/denmark/harbor' collides with "
                          "content/photography/denmark/harbor.md", done.stderr)
        self.assertFalse((self.photos / "quay").exists())
        self.assertFalse((self.photos / "pier").exists())
        self.assertEqual(sorted(p.name for p in (self.photos / "denmark").iterdir()),
                         ["harbor.md", "index.md"])

    def test_a_tag_cannot_claim_what_the_same_import_creates(self):
        bulk = self.run_bulk([f"{self.original}\tquay"], "--series", "harbor-roll",
                             "--tags", "harbor-roll")
        self.assertNotEqual(bulk.returncode, 0)
        self.assertIn("tag 'photography/harbor-roll' collides with the series 'harbor-roll': "
                      "both would claim /photography/harbor-roll/", bulk.stderr)
        self.assertFalse((self.photos / "harbor-roll").exists())

    def test_a_photograph_cannot_claim_a_tag_page(self):
        (self.photos / "old").mkdir()
        (self.photos / "old" / "index.md").write_text(
            '---\ntitle: Old\ndate: 2026-01-01\ntags: ["photography", "photography/roll/quay"]\n---\n')
        bulk = self.run_bulk([f"{self.original}\tquay"], "--series", "roll")
        self.assertNotEqual(bulk.returncode, 0)
        self.assertIn("series 'roll' collides with the tag 'photography/roll'", bulk.stderr)
        self.assertIn("photograph 'quay' being imported collides with the tag "
                      "'photography/roll/quay'", bulk.stderr)
        self.assertFalse((self.photos / "roll").exists())

    def test_bulk_import_still_imports(self):
        done = self.run_bulk([f"{self.original}\tquay-one", f"{self.original}\tquay-two\tQuay Two"],
                             "--series", "harbor-roll", "--tags", "night")
        self.assertEqual(done.returncode, 0, done.stderr)
        fm = self.front_matter(self.photos / "harbor-roll" / "quay-two.md")
        self.assertEqual((fm["title"], fm["tags"]), ("Quay Two", ["photography", "photography/night"]))
        self.assertTrue((self.photos / "harbor-roll" / "index.md").exists())


if __name__ == "__main__":
    unittest.main()
