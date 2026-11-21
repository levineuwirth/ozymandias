"""A photograph's variants, on its page and on the map, are those the
tools' record pairs with it (build/DerivedImages.hs), and a page built with
one answer is built again when the answer changes.

The fixture site, with photographs on, built by the generator alone:

  · the page of a photograph offers the variants and WebPs the record
    says were made from it, and the map's pin shows its 480px variant;
  · a file that only has a variant's name is offered by neither;
  · a file its author declared another image's companion is that image's
    alone, whatever the record says it was made from;
  · a variant changed by hand since it was made is no longer offered, and
    one recorded since is, on the next build, without a clean: the pages
    that asked are rebuilt, and no others for that reason.

    python -m unittest tests.test_derived_pairing -v
"""

from __future__ import annotations

import json
import re
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests import fixture_variants
from tests._helpers import ENGINE, record_derived, requires_cabal, script_env, site_binary

SERIES = "content/photography/fixture-series"


@requires_cabal
class PhotographPairings(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory(prefix="derived-pairing-")
        self.addCleanup(tmp.cleanup)
        self.site = Path(tmp.name) / "site"
        build = next(b for b in fixture_variants.builds() if b.name == "minimal")
        fixture_variants.copy_site(build, self.site, ENGINE)
        # A file of a variant's name beside the second frame, which no
        # tool made of it.
        shutil.copy(self.site / SERIES / "one.w480.jpg", self.site / SERIES / "two.w480.jpg")
        self.env = script_env(SITE_ENV="production", SITE_ROOT=str(self.site), SITE_THREADS="1")
        self.run_site("footer-data")

    def run_site(self, command: str) -> str:
        done = subprocess.run([str(site_binary()), command], cwd=self.site, env=self.env,
                              capture_output=True, text=True, timeout=600)
        self.assertEqual(done.returncode, 0, done.stdout[-3000:] + done.stderr[-3000:])
        return done.stdout + done.stderr

    def thumbs(self) -> dict[str, str]:
        pins = json.loads((self.site / "_site/photography/map.json").read_text(encoding="utf-8"))
        return {pin["title"]: pin["thumb"].rsplit("/", 1)[-1] for pin in pins}

    def page(self, frame: str) -> str:
        return (self.site / f"_site/photography/fixture-series/{frame}/index.html").read_text(encoding="utf-8")

    def updated(self, log: str) -> set[str]:
        return set(re.findall(r"updated (content/\S+)", log))

    def test_a_variant_is_offered_where_the_record_pairs_it_and_pages_follow_the_record(self):
        self.run_site("build")
        # The first frame's variants are the fixture's record's; the second
        # frame's namesake is no one's.
        self.assertEqual(self.thumbs()["One"], "one.w480.jpg")
        self.assertEqual(self.thumbs()["Two"], "two.jpg")
        self.assertIn("one.w480.jpg 480w", self.page("one"))
        self.assertIn("one.w480.webp 480w", self.page("one"))
        self.assertNotIn("two.w480", self.page("two"))
        self.assertEqual(self.updated(self.run_site("build")), set())

        # The first frame's WebP variant is changed by hand: no longer what
        # the record says was written, and no longer offered.
        original = (self.site / SERIES / "one.w480.webp").read_bytes()
        (self.site / SERIES / "one.w480.webp").write_bytes(b"retouched by hand")
        updated = self.updated(self.run_site("build"))
        self.assertIn(f"{SERIES}/one.md", updated)
        self.assertNotIn("one.w480.webp", self.page("one"))
        self.assertIn("one.w480.jpg 480w", self.page("one"))
        # A page that shows no photograph of that directory is not rebuilt.
        self.assertFalse({u for u in updated if u.startswith("content/essays/")}, updated)
        self.assertEqual(self.updated(self.run_site("build")), set())

        # The author declares the first frame's WebP variant the companion
        # of its JPEG variant: it is then that image's and no other's, and
        # the photograph, whose entry for it the record still has, is not
        # offered it.
        (self.site / SERIES / "one.w480.webp").write_bytes(original)
        self.run_site("build")
        self.assertIn("one.w480.webp 480w", self.page("one"))
        (self.site / "data/companions.json").write_text(
            json.dumps({f"{SERIES}/one.w480.webp": f"{SERIES}/one.w480.jpg"}), encoding="utf-8")
        self.assertIn(f"{SERIES}/one.md", self.updated(self.run_site("build")))
        self.assertNotIn("one.w480.webp", self.page("one"))
        self.assertIn("one.w480.jpg 480w", self.page("one"))

        # The second frame's variant is recorded (as an adoption would):
        # its pin takes it on the next build.
        record_derived(self.site, {f"{SERIES}/two.w480.jpg": f"{SERIES}/two.jpg"}, "generate-thumbnails")
        self.run_site("build")
        self.assertEqual(self.thumbs()["Two"], "two.w480.jpg")
        self.assertEqual(self.updated(self.run_site("build")), set())


if __name__ == "__main__":
    unittest.main()
