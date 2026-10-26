#!/usr/bin/env python3
"""``site footer-data``: the per-page Backlinks and Related files.

Every page depends on its own data/footer/ file rather than on
data/backlinks.json and data/similar-links.json, which change on almost
every edit (audit H01; build/FooterData.hs). That only saves anything if a
file is rewritten exactly when the page's rendered footer would change: an
untouched file keeps its mtime, and its mtime is all Hakyll looks at. These
tests run the split in a scratch directory and check that contract — and
that a key which loses its entries is emptied, not deleted, because Hakyll
does not notice an identifier going away.
"""

from __future__ import annotations

import json
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import requires_cabal, script_env, site_binary


def entry(url: str, score: float) -> dict:
    return {"url": url, "title": url.strip("/").title(), "score": score}


BACKLINK = {"url": "/essays/b/", "title": "B", "abstract": "", "sentence": "s",
            "paragraph": "p", "fragment": ""}


@requires_cabal
class FooterDataTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.binary = site_binary()

    def setUp(self) -> None:
        self.dir = Path(tempfile.mkdtemp(prefix="footer-data-"))
        self.addCleanup(shutil.rmtree, self.dir)
        (self.dir / "_site" / "data").mkdir(parents=True)
        (self.dir / "data").mkdir()
        self.backlinks = {"/essays/a": [BACKLINK]}
        self.similar = {
            "/essays/a.html": [entry(f"/essays/{c}/", 0.9 - i / 100) for i, c in enumerate("bcdef")],
            "/essays/b/": [entry("/essays/a.html", 0.8)],
            "/essays/caf%C3%A9.html": [entry("/essays/a.html", 0.7)],
        }

    def split(self, env: dict | None = None) -> subprocess.CompletedProcess:
        (self.dir / "_site/data/backlinks.json").write_text(json.dumps(self.backlinks))
        (self.dir / "data/similar-links.json").write_text(json.dumps(self.similar))
        return subprocess.run([str(self.binary), "footer-data"], cwd=self.dir,
                              capture_output=True, text=True,
                              env=script_env(**{"SITE_ENV": "production", **(env or {})}))

    def read(self, name: str) -> dict:
        return json.loads((self.dir / "data/footer" / name).read_text())

    def mtimes(self) -> dict[str, int]:
        return {p.name: p.stat().st_mtime_ns for p in (self.dir / "data/footer").iterdir()}

    def test_one_file_per_page_keyed_like_its_route(self) -> None:
        done = self.split()
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(sorted(p.name for p in (self.dir / "data/footer").iterdir()),
                         ["%2Fessays%2Fa.json", "%2Fessays%2Fb%2F.json", "%2Fessays%2Fcaf%C3%A9.json"])
        a = self.read("%2Fessays%2Fa.json")
        self.assertEqual(a["backlinks"], [BACKLINK])
        # Only what is rendered, without scores.
        self.assertEqual(a["related"], [{"url": f"/essays/{c}/", "title": f"Essays/{c}".title()}
                                        for c in "bcd"])
        self.assertEqual(self.read("%2Fessays%2Fb%2F.json")["backlinks"], [])

    def test_unchanged_entries_leave_files_alone(self) -> None:
        self.split()
        before = self.mtimes()
        # A score that moved and an entry below the rendered three are not
        # visible on any page, so they rewrite nothing.
        self.similar["/essays/a.html"][0]["score"] = 0.95
        self.similar["/essays/a.html"][4]["url"] = "/essays/z/"
        done = self.split()
        self.assertIn("0 rewritten", done.stdout)
        self.assertEqual(self.mtimes(), before)

    def test_literal_unicode_and_encoded_urls_share_a_footer(self) -> None:
        self.backlinks = {"/essays/café": [BACKLINK]}
        for url in ("/essays/café.html", "/essays/caf%C3%A9.html"):
            with self.subTest(url=url):
                self.similar = {url: [entry("/essays/a.html", 0.7)]}
                done = self.split()
                self.assertEqual(done.returncode, 0, done.stderr)
                footer = self.read("%2Fessays%2Fcaf%C3%A9.json")
                self.assertEqual(footer["backlinks"], [BACKLINK])
                self.assertEqual(len(footer["related"]), 1)

    def test_a_changed_page_rewrites_only_its_file(self) -> None:
        self.split()
        before = self.mtimes()
        self.similar["/essays/b/"] = [entry("/essays/c/", 0.8)]
        done = self.split()
        self.assertIn("1 rewritten", done.stdout)
        after = self.mtimes()
        self.assertEqual([n for n in after if after[n] != before[n]], ["%2Fessays%2Fb%2F.json"])

    def test_a_page_that_loses_its_entries_is_emptied_not_deleted(self) -> None:
        self.split()
        del self.similar["/essays/b/"]
        self.split()
        self.assertEqual(self.read("%2Fessays%2Fb%2F.json"), {"backlinks": [], "related": []})

    def test_missing_inputs_count_as_empty(self) -> None:
        done = subprocess.run([str(self.binary), "footer-data"], cwd=self.dir,
                              capture_output=True, text=True,
                              env=script_env(SITE_ENV="production"))
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(list((self.dir / "data/footer").iterdir()), [])

    def test_dev_builds_refuse(self) -> None:
        # A dev build's backlinks include drafts; the files are shared with
        # the production build, so a dev split could publish a draft's link.
        done = self.split({"SITE_ENV": "dev"})
        self.assertNotEqual(done.returncode, 0)
        self.assertFalse((self.dir / "data/footer").exists())


if __name__ == "__main__":
    unittest.main()
