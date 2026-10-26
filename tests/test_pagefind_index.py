"""The keyword index is replaced by content (tools/pagefind-index.sh).

The build used to delete _site/pagefind and index afresh: Pagefind's output
is deterministic, but every file got a new mtime, so each deploy re-sent all
~450 index files and readers re-downloaded unchanged ones (nginx's ETag is
the mtime). Now an unchanged file keeps its mtime, a changed one is
replaced, and a fragment Pagefind no longer writes is removed.
"""

from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import engine_path, script_env

SCRIPT = engine_path("tools/pagefind-index.sh")
OLD = 1_000_000_000  # 2001: any file the script rewrites is newer


def page(title: str, words: str) -> str:
    return (f"<!doctype html><html lang=\"en\"><head><title>{title}</title></head>"
            f"<body><main><h1>{title}</h1><p>{words}</p></main></body></html>\n")


@unittest.skipUnless(shutil.which("pagefind") and shutil.which("rsync"), "needs pagefind and rsync")
class PagefindIndexTests(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.site = Path(tmp.name) / "site"
        self.site.mkdir()
        (self.site / "a.html").write_text(page("Alpha", "domination numbers of random graphs"))
        (self.site / "b.html").write_text(page("Beta", "lattice cryptography and vector units"))
        self.index()

    def index(self):
        done = subprocess.run(["bash", str(SCRIPT), str(self.site)], env=script_env(),
                              capture_output=True, text=True, timeout=120)
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)

    def files(self) -> dict[str, float]:
        root = self.site / "pagefind"
        return {p.relative_to(root).as_posix(): p.stat().st_mtime
                for p in root.rglob("*") if p.is_file()}

    def age_everything(self):
        for p in (self.site / "pagefind").rglob("*"):
            if p.is_file():
                os.utime(p, (OLD, OLD))

    def fragments(self) -> set[str]:
        return {f for f in self.files() if f.startswith("fragment/")}

    def test_an_unchanged_site_rewrites_nothing(self):
        self.age_everything()
        self.index()
        self.assertEqual({f for f, t in self.files().items() if t != OLD}, set())

    def test_an_edited_page_replaces_its_fragment_and_keeps_the_others(self):
        before = self.fragments()
        self.age_everything()
        (self.site / "a.html").write_text(page("Alpha", "domination numbers, revised"))
        self.index()
        after = self.files()
        new = self.fragments() - before
        self.assertEqual(len(new), 1)
        self.assertEqual(len(before - self.fragments()), 1, "the old fragment is removed")
        self.assertTrue(all(after[f] == OLD for f in self.fragments() - new))

    def test_a_removed_page_loses_its_fragment(self):
        before = self.fragments()
        (self.site / "b.html").unlink()
        self.index()
        self.assertEqual(len(self.fragments()), len(before) - 1)

    def test_compressed_copies_are_left_to_compress_assets(self):
        sidecar = self.site / "pagefind" / "pagefind.js.gz"
        sidecar.write_bytes(b"gzip bytes")
        self.index()
        self.assertEqual(sidecar.read_bytes(), b"gzip bytes")


if __name__ == "__main__":
    unittest.main()
