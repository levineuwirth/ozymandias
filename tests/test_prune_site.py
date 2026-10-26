#!/usr/bin/env python3
"""tools/prune-site.py removes published score files whose gitignored source
is gone (audit M02), with their sidecars, and nothing else.

Run with: ``python3 -m unittest tests.test_prune_site``.
"""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path
from tests._helpers import load_tool

prune_site = load_tool("prune-site.py")


class Prune(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        root = Path(tmp.name)
        self.site, self.content = root / "_site", root / "content" / "music"
        self.trees = [("music", self.content, "scores")]

    def touch(self, base: Path, *rels: str) -> None:
        for rel in rels:
            (base / rel).parent.mkdir(parents=True, exist_ok=True)
            (base / rel).write_text("x")

    def test_withheld_pages_and_their_sidecars_go(self):
        # Three pages published, then the work re-imported with two.
        self.touch(self.content, "w/scores/page-1.svg", "w/scores/page-2.svg")
        self.touch(self.site, "w/score/index.html", "w/index.html",
                   *(f"music/w/scores/page-{i}.svg{s}" for i in (1, 2, 3) for s in ("", ".br", ".gz")))
        # (the HTML above sits outside music/, and must survive)
        removed = prune_site.prune(self.site, self.trees)
        self.assertEqual(sorted(p.name for p in removed),
                         ["page-3.svg", "page-3.svg.br", "page-3.svg.gz"])
        self.assertTrue((self.site / "music/w/scores/page-2.svg.br").exists())
        self.assertTrue((self.site / "w/score/index.html").exists())

    def test_audio_dropped_on_reimport_goes(self):
        self.touch(self.content, "w/scores/page-1.svg")
        self.touch(self.site, "music/w/scores/page-1.svg", "music/w/scores/realization.mp3",
                   "music/w/scores/timing.json", "music/w/score/index.html")
        removed = prune_site.prune(self.site, self.trees)
        self.assertEqual(sorted(p.name for p in removed), ["realization.mp3", "timing.json"])
        self.assertTrue((self.site / "music/w/score/index.html").exists())

    def test_nothing_to_do(self):
        self.touch(self.content, "w/scores/page-1.svg")
        self.touch(self.site, "music/w/scores/page-1.svg", "music/w/scores/page-1.svg.br")
        self.assertEqual(prune_site.prune(self.site, self.trees), [])


if __name__ == "__main__":
    unittest.main()
