"""A citation marker keeps what its author wrote around the key
(build/Citations.hs, markerLabel).

Pandoc keeps a locator in the citation's suffix: `[@PralatWormald,
Theorem 4.1]` is the key and ", Theorem 4.1". The marker's label was the
cluster's numbers alone, so a page cited "[8]" and lost which theorem.

`MarkerLabels` reads each case's Markdown as pandoc does and labels its
citations, through GHCi as tests/test_pdf_urls.py does (the label is
private to the module). `ThroughThePipeline` renders a page with
`site render-fixture` in a copy of the fixture site, for what the label
cannot show alone: a footnote in a locator becomes the page's footnote,
and the marker stays one link.
"""

import json
import re
import subprocess
import unittest

from tests._helpers import CABAL_PROJECT, engine_path, requires_cabal, script_env, site_binary
from tests.test_golden import profile_site

# Markdown → (each citation cluster's label, how many footnotes it lifts out).
# Pandoc's smart extension writes "p. 3" with a no-break space (\u00a0).
CASES = [
    ("[@a]", [("[1]", 0)]),
    ("[@a; @b]", [("[1,2]", 0)]),
    ("[@a, Theorem 4.1]", [("[1, Theorem 4.1]", 0)]),
    ("[see @a, p. 3; @b]", [("[see 1, p.\u00a03; 2]", 0)]),
    # A locator's own separator is kept, and a textual citation's, which
    # has none, is a comma: neither reads as reference 123.
    ("[@a 23]", [("[1 23]", 0)]),
    ("@a [23]", [("[1, 23]", 0)]),
    ("@a [p. 3]", [("[1, p.\u00a03]", 0)]),
    ("[@a, *Lemma* 2]", [("[1, <em>Lemma</em> 2]", 0)]),
    ("[@a, [ch. 2](https://example.org)]", [("[1, ch.\u00a02]", 0)]),
    ('[@a, <a href="#chapter">chapter</a>]', [("[1, chapter]", 0)]),
    ("[@a, chapter^[A detail.]]", [("[1, chapter]", 1)]),
    ("[@a, p. <3 & up]", [("[1, p.\u00a0&lt;3 &amp; up]", 0)]),
]

GHCI_LABELS = r"""
let labels s = case runPure (readMarkdown def { readerExtensions = pandocExtensions } (T.pack s)) of { Right (Pandoc _ bs) -> let { cs = query (\i -> case i of { Cite c _ -> [c]; _ -> [] }) bs; ks = nub (map citationId (concat cs)) } in [ markerLabel [ (x, n) | x <- c, Just n <- [lookup (citationId x) (zip ks [1 :: Int ..])] ] | c <- cs ]; Left _ -> [] }
"""


@requires_cabal
class MarkerLabels(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        commands = [
            ":module *Citations",
            "import qualified Data.Text as T",
            "import qualified Data.Text.IO as TIO",
            "import Data.List (nub)",
            GHCI_LABELS.strip(),
            "mapM_ (\\s -> mapM_ (\\(l, ns) -> TIO.putStrLn (l <> T.pack (\"\\t\" ++ show (length ns)))) (labels s) >> putStrLn \"--\") "
            + "[" + ", ".join(json.dumps(md) for md, _ in CASES) + "]",
            ":quit",
        ]
        done = subprocess.run(
            ["cabal", "exec", "--", "ghc", "--interactive", "-ignore-dot-ghci", "-v0",
             f"-i{engine_path('build')}", str(engine_path("build/Citations.hs"))],
            cwd=CABAL_PROJECT, input="\n".join(commands) + "\n", text=True,
            capture_output=True, check=True, timeout=300,
        )
        if "error" in done.stderr.lower():
            raise AssertionError(done.stderr)
        groups = done.stdout.split("--\n")
        cls.labels = [[tuple(line.rsplit("\t", 1)) for line in g.splitlines() if line] for g in groups[:-1]]

    def test_labels(self):
        self.assertEqual(len(self.labels), len(CASES), self.labels)
        for (md, want), got in zip(CASES, self.labels):
            with self.subTest(markdown=md):
                self.assertEqual([(label, int(n)) for label, n in got], want)


PAGE = """---
title: Locators
---

A footnote in a locator [@alpha2001, chapter^[A detail.]] and an anchor
[@beta2010, <a href="#chapter">chapter</a>] keep the paragraph whole.
"""


@requires_cabal
class ThroughThePipeline(unittest.TestCase):
    def test_a_marker_is_one_link_and_its_footnote_the_page_s(self):
        site = profile_site("full")
        (site / "locators.md").write_text(PAGE, encoding="utf-8")
        done = subprocess.run([str(site_binary()), "render-fixture", "locators.md"], cwd=site,
                              env=script_env(SITE_ROOT=str(site)), capture_output=True, text=True, timeout=300)
        self.assertEqual(done.returncode, 0, done.stderr)
        body = done.stdout.split("<!-- render-fixture: toc -->")[0]
        markers = re.findall(r'<sup class="cite-marker"[^>]*>(.*?)</sup>', body, re.S)
        self.assertEqual(len(markers), 2, body)
        for marker in markers:
            with self.subTest(marker=marker):
                # One link: no anchor, footnote or section inside it.
                self.assertEqual(marker.count("<a "), 1, marker)
                self.assertNotIn("<section", marker)
        self.assertIn(">[1, chapter]</a>", markers[0])
        self.assertIn(">[2, chapter]</a>", markers[1])
        # The paragraph is one paragraph, and the footnote is the page's.
        self.assertEqual(body.count("A footnote in a locator"), 1)
        self.assertEqual(len(re.findall(r"<p>A footnote in a locator.*?keep the paragraph whole\.</p>", body, re.S)), 1, body)
        self.assertIn("A detail.", body)
        self.assertNotIn('href="#chapter"', body)


if __name__ == "__main__":
    unittest.main()
