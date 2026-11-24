"""A citation marker keeps what its author wrote around the key
(build/Citations.hs, markerLabel).

Pandoc keeps a locator in the citation's suffix: `[@PralatWormald,
Theorem 4.1]` is the key and ", Theorem 4.1". The marker's label was the
cluster's numbers alone, so a page cited "[8]" and lost which theorem.
The golden core page shows a locator through the real pipeline; this
covers the shapes a cluster can take, through GHCi as tests/test_pdf_urls.py
does, the label being private to the module.
"""

import json
import subprocess
import unittest

from tests._helpers import CABAL_PROJECT, engine_path, requires_cabal

# (citations as (prefix, key, suffix) Markdown, numbers) → label
CASES = [
    ([("", "a", "")], "[1]"),
    ([("", "a", ""), ("", "b", "")], "[1,2]"),
    ([("", "a", ", Theorem 4.1")], "[1, Theorem 4.1]"),
    ([("see", "a", ", p. 3"), ("", "b", "")], "[see 1, p. 3; 2]"),
    ([("", "a", ", *Lemma* 2")], "[1, <em>Lemma</em> 2]"),
    ([("", "a", ", [ch. 2](https://example.org)")], "[1, ch. 2]"),
    ([("", "a", ", p. <3 & up")], "[1, p. &lt;3 &amp; up]"),
]


@requires_cabal
class MarkerLabels(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        def inlines(md: str) -> str:
            # The suffix as pandoc reads it: Markdown inlines.
            return ("(case runPure (readMarkdown def (T.pack " + json.dumps(md) + ")) of "
                    "Right (Pandoc _ [Para xs]) -> xs; _ -> [])")
        exprs = []
        for cites, _ in CASES:
            numbered = ", ".join(
                f"(Citation (T.pack {json.dumps(key)}) {inlines(pre) if pre else '[]'} "
                f"{inlines(suf) if suf else '[]'} NormalCitation 0 0, {n})"
                for n, (pre, key, suf) in enumerate(cites, start=1))
            exprs.append(f"markerLabel [{numbered}]")
        commands = [
            ":module *Citations",
            "import qualified Data.Text as T",
            "import qualified Data.Text.IO as TIO",
            "mapM_ TIO.putStrLn [" + ", ".join(exprs) + "]",
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
        cls.labels = done.stdout.splitlines()

    def test_labels(self):
        self.assertEqual(len(self.labels), len(CASES), self.labels)
        for (cites, want), got in zip(CASES, self.labels):
            with self.subTest(cites=cites):
                self.assertEqual(got, want)


if __name__ == "__main__":
    unittest.main()
