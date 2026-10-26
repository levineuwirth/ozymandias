"""PDF viewer query values preserve filenames through one query decode."""

import json
import subprocess
import unittest
from urllib.parse import parse_qs, urlsplit

from bs4 import BeautifulSoup
from tests._helpers import ENGINE, engine_path, requires_cabal
PATHS = [
    "/papers/result%20final.pdf",
    "/papers/result%2Ffinal.pdf",
    "/papers/100%.pdf",
    "/papers/result%ZZ.pdf",
    "/papers/research & notes+appendix?.pdf",
    '/papers/café "quoted".pdf',
]


@requires_cabal
class PdfViewerUrls(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # Exercise the shared builder and the Related renderer which lost
        # its percent encoding during consolidation. GHCi exposes the actual
        # private renderer without a test-only command in the site binary.
        paths = json.dumps(PATHS, ensure_ascii=False)
        commands = [
            ":module *SimilarLinks",
            "import qualified Utils as U",
            "import qualified Data.ByteString.Lazy.Char8 as LBS",
            "LBS.putStrLn (Aeson.encode "
            "[(U.pdfViewerUrl p, renderSimilarLinks "
            '[SimilarEntry (p ++ "#page=5") "Probe"]) | p <- ' + paths + "])",
            ":quit",
        ]
        done = subprocess.run(
            ["cabal", "exec", "--", "ghc", "--interactive", "-ignore-dot-ghci", "-v0",
             f"-i{engine_path('build')}", str(engine_path("build/SimilarLinks.hs"))],
            cwd=ENGINE, input="\n".join(commands) + "\n", text=True,
            capture_output=True, check=True, timeout=60,
        )
        if "error:" in done.stderr:
            raise AssertionError(done.stderr)
        cls.rendered = json.loads(done.stdout)

    def test_viewer_query_preserves_the_original_path(self):
        self.assertEqual(len(self.rendered), len(PATHS))
        for path, (url, _) in zip(PATHS, self.rendered):
            with self.subTest(path=path):
                parsed = urlsplit(url)
                self.assertEqual(parsed.path, "/pdfjs/web/viewer.html")
                self.assertEqual(parse_qs(parsed.query), {"file": [path]})
                self.assertEqual(parsed.fragment, "")

    def test_related_pdf_preserves_the_path_and_page_fragment(self):
        for path, (_, html) in zip(PATHS, self.rendered):
            with self.subTest(path=path):
                link = BeautifulSoup(html, "html.parser").select_one("a.pdf-link")
                self.assertIsNotNone(link)
                parsed = urlsplit(link["href"])
                self.assertEqual(parse_qs(parsed.query), {"file": [path]})
                self.assertEqual(parsed.fragment, "page=5")
                self.assertEqual(link["data-pdf-src"], path + "#page=5")


if __name__ == "__main__":
    unittest.main()
