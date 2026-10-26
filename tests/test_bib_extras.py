"""build/BibExtras.hs, the scanner behind /bibliography/'s keyword pages and
PDF links, through `site bib-extras FILE` (key, file, keywords per line).
A malformed or fieldless entry must not swallow the next one."""

import subprocess
import tempfile
import unittest
from pathlib import Path
from tests._helpers import requires_cabal, site_binary



@requires_cabal
class BibExtrasTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.binary = site_binary()

    def scan(self, text: str) -> dict[str, tuple[str, str]]:
        with tempfile.NamedTemporaryFile("w", suffix=".bib", delete=False) as f:
            f.write(text)
        self.addCleanup(Path(f.name).unlink)
        done = subprocess.run([str(self.binary), "bib-extras", f.name], capture_output=True, text=True, timeout=60)
        self.assertEqual(done.returncode, 0, done.stderr)
        return {k: (pdf, kws) for k, pdf, kws in (line.split("\t") for line in done.stdout.splitlines())}

    def test_an_unterminated_entry_does_not_take_the_next_entrys_fields(self):
        self.assertEqual(self.scan("@misc{broken\n@misc{good, file = {/good.pdf}}\n"),
                         {"good": ("/good.pdf", "")})

    def test_a_fieldless_entry_keeps_its_key_and_the_next_entry_survives(self):
        self.assertEqual(self.scan("@misc{empty}\n@article{after, keywords = {a, b}}\n"),
                         {"empty": ("", ""), "after": ("", "a,b")})

    def test_a_malformed_entry_does_not_drop_the_rest_of_the_file(self):
        out = self.scan("@article{first, file = {/1.pdf}}\n@misc{bad no comma\n"
                        "@book{last, keywords = {x}}\n")
        self.assertEqual(out["first"], ("/1.pdf", ""))
        self.assertEqual(out["last"], ("", "x"))

    def test_string_and_comment_blocks_are_not_entries(self):
        self.assertEqual(self.scan('@string{j = "Journal"}\n@comment{ note }\n@misc{k, keywords = {y}}\n'),
                         {"k": ("", "y")})


if __name__ == "__main__":
    unittest.main()
