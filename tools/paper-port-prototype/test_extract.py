"""The prototype's failure handling: every refusal exits 1 and publishes
nothing; marks must match the source's inventory exactly; the converter's
one excused abort is accepted only with fresh, complete output.

    ./make-fixtures.sh       # once, or after the fixtures are lost
    uv run --no-project --with pypdf --with pillow python -I test_extract.py

Fixtures (FIXTURES, default fixtures/ here): proof-broker's builds for the
probes, SIMD's for the genuine converter abort. EXTRACT runs another
extractor against the same tests."""
import importlib.util
import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

from pypdf import PdfReader, PdfWriter

HERE = Path(__file__).resolve().parent
EXTRACT = Path(os.environ.get("EXTRACT", HERE / "extract.py"))
FIXTURES = Path(os.environ.get("FIXTURES", HERE / "fixtures"))
KNOWN_ABORT = "pdftocairo: ../cairo/src/cairo-hash.c:217: _cairo_hash_table_destroy: Assertion `hash_table->live_entries == 0' failed."

spec = importlib.util.spec_from_file_location("inventory", HERE / "inventory.py")
inventory = importlib.util.module_from_spec(spec)
spec.loader.exec_module(inventory)

# Stand-ins for pdftocairo, used only for -svg; anything else is the real one.
STUB_HEAD = f"""#!/usr/bin/python3
import os, subprocess, sys
args = sys.argv[1:]
if "-svg" not in args:
    os.execv("/usr/bin/pdftocairo", ["/usr/bin/pdftocairo"] + args)
KNOWN = {KNOWN_ABORT!r}
def real():
    subprocess.run(["/usr/bin/pdftocairo"] + args, check=True)
"""
STUBS = {
    # The first review's probe: fails, writes nothing.
    "fails-writes-nothing": 'sys.stderr.write("synthetic failure\\n"); sys.exit(1)',
    # Writes a complete SVG, then dies some other way.
    "other-abort": 'real(); sys.stderr.write("pdftocairo: some other assertion\\n"); os.abort()',
    # The known message, but with a nonzero exit that is not an abort.
    "known-message-exit-1": 'real(); sys.stderr.write(KNOWN + "\\n"); sys.exit(1)',
    # The known abort, with nothing written.
    "known-abort-nothing": 'sys.stderr.write(KNOWN + "\\n"); sys.stderr.flush(); os.abort()',
    # The known abort, with the SVG cut short.
    "known-abort-truncated": (
        'real(); p = args[-1]; t = open(p).read(); open(p, "w").write(t[: len(t) // 2] + "</svg>\\n"); '
        'sys.stderr.write(KNOWN + "\\n"); sys.stderr.flush(); os.abort()'),
    # The known abort after a complete SVG: the one excused case.
    "known-abort-complete": 'real(); sys.stderr.write(KNOWN + "\\n"); sys.stderr.flush(); os.abort()',
}


def setUpModule():
    for name in ("proof-broker", "simd"):
        if not (FIXTURES / name / "inventory.json").is_file():
            raise unittest.SkipTest(f"no fixture {FIXTURES / name}: run make-fixtures.sh")


class Extract(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix="test-", dir=HERE))
        self.addCleanup(shutil.rmtree, self.tmp)
        self.paper = self.fixture("proof-broker")
        self.out = self.tmp / "out"
        self.pages = len(PdfReader(self.paper / "A" / "main.pdf").pages)

    def fixture(self, name):
        paper = self.tmp / name
        for v in ("A", "B"):
            (paper / v).mkdir(parents=True)
            shutil.copy(FIXTURES / name / v / "main.pdf", paper / v)
        shutil.copy(FIXTURES / name / "B" / "main.ozyfig", paper / "B")
        shutil.copy(FIXTURES / name / "inventory.json", paper)
        return paper

    def extract(self, stub=None, paper=None):
        env = dict(os.environ)
        if stub:
            bin_ = self.tmp / "bin"
            bin_.mkdir(exist_ok=True)
            s = bin_ / "pdftocairo"
            s.write_text(STUB_HEAD + STUBS[stub] + "\n")
            s.chmod(0o755)
            env["PATH"] = f"{bin_}:{env['PATH']}"
        return subprocess.run([sys.executable, "-I", str(EXTRACT), str(paper or self.paper), str(self.out)],
                              capture_output=True, text=True, env=env)

    def refused(self, r, why):
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)
        self.assertIn("REFUSE:", r.stdout)
        self.assertIn(why, r.stdout)
        self.assertFalse(self.out.exists(), "a refused run published something")

    def published(self, r, n):
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)
        self.assertNotIn("REFUSE", r.stdout)
        self.assertEqual(sorted(p.name for p in self.out.iterdir()), [f"figure-{i}.svg" for i in range(1, n + 1)])

    def edit_marks(self, fn):
        """fn(fields) -> fields, or None to drop the line."""
        f = self.paper / "B" / "main.ozyfig"
        lines = [fn(line.split()) for line in f.read_text().split("\n") if line.strip()]
        f.write_text("".join(" ".join(x) + "\n" for x in lines if x is not None))

    def edit_inventory(self, fn):
        f = self.paper / "inventory.json"
        d = json.loads(f.read_text())
        fn(d)
        f.write_text(json.dumps(d))

    # The baseline, and the output-path rule.

    def test_baseline_publishes(self):
        self.published(self.extract(), 2)

    def test_existing_output_is_refused_and_left_alone(self):
        self.out.mkdir()
        (self.out / "figure-1.svg").write_text("old")
        r = self.extract()
        self.assertEqual(r.returncode, 1)
        self.assertIn("already exists", r.stdout)
        self.assertEqual((self.out / "figure-1.svg").read_text(), "old")

    # The converter.

    def test_converter_failure_without_output_is_refused(self):
        self.refused(self.extract("fails-writes-nothing"), "pdftocairo exited 1")

    def test_other_abort_is_refused_even_with_a_complete_svg(self):
        self.refused(self.extract("other-abort"), "some other assertion")

    def test_known_message_without_abort_is_refused(self):
        self.refused(self.extract("known-message-exit-1"), "pdftocairo exited 1")

    def test_known_abort_without_output_is_refused(self):
        self.refused(self.extract("known-abort-nothing"), "wrote no SVG")

    def test_known_abort_with_truncated_output_is_refused(self):
        self.refused(self.extract("known-abort-truncated"), "not well-formed")

    def test_known_abort_with_complete_output_is_accepted(self):
        r = self.extract("known-abort-complete")
        self.published(r, 2)
        self.assertIn("known cairo teardown abort", r.stdout)

    def test_real_abort_on_simd_is_accepted(self):
        r = self.extract(paper=self.fixture("simd"))
        self.published(r, 6)
        self.assertIn("known cairo teardown abort", r.stdout)

    # The page and region checks.

    def test_overlapping_regions_are_refused(self):
        # Figure 2's marks moved onto figure 1's.
        first = {}
        def move(f):
            if f[0] == "1":
                first[f[1]] = f[2:5]
            elif f[0] == "2":
                f[2:5] = first[f[1]]
            return f
        self.edit_marks(move)
        self.refused(self.extract(), "overlap")

    def test_changed_marked_page_is_refused(self):
        r = PdfReader(self.paper / "B" / "main.pdf")
        w = PdfWriter()
        for i, page in enumerate(r.pages):
            w.add_page(r.pages[i - 1] if i == len(r.pages) - 1 else page)
        with open(self.paper / "B" / "main.pdf", "wb") as f:
            w.write(f)
        self.refused(self.extract(), f"pages [{self.pages}] draw differently")

    def test_page_count_change_is_refused(self):
        r = PdfReader(self.paper / "B" / "main.pdf")
        w = PdfWriter()
        for page in r.pages[:-1]:
            w.add_page(page)
        with open(self.paper / "B" / "main.pdf", "wb") as f:
            w.write(f)
        self.refused(self.extract(), f"{self.pages - 1} pages, the plain build {self.pages}")

    def test_region_cutting_words_is_refused(self):
        # Figure 1's caption starts 14 pt lower: its first line is cut.
        self.edit_marks(lambda f: f[:4] + [str(int(f[4]) - 14 * 65536)] + f[5:] if f[:2] == ["1", "capbegin"] else f)
        self.refused(self.extract(), "figure 1: words cut by its region's edge")

    # The marks against the source's inventory.

    def test_missing_capbegin_is_refused(self):
        # The second review's probe: the crop took in the caption (59 words to 100).
        self.edit_marks(lambda f: None if f[:2] == ["1", "capbegin"] else f)
        self.refused(self.extract(), "figure 1 (fig:pipeline): marks capbegin 0 (expected 1)")

    def test_missing_capend_is_refused(self):
        self.edit_marks(lambda f: None if f[:2] == ["2", "capend"] else f)
        self.refused(self.extract(), "figure 2 (fig:grid): marks capend 0 (expected 1)")

    def test_missing_end_is_refused(self):
        self.edit_marks(lambda f: None if f[:2] == ["1", "end"] else f)
        self.refused(self.extract(), "figure 1 (fig:pipeline): marks end 0 (expected 1)")

    def test_a_declared_figure_without_marks_is_refused(self):
        # The second review's probe: figure 2 had no marks and figure 1 was published alone.
        self.edit_marks(lambda f: None if f[0] == "2" else f)
        self.refused(self.extract(), "figure 2 (fig:grid): declared in the source, but the build recorded no marks")

    def test_no_marks_at_all_is_refused(self):
        (self.paper / "B" / "main.ozyfig").write_text("")
        self.refused(self.extract(), "figure 1 (fig:pipeline): declared in the source, but the build recorded no marks")

    def test_duplicate_mark_is_refused(self):
        f = self.paper / "B" / "main.ozyfig"
        line = next(x for x in f.read_text().split("\n") if x.startswith("1 capbegin "))
        f.write_text(f.read_text() + line + "\n")
        self.refused(self.extract(), "figure 1 (fig:pipeline): marks capbegin 2 (expected 1)")

    def test_marks_for_an_undeclared_figure_are_refused(self):
        f = self.paper / "B" / "main.ozyfig"
        extra = [x.replace("2 ", "3 ", 1) for x in f.read_text().split("\n") if x.startswith("2 ")]
        f.write_text(f.read_text() + "\n".join(extra) + "\n")
        self.refused(self.extract(), "marks name figure 3, which the source does not declare")

    def test_caption_marks_on_an_uncaptioned_figure_are_refused(self):
        self.edit_inventory(lambda d: d["figures"][0].update(captions=0))
        self.refused(self.extract(), "figure 1 (fig:pipeline): marks capbegin 1 (expected 0), capend 1 (expected 0)")

    def test_two_captions_are_refused(self):
        self.edit_inventory(lambda d: d["figures"][1].update(captions=2))
        self.refused(self.extract(), "figure 2 (fig:grid): 2 captions; one caption per figure is supported")

    def test_marks_across_pages_are_refused(self):
        self.edit_marks(lambda f: f[:2] + [str(int(f[2]) + 1)] + f[3:] if f[:2] == ["1", "end"] else f)
        self.refused(self.extract(), "figure 1 (fig:pipeline): its marks span pages")

    def test_missing_inventory_is_refused(self):
        (self.paper / "inventory.json").unlink()
        self.refused(self.extract(), "cannot read the inventory or the marks")

    def test_malformed_mark_is_refused(self):
        f = self.paper / "B" / "main.ozyfig"
        f.write_text(f.read_text() + "1 begin 3\n")
        self.refused(self.extract(), "is not a mark")


class Inventory(unittest.TestCase):
    def paper(self, files):
        tmp = Path(tempfile.mkdtemp(prefix="inv-", dir=HERE))
        self.addCleanup(shutil.rmtree, tmp)
        for name, text in files.items():
            (tmp / name).parent.mkdir(parents=True, exist_ok=True)
            (tmp / name).write_text(text)
        return inventory.inventory(tmp / "main.tex")["figures"]

    def test_follows_inputs_from_the_main_files_directory(self):
        figs = self.paper({
            "main.tex": "\\begin{document}\\input{sections/a}\\end{document}",
            "sections/a.tex": "\\begin{figure*}\\input{figures/f}\\caption{x}\\label{fig:a}\\end{figure*}",
            "figures/f.tex": "\\begin{tikzpicture}\\end{tikzpicture}",
        })
        self.assertEqual(figs, [{"n": 1, "env": "figure*", "label": "fig:a", "captions": 1}])

    def test_comments_subcaptions_and_captionof_are_not_figures_or_captions(self):
        figs = self.paper({"main.tex": (
            "\\begin{document}\n"
            "% \\begin{figure}\\caption{gone}\\end{figure}\n"
            "\\begin{figure}\\subcaption{a}\\subcaption{b}\\caption{whole} 50\\% done\\end{figure}\n"
            "\\begin{minipage}{1cm}\\captionof{figure}{not a float}\\end{minipage}\n"
            "\\begin{figure}\\centering\\includegraphics{x}\\end{figure}\n"
            "\\end{document}")})
        self.assertEqual([(f["captions"], f["label"]) for f in figs], [(1, None), (0, None)])

    def test_a_missing_input_is_an_error(self):
        with self.assertRaises(ValueError):
            self.paper({"main.tex": "\\begin{document}\\input{nowhere}\\end{document}"})


if __name__ == "__main__":
    unittest.main(verbosity=2)
