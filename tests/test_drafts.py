"""`draft: true` withholds a page in production builds (build/Drafts.hs,
audit C06). ``site list-unpublished`` prints exactly what the provider will
ignore; it runs here over a temporary content tree."""

from __future__ import annotations

import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path
from tests._helpers import requires_cabal, site_binary


DRAFT = "---\ntitle: T\ndraft: true\n---\nBody.\n"
LIVE = "---\ntitle: T\n---\nBody.\n"


@requires_cabal
class UnpublishedTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.binary = site_binary()

    def setUp(self):
        self.root = Path(tempfile.mkdtemp(prefix="drafts-"))
        self.addCleanup(shutil.rmtree, self.root)

    def write(self, rel: str, text: str = "x") -> None:
        p = self.root / rel
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(text, encoding="utf-8")

    def unpublished(self) -> list[str]:
        done = subprocess.run([str(self.binary), "list-unpublished"], cwd=self.root,
                              capture_output=True, text=True, timeout=60)
        self.assertEqual(done.returncode, 0, done.stderr)
        return done.stdout.splitlines()

    def test_a_flagged_page_of_any_kind_is_withheld(self):
        for rel in ("content/essays/flat.md", "content/poetry/poem.md", "content/fiction/story.md",
                    "content/blog/post.md", "content/about-me.md", "content/notes/page.md"):
            self.write(rel, DRAFT)
        self.write("content/essays/live.md", LIVE)
        self.assertEqual(self.unpublished(), sorted([
            "file content/about-me.md", "file content/blog/post.md", "file content/essays/flat.md",
            "file content/fiction/story.md", "file content/notes/page.md", "file content/poetry/poem.md"]))

    def test_a_directory_entry_withholds_its_directory(self):
        self.write("content/essays/dir-essay/index.md", DRAFT)
        self.write("content/music/symphony/index.md", DRAFT)
        self.write("content/photography/series/index.md", DRAFT)
        self.assertEqual(self.unpublished(), [
            "dir content/essays/dir-essay", "dir content/music/symphony", "dir content/photography/series",
            "file content/essays/dir-essay/index.md", "file content/music/symphony/index.md",
            "file content/photography/series/index.md"])

    def test_a_collection_landing_withholds_only_itself(self):
        self.write("content/poetry/selected/index.md", DRAFT)
        self.write("content/poetry/selected/poem.md", LIVE)
        self.assertEqual(self.unpublished(), ["file content/poetry/selected/index.md"])

    def test_a_flat_photograph_withholds_its_images(self):
        self.write("content/photography/harbor.md",
                   "---\ntitle: Harbor\nphoto: IMG_0042.jpg\ndraft: true\n---\n")
        self.assertEqual(self.unpublished(), [
            "file content/photography/harbor.md",
            "stem content/photography/IMG_0042.", "stem content/photography/harbor."])

    def test_truthy_spellings_and_non_flags(self):
        self.write("content/a.md", "---\ndraft: yes\n---\n")
        self.write("content/b.md", "---\ndraft: \"true\"\n---\n")
        self.write("content/c.md", "---\ndraft: false\n---\n")
        self.write("content/d.md", "---\ntitle: D\n---\ndraft: true\n")          # body, not front matter
        self.write("content/e.md", "---\nstatus: Draft\n---\n")                  # the epistemic status
        self.write("content/f.md", "---\ndraft: true\n")                         # unterminated: not front matter
        self.write("content/drafts/essays/g.md", DRAFT)                        # drafts/ is never published anyway
        self.assertEqual(self.unpublished(), ["file content/a.md", "file content/b.md"])

    def test_fences_hakyll_reads_and_a_near_miss(self):
        # Hakyll takes a fence of four dashes closed by four; the scanner
        # read only three and published this draft until 2026-10-04.
        self.write("content/a.md", "----\ndraft: true\n----\n")
        # Hakyll takes no front matter here (trailing space on the fence)
        # and would render the YAML as the body; the draft is withheld.
        self.write("content/b.md", "--- \ndraft: true\n---\n")
        self.write("content/c.md", "----\ndraft: true\n---\n")             # unclosed to both
        self.write("content/d.md", "---\ndraft: off\n---\n")
        self.assertEqual(self.unpublished(), ["file content/a.md", "file content/b.md"])

    def test_all_documented_true_values_are_withheld(self):
        for index, flag in enumerate(('true', 'yes', 'on', '1', '"1"', '" t r u e "')):
            self.write(f"content/true{index}.md", f"---\ndraft: {flag}\n---\n")
        self.assertEqual(self.unpublished(), [f"file content/true{i}.md" for i in range(6)])


if __name__ == "__main__":
    unittest.main()
