"""The build refuses two items on one route (build/RouteCheck.hs).

Hakyll's own check sees only items compiled in the same run, after both
have written the file; an incremental build let a new page collection
named like a tag replace the tag page with exit 0. These run the
generator over small scratch trees: the rules are evaluated, and the
refusal comes before anything compiles, so no templates are needed.
"""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import requires_cabal, script_env, site_binary, write_site_config


def page(path: Path, front: str, body: str = "Body.") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(f"---\n{front}\n---\n\n{body}\n", encoding="utf-8")


def scratch_site(root: Path) -> None:
    """An essay tagged `notes` and `photography/denmark/harbor`, a page
    collection `notes`, and a photo series `denmark` with a `harbor` entry:
    three routes claimed twice."""
    (root / "data").mkdir()
    page(root / "content/essays/a.md",
         "title: A\ndate: 2026-01-01\ntags: [notes, photography/denmark/harbor]")
    page(root / "content/notes/index.md", "title: Notes")
    page(root / "content/photography/denmark/index.md",
         "title: Denmark\ndate: 2026-01-01\ntags: [photography]")
    page(root / "content/photography/denmark/harbor.md",
         "title: Harbor\ndate: 2026-01-02\nphoto: harbor.jpg\nseries: denmark\n"
         "tags: [photography]", body="")


@requires_cabal
class RouteCheckTests(unittest.TestCase):
    def run_site(self, root: Path, *args: str) -> subprocess.CompletedProcess[str]:
        write_site_config(root)
        return subprocess.run([str(site_binary()), *args], cwd=root, env=script_env(),
                              capture_output=True, text=True, timeout=120)

    def test_list_routes_names_every_claimant(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            scratch_site(root)
            done = self.run_site(root, "list-routes")
            self.assertEqual(done.returncode, 0, done.stderr)
            rows = [line.split("\t") for line in done.stdout.splitlines()]
            claims: dict[str, list[str]] = {}
            for route, source in rows:
                claims.setdefault(route, []).append(source)
            self.assertEqual(claims["notes/index.html"],
                             ["content/notes/index.md", "notes/index.html"])
            self.assertEqual(claims["photography/denmark/harbor/index.html"],
                             ["content/photography/denmark/harbor.md",
                              "photography/denmark/harbor/index.html"])
            self.assertEqual(claims["essays/a.html"], ["content/essays/a.md"])

    def test_expand_tags_is_the_build_expansion(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            # The sections' landings come from site.yaml.
            write_site_config(Path(directory))
            done = subprocess.run([str(site_binary()), "expand-tags"], cwd=directory,
                                  input="photography//denmark/harbor\nmusic\nnotes/a/\n",
                                  env=script_env(), capture_output=True, text=True, timeout=60)
        self.assertEqual(done.returncode, 0, done.stderr)
        # Ancestors added, empty segments dropped, section names left out.
        self.assertEqual(done.stdout.splitlines(),
                         ["photography/denmark", "photography/denmark/harbor", "notes", "notes/a"])

    def test_build_refuses_before_writing_anything(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            scratch_site(root)
            done = self.run_site(root, "build")
            self.assertNotEqual(done.returncode, 0)
            self.assertIn("Two or more items route to the same file", done.stderr)
            self.assertIn("notes/index.html <- content/notes/index.md, notes/index.html",
                          done.stderr)
            self.assertIn("photography/denmark/harbor/index.html <- "
                          "content/photography/denmark/harbor.md", done.stderr)
            self.assertFalse((root / "_site").exists())

    def test_a_tree_without_clashes_gets_past_the_check(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "data").mkdir()
            page(root / "content/essays/a.md", "title: A\ndate: 2026-01-01\ntags: [notes]")
            done = self.run_site(root, "build")
            # Compiling fails here (no templates); the check must not.
            self.assertNotIn("Two or more items route to the same file", done.stderr)
            self.assertIn("Compiling", done.stdout)


if __name__ == "__main__":
    unittest.main()
