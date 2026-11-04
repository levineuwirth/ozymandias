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

from tests._helpers import PHOTOGRAPHY_SECTION, requires_cabal, script_env, site_binary, write_photo_site_config, write_site_config


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
        write_photo_site_config(root)
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
        # Ancestors added, empty segments dropped, section names left out:
        # the sections' landings come from site.yaml, and music and
        # photography are ones only with their switches on.
        music = {"dir": "music", "kind": "Composition", "dingbat": None}
        for on, kept, photo in ((False, ["music"], ["photography"]), (True, [], [])):
            with self.subTest(on=on), tempfile.TemporaryDirectory() as directory:
                config = write_site_config(Path(directory))
                if on:
                    write_site_config(Path(directory), switches={"music": True, "photography": True},
                                      sections=config["sections"] + [music, PHOTOGRAPHY_SECTION])
                done = subprocess.run([str(site_binary()), "expand-tags"], cwd=directory,
                                      input="photography//denmark/harbor\nmusic\nnotes/a/\n",
                                      env=script_env(), capture_output=True, text=True, timeout=60)
                self.assertEqual(done.returncode, 0, done.stderr)
                self.assertEqual(done.stdout.splitlines(),
                                 [*photo, "photography/denmark", "photography/denmark/harbor", *kept, "notes", "notes/a"])

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

    def test_a_link_to_a_page_a_switch_leaves_unbuilt_is_refused(self) -> None:
        # site.yaml's navigation or library see-also linking the page of a
        # switch it has off (its registry routes), unless another item
        # builds it: a top-level page new.md, or a tag stats once the
        # stats-page switch leaves /stats/ free.
        new = {"label": "New", "href": "/new.html#top"}
        stats = {"label": "Statistics", "href": "/stats/"}
        library = {"shelf-cap": 4, "shelf-max": 5, "description": "Every shelf.", "see-also": [new]}
        refused_new = "/new.html#top (the new switch is off)"
        for switches, changes, also, refusal in (
                ({"new": True}, {"nav-primary": [new]}, None, None),
                ({"new": False}, {"nav-primary": [new]}, None, refused_new),
                ({"new": False}, {"library": library}, None, refused_new),
                ({"new": False}, {"nav-primary": [new]}, "content/new.md", None),
                ({"stats-page": False}, {"nav-primary": [stats]}, None,
                 "/stats/ (the stats-page switch is off)"),
                ({"stats-page": False, "tag-pages": True}, {"nav-primary": [stats]},
                 "content/essays/b.md", None)):
            with self.subTest(switches=switches, changes=sorted(changes), also=also), \
                    tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                (root / "data").mkdir()
                page(root / "content/essays/a.md", "title: A\ndate: 2026-01-01\ntags: [notes]")
                if also:
                    page(root / also, "title: Also\ndate: 2026-01-02\ntags: [stats]")
                write_site_config(root, switches=switches, **changes)
                done = subprocess.run([str(site_binary()), "build"], cwd=root, env=script_env(),
                                      capture_output=True, text=True, timeout=120)
                if refusal:
                    self.assertNotEqual(done.returncode, 0)
                    self.assertIn(refusal, done.stderr)
                    self.assertFalse((root / "_site").exists())
                else:
                    self.assertNotIn("link pages that are not built", done.stderr)
                    self.assertIn("Compiling", done.stdout)


if __name__ == "__main__":
    unittest.main()
