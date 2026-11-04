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

from tests._helpers import (PHOTOGRAPHY_SECTION, load_tool, requires_cabal, script_env, site_binary,
                            write_photo_site_config, write_site_config)


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

    def test_the_browser_kinds_read_content_as_the_generator_does(self) -> None:
        # routes.py's KINDS, for the sweeps (and CI, with no generator), against
        # the route table: each source alone, and whether a build withholds it.
        routes_module = load_tool("browser/routes.py", "route_check_kinds")
        cases = []
        for section, kind in (("poetry", "poem"), ("fiction", "story"), ("blog", "post")):
            cases += [([f"content/{section}/a.md"], kind, True), ([f"content/{section}/set/a.md"], kind, True),
                      ([f"content/{section}/set/index.md"], kind, False),
                      ([f"content/{section}/a.md!"], kind, False), ([f"content/{section}/set/a.md!"], kind, False)]
        cases += [(["content/poetry/set/index.md"], "collection", True), (["content/fiction/set/index.md"], "collection", True),
                  (["content/blog/set/index.md"], "collection", False), (["content/poetry/set/index.md!"], "collection", False),
                  (["content/essays/a.md"], "essay", True), (["content/essays/d/index.md"], "essay", True),
                  (["content/essays/d/a.md"], "essay", True), (["content/essays/a.md!"], "essay", False),
                  (["content/essays/d/index.md!", "content/essays/d/a.md"], "essay", False),
                  (["content/photography/a.md"], "photo-single", True), (["content/photography/s/a.md"], "photo-single", True),
                  (["content/photography/index.md"], "photo-single", False),
                  (["content/photography/s/index.md!", "content/photography/s/a.md"], "photo-single", False),
                  (["content/photography/s/index.md", "content/photography/s/a.md"], "photo-series", True),
                  (["content/photography/s/index.md"], "photo-series", False),
                  (["content/music/w/index.md"], "composition", True), (["content/music/w/index.md!"], "composition", False),
                  (["content/music/w/index.md"], "score-reader", False)]
        music = {"dir": "music", "kind": "Composition", "dingbat": None}
        for sources, kind, expected in cases:
            with self.subTest(sources=sources, kind=kind), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                (root / "tests").mkdir()
                (root / "data").mkdir()
                (root / "tests/routes.yaml").write_text("pages: {home: /}\n")
                for source in sources:
                    draft = source.endswith("!")
                    page(root / source.removesuffix("!"), "title: T\ndate: 2026-01-01" + ("\ndraft: true" if draft else ""))
                config = write_photo_site_config(root)
                config = write_site_config(root, switches={**config["switches"], "music": True},
                                           sections=config["sections"] + [music])
                done = subprocess.run([str(site_binary()), "list-routes"], cwd=root, env=script_env(SITE_ENV="production"),
                                      capture_output=True, text=True, timeout=120)
                self.assertEqual(done.returncode, 0, done.stderr)
                routed = {line.split("\t")[1] for line in done.stdout.splitlines()}
                for source in sources:
                    withheld = source.endswith("!") or any(s.endswith("index.md!") and Path(s).parent == Path(source).parent
                                                           and "/poetry/" not in s and "/fiction/" not in s and "/blog/" not in s
                                                           for s in sources)
                    self.assertEqual(source.removesuffix("!") in routed, not withheld, source)
                routes = routes_module.Routes(root)
                self.assertEqual(routes.has(kind, {k.switch: True for k in routes_module.KINDS.values() if k.switch}), expected)

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
