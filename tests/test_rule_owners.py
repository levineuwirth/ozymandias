"""One rule for one item (build/RouteCheck.hs).

Hakyll keeps one compiler of two for the same item and says nothing, so
the build refuses a second rule for an item. Rules that copied the same
file to the same place were harmless before that and stop the build now:
an essay's own script (the per-page script rule and the essay's assets'),
a photograph's (and the photography assets'), a directory's score
fragments under two standalone pages with scores, a fragment named like
a monogram (and the monograms' rule), a dimension sidecar kept among the
stylesheets or the scripts (and theirs).

A scratch site with a file of every kind beside every kind of page, the
sections and their switches on, and its rules checked as a build and as a
dev build check them: `site build --dry-run` evaluates the rules, runs
the checks and compiles nothing. `site list-routes` does not run them (it
prints the table for a site whose build would be refused, too), so it is
read here only for where each file goes.

    python -m unittest tests.test_rule_owners -v
"""

from __future__ import annotations

import json
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import engine_path, requires_cabal, script_env, site_binary, write_site_config

# What sits beside pages: scripts, images, documents, data, the sidecars
# the build reads, a monogram by each of its names.
FILES = ("page.js", "figure.svg", "plate.png", "photo.jpg", "plate.webp", "paper.pdf", "take.mp3",
         "data.json", "table.csv", "sheet.css", "notes.txt", "refs.bib", "meta.yaml", "plate.dims.yaml",
         "photo.exif.yaml", "photo.palette.yaml", "mark.svg", "page.mark.svg", "plot.py", "frame.html")

# Every shape of directory the rules know, each with its pages.
PAGES = {
    "content": ("index.md", "about.md"),
    "content/essays": ("flat.md",),
    "content/essays/long": ("index.md",),
    "content/essays/long/figures": (),
    "content/essays/long/deep/er": (),
    "content/essays/cycle": ("index.md", "one.md"),
    "content/blog": ("flat.md",),
    "content/blog/cycle": ("index.md", "one.md"),
    "content/blog/cycle/deep": (),
    "content/poetry": ("flat.md",),
    "content/poetry/cycle": ("index.md", "one.md"),
    "content/fiction": ("flat.md",),
    "content/fiction/cycle": ("index.md", "one.md"),
    "content/music": ("index.md",),
    "content/music/piece": ("index.md",),
    "content/music/piece/scores": (),
    "content/photography": ("index.md", "single.md"),
    "content/photography/series": ("index.md", "frame.md"),
    "content/photography/series/deep": (),
    # Two standalone pages with scores, of one directory.
    "content/works": ("first.md", "second.md"),
    "content/works/scores": (),
    # A page collection.
    "content/jottings": ("index.md", "one.md"),
    "content/jottings/deep": (),
    "content/drafts": (),
    "content/drafts/essays": ("flat.md",),
    "content/drafts/essays/draft": ("index.md",),
    "content/drafts/essays/draft/figures": (),
    "content/drafts/blog": ("flat.md",),
    "content/scripts": (),
    "content/tag-meta": ("notes.md",),
    "content/bibliography-meta": ("notes.md",),
    "static": (),
    "static/images": (),
    "static/js": (),
    "static/css": (),
    "static/papers": (),
    "code-refs/github/owner": (),
    "data": (),
}

SECTIONS = [{"dir": d, "kind": k, "dingbat": None} for d, k in (
    ("essays", "Essay"), ("blog", "Post"), ("poetry", "Poem"), ("fiction", "Fiction"),
    ("music", "Composition"), ("photography", "Photograph"))]
STANDALONE = [{"source": f"content/works/{name}.md", "route": f"works/{name}.html", "template": "essay.html",
               "context": "essay", "scores": True, "dingbat": None, "flags": []} for name in ("first", "second")] \
    + [{"source": "content/about.md", "route": "about.html", "template": "page.html", "context": "page",
        "scores": False, "dingbat": None, "flags": []}]
SWITCHES = {"music": True, "photography": True, "marks": True, "viz": True, "tag-pages": True, "authors": True,
            "popups": True, "code": True, "source-refs": True,
            "new": True, "stats-page": True, "build-page": True, "bibliography": True, "bibliography-index": True,
            "archive": True, "library": True, "commonplace": True}


@requires_cabal
class OneRuleForOneItem(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory(prefix="rule-owners-")
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        (self.root / "engine").symlink_to(engine_path(""))
        for directory, pages in PAGES.items():
            where = self.root / directory
            where.mkdir(parents=True, exist_ok=True)
            for name in pages:
                (where / name).write_text(f"---\ntitle: {directory} {name}\ndate: 2026-01-02\ntags: [notes]\n"
                                          "---\n\nBody.\n", encoding="utf-8")
            for name in FILES:
                (where / name).write_text("", encoding="utf-8")
        # Site files that shadow the engine's, by name.
        for shadow in ("static/js/utils.js", "static/js/nav.js", "static/css/reset.css", "static/css/layout.css",
                       "static/favicon.svg"):
            self.assertTrue(engine_path(shadow).is_file(), shadow)
            (self.root / shadow).write_text("", encoding="utf-8")
        config = write_site_config(self.root, sections=SECTIONS, standalone=STANDALONE, switches=SWITCHES)
        (self.root / "site.yaml").write_text(json.dumps(config), encoding="utf-8")

    def site(self, environment: str, *args: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run([str(site_binary()), *args], cwd=self.root,
                              env=script_env(SITE_ENV=environment, SITE_THREADS="1"),
                              capture_output=True, text=True, timeout=240)

    def routes(self, environment: str) -> dict[str, list[str]]:
        checked = self.site(environment, "build", "--dry-run")
        self.assertEqual(checked.returncode, 0, checked.stdout[-3000:] + checked.stderr)
        done = self.site(environment, "list-routes")
        self.assertEqual(done.returncode, 0, done.stdout[-2000:] + done.stderr)
        routed: dict[str, list[str]] = {}
        for route, _, source in (line.partition("\t") for line in done.stdout.splitlines()):
            routed.setdefault(source, []).append(route)
        return routed

    def test_no_file_beside_a_page_has_two_rules(self):
        for environment in ("build", "dev"):
            with self.subTest(environment=environment):
                routed = self.routes(environment)
                # The cases that stopped a build, each still published once.
                for source, route in (("content/essays/long/page.js", "essays/long/page.js"),
                                      ("content/photography/series/page.js", "photography/series/page.js"),
                                      ("content/photography/page.js", "photography/page.js"),
                                      ("content/blog/cycle/page.js", "blog/cycle/page.js"),
                                      ("content/works/scores/figure.svg", "scores/figure.svg")):
                    self.assertEqual(routed.get(source), [route], source)
                drafts = {s: r for s, r in routed.items() if s.startswith("content/drafts/")}
                if environment == "dev":
                    self.assertEqual(drafts.get("content/drafts/essays/draft/page.js"),
                                     ["drafts/essays/draft/page.js"])
                    self.assertEqual(drafts.get("content/drafts/blog/page.js"), ["drafts/blog/page.js"])
                else:
                    self.assertEqual(drafts, {})

    def test_the_check_is_the_build_s(self):
        # What this module relies on: the dry run refuses what a build
        # refuses. A feed at sitemap.xml is two rules for one item.
        config = write_site_config(self.root, sections=SECTIONS, standalone=STANDALONE, switches=SWITCHES,
                                   feeds=[{"path": "sitemap.xml", "title": "Notes", "description": "Notes.",
                                           "sections": ["blog"], "advertise": True, "limit": 30}])
        (self.root / "site.yaml").write_text(json.dumps(config), encoding="utf-8")
        done = self.site("build", "build", "--dry-run")
        self.assertNotEqual(done.returncode, 0)
        self.assertIn("Two or more rules compile the same item", done.stdout + done.stderr)

FRAGMENT = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><title>{}</title><path d="M0 0h10"/></svg>\n'
SCORED = """---
title: {title}
date: 2026-01-02
---

::: {{.score-fragment score-name="Motif" score-caption="A motif."}}
![](scores/sample.svg)
:::
"""


@requires_cabal
class SharedScoreFragments(unittest.TestCase):
    """Two standalone pages with scores in one directory: its fragments
    have one copy rule, and each page is rebuilt when one changes."""

    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory(prefix="shared-scores-")
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        (self.root / "engine").symlink_to(engine_path(""))
        (self.root / "content/works/scores").mkdir(parents=True)
        (self.root / "content/index.md").write_text("---\ntitle: Home\nhome: true\n---\n\nHome.\n", encoding="utf-8")
        for name in ("first", "second"):
            (self.root / f"content/works/{name}.md").write_text(SCORED.format(title=name), encoding="utf-8")
        self.fragment = self.root / "content/works/scores/sample.svg"
        self.fragment.write_text(FRAGMENT.format("engraved-once"), encoding="utf-8")
        config = write_site_config(
            self.root, standalone=[s for s in STANDALONE if s["scores"]], switches={"music": True},
            sections=[s for s in SECTIONS if s["dir"] != "photography"])
        (self.root / "site.yaml").write_text(json.dumps(config), encoding="utf-8")

    def site(self, command: str) -> None:
        done = subprocess.run([str(site_binary()), command], cwd=self.root, env=script_env(SITE_THREADS="1"),
                              capture_output=True, text=True, timeout=240)
        self.assertEqual(done.returncode, 0, done.stdout[-3000:] + done.stderr[-3000:])

    def pages(self) -> list[str]:
        return [(self.root / f"_site/works/{name}.html").read_text(encoding="utf-8") for name in ("first", "second")]

    def test_both_pages_follow_the_fragment_they_share(self):
        self.site("rebuild")
        self.assertTrue((self.root / "_site/scores/sample.svg").is_file())
        for page in self.pages():
            self.assertIn("engraved-once", page)
        self.fragment.write_text(FRAGMENT.format("engraved-again"), encoding="utf-8")
        self.site("build")
        for page in self.pages():
            self.assertIn("engraved-again", page)
            self.assertNotIn("engraved-once", page)
        self.assertIn("engraved-again", (self.root / "_site/scores/sample.svg").read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
