#!/usr/bin/env python3
"""Golden pages: the Haskell filters, through the real pipeline, against
saved HTML.

The Pandoc filters in build/Filters/ and build/Citations.hs carry most of
the site's behaviour (sidenotes, citations and their back-links, wikilinks,
transclusion and PDF placeholders, small caps, link classification, WebP
<picture>s, score fragments, figure numbering) and had no tests of their
own: a page either built or it did not. A change that moved an id or
dropped a class went unnoticed until a reader followed the link.

Each tests/golden/<name>.md is rendered with ``site render-fixture``
(build/Golden.hs), which runs the same steps as Compilers.essayCompiler
without Hakyll, and compared with tests/golden/<name>.html. A fixture is
never published, and its sources never change, so a difference is always
the pipeline's.

Each renders from a copy of the engine's fixture site, full profile: never
from a real site's directory, whose identity and switches would be in the
page. core.md and initials.md are what every site has; the others are one
switch's feature each (SWITCHED), and leave no markup under the minimal
profile.

When a difference is intended, review it, then regenerate:

    UPDATE_GOLDEN=1 .venv/bin/python3 -m unittest tests.test_golden

Regenerating cannot bless a broken page: InvariantTests checks every render
for the defects a golden diff is easiest to wave through — an anchor that
leads nowhere, a duplicated id, an unresolved figure reference, a wikilink
or directive left in the prose.

The generator is built first (``cabal build``, a no-op when it is current),
so the render is never from a stale binary.
"""

from __future__ import annotations

import difflib
import functools
import html
import os
import re
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

from tests import fixture_variants
from tests._helpers import ENGINE, engine_path, requires_cabal, script_env, site_binary

GOLDEN_DIR = engine_path("tests/golden")
UPDATE = os.environ.get("UPDATE_GOLDEN") == "1"

# Each fixture of one switch's feature, and the markup that feature leaves.
SWITCHED = {
    "archive": 'archive-affordance',
    "code": 'class="language-',
    "code-refs": 'data-code-ref',
    "math": 'class="math',
    "pdf": 'pdf-embed',
    "score": 'data-exhibit-type="score"',
    "source-refs": 'class="source-ref"',
    "transclusion": 'class="transclude"',
    "viz": 'class="viz-figure"',
}
# What every site has: the same page under any profile.
CORE = ("core", "initials")

ID_RE = re.compile(r"""(?<![\w-])id\s*=\s*["']([^"']+)["']""")
LOCAL_HREF_RE = re.compile(r"""(?<![\w-])href\s*=\s*["']#([^"']*)["']""")
CODE_RE = re.compile(r"<(pre|code)\b.*?</\1>", re.S)


def fixtures() -> list[Path]:
    return sorted(GOLDEN_DIR.glob("*.md"))


@functools.cache
def profile_site(profile: str) -> Path:
    """A copy of the fixture site with `profile`'s switches, whose archive
    index the build's fetch stage has written."""
    tmp = tempfile.TemporaryDirectory(prefix=f"golden-{profile}-")
    unittest.addModuleCleanup(tmp.cleanup)
    build = next(b for b in fixture_variants.builds() if b.name == profile)
    site = Path(tmp.name) / "site"
    fixture_variants.copy_site(build, site, ENGINE)
    if build.switches["archive"]:
        subprocess.run([sys.executable, str(engine_path("tools/archive.py")), "fetch"], cwd=site,
                       env=script_env(SITE_ROOT=str(site)), check=True, capture_output=True, timeout=120)
    return site


def render(binary: Path, fixture: Path, profile: str = "full") -> str:
    site = profile_site(profile)
    done = subprocess.run(
        [str(binary), "render-fixture", f"engine/tests/golden/{fixture.name}"],
        cwd=site, env=script_env(SITE_ROOT=str(site)), capture_output=True, text=True,
    )
    if done.returncode:
        raise AssertionError(f"render-fixture {fixture.name} failed:\n{done.stderr}")
    return done.stdout


@requires_cabal
class GoldenTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.binary = site_binary()
        cls.rendered = {f.stem: render(cls.binary, f) for f in fixtures()}

    def test_there_are_fixtures(self) -> None:
        self.assertTrue(self.rendered, f"no fixtures in {GOLDEN_DIR}")

    def test_fixtures_match_their_golden_html(self) -> None:
        for name, actual in self.rendered.items():
            expected_path = GOLDEN_DIR / f"{name}.html"
            if UPDATE:
                expected_path.write_text(actual, encoding="utf-8")
                continue
            # A fixture without its golden page fails rather than writing
            # one: written during `make deploy`, it would be an untracked
            # file in tests/ that stops deploy-recheck with a confusing
            # "inputs changed" (audit T13), and a golden nobody reviewed
            # asserts nothing.
            with self.subTest(fixture=name):
                self.assertTrue(
                    expected_path.exists(),
                    f"tests/golden/{name}.html is missing: review the rendering, "
                    f"then create it with UPDATE_GOLDEN=1",
                )
            if not expected_path.exists():
                continue
            expected = expected_path.read_text(encoding="utf-8")
            with self.subTest(fixture=name):
                if actual != expected:
                    diff = "".join(list(difflib.unified_diff(
                        expected.splitlines(keepends=True),
                        actual.splitlines(keepends=True),
                        f"tests/golden/{name}.html (expected)",
                        f"render-fixture tests/golden/{name}.md",
                    ))[:80])
                    self.fail(
                        f"{name}: the rendered page differs from its golden copy. "
                        f"If the change is intended, review it and run with "
                        f"UPDATE_GOLDEN=1.\n{diff}"
                    )

    def test_rendering_writes_nothing(self) -> None:
        # render-fixture must stay outside the build: no build stamp, no
        # cache, no _site.
        site = profile_site("full")
        stamp = site / "data" / "build-stamp.txt"
        before = stamp.stat().st_mtime_ns if stamp.exists() else None
        render(self.binary, fixtures()[0])
        after = stamp.stat().st_mtime_ns if stamp.exists() else None
        self.assertEqual(before, after, "render-fixture touched data/build-stamp.txt")
        self.assertFalse((site / "_site").exists(), "render-fixture wrote _site")


@requires_cabal
class InvariantTests(unittest.TestCase):
    """What must hold of any render, golden or not."""

    @classmethod
    def setUpClass(cls) -> None:
        binary = site_binary()
        cls.rendered = {f.stem: render(binary, f) for f in fixtures()}

    def test_same_page_anchors_resolve(self) -> None:
        # The page carries the body, the bibliography and the further
        # reading together, so an anchor may land in any of them.
        for name, out in self.rendered.items():
            ids = {html.unescape(i) for i in ID_RE.findall(out)}
            with self.subTest(fixture=name):
                missing = sorted({html.unescape(h) for h in LOCAL_HREF_RE.findall(out)}
                                 - ids - {""})
                self.assertEqual(missing, [], "href=\"#…\" with no matching id")

    def test_ids_are_unique(self) -> None:
        for name, out in self.rendered.items():
            ids = ID_RE.findall(out)
            with self.subTest(fixture=name):
                self.assertEqual(sorted({i for i in ids if ids.count(i) > 1}), [])

    def test_every_image_has_alt(self) -> None:
        # An image without a description must say {.decorative}
        # (build/Filters/Images.hs); check-site refuses a bare <img>.
        for name, out in self.rendered.items():
            prose = CODE_RE.sub("", out)
            with self.subTest(fixture=name):
                bare = [tag for tag in re.findall(r"<img\b[^>]*>", prose)
                        if not re.search(r"(?<![\w-])alt(?:\s*=|\s|/?>)", tag)]
                self.assertEqual(bare, [], "an <img> with no alt")

    def test_nothing_is_left_unprocessed(self) -> None:
        for name, out in self.rendered.items():
            prose = CODE_RE.sub("", out)
            with self.subTest(fixture=name):
                self.assertNotIn("Figure ?", prose, "unresolved figure reference")
                self.assertNotIn("[[", prose, "a wikilink survived")
                self.assertNotIn("{{", prose, "a transclusion or PDF directive survived")
                self.assertNotIn("score-fragment--error", prose)
                self.assertNotIn("viz-error", prose)


@requires_cabal
class ProfileTests(unittest.TestCase):
    """The minimal profile against the full one, fixture by fixture."""

    @classmethod
    def setUpClass(cls) -> None:
        binary = site_binary()
        cls.full = {f.stem: render(binary, f) for f in fixtures()}
        cls.minimal = {f.stem: render(binary, f, "minimal") for f in fixtures()}

    def test_every_fixture_is_the_core_s_or_one_switch_s(self):
        self.assertEqual(set(self.full), set(CORE) | set(SWITCHED))

    def test_the_core_is_the_same_page_under_both_profiles(self):
        for name in CORE:
            with self.subTest(fixture=name):
                self.assertEqual(self.minimal[name], self.full[name])

    def test_a_switched_feature_shows_under_the_full_profile(self):
        # So its absence below isn't a misspelt mark's.
        for name, mark in SWITCHED.items():
            with self.subTest(fixture=name):
                self.assertIn(mark, self.full[name])

    def test_a_switched_feature_leaves_no_markup_under_the_minimal_profile(self):
        for name, mark in SWITCHED.items():
            with self.subTest(fixture=name):
                self.assertNotIn(mark, self.minimal[name])


if __name__ == "__main__":
    unittest.main()
