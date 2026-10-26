#!/usr/bin/env python3
"""The tests the GitHub workflow runs (.github/workflows/tests.yml).

A stock runner has Python, Node, git, gpg and rsync, but not the Haskell
generator, a built _site, or the media and search tools. The modules below
need none of those, and here none of their tests may skip: a skip means the
runner lacked something, and a badge that goes green by skipping says
nothing. The full suite is `make test` and `make test-clean` on the
author's machine, and every deploy runs it.

Chosen 2026-10-06 from `make test-clean`'s run: the modules with no skips
there that use neither the generator nor ImageMagick, exiftool, Pagefind,
brotli or Docker. The engine's are listed below; the site lists its own
modules, and suites elsewhere (each discovered as `<dir>/tests` from
`<dir>/`), in its tests/ci.yaml. A module runs here only once listed.

What the engine's need from outside Python, all of which a test skips
without: node, sha256sum, gpg and gpgconf, rsync and flock, and a non-root
user, all on a stock runner. A site's workflow installs what its own
modules need beyond that. The first run showed why such a list matters: a
test checked for borg with `which('borg')`, a spelling the selection's
scan did not look for.

A skip a switch explains, "switch off: <name>" (tests/target.py), is
accepted when the site's config does turn that switch off.

    python3 engine/tests/ci.py
"""

from __future__ import annotations

import os
import sys
import unittest
from pathlib import Path

import yaml

# The repository the suite runs in: $SITE_ROOT, else the working directory,
# as for `make test` (tests/_helpers.py); and the engine these tests are in.
SITE_ROOT = Path(os.environ.get("SITE_ROOT") or os.getcwd()).resolve()
ENGINE = Path(__file__).resolve().parents[1]

MODULES = [
    "test_annotations", "test_archive_fetch", "test_archive_probe", "test_build_inputs",
    "test_build_time", "test_check_site", "test_deploy_guard", "test_dynamic_assets",
    "test_embed_extract", "test_font_sources", "test_fonts", "test_front_matter",
    "test_js_dates", "test_photo_tools", "test_pin_check", "test_popup_providers",
    "test_prune_site", "test_score_reader", "test_sitelib", "test_thumbnails",
    "test_viz_lifecycle", "test_wikilink_routes", "test_with_lock",
]
SITE_CI = "tests/ci.yaml"


def site_ci(root: Path) -> dict[str, list[str]]:
    """The site's own modules and suites, from its tests/ci.yaml; a site
    without one has none."""
    path = root / SITE_CI
    document = (yaml.safe_load(path.read_text(encoding="utf-8")) if path.is_file() else None) or {}
    listed = {key: document.get(key) or [] for key in ("modules", "suites")}
    if set(document) - set(listed) or not all(
            isinstance(v, list) and all(isinstance(e, str) and e for e in v) for v in listed.values()):
        raise ValueError(f"{SITE_CI}: only lists of names under modules and suites")
    return listed


def unexpected_skips(skipped: list, settings: dict | None = None) -> list:
    """Every skip, but one a switch explains that the site (or `settings`)
    does turn off."""
    from tests.target import SWITCH_OFF, switch_on
    return [(test, reason) for test, reason in skipped
            if not (reason.startswith(SWITCH_OFF)
                    and not switch_on(reason.removeprefix(SWITCH_OFF), settings))]


def run(suite: unittest.TestSuite) -> unittest.TestResult:
    return unittest.TextTestRunner(verbosity=2).run(suite)


def main() -> int:
    # `tests` is one namespace package over the site's tests/ and the
    # engine's, as `make test` has it (the Makefile's test target).
    sys.path[:0] = [str(SITE_ROOT), str(ENGINE)]
    os.chdir(SITE_ROOT)
    site = site_ci(SITE_ROOT)
    results = [run(unittest.defaultTestLoader.loadTestsFromNames(
        [f"tests.{m}" for m in MODULES + site["modules"]]))]
    # As `make test` runs them: from the directory, discovering tests/.
    for suite in site["suites"]:
        if not (SITE_ROOT / suite / "tests").is_dir():
            print(f"ci: {SITE_CI} names {suite}/, which has no tests/", file=sys.stderr)
            return 1
        os.chdir(SITE_ROOT / suite)
        results.append(run(unittest.defaultTestLoader.discover("tests")))
        os.chdir(SITE_ROOT)
    skipped = unexpected_skips([s for r in results for s in r.skipped])
    if skipped:
        print(f"\nci: {len(skipped)} test(s) skipped; every test here must run:", file=sys.stderr)
        for test, reason in skipped:
            print(f"  {test.id()}: {reason}", file=sys.stderr)
        return 1
    return 0 if all(r.wasSuccessful() for r in results) else 1


if __name__ == "__main__":
    sys.exit(main())
