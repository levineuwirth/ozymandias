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
brotli or Docker; plus the CV/résumé variant resolver's suite
(yaml-source/tests). A module added to engine/tests/ or tests/ runs here only
once listed.

What they need from outside Python, all of which a test skips without:
node, sha256sum, gpg and gpgconf, rsync, flock, and borg (the off-host
restore test); and a non-root user. The runner has all but borg, which the
workflow installs (Ubuntu's 1.2.8 passes; checked locally). The first run
showed why this list matters: that test checks for borg with
`which('borg')`, a spelling the selection's scan did not look for.

    python3 engine/tests/ci.py
"""

from __future__ import annotations

import os
import sys
import unittest
from pathlib import Path

# The repository the suite runs in: $SITE_ROOT, else the working directory,
# as for `make test` (tests/_helpers.py); and the engine these tests are in.
SITE_ROOT = Path(os.environ.get("SITE_ROOT") or os.getcwd()).resolve()
ENGINE = Path(__file__).resolve().parents[1]

MODULES = [
    "test_annotations", "test_anubis_update", "test_archive_fetch", "test_archive_probe",
    "test_backup_pair", "test_bibliography", "test_build_inputs", "test_build_time",
    "test_check_site", "test_couchdb_backup", "test_couchdb_update", "test_cv_pdfs",
    "test_deploy_guard", "test_dynamic_assets", "test_embed_extract", "test_font_sources",
    "test_fonts", "test_forgejo_backup_offhost", "test_forgejo_update", "test_front_matter",
    "test_js_dates", "test_photo_tools", "test_pin_check", "test_popup_providers",
    "test_prune_site", "test_score_reader", "test_sitelib", "test_thumbnails",
    "test_viz_lifecycle", "test_vps_config_backup", "test_wikilink_routes",
    "test_with_lock",
]


def run(suite: unittest.TestSuite) -> unittest.TestResult:
    return unittest.TextTestRunner(verbosity=2).run(suite)


def main() -> int:
    # `tests` is one namespace package over the site's tests/ and the
    # engine's, as `make test` has it (the Makefile's test target).
    sys.path[:0] = [str(SITE_ROOT), str(ENGINE)]
    os.chdir(SITE_ROOT)
    results = [run(unittest.defaultTestLoader.loadTestsFromNames(
        [f"tests.{m}" for m in MODULES]))]
    # As `make test` runs it: from yaml-source/, discovering tests/.
    os.chdir(SITE_ROOT / "yaml-source")
    results.append(run(unittest.defaultTestLoader.discover("tests")))
    skipped = [s for r in results for s in r.skipped]
    if skipped:
        print(f"\nci: {len(skipped)} test(s) skipped; every test here must run:", file=sys.stderr)
        for test, reason in skipped:
            print(f"  {test.id()}: {reason}", file=sys.stderr)
        return 1
    return 0 if all(r.wasSuccessful() for r in results) else 1


if __name__ == "__main__":
    sys.exit(main())
