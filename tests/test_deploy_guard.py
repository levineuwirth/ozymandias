#!/usr/bin/env python3
"""tools/deploy-guard.sh refuses an rsync --delete aimed at the wrong place
(audit D04): a malformed VPS_PATH, a destination that is not the live site,
or a transfer that would delete too much.

Run with: ``python3 -m unittest tests.test_deploy_guard``.
"""

from __future__ import annotations

import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import engine_path, script_env

GUARD = engine_path("tools/deploy-guard.sh")


def guard(*args: str, **env: str) -> subprocess.CompletedProcess:
    # script_env drops the deploy's own DEPLOY_* overrides, which reach this
    # suite when `make deploy` runs it; each test sets what it needs.
    return subprocess.run(["bash", str(GUARD), *args], capture_output=True, text=True,
                          env=script_env(**env))


class Docroot(unittest.TestCase):
    def test_accepted_and_normalised(self):
        for raw in ("/srv/http/levineuwirth.org", "/srv/http/levineuwirth.org/",
                    "  /srv/http/levineuwirth.org  "):
            done = guard("docroot", raw)
            self.assertEqual((done.returncode, done.stdout.strip()),
                             (0, "/srv/http/levineuwirth.org"), raw)

    def test_refused(self):
        for raw in ("", "/", "/var/www/", "/var/www", "/srv/http", "/home/levi",
                    "srv/http/site", "/srv/http/site # docroot", "/srv/http/site//",
                    "/srv//http/site", "/srv/http/../etc/x"):
            self.assertEqual(guard("docroot", raw).returncode, 1, repr(raw))


@unittest.skipUnless(shutil.which("rsync"), "rsync not on PATH")
class Target(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.site = self.root / "_site"
        self.site.mkdir()
        (self.site / "index.html").write_text("<!doctype html>")
        self.live = self.root / "www" / "site"
        shutil.copytree(self.site, self.live)

    def dest(self, path: Path) -> str:
        return f"{path}/"

    def test_live_docroot_passes_and_nothing_is_deleted(self):
        (self.live / "stale.txt").write_text("old")
        done = guard("target", str(self.site), self.dest(self.live))
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertIn("deletes 1 file", done.stdout)
        self.assertTrue((self.live / "stale.txt").exists())

    def test_directory_without_index_is_refused(self):
        # A fresh directory, or the parent one level too high.
        for path in (self.root / "fresh", self.root / "www"):
            path.mkdir(exist_ok=True)
            done = guard("target", str(self.site), self.dest(path))
            self.assertEqual(done.returncode, 1, path)
            self.assertIn("not the live site", done.stderr)
        self.assertEqual(guard("target", str(self.site), self.dest(self.root / "fresh"),
                               DEPLOY_NEW_DOCROOT="1").returncode, 0)

    def test_mass_delete_is_refused(self):
        for i in range(12):
            (self.live / f"x{i}").write_text("x")
        done = guard("target", str(self.site), self.dest(self.live), DEPLOY_MAX_DELETE="10")
        self.assertEqual(done.returncode, 1)
        self.assertIn("would delete 12 files", done.stderr)
        self.assertEqual(guard("target", str(self.site), self.dest(self.live),
                               DEPLOY_MAX_DELETE="10", DEPLOY_ALLOW_DELETE="1").returncode, 0)
        self.assertEqual(len(list(self.live.glob("x*"))), 12)

    def test_empty_site_is_refused(self):
        (self.site / "index.html").write_text("")
        self.assertEqual(guard("target", str(self.site), self.dest(self.live)).returncode, 1)


if __name__ == "__main__":
    unittest.main()
