"""Revisions captured before compilation must still describe the output."""
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from tests._helpers import engine_path, script_env

TOOL = engine_path("tools/build-provenance.py")


class BuildProvenance(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory(prefix="build-provenance-")
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.site = self.root / "site"
        self.engine = self.root / "engine"
        for directory in (self.site, self.engine):
            directory.mkdir()
            self.git(directory, "init", "-qb", "main")
            (directory / ".gitignore").write_text("data/\n_site/\n")
            (directory / "input").write_text("A\n")
            self.git(directory, "add", ".")
            self.git(directory, "commit", "-qm", "A")
        (self.site / "engine").symlink_to(self.engine)
        self.origin = self.root / "origin.git"
        self.git(self.root, "init", "--bare", "-q", str(self.origin))
        self.git(self.engine, "remote", "add", "origin", str(self.origin))
        self.git(self.engine, "push", "-q", "origin", "main")

    def git(self, directory, *args):
        return subprocess.check_output(["git", "-c", "user.name=Test", "-c", "user.email=test@example.org",
                                        "-c", "commit.gpgsign=false", "-C", str(directory), *args],
                                       stderr=subprocess.PIPE).decode().strip()

    def run_tool(self, command):
        return subprocess.run([sys.executable, str(TOOL), command], cwd=self.site,
                              env=script_env(), capture_output=True, text=True)

    def test_a_clean_remote_backed_move_during_build_is_refused(self):
        self.assertEqual(self.run_tool("begin").returncode, 0)
        (self.engine / "input").write_text("B\n")
        self.git(self.engine, "commit", "-qam", "B")
        self.git(self.engine, "push", "-q", "origin", "main")
        done = self.run_tool("stamp")
        self.assertNotEqual(done.returncode, 0)
        self.assertIn("changed since the build began", done.stderr)
        self.assertFalse((self.site / "data/build-provenance.json").exists())

    def test_both_revisions_are_checked_before_transfer(self):
        for directory in (self.site, self.engine):
            with self.subTest(directory=directory.name):
                self.assertEqual(self.run_tool("begin").returncode, 0)
                self.assertEqual(self.run_tool("stamp").returncode, 0)
                self.assertEqual(self.run_tool("check").returncode, 0)
                self.git(directory, "commit", "--allow-empty", "-qm", "moved")
                self.assertNotEqual(self.run_tool("check").returncode, 0)

    def test_changes_to_an_already_dirty_engine_are_detected(self):
        (self.engine / "input").write_text("development\n")
        self.assertEqual(self.run_tool("begin").returncode, 0)
        (self.engine / "input").write_text("different development\n")
        self.assertNotEqual(self.run_tool("stamp").returncode, 0)
        self.assertEqual(self.run_tool("begin").returncode, 0)
        self.assertEqual(self.run_tool("stamp").returncode, 0)
        self.assertIn("dirty", (self.site / "data/last-build-engine.txt").read_text())

    def test_an_incomplete_new_build_invalidates_the_previous_record(self):
        for command in ("begin", "stamp", "check", "begin"):
            self.assertEqual(self.run_tool(command).returncode, 0)
        self.assertNotEqual(self.run_tool("check").returncode, 0)
        # The old revision still informs the incremental freshness decision.
        self.assertTrue((self.site / "data/last-build-commit.txt").exists())
