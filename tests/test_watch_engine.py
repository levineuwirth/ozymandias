"""make watch sees the engine's edits when the engine is linked.

Hakyll's watcher doesn't descend into a linked directory. tools/watch-engine.py
runs the watch command and, with `engine` a link to a checkout beside the
site, touches data/engine-changed.stamp when an engine file changes, which
Hakyll's watcher sees. Here the command is a stand-in that only waits."""

import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path

from tests._helpers import engine_path, script_env

TOOL = engine_path("tools/watch-engine.py")
WAIT = [sys.executable, "-c", "import time; time.sleep(3)"]


class WatchEngine(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory(prefix="watch-engine-")
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.site = self.root / "site"
        self.site.mkdir()
        self.engine = self.root / "ozymandias"
        (self.engine / "templates").mkdir(parents=True)
        (self.engine / "templates/page.html").write_text("before\n")
        (self.engine / "tests").mkdir()
        (self.engine / "tests/test_x.py").write_text("")
        self.stamp = self.site / "data/engine-changed.stamp"

    def start(self, command=WAIT) -> subprocess.Popen:
        process = subprocess.Popen([sys.executable, str(TOOL), "--", *command], cwd=self.site,
                                   env=script_env(WATCH_ENGINE_INTERVAL="0.1"))
        time.sleep(0.6)  # past its first look
        return process

    def test_an_engine_edit_touches_the_stamp_hakyll_sees(self):
        (self.site / "engine").symlink_to("../ozymandias")
        process = self.start()
        self.assertFalse(self.stamp.exists())
        (self.engine / "templates/page.html").write_text("after, and longer\n")
        time.sleep(0.6)
        self.assertTrue(self.stamp.exists())
        self.assertEqual(process.wait(timeout=10), 0)

    def test_the_engine_s_tests_are_not_watched(self):
        (self.site / "engine").symlink_to("../ozymandias")
        process = self.start()
        (self.engine / "tests/test_x.py").write_text("changed\n")
        time.sleep(0.6)
        self.assertFalse(self.stamp.exists())
        process.wait(timeout=10)

    def test_an_engine_inside_the_site_is_left_to_hakyll(self):
        (self.site / "engine").mkdir()
        (self.site / "engine/page.html").write_text("before\n")
        process = self.start()
        (self.site / "engine/page.html").write_text("after, and longer\n")
        time.sleep(0.6)
        self.assertFalse(self.stamp.exists())
        process.wait(timeout=10)

    def test_the_command_s_status_is_its_own(self):
        (self.site / "engine").symlink_to("../ozymandias")
        done = subprocess.run([sys.executable, str(TOOL), "--", sys.executable, "-c", "raise SystemExit(3)"],
                              cwd=self.site, env=script_env(WATCH_ENGINE_INTERVAL="0.1"), timeout=10)
        self.assertEqual(done.returncode, 3)


if __name__ == "__main__":
    unittest.main()
