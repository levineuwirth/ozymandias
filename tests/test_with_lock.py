"""tools/with-lock.sh: exclusive between sessions, re-entrant within one (audit D13)."""

import shutil
import subprocess
import tempfile
import time
import unittest
from pathlib import Path

from tests._helpers import engine_path, script_env

WITH_LOCK = str(engine_path("tools/with-lock.sh"))


@unittest.skipUnless(shutil.which("flock"), "flock(1) not installed")
class WithLockTests(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix="with-lock-"))
        self.addCleanup(shutil.rmtree, self.tmp)
        self.lock = str(self.tmp / "site.lock")

    def run_locked(self, *cmd, env=None):
        return subprocess.run([WITH_LOCK, self.lock, *cmd], env=env or script_env(),
                              capture_output=True, text=True, timeout=30)

    def test_a_target_run_by_the_holder_passes_through(self):
        # The locked build runs `make thumbnails`, which takes the lock again.
        r = self.run_locked(WITH_LOCK, self.lock, "sh", "-c", 'echo "inner $SITE_LOCK_HELD"')
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertEqual(r.stdout.strip(), f"inner {self.lock}")

    def test_a_second_session_is_refused_while_the_lock_is_held(self):
        ready = self.tmp / "ready"
        holder = subprocess.Popen([WITH_LOCK, self.lock, "sh", "-c", f'touch "{ready}"; sleep 5'],
                                  env=script_env())
        self.addCleanup(holder.kill)
        for _ in range(100):
            if ready.exists():
                break
            time.sleep(0.05)
        self.assertTrue(ready.exists(), "holder never started")
        r = self.run_locked("true")
        self.assertEqual(r.returncode, 1)
        self.assertIn("already holds", r.stderr)

    def test_a_different_lock_is_not_passed_through(self):
        env = script_env()
        env["SITE_LOCK_HELD"] = str(self.tmp / "other.lock")
        r = self.run_locked("sh", "-c", 'echo "$SITE_LOCK_HELD"', env=env)
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertEqual(r.stdout.strip(), self.lock)


if __name__ == "__main__":
    unittest.main()
