"""Exercise the real Stability module's IO caches in one long-lived process."""

import json
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

from tests._helpers import ENGINE, engine_path, requires_cabal, script_env


@requires_cabal
class StabilityCacheTests(unittest.TestCase):
    def run_probe(self, commands):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            stub = root / "git"
            stub.write_text('''#!/usr/bin/env python3
import os, sys, time
from pathlib import Path
if sys.argv[1] != 'log':
    os.execv(os.environ['REAL_GIT'], ['git', *sys.argv[1:]])
root = Path(os.environ['CACHE_PROBE'])
with (root / 'calls').open('a') as out:
    out.write(sys.argv[-1] + '\\n')
time.sleep(0.1)
if sys.argv[-1] == 'new.md' and not (root / 'tracked').exists():
    (root / 'tracked').touch()
else:
    print('2026-10-03')
''')
            stub.chmod(0o755)
            env = script_env(directory, CACHE_PROBE=directory, REAL_GIT=shutil.which("git"))
            setup = [":module *Stability", "import Control.Concurrent", "import Control.Monad",
                     "import Data.Time.Clock (addUTCTime)",
                     "import System.Directory", f"setCurrentDirectory {json.dumps(directory)}"]
            done = subprocess.run(
                ["cabal", "exec", "--", "ghc", "--interactive", "-ignore-dot-ghci", "-v0",
                 f"-i{engine_path('build')}", str(engine_path("build/Stability.hs"))],
                cwd=ENGINE, env=env,
                input="\n".join(setup + commands + [":quit"]) + "\n",
                capture_output=True, text=True, timeout=60,
            )
            self.assertEqual(done.returncode, 0, done.stderr)
            self.assertNotIn("error:", done.stderr, done.stderr)
            calls = (root / "calls").read_text().splitlines() if (root / "calls").exists() else []
            return done.stdout.splitlines(), calls

    def test_untracked_empty_history_is_retried_then_cached(self):
        output, calls = self.run_probe(['print =<< gitDates "new.md"'] * 3)
        self.assertEqual(output, ['[]', '["2026-10-03"]', '["2026-10-03"]'])
        self.assertEqual(calls, ["new.md", "new.md"])

    def test_overlapping_requests_share_one_git_lookup(self):
        output, calls = self.run_probe([
            "results <- replicateM 8 (newEmptyMVar :: IO (MVar [String]))",
            'mapM_ (\\result -> forkIO (gitDates "page.md" >>= putMVar result)) results',
            "print =<< mapM takeMVar results",
        ])
        self.assertEqual(output, ['[' + ','.join(['["2026-10-03"]'] * 8) + ']'])
        self.assertEqual(calls, ["page.md"])

    def test_ignore_cache_observes_a_changed_file(self):
        output, _ = self.run_probe([
            'writeFile "IGNORE.txt" "first.md\\n"', "print =<< readIgnore",
            'stamp <- getModificationTime "IGNORE.txt"',
            'writeFile "IGNORE.txt" "second.md\\n"',
            'setModificationTime "IGNORE.txt" (addUTCTime 1 stamp)', "print =<< readIgnore",
        ])
        self.assertEqual(output, ['["first.md"]', '["second.md"]'])
