"""How many items the generator compiles at once (build/SiteThreads.hs).

Hakyll runs one worker per capability, and the generator used to get one;
it now gets SITE_THREADS, a whole number from 1 to 256 (the RTS's limit),
or 4. Configurable per run: `SITE_THREADS=8 make build` or `make build
SITE_THREADS=8`, and a setting in the environment reaches the tests'
generator runs too.
"""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import ENGINE, SITE_ROOT, engine_path, requires_cabal, script_env, site_binary, write_site_config


@requires_cabal
class SiteThreadsTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # Exercise the real environment reader at its boundary without
        # allocating 256 runtime capabilities in a deployment's test gate.
        temporary = tempfile.TemporaryDirectory()
        cls.addClassCleanup(temporary.cleanup)
        root = Path(temporary.name)
        source = root / "Probe.hs"
        source.write_text("import SiteThreads (siteThreads)\nmain = siteThreads >>= print\n")
        cls.probe = root / "probe"
        done = subprocess.run(
            ["cabal", "exec", "--", "ghc", "-v0", f"-i{engine_path('build')}", "-outputdir", str(root),
             str(source), "-o", str(cls.probe)], cwd=ENGINE,
            capture_output=True, text=True, timeout=120,
        )
        if done.returncode:
            raise AssertionError(f"Thread setting probe failed ({done.returncode}):\n{done.stderr}")

    def environment(self, **env) -> dict[str, str]:
        # Only these configuration tests drop an inherited setting.
        environment = script_env(**env)
        if "SITE_THREADS" not in env:
            environment.pop("SITE_THREADS", None)
        return environment

    def setting(self, **env) -> subprocess.CompletedProcess[str]:
        done = subprocess.run([str(self.probe)], env=self.environment(**env),
                              capture_output=True, text=True, timeout=30)
        self.assertEqual(done.returncode, 0, f"stdout: {done.stdout}\nstderr: {done.stderr}")
        return done

    def threads(self, **env) -> subprocess.CompletedProcess[str]:
        # The actual generator logs its count before missing templates
        # stop this scratch build. Keep runtime integration counts small.
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            write_site_config(root)
            (root / "data").mkdir()
            essay = root / "content" / "essays" / "a.md"
            essay.parent.mkdir(parents=True)
            essay.write_text("---\ntitle: A\ndate: 2026-01-01\n---\n\nBody.\n")
            done = subprocess.run([str(site_binary()), "build"], cwd=root,
                                  env=self.environment(**env), capture_output=True, text=True,
                                  timeout=120)
            self.assertEqual(done.returncode, 1,
                             f"stdout: {done.stdout}\nstderr: {done.stderr}")
            return done

    def test_four_by_default(self):
        self.assertIn("async runtime with 4 threads", self.threads().stdout)

    def test_site_threads_sets_the_count_from_1_to_256(self):
        for value in ("1", "2", "256"):
            with self.subTest(value=value):
                done = self.setting(SITE_THREADS=value)
                self.assertEqual(done.stdout.strip(), value)
                self.assertEqual(done.stderr, "")

    def test_generator_applies_the_setting(self):
        for value in ("1", "2"):
            with self.subTest(value=value):
                done = self.threads(SITE_THREADS=value)
                self.assertIn(f"async runtime with {value} threads", done.stdout, done.stderr)

    def test_empty_setting_uses_four(self):
        done = self.setting(SITE_THREADS="")
        self.assertEqual(done.stdout.strip(), "4")
        self.assertEqual(done.stderr, "")

    def test_anything_else_warns_and_uses_four(self):
        # Out-of-range values are judged before they become an Int: read
        # straight into one, -18446744073709551614 wrapped round to 2 and
        # 4294967296 reached the RTS, which warned and used 1.
        for value in ("0", "257", "-3", "eight", "2.5", "-18446744073709551614",
                      "4294967296", "18446744073709551617"):
            with self.subTest(value=value):
                done = self.setting(SITE_THREADS=value)
                self.assertIn(f'SITE_THREADS="{value}" is not a whole number from 1 to 256; '
                              "using 4", done.stderr)
                self.assertEqual(done.stdout.strip(), "4")

    def test_make_passes_it_to_the_generator(self):
        done = subprocess.run(["make", "-s", "--eval", "probe-threads: ; @echo $$SITE_THREADS",
                               "probe-threads", "SITE_THREADS=8"], cwd=SITE_ROOT, env=script_env(),
                              capture_output=True, text=True, timeout=60)
        self.assertEqual(done.stdout.strip(), "8", done.stderr)


if __name__ == "__main__":
    unittest.main()
