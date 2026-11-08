"""make test-clean clones what the tests run on, as committed.

tools/test-clean.sh clones the site at HEAD into a scratch directory and
runs `make test` there. Once the engine is a repository of its own, the
site's `engine` a link to it, the engine is cloned too, at its HEAD and
beside the site's clone, where the clone's Makefile links it as
../ozymandias. Each case is a scratch site whose `make test` only reports
what it sees."""

import os
import re
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import engine_path, script_env

MAKEFILE = """\
test:
\t@test -e engine || ln -s ../ozymandias engine
\t@echo "marker: $$(cat engine/marker)"
"""


def git(where: Path, *args: str) -> str:
    return subprocess.run(["git", "-c", "user.email=test@example.org", "-c", "user.name=Test",
                           "-c", "commit.gpgsign=false", *args], cwd=where,
                          check=True, capture_output=True, text=True).stdout


def repository(where: Path, files: dict[str, str]) -> None:
    where.mkdir(parents=True)
    git(where, "init", "-q", "-b", "main")
    for rel, text in files.items():
        (where / rel).parent.mkdir(parents=True, exist_ok=True)
        (where / rel).write_text(text)
    git(where, "add", "-A")
    git(where, "commit", "-qm", "start")


class TestClean(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory(prefix="test-clean-")
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)

    def run_clean(self, site: Path) -> subprocess.CompletedProcess:
        return subprocess.run(["bash", str(engine_path("tools/test-clean.sh"))], cwd=site,
                              env=script_env(KEEP="1", TMPDIR=str(self.root)),
                              capture_output=True, text=True, timeout=120)

    def kept(self, done: subprocess.CompletedProcess) -> Path:
        return Path(re.search(r"the clones are kept in (\S+)", done.stdout).group(1))

    def test_an_engine_of_its_own_is_cloned_beside_the_site(self):
        engine = self.root / "engine-checkout"
        repository(engine, {"marker": "committed\n"})
        site = self.root / "site"
        repository(site, {"Makefile": MAKEFILE, ".gitignore": "/engine\n"})
        (site / "engine").symlink_to("../engine-checkout")
        # Uncommitted in either: not what is tested.
        (engine / "marker").write_text("uncommitted\n")
        done = self.run_clean(site)
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        head = git(engine, "log", "-1", "--format=%h %s").strip()
        self.assertIn(f"test-clean: engine {head}", done.stdout)
        self.assertIn("marker: committed", done.stdout)
        work = self.kept(done)
        self.assertEqual(os.readlink(work / "repo/engine"), "../ozymandias")
        self.assertEqual((work / "ozymandias/marker").read_text(), "committed\n")

    def test_an_engine_in_the_site_comes_with_it(self):
        site = self.root / "site"
        repository(site, {"Makefile": MAKEFILE, "engine/marker": "in the site\n"})
        done = self.run_clean(site)
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertIn("test-clean: engine in this repository", done.stdout)
        self.assertIn("marker: in the site", done.stdout)
        self.assertFalse((self.kept(done) / "ozymandias").exists())


if __name__ == "__main__":
    unittest.main()
