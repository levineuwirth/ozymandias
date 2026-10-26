"""What the last finished build was built from (tools/build-inputs.py).

`make build` records its inputs' sizes and mtimes as its last step; the
browser tests refuse a _site whose inputs have changed, appeared or gone
since. Each test copies the tool into a throwaway tree, which it then takes
for the repository, and runs it there.
"""

from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import engine_path, script_env

TOOL = engine_path("tools/build-inputs.py")
INPUTS = ["engine", "static", "data", "tools", "nginx", "Makefile", "yaml-source"]


class BuildInputsTests(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory(prefix="build-inputs-")
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        for rel, text in {"static/js/a.js": "a", "static/css/b.css": "b",
                          "data/now.yaml": "now", "Makefile": "all:",
                          "nginx/site.conf": "server {}", "tools/browser/lib.py": "",
                          "engine/tools/browser/lib.py": "", "engine/tests/test_a.py": "",
                          "data/.compress-cache/x.gz": "", "data/.site-build.lock": "",
                          "data/sign-manifest.txt": "", "yaml-source/data/vita.yml": "",
                          "yaml-source/build/r.aux": "", "yaml-source/output/r.pdf": "",
                          "yaml-source/variants/private/r.yml": ""}.items():
            self.write(rel, text)
        (self.root / "tools" / "build-inputs.py").write_bytes(TOOL.read_bytes())

    def write(self, rel: str, text: str) -> Path:
        path = self.root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)
        return path

    def run_tool(self, *args: str) -> subprocess.CompletedProcess:
        return subprocess.run(["python3", str(self.root / "tools" / "build-inputs.py"), *args],
                              cwd=self.root, env=script_env(), capture_output=True, text=True,
                              timeout=30)

    def check(self) -> tuple[int, list[str]]:
        done = self.run_tool("check")
        return done.returncode, done.stdout.splitlines()

    def test_no_manifest_is_stale(self) -> None:
        code, lines = self.check()
        self.assertEqual(code, 1)
        self.assertEqual(len(lines), 1)
        self.assertIn("run `make build`", lines[0])

    def test_unchanged_after_record(self) -> None:
        self.assertEqual(self.run_tool("record", *INPUTS).returncode, 0)
        self.assertEqual(self.check(), (0, []))

    def test_changed_added_and_removed(self) -> None:
        self.run_tool("record", *INPUTS)
        now = self.root / "data" / "now.yaml"
        st = now.stat()
        os.utime(now, ns=(st.st_atime_ns, st.st_mtime_ns + 1_000_000_000))
        self.write("static/js/new.js", "")
        (self.root / "static" / "css" / "b.css").unlink()
        self.write("Makefile", "all: site")
        code, lines = self.check()
        self.assertEqual(code, 1)
        self.assertEqual(lines, ["changed Makefile", "changed data/now.yaml",
                                 "added static/js/new.js", "removed static/css/b.css"])

    def test_same_mtime_new_size_is_a_change(self) -> None:
        self.run_tool("record", *INPUTS)
        path = self.root / "static" / "js" / "a.js"
        st = path.stat()
        path.write_text("ab")
        os.utime(path, ns=(st.st_atime_ns, st.st_mtime_ns))
        self.assertEqual(self.check(), (1, ["changed static/js/a.js"]))

    def test_what_is_not_an_input(self) -> None:
        self.run_tool("record", *INPUTS)
        for rel in ("nginx/site.conf", "tools/browser/lib.py", "engine/tools/browser/lib.py",
                    "engine/tests/test_a.py", "data/.compress-cache/x.gz",
                    "data/.site-build.lock", "data/sign-manifest.txt",
                    # The résumé's artifacts and private variants: the site
                    # reads yaml-source/data/ alone.
                    "yaml-source/build/r.aux", "yaml-source/output/r.pdf",
                    "yaml-source/variants/private/r.yml"):
            self.write(rel, "rewritten after the build")
        self.write("tools/__pycache__/x.cpython-314.pyc", "")
        self.write("data/.compress-cache/new.br", "")
        self.write("yaml-source/variants/private/new.yml", "")
        self.assertEqual(self.check(), (0, []))
        self.write("yaml-source/data/vita.yml", "rewritten after the build")
        self.assertEqual(self.check(), (1, ["changed yaml-source/data/vita.yml"]))

    def test_record_replaces_the_manifest_whole(self) -> None:
        self.run_tool("record", *INPUTS)
        self.write("static/js/new.js", "")
        self.run_tool("record", *INPUTS)
        self.assertEqual(self.check(), (0, []))
        self.assertFalse((self.root / "data" / "build-inputs.json.tmp").exists())

    def test_recovers_from_an_interrupted_write(self) -> None:
        # A record stopped before its rename leaves the .tmp behind; the
        # next record must not take it for an input it then renames away.
        self.write("data/build-inputs.json.tmp", '{"paths": [], "files": {}')
        self.assertEqual(self.run_tool("record", *INPUTS).returncode, 0)
        self.assertEqual(self.check(), (0, []))
        self.write("data/build-inputs.json.tmp", "debris")
        self.assertEqual(self.check(), (0, []))

    def test_a_separate_engine_is_recorded_by_commit(self) -> None:
        # Inside the site's tree, the engine's files are inputs like any other.
        self.write("engine/build/Site.hs", "main = pure ()")
        self.run_tool("record", *INPUTS, "engine")
        self.assertEqual(self.check(), (0, []))
        # A repository of its own (the engine's checkout behind a symlink)
        # is named by its commit.
        def git(*args: str) -> str:
            return subprocess.run(["git", "-C", str(self.root / "engine"), "-c", "user.name=T",
                                   "-c", "user.email=t@example.org", *args], env=script_env(),
                                  capture_output=True, text=True, check=True).stdout.strip()
        git("init", "-q")
        git("commit", "-q", "--allow-empty", "-m", "one")
        first = git("rev-parse", "HEAD")
        self.run_tool("record", *INPUTS, "engine")
        self.assertEqual(self.check(), (0, []))
        # Its metadata and caches are not inputs: a fetched ref, a build
        # tree, a linter's cache.
        git("update-ref", "refs/remotes/origin/main", "HEAD")
        for rel in ("engine/dist-newstyle/cache/plan.json", "engine/.ruff_cache/x",
                    "engine/.venv/pyvenv.cfg"):
            self.write(rel, "")
        self.assertEqual(self.check(), (0, []))
        self.write("engine/build/Site.hs", "main = print 1")
        self.assertEqual(self.check(), (1, ["changed engine/build/Site.hs"]))
        self.run_tool("record", *INPUTS, "engine")
        git("commit", "-q", "--allow-empty", "-m", "two")
        self.assertEqual(self.check(), (1, [f"engine {first} -> {git('rev-parse', 'HEAD')}"]))

    def test_usage(self) -> None:
        self.assertEqual(self.run_tool().returncode, 2)
        self.assertEqual(self.run_tool("record").returncode, 2)


if __name__ == "__main__":
    unittest.main()
