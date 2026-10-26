"""tools/sitelib.py: the atomic writer every tool's output goes through.

It replaced eleven copies with five different sets of guarantees (fsync or
not, a PID-unique or a fixed temporary, cleanup on failure or not); the
merged one keeps the strongest of each, and these check them."""

import os
import shutil
import stat
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from tests._helpers import engine_path, stub_bin

# The directory sitelib.py is in, not tools/ wherever one exists: A6 can
# leave engine/tools/ holding only what moved first.
sys.path.insert(0, str(engine_path("tools/sitelib.py").parent))
import sitelib  # noqa: E402


class AtomicPath(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.dir = Path(tmp.name)

    def test_temporary_is_hidden_unique_and_gitignorable(self):
        target = self.dir / "sub" / "photo.jpg.exif.yaml"
        with sitelib.atomic_path(target) as tmp:
            # A dotfile (Hakyll skips it), in the target's directory (one
            # rename), unique to the process, ending in .tmp.
            self.assertEqual(tmp.parent, target.parent)
            self.assertTrue(tmp.name.startswith("."))
            self.assertIn(str(os.getpid()), tmp.name)
            self.assertTrue(tmp.name.endswith(".tmp"))
            tmp.write_text("new")
            self.assertFalse(target.exists())
        self.assertEqual(target.read_text(), "new")
        self.assertEqual(os.listdir(target.parent), [target.name])

    def test_failure_leaves_the_old_file_and_no_debris(self):
        target = self.dir / "index.json"
        target.write_text("old")
        with self.assertRaises(RuntimeError):
            with sitelib.atomic_path(target) as tmp:
                tmp.write_text("half")
                raise RuntimeError("interrupted")
        self.assertEqual(target.read_text(), "old")
        self.assertEqual(os.listdir(self.dir), ["index.json"])

    def test_durable_fsyncs_file_and_directory(self):
        target = self.dir / "a.json"
        with mock.patch.object(sitelib.os, "fsync", wraps=os.fsync) as fsync:
            sitelib.atomic_write_text(target, "x")
        self.assertEqual(fsync.call_count, 2)
        with mock.patch.object(sitelib.os, "fsync", wraps=os.fsync) as fsync:
            sitelib.atomic_write_text(target, "y", durable=False)
        self.assertEqual(fsync.call_count, 0)
        self.assertEqual(target.read_text(), "y")

    def test_mode(self):
        target = self.dir / "v.w480.jpg"
        with sitelib.atomic_path(target, durable=False, mode=0o644) as tmp:
            tmp.write_bytes(b"jpeg")
        self.assertEqual(stat.S_IMODE(target.stat().st_mode), 0o644)

    def test_skip_if_unchanged_keeps_the_mtime(self):
        target = self.dir / "similar-links.json"
        target.write_text("{}")
        os.utime(target, (1, 1))
        self.assertFalse(sitelib.atomic_write_text(target, "{}", skip_if_unchanged=True))
        self.assertEqual(target.stat().st_mtime, 1)
        self.assertTrue(sitelib.atomic_write_text(target, "{}"))
        self.assertNotEqual(target.stat().st_mtime, 1)

    def test_engine_path_is_the_engine_s_never_the_site_s(self):
        # A site file at the same path overrides the engine's in the build
        # (build/EngineFiles.hs); it is never the engine's own.
        engine, site = self.dir / "engine", self.dir / "site"
        for rel in ("engine/static/css/base.css", "site/static/css/base.css"):
            (self.dir / rel).parent.mkdir(parents=True, exist_ok=True)
            (self.dir / rel).write_text("")
        with mock.patch.object(sitelib, "ENGINE", engine), mock.patch.object(sitelib, "SITE_ROOT", site):
            self.assertEqual(sitelib.engine_path("static/css/base.css"), engine / "static/css/base.css")
            self.assertEqual(sitelib.engine_path("static/css/new.css"), engine / "static/css/new.css")

    def test_slugify(self):
        self.assertEqual(sitelib.slugify("Rilke's  Duino_Elegies — I"), "rilkes-duino-elegies-i")


class SiteConfig(unittest.TestCase):
    """The site's identity comes from the generator last built: the link-rot
    timer and the passphrase preset ask outside the build lock, where a
    compile could race a build's. A stub cabal notes each call."""

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.dir = Path(tmp.name)
        self.log = self.dir / "cabal.log"
        self.binary = self.dir / "site"
        self.built = self.dir / "built"
        self.built.write_text("#!/bin/sh\necho '{\"host\": \"example.org\"}'\n")
        self.built.chmod(0o755)
        stub = stub_bin(self.dir / "bin", ["cabal"],
                        f'#!/bin/sh\necho "$1" >> "{self.log}"\n'
                        f'case "$1" in\n'
                        f'  build) cp "{self.built}" "{self.binary}" ;;\n'
                        f'  list-bin) echo "{self.binary}" ;;\n'
                        f'esac\n')
        env = mock.patch.dict(os.environ, {"PATH": f"{stub}{os.pathsep}{os.environ['PATH']}"})
        env.start()
        self.addCleanup(env.stop)
        os.environ.pop("SITE_BINARY", None)
        for cached in (sitelib.site_binary, sitelib.site_config):
            cached.cache_clear()
            self.addCleanup(cached.cache_clear)

    def calls(self) -> list[str]:
        return self.log.read_text().split() if self.log.exists() else []

    def test_asks_the_generator_last_built(self):
        shutil.copy(self.built, self.binary)
        self.assertEqual(sitelib.site_config()["host"], "example.org")
        self.assertEqual(self.calls(), ["list-bin"])

    def test_the_shell_tools_ask_it_too(self):
        # site-paths.sh's site_config, for sign-site.sh and the preset.
        shutil.copy(self.built, self.binary)
        done = subprocess.run([sys.executable, sitelib.__file__, "config", "host"],
                              capture_output=True, text=True, timeout=30)
        self.assertEqual(done.stdout, "example.org\n", done.stderr)
        self.assertEqual(self.calls(), ["list-bin"])

    def test_builds_only_when_there_is_none(self):
        self.assertEqual(sitelib.site_config()["host"], "example.org")
        self.assertEqual(self.calls(), ["list-bin", "build", "list-bin"])

    def test_a_check_may_ask_for_a_fresh_one(self):
        shutil.copy(self.built, self.binary)
        sitelib.site_config(build=True)
        self.assertEqual(self.calls(), ["build", "list-bin"])


if __name__ == "__main__":
    unittest.main()
