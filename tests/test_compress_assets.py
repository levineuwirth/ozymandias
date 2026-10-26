#!/usr/bin/env python3
"""tools/compress-assets.sh: sidecars are right for the bytes on disk and are
reused across runs (audit X2/D01).

Before, reuse was judged by mtimes. The brotli CLI copies its input's mtime to
its output, so every .br looked stale and was recompressed on every build, and
no mtime rule can see changed content behind an unchanged mtime. The script
now keys sidecars on a hash of the content.

Run with: ``python3 -m unittest tests.test_compress_assets``.
"""

from __future__ import annotations

import gzip
import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import engine_path, script_env, stub_bin

SCRIPT = engine_path("tools/compress-assets.sh")
HAVE_BROTLI = shutil.which("brotli") is not None


def decode(path: Path) -> bytes:
    if path.suffix == ".gz":
        return gzip.decompress(path.read_bytes())
    return subprocess.run(["brotli", "-dc", str(path)], capture_output=True, check=True).stdout


@unittest.skipUnless(shutil.which("bash"), "bash not on PATH")
class CompressAssets(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.site = self.root / "_site"
        self.site.mkdir()
        self.cache = self.root / "cache"
        self.page = self.site / "page.html"
        self.page.write_text("<p>" + "words and more words " * 200 + "</p>")
        self.kinds = [".gz", ".br"] if HAVE_BROTLI else [".gz"]

    def run_script(self, path_prefix: str | None = None) -> subprocess.CompletedProcess:
        env = script_env(path_prefix, COMPRESS_CACHE=str(self.cache))
        return subprocess.run(["bash", str(SCRIPT), str(self.site)], capture_output=True, text=True, env=env)

    def sidecars(self, src: Path) -> dict[str, tuple[int, int]]:
        return {k: (os.stat(f"{src}{k}").st_ino, os.stat(f"{src}{k}").st_mtime_ns) for k in self.kinds}

    def assert_sidecars_match(self, src: Path) -> None:
        for k in self.kinds:
            self.assertEqual(decode(Path(f"{src}{k}")), src.read_bytes(), k)

    def test_sidecars_round_trip(self):
        self.assertEqual(self.run_script().returncode, 0)
        self.assert_sidecars_match(self.page)

    def test_second_run_leaves_every_sidecar_alone(self):
        self.run_script()
        before = self.sidecars(self.page)
        self.assertEqual(self.run_script().returncode, 0)
        self.assertEqual(self.sidecars(self.page), before)

    def test_changed_content_behind_an_unchanged_mtime(self):
        # The case no mtime rule catches.
        self.run_script()
        stamp = os.stat(self.page)
        self.page.write_text("<p>" + "different words entirely " * 200 + "</p>")
        os.utime(self.page, ns=(stamp.st_atime_ns, stamp.st_mtime_ns))
        self.assertEqual(self.run_script().returncode, 0)
        self.assert_sidecars_match(self.page)

    def test_a_cleaned_site_is_refilled_from_the_cache(self):
        # After `site -- clean` the sidecars are gone but the cache is not:
        # nothing is compressed again (compressors that always fail prove it).
        self.run_script()
        for k in self.kinds:
            Path(f"{self.page}{k}").unlink()
        shim = stub_bin(self.root / "shim", ("gzip", "brotli"),
                        "#!/bin/sh\necho 'compressor called' >&2\nexit 1\n")
        done = self.run_script(shim)
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertNotIn("compressor called", done.stderr)
        self.assert_sidecars_match(self.page)

    def test_source_below_min_size_drops_its_sidecars(self):
        self.run_script()
        self.page.write_text("<p>tiny</p>")
        self.assertEqual(self.run_script().returncode, 0)
        for k in self.kinds:
            self.assertFalse(Path(f"{self.page}{k}").exists(), k)

    def test_missing_brotli_never_keeps_a_stale_sidecar(self):
        self.run_script()
        # Model a checkout moved to a machine without the optional encoder.
        if not Path(f"{self.page}.br").exists():
            Path(f"{self.page}.br").write_bytes(b"old compressed bytes")
        self.page.write_text("<p>" + "new content " * 200 + "</p>")
        # Override only command discovery, retaining the actual tools used
        # for gzip/cache operations. No real Brotli invocation is possible.
        shell = '''command() {
            if [ "$1" = -v ] && [ "$2" = brotli ]; then return 1; fi
            builtin command "$@"
        }
        export -f command
        exec bash "$1" "$2"
        '''
        done = subprocess.run(["bash", "-c", shell, "test", str(SCRIPT), str(self.site)],
                              env=script_env(COMPRESS_CACHE=str(self.cache)),
                              capture_output=True, text=True)
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertFalse(Path(f"{self.page}.br").exists())
        self.assertEqual(decode(Path(f"{self.page}.gz")), self.page.read_bytes())

    def test_orphaned_sidecars_are_swept(self):
        self.run_script()
        self.page.unlink()
        self.assertEqual(self.run_script().returncode, 0)
        for k in self.kinds:
            self.assertFalse(Path(f"{self.page}{k}").exists(), k)


if __name__ == "__main__":
    unittest.main()
