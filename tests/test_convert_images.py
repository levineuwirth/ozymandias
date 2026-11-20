"""tools/convert-images.sh: a WebP companion beside every JPEG and PNG.

Run over a scratch site and a scratch engine (the script walks both, and
this must write nothing into the real one) with a stand-in for cwebp:

  · an image gets its companion whichever case its extension is in, as the
    image filter and the site check expect one (IMG_0042.JPG is how a
    camera writes it);
  · a companion that is not older than its image is left alone, and one
    that is older is made again;
  · a file that is not a JPEG or PNG gets none;
  · two images with one companion between them (map.png, map.JPG) are
    refused, both named, before anything is written, with or without
    cwebp: the companion a published page shows must not become a
    draft's picture;
  · a directory named as an image is passed over, and a link to an image
    is converted.

    python -m unittest tests.test_convert_images -v
"""

from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import engine_path, script_env, stub_bin

# cwebp -quiet -q 85 <image> -o <companion>: writes the image's name.
CWEBP = """#!/bin/sh
image=$4
while [ "$1" != "-o" ]; do shift; done
printf '%s' "$(basename "$image")" > "$2"
"""


class ConvertImages(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory(prefix="convert-images-")
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.stubs = stub_bin(self.root / "bin", ["cwebp"], CWEBP)
        # The script takes the engine to be the directory above its own.
        tools = self.root / "engine/tools"
        tools.mkdir(parents=True)
        for name in ("convert-images.sh", "site-paths.sh"):
            shutil.copy(engine_path(f"tools/{name}"), tools / name)

    def write(self, rel: str, text: str = "x") -> Path:
        path = self.root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")
        return path

    def run_script(self, **env: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(["bash", str(self.root / "engine/tools/convert-images.sh")], cwd=self.root,
                              env={**script_env(self.stubs, SITE_ROOT=str(self.root)), **env},
                              capture_output=True, text=True, timeout=60)

    def convert(self) -> str:
        done = self.run_script()
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        return done.stdout

    def companions(self) -> dict[str, str]:
        return {p.relative_to(self.root).as_posix(): p.read_text(encoding="utf-8")
                for p in self.root.rglob("*.webp")}

    def test_an_extension_counts_in_either_case(self):
        for rel in ("content/blog/lake.jpg", "content/blog/big/Lake.JPG", "content/essays/a/Figure.Png",
                    "static/images/portrait.JPEG", "engine/static/images/Mark.PNG",
                    "content/blog/chart.svg", "content/blog/moving.GIF"):
            self.write(rel)
        self.assertIn("5 converted, 0 already present", self.convert())
        self.assertEqual(self.companions(), {
            "content/blog/lake.webp": "lake.jpg", "content/blog/big/Lake.webp": "Lake.JPG",
            "content/essays/a/Figure.webp": "Figure.Png", "static/images/portrait.webp": "portrait.JPEG",
            "engine/static/images/Mark.webp": "Mark.PNG"})
        self.assertIn("0 converted, 5 already present", self.convert())

    def test_a_current_companion_is_left_and_a_stale_one_is_made_again(self):
        image = self.write("content/blog/big/Lake.JPG")
        kept = self.write("content/blog/big/Lake.webp", "kept")
        os.utime(image, (1_000_000_000, 1_000_000_000))
        self.assertIn("0 converted, 1 already present", self.convert())
        self.assertEqual(kept.read_text(encoding="utf-8"), "kept")
        os.utime(kept, (900_000_000, 900_000_000))
        self.assertIn("1 converted, 0 already present", self.convert())
        self.assertEqual(kept.read_text(encoding="utf-8"), "Lake.JPG")

    def test_two_images_with_one_companion_are_refused_before_anything_is_written(self):
        # A published page shows map.png, and its companion is made.
        self.write("content/blog/map.png")
        self.convert()
        self.assertEqual(self.companions(), {"content/blog/map.webp": "map.png"})
        # Then a draft's map.JPG arrives, newer, beside an image not yet
        # converted. Its pixels must not become map.webp.
        self.write("content/blog/map.JPG")
        self.write("content/blog/also.jpeg")
        self.write("content/blog/other.jpg")
        self.write("content/blog/also.PNG")
        # With cwebp, and on a machine without it (a PATH of what the
        # script needs besides).
        bare = self.root / "bare"
        bare.mkdir()
        for tool in ("bash", "cat", "dirname", "find", "sort"):
            (bare / tool).symlink_to(shutil.which(tool))
        for env in ({}, {"PATH": str(bare)}):
            with self.subTest(env=env):
                done = self.run_script(**env)
                self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
                self.assertIn("content/blog/map.JPG and content/blog/map.png would both be converted to "
                              "content/blog/map.webp", done.stderr)
                self.assertIn("content/blog/also.PNG and content/blog/also.jpeg would both be converted to "
                              "content/blog/also.webp", done.stderr)
                self.assertNotIn("other.jpg", done.stderr)
                self.assertEqual(self.companions(), {"content/blog/map.webp": "map.png"})
        # One of each pair renamed, the rest is converted.
        (self.root / "content/blog/map.JPG").rename(self.root / "content/blog/plan.JPG")
        (self.root / "content/blog/also.PNG").unlink()
        self.assertIn("3 converted, 1 already present", self.convert())
        self.assertEqual(self.companions(), {
            "content/blog/map.webp": "map.png", "content/blog/plan.webp": "plan.JPG",
            "content/blog/also.webp": "also.jpeg", "content/blog/other.webp": "other.jpg"})

    def test_a_directory_named_as_an_image_is_passed_over_and_a_link_is_followed(self):
        self.write("content/photography/album.JPG/inside.png")
        (self.root / "content/photography/album.png").mkdir()
        original = self.write("originals/real.jpg")
        (self.root / "content/photography/linked.jpg").symlink_to(original)
        (self.root / "content/photography/dangling.jpg").symlink_to(self.root / "originals/gone.jpg")
        self.assertIn("2 converted, 0 already present", self.convert())
        self.assertEqual(self.companions(), {"content/photography/album.JPG/inside.webp": "inside.png",
                                             "content/photography/linked.webp": "linked.jpg"})


if __name__ == "__main__":
    unittest.main()
