"""tools/convert-images.sh: a WebP companion beside every JPEG and PNG.

Run over a scratch site and a scratch engine (the script walks both, and
this must write nothing into the real one) with a stand-in for cwebp:

  · an image gets its companion whichever case its extension is in, as the
    image filter and the site check expect one (IMG_0042.JPG is how a
    camera writes it);
  · a companion that is not older than its image is left alone, and one
    that is older is made again;
  · a file that is not a JPEG or PNG gets none.

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

    def convert(self) -> str:
        done = subprocess.run(["bash", str(self.root / "engine/tools/convert-images.sh")], cwd=self.root,
                              env=script_env(self.stubs, SITE_ROOT=str(self.root)),
                              capture_output=True, text=True, timeout=60)
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


if __name__ == "__main__":
    unittest.main()
