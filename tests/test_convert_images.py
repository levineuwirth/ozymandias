"""tools/convert-images.sh: a WebP companion beside every JPEG and PNG.

Run over a scratch site and a scratch engine (the script walks both, and
this must write nothing into the real one) with a stand-in for cwebp:

  · an image gets its companion whichever case its extension is in, as the
    image filter and the site check expect one (IMG_0042.JPG is how a
    camera writes it);
  · a companion that is not older than its image is left alone, and one
    that is older is made again;
  · a file that is not a JPEG or PNG gets none;
  · two images a page would be shown with one companion for (map.png,
    map.JPG, map.gif) are refused, each named, before anything is
    converted, with or without cwebp, and the companion they share is
    removed: the one a published page shows must not be, or stay, a
    draft's picture, and renaming the draft's image must be enough;
  · so are an image of the site's static/ and one of the engine's that
    are served with one companion, while a site image that replaces the
    engine's of the same name is not;
  · a walk that fails is a failure, not a site with no images;
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

    def bare_path(self) -> str:
        """A PATH of what the script needs besides cwebp: a machine
        without the converter."""
        bare = self.root / "bare"
        if not bare.is_dir():
            bare.mkdir()
            for tool in ("bash", "cat", "dirname", "find", "mktemp", "rm", "sort"):
                (bare / tool).symlink_to(shutil.which(tool))
        return str(bare)

    def refused(self, **env: str) -> str:
        done = self.run_script(**env)
        self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
        self.assertNotIn("converted", done.stdout)
        return done.stderr

    def test_two_images_with_one_companion_are_refused_before_anything_is_converted(self):
        # A published page shows map.png, and its companion is made.
        self.write("content/blog/map.png")
        self.convert()
        self.assertEqual(self.companions(), {"content/blog/map.webp": "map.png"})
        # Then a draft's map.JPG arrives, newer, beside images not yet
        # converted. Its pixels must not become map.webp.
        for name in ("map.JPG", "also.jpeg", "also.PNG", "also.jpg", "other.jpg"):
            self.write(f"content/blog/{name}")
        for env, removed in (({}, True), ({"PATH": self.bare_path()}, False)):
            with self.subTest(env=env):
                said = self.refused(**env)
                self.assertIn("content/blog/map.JPG and content/blog/map.png share one WebP companion, "
                              "content/blog/map.webp", said)
                self.assertEqual("Removed content/blog/map.webp" in said, removed)
                self.assertIn("content/blog/also.PNG, content/blog/also.jpeg and content/blog/also.jpg share "
                              "one WebP companion, content/blog/also.webp", said)
                self.assertNotIn("other.jpg", said)
                # Nothing was converted, and the companion that either of
                # the two may have made is gone.
                self.assertEqual(self.companions(), {})
        # All but one of each renamed or taken away, everything is converted.
        (self.root / "content/blog/map.JPG").rename(self.root / "content/blog/plan.JPG")
        (self.root / "content/blog/also.PNG").unlink()
        (self.root / "content/blog/also.jpg").unlink()
        self.assertIn("4 converted, 0 already present", self.convert())
        self.assertEqual(self.companions(), {
            "content/blog/map.webp": "map.png", "content/blog/plan.webp": "plan.JPG",
            "content/blog/also.webp": "also.jpeg", "content/blog/other.webp": "other.jpg"})

    def test_a_companion_already_made_from_the_wrong_image_does_not_outlast_the_rename(self):
        # As a converter before this check left it: the draft's newer
        # map.JPG has overwritten the companion of the published map.png,
        # and the companion is newer than both.
        for age, (name, text) in enumerate((("map.png", "x"), ("map.JPG", "x"), ("map.webp", "map.JPG"))):
            os.utime(self.write(f"content/blog/{name}", text), (1_000_000_000 + age, 1_000_000_000 + age))
        said = self.refused()
        self.assertIn("Removed content/blog/map.webp", said)
        self.assertIn("Rename all but one of the images.", said)
        # The author does what the message says, and no more.
        (self.root / "content/blog/map.JPG").rename(self.root / "content/blog/plan.JPG")
        self.convert()
        self.assertEqual(self.companions(), {"content/blog/map.webp": "map.png", "content/blog/plan.webp": "plan.JPG"})
        # The same on a machine that cannot make it again: no companion,
        # and the page shows its own image.
        self.write("content/blog/sea.png")
        self.write("content/blog/sea.jpeg")
        self.write("content/blog/sea.webp", "sea.jpeg")
        self.refused(PATH=self.bare_path())
        (self.root / "content/blog/sea.jpeg").unlink()
        self.assertEqual(self.run_script(PATH=self.bare_path()).returncode, 0)
        self.assertNotIn("content/blog/sea.webp", self.companions())

    def test_a_gif_is_shown_with_a_companion_too(self):
        # The image filter gives color.gif the companion color.webp, which
        # this script makes from color.JPG.
        self.write("content/blog/color.gif")
        self.write("content/blog/color.JPG")
        self.write("content/blog/color.webp", "color.JPG")
        # A GIF with a companion made elsewhere, and no other image of its name.
        self.write("content/blog/moving.GIF")
        self.write("content/blog/moving.webp", "by hand")
        said = self.refused()
        self.assertIn("content/blog/color.JPG and content/blog/color.gif share one WebP companion, "
                      "content/blog/color.webp", said)
        self.assertNotIn("moving", said)
        self.assertEqual(self.companions(), {"content/blog/moving.webp": "by hand"})
        (self.root / "content/blog/color.JPG").rename(self.root / "content/blog/draft.JPG")
        self.assertIn("1 converted, 0 already present", self.convert())
        self.assertEqual(self.companions(), {"content/blog/moving.webp": "by hand",
                                             "content/blog/draft.webp": "draft.JPG"})

    def test_the_site_s_static_image_and_the_engine_s_are_served_as_one_tree(self):
        # The engine's color.JPG and the site's color.png are both shown
        # with /images/color.webp, and the site's file is the one served.
        self.write("engine/static/images/color.JPG")
        self.write("engine/static/images/color.webp", "color.JPG")
        self.write("static/images/color.png")
        self.write("static/images/color.webp", "color.png")
        # A site image that replaces the engine's of its name is one image,
        # and each has its companion beside it.
        self.write("engine/static/images/logo.png")
        self.write("static/images/logo.png")
        # Under content/ the same names are other addresses.
        self.write("content/images/color.jpeg")
        for env in ({}, {"PATH": self.bare_path()}):
            with self.subTest(env=env):
                said = self.refused(**env)
                self.assertIn("engine/static/images/color.JPG and static/images/color.png share the WebP "
                              "companion served at /images/color.webp", said)
                self.assertNotIn("logo", said)
                self.assertNotIn("content/", said)
                self.assertEqual(self.companions(), {})
        # The site's image renamed, the engine's is shown with its own again.
        (self.root / "static/images/color.png").rename(self.root / "static/images/colour.png")
        self.assertIn("5 converted, 0 already present", self.convert())
        self.assertEqual(self.companions(), {
            "engine/static/images/color.webp": "color.JPG", "static/images/colour.webp": "colour.png",
            "engine/static/images/logo.webp": "logo.png", "static/images/logo.webp": "logo.png",
            "content/images/color.webp": "color.jpeg"})

    def test_names_a_shell_reads_twice_are_names(self):
        for name in ("a b.png", "it's].jpg", "$(touch pwned).png", "[x]=1.PNG", "it's].JPEG"):
            self.write(f"content/blog/{name}")
        said = self.refused()
        self.assertIn("content/blog/it's].JPEG and content/blog/it's].jpg share one WebP companion", said)
        (self.root / "content/blog/it's].JPEG").unlink()
        self.assertIn("4 converted, 0 already present", self.convert())
        self.assertEqual(set(self.companions()), {
            "content/blog/a b.webp", "content/blog/it's].webp", "content/blog/$(touch pwned).webp",
            "content/blog/[x]=1.webp"})
        self.assertFalse((self.root / "pwned").exists())

    def test_a_walk_that_fails_is_a_failure(self):
        # With two images that share a companion in the tree: a failed
        # walk must not read as a tree with nothing to refuse.
        self.write("content/blog/map.png")
        self.write("content/blog/map.JPG")
        self.write("content/blog/map.webp", "map.png")
        for tool in ("find", "sort"):
            with self.subTest(tool=tool):
                broken = stub_bin(self.root / f"broken-{tool}", [tool], "#!/bin/sh\nexit 1\n")
                done = self.run_script(PATH=os.pathsep.join((str(broken), str(self.stubs), os.environ["PATH"])))
                self.assertNotEqual(done.returncode, 0, done.stdout + done.stderr)
                self.assertIn(f"could not list the images ({tool} failed)", done.stderr)
                self.assertNotIn("converted", done.stdout)
                self.assertEqual(self.companions(), {"content/blog/map.webp": "map.png"})
        # A walk that reaches only part of the tree is one that failed.
        closed = self.root / "content/closed"
        closed.mkdir()
        closed.chmod(0)
        self.addCleanup(closed.chmod, 0o755)
        if os.access(closed, os.R_OK):
            self.skipTest("this user reads a directory without permission")
        done = self.run_script()
        self.assertNotEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertIn("could not list the images (find failed)", done.stderr)

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
