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
    converted, with or without cwebp: the companion a published page
    shows must not become a draft's picture. Nothing is deleted: the
    message names the companion that is already there, which may be the
    author's own;
  · so are an image of the site's static/ and one of the engine's that
    are served with one companion, while a site image that replaces the
    engine's of the same name is not;
  · so is an image beside a WebP that a page shows by name, which is an
    image of its own and not to be converted over;
  · no name is told from another by what is in it, a newline included;
  · a walk that fails, or a generator that cannot be asked, is a failure,
    not a site with nothing to refuse;
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

from tests._helpers import engine_path, requires_cabal, script_env, site_binary, stub_bin, write_site_config

# cwebp -quiet -q 85 <image> -o <companion>: writes the image's name.
CWEBP = """#!/bin/sh
image=$4
while [ "$1" != "-o" ]; do shift; done
printf '%s' "$(basename "$image")" > "$2"
"""


@requires_cabal
class ConvertImages(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory(prefix="convert-images-")
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.stubs = stub_bin(self.root / "bin", ["cwebp"], CWEBP)
        # The script takes the engine to be the directory above its own.
        tools = self.root / "engine/tools"
        tools.mkdir(parents=True)
        for name in ("convert-images.sh", "site-paths.sh", "sitelib.py"):
            shutil.copy(engine_path(f"tools/{name}"), tools / name)
        # The generator says where a page's paths lead, by the site's
        # sections: essays are served at /articles/ here.
        write_site_config(self.root, sections=[
            {"dir": "essays", "address": "articles", "kind": "Essay", "dingbat": None},
            {"dir": "blog", "kind": "Post", "dingbat": None}])

    def write(self, rel: str, text: str = "x") -> Path:
        path = self.root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")
        return path

    def run_script(self, **env: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(["bash", str(self.root / "engine/tools/convert-images.sh")], cwd=self.root,
                              env={**script_env(self.stubs, SITE_ROOT=str(self.root),
                                                SITE_BINARY=str(site_binary())), **env},
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
            for tool in ("bash", "cat", "dirname", "find", "mktemp", "python3", "rm", "sort"):
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
        for env in ({}, {"PATH": self.bare_path()}):
            with self.subTest(env=env):
                said = self.refused(**env)
                self.assertIn("content/blog/map.JPG and content/blog/map.png share one WebP companion, "
                              "content/blog/map.webp", said)
                self.assertIn("content/blog/also.PNG, content/blog/also.jpeg and content/blog/also.jpg share "
                              "one WebP companion, content/blog/also.webp", said)
                self.assertIn("content/blog/map.webp is there already", said)
                self.assertNotIn("content/blog/also.webp is there", said)
                self.assertNotIn("other.jpg", said)
                # Nothing was converted, and nothing was taken away.
                self.assertEqual(self.companions(), {"content/blog/map.webp": "map.png"})
        # All but one of each renamed or taken away, everything is converted.
        (self.root / "content/blog/map.JPG").rename(self.root / "content/blog/plan.JPG")
        (self.root / "content/blog/also.PNG").unlink()
        (self.root / "content/blog/also.jpg").unlink()
        self.assertIn("3 converted, 1 already present", self.convert())
        self.assertEqual(self.companions(), {
            "content/blog/map.webp": "map.png", "content/blog/plan.webp": "plan.JPG",
            "content/blog/also.webp": "also.jpeg", "content/blog/other.webp": "other.jpg"})

    def test_a_companion_already_made_from_the_wrong_image_is_named_for_the_author(self):
        # As a converter before this check left it: the draft's newer
        # map.JPG has overwritten the companion of the published map.png,
        # and the companion is newer than both.
        for age, (name, text) in enumerate((("map.png", "x"), ("map.JPG", "x"), ("map.webp", "map.JPG"))):
            os.utime(self.write(f"content/blog/{name}", text), (1_000_000_000 + age, 1_000_000_000 + age))
        said = self.refused()
        self.assertIn("Rename all but one of the images.", said)
        # The script cannot tell whose pixels the companion holds, and it
        # may be the author's own file: it says which one to see to.
        self.assertIn("content/blog/map.webp is there already, and may hold the pixels of an image that is renamed. "
                      "Move it aside before the next build, or delete it if this script made it", said)
        self.assertEqual(self.companions(), {"content/blog/map.webp": "map.JPG"})
        # The author does both things the message says.
        (self.root / "content/blog/map.JPG").rename(self.root / "content/blog/plan.JPG")
        (self.root / "content/blog/map.webp").unlink()
        self.convert()
        self.assertEqual(self.companions(), {"content/blog/map.webp": "map.png", "content/blog/plan.webp": "plan.JPG"})

    def test_a_gif_is_shown_with_a_companion_too(self):
        # The image filter gives color.gif the companion color.webp, here
        # one the author made, which this script would make from color.JPG.
        self.write("content/blog/color.gif")
        self.write("content/blog/color.webp", "by hand")
        self.write("content/blog/color.JPG")
        said = self.refused()
        self.assertIn("content/blog/color.JPG and content/blog/color.gif share one WebP companion, "
                      "content/blog/color.webp", said)
        self.assertIn("content/blog/color.webp is there already", said)
        # The author's file is the author's: refused or not, it stays.
        self.assertEqual(self.companions(), {"content/blog/color.webp": "by hand"})
        (self.root / "content/blog/color.JPG").rename(self.root / "content/blog/draft.JPG")
        self.assertIn("1 converted, 0 already present", self.convert())
        self.assertEqual(self.companions(), {"content/blog/color.webp": "by hand",
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
        before = {"engine/static/images/color.webp": "color.JPG", "static/images/color.webp": "color.png"}
        for env in ({}, {"PATH": self.bare_path()}):
            with self.subTest(env=env):
                said = self.refused(**env)
                self.assertIn("engine/static/images/color.JPG and static/images/color.png share the WebP "
                              "companion served at /images/color.webp", said)
                for file in before:
                    self.assertIn(f"{file} is there already", said)
                self.assertNotIn("logo", said)
                self.assertNotIn("content/", said)
                # Neither tree is touched, the engine's least of all.
                self.assertEqual(self.companions(), before)
        # The site's image renamed and its companion taken away, the
        # engine's image is shown with its own again.
        (self.root / "static/images/color.png").rename(self.root / "static/images/colour.png")
        (self.root / "static/images/color.webp").unlink()
        self.assertIn("4 converted, 1 already present", self.convert())
        self.assertEqual(self.companions(), {
            "engine/static/images/color.webp": "color.JPG", "static/images/colour.webp": "colour.png",
            "engine/static/images/logo.webp": "logo.png", "static/images/logo.webp": "logo.png",
            "content/images/color.webp": "color.jpeg"})

    def page(self, rel: str, body: str, front: str = "title: A page") -> None:
        self.write(rel, f"---\n{front}\n---\n\n{body}\n")

    def test_a_webp_a_page_shows_by_name_is_not_another_image_s_companion(self):
        # A published page shows a WebP the author made, by its name.
        self.page("content/blog/public.md", "![Color](color.webp)")
        self.write("content/blog/color.webp", "by hand")
        self.convert()
        # A draft's newer color.JPG would be converted over it.
        self.write("content/blog/color.JPG")
        for env in ({}, {"PATH": self.bare_path()}):
            with self.subTest(env=env):
                said = self.refused(**env)
                self.assertIn("content/blog/public.md shows content/blog/color.webp by name, as an image of its "
                              "own, and content/blog/color.JPG would be converted over it", said)
                self.assertIn("Rename one of the two.", said)
                self.assertEqual(self.companions(), {"content/blog/color.webp": "by hand"})
        (self.root / "content/blog/color.JPG").rename(self.root / "content/blog/secret.JPG")
        self.convert()
        self.assertEqual(self.companions(), {"content/blog/color.webp": "by hand",
                                             "content/blog/secret.webp": "secret.JPG"})

    def test_every_way_a_page_names_a_webp_counts(self):
        named = {
            # a link, from a draft; an image from a directory above
            "content/blog/secret.md": ("[Full size](linked.webp) ![Up](../essays/up.webp)", "title: S\ndraft: true"),
            # raw HTML: an image, and a source's candidates
            "content/essays/html.md": ('<img src="raw.webp" alt="">\n\n<picture><source srcset="one.webp, two.webp 2x">'
                                       '</picture>', "title: H"),
            # escapes: of a space, of the name's own letters, a character reference
            "content/essays/escaped.md": ("![A](my%20pic.webp) ![B](hid.%77ebp) ![C](ref.we&#98;p)", "title: E"),
            # from the site's root, which static/ serves, and not above it
            "content/drafts/essays/rooted.md": ("![Brand](/images/brand.webp) ![Climb](/../../images/climb.webp)",
                                                "title: R"),
            # from the site's root, where a section is served at its
            # address and any other directory of content/ under its name
            "content/blog/routed.md": ("![Map](/articles/trip/figures/map.webp) [Cover](/music/suite/cover.webp)",
                                       "title: Routes"),
            # in the front matter: a photograph's file, beside the page; a
            # list; a name YAML spells with an escape
            "content/photography/harbor/index.md": ("", "title: Harbor\nphoto: harbor.webp"),
            "content/essays/fields.md": ("", 'title: F\nframes: [one.frame.webp, {still: "two.frame.\\x77ebp"}]'),
            # raw HTML, where a character reference is the character
            "content/essays/references.md": ('<a href="/images/A&amp;B.webp">A and B</a> '
                                             '<img src="dotted&period;webp" alt=""> <img src="fj&#46;webp" alt="">',
                                             "title: C"),
        }
        for rel, (body, front) in named.items():
            self.page(rel, body, front)
        images = ("content/blog/linked.png", "content/essays/up.jpg", "content/essays/raw.jpeg",
                  "content/essays/one.png", "content/essays/two.png", "content/essays/my pic.jpg",
                  "content/essays/hid.png", "content/essays/ref.png", "static/images/brand.png",
                  "engine/static/images/climb.JPG", "content/essays/trip/figures/map.JPG",
                  "content/music/suite/cover.png", "content/photography/harbor/harbor.JPG",
                  "content/essays/one.frame.jpg", "content/essays/two.frame.jpg", "static/images/A&B.JPG",
                  "content/essays/dotted.png", "content/essays/fj.png")
        # A path from the root that leads to a section's directory by the
        # directory's name leads nowhere: the section is at its address.
        self.page("content/blog/unrouted.md", "![No](/essays/elsewhere.webp)")
        # What no page names: a WebP of another name, a WebP in code, one
        # on another host, and a GIF, which is never converted.
        self.page("content/essays/quiet.md", "`![](code.webp)` and ![Far](https://example.org/far.webp) "
                  "and ![Other](other.webp) and ![Moving](moving.webp)")
        quiet = ("content/essays/code.png", "content/essays/far.png", "content/essays/moving.gif",
                 "content/essays/elsewhere.png")
        for rel in images + quiet:
            self.write(rel)
        said = self.refused()
        for rel in images:
            shown = Path(rel).with_suffix(".webp").as_posix()
            if "static/" in rel:
                shown = "/" + shown.split("static/", 1)[1]
            self.assertIn(f"shows {shown} by name, as an image of its own, and {rel} would be converted over it", said)
        for rel in quiet:
            self.assertNotIn(rel, said)
        self.assertEqual(self.companions(), {})

    def test_names_a_shell_reads_twice_are_names(self):
        names = ("a b.png", "it's].jpg", "$(touch pwned).png", "[x]=1.PNG", "two\nlines.jpg", "it's].JPEG",
                 "one\nstem.JPG", "one\nstem.png", "one\nother.png")
        for name in names:
            self.write(f"content/blog/{name}")
        said = self.refused()
        self.assertIn("content/blog/it's].JPEG and content/blog/it's].jpg share one WebP companion", said)
        # A newline in the name is part of the name: these are two images
        # with one companion, as any two are.
        self.assertIn("content/blog/one\nstem.JPG and content/blog/one\nstem.png share one WebP companion, "
                      "content/blog/one\nstem.webp", said)
        self.assertEqual(said.count("share one WebP companion"), 2)
        self.assertEqual(self.companions(), {})
        (self.root / "content/blog/it's].JPEG").unlink()
        (self.root / "content/blog/one\nstem.JPG").unlink()
        self.assertIn("7 converted, 0 already present", self.convert())
        self.assertEqual(set(self.companions()), {
            "content/blog/a b.webp", "content/blog/it's].webp", "content/blog/$(touch pwned).webp",
            "content/blog/[x]=1.webp", "content/blog/two\nlines.webp", "content/blog/one\nstem.webp",
            "content/blog/one\nother.webp"})
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
        # So with the generator, which says what the pages show by name.
        for binary in ("/bin/false", str(self.root / "no-such-generator")):
            with self.subTest(binary=binary):
                done = self.run_script(SITE_BINARY=binary)
                self.assertNotEqual(done.returncode, 0, done.stdout + done.stderr)
                self.assertIn("could not ask the generator", done.stderr)
                self.assertNotIn("converted", done.stdout)
        (self.root / "content/blog/map.JPG").unlink()
        done = self.run_script(SITE_BINARY="/bin/false")
        self.assertNotEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertIn("could not ask the generator", done.stderr)
        self.assertEqual(self.companions(), {"content/blog/map.webp": "map.png"})
        self.write("content/blog/map.JPG")
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
