"""tools/convert-images.sh: a WebP companion beside every JPEG and PNG, and
no other file touched.

Run over a scratch site and a scratch engine (the script walks both, and
this must write nothing into the real one) with a stand-in for cwebp:

  · an image gets its companion whichever case its extension is in, and
    the ownership record says which image and which bytes
    (tools/derived_images.py; the engine's images in the engine's record);
  · the script's own companion is made again when its image is newer, and
    left when it is not;
  · a WebP the script did not record is never written over, however old:
    one beside a JPEG that was there first, one a page shows by any name
    or path, one the script made and the author has changed since. It is
    counted and named, and stays byte for byte;
  · a companion whose image is gone is still the script's, and the image
    that takes the name gets a new one;
  · a declared companion (data/companions.json) is the author's: not
    written, not counted as left, however its line spells the path, and
    out of the record once declared; a line that leads to another tree's
    file (the engine's inside the site, too) stops the tools;
  · a companion is its image's by the image's bytes: kept when the time
    alone changed, made again when the bytes did;
  · its own companion is made again when its image is another file than
    it was made from, whichever is newer;
  · an engine image's companion is declared in the engine, for every site
    that links it, and two sites converting the engine's images at once
    both keep their entries;
  · `make adopt-images` records the companions already there that a fresh
    conversion reproduces and lists the others, and a preview of it
    changes nothing at all;
  · two images that would be shown with one companion (map.png, map.JPG,
    map.gif; the site's static image and the engine's) are refused, each
    named, before anything is converted, with or without cwebp, and
    nothing is deleted;
  · another tool's output (a photograph's variant) is not an image to
    convert;
  · no name is told from another by what is in it, a newline included;
  · a walk that fails, or a record that cannot be read, is a failure;
  · a directory named as an image is passed over, and a link to an image
    is converted.

    python -m unittest tests.test_convert_images -v
"""

from __future__ import annotations

import hashlib
import json
import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import engine_path, script_env, stub_bin

# cwebp -quiet -q 85 <image> -o <companion>: writes the image's name and
# what the image holds, as a conversion depends on both.
CWEBP = """#!/bin/sh
image=$4
while [ "$1" != "-o" ]; do shift; done
{ printf '%s:' "$(basename "$image")"; cat "$image"; } > "$2"
"""
TOOLS = ("convert-images.sh", "convert_images.py", "derived_images.py", "sitelib.py",
         "generate-thumbnails.py", "photo_naming.py")


def made(name: str, holds: str = "x") -> str:
    """What the stand-in makes of an image of that name and content."""
    return f"{name}:{holds}"


class ConvertImages(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory(prefix="convert-images-")
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name).resolve()
        self.stubs = stub_bin(self.root / "bin", ["cwebp"], CWEBP)
        # The script takes the engine to be the directory above its own.
        tools = self.root / "engine/tools"
        tools.mkdir(parents=True)
        for name in TOOLS:
            shutil.copy(engine_path(f"tools/{name}"), tools / name)

    def write(self, rel: str, text: str = "x") -> Path:
        path = self.root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")
        return path

    def run_script(self, *argv: str, **env: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(["bash", str(self.root / "engine/tools/convert-images.sh"), *argv], cwd=self.root,
                              env={**script_env(self.stubs, SITE_ROOT=str(self.root)), **env},
                              capture_output=True, text=True, timeout=60)

    def convert(self, *argv: str) -> str:
        done = self.run_script(*argv)
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        return done.stdout + done.stderr

    def adopt(self, *argv: str, **env: str) -> subprocess.CompletedProcess[str]:
        """`make adopt-images`: both tools' stages, in one run."""
        return subprocess.run([shutil.which("python3"), str(self.root / "engine/tools/derived_images.py"), "adopt",
                               *argv], cwd=self.root, capture_output=True, text=True, timeout=60,
                              env={**script_env(self.stubs, SITE_ROOT=str(self.root)), **env})

    def run_declare(self, *argv: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run([shutil.which("python3"), str(self.root / "engine/tools/derived_images.py"), "declare",
                               *argv], cwd=self.root, capture_output=True, text=True, timeout=60,
                              env=script_env(self.stubs, SITE_ROOT=str(self.root)))

    def refused(self, **env: str) -> str:
        done = self.run_script(**env)
        self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
        self.assertNotIn("converted", done.stdout)
        return done.stderr

    def companions(self) -> dict[str, str]:
        return {p.relative_to(self.root).as_posix(): p.read_text(encoding="utf-8")
                for p in self.root.rglob("*.webp")}

    def record(self, tree: str = "") -> dict[str, dict[str, str]]:
        path = self.root / tree / "data/derived-images.json"
        return json.loads(path.read_text(encoding="utf-8"))["outputs"] if path.is_file() else {}

    def bare_path(self) -> str:
        """A PATH of what the script needs besides cwebp: a machine
        without the converter."""
        bare = self.root / "bare"
        if not bare.is_dir():
            bare.mkdir()
            for tool in ("bash", "dirname", "python3"):
                (bare / tool).symlink_to(shutil.which(tool))
        return str(bare)

    # -- what it makes, and says it made ---------------------------------

    def test_an_extension_counts_in_either_case_and_each_companion_is_recorded(self):
        for rel in ("content/blog/lake.jpg", "content/blog/big/Lake.JPG", "content/essays/a/Figure.Png",
                    "static/images/portrait.JPEG", "engine/static/images/Mark.PNG",
                    "content/blog/chart.svg", "content/blog/moving.GIF"):
            self.write(rel)
        self.assertIn("5 converted, 0 already present", self.convert())
        self.assertEqual(self.companions(), {
            "content/blog/lake.webp": made("lake.jpg"), "content/blog/big/Lake.webp": made("Lake.JPG"),
            "content/essays/a/Figure.webp": made("Figure.Png"), "static/images/portrait.webp": made("portrait.JPEG"),
            "engine/static/images/Mark.webp": made("Mark.PNG")})
        # Each bound to the script, its image and the bytes written; the
        # engine's in the engine's own record, which its sites share.
        site, engine = self.record(), self.record("engine")
        self.assertEqual(set(site), {"content/blog/lake.webp", "content/blog/big/Lake.webp",
                                     "content/essays/a/Figure.webp", "static/images/portrait.webp"})
        self.assertEqual(site["content/blog/big/Lake.webp"]["producer"], "convert-images")
        self.assertEqual(site["content/blog/big/Lake.webp"]["source"], "content/blog/big/Lake.JPG")
        self.assertRegex(site["content/blog/big/Lake.webp"]["digest"], r"^sha256:[0-9a-f]{64}$")
        self.assertEqual({k: v["source"] for k, v in engine.items()},
                         {"static/images/Mark.webp": "static/images/Mark.PNG"})
        self.assertIn("0 converted, 5 already present", self.convert())

    def test_its_own_companion_is_made_again_when_the_image_is_newer(self):
        image = self.write("content/blog/big/Lake.JPG")
        self.convert()
        companion = self.root / "content/blog/big/Lake.webp"
        self.assertIn("0 converted, 1 already present", self.convert())
        os.utime(companion, (1_000_000_000,) * 2)
        image.write_text("edited", encoding="utf-8")
        self.assertIn("1 converted, 0 already present", self.convert())
        self.assertEqual(companion.read_text(encoding="utf-8"), made("Lake.JPG", "edited"))
        self.assertIn("0 converted, 1 already present", self.convert())

    # -- what it leaves alone --------------------------------------------

    def test_a_webp_it_did_not_record_is_never_written_over(self):
        # Every way an author's WebP came to be beside an image of its
        # name, each older than the image, which is when a converter that
        # went by the dates wrote over it. No page is read to find out.
        theirs = ("content/blog/color.webp",                  # shown by a page: ![](color.webp)
                  "content/essays/trip/figures/map.webp",     # shown by a path from the site's root
                  "content/photography/harbor/harbor.webp",   # a photograph's `photo:`
                  "static/images/A&B.webp",                   # named with a character reference
                  "static/images/art.webp",                   # in a srcset
                  "engine/static/images/brand.webp")
        for rel in theirs:
            webp = self.write(rel, "the author's")
            os.utime(webp, (1_000_000_000,) * 2)
        for rel in ("content/blog/color.JPG", "content/essays/trip/figures/map.JPG",
                    "content/photography/harbor/harbor.JPG", "static/images/A&B.JPG", "static/images/art.png",
                    "engine/static/images/brand.png", "content/blog/new.jpg"):
            self.write(rel)
        for counted in ("1 converted, 0 already present", "0 converted, 1 already present"):
            said = self.convert()
            self.assertIn(counted, said)
            self.assertIn("6 WebP file(s) beside an image are not recorded as made from it by this script, and are "
                          "left as they are", said)
            self.assertIn("content/blog/color.webp", said)
            self.assertEqual(self.companions(), {**{rel: "the author's" for rel in theirs},
                                                 "content/blog/new.webp": made("new.jpg")})
            self.assertEqual(set(self.record()), {"content/blog/new.webp"})
            self.assertEqual(self.record("engine"), {})

    def test_its_own_companion_changed_by_the_author_is_the_author_s(self):
        image = self.write("content/blog/lake.jpg")
        self.convert()
        companion = self.root / "content/blog/lake.webp"
        companion.write_text("retouched by hand", encoding="utf-8")
        os.utime(companion, (1_000_000_000,) * 2)
        image.write_text("edited too", encoding="utf-8")
        said = self.convert()
        self.assertIn("0 converted, 0 already present", said)
        self.assertIn("1 WebP file(s) beside an image are not recorded", said)
        self.assertEqual(companion.read_text(encoding="utf-8"), "retouched by hand")

    def test_a_companion_whose_image_is_gone_is_the_script_s_still(self):
        self.write("content/blog/map.png")
        self.convert()
        # The image is exported again under another extension: the old
        # companion is the script's own bytes, of an image that is gone.
        (self.root / "content/blog/map.png").unlink()
        self.write("content/blog/map.JPG", "exported again")
        self.assertIn("1 converted, 0 already present", self.convert())
        self.assertEqual(self.companions(), {"content/blog/map.webp": made("map.JPG", "exported again")})
        self.assertEqual(self.record()["content/blog/map.webp"]["source"], "content/blog/map.JPG")
        # Had the author changed it meanwhile, it would not be.
        (self.root / "content/blog/map.webp").write_text("retouched", encoding="utf-8")
        (self.root / "content/blog/map.JPG").unlink()
        self.write("content/blog/map.jpeg")
        self.assertIn("0 converted, 0 already present", self.convert())
        self.assertEqual(self.companions(), {"content/blog/map.webp": "retouched"})

    def test_an_image_that_is_another_file_gets_its_companion_again_whatever_the_dates(self):
        # map.png is replaced by a map.JPG that is older than the companion
        # made from the PNG: by the dates the companion is current, and it
        # holds another image's pixels.
        self.write("content/blog/map.png", "the old map")
        self.convert()
        (self.root / "content/blog/map.png").unlink()
        os.utime(self.write("content/blog/map.JPG", "another map"), (1_000_000_000,) * 2)
        self.assertIn("1 converted, 0 already present", self.convert())
        self.assertEqual(self.companions(), {"content/blog/map.webp": made("map.JPG", "another map")})
        self.assertEqual(self.record()["content/blog/map.webp"]["source"], "content/blog/map.JPG")
        self.assertIn("0 converted, 1 already present", self.convert())
        # And a file of the same name put in its place, older too.
        os.utime(self.write("content/blog/map.JPG", "a third map."), (900_000_000,) * 2)
        self.assertIn("1 converted, 0 already present", self.convert())
        self.assertEqual(self.companions(), {"content/blog/map.webp": made("map.JPG", "a third map.")})
        self.assertIn("0 converted, 1 already present", self.convert())

    def test_a_declared_companion_is_the_author_s_and_is_not_written(self):
        gif = self.write("content/blog/moving.gif")
        webp = self.write("content/blog/moving.webp", "by hand")
        still = self.write("content/blog/still.jpg")
        hand = self.write("content/blog/still.webp", "by hand too")
        os.utime(hand, (1_000_000_000,) * 2)
        declare = [shutil.which("python3"), str(self.root / "engine/tools/derived_images.py"), "declare"]
        env = script_env(SITE_ROOT=str(self.root))
        (self.root / "data").mkdir()
        (self.root / "data/rules-hash.txt").write_text("abc\n", encoding="utf-8")
        # The one image of its name beside it, or the image named.
        for argv in ([str(webp)], [str(hand), str(still)]):
            done = subprocess.run(declare + argv, cwd=self.root, env=env, capture_output=True, text=True)
            self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertEqual(json.loads((self.root / "data/companions.json").read_text(encoding="utf-8")),
                         {"content/blog/moving.webp": "content/blog/moving.gif",
                          "content/blog/still.webp": "content/blog/still.jpg"})
        # Pages were built without it: the next build is a full one.
        self.assertFalse((self.root / "data/rules-hash.txt").exists())
        said = self.convert()
        self.assertIn("0 converted, 1 already present", said)
        self.assertNotIn("not recorded", said)
        self.assertEqual(self.companions(), {"content/blog/moving.webp": "by hand",
                                             "content/blog/still.webp": "by hand too"})
        self.assertEqual(self.record(), {})
        # What cannot be declared: a file that is not there, one with two
        # images of its name and none said.
        self.write("content/blog/moving.png")
        for argv in ([str(self.root / "content/blog/none.webp")], [str(webp)]):
            done = subprocess.run(declare + argv, cwd=self.root, env=env, capture_output=True, text=True)
            self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
        self.assertEqual(gif.read_text(encoding="utf-8"), "x")

    def test_a_declaration_is_read_as_the_file_it_leads_to(self):
        # The converter made lake.webp; its author then made it their own
        # and wrote the line by hand, with a path as a shell would give it.
        image = self.write("static/lake.jpg")
        self.convert()
        webp = self.root / "static/lake.webp"
        webp.write_text("by hand", encoding="utf-8")
        declared = self.root / "data/companions.json"
        for line in ({"./static/lake.webp": "static/lake.jpg"},
                     {"static/../static/lake.webp": "./static/./lake.jpg"},
                     {"static//lake.webp": "static/lake.jpg"}):
            with self.subTest(line=line):
                declared.write_text(json.dumps(line), encoding="utf-8")
                image.write_text("edited " + next(iter(line)), encoding="utf-8")
                for argv in ((), ()):
                    said = self.convert(*argv)
                    self.assertIn("0 converted, 1 already present", said)
                    self.assertNotIn("not recorded", said)
                    self.assertEqual(webp.read_text(encoding="utf-8"), "by hand")
        # Two lines for one file, of two images: which is it?
        self.write("static/other.jpg")
        declared.write_text(json.dumps({"./static/lake.webp": "static/lake.jpg",
                                        "static/lake.webp": "static/other.jpg"}), encoding="utf-8")
        self.assertIn("declared the companion of two images", self.refused())
        # A line that leads out of the tree is no line of this tree's.
        for line in ({"../lake.webp": "static/lake.jpg"}, {"static/lake.webp": "/etc/hostname"},
                     {"static/lake.webp": 3}, {"": "static/lake.jpg"}):
            with self.subTest(line=line):
                declared.write_text(json.dumps(line), encoding="utf-8")
                self.assertIn("is not a file of this tree", self.refused())
        self.assertEqual(webp.read_text(encoding="utf-8"), "by hand")

    def test_a_site_s_line_for_a_file_of_the_engine_inside_it_stops_the_tools(self):
        # The engine as a directory of the site: its files are under the
        # site's root, and are the engine's all the same.
        image = self.write("engine/static/lake.jpg")
        self.convert()
        webp = self.root / "engine/static/lake.webp"
        webp.write_text("by hand", encoding="utf-8")
        (self.root / "data/companions.json").write_text(
            '{"engine/static/lake.webp": "engine/static/lake.jpg"}', encoding="utf-8")
        image.write_text("edited", encoding="utf-8")
        self.assertIn("an engine image's companion is declared in the engine", self.refused())
        self.assertEqual(webp.read_text(encoding="utf-8"), "by hand")
        # Declared where it belongs, it is kept.
        (self.root / "data/companions.json").unlink()
        (self.root / "engine/data/companions.json").write_text(
            '{"static/lake.webp": "static/lake.jpg"}', encoding="utf-8")
        self.assertIn("0 converted, 1 already present", self.convert())
        self.assertEqual(webp.read_text(encoding="utf-8"), "by hand")

    def test_declaring_a_file_the_tools_made_takes_it_out_of_the_record(self):
        self.write("content/blog/lake.jpg")
        self.convert()
        self.assertIn("content/blog/lake.webp", self.record())
        done = subprocess.run([shutil.which("python3"), str(self.root / "engine/tools/derived_images.py"),
                               "declare", "content/blog/lake.webp"], cwd=self.root,
                              env=script_env(SITE_ROOT=str(self.root)), capture_output=True, text=True)
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertEqual(self.record(), {})
        # Withdrawn again, it is no one's: not the converter's to write.
        (self.root / "data/companions.json").write_text("{}", encoding="utf-8")
        self.write("content/blog/lake.jpg", "edited")
        said = self.convert()
        self.assertIn("0 converted, 0 already present", said)
        self.assertIn("1 WebP file(s) beside an image are not recorded", said)
        self.assertEqual(self.companions(), {"content/blog/lake.webp": made("lake.jpg")})

    def test_a_companion_is_its_image_s_by_the_image_s_bytes(self):
        # Another time and the same bytes (a checkout): read once, kept,
        # and the entry brought up to the time. Other bytes: made again.
        image = self.write("content/blog/lake.jpg")
        self.convert()
        entry = self.record()["content/blog/lake.webp"]
        self.assertRegex(entry["source-digest"], r"^sha256:[0-9a-f]{64}$")
        os.utime(image, (1_000_000_000,) * 2)
        self.assertIn("0 converted, 1 already present", self.convert())
        self.assertEqual(self.record()["content/blog/lake.webp"]["source-stamp"],
                         [image.stat().st_mtime_ns, image.stat().st_size])
        self.assertNotEqual(self.record()["content/blog/lake.webp"]["source-stamp"], entry["source-stamp"])
        # Older than the companion, and another picture.
        image.write_text("y", encoding="utf-8")
        os.utime(image, (900_000_000,) * 2)
        self.assertIn("1 converted, 0 already present", self.convert())
        self.assertEqual(self.companions(), {"content/blog/lake.webp": made("lake.jpg", "y")})

    def shared_engine(self) -> tuple[Path, Path, Path]:
        """Two sites that link one engine, as two sites on one machine do,
        each with the tools as that engine has them."""
        engine = self.root / "shared-engine"
        (self.root / "engine").rename(engine)
        sites = []
        for name in ("site-a", "site-b"):
            site = self.root / name
            site.mkdir()
            (site / "engine").symlink_to(engine)
            sites.append(site)
        return engine, sites[0], sites[1]

    def run_in(self, site: Path, script: list[str], **env: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(script, cwd=site, capture_output=True, text=True, timeout=60,
                              env={**script_env(self.stubs, SITE_ROOT=str(site)), **env})

    def test_an_engine_image_s_companion_is_declared_in_the_engine_for_every_site(self):
        engine, first, second = self.shared_engine()
        image = engine / "static/images/probe.jpg"
        image.parent.mkdir(parents=True)
        image.write_text("x", encoding="utf-8")
        webp = engine / "static/images/probe.webp"
        webp.write_text("by hand", encoding="utf-8")
        os.utime(webp, (1_000_000_000,) * 2)
        # Declared from one site, by the path that site has it at.
        done = self.run_in(first, [shutil.which("python3"), "engine/tools/derived_images.py", "declare",
                                   "engine/static/images/probe.webp"])
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertEqual(json.loads((engine / "data/companions.json").read_text(encoding="utf-8")),
                         {"static/images/probe.webp": "static/images/probe.jpg"})
        self.assertFalse((first / "data/companions.json").exists())
        # Neither site's converter writes to it, whatever happens to the image.
        image.write_text("edited", encoding="utf-8")
        for site in (first, second):
            done = self.run_in(site, ["bash", "engine/tools/convert-images.sh"])
            self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
            self.assertIn("0 converted, 1 already present", done.stdout)
            self.assertEqual(webp.read_text(encoding="utf-8"), "by hand")
        # A site's own line for an engine file would bind that site alone:
        # the tools stop at it.
        (second / "data").mkdir(exist_ok=True)
        (second / "data/companions.json").write_text(
            '{"engine/static/images/probe.webp": "engine/static/images/probe.jpg"}', encoding="utf-8")
        done = self.run_in(second, ["bash", "engine/tools/convert-images.sh"])
        self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
        self.assertIn("is not a file of this tree", done.stderr)
        self.assertEqual(webp.read_text(encoding="utf-8"), "by hand")

    def test_two_sites_converting_the_engine_s_images_at_once_both_keep_their_entries(self):
        engine, first, second = self.shared_engine()
        for name in ("one.png", "two.png"):
            path = engine / "static/images" / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("x", encoding="utf-8")
        for site in (first, second):
            (site / "content").mkdir()
            (site / "content/own.jpg").write_text("x", encoding="utf-8")
        # A converter that takes its time, so that the second site starts
        # while the first is at work.
        slow = stub_bin(self.root / "slow", ["cwebp"], CWEBP.replace("image=$4", "image=$4; sleep 0.4"))
        runs = [subprocess.Popen(["bash", "engine/tools/convert-images.sh"], cwd=site, text=True,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                 env=script_env(slow, SITE_ROOT=str(site)))
                for site in (first, second)]
        outputs = [run.communicate(timeout=60) + (run.returncode,) for run in runs]
        for out, err, code in outputs:
            self.assertEqual(code, 0, out + err)
        self.assertEqual(sorted(out.splitlines()[-1] for out, _, _ in outputs),
                         ["convert-images: 1 converted, 2 already present.",
                          "convert-images: 3 converted, 0 already present."])
        self.assertTrue(any("waiting for another tool" in err for _, err, _ in outputs))
        record = json.loads((engine / "data/derived-images.json").read_text(encoding="utf-8"))["outputs"]
        self.assertEqual(set(record), {"static/images/one.webp", "static/images/two.webp"})
        for site in (first, second):
            own = json.loads((site / "data/derived-images.json").read_text(encoding="utf-8"))["outputs"]
            self.assertEqual(set(own), {"content/own.webp"})

    def test_another_tool_s_output_is_not_an_image_to_convert(self):
        # A photograph's variant and its WebP are generate-thumbnails'.
        self.write("content/photography/trip/view.jpg")
        self.write("content/photography/trip/view.w480.jpg")
        (self.root / "data").mkdir()
        (self.root / "data/derived-images.json").write_text(json.dumps({"version": 1, "outputs": {
            "content/photography/trip/view.w480.jpg": {
                "producer": "generate-thumbnails", "source": "content/photography/trip/view.jpg",
                "digest": "sha256:" + "0" * 64}}}), encoding="utf-8")
        said = self.convert()
        self.assertIn("1 converted, 0 already present", said)
        self.assertEqual(set(self.companions()), {"content/photography/trip/view.webp"})
        self.assertIn("content/photography/trip/view.w480.jpg", self.record())

    # -- adoption ----------------------------------------------------------

    def with_cwebp(self, name: str, script: str) -> subprocess.CompletedProcess[str]:
        """The script, with another stand-in for cwebp."""
        stubs = stub_bin(self.root / name, ["cwebp"], script)
        return subprocess.run(["bash", str(self.root / "engine/tools/convert-images.sh")], cwd=self.root,
                              env=script_env(stubs, SITE_ROOT=str(self.root)),
                              capture_output=True, text=True, timeout=60)

    @staticmethod
    def sha(text: str) -> str:
        return "sha256:" + hashlib.sha256(text.encode("utf-8")).hexdigest()

    def test_a_companion_saved_during_conversion_is_preserved(self):
        source = self.write("content/lake.jpg", "first")
        companion = source.with_suffix(".webp")
        done = self.with_cwebp("author-save", CWEBP + f'printf handmade > "{companion}"\n')
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(companion.read_text(), "handmade")
        self.assertNotIn("content/lake.webp", self.record())

        companion.unlink()
        self.convert()
        old = self.record()
        source.write_text("second export")
        done = self.with_cwebp("author-edit", CWEBP + f'printf edited > "{companion}"\n')
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(companion.read_text(), "edited")
        self.assertEqual(self.record(), old)

    def test_a_companion_edited_on_a_retry_is_preserved(self):
        source = self.write("content/lake.jpg", "first")
        companion = source.with_suffix(".webp")
        done = self.with_cwebp("save-on-retry", CWEBP + f"""
if [ -e "{companion}" ]; then
    printf handmade > "{companion}"
else
    printf second > "{source}"
fi
""")
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(companion.read_text(), "handmade")
        entry = self.record()["content/lake.webp"]
        self.assertEqual(entry["digest"], self.sha(made("lake.jpg", "first")))
        self.convert()
        self.assertEqual(companion.read_text(), "handmade")

    def test_an_image_saved_again_while_it_is_converted_is_converted_again(self):
        # Another picture is saved over the image after cwebp has read it
        # and before the script has recorded what it made (once: the
        # stand-in leaves a mark). The companion is of the picture before,
        # and is made again, in the same run, of the one that is there.
        image = self.write("content/blog/lake.jpg", "first")
        mark = self.root / "saved-again"
        done = self.with_cwebp("saving", CWEBP + f'''
[ -e "{mark}" ] || {{ : > "{mark}"; printf 'another, longer' > "{image}"; }}
''')
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertTrue(mark.exists())
        self.assertEqual(self.companions(), {"content/blog/lake.webp": made("lake.jpg", "another, longer")})
        entry = self.record()["content/blog/lake.webp"]
        self.assertEqual(entry["source-digest"], self.sha("another, longer"))
        self.assertEqual(entry["digest"], self.sha(made("lake.jpg", "another, longer")))
        self.assertIn("0 converted, 1 already present", self.convert())

    def test_an_image_saved_again_every_time_is_recorded_as_the_one_that_was_converted(self):
        # The image never holds still. The script stops trying, and what
        # it leaves is true: the entry names the bytes the companion was
        # made of, which are not the image's now, so nothing takes the
        # companion for the image's and the next run makes it again.
        image = self.write("content/blog/lake.jpg", "v")
        done = self.with_cwebp("restless", CWEBP + f'''printf '+' >> "{image}"\n''')
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertIn("was saved again each of the 3 times", done.stderr)
        self.assertEqual(image.read_text(encoding="utf-8"), "v+++")
        self.assertEqual(self.companions(), {"content/blog/lake.webp": made("lake.jpg", "v++")})
        entry = self.record()["content/blog/lake.webp"]
        self.assertEqual(entry["source-digest"], self.sha("v++"))
        self.assertEqual(entry["digest"], self.sha(made("lake.jpg", "v++")))
        self.assertIn("1 converted, 0 already present", self.convert())
        self.assertEqual(self.companions(), {"content/blog/lake.webp": made("lake.jpg", "v+++")})
        self.assertEqual(self.record()["content/blog/lake.webp"]["source-digest"], self.sha("v+++"))

    def test_declaring_a_site_s_webp_for_an_image_of_the_engine_inside_it_is_refused(self):
        # The engine as a directory of the site: `declare` writes no line
        # the next run of the tools would stop on.
        self.write("engine/static/lake.png")
        self.write("static/lake.webp", "by hand")
        done = subprocess.run([shutil.which("python3"), str(self.root / "engine/tools/derived_images.py"),
                               "declare", "static/lake.webp", "engine/static/lake.png"], cwd=self.root,
                              env=script_env(SITE_ROOT=str(self.root)), capture_output=True, text=True)
        self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
        self.assertIn("are not files of one tree", done.stderr)
        self.assertFalse((self.root / "data/companions.json").exists())
        self.assertFalse((self.root / "engine/data/companions.json").exists())
        self.assertIn("1 converted", self.convert())

    def test_a_declaration_through_a_link_to_a_directory_is_the_file_it_leads_to(self):
        self.write("static/brand-dir/lake.jpg")
        webp = self.write("static/brand-dir/lake.webp", "by hand")
        (self.root / "shortcut").symlink_to("static/brand-dir")
        self.write("data/companions.json", '{"shortcut/lake.webp": "shortcut/lake.jpg"}')
        self.assertIn("0 converted, 1 already present", self.convert())
        self.assertEqual(webp.read_text(encoding="utf-8"), "by hand")

    def test_adoption_completes_an_entry_that_does_not_say_which_bytes_its_image_had(self):
        # A record of an earlier form: each entry with the image's path,
        # and nothing of its bytes or its time. One image was edited since.
        for name in ("lake.jpg", "edited.jpg"):
            self.write(f"content/blog/{name}")
        self.convert()
        path = self.root / "data/derived-images.json"
        document = json.loads(path.read_text(encoding="utf-8"))
        for entry in document["outputs"].values():
            del entry["source-digest"], entry["source-stamp"]
        path.write_text(json.dumps(document), encoding="utf-8")
        self.write("content/blog/edited.jpg", "edited since")
        before = (self.companions(), path.read_bytes())

        said = self.adopt("--dry-run").stdout
        self.assertIn("1 companion(s) would be recorded as this script's; 0 differ", said)
        self.assertEqual((self.companions(), path.read_bytes()), before)

        done = self.adopt()
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertIn("1 companion(s) recorded as this script's; 0 differ", done.stdout)
        self.assertIn("1 more are this script's by an entry that does not say which bytes", done.stdout)
        self.assertEqual(self.record()["content/blog/lake.webp"]["source-digest"], self.sha("x"))
        self.assertNotIn("source-digest", self.record()["content/blog/edited.webp"])
        self.assertEqual(self.companions(), before[0])
        # The one that is not its image's any more is made again.
        self.assertIn("1 converted, 1 already present", self.convert())
        self.assertEqual(self.record()["content/blog/edited.webp"]["source-digest"], self.sha("edited since"))

    def test_adoption_records_what_a_fresh_conversion_reproduces(self):
        # Companions from before there was a record: three the script
        # made, one of them of an image edited since, and one an author
        # made.
        for name in ("lake.jpg", "hill.png", "edited.jpg", "sea.jpg"):
            self.write(f"content/blog/{name}")
        self.convert()
        (self.root / "data/derived-images.json").unlink()
        self.write("content/blog/edited.jpg", "edited since")
        self.write("content/blog/sea.webp", "by hand")
        (self.root / "data/rules-hash.txt").write_text("abc\n", encoding="utf-8")
        before = self.companions()
        # Until adopted, they are no one's: nothing is converted or kept.
        said = self.convert()
        self.assertIn("0 converted, 0 already present", said)
        self.assertIn("4 WebP file(s) beside an image are not recorded", said)
        self.assertIn("make adopt-images", said)

        said = self.adopt("--dry-run").stdout
        self.assertIn("2 companion(s) would be recorded as this script's; 2 differ and are left unrecorded", said)
        self.assertEqual(self.record(), {})
        self.assertFalse((self.root / "data/derived-images.json").exists())
        self.assertTrue((self.root / "data/rules-hash.txt").exists())

        done = self.adopt()
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        said = done.stdout
        self.assertIn("2 companion(s) recorded as this script's; 2 differ and are left unrecorded", said)
        self.assertIn("differs  content/blog/edited.webp", said)
        self.assertIn("differs  content/blog/sea.webp", said)
        self.assertEqual(set(self.record()), {"content/blog/lake.webp", "content/blog/hill.webp"})
        self.assertEqual(self.record()["content/blog/hill.webp"]["source"], "content/blog/hill.png")
        # Nothing was written but the record, and the next build is full.
        self.assertEqual(self.companions(), before)
        self.assertFalse((self.root / "data/rules-hash.txt").exists())
        said = self.convert()
        self.assertIn("0 converted, 2 already present", said)
        self.assertIn("2 WebP file(s) beside an image are not recorded", said)
        self.assertEqual(self.companions(), before)
        # Adoption needs the converter it compares with.
        done = self.adopt(PATH=self.bare_path())
        self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
        self.assertIn("no cwebp", done.stderr)

    def test_a_preview_of_adoption_changes_nothing(self):
        # A record with an entry whose file is gone, which a run tidies
        # away, and a companion from before the record, which an adoption
        # enters: a preview does neither.
        for name in ("lake.jpg", "hill.png"):
            self.write(f"content/blog/{name}")
        self.convert()
        (self.root / "content/blog/hill.webp").unlink()
        record = self.root / "data/derived-images.json"
        document = json.loads(record.read_text(encoding="utf-8"))
        del document["outputs"]["content/blog/lake.webp"]
        record.write_text(json.dumps(document), encoding="utf-8")
        before = record.read_bytes()
        done = self.adopt("--dry-run")
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertIn("1 companion(s) would be recorded", done.stdout)
        self.assertEqual(record.read_bytes(), before)
        self.assertEqual(set(p.name for p in (self.root / "data").iterdir()), {"derived-images.json"})
        self.assertEqual(self.adopt().returncode, 0)
        self.assertEqual(set(self.record()), {"content/blog/lake.webp"})

    # -- one companion, one image -----------------------------------------

    def test_two_images_with_one_companion_are_refused_before_anything_is_converted(self):
        # A published page shows map.png, and its companion is made.
        self.write("content/blog/map.png")
        self.convert()
        self.assertEqual(self.companions(), {"content/blog/map.webp": made("map.png")})
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
                self.assertIn("Rename all but one of the images.", said)
                self.assertIn("content/blog/map.webp is there already; this script made it from "
                              "content/blog/map.png", said)
                self.assertNotIn("content/blog/also.webp is there", said)
                self.assertNotIn("other.jpg", said)
                # Nothing was converted, and nothing was taken away.
                self.assertEqual(self.companions(), {"content/blog/map.webp": made("map.png")})
        # The draft's image renamed, and no more than that: the published
        # image keeps the companion made from it.
        (self.root / "content/blog/map.JPG").rename(self.root / "content/blog/plan.JPG")
        (self.root / "content/blog/also.PNG").unlink()
        (self.root / "content/blog/also.jpg").unlink()
        self.assertIn("3 converted, 1 already present", self.convert())
        self.assertEqual(self.companions(), {
            "content/blog/map.webp": made("map.png"), "content/blog/plan.webp": made("plan.JPG"),
            "content/blog/also.webp": made("also.jpeg"), "content/blog/other.webp": made("other.jpg")})

    def test_a_companion_already_made_from_the_wrong_image_does_not_outlast_the_rename(self):
        # As a converter before the record left it: the draft's newer
        # map.JPG has overwritten the companion of the published map.png,
        # and the companion is newer than both.
        for age, (name, text) in enumerate((("map.png", "x"), ("map.JPG", "x"), ("map.webp", made("map.JPG")))):
            os.utime(self.write(f"content/blog/{name}", text), (1_000_000_000 + age,) * 2)
        said = self.refused()
        self.assertIn("content/blog/map.webp is there already; it is not one this script made: if it is yours, "
                      "move it aside until the images are renamed", said)
        self.assertEqual(self.companions(), {"content/blog/map.webp": made("map.JPG")})
        # The author renames the draft's image and does nothing else. The
        # WebP is no one's by the record: it is not written over, and not
        # the companion of map.png either (the generator asks the record),
        # so the draft's pixels are shown with nothing.
        (self.root / "content/blog/map.JPG").rename(self.root / "content/blog/plan.JPG")
        said = self.convert()
        self.assertIn("1 converted, 0 already present", said)
        self.assertIn("1 WebP file(s) beside an image are not recorded", said)
        self.assertEqual(self.companions(), {"content/blog/map.webp": made("map.JPG"),
                                             "content/blog/plan.webp": made("plan.JPG")})
        self.assertNotIn("content/blog/map.webp", self.record())
        # Deleted, it is made again from the image that kept the name.
        (self.root / "content/blog/map.webp").unlink()
        self.convert()
        self.assertEqual(self.companions()["content/blog/map.webp"], made("map.png"))

    def aside(self, rel: str, holds: str | None, *, named: bool = True, tail: str = "abcd1234") -> Path:
        """What a stopped run leaves beside the companion at `rel`: the
        directory it reserved, the companion's name in it, and (unless
        `holds` is None) the file it took aside."""
        companion = self.root / rel
        key = hashlib.sha256(companion.name.encode()).hexdigest()[:16]
        reserved = companion.with_name(f".previous-image-{key}-{tail}.tmp")
        reserved.mkdir(parents=True)
        if named:
            (reserved / "name.tmp").write_text(companion.name, encoding="utf-8")
        if holds is not None:
            (reserved / "displaced.tmp").write_text(holds, encoding="utf-8")
        return reserved

    def asides(self) -> list[str]:
        return sorted(p.relative_to(self.root).as_posix() for p in self.root.rglob(".previous-image-*"))

    def test_what_a_stopped_run_took_aside_is_put_back_before_anything_is_converted(self):
        # A run stopped while it replaced a companion leaves the file it
        # took aside in a directory that says which it was. The script's
        # own goes back and is its own as before; a save of the author's,
        # made in the moment before it was taken, goes back, is named, and
        # is not converted over. In the engine's tree as in the site's.
        self.write("content/lake.jpg")
        self.write("engine/static/mark.png")
        self.convert()
        (self.root / "content/lake.webp").unlink()
        self.aside("content/lake.webp", "saved by its author")
        (self.root / "engine/static/mark.webp").unlink()
        self.aside("engine/static/mark.webp", made("mark.png"))

        done = self.run_script()
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertEqual(self.companions(), {"content/lake.webp": "saved by its author",
                                             "engine/static/mark.webp": made("mark.png")})
        self.assertIn("convert-images: 0 converted, 1 already present.", done.stdout)
        self.assertIn("content/lake.webp was set aside by a run that was stopped, and is back in its place",
                      done.stderr)
        self.assertNotIn("mark.webp", done.stderr)
        self.assertIn("1 WebP file(s) beside an image are not recorded as made from it", done.stderr)
        self.assertEqual(self.asides(), [])

    def test_a_run_stopped_before_it_took_a_file_aside_leaves_nothing_to_put_back(self):
        # The directory is reserved first. A run stopped then took nothing
        # aside: the next run clears it and says nothing, in the engine's
        # tree too, where a file Git shows would stop the next build. A
        # file taken aside that happens to be empty is still a file.
        self.write("content/lake.jpg")
        self.write("content/empty.jpg")
        self.write("engine/static/mark.png")
        self.convert()
        self.aside("content/lake.webp", None)
        self.aside("engine/static/mark.webp", None, named=False)
        (self.root / "content/empty.webp").unlink()
        self.aside("content/empty.webp", "")
        before = {**self.companions(), "content/empty.webp": ""}

        done = self.run_script()
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertEqual(self.companions(), before)
        self.assertEqual(self.asides(), [])
        self.assertEqual(done.stderr.count("was set aside by a run that was stopped"), 1, done.stderr)
        self.assertIn("content/empty.webp was set aside by a run that was stopped, and is back in its place",
                      done.stderr)

    def test_only_what_a_run_reserved_is_ever_put_back_or_cleared(self):
        # Names that begin as a reserved directory's does and are not one:
        # the author's, and left exactly as they are. So is a directory
        # named as one that is a link, or holds what no run puts there.
        self.write("content/lake.jpg")
        self.convert()
        notes = self.write("content/.previous-image-notes.tmp", "mine")
        named = self.write("content/.previous-image-0123456789abcdef-abcd1234.tmp", "a file, not a directory")
        folder = self.write("content/.previous-image-notes-2.tmp/displaced.tmp", "mine too")
        other = self.write("elsewhere/kept.txt", "another place").parent
        link = self.root / "content/.previous-image-0123456789abcdef-zzzz9999.tmp"
        link.symlink_to(other, target_is_directory=True)
        holds = self.aside("content/lake.webp", None)
        (holds / "sketch.txt").write_text("put here by hand", encoding="utf-8")
        (holds / "probe.tmp").write_text("and this", encoding="utf-8")

        done = self.run_script()
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertEqual(done.stderr, "")
        self.assertEqual((notes.read_text(), named.read_text(), folder.read_text()),
                         ("mine", "a file, not a directory", "mine too"))
        self.assertTrue(link.is_symlink())
        self.assertEqual(sorted(p.name for p in other.iterdir()), ["kept.txt"])
        self.assertEqual(sorted((p.name, p.read_text(encoding="utf-8")) for p in holds.iterdir()),
                         [("name.tmp", "lake.webp"), ("probe.tmp", "and this"), ("sketch.txt", "put here by hand")])

    def test_a_directory_that_is_another_tree_s_now_is_not_looked_into(self):
        # content/ was the site's own directory when a run was stopped in
        # it, and is a link into another site now. What is aside there is
        # that site's, to be put back by its record and under its lock.
        elsewhere = tempfile.TemporaryDirectory(prefix="convert-images-other-")
        self.addCleanup(elsewhere.cleanup)
        other = Path(elsewhere.name).resolve()
        (other / "content").mkdir()
        (other / "content/lake.jpg").write_text("x", encoding="utf-8")
        (other / "content/lake.webp").write_text("the other site's", encoding="utf-8")
        self.write("content/lake.jpg")
        self.convert()
        shutil.rmtree(self.root / "content")
        (self.root / "content").symlink_to(other / "content", target_is_directory=True)
        key = hashlib.sha256(b"lake.webp").hexdigest()[:16]
        taken = [other / f"content/.previous-image-{key}-abcd1234.tmp",
                 other / f"content/.previous-image-{key}-efgh5678.tmp"]
        for reserved, holds in zip(taken, (made("lake.jpg"), "saved by the other site's author")):
            reserved.mkdir()
            (reserved / "name.tmp").write_text("lake.webp", encoding="utf-8")
            (reserved / "displaced.tmp").write_text(holds, encoding="utf-8")

        done = self.run_script()
        self.assertNotIn("was set aside", done.stdout + done.stderr)
        self.assertEqual([(reserved / "displaced.tmp").read_text(encoding="utf-8") for reserved in taken],
                         [made("lake.jpg"), "saved by the other site's author"])
        self.assertEqual((other / "content/lake.webp").read_text(encoding="utf-8"), "the other site's")

    def test_an_adoption_waits_for_what_a_stopped_run_took_aside(self):
        # A replacement stopped with the script's old companion aside and
        # the name empty: an adoption would drop the entry of a file that
        # is coming back, and the companion would come back as nobody's.
        # Stopped after the new one was put in: it would bring the entry
        # up to the new one, and the old one would come back as a save of
        # the author's. So it changes nothing, and says what puts them back.
        for name in ("before", "after"):
            self.write(f"content/{name}.jpg")
        self.convert()
        for name in ("before", "after"):
            self.write(f"content/{name}.jpg", "saved again")
            (self.root / f"content/{name}.webp").unlink()
            self.aside(f"content/{name}.webp", made(f"{name}.jpg"))
        self.write("content/after.webp", made("after.jpg", "saved again"))
        path = self.root / "data/derived-images.json"
        before = (self.companions(), path.read_bytes(), self.asides())

        for argv in (("--dry-run",), ()):
            done = self.adopt(*argv)
            self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
            self.assertIn("a run that was stopped took 2 file(s) aside, and they are not back in their places",
                          done.stderr)
            self.assertIn("Nothing was adopted.", done.stderr)
            self.assertIn("The next conversion puts these back (make convert-images, or a build):", done.stderr)
            self.assertNotIn("These no tool moves", done.stderr)
            self.assertNotIn("recorded as this script's", done.stdout)
            self.assertEqual((self.companions(), path.read_bytes(), self.asides()), before)
        # So does a declaration, of the file that is there and of the one
        # that is aside itself, which it names.
        for name in ("after", "before"):
            declared = self.run_declare(f"content/{name}.webp")
            self.assertEqual(declared.returncode, 1, declared.stdout + declared.stderr)
            self.assertIn("Nothing was declared", declared.stderr)
            self.assertRegex(declared.stderr, r"The next conversion puts these back .*:\n(    \S+\n)*    content/"
                                              r"\.previous-image-[0-9a-f]{16}-abcd1234\.tmp/displaced\.tmp\n")
        self.assertFalse((self.root / "data/companions.json").exists())

        # The conversion puts them back: the one is the script's own and
        # is made again, the other is aside from a name that has a file.
        done = self.run_script()
        self.assertEqual((done.returncode, self.asides()), (0, []), done.stdout + done.stderr)
        self.assertNotIn("was set aside", done.stderr)
        self.assertEqual(self.companions(), {"content/before.webp": made("before.jpg", "saved again"),
                                             "content/after.webp": made("after.jpg", "saved again")})
        self.assertIn("content/after.webp", done.stderr)
        # And the adoption then finds the one whose entry the run never saved.
        done = self.adopt()
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertIn("  repair   content/after.webp", done.stdout)
        self.assertIn("0 converted, 2 already present", self.convert())

    def test_what_no_tool_puts_back_is_not_said_to_be_put_back_by_a_conversion(self):
        # A reserved directory that is not as a run leaves one (a file
        # beside the file aside, here) is left whole by a conversion. An
        # adoption that waits for it says so, and not to run a conversion,
        # which would change nothing; and what it says to do loses
        # nothing: not the file aside, not a newer save under its name,
        # not what else is in the directory.
        self.write("content/lake.jpg")
        self.write("content/pier.jpg")
        self.convert()
        (self.root / "content/lake.webp").unlink()
        crowded = self.aside("content/lake.webp", "saved by its author")
        (crowded / "sketch.txt").write_text("put here by hand", encoding="utf-8")
        newer = self.write("content/pier.webp", "a newer save")
        beside = self.aside("content/pier.webp", "an earlier save", tail="efgh5678")
        (beside / "notes.txt").write_text("and these", encoding="utf-8")

        done = self.run_script()
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertEqual(done.stderr.count("looks like a file set aside by a run that was stopped"), 2)
        for run in (self.adopt, lambda: self.run_declare("content/lake.webp")):
            done = run()
            self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
            self.assertIn("These no tool moves", done.stderr)
            for reserved in (crowded, beside):
                self.assertIn(f"    content/{reserved.name}/displaced.tmp\n", done.stderr)
            self.assertNotIn("The next conversion puts these back", done.stderr)
            said = " ".join(done.stderr.split())
            self.assertIn("delete none before you have looked at it", said)
            self.assertIn("to the name it had, if no file has that name now, and otherwise to a new name beside "
                          "that file, so that both are kept (not over it)", said)
            self.assertIn("remove the directory only once it is empty", said)
            self.assertEqual((crowded / "displaced.tmp").read_text(encoding="utf-8"), "saved by its author")

        # Done as it says: the one to its name, which is free; the other
        # to a new name, beside the newer save. The directories still hold
        # what somebody put in them, and stay.
        os.replace(crowded / "displaced.tmp", self.root / "content/lake.webp")
        os.replace(beside / "displaced.tmp", self.root / "content/pier.earlier.webp")
        done = self.adopt()
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        done = self.run_script()
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertNotIn("set aside", done.stderr)
        self.assertEqual((crowded / "sketch.txt").read_text(encoding="utf-8"), "put here by hand")
        self.assertEqual((beside / "notes.txt").read_text(encoding="utf-8"), "and these")
        self.assertEqual(newer.read_text(encoding="utf-8"), "a newer save")
        self.assertEqual(self.companions(), {"content/lake.webp": "saved by its author",
                                             "content/pier.webp": "a newer save",
                                             "content/pier.earlier.webp": "an earlier save"})

        self.write("prepared/view.w480.webp", "made elsewhere")
        self.aside("prepared/other.w480.webp", "aside elsewhere")
        done = self.run_declare("prepared/view.w480.webp", "content/lake.jpg")
        self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
        self.assertIn("These are outside the directories a conversion looks into. The thumbnail script puts them back",
                      done.stderr)
        # The directory to name is the photographs', not the one the file is in now.
        self.assertIn("\n    python3 engine/tools/generate-thumbnails.py prepared\n", done.stderr)
        self.assertNotIn("The next conversion puts these back", done.stderr)

    def test_no_image_is_looked_for_in_a_directory_reserved_to_take_a_file_aside(self):
        # One left for its author to look at may hold anything: a drawing
        # is not an image of the site's there, to convert, or to refuse
        # the site for when another of its name is beside it.
        self.write("content/lake.jpg")
        self.convert()
        left = self.aside("content/lake.webp", None)
        for name in ("sketch.jpg", "sketch.png", "drawing.png"):
            (left / name).write_text("put here by hand", encoding="utf-8")
        done = self.run_script()
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertIn("0 converted, 1 already present", done.stdout)
        self.assertEqual(sorted(p.name for p in left.iterdir()),
                         ["drawing.png", "name.tmp", "sketch.jpg", "sketch.png"])
        self.assertEqual(list(self.record()), ["content/lake.webp"])
        done = self.adopt()
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertIn("0 companion(s) recorded", done.stdout)

    def test_a_directory_or_a_device_aside_is_nobody_s_image(self):
        # Only a file (or a link) is ever taken aside. A directory of
        # that name in a reserved directory is put nowhere a page looks
        # for an image, whatever is in it.
        self.write("content/lake.jpg")
        self.write("content/pier.jpg")
        self.convert()
        for name in ("lake", "pier"):
            (self.root / f"content/{name}.webp").unlink()
        folder = self.aside("content/lake.webp", None)
        self.write(f"content/{folder.name}/displaced.tmp/sketch.txt", "put here by hand")
        pipe = self.aside("content/pier.webp", None, tail="efgh5678")
        os.mkfifo(pipe / "displaced.tmp")
        for name in ("lake", "pier"):
            (self.root / f"content/{name}.jpg").unlink()

        done = self.run_script()
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertEqual(done.stderr.count("looks like a file set aside by a run that was stopped"), 2, done.stderr)
        self.assertFalse(os.path.lexists(self.root / "content/lake.webp"))
        self.assertFalse(os.path.lexists(self.root / "content/pier.webp"))
        self.assertEqual((folder / "displaced.tmp/sketch.txt").read_text(encoding="utf-8"), "put here by hand")
        self.assertTrue((pipe / "displaced.tmp").is_fifo())

    def test_a_directory_reached_by_two_roots_is_looked_into_once(self):
        # static/ is a link to a directory under content/: what a stopped
        # run took aside there is put back once, and not looked for again
        # where it no longer is.
        self.write("content/images/lake.jpg")
        (self.root / "static").symlink_to(self.root / "content/images", target_is_directory=True)
        done = self.run_script()
        first = (done.returncode, done.stderr)
        (self.root / "content/images/lake.webp").unlink(missing_ok=True)
        self.aside("content/images/lake.webp", "saved by its author")

        done = self.run_script()
        self.assertEqual(done.stderr.count("was set aside by a run that was stopped, and is back in its place"), 1,
                         done.stderr)
        self.assertEqual(done.returncode, first[0], done.stdout + done.stderr)
        self.assertEqual((self.root / "content/images/lake.webp").read_text(encoding="utf-8"),
                         "saved by its author")
        self.assertEqual(self.asides(), [])

    def test_a_file_an_earlier_form_of_the_tools_took_aside_is_named_and_not_moved(self):
        # One commit of the tools kept the file it took aside as a file of
        # the reserved name. One left by a run of that form may be the
        # only copy of a save: nothing is converted until its author has
        # put it back or deleted it. A file of such a name that says
        # nothing of an output the record has is not that.
        self.write("content/lake.jpg")
        self.write("content/pier.jpg")
        self.convert()
        self.write("content/lake.jpg", "saved again")
        self.write("content/pier.jpg", "saved again")
        lake = self.root / "content/lake.webp"
        lake.unlink()
        key = hashlib.sha256(b"lake.webp").hexdigest()[:16]
        earlier = self.write(f"content/.previous-image-{key}-abcd1234.tmp", "saved by its author")
        unrelated = self.write("content/.previous-image-0123456789abcdef-abcd1234.tmp", "mine")

        for run in (self.run_script, self.adopt):
            done = run()
            self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
            self.assertIn(f"content/{earlier.name}", done.stderr)
            self.assertEqual(earlier.read_text(encoding="utf-8"), "saved by its author")
            self.assertEqual(self.companions(), {"content/pier.webp": made("pier.jpg")})
        done = self.run_script()
        self.assertIn("an earlier form of this tool took a file aside when it was stopped", done.stderr)
        self.assertIn("(from lake.webp, in that directory)", done.stderr)
        self.assertIn("nothing was checked or converted", done.stderr)

        os.replace(earlier, lake)
        done = self.run_script()
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertEqual(self.companions(), {"content/lake.webp": "saved by its author",
                                             "content/pier.webp": made("pier.jpg", "saved again")})
        self.assertEqual(unrelated.read_text(encoding="utf-8"), "mine")

    def test_adoption_brings_a_stale_entry_up_to_a_file_the_script_would_make_now(self):
        # A run stopped after it put the new companion in place and before
        # it saved the record: the entry has the bytes of the companion
        # before. No conversion writes over such a file, which may be one
        # its author changed. An adoption makes it again, and brings the
        # entry up to it if that gives the same bytes. Not one its author
        # changed; not one whose entry is of another image; not a declared one.
        for name in ("lake", "edited", "other", "mine"):
            self.write(f"content/blog/{name}.jpg")
        self.convert()
        path = self.root / "data/derived-images.json"
        document = json.loads(path.read_text(encoding="utf-8"))
        for name in ("lake", "other", "mine"):
            self.write(f"content/blog/{name}.jpg", "saved again")
            self.write(f"content/blog/{name}.webp", made(f"{name}.jpg", "saved again"))
        self.write("content/blog/edited.webp", "changed by its author")
        document["outputs"]["content/blog/other.webp"]["source"] = "content/blog/lake.jpg"
        path.write_text(json.dumps(document), encoding="utf-8")
        self.write("data/companions.json", json.dumps({"content/blog/mine.webp": "content/blog/mine.jpg"}))
        before = (self.companions(), path.read_bytes())

        done = self.run_script()
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertIn("0 converted", done.stdout)
        self.assertIn("content/blog/lake.webp", done.stderr)
        self.assertEqual((self.companions(), self.record()), (before[0], document["outputs"]))

        preview = self.adopt("--dry-run")
        self.assertEqual(preview.returncode, 0, preview.stdout + preview.stderr)
        self.assertEqual((self.companions(), path.read_bytes()), before)
        self.assertEqual(preview.stdout.count("  repair   "), 1)
        self.assertIn("  repair   content/blog/lake.webp  (what cwebp makes of content/blog/lake.jpg now",
                      preview.stdout)
        self.assertIn("their entries would be brought up to them", preview.stdout)
        self.assertIn("1 more have an entry of this script's and other bytes than it says", preview.stdout)

        done = self.adopt()
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertEqual(done.stdout, preview.stdout.replace("would be recorded", "recorded")
                         .replace("would be", "were"))
        self.assertEqual(self.companions(), before[0])
        record = self.record()
        self.assertEqual((record["content/blog/lake.webp"]["digest"], record["content/blog/lake.webp"]["source-digest"]),
                         (self.sha(made("lake.jpg", "saved again")), self.sha("saved again")))
        for name in ("edited", "other", "mine"):
            self.assertEqual(record[f"content/blog/{name}.webp"], document["outputs"][f"content/blog/{name}.webp"])
        # It is the script's own again: shown with its image, and made
        # again when the image is.
        done = self.run_script()
        self.assertNotIn("content/blog/lake.webp", done.stderr)
        self.write("content/blog/lake.jpg", "and again")
        self.convert()
        self.assertEqual(self.companions()["content/blog/lake.webp"], made("lake.jpg", "and again"))

    def test_a_gif_is_shown_with_a_companion_too(self):
        # The image filter gives color.gif the companion color.webp, here
        # one the author made, which this script would make from color.JPG.
        self.write("content/blog/color.gif")
        self.write("content/blog/color.webp", "by hand")
        self.write("content/blog/color.JPG")
        said = self.refused()
        self.assertIn("content/blog/color.JPG and content/blog/color.gif share one WebP companion, "
                      "content/blog/color.webp", said)
        self.assertIn("content/blog/color.webp is there already; it is not one this script made", said)
        self.assertEqual(self.companions(), {"content/blog/color.webp": "by hand"})
        (self.root / "content/blog/color.JPG").rename(self.root / "content/blog/draft.JPG")
        self.assertIn("1 converted, 0 already present", self.convert())
        self.assertEqual(self.companions(), {"content/blog/color.webp": "by hand",
                                             "content/blog/draft.webp": made("draft.JPG")})

    def test_the_site_s_static_image_and_the_engine_s_are_served_as_one_tree(self):
        # The engine's color.JPG and the site's color.png are both shown
        # with /images/color.webp, and the site's file is the one served.
        self.write("engine/static/images/color.JPG")
        self.write("static/images/color.png")
        # A site image that replaces the engine's of its name is one image,
        # and each has its companion beside it.
        self.write("engine/static/images/logo.png")
        self.write("static/images/logo.png")
        # Under content/ the same names are other addresses.
        self.write("content/images/color.jpeg")
        self.write("static/images/color.webp", "someone's")
        for env in ({}, {"PATH": self.bare_path()}):
            with self.subTest(env=env):
                said = self.refused(**env)
                self.assertIn("engine/static/images/color.JPG and static/images/color.png share the WebP "
                              "companion served at /images/color.webp", said)
                self.assertIn("static/images/color.webp is there already; it is not one this script made", said)
                self.assertNotIn("logo", said)
                self.assertNotIn("content/", said)
                self.assertEqual(self.companions(), {"static/images/color.webp": "someone's"})
        (self.root / "static/images/color.png").rename(self.root / "static/images/colour.png")
        (self.root / "static/images/color.webp").unlink()
        self.assertIn("5 converted, 0 already present", self.convert())
        self.assertEqual(self.companions(), {
            "engine/static/images/color.webp": made("color.JPG"), "static/images/colour.webp": made("colour.png"),
            "engine/static/images/logo.webp": made("logo.png"), "static/images/logo.webp": made("logo.png"),
            "content/images/color.webp": made("color.jpeg")})
        self.assertEqual(set(self.record("engine")), {"static/images/color.webp", "static/images/logo.webp"})

    # -- names, and failures -----------------------------------------------

    def test_names_a_shell_reads_twice_are_names(self):
        names = ("a b.png", "it's].jpg", "$(touch pwned).png", "[x]=1.PNG", "two\nlines.jpg", "it's].JPEG",
                 "one\nstem.JPG", "one\nstem.png", "one\nother.png", "tab\there.jpg", 'quote"d.jpg')
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
        self.assertIn("9 converted, 0 already present", self.convert())
        expected = {"content/blog/a b.webp", "content/blog/it's].webp", "content/blog/$(touch pwned).webp",
                    "content/blog/[x]=1.webp", "content/blog/two\nlines.webp", "content/blog/one\nstem.webp",
                    "content/blog/one\nother.webp", "content/blog/tab\there.webp", 'content/blog/quote"d.webp'}
        self.assertEqual(set(self.companions()), expected)
        # The record holds each name as it is, and knows them all again.
        self.assertEqual(set(self.record()), expected)
        self.assertIn("0 converted, 9 already present", self.convert())
        self.assertFalse((self.root / "pwned").exists())

    def test_a_walk_that_fails_or_a_record_that_cannot_be_read_is_a_failure(self):
        self.write("content/blog/map.png")
        # A record that is not what the script writes: nothing says which
        # files are its own, so it touches none.
        for text in ("{", "[]", '{"version": 2, "outputs": {}}', '{"version": 1, "outputs": {"a.webp": "mine"}}'):
            with self.subTest(record=text):
                self.write("data/derived-images.json", text)
                done = self.run_script()
                self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
                self.assertIn("data/derived-images.json", done.stderr)
                self.assertIn("nothing was checked or converted", done.stderr)
                self.assertEqual(self.companions(), {})
        (self.root / "data/derived-images.json").unlink()
        # A walk that reaches only part of the tree is one that failed.
        closed = self.root / "content/closed"
        closed.mkdir()
        closed.chmod(0)
        self.addCleanup(closed.chmod, 0o755)
        if os.access(closed, os.R_OK):
            self.skipTest("this user reads a directory without permission")
        done = self.run_script()
        self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
        self.assertIn("could not list the images", done.stderr)
        self.assertEqual(self.companions(), {})

    def test_a_directory_named_as_an_image_is_passed_over_and_a_link_is_followed(self):
        self.write("content/photography/album.JPG/inside.png")
        (self.root / "content/photography/album.png").mkdir()
        original = self.write("originals/real.jpg")
        (self.root / "content/photography/linked.jpg").symlink_to(original)
        (self.root / "content/photography/dangling.jpg").symlink_to(self.root / "originals/gone.jpg")
        # A WebP that is a link is someone's arrangement, not a file to replace.
        self.write("content/photography/arranged.jpg")
        (self.root / "content/photography/arranged.webp").symlink_to(original)
        said = self.convert()
        self.assertIn("2 converted, 0 already present", said)
        self.assertIn("1 WebP file(s) beside an image are not recorded", said)
        self.assertTrue((self.root / "content/photography/arranged.webp").is_symlink())
        self.assertEqual({k: v for k, v in self.companions().items() if "arranged" not in k},
                         {"content/photography/album.JPG/inside.webp": made("inside.png"),
                          "content/photography/linked.webp": made("linked.jpg")})


if __name__ == "__main__":
    unittest.main()
