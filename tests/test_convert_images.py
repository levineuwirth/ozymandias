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
    written, not counted as left;
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
