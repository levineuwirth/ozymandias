"""The images a blog page shows by a relative path, from beside it.

Whole builds of a small scratch site against this engine:

  · an image under content/blog/ that a published entry or collection
    landing refers to is published where it is, at the section's address,
    with its WebP companion; its dimension sidecar is read for the page
    and not published;
  · an image no published page refers to is withheld, whatever it is
    called and whichever directory it is in: a draft's own, and one no
    page shows; an image a draft and a published page share is public;
  · posts, collections and their entries are where they were;
  · a feed's entry names its images by their whole address;
  · a page is rebuilt when a sidecar of an image it shows changes or
    appears, and no other page is;
  · an image is shown with a WebP of its name, and the WebP published,
    only where the WebP is the image's: the converter's by the ownership
    record, or its author's by declaration (build/DerivedImages.hs); one
    that only has the name is another picture, withheld unless a page
    shows it itself;
  · a page built with a companion is built again when the companion
    stops being its image's (changed by hand) or starts to be (recorded,
    declared), on the next build and without a clean; so is one whose
    image was replaced since the companion was made (it is shown alone
    until the converter has run), and one that shows a rooted image whose
    first companion has just appeared;
  · a watch follows the pairings as they change, with no page edited;
  · under data/pairings/ the build deletes only a state file it wrote,
    and writes nothing through a link;
  · through `make build`: an entry made a draft takes its page and its
    images out of the site, and so does a reference taken out of a page;
    a draft's image that would take the WebP companion of a published
    image stops the build, and one that has taken it is not shown,
    published or written over; a WebP made by hand is never written, and
    is its image's once declared.

    python -m unittest tests.test_blog_images -v
"""

from __future__ import annotations

import base64
import html
import json
import os
import pty
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import time
import unittest
from pathlib import Path

from tests._helpers import (CABAL_PROJECT, engine_path, record_derived, requires_cabal, script_env, site_binary,
                            srcset_urls, stub_bin, write_site_config)

NOTEBOOK = {"dir": "blog", "address": "notebook", "title": "Notebook", "kind": "Note", "dingbat": None}
FEED = {"path": "feed.xml", "title": "Notes", "description": "Notes.", "sections": ["blog"],
        "advertise": True, "limit": 30}
# A one-pixel PNG; the others need only be files.
PNG = base64.b64decode("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=")
# What stands for a draft's pixels, and for a WebP its author made: bytes
# no build of the fixture writes.
DRAFTED = b"RIFF\x1a\x00\x00\x00WEBPVP8 the draft's lake"
AUTHORED = b"RIFF\x1a\x00\x00\x00WEBPVP8 made by its author"
SVG = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 4 4"><path d="M0 0h4v4z"/></svg>\n'


def page(path: Path, front: str, body: str = "Body.") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(f"---\n{front}\n---\n\n{body}\n", encoding="utf-8")


def image(path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(SVG.encode() if path.suffix == ".svg" else PNG)


FIRST = """![The lake](first.lake.png)

![A chart](charts/depth.svg)

![The way there](secret.maps/route.svg)

![Both pages show this](shared.svg)

The [full size](big/Lake.JPG) picture, and two written as HTML, where
a character reference is the character:
<img src="raw.gif" alt="Written as HTML"> <img src="R&amp;D.gif" alt="Research and development">
<img src="raw.gif" srcset="wide.png&#32;2x&#44; wider.png 3x" alt="A reference may write the space or the comma">

![With a WebP that only has its name](alone.png)

![With a WebP its author declared](hand.gif)

## The lake {#lake}

The [lake](#lake) again, the [home page](/index.html) and [elsewhere](https://example.org/a.png).
"""


class Site(unittest.TestCase):
    """A scratch site whose notebook has published pages and drafts."""

    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory(prefix="blog-images-")
        self.addCleanup(tmp.cleanup)
        self.root = root = Path(tmp.name)
        (root / "engine").symlink_to(engine_path(""))
        self.blog = blog = root / "content/blog"
        page(root / "content/index.md", "title: Home\nhome: true", "Home.")
        # A post that shows images every way a page can: beside it under
        # its own name (with a companion and a sidecar), in a directory
        # below, in a directory named as a draft is, by a link, in HTML.
        page(blog / "first.md", "title: First\ndate: 2026-01-02", FIRST)
        for name in ("first.lake.png", "first.lake.webp", "charts/depth.svg", "secret.maps/route.svg",
                     "shared.svg", "big/Lake.JPG", "raw.gif", "R&D.gif",
                     "alone.png", "alone.webp", "hand.gif", "hand.webp", "wide.png", "wider.png"):
            image(blog / name)
        # Whose a WebP beside an image is: the converter's by its record
        # (first.lake.webp, and the draft's below), its author's by
        # declaration (hand.webp), or no one's that the build knows of
        # (alone.webp), which is then another picture and not alone.png's.
        record_derived(root, {"content/blog/first.lake.webp": "content/blog/first.lake.png"})
        (root / "data/companions.json").write_text('{"content/blog/hand.webp": "content/blog/hand.gif"}',
                                                   encoding="utf-8")
        (blog / "first.lake.png.dims.yaml").write_text("width: 640\nheight: 480\n", encoding="utf-8")
        # A post that shows none.
        page(blog / "second.md", "title: Second\ndate: 2026-01-01")
        # A collection: its landing and an entry show one image, and the
        # entry one of its own and one from the directory above.
        page(blog / "trip/index.md", "title: Trip", "![The route](route.jpg)")
        page(blog / "trip/day-one.md", "title: Day one\ndate: 2026-01-03",
             "![The map](day-one.map.svg)\n\n![The route](route.jpg)\n\n![Shared](../shared.svg)")
        for name in ("trip/day-one.map.svg", "trip/route.jpg"):
            image(blog / name)
        # Drafts: a post and a collection's entry. The post shows an image
        # named for it (with a companion and a sidecar), one under another
        # name, and the one the first post shows too.
        page(blog / "secret.md", "title: Secret\ndate: 2026-01-04\ndraft: true",
             "![The plan](secret.plan.png)\n\n![Another](unrelated.svg)\n\n![Shared](shared.svg)")
        for name in ("secret.plan.png", "secret.plan.webp", "unrelated.svg"):
            image(blog / name)
        record_derived(root, {"content/blog/secret.plan.webp": "content/blog/secret.plan.png"})
        (blog / "secret.plan.png.dims.yaml").write_text("width: 10\nheight: 10\n", encoding="utf-8")
        page(blog / "trip/day-two.md", "title: Day two\ndate: 2026-01-06\ndraft: true",
             "![A photograph](day-two.photo.jpg)")
        image(blog / "trip/day-two.photo.jpg")
        # And an image no page shows.
        image(blog / "nobody.svg")
        self.configure(sections=[NOTEBOOK])

    def configure(self, **settings) -> None:
        config = write_site_config(self.root, **{"feeds": [FEED], **settings})
        (self.root / "site.yaml").write_text(json.dumps(config), encoding="utf-8")

    def files(self, under: str = "notebook", site: str = "_site") -> set[str]:
        out = self.root / site
        return {p.relative_to(out).as_posix() for p in (out / under).rglob("*")
                if p.is_file() and p.suffix not in (".br", ".gz")}

    def read(self, path: str, site: str = "_site") -> str:
        return (self.root / site / path).read_text(encoding="utf-8")

    PAGES = {"notebook/index.html", "notebook/first.html", "notebook/second.html",
             "notebook/trip/index.html", "notebook/trip/day-one.html"}
    SHOWN = {"notebook/first.lake.png", "notebook/first.lake.webp", "notebook/charts/depth.svg",
             "notebook/secret.maps/route.svg", "notebook/shared.svg", "notebook/big/Lake.JPG",
             "notebook/raw.gif", "notebook/R&D.gif", "notebook/trip/day-one.map.svg",
             "notebook/trip/route.jpg", "notebook/alone.png", "notebook/hand.gif", "notebook/hand.webp",
             "notebook/wide.png", "notebook/wider.png"}
    PUBLISHED = PAGES | SHOWN
    WITHHELD = {"notebook/secret.html", "notebook/secret.plan.png", "notebook/secret.plan.webp",
                "notebook/unrelated.svg", "notebook/trip/day-two.html", "notebook/trip/day-two.photo.jpg",
                "notebook/nobody.svg", "notebook/alone.webp"}


@requires_cabal
class BlogImages(Site):
    def site(self, command: str, environment: str = "build", stops: bool = False) -> str:
        # One worker: with several, a build of a site this small has failed
        # in Hakyll's metadata cache before any page was compiled.
        done = subprocess.run([str(site_binary()), *command.split()], cwd=self.root,
                              env=script_env(SITE_ENV=environment, SITE_THREADS="1"),
                              capture_output=True, text=True, timeout=240)
        (self.assertNotEqual if stops else self.assertEqual)(
            done.returncode, 0, done.stdout[-3000:] + done.stderr[-3000:])
        return done.stdout + done.stderr

    def test_an_image_a_published_page_shows_is_published_and_no_other(self):
        self.site("rebuild")
        # Posts, the collection and its entry where they were; the images
        # they show; no sidecar, nothing only a draft shows, nothing no
        # page shows.
        self.assertEqual(self.files(), self.PUBLISHED)
        self.assertEqual((self.root / "_site/notebook/first.lake.png").read_bytes(), PNG)
        first = self.read("notebook/first.html")
        self.assertRegex(first, r'<source srcset="first\.lake\.webp" type="image/webp">')
        self.assertRegex(first, r'<img[^>]*src="first\.lake\.png"[^>]*width="640"[^>]*height="480"')
        self.assertRegex(first, r'<img[^>]*src="charts/depth\.svg"')
        # An image is shown with a WebP of its name only where the WebP is
        # its own: the converter's by the record, or declared by hand.
        self.assertIn('<source srcset="hand.webp" type="image/webp">', first)
        self.assertRegex(first, r'<img[^>]*src="alone\.png"')
        self.assertNotIn("alone.webp", first)
        self.assertRegex(self.read("notebook/trip/day-one.html"), r'<img[^>]*src="day-one\.map\.svg"')
        self.assertRegex(self.read("notebook/trip/index.html"), r'<img[^>]*src="route\.jpg"')
        everything = "\n".join(self.read(p) for p in sorted(self.PAGES))
        for name in ("secret.plan", "unrelated", "day-two", "Day two", ">Secret<", "nobody"):
            self.assertNotIn(name, everything)

    def test_the_withheld_images_are_listed_with_the_drafts(self):
        # What the freshness check and the snapshot ask about (tools/unpublished.py).
        done = subprocess.run([str(site_binary()), "list-unpublished"], cwd=self.root, env=script_env(),
                              capture_output=True, text=True, timeout=60)
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(done.stdout.splitlines(), [
            "file content/blog/alone.webp", "file content/blog/nobody.svg", "file content/blog/secret.md",
            "file content/blog/secret.plan.png",
            "file content/blog/secret.plan.webp", "file content/blog/trip/day-two.md",
            "file content/blog/trip/day-two.photo.jpg", "file content/blog/unrelated.svg"])

    def test_a_dev_build_publishes_a_draft_with_its_images(self):
        # A dev build withholds nothing, and writes a site of its own.
        self.site("rebuild", "dev")
        self.assertEqual(self.files(site="_site-dev"), self.PUBLISHED | self.WITHHELD)
        self.assertRegex(self.read("notebook/secret.html", "_site-dev"),
                         r'<img[^>]*src="secret\.plan\.png"[^>]*width="10"[^>]*height="10"')

    def test_the_images_follow_the_section_s_address(self):
        self.configure(sections=[{"dir": "blog", "kind": "Post", "dingbat": None}])
        self.site("rebuild")
        self.assertEqual(self.files("blog"), {p.replace("notebook/", "blog/", 1) for p in self.PUBLISHED})
        self.assertFalse((self.root / "_site/notebook").exists())

    def test_a_feed_names_an_entry_s_images_by_their_whole_address(self):
        with (self.blog / "first.md").open("a", encoding="utf-8") as first:
            first.write('\n<!-- Not shown: <img src="commented.png" alt=""> -->\n')
            # Rooted, though a character reference writes its slash.
            first.write('\nThe <a href="&#47;index.html">home page</a>, by a reference.\n')
            # Rooted too: the URL parser trims a space a reference writes.
            first.write('\nThe <a href="&#32;/index.html">home page</a>, spaced.\n')
            # A comma inside a srcset URL is the URL's.
            first.write('\n<img src="lake.svg" srcset="lake&#44;summer.svg 1x, lake.svg 2x" alt="Seasons">\n')
            # And a data: URL's comma is its own, beside a candidate made whole.
            first.write('\n<img src="one.gif" srcset="data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7 1x, two.gif 2x" alt="Embedded">\n')
        for name in ("lake,summer.svg", "lake.svg", "one.gif", "two.gif"):
            image(self.blog / name)
        self.site("rebuild")
        feed = self.read("feed.xml")
        at = "https://example.org/notebook/"
        # Beside the feed there is no page for a relative URL to start from.
        for url in ("first.lake.png", "charts/depth.svg", "secret.maps/route.svg", "shared.svg", "raw.gif"):
            self.assertRegex(feed, rf'<img[^>]*src="{re.escape(at + url)}"')
        self.assertIn(f'<source srcset="{at}first.lake.webp" type="image/webp">', feed)
        self.assertIn(f'href="{at}big/Lake.JPG"', feed)
        # The collection's entry, from its own directory and the one above.
        self.assertRegex(feed, rf'<img[^>]*src="{re.escape(at)}trip/day-one\.map\.svg"')
        self.assertRegex(feed, rf'<img[^>]*src="{re.escape(at)}shared\.svg"')
        # What was whole, rooted at the site or within the page is as written.
        for kept in ('href="#lake"', 'href="/index.html"', 'href="https://example.org/a.png"',
                     'href="&#47;index.html"', 'href="&#32;/index.html"'):
            self.assertIn(kept, feed)
        # A comment is not markup, and is as written too.
        self.assertIn('<!-- Not shown: <img src="commented.png" alt=""> -->', feed)
        values = re.findall(r'(?:src|srcset|href|poster)="([^"]*)"', re.sub(r"<!--.*?-->", "", feed, flags=re.S))
        self.assertEqual([v for v in values if not re.match(r"[a-z]+:|[/#]", html.unescape(v).strip())], [])
        # Every candidate of a srcset, read whole and parsed as a browser
        # parses it: the fixture writes one's separators as references
        # (wide.png&#32;2x&#44; wider.png 3x), and a comma in a URL.
        for srcset in re.findall(r'srcset="([^"]*)"', feed):
            for url in srcset_urls(srcset):
                self.assertRegex(url, r"^(https?:|/|data:)", srcset)
        seasons = re.search(r'<img[^>]*srcset="([^"]*)"[^>]*alt="Seasons"', feed)
        self.assertIsNotNone(seasons)
        self.assertEqual(srcset_urls(seasons.group(1)), [at + "lake,summer.svg", at + "lake.svg"])
        embedded = re.search(r'<img[^>]*srcset="([^"]*)"[^>]*alt="Embedded"', feed)
        self.assertIsNotNone(embedded)
        self.assertEqual(srcset_urls(embedded.group(1)), ["data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7", at + "two.gif"])
        # The page itself still names them from where it is.
        self.assertRegex(self.read("notebook/first.html"), r'<img[^>]*src="charts/depth\.svg"')

    def updated(self, log: str) -> set[str]:
        return set(re.findall(r"updated (content/blog/\S+)", log))

    def test_a_page_is_rebuilt_for_a_sidecar_of_an_image_it_shows_and_no_other_page_is(self):
        self.site("rebuild")
        # Nothing is out of date on the next build: every file a page
        # depends on is claimed by a rule.
        self.assertEqual(self.updated(self.site("build")), set())
        (self.blog / "first.lake.png.dims.yaml").write_text("width: 800\nheight: 600\n", encoding="utf-8")
        self.assertEqual(self.updated(self.site("build")),
                         {"content/blog/first.md", "content/blog/first.lake.png.dims.yaml"})
        self.assertRegex(self.read("notebook/first.html"), r'width="800"[^>]*height="600"')
        # A sidecar that was not there, of the image the collection's
        # landing and entry show: those two, and not the post with no image.
        (self.blog / "trip/route.jpg.dims.yaml").write_text("width: 30\nheight: 20\n", encoding="utf-8")
        self.assertEqual(self.updated(self.site("build")),
                         {"content/blog/trip/index.md", "content/blog/trip/day-one.md",
                          "content/blog/trip/route.jpg.dims.yaml"})
        self.assertRegex(self.read("notebook/trip/day-one.html"),
                         r'<img[^>]*src="route\.jpg"[^>]*width="30"[^>]*height="20"')
        # A companion that was not there, made and recorded by the converter.
        image(self.blog / "trip/route.webp")
        record_derived(self.root, {"content/blog/trip/route.webp": "content/blog/trip/route.jpg"})
        self.assertEqual(self.updated(self.site("build")),
                         {"content/blog/trip/index.md", "content/blog/trip/day-one.md",
                          "content/blog/trip/route.webp"})
        self.assertEqual(self.updated(self.site("build")), set())
        self.assertEqual(self.files(), self.PUBLISHED | {"notebook/trip/route.webp"})


    def test_a_page_is_rebuilt_when_a_webp_stops_or_starts_being_its_image_s(self):
        # The second post shows an image of the site's static tree, by its
        # path from the root, with a companion the converter made.
        static = self.root / "static/images"
        image(static / "brand.png")
        image(static / "brand.webp")
        record_derived(self.root, {"static/images/brand.webp": "static/images/brand.png"})
        second = self.blog / "second.md"
        second.write_text(second.read_text(encoding="utf-8") + "\n![Brand](/images/brand.png)\n", encoding="utf-8")
        self.site("rebuild")
        self.assertIn('<source srcset="../images/brand.webp" type="image/webp">', self.read("notebook/second.html"))
        self.assertIn('<source srcset="first.lake.webp" type="image/webp">', self.read("notebook/first.html"))
        self.assertEqual(self.updated(self.site("build")), set())
        # Both companions are changed by hand. They are no longer what the
        # record says was written, and the pages built when they were are
        # built again, without a clean.
        (static / "brand.webp").write_bytes(DRAFTED)
        (self.blog / "first.lake.webp").write_bytes(DRAFTED)
        pages = {u for u in self.updated(self.site("build")) if u.endswith(".md")}
        self.assertEqual(pages, {"content/blog/first.md", "content/blog/second.md"})
        self.assertNotIn("brand.webp", self.read("notebook/second.html"))
        self.assertNotIn("first.lake.webp", self.read("notebook/first.html"))
        self.assertEqual(self.updated(self.site("build")), set())
        # The converter makes one again (here: its entry), and the author
        # declares another image's: each page that asked follows.
        record_derived(self.root, {"static/images/brand.webp": "static/images/brand.png"})
        (self.root / "data/companions.json").write_text(json.dumps({
            "content/blog/hand.webp": "content/blog/hand.gif", "content/blog/alone.webp": "content/blog/alone.png"}),
            encoding="utf-8")
        pages = {u for u in self.updated(self.site("build")) if u.endswith(".md")}
        self.assertEqual(pages, {"content/blog/first.md", "content/blog/second.md"})
        self.assertIn('<source srcset="../images/brand.webp" type="image/webp">', self.read("notebook/second.html"))
        self.assertIn('<source srcset="alone.webp" type="image/webp">', self.read("notebook/first.html"))
        self.assertEqual(self.updated(self.site("build")), set())


    def brand(self) -> Path:
        """The second post shows an image of the site's static tree, by
        its path from the root, with a companion the converter made."""
        static = self.root / "static/images"
        image(static / "brand.png")
        image(static / "brand.webp")
        record_derived(self.root, {"static/images/brand.webp": "static/images/brand.png"})
        second = self.blog / "second.md"
        second.write_text(second.read_text(encoding="utf-8") + "\n![Brand](/images/brand.png)\n", encoding="utf-8")
        return static / "brand.png"

    def pages(self, log: str) -> set[str]:
        return {u for u in self.updated(log) if u.endswith(".md")}

    BRAND = '<source srcset="../images/brand.webp" type="image/webp">'

    def test_an_image_replaced_since_its_companion_was_made_is_shown_alone(self):
        # A build with no converter (no cwebp on the machine) after the
        # image was replaced: the companion holds the picture that was
        # there before, and the record says which image it was made of.
        brand = self.brand()
        self.site("rebuild")
        self.assertIn(self.BRAND, self.read("notebook/second.html"))
        brand.write_bytes(PNG + b"another picture")
        self.assertEqual(self.pages(self.site("build")), {"content/blog/second.md"})
        self.assertNotIn("brand.webp", self.read("notebook/second.html"))
        self.site("rebuild")
        self.assertNotIn("brand.webp", self.read("notebook/second.html"))
        # Converted again (here: its entry), it is the image's again.
        record_derived(self.root, {"static/images/brand.webp": "static/images/brand.png"})
        self.assertEqual(self.pages(self.site("build")), {"content/blog/second.md"})
        self.assertIn(self.BRAND, self.read("notebook/second.html"))
        # Another time and the same bytes (a checkout): the same image.
        os.utime(brand, (1_000_000_000,) * 2)
        self.assertEqual(self.pages(self.site("build")), set())
        self.assertIn(self.BRAND, self.read("notebook/second.html"))
        # An entry that notes the image's time and size alone holds while
        # they do, and one that says nothing of its image pairs nothing.
        record = self.root / "data/derived-images.json"
        document = json.loads(record.read_text(encoding="utf-8"))
        entry = document["outputs"]["static/images/brand.webp"]
        del entry["source-digest"]
        entry["source-stamp"] = [brand.stat().st_mtime_ns, brand.stat().st_size]
        record.write_text(json.dumps(document), encoding="utf-8")
        self.assertEqual(self.pages(self.site("build")), set())
        self.assertIn(self.BRAND, self.read("notebook/second.html"))
        os.utime(brand, (1_100_000_000,) * 2)
        self.assertEqual(self.pages(self.site("build")), {"content/blog/second.md"})
        self.assertNotIn("brand.webp", self.read("notebook/second.html"))
        del entry["source-stamp"]
        record.write_text(json.dumps(document), encoding="utf-8")
        self.site("rebuild")
        self.assertNotIn("brand.webp", self.read("notebook/second.html"))

    def test_a_rooted_image_takes_its_first_companion_on_the_next_build(self):
        # An image under static/, in a directory where nothing is paired
        # yet, shown by its path from the root. Its author makes it a
        # companion and declares it: the page that asked is built again.
        image(self.root / "static/newdir/blue.gif")
        second = self.blog / "second.md"
        second.write_text(second.read_text(encoding="utf-8") + "\n![Blue](/newdir/blue.gif)\n", encoding="utf-8")
        self.site("rebuild")
        self.assertNotIn("blue.webp", self.read("notebook/second.html"))
        self.assertEqual(self.updated(self.site("build")), set())
        (self.root / "static/newdir/blue.webp").write_bytes(AUTHORED)
        declared = json.loads((self.root / "data/companions.json").read_text(encoding="utf-8"))
        (self.root / "data/companions.json").write_text(
            json.dumps({**declared, "./static/newdir/blue.webp": "static/newdir/blue.gif"}), encoding="utf-8")
        self.assertEqual(self.pages(self.site("build")), {"content/blog/second.md"})
        self.assertIn('<source srcset="../newdir/blue.webp" type="image/webp">', self.read("notebook/second.html"))
        self.assertEqual(self.updated(self.site("build")), set())

    def test_only_a_state_file_the_build_wrote_is_ever_written_over_or_deleted(self):
        # Files of the author's under data/pairings/: beside the state
        # files, where one goes, named as a temporary file might be, in
        # the place of another layout, and behind a link to elsewhere.
        pairings = self.root / "data/pairings"
        outside = self.root / "elsewhere"
        for directory in (pairings / "states/content", pairings / "content", outside / "images"):
            directory.mkdir(parents=True)
        state = pairings / "states/content/blog.json"
        mine = {pairings / "notes.md": "notes", pairings / "states/notes.md": "more",
                pairings / "states/content/mine.json": "[]", pairings / "content/blog.json": "[]",
                state: "mine, where the notebook's state goes", state.with_name("blog.json.tmp"): "mine too",
                state.with_name(".blog.json.tmp"): "and this",
                outside / "kept.json": "[]", outside / "images/deep.json": "[]"}
        for path, text in mine.items():
            path.write_text(text, encoding="utf-8")
        (pairings / "states/linked").symlink_to(outside)
        listing = pairings / "manifest.json"

        def manifest() -> set[str]:
            # The site's own: the engine's images have their states too.
            return {name for name in json.loads(listing.read_text(encoding="utf-8"))
                    if not name.startswith("data/pairings/states/engine/")}

        def intact() -> None:
            for path, text in mine.items():
                self.assertEqual(path.read_text(encoding="utf-8"), text, path)
            self.assertTrue((self.root / "data/companions.json").is_file())
            self.assertEqual(sorted(p.name for p in outside.rglob("*") if p.is_file()), ["deep.json", "kept.json"])
            self.assertFalse((pairings / "manifest.pending.json").exists())
            # No temporary file of the build's is left beside the author's.
            self.assertEqual([p.name for p in pairings.rglob("*.tmp") if p.name not in ("blog.json.tmp",
                                                                                          ".blog.json.tmp")], [])

        # A file where a state file goes, which no build wrote: the build
        # stops and names it. Written over, it would be lost; built
        # around, the notebook's pages would not follow their pairings.
        log = self.site("build", stops=True)
        self.assertIn("data/pairings/states/content/blog.json: not a file this build wrote", log)
        intact()
        self.assertFalse((self.root / "_site/notebook/first.html").exists())
        state.unlink()
        del mine[state]
        self.site("rebuild")
        intact()
        self.assertEqual(manifest(), {"data/pairings/states/content/blog.json"})
        self.assertIn("content/blog/first.lake.webp", state.read_text(encoding="utf-8"))

        # A manifest that is not the build's (it names what is no state
        # file) says nothing of what the build may delete.
        written = listing.read_text(encoding="utf-8")
        for other in ({"data/pairings/notes.md": "sha256:0"}, {"data/companions.json": "sha256:0"},
                      {"data/pairings/states/../../companions.json": "sha256:0"}, ["a list"], "not JSON {"):
            with self.subTest(manifest=other):
                listing.write_text(other if isinstance(other, str) else json.dumps(other), encoding="utf-8")
                self.assertIn("manifest.json: not the build's list of its state files", self.site("build", stops=True))
                intact()
        listing.write_text(written, encoding="utf-8")

        # The state file edited since it was written is the author's by
        # now. Nothing is paired in the notebook any more, and its state
        # file would go: it stays, and the build stops.
        built = state.read_text(encoding="utf-8")
        state.write_text(built + "\n", encoding="utf-8")
        record = (self.root / "data/derived-images.json").read_text(encoding="utf-8")
        declared = (self.root / "data/companions.json").read_text(encoding="utf-8")
        (self.root / "data/derived-images.json").unlink()
        (self.root / "data/companions.json").write_text("{}", encoding="utf-8")
        self.assertIn("blog.json: not a file this build wrote, or changed since", self.site("build", stops=True))
        self.assertEqual(state.read_text(encoding="utf-8"), built + "\n")
        intact()
        # As the build wrote it, it is the build's to delete.
        state.write_text(built, encoding="utf-8")
        self.site("build")
        self.assertFalse(state.exists())
        self.assertEqual(manifest(), set())
        intact()

        # A build killed after it wrote a state file and before it wrote
        # the manifest: its note of what it was writing says the file is
        # its own. Without the note, the file is no one's it knows of.
        (self.root / "data/derived-images.json").write_text(record, encoding="utf-8")
        (self.root / "data/companions.json").write_text(declared, encoding="utf-8")
        self.site("build")
        after = listing.read_text(encoding="utf-8")
        name = "data/pairings/states/content/blog.json"
        listed = json.loads(after)
        digest = listed.pop(name)
        listing.write_text(json.dumps(listed), encoding="utf-8")
        self.site("build", stops=True)
        (pairings / "manifest.pending.json").write_text(json.dumps({name: ["sha256:0", digest]}), encoding="utf-8")
        self.site("build")
        self.assertEqual(json.loads(listing.read_text(encoding="utf-8")), json.loads(after))
        intact()

        # A link on the way to where a state file goes: nothing is written
        # through it, and the build stops.
        (pairings / "states/static").symlink_to(outside)
        self.brand()
        self.assertIn("a link is in the way", self.site("build", stops=True))
        intact()
        (pairings / "states/static").unlink()
        self.site("build")
        self.assertIn(self.BRAND, self.read("notebook/second.html"))
        # And data/pairings itself a link, or data/: a page would keep the
        # pairing it was built with (here: a companion changed by hand).
        (self.root / "static/images/brand.webp").write_bytes(DRAFTED)
        moved = self.root / "pairings-elsewhere"
        pairings.rename(moved)
        pairings.symlink_to(moved)
        self.assertIn("data/pairings: it, or a directory on the way to it", self.site("build", stops=True))
        pairings.unlink()
        moved.rename(pairings)
        (self.root / "data").rename(self.root / "data-elsewhere")
        (self.root / "data").symlink_to("data-elsewhere")
        self.assertIn("data/pairings: it, or a directory on the way to it", self.site("build", stops=True))
        (self.root / "data").unlink()
        (self.root / "data-elsewhere").rename(self.root / "data")
        self.assertIn(self.BRAND, self.read("notebook/second.html"))
        self.site("build")
        self.assertNotIn("brand.webp", self.read("notebook/second.html"))
        intact()

    def test_a_build_asked_for_with_a_flag_before_the_command_follows_the_pairings_too(self):
        # `site --verbose build` is a build: the companion changed by hand
        # since the last one is no longer shown.
        brand = self.brand()
        self.site("rebuild")
        self.assertIn(self.BRAND, self.read("notebook/second.html"))
        brand.with_suffix(".webp").write_bytes(DRAFTED)
        self.site("--verbose build")
        self.assertNotIn("brand.webp", self.read("notebook/second.html"))
        brand.with_suffix(".webp").write_bytes(AUTHORED)
        record_derived(self.root, {"static/images/brand.webp": "static/images/brand.png"})
        self.site("-v build")
        self.assertIn(self.BRAND, self.read("notebook/second.html"))

    def test_a_declaration_through_a_link_to_a_directory_is_the_file_it_leads_to(self):
        # The tools read the line as the file it leads to, and write no
        # companion there (tests/test_convert_images.py). The page shows
        # the image with it, as it would by the plain path.
        image(self.root / "static/newdir/blue.gif")
        (self.root / "static/newdir/blue.webp").write_bytes(AUTHORED)
        (self.root / "shortcut").symlink_to("static/newdir")
        second = self.blog / "second.md"
        second.write_text(second.read_text(encoding="utf-8") + "\n![Blue](/newdir/blue.gif)\n", encoding="utf-8")
        declared = json.loads((self.root / "data/companions.json").read_text(encoding="utf-8"))
        (self.root / "data/companions.json").write_text(
            json.dumps({**declared, "shortcut/blue.webp": "shortcut/blue.gif"}), encoding="utf-8")
        self.site("rebuild")
        self.assertIn('<source srcset="../newdir/blue.webp" type="image/webp">', self.read("notebook/second.html"))
        self.assertIn('<source srcset="hand.webp" type="image/webp">', self.read("notebook/first.html"))

    def test_idle_watch_exits_when_an_existing_state_is_edited(self):
        self.site("build")
        stamp = self.root / "data/build-stamp.txt"
        original = stamp.read_bytes()
        watch = subprocess.Popen([str(site_binary()), "watch", "--no-server"], cwd=self.root,
                                 stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True,
                                 env=script_env(SITE_ENV="build", SITE_THREADS="1"))
        try:
            # Wait until the watch has finished its startup refresh.
            deadline = time.monotonic() + 20
            while stamp.read_bytes() == original and time.monotonic() < deadline:
                time.sleep(0.05)
            self.assertNotEqual(stamp.read_bytes(), original)
            state = self.root / "data/pairings/states/content/blog.json"
            state.write_text("local edit")
            _, errors = watch.communicate(timeout=18)
            self.assertNotEqual(watch.returncode, 0)
            self.assertIn("blog.json: not a file this build wrote", errors)
            self.assertEqual(state.read_text(), "local edit")
        finally:
            if watch.poll() is None:
                watch.terminate()
            watch.communicate(timeout=10)

    def test_a_watch_follows_the_pairings_as_they_change(self):
        # second.md shows a rooted image with no companion yet; first.md
        # shows hand.gif with the WebP its author declared.
        image(self.root / "static/newdir/blue.gif")
        second = self.blog / "second.md"
        second.write_text(second.read_text(encoding="utf-8") + "\n![Blue](/newdir/blue.gif)\n", encoding="utf-8")
        # On a terminal, where the generator says each line as it has it:
        # into a file its "Success" would wait in a buffer.
        master, slave = pty.openpty()
        watch = subprocess.Popen([str(site_binary()), "watch", "--no-server"], cwd=self.root, stdout=slave,
                                 stderr=slave, stdin=subprocess.DEVNULL,
                                 env=script_env(SITE_ENV="build", SITE_THREADS="1"))
        os.close(slave)
        said: list[str] = []

        def listen() -> None:
            try:
                while chunk := os.read(master, 65536):
                    said.append(chunk.decode("utf-8", "replace"))
            except OSError:
                pass
        listener = threading.Thread(target=listen, daemon=True)
        listener.start()

        def stop() -> None:
            watch.terminate()
            try:
                watch.wait(timeout=20)
            except subprocess.TimeoutExpired:
                watch.kill()
                watch.wait()
            listener.join(timeout=5)
            os.close(master)
        self.addCleanup(stop)

        def until(holds, what: str, seconds: float = 120) -> None:
            deadline = time.monotonic() + seconds
            while time.monotonic() < deadline:
                self.assertIsNone(watch.poll(), "".join(said)[-3000:])
                try:
                    if holds():
                        return
                except FileNotFoundError:
                    pass
                time.sleep(0.2)
            self.fail(f"the watch never showed {what}:\n" + "".join(said)[-3000:])

        hand = '<source srcset="hand.webp" type="image/webp">'
        until(lambda: "Success" in "".join(said), "a first build", 300)
        self.assertIn(hand, self.read("notebook/first.html"))
        time.sleep(1)       # the watch is set up when the first build is done
        # The declaration is withdrawn, and no page is edited.
        declared = self.root / "data/companions.json"
        was = declared.read_text(encoding="utf-8")
        declared.write_text("{}", encoding="utf-8")
        until(lambda: hand not in self.read("notebook/first.html"), "first.html without the withdrawn WebP")
        # Made again, and a first companion declared in a directory where
        # nothing was paired (so no state file, and no directory for one).
        (self.root / "static/newdir/blue.webp").write_bytes(AUTHORED)
        declared.write_text(json.dumps({**json.loads(was), "static/newdir/blue.webp": "static/newdir/blue.gif"}),
                            encoding="utf-8")
        until(lambda: hand in self.read("notebook/first.html"), "first.html with the WebP declared again")
        until(lambda: "../newdir/blue.webp" in self.read("notebook/second.html"),
              "second.html with its first companion")
        # A file of the author's where the next state file goes: the watch
        # cannot keep its pages to their pairings, and ends, saying why.
        blocked = self.root / "data/pairings/states/static/other.json"
        blocked.write_text("mine", encoding="utf-8")
        image(self.root / "static/other/red.gif")
        (self.root / "static/other/red.webp").write_bytes(AUTHORED)
        declared.write_text(json.dumps({**json.loads(declared.read_text(encoding="utf-8")),
                                        "static/other/red.webp": "static/other/red.gif"}), encoding="utf-8")
        try:
            code = watch.wait(timeout=60)
        except subprocess.TimeoutExpired:
            self.fail("the watch went on:\n" + "".join(said)[-3000:])
        listener.join(timeout=5)
        self.assertNotEqual(code, 0)
        self.assertIn("other.json: not a file this build wrote", "".join(said).replace("\r", ""))
        self.assertEqual(blocked.read_text(encoding="utf-8"), "mine")


CABAL = """#!/bin/sh
# The site's generator, as this test run built it: `make build` asks cabal for it.
case "$1" in
    run)      shift; shift; shift; exec "{binary}" "$@" ;;
    list-bin) echo "{binary}" ;;
    build)    : ;;
    *)        echo "cabal stub: $*" >&2; exit 2 ;;
esac
"""


@requires_cabal
class ThroughMake(Site):
    """`make build`, which decides between an incremental build and a full
    one from what the generator says is withheld (tools/build-freshness.sh,
    tools/unpublished.py)."""

    def setUp(self) -> None:
        super().setUp()
        root = self.root
        (root / "Makefile").write_text("include engine/engine.mk\n", encoding="utf-8")
        # The project the generator was compiled in: it runs only where
        # the project settings are the ones it was built with.
        for name in ("cabal.project", "cabal.project.local", "cabal.project.freeze"):
            if (CABAL_PROJECT / name).is_file():
                shutil.copy(CABAL_PROJECT / name, root / name)
        (root / ".gitignore").write_text("/engine\n/_site/\n/_cache/\n/data/\n/bin/\n", encoding="utf-8")
        self.stubs = stub_bin(root / "bin", ["cabal"], CABAL.format(binary=site_binary()))
        self.configure(sections=[NOTEBOOK], switches={
            "authors": False, "new": False, "popups": False, "reading-progress": False, "tag-pages": False})
        for command in (["init", "-q", "-b", "main"], ["add", "-A"],
                        ["-c", "user.name=Test", "-c", "user.email=test@example.invalid", "commit", "-qm", "site"]):
            subprocess.run(["git", *command], cwd=root, check=True, capture_output=True)

    def make(self) -> subprocess.CompletedProcess[str]:
        return subprocess.run(["make", "--no-print-directory", f"PY={sys.executable}", "build"], cwd=self.root,
                              env=script_env(self.stubs, SKIP_SNAPSHOT="1", ENGINE_DEV="1", SITE_THREADS="1"),
                              capture_output=True, text=True, timeout=600)

    def make_build(self) -> str:
        done = self.make()
        self.assertEqual(done.returncode, 0, done.stdout[-4000:] + done.stderr[-4000:])
        return done.stdout + done.stderr

    def expected(self, published: set[str]) -> set[str]:
        """`published`, with the companions the build made, and recorded,
        where cwebp is installed (tools/convert-images.sh)."""
        recorded = json.loads((self.root / "data/derived-images.json").read_text(encoding="utf-8"))["outputs"]
        made = {p.rsplit(".", 1)[0] + ".webp" for p in published
                if p.rsplit(".", 1)[-1].lower() in ("jpg", "jpeg", "png")}
        return published | {p for p in made if "content/" + p.replace("notebook/", "blog/", 1) in recorded}

    def test_an_entry_made_a_draft_takes_its_page_and_images_out_of_the_site(self):
        self.make_build()
        self.assertEqual(self.files(), self.expected(self.PUBLISHED))
        self.assertIn("incremental build OK", self.make_build())
        # The first post becomes a draft. Its page goes, and every image
        # only it showed, companion included; the image the collection's
        # entry shows too stays.
        first = self.blog / "first.md"
        first.write_text(first.read_text(encoding="utf-8").replace("title: First\n", "title: First\ndraft: true\n"),
                         encoding="utf-8")
        log = self.make_build()
        self.assertIn("full rebuild required", log)
        self.assertIn("draft flag changed", log)
        self.assertEqual(self.files(), self.expected({
            "notebook/index.html", "notebook/second.html", "notebook/trip/index.html", "notebook/trip/day-one.html",
            "notebook/trip/day-one.map.svg", "notebook/trip/route.jpg", "notebook/shared.svg"}))
        self.assertNotIn("first.lake", self.read("feed.xml"))
        self.assertIn("incremental build OK", self.make_build())

    def test_a_reference_taken_out_of_a_page_takes_the_image_out_of_the_site(self):
        self.make_build()
        # No flag changes: the collection's entry stops showing its map.
        entry = self.blog / "trip/day-one.md"
        entry.write_text(entry.read_text(encoding="utf-8").replace("![The map](day-one.map.svg)", "No map."),
                         encoding="utf-8")
        self.assertIn("full rebuild required", self.make_build())
        self.assertEqual(self.files(), self.expected(self.PUBLISHED - {"notebook/trip/day-one.map.svg"}))

    def refused(self) -> str:
        done = self.make()
        self.assertNotEqual(done.returncode, 0, done.stdout[-4000:])
        return done.stderr

    def served(self) -> bytes:
        return b"".join(p.read_bytes() for p in sorted((self.root / "_site").rglob("*")) if p.is_file())

    def test_a_draft_s_image_cannot_take_a_published_image_s_companion(self):
        self.make_build()
        companion = self.blog / "first.lake.webp"
        # The draft shows first.lake.JPG, newer than the published post's
        # first.lake.png, and a converter that did not check has already
        # made the companion they share from it.
        secret = self.blog / "secret.md"
        secret.write_text(secret.read_text(encoding="utf-8") + "\n![The lake, drafted](first.lake.JPG)\n",
                          encoding="utf-8")
        image(self.blog / "first.lake.JPG")
        companion.write_bytes(DRAFTED)
        # The build stops, says which two, and which file is in question.
        # It takes nothing away.
        said = self.refused()
        self.assertIn("content/blog/first.lake.JPG and content/blog/first.lake.png share one WebP companion, "
                      "content/blog/first.lake.webp", said)
        self.assertIn("content/blog/first.lake.webp is there already; it is not one this script made", said)
        self.assertEqual(companion.read_bytes(), DRAFTED)
        # The author renames the draft's image, and no more. The WebP is
        # not what the record says the converter wrote: it is not written
        # over, the published post is shown without it, and it is not
        # published. The draft's pixels are nowhere in the site.
        (self.blog / "first.lake.JPG").rename(self.blog / "secret.lake.JPG")
        secret.write_text(secret.read_text(encoding="utf-8").replace("first.lake.JPG", "secret.lake.JPG"),
                          encoding="utf-8")
        self.make_build()
        self.assertEqual(companion.read_bytes(), DRAFTED)
        self.assertEqual(self.files(), self.expected(self.PUBLISHED) - {"notebook/first.lake.webp"})
        self.assertNotIn("first.lake.webp", self.read("notebook/first.html"))
        self.assertNotIn(DRAFTED, self.served())
        # Deleted, it is made again from the image that kept the name
        # (where there is a converter), and shown with it.
        companion.unlink()
        self.make_build()
        self.assertEqual(companion.exists(), shutil.which("cwebp") is not None)
        self.assertEqual("first.lake.webp" in self.read("notebook/first.html"), companion.exists())
        self.assertNotIn(DRAFTED, self.served())

    def test_a_retargeted_static_directory_changes_the_pages_pairing(self):
        for name in ("red", "blue"):
            for file in ("map.gif", "map.webp"):
                image(self.root / "static" / name / file)
        declared = self.root / "data/companions.json"
        pairs = json.loads(declared.read_text())
        pairs["static/red/map.webp"] = "static/red/map.gif"
        declared.write_text(json.dumps(pairs))
        alias = self.root / "static/album"
        alias.symlink_to("red")
        page = self.blog / "second.md"
        page.write_text(page.read_text() + "\n![Map](/album/map.gif)\n")
        self.make_build()
        self.assertIn("album/map.webp", self.read("notebook/second.html"))
        alias.unlink()
        alias.symlink_to("blue")
        self.make_build()
        self.assertNotIn("album/map.webp", self.read("notebook/second.html"))
        alias.unlink()
        alias.symlink_to("red")
        self.make_build()
        self.assertIn("album/map.webp", self.read("notebook/second.html"))

    def test_a_companion_changed_by_hand_leaves_the_page_on_the_next_build(self):
        # A published post shows a static image by its path from the root,
        # with the companion the converter made of it.
        static = self.root / "static/images"
        image(static / "brand.png")
        image(static / "brand.webp")
        record_derived(self.root, {"static/images/brand.webp": "static/images/brand.png"})
        second = self.blog / "second.md"
        second.write_text(second.read_text(encoding="utf-8") + "\n![Brand](/images/brand.png)\n", encoding="utf-8")
        for command in (["add", "-A"], ["-c", "user.name=Test", "-c", "user.email=test@example.invalid",
                                        "commit", "-qm", "brand"]):
            subprocess.run(["git", *command], cwd=self.root, check=True, capture_output=True)
        self.make_build()
        self.assertIn('<source srcset="../images/brand.webp" type="image/webp">', self.read("notebook/second.html"))
        # The author replaces the companion with another picture. The
        # converter leaves it; the build is an incremental one, and the
        # page that was built with it is built again without it.
        (static / "brand.webp").write_bytes(DRAFTED)
        log = self.make_build()
        self.assertIn("incremental build OK", log)
        self.assertEqual((static / "brand.webp").read_bytes(), DRAFTED)
        self.assertNotIn("brand.webp", self.read("notebook/second.html"))
        self.assertRegex(self.read("notebook/second.html"), r'<img[^>]*src="\.\./images/brand\.png"')

    def test_a_webp_made_by_hand_is_its_image_s_once_declared_and_is_never_written(self):
        # A published post shows moving.gif, and its author made a
        # moving.webp for it. Undeclared, that is a picture of its own.
        second = self.blog / "second.md"
        second.write_text(second.read_text(encoding="utf-8") + "\n![Moving](moving.gif)\n", encoding="utf-8")
        image(self.blog / "moving.gif")
        (self.blog / "moving.webp").write_bytes(AUTHORED)
        self.make_build()
        self.assertNotIn("moving.webp", self.read("notebook/second.html"))
        self.assertEqual(self.files(), self.expected(self.PUBLISHED | {"notebook/moving.gif"}))
        # A draft's moving.JPG would be converted to that name: refused,
        # and the author's file is as it was.
        secret = self.blog / "secret.md"
        secret.write_text(secret.read_text(encoding="utf-8") + "\n![Still](moving.JPG)\n", encoding="utf-8")
        image(self.blog / "moving.JPG")
        self.assertIn("content/blog/moving.JPG and content/blog/moving.gif share one WebP companion, "
                      "content/blog/moving.webp", self.refused())
        self.assertEqual((self.blog / "moving.webp").read_bytes(), AUTHORED)
        (self.blog / "moving.JPG").rename(self.blog / "still.JPG")
        secret.write_text(secret.read_text(encoding="utf-8").replace("moving.JPG", "still.JPG"), encoding="utf-8")
        # Declared, it is the GIF's companion: shown with it, published,
        # and still the author's bytes.
        done = subprocess.run([sys.executable, str(engine_path("tools/derived_images.py")), "declare",
                               str(self.blog / "moving.webp")], cwd=self.root,
                              env=script_env(SITE_ROOT=str(self.root)), capture_output=True, text=True)
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertIn("full rebuild required", self.make_build())
        self.assertEqual(self.files(), self.expected(self.PUBLISHED | {"notebook/moving.gif", "notebook/moving.webp"}))
        self.assertEqual((self.root / "_site/notebook/moving.webp").read_bytes(), AUTHORED)
        self.assertEqual((self.blog / "moving.webp").read_bytes(), AUTHORED)
        self.assertIn('<source srcset="moving.webp" type="image/webp">', self.read("notebook/second.html"))

    def test_a_draft_s_image_is_not_converted_over_a_webp_a_published_page_shows(self):
        # A published post shows a WebP its author made, by name.
        second = self.blog / "second.md"
        second.write_text(second.read_text(encoding="utf-8") + "\n![Color](color.webp)\n", encoding="utf-8")
        (self.blog / "color.webp").write_bytes(AUTHORED)
        self.make_build()
        self.assertEqual((self.root / "_site/notebook/color.webp").read_bytes(), AUTHORED)
        # A draft shows a newer color.JPG. The converter makes a companion
        # where there is none or where its own is; this is neither.
        secret = self.blog / "secret.md"
        secret.write_text(secret.read_text(encoding="utf-8") + "\n![Color, drafted](color.JPG)\n", encoding="utf-8")
        image(self.blog / "color.JPG")
        log = self.make_build()
        self.assertIn("not recorded as made from it by this script, and are left as they are", log)
        self.assertEqual((self.blog / "color.webp").read_bytes(), AUTHORED)
        self.assertEqual((self.root / "_site/notebook/color.webp").read_bytes(), AUTHORED)
        self.assertEqual(self.files(), self.expected(self.PUBLISHED | {"notebook/color.webp"}))


if __name__ == "__main__":
    unittest.main()
