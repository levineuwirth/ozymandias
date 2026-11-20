"""A blog entry's images, shown by a relative path from beside it.

Whole builds of a small scratch site against this engine:

  · an image under content/blog/ is published where it is, at the
    section's address, with its WebP companion; its dimension sidecar is
    read for the page and not published;
  · posts, collections and their entries are where they were;
  · a draft entry's images, the ones named for it, are withheld with it
    in a production build and published with it in a dev build;
  · a page is rebuilt when an image's sidecar changes, and not otherwise.

    python -m unittest tests.test_blog_images -v
"""

from __future__ import annotations

import base64
import json
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import engine_path, requires_cabal, script_env, site_binary, write_site_config

NOTEBOOK = {"dir": "blog", "address": "notebook", "title": "Notebook", "kind": "Note", "dingbat": None}
# A one-pixel PNG; the others need only be files.
PNG = base64.b64decode("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=")
SVG = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 4 4"><path d="M0 0h4v4z"/></svg>\n'


def page(path: Path, front: str, body: str = "Body.") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(f"---\n{front}\n---\n\n{body}\n", encoding="utf-8")


def image(path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(SVG.encode() if path.suffix == ".svg" else PNG)


@requires_cabal
class BlogImages(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory(prefix="blog-images-")
        self.addCleanup(tmp.cleanup)
        self.root = root = Path(tmp.name)
        (root / "engine").symlink_to(engine_path(""))
        blog = root / "content/blog"
        page(root / "content/index.md", "title: Home\nhome: true", "Home.")
        # A post with an image named for it (with a companion and a
        # sidecar), and one under another name in a directory below.
        page(blog / "first.md", "title: First\ndate: 2026-01-02",
             "![The lake](first.lake.png)\n\n![A chart](charts/depth.svg)")
        for name in ("first.lake.png", "first.lake.webp", "charts/depth.svg"):
            image(blog / name)
        (blog / "first.lake.png.dims.yaml").write_text("width: 640\nheight: 480\n", encoding="utf-8")
        # A collection, an entry with its image, an image its entries share.
        page(blog / "trip/index.md", "title: Trip", "![The route](route.jpg)")
        page(blog / "trip/day-one.md", "title: Day one\ndate: 2026-01-03",
             "![The map](day-one.map.svg)\n\n![The route](route.jpg)")
        for name in ("trip/day-one.map.svg", "trip/route.jpg"):
            image(blog / name)
        # Drafts: a post and a collection's entry, each with images named
        # for it, a companion and a sidecar. A published post whose name
        # begins as the draft's does is not one of its images.
        page(blog / "secret.md", "title: Secret\ndate: 2026-01-04\ndraft: true",
             "![The plan](secret.plan.png)\n\n![](secret.png)")
        for name in ("secret.plan.png", "secret.plan.webp", "secret.png", "secret.gif"):
            image(blog / name)
        (blog / "secret.plan.png.dims.yaml").write_text("width: 10\nheight: 10\n", encoding="utf-8")
        page(blog / "secret.2.md", "title: Not secret\ndate: 2026-01-05")
        page(blog / "trip/day-two.md", "title: Day two\ndate: 2026-01-06\ndraft: true",
             "![](day-two.photo.jpg)")
        image(blog / "trip/day-two.photo.jpg")
        self.configure(sections=[NOTEBOOK])

    def configure(self, **settings) -> None:
        config = write_site_config(self.root, feeds=[], **settings)
        (self.root / "site.yaml").write_text(json.dumps(config), encoding="utf-8")

    def site(self, command: str, environment: str = "build") -> str:
        # One worker: with several, a build of a site this small has failed
        # in Hakyll's metadata cache before any page was compiled.
        done = subprocess.run([str(site_binary()), command], cwd=self.root,
                              env=script_env(SITE_ENV=environment, SITE_THREADS="1"),
                              capture_output=True, text=True, timeout=240)
        self.assertEqual(done.returncode, 0, done.stdout[-3000:] + done.stderr[-3000:])
        return done.stdout + done.stderr

    def files(self, under: str = "notebook", site: str = "_site") -> set[str]:
        out = self.root / site
        return {p.relative_to(out).as_posix() for p in (out / under).rglob("*") if p.is_file()}

    def read(self, path: str, site: str = "_site") -> str:
        return (self.root / site / path).read_text(encoding="utf-8")

    PUBLISHED = {"notebook/index.html", "notebook/first.html", "notebook/secret.2.html",
                 "notebook/trip/index.html", "notebook/trip/day-one.html",
                 "notebook/first.lake.png", "notebook/first.lake.webp", "notebook/charts/depth.svg",
                 "notebook/trip/day-one.map.svg", "notebook/trip/route.jpg"}
    DRAFTS = {"notebook/secret.html", "notebook/secret.plan.png", "notebook/secret.plan.webp",
              "notebook/secret.png", "notebook/secret.gif", "notebook/trip/day-two.html",
              "notebook/trip/day-two.photo.jpg"}

    def test_an_entry_s_images_are_published_beside_it_and_a_draft_s_are_withheld(self):
        self.site("rebuild")
        # Posts, the collection and its entry where they were; the images
        # beside them; no sidecar, and nothing of either draft.
        self.assertEqual(self.files(), self.PUBLISHED)
        self.assertEqual((self.root / "_site/notebook/first.lake.png").read_bytes(), PNG)
        first = self.read("notebook/first.html")
        self.assertRegex(first, r'<source srcset="first\.lake\.webp" type="image/webp">')
        self.assertRegex(first, r'<img[^>]*src="first\.lake\.png"[^>]*width="640"[^>]*height="480"')
        self.assertRegex(first, r'<img[^>]*src="charts/depth\.svg"')
        self.assertRegex(self.read("notebook/trip/day-one.html"), r'<img[^>]*src="day-one\.map\.svg"')
        self.assertRegex(self.read("notebook/trip/index.html"), r'<img[^>]*src="route\.jpg"')
        everything = "\n".join(self.read(p) for p in sorted(self.PUBLISHED) if p.endswith(".html"))
        for name in ("secret.plan", "secret.png", "secret.gif", "day-two", "Day two", ">Secret<"):
            self.assertNotIn(name, everything)

    def test_a_dev_build_publishes_a_draft_with_its_images(self):
        # A dev build ignores the flag, and writes a site of its own.
        self.site("rebuild", "dev")
        self.assertEqual(self.files(site="_site-dev"), self.PUBLISHED | self.DRAFTS)
        self.assertRegex(self.read("notebook/secret.html", "_site-dev"),
                         r'<img[^>]*src="secret\.plan\.png"[^>]*width="10"[^>]*height="10"')

    def test_the_images_follow_the_section_s_address(self):
        self.configure(sections=[{"dir": "blog", "kind": "Post", "dingbat": None}])
        self.site("rebuild")
        self.assertEqual(self.files("blog"), {p.replace("notebook/", "blog/", 1) for p in self.PUBLISHED})
        self.assertFalse((self.root / "_site/notebook").exists())

    def test_a_page_is_rebuilt_for_its_image_s_sidecar_and_not_otherwise(self):
        self.site("rebuild")
        # Nothing is out of date on the next build: every file the pages
        # depend on is claimed by a rule, the withheld drafts' aside.
        again = self.site("build")
        self.assertNotIn("updated content/blog/", again)
        (self.root / "content/blog/first.lake.png.dims.yaml").write_text("width: 800\nheight: 600\n",
                                                                           encoding="utf-8")
        changed = self.site("build")
        self.assertIn("updated content/blog/first.md", changed)
        self.assertRegex(self.read("notebook/first.html"), r'width="800"[^>]*height="600"')
        # A sidecar that was not there: the collection's shared image.
        (self.root / "content/blog/trip/route.jpg.dims.yaml").write_text("width: 30\nheight: 20\n",
                                                                           encoding="utf-8")
        self.site("build")
        self.assertRegex(self.read("notebook/trip/day-one.html"),
                         r'<img[^>]*src="route\.jpg"[^>]*width="30"[^>]*height="20"')
        self.assertEqual(self.files(), self.PUBLISHED)


if __name__ == "__main__":
    unittest.main()
