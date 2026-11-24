"""A quoted paragraph's links in a Backlinks footer, from the quoting page.

A page's Backlinks footer quotes the sentence (and, in a popup, the
paragraph) that links to it. The quote was rendered in its source page's
terms, so a relative link in it named a file beside the *quoting* page:
`[shaney.py](shaney.py)` in an essay that also linked another essay showed
up in that essay's footer as a link to nothing, and check-site's
link-targets gate stopped the deploy (2026-11-24). A relative link to a
page was keyed as written, and the page it named got no backlink.

Whole builds of a small scratch site, in make build's order: a build, the
footer files, a build.

    python -m unittest tests.test_backlink_quotes -v
"""

from __future__ import annotations

import json
import posixpath
import re
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import engine_path, requires_cabal, script_env, site_binary, write_site_config

# One paragraph, one sentence: every link in it is in the quote. A link
# beside the page, one below it, one in raw HTML, a relative link to a page
# and a rooted one.
A = ("The [script](tool.py), the [figure](fig/one.svg) and "
     '<a href="notes.txt">the notes</a> are beside this page; see [B](/essays/b/) and [C](../c/).')
# A flat page: its relative links start from the section's directory.
D = "The [script](a/tool.py) is A's; see [B](/essays/b/)."


def page(path: Path, title: str, body: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(f"---\ntitle: {title}\ndate: 2026-01-01\n---\n\n{body}\n", encoding="utf-8")


@requires_cabal
class BacklinkQuotes(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        tmp = tempfile.TemporaryDirectory(prefix="backlink-quotes-")
        cls.addClassCleanup(tmp.cleanup)
        cls.root = root = Path(tmp.name)
        (root / "engine").symlink_to(engine_path(""))
        essays = root / "content/essays"
        page(root / "content/index.md", "Home", "Home.")
        page(essays / "a/index.md", "A", A)
        for name in ("tool.py", "fig/one.svg", "notes.txt"):
            (essays / "a" / name).parent.mkdir(parents=True, exist_ok=True)
            (essays / "a" / name).write_text("x\n", encoding="utf-8")
        page(essays / "b/index.md", "B", "B.")
        page(essays / "c/index.md", "C", "C.")
        page(essays / "d.md", "D", D)
        (root / "site.yaml").write_text(json.dumps(write_site_config(root)), encoding="utf-8")
        for command in ("build", "footer-data", "build"):
            # One worker: with several, a build of a site this small has
            # failed in Hakyll's metadata cache (tests/test_blog_images.py).
            done = subprocess.run([str(site_binary()), command], cwd=root,
                                  env=script_env(SITE_ENV="production", SITE_THREADS="1"),
                                  capture_output=True, text=True, timeout=240)
            if done.returncode:
                raise AssertionError(f"site {command} failed:\n{done.stdout[-3000:]}{done.stderr[-3000:]}")

    def read(self, path: str) -> str:
        return (self.root / "_site" / path).read_text(encoding="utf-8")

    def footer(self, path: str) -> str:
        found = re.search(r'<section[^>]*id="backlinks".*?</section>', self.read(path), re.S)
        self.assertIsNotNone(found, f"{path} has no Backlinks footer")
        return found.group(0)

    def targets(self, path: str) -> set[str]:
        """Every href in a page's footer, as the file it names in _site."""
        here = posixpath.dirname(path)
        out = set()
        for href in re.findall(r'href="([^"#]+)', self.footer(path)):
            target = posixpath.normpath(posixpath.join(here, href))
            out.add(target + "/index.html" if href.endswith("/") else target)
        return out

    def test_a_quoted_link_names_the_file_beside_the_page_it_was_quoted_from(self):
        targets = self.targets("essays/b/index.html")
        for name in ("essays/a/tool.py", "essays/a/fig/one.svg", "essays/a/notes.txt"):
            self.assertIn(name, targets)
        # What the deploy gate checks: every link in the footer is there.
        for target in targets:
            self.assertTrue((self.root / "_site" / target).is_file(), f"{target} resolves to nothing")

    def test_a_flat_page_s_links_start_from_its_section(self):
        # D's sentence is in B's footer too, and its script is A's.
        self.assertGreaterEqual(self.footer("essays/b/index.html").count("essays/a/tool.py"), 2)

    def test_a_relative_link_to_a_page_is_a_backlink(self):
        self.assertIn("essays/a/index.html", self.targets("essays/c/index.html"))

    def test_the_page_itself_is_as_written(self):
        a = self.read("essays/a/index.html")
        for href in ('href="tool.py"', 'href="fig/one.svg"', 'href="notes.txt"'):
            self.assertIn(href, a)


if __name__ == "__main__":
    unittest.main()
