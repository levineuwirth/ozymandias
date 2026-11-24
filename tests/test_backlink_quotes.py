"""A quoted paragraph's links in a Backlinks footer, from the quoting page.

A page's Backlinks footer quotes the sentence (and, in a popup, the
paragraph) that links to it. The quote was rendered in its source page's
terms, so a relative link in it named a file beside the *quoting* page:
`[shaney.py](shaney.py)` in an essay that also linked another essay showed
up in that essay's footer as a link to nothing, and check-site's
link-targets gate stopped the deploy (2026-11-24). A relative link to a
page was keyed as written, and the page it named got no backlink.

The first review of the fix added what a quote must also get right: a
query of the page, a responsive image's srcset (in the paragraph popup,
not the sentence), a directory named by a final `.`, and raw HTML whose
URL is written with a character reference. The second added more of
what a character reference can hide: a space in a value without quotes,
a fragment, and a srcset's separators. The third: anchors the quote
defines with their ids written in other spellings, a comma inside a srcset
URL, and whitespace a reference writes before a rooted URL. The fourth:
a data: URL's comma beside a rebased candidate, a comma in a srcset whose
candidates are already rooted (which only the page's own relativizing
pass reads), and a percent-encoded fragment naming an anchor in the quote.
The fifth, of that page pass itself: an SVG's self-closing <style/> and a
script whose data holds a prefix of its closing tag.

Whole builds of a small scratch site, in make build's order: a build, the
footer files, a build.

    python -m unittest tests.test_backlink_quotes -v
"""

from __future__ import annotations

import html
import json
import posixpath
import re
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import engine_path, requires_cabal, script_env, site_binary, srcset_urls, write_site_config

# Script data with a prefix of its closing tag, and markup in a string.
SCRIPT = '{"a": "</scripture>", "b": "<a href=\'/essays/a/\'>A</a>"}'
# The first sentence is the quote: a link beside the page, one below it,
# raw HTML (one value without quotes, with an escaped space), a query of
# the page, fragments of the page (one the quote defines itself), relative
# links to pages (one ending in a dot), a rooted one, and raw HTML rooted
# by a character reference. The second is only in the paragraph popup:
# images with a srcset, one written with escaped separators.
A = ("The [script](tool.py), the [figure](fig/one.svg), "
     '<a href="notes.txt">the notes</a>, <a href=field&#32;notes.txt>the field notes</a> '
     'and a [compact view](?view=compact) are beside this page, as are <a href="&#35;atlas">the atlas</a> '
     'and [a mark]{#mark} (<a href="&#35;mark">the mark</a>); '
     "with <bdi id='here'>an anchor</bdi> (<a href='&#35;here'>back here</a>) and "
     '<span id="r&amp;d">another</span> (<a href="&#35;r&amp;d">to R&amp;D</a>) and '
     '<span id="café">a third</span> (<a href="#caf%C3%A9">to the café</a>); '
     'see [B](/essays/b/), [C](../c/), [E](../e/.), <a href="&#47;essays/c/">C again</a> '
     'and <a href="&#32;/essays/c/">C, spaced</a>. '
     'Here is the figure: ![figure](fig/one.svg){srcset="fig/one.svg 1x, fig/two.svg 2x"} '
     '<img src="small.svg" srcset="small.svg&#32;1x&#44; large.svg 2x" alt="sizes"> '
     '<img src="lake.svg" srcset="lake&#44;summer.svg 1x, lake.svg 2x" alt="seasons"> '
     '<img src="/essays/a/lake.svg" srcset="/essays/a/lake,summer.svg 1x, /essays/a/lake.svg 2x" '
     'alt="rooted seasons"> '
     '<img src="one.gif" srcset="data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7 1x, two.gif 2x" alt="embedded">'
     "\n\n## Atlas {#atlas}\n\nThe atlas."
     # Not quoted: what the page's own relativizing pass must read as HTML does.
     # (The script first: an unclosed style would hide it from the pass.)
     '\n\n<script type="application/json" id="data">' + SCRIPT + "</script>"
     '\n\n<svg width="1" height="1"><style/><rect width="1" height="1"/></svg> '
     'And <a href="/essays/c/">after the svg</a>.')
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
        for name in ("tool.py", "fig/one.svg", "fig/two.svg", "notes.txt", "field notes.txt",
                     "small.svg", "large.svg", "lake,summer.svg", "lake.svg", "one.gif", "two.gif"):
            (essays / "a" / name).parent.mkdir(parents=True, exist_ok=True)
            (essays / "a" / name).write_text("x\n", encoding="utf-8")
        page(essays / "b/index.md", "B", "B.")
        page(essays / "c/index.md", "C", "C.")
        page(essays / "e/index.md", "E", "E.")
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

    def links(self, path: str) -> set[tuple[str, str]]:
        """Every URL in a page's footer (href, src and each srcset
        candidate), as the file it names in _site and its query."""
        footer = self.footer(path)
        urls = [q or b for q, b in re.findall(r'(?:href|src)=(?:"([^"]*)"|([^\s"\'>]+))', footer)]
        for srcset in re.findall(r'srcset="([^"]+)"', footer):
            urls += srcset_urls(srcset)
        # A URL with a scheme (data:, https:) names no file of the site's.
        return {self.resolve(path, url) for url in map(html.unescape, urls)
                if url.split("#")[0] and not re.match(r"[a-zA-Z][a-zA-Z0-9+.-]*:", url)}

    @staticmethod
    def resolve(path: str, url: str) -> tuple[str, str]:
        """A URL read on the page at @path, as the file it names in _site
        and its query; trimmed first, as the URL parser trims it."""
        url = re.sub(r"[\t\n\r]", "", url.strip("".join(map(chr, range(33)))))
        address, _, query = url.split("#")[0].partition("?")
        target = posixpath.normpath(address.lstrip("/") if address.startswith("/")
                                    else posixpath.join(posixpath.dirname(path), address))
        return (target + "/index.html" if address.endswith("/") else target, query)

    def anchors(self, footer: str, text: str) -> list[str]:
        """The hrefs of the footer's links with this text, as read."""
        found = re.findall(r'<a href=(?:"([^"]*)"|\'([^\']*)\'|([^\s"\'>]+))[^>]*>' + re.escape(text) + "</a>",
                           footer)
        self.assertTrue(found, text)
        return [html.unescape(d or s or b) for d, s, b in found]

    def targets(self, path: str) -> set[str]:
        return {target for target, _ in self.links(path)}

    def graph(self) -> dict:
        return json.loads(self.read("data/backlinks.json"))

    def test_a_quoted_link_names_the_file_beside_the_page_it_was_quoted_from(self):
        targets = self.targets("essays/b/index.html")
        for name in ("essays/a/tool.py", "essays/a/fig/one.svg", "essays/a/fig/two.svg", "essays/a/notes.txt",
                     "essays/a/field notes.txt", "essays/a/small.svg", "essays/a/large.svg"):
            self.assertIn(name, targets)
        # What the deploy gate checks: every link in the footer is there.
        for target in targets:
            self.assertTrue((self.root / "_site" / target).is_file(), f"{target} resolves to nothing")

    def test_a_query_of_the_page_is_the_quoted_page_s(self):
        self.assertIn(("essays/a/index.html", "view=compact"), self.links("essays/b/index.html"))
        # And it is no link to another page: not to the home page, not to A.
        graph = self.graph()
        self.assertIn("/essays/b/", graph)  # how the graph keys a page
        for key in ("/", "/essays/a/"):
            self.assertNotIn("/essays/a/", [s["url"] for s in graph.get(key, [])], key)

    def test_the_paragraph_popup_s_image_loads_from_the_quoted_page(self):
        footer = self.footer("essays/b/index.html")
        popup = re.search(r'class="backlink-full-popup".*?</li>', footer, re.S)
        self.assertIsNotNone(popup)
        self.assertRegex(popup.group(0), r'<img[^>]*srcset="[^"]*essays/a/fig/one\.svg 1x, [^"]*essays/a/fig/two\.svg 2x"')
        # Escaped separators: both candidates are A's, not only the first.
        srcset = re.search(r'<img[^>]*srcset="([^"]*)"[^>]*alt="sizes"', popup.group(0))
        self.assertIsNotNone(srcset)
        self.assertEqual([self.resolve("essays/b/index.html", c)[0] for c in srcset_urls(srcset.group(1))],
                         ["essays/a/small.svg", "essays/a/large.svg"])
        # A comma inside a URL is the URL's, rebased here or rooted already.
        for alt in ("seasons", "rooted seasons"):
            self.assertEqual(self.candidates(popup.group(0), alt), ["essays/a/lake,summer.svg", "essays/a/lake.svg"])
        # A data: URL keeps the comma it needs beside a candidate rebased.
        srcset = re.search(r'<img[^>]*srcset="([^"]*)"[^>]*alt="embedded"', popup.group(0))
        self.assertIsNotNone(srcset)
        urls = srcset_urls(srcset.group(1))
        self.assertEqual(urls[0], "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7")
        self.assertEqual(self.resolve("essays/b/index.html", urls[1])[0], "essays/a/two.gif")

    def candidates(self, markup: str, alt: str, page: str = "essays/b/index.html") -> list[str]:
        srcset = re.search(r'<img[^>]*srcset="([^"]*)"[^>]*alt="' + re.escape(alt) + '"', markup)
        self.assertIsNotNone(srcset, alt)
        return [self.resolve(page, c)[0] for c in srcset_urls(srcset.group(1))]

    def test_a_self_closing_style_in_svg_leaves_the_page_s_links_to_the_pass(self):
        # In SVG, <style/> closes itself; read as HTML's raw-text style, it
        # hid every link after it from relativizing.
        hrefs = self.anchors(self.read("essays/a/index.html"), "after the svg")
        self.assertEqual(hrefs, ["../../essays/c/"])

    def test_script_data_is_left_as_written(self):
        # </scripture> does not close the script; its data is not markup.
        self.assertIn('<script type="application/json" id="data">' + SCRIPT + "</script>",
                      self.read("essays/a/index.html"))

    def test_the_page_itself_keeps_a_comma_in_a_srcset_url(self):
        # Hakyll's relativizeUrls cut these at the comma on A's own page.
        a = self.read("essays/a/index.html")
        for alt in ("seasons", "rooted seasons"):
            self.assertEqual(self.candidates(a, alt, "essays/a/index.html"),
                             ["essays/a/lake,summer.svg", "essays/a/lake.svg"])

    def test_a_value_without_quotes_keeps_its_whole_url(self):
        for href in self.anchors(self.footer("essays/b/index.html"), "the field notes"):
            self.assertEqual(self.resolve("essays/b/index.html", href), ("essays/a/field notes.txt", ""))

    def test_a_fragment_written_with_a_character_reference_is_the_quoted_page_s(self):
        footer = self.footer("essays/b/index.html")
        for href in self.anchors(footer, "the atlas"):
            self.assertEqual(self.resolve("essays/b/index.html", href), ("essays/a/index.html", ""))
            self.assertTrue(href.endswith("#atlas"), href)
        # One the quote defines itself stays in the quote, where its anchor
        # is, however its id is written.
        self.assertEqual(set(self.anchors(footer, "the mark")), {"#mark"})
        self.assertEqual(set(self.anchors(footer, "back here")), {"#here"})
        self.assertEqual(set(self.anchors(footer, "to R&amp;D")), {"#r&d"})
        # Percent-encoded, as a browser looks for the anchor: UTF-8, decoded.
        self.assertEqual(set(self.anchors(footer, "to the café")), {"#caf%C3%A9"})

    def test_a_raw_html_link_written_with_a_character_reference_is_read_first(self):
        # &#47;essays/c/ is /essays/c/, rooted: no directory of A's goes before it.
        footer = self.footer("essays/b/index.html")
        for href in self.anchors(footer, "C again"):
            self.assertEqual(self.resolve("essays/b/index.html", href), ("essays/c/index.html", ""), href)
        # &#32;/essays/c/ too: the URL parser trims the space, so it is rooted.
        for href in self.anchors(footer, "C, spaced"):
            self.assertEqual(self.resolve("essays/b/index.html", href), ("essays/c/index.html", ""), href)

    def test_a_flat_page_s_links_start_from_its_section(self):
        # D's sentence is in B's footer too, and its script is A's.
        items = self.footer("essays/b/index.html").split('<li class="backlink-item">')
        d = [item for item in items if re.search(r'class="backlink-source"[^>]*>D</a>', item)]
        self.assertEqual(len(d), 1)
        for href in self.anchors(d[0], "script"):
            self.assertEqual(self.resolve("essays/b/index.html", href), ("essays/a/tool.py", ""))

    def test_a_relative_link_to_a_page_is_a_backlink(self):
        self.assertIn("essays/a/index.html", self.targets("essays/c/index.html"))
        # A final dot names the directory, as a browser reads it.
        self.assertIn("essays/a/index.html", self.targets("essays/e/index.html"))

    def test_the_page_itself_is_as_written(self):
        a = self.read("essays/a/index.html")
        for href in ('href="tool.py"', 'href="fig/one.svg"', 'href="notes.txt"', 'href="?view=compact"',
                     'href="../e/."', 'srcset="fig/one.svg 1x, fig/two.svg 2x"'):
            self.assertIn(href, a)


if __name__ == "__main__":
    unittest.main()
