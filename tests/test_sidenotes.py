"""Notes through the real Markdown and filter pipeline (Filters/Sidenotes.hs).

Each footnote becomes a reference in the text and one item in a list at
the end of the body: the note's only copy, rendered as block content.

Checked: the reference links to the item and the item back to the
reference; a note's ids appear once; paragraphs, lists, quotations and code
stay inside the item, and no block element is ever written inside a <p>
(a list inside an inline note span made the browser close the paragraph
and spill the list into the article); the back-link closes the last
paragraph; letters run on past z.
"""

from html.parser import HTMLParser
from pathlib import Path
import subprocess
import tempfile
import unittest

from bs4 import BeautifulSoup

from tests._helpers import SITE_ROOT, requires_cabal, site_binary

# Elements whose start tag closes an open <p> (HTML's parsing rules).
CLOSES_P = {"address", "article", "aside", "blockquote", "details", "div", "dl", "fieldset",
            "figcaption", "figure", "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6",
            "header", "hgroup", "hr", "main", "menu", "nav", "ol", "p", "pre", "section",
            "table", "ul"}


class BlocksInParagraphs(HTMLParser):
    """Every block start tag met while a <p> is open."""

    def __init__(self) -> None:
        super().__init__()
        self.open_p = 0
        self.found: list[str] = []

    def handle_starttag(self, tag, attrs) -> None:
        if tag in CLOSES_P and self.open_p:
            self.found.append(tag)
        if tag == "p":
            self.open_p += 1

    def handle_endtag(self, tag) -> None:
        if tag == "p" and self.open_p:
            self.open_p -= 1


@requires_cabal
class SidenotesTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.binary = site_binary()

    def render(self, markdown: str) -> str:
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp) / "passage.md"
            source.write_text(markdown, encoding="utf-8")
            done = subprocess.run(
                [str(self.binary), "render-fixture", str(source)],
                cwd=SITE_ROOT, capture_output=True, text=True, timeout=120,
            )
        self.assertEqual(done.returncode, 0, done.stderr)
        return done.stdout.split("<!-- render-fixture: toc -->")[0]

    def test_a_rich_note_is_one_block_item(self):
        html = self.render(
            "Opening text with a note.[^a] More text.\n\n"
            "[^a]: [An anchor]{#note-anchor} and [a reference](https://example.org/r).\n\n"
            "    Second paragraph.\n\n"
            "    - First item\n"
            "    - Second item\n\n"
            "    > A quotation.\n\n"
            "    ```text\n"
            "    sample code\n"
            "    ```\n"
        )
        soup = BeautifulSoup(html, "html.parser")

        ref = soup.select_one("p sup.sidenote-ref#snref-a > a")
        self.assertEqual(ref["href"], "#fn-a")
        self.assertEqual(ref["role"], "doc-noteref")

        item = soup.select_one("section.footnotes > ol.footnotes-list > li#fn-a.footnote-item")
        self.assertIsNotNone(item)
        self.assertEqual(item.select_one(".footnote-label").get_text(), "a")
        self.assertEqual([li.get_text() for li in item.select("ul > li")], ["First item", "Second item"])
        self.assertIn("A quotation.", item.select_one("blockquote").get_text())
        self.assertEqual(item.select_one("pre").get_text(), "sample code")
        self.assertEqual(len(item.find_all("p", recursive=False)), 2)

        # One copy: its id once, its text once.
        self.assertEqual(len(soup.select("#note-anchor")), 1)
        self.assertEqual(html.count("Second paragraph."), 1)
        self.assertEqual(soup.select(".sidenote"), [])

        # A note that ends in a code block has no paragraph to close:
        # the back-link follows it.
        back = item.select_one("a.footnote-back")
        self.assertEqual(back["href"], "#snref-a")
        self.assertEqual(back["role"], "doc-backlink")
        self.assertEqual(back.parent, item)

        parser = BlocksInParagraphs()
        parser.feed(html)
        self.assertEqual(parser.found, [])

    def test_the_back_link_closes_the_last_paragraph(self):
        soup = BeautifulSoup(self.render("One.^[A short note.] Two.^[Another.]"), "html.parser")
        for label in "ab":
            back = soup.select_one(f"#fn-{label} a.footnote-back")
            self.assertEqual(back.parent.name, "p")
            self.assertEqual(back["href"], f"#snref-{label}")

    def test_letters_run_on_past_z(self):
        soup = BeautifulSoup(self.render(" ".join(f"Word.^[Note {i}.]" for i in range(28))),
                             "html.parser")
        refs = [a.get_text() for a in soup.select(".sidenote-ref a")]
        self.assertEqual(refs[:2] + refs[25:], ["a", "b", "z", "aa", "ab"])
        self.assertEqual([li["id"] for li in soup.select(".footnote-item")][-1], "fn-ab")


if __name__ == "__main__":
    unittest.main()
