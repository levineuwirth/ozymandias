"""tools/front_matter.py, the split the tools and tests share, against the
cases Hakyll's own (Hakyll.Core.Provider.Metadata.splitMetadata) decides.
build/Drafts.hs reads pages both through Hakyll and through a lenient
split; tests/test_drafts.py covers that side."""

import unittest
from tests._helpers import load_tool

try:
    import yaml
except ImportError:  # pragma: no cover
    yaml = None


@unittest.skipIf(yaml is None, "PyYAML not installed")
class FrontMatterSplit(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.fm = load_tool("front_matter.py")

    def test_splits_as_hakyll_does(self) -> None:
        cases = {
            # A value holding Pandoc's em dash is not a fence.
            "---\ntitle: A --- B\n---\nbody\n": ("title: A --- B", "body\n"),
            # Dots close, and CRLF is a newline.
            "---\r\ntitle: x\r\n...\r\nbody": ("title: x", "body"),
            # A longer fence opens, closed only by one as long.
            "----\ndraft: true\n----\nb": ("draft: true", "b"),
            "----\ndraft: true\n---\nb": None,
            "---\ntitle: x\n----\nb": None,
            "---\n---\nb": ("", "b"),
            "---\ntitle: x\n---": ("title: x", ""),
            # Unterminated, or not at the very start: no front matter.
            "---\ndraft: true\n": None,
            "\n---\ntitle: x\n---\n": None,
            # More lenient than Hakyll, as build/Drafts.hs is.
            "--- \ntitle: x\n---\n": ("title: x", ""),
        }
        for text, want in cases.items():
            with self.subTest(text=text):
                self.assertEqual(self.fm.split(text), want)

    def test_load(self) -> None:
        self.assertEqual(self.fm.load("---\ntitle: T\n---\n"), {"title": "T"})
        self.assertEqual(self.fm.load("no front matter\n"), {})
        self.assertEqual(self.fm.load("---\n- a list\n---\n"), {})
        with self.assertRaises(yaml.YAMLError):
            self.fm.load("---\ntitle: [unclosed\n---\n")


if __name__ == "__main__":
    unittest.main()
