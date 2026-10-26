"""Opt-in trailing icons through the real Markdown and filter pipeline."""

from pathlib import Path
import subprocess
import tempfile
import unittest

from bs4 import BeautifulSoup

from tests._helpers import SITE_ROOT, requires_cabal, site_binary


@requires_cabal
class LinkIconsEndTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.binary = site_binary()

    def render(self, markdown):
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp) / "passage.md"
            source.write_text(markdown, encoding="utf-8")
            done = subprocess.run(
                [str(self.binary), "render-fixture", str(source)],
                cwd=SITE_ROOT, capture_output=True, text=True, timeout=120,
            )
        self.assertEqual(done.returncode, 0, done.stderr)
        return BeautifulSoup(done.stdout, "html.parser")

    def test_quote_keeps_text_links_and_collects_named_icons_before_note(self):
        soup = self.render(
            '["[A **claim**](https://example.org/a){#claim} mentions '
            '[4chan](https://en.wikipedia.org/wiki/4chan) here?"]'
            '{.link-icons-end}^[Note.]'
        )
        group = soup.select_one('.link-icons-end')
        originals = group.select('a.link-icon-deferred')
        icons = group.select('.link-icons-tail a')
        self.assertEqual(len(originals), 2)
        self.assertEqual([a['href'] for a in icons], [a['href'] for a in originals])
        self.assertEqual([a['data-link-icon'] for a in icons], ['external', 'wikipedia'])
        self.assertEqual([a['aria-label'] for a in icons], ['A claim', '4chan'])
        self.assertEqual(len(soup.select('#claim')), 1)
        self.assertEqual(group.select_one('.link-icons-tail').previous_sibling.strip(), 'here?”')
        self.assertEqual(group.find_next_sibling()['class'], ['sidenote-ref'])
        for icon in icons:
            self.assertEqual(icon['target'], '_blank')
            self.assertIn('noopener', icon['rel'])
            self.assertEqual(icon.get_text(), '')

    def test_nested_groups_and_notes_keep_their_own_links(self):
        soup = self.render(
            '[[outer](https://example.org/outer) '
            '[[inner](https://example.org/inner)]{.link-icons-end} '
            'note^[A [reference](https://example.org/note).].]{.link-icons-end}'
        )
        outer, inner = soup.select('.link-icons-end')[:2]
        self.assertEqual(
            [a['href'] for a in outer.select(':scope > .link-icons-tail > a')],
            ['https://example.org/outer'],
        )
        self.assertEqual(
            [a['href'] for a in inner.select('.link-icons-tail > a')],
            ['https://example.org/inner'],
        )
        note = soup.select_one('.footnote-item a[href="https://example.org/note"]')
        self.assertNotIn('link-icon-deferred', note['class'])

    def test_repeated_icons_and_internal_destinations_stay_distinct(self):
        soup = self.render(
            '[[one](https://example.org/one) [two](https://example.org/two) '
            '[local](/colophon.html) [jump](#part) [PDF](/paper.pdf).]{.link-icons-end}'
        )
        icons = soup.select('.link-icons-tail a')
        self.assertEqual([a['href'] for a in icons],
                         ['https://example.org/one', 'https://example.org/two', '/colophon.html'])
        self.assertEqual([a['data-link-icon'] for a in icons], ['external', 'external', 'internal'])
        self.assertNotIn('target', icons[-1].attrs)
        self.assertNotIn('link-icon-deferred', soup.select_one('.pdf-link')['class'])

    def test_plain_links_and_empty_passages_do_not_gain_trailing_icons(self):
        soup = self.render('[normal](https://example.org) [Plain prose.]{.link-icons-end}')
        self.assertFalse(soup.select('.link-icons-tail, .link-icon-deferred'))
        self.assertEqual(soup.select_one('a')['data-link-icon'], 'external')
