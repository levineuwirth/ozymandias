"""A citation marker keeps what its author wrote around the key
(build/Citations.hs, markerLabel).

Pandoc keeps a locator in the citation's suffix: `[@PralatWormald,
Theorem 4.1]` is the key and ", Theorem 4.1". The marker's label was the
cluster's numbers alone, so a page cited "[8]" and lost which theorem.

The cases below render Markdown through the essay pipeline and exercise the
site-wide bibliography through a fixture build.
"""

import re
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

from tests import fixture_variants
from tests._helpers import ENGINE, engine_path, requires_cabal, script_env, site_binary

# Markdown, rendered label, number of note references beside the label.
# Pandoc's smart extension writes "p. 3" with a no-break space (\u00a0).
CASES = [
    ("[@alpha2001]", "[1]", 0),
    ("[@alpha2001; @beta2010]", "[1,2]", 0),
    ("[@alpha2001, Theorem 4.1]", "[1, Theorem 4.1]", 0),
    ("[see @alpha2001, p. 3; @beta2010]", "[see 1, p.\u00a03; 2]", 0),
    # A locator's own separator is kept, and a textual citation's, which
    # has none, is a comma: neither reads as reference 123.
    ("[@alpha2001 23]", "[1 23]", 0),
    # A no-break space is a separator too, escaped or as the character.
    ("[@alpha2001\\ 23]", "[1\u00a023]", 0),
    ("[@alpha2001\u00a023]", "[1\u00a023]", 0),
    ("@alpha2001 [23]", "[1, 23]", 0),
    # A bracketed citation's suffix is as written: no comma is guessed in.
    ("[@alpha2001—passim]", "[1—passim]", 0),
    ("[@alpha2001(23)]", "[1(23)]", 0),
    ("@alpha2001 [p. 3]", "[1, p.\u00a03]", 0),
    ("[@alpha2001, *Lemma* 2]", "[1, <em>Lemma</em> 2]", 0),
    ("[@alpha2001, [ch. 2](https://example.org)]", "[1, ch.\u00a02]", 0),
    ('[@alpha2001, <a href="#chapter">chapter</a>]', "[1, chapter]", 0),
    ('[@alpha2001, `<a href="#chapter">chapter</a>`{=html}]', "[1, chapter]", 0),
    ('[@alpha2001, `<em><a href="#chapter">chapter</a></em>`{=html}]', "[1, <em>chapter</em>]", 0),
    # A > in a quoted attribute is the tag's, not its end.
    ('[@alpha2001, <a title="n > 1" href="#chapter">chapter</a>]', "[1, chapter]", 0),
    ("[@alpha2001, chapter^[A detail.]]", "[1, chapter]", 1),
    # A note in a note goes out with it: one note, not two.
    ("[@alpha2001, chapter^[Outer^[Inner.].]]", "[1, chapter]", 1),
    # Two notes that read the same are two notes.
    ("[@alpha2001, ch^[Outer^[Middle^[Same.].].] and again^[Same.]]", "[1, ch and again]", 2),
    ("[@alpha2001, p. <3 & up]", "[1, p.\u00a0&lt;3 &amp; up]", 0),
]

PAGE = """---
title: Locators
---

A footnote in a locator [@alpha2001, chapter^[A detail.]] and an anchor
[@beta2010, <a href="#chapter">chapter</a>] keep the paragraph whole.
"""

# A citation in a locator's footnote, a note in a note, and four notes,
# two of them alike.
NESTED = """---
title: Nested
---

A claim [@alpha2001, chapter^[See [@gamma2020].]] and another
[@beta2010, part^[Outer^[Inner.].]], and a third
[@alpha2001, ch^[First^[Middle^[Same.].].] and again^[Same.]].
"""


# A key the bibliography lacks, between two it has, and first in a cluster.
MISSING = """---
title: Missing
---

One [@alpha2001], a key with no entry [@nosuchkey], two [@beta2010], and a
cluster led by another [@nosuchother; @gamma2020].
"""


# The cases a missing key, a locator or the bibliography's place can put a
# number, a note or a back-link wrong in.
SETTLED = {
    # A note on a missing key's locator, in a cluster that also resolves.
    "note": "A claim [@nosuch, p. 1^[See [@gamma2020].]; @alpha2001].",
    # A citation in a missing key's locator, discarded with it, before the
    # same key is cited where the page shows it.
    "discarded": "A claim [@nosuch, see also [@gamma2020]]. Then [@gamma2020].",
    # The bibliography inside a wrapper Div.
    "nested": "A claim [@alpha2001].\n\n::: {.wrapper}\n::: {#refs}\n:::\n:::",
    # A BibLaTeX alias of an entry (ids = {alphaAlias}), cited beside it,
    # and alone.
    "alias": "A claim [@alpha2001] and the same by its alias [@alphaAlias].",
    "alias-alone": "A claim by an alias alone [@alphaAlias].",
    # A citation in a shown locator, with a locator of its own.
    "inner": "A claim [@alpha2001, see [@gamma2020, Theorem 4.1]].",
    # A discarded citation, and the bibliography in a wrapper Div.
    "wrapped-discard": "A claim [@nosuch, see [@gamma2020]].\n\n::: {.wrapper}\n::: {#refs}\n:::\n:::",
}


@requires_cabal
class ThroughThePipeline(unittest.TestCase):
    """A copy of the fixture site of this module's own: the golden tests
    keep theirs (tests.test_golden.profile_site) for their module's life."""

    @classmethod
    def setUpClass(cls):
        tmp = tempfile.TemporaryDirectory(prefix="locators-")
        cls.addClassCleanup(tmp.cleanup)
        build = next(b for b in fixture_variants.builds() if b.name == "full")
        cls.site = Path(tmp.name) / "site"
        fixture_variants.copy_site(build, cls.site, ENGINE)
        if build.switches["archive"]:
            subprocess.run([sys.executable, str(engine_path("tools/archive.py")), "fetch"], cwd=cls.site,
                           env=script_env(SITE_ROOT=str(cls.site)), check=True, capture_output=True, timeout=120)
        # The numeric year precedes the aliases. beta2010 is already a real
        # entry and takes precedence over Alpha's claim on that key.
        bib = cls.site / "data" / "bibliography.bib"
        text = bib.read_text(encoding="utf-8")
        year = "year      = {2001}"
        assert text.count(year) == 1
        bib.write_text(
            text.replace(year, "year = 2001,\n  ids = {alphaAlias, beta2010}")
            + "\n@misc{epsilon2030, author = {Epsilon, Eve}, title = {Cited Out of Sight}, year = 2030}\n"
            + "@book{zeta1980, author = {Zeta, Zoe}, year = 1980, ids = {readingAlias}, "
              "title = {Explicit Further Reading}}\n", encoding="utf-8")

    def render(self, text: str) -> tuple[str, str]:
        (self.site / "locators.md").write_text(text, encoding="utf-8")
        done = subprocess.run([str(site_binary()), "render-fixture", "locators.md"], cwd=self.site,
                              env=script_env(SITE_ROOT=str(self.site)), capture_output=True, text=True, timeout=300)
        self.assertEqual(done.returncode, 0, done.stderr)
        body, rest = done.stdout.split("<!-- render-fixture: toc -->")
        bibliography, self.last_further = rest.split("<!-- render-fixture: bibliography -->")[1].split(
            "<!-- render-fixture: further-reading -->")
        return body, bibliography

    def test_labels(self):
        body, _ = self.page("\n\n".join(md for md, _, _ in CASES))
        prose = body.split('<section class="footnotes"', 1)[0]
        paragraphs = re.findall(r"<p>(.*?)</p>", prose, re.S)
        self.assertEqual(len(paragraphs), len(CASES))
        for (md, label, notes), paragraph in zip(CASES, paragraphs):
            with self.subTest(markdown=md):
                self.assertEqual(re.findall(r'class="cite-link"[^>]*>(.*?)</a>', paragraph, re.S), [label])
                self.assertEqual(paragraph.count('class="sidenote-ref"'), notes)

    def test_a_marker_is_one_link_and_its_footnote_the_page_s(self):
        body, _ = self.render(PAGE)
        markers = re.findall(r'<sup class="cite-marker"[^>]*>(.*?)</sup>', body, re.S)
        self.assertEqual(len(markers), 2, body)
        for marker in markers:
            with self.subTest(marker=marker):
                # One link: no anchor, footnote or section inside it.
                self.assertEqual(marker.count("<a "), 1, marker)
                self.assertNotIn("<section", marker)
        self.assertIn(">[1, chapter]</a>", markers[0])
        self.assertIn(">[2, chapter]</a>", markers[1])
        # The paragraph is one paragraph, and the footnote is the page's.
        self.assertEqual(body.count("A footnote in a locator"), 1)
        self.assertEqual(len(re.findall(r"<p>A footnote in a locator.*?keep the paragraph whole\.</p>", body, re.S)), 1, body)
        self.assertIn("A detail.", body)
        self.assertNotIn('href="#chapter"', body)

    def test_backlinks_land_and_notes_are_printed_once(self):
        body, bibliography = self.render(NESTED)
        ids = set(re.findall(r'id="([^"]+)"', body))
        backs = re.findall(r'href="#(cite-back-[^"]+)"', bibliography)
        self.assertEqual(len(backs), 3, bibliography)
        for back in backs:
            with self.subTest(back=back):
                self.assertIn(back, ids)
        self.assertEqual(body.count("Outer"), 1, body)
        self.assertEqual(body.count("Inner."), 1, body)
        self.assertEqual(body.count("Same."), 2, body)
        self.assertEqual(body.count("First"), 1, body)
        self.assertEqual(body.count("Middle"), 1, body)

    def test_a_key_with_no_entry_takes_no_number(self):
        # Each marker shows its entry's number and links to that entry: a
        # missing key took a number no entry had, and every marker after it
        # named the next entry's ([3] on the entry numbered [2]).
        body, bibliography = self.render(MISSING)
        entries = dict(re.findall(
            r'<div id="ref-([^"]+)" class="csl-entry"[^>]*>\s*<a class="ref-num" href="#[^"]+">\[(\d+)\]</a>',
            bibliography))
        self.assertEqual(entries, {"alpha2001": "1", "beta2010": "2", "gamma2020": "3"}, bibliography)
        markers = re.findall(r'<a href="#ref-([^"]+)" class="cite-link" data-cite-keys="([^"]*)">\[(\d+)\]</a>', body)
        self.assertEqual([(k, n) for k, _, n in markers],
                         [("alpha2001", "1"), ("beta2010", "2"), ("gamma2020", "3")], body)
        for key, keys, number in markers:
            with self.subTest(key=key):
                self.assertEqual(entries[key], number)
                self.assertEqual(keys, f"ref-{key}")
        # A citation with no entry at all is shown at the claim, unlinked,
        # with the key to fix.
        missing = re.findall(r'<sup class="cite-marker cite-missing"[^>]*>(.*?)</sup>', body)
        self.assertEqual(missing, ["[?nosuchkey]"], body)
        self.assertNotIn("#ref-nosuchkey", body)
        self.assertNotIn("ref-nosuchother", body)

    def markers(self, body: str) -> list[tuple[str, str]]:
        return re.findall(r'<a href="#ref-([^"]+)" class="cite-link"[^>]*>(\[[^<]*\])</a>', body)

    def landed(self, body: str, bibliography: str) -> None:
        ids = set(re.findall(r'id="([^"]+)"', body))
        for back in re.findall(r'href="#(cite-back-[^"]+)"', bibliography):
            with self.subTest(back=back):
                self.assertIn(back, ids)

    def page(self, text: str) -> tuple[str, str]:
        return self.render(f"---\ntitle: Settled\n---\n\n{text}\n")

    def test_a_missing_key_s_note_is_kept(self):
        body, bibliography = self.page(SETTLED["note"])
        self.assertEqual(self.markers(body), [("alpha2001", "[1]"), ("gamma2020", "[2]")], body)
        self.assertIn("See", body)
        self.landed(body, bibliography)

    def test_a_discarded_locator_spends_no_id(self):
        body, bibliography = self.page(SETTLED["discarded"])
        self.assertIn(">[?nosuch]</sup>", body)
        self.assertEqual(self.markers(body), [("gamma2020", "[1]")], body)
        self.assertIn('id="cite-back-1"', body)
        self.assertNotIn("see also", body)
        self.landed(body, bibliography)

    def test_a_bibliography_in_a_wrapper_is_found(self):
        body, bibliography = self.page(SETTLED["nested"])
        self.assertNotIn("cite-missing", body)
        self.assertEqual(self.markers(body), [("alpha2001", "[1]")], body)
        self.assertIn('id="refs"', bibliography)
        self.assertNotIn('id="refs"', body)

    def test_an_alias_is_its_entry(self):
        body, bibliography = self.page(SETTLED["alias"])
        self.assertNotIn("cite-missing", body)
        self.assertEqual(self.markers(body), [("alpha2001", "[1]"), ("alpha2001", "[1]")], body)
        self.assertEqual(len(re.findall(r'class="csl-entry"', bibliography)), 1, bibliography)
        self.landed(body, bibliography)

    def test_an_alias_alone_is_its_entry(self):
        body, bibliography = self.page(SETTLED["alias-alone"])
        self.assertNotIn("cite-missing", body)
        self.assertEqual(self.markers(body), [("alpha2001", "[1]")], body)
        self.assertIn('id="ref-alpha2001"', bibliography)
        self.landed(body, bibliography)

    def test_primary_key_wins_over_an_alias(self):
        body, bibliography = self.page("First [@beta2010], then [@alpha2001].")
        self.assertEqual(self.markers(body), [("beta2010", "[1]"), ("alpha2001", "[2]")])
        self.assertIn('id="ref-beta2010"', bibliography)
        self.assertIn('id="ref-alpha2001"', bibliography)
        self.landed(body, bibliography)

    def test_author_spans_survive_the_citation_pass(self):
        body, _ = self.page("[Term]{#target .cite-settled} and [jump](#target). "
                            "See [@alpha2001, [here]{#place .cite-in-label}].")
        self.assertIn('<span id="target" class="cite-settled">Term</span>', body)
        self.assertIn('href="#target"', body)
        self.assertIn('<span id="place" class="cite-in-label">here</span>', body)

    def test_further_reading_alone_keeps_the_refs_target(self):
        body, bibliography = self.render('---\nfurther-reading: [readingAlias]\n---\n\n[Reading](#refs).\n\n::: {#refs}\n:::\n')
        self.assertIn('href="#refs"', body)
        self.assertFalse(bibliography.strip())
        self.assertEqual(self.last_further.count('id="refs"'), 1)
        self.assertIn('id="ref-zeta1980"', self.last_further)

    def test_refs_heading_remains_the_only_target(self):
        body, bibliography = self.page("## References {#refs}\n\n[See references](#refs) [@alpha2001].")
        self.assertEqual(body.count('id="refs"'), 1)
        self.assertNotIn('id="refs"', bibliography)

    def test_a_citation_in_a_locator_keeps_its_own(self):
        body, bibliography = self.page(SETTLED["inner"])
        self.assertEqual(self.markers(body), [("alpha2001", "[1, see [2, Theorem 4.1]]")], body)
        # Its entry links back to the marker it is shown in.
        self.assertIn('<a class="ref-num" href="#cite-back-1">[2]</a>', bibliography)
        self.landed(body, bibliography)

    def test_further_reading_star_lists_every_uncited_entry(self):
        body, rest = self.render("---\ntitle: Star\nfurther-reading: [\"*\"]\n---\n\nA claim [@alpha2001].\n")
        further = self.last_further
        self.assertEqual(sorted(re.findall(r'id="ref-([^"]+)" class="csl-entry"', further)),
                         ["beta2010", "delta2026", "epsilon2030", "gamma2020", "zeta1980"], further)

    def test_a_wrapped_bibliography_lists_only_what_is_shown(self):
        body, bibliography = self.page(SETTLED["wrapped-discard"])
        self.assertIn(">[?nosuch]</sup>", body)
        self.assertNotIn("ref-gamma2020", body)
        self.assertNotIn("ref-gamma2020", bibliography)

    def test_the_site_bibliography_lists_what_pages_show(self):
        # A work cited only in a missing key's discarded locator is on no
        # page, and so not on /bibliography/.
        essay = self.site / "content" / "essays" / "out-of-sight.md"
        essay.write_text("---\ntitle: Out of Sight\ndate: 2026-01-02\ntags: [notes]\n"
                         "further-reading: [readingAlias]\n---\n\n"
                         "A claim [@nosuch, see [@epsilon2030]] and one shown [@beta2010]. "
                         "See [the references](#refs) and [this term](#target): [term]{#target .cite-settled}.\n\n"
                         ":::: {.wrapper}\n::: {#refs}\n:::\n::::\n", encoding="utf-8")
        self.addCleanup(essay.unlink)
        done = subprocess.run([str(site_binary()), "build"], cwd=self.site,
                              env=script_env(SITE_ROOT=str(self.site), SITE_ENV="build", SITE_THREADS="2"),
                              capture_output=True, text=True, timeout=900)
        self.assertEqual(done.returncode, 0, done.stdout[-3000:] + done.stderr[-3000:])
        index = (self.site / "_site" / "bibliography" / "index.html").read_text(encoding="utf-8")
        page = (self.site / "_site/essays/out-of-sight.html").read_text(encoding="utf-8")
        for fragment in ("refs", "target"):
            self.assertIn(f'href="#{fragment}"', page)
            self.assertEqual(page.count(f'id="{fragment}"'), 1)
        self.assertIn('id="ref-zeta1980"', page)
        self.assertIn('id="ref-zeta1980"', index)
        self.assertIn("ref-beta2010", index)
        self.assertNotIn("epsilon2030", index)
        self.assertNotIn("Cited Out of Sight", index)


if __name__ == "__main__":
    unittest.main()
