"""tools/archive.py's pure functions: slugs, URL equivalence, link-rot
hysteresis, .bib citations, snapshot grading (audit T14). The network probe
has its own tests in test_archive_probe.py."""

import datetime
import json
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock
from tests._helpers import load_tool, requires_cabal, site_binary

archive = load_tool("archive.py", "archive_tool")

try:
    import bs4  # noqa: F401
    HAVE_BS4 = True
except ImportError:
    HAVE_BS4 = False

DAY = datetime.date(2026, 10, 1)


def days(n):
    return DAY + datetime.timedelta(days=n)


class SlugTests(unittest.TestCase):
    def test_domain_label_and_path_tail(self):
        self.assertEqual(archive.derive_slug("https://www.example.com/papers/Foo_Bar.pdf"),
                         "example-foo-bar-pdf")

    def test_trailing_slash_and_bare_host(self):
        self.assertEqual(archive.derive_slug("https://blog.example.org/post/"), "example-post")
        self.assertEqual(archive.derive_slug("https://example.org/"), "example-index")

    def test_truncated_without_a_trailing_hyphen(self):
        slug = archive.derive_slug("https://example.com/" + "word-" * 30)
        self.assertLessEqual(len(slug), 64)
        self.assertFalse(slug.endswith("-"))

    def test_nothing_sluggable_falls_back_to_a_hash(self):
        slug = archive.derive_slug("https://例え.テスト/記事")
        self.assertRegex(slug, r"^[0-9a-f]{12}$")

    def test_manifest_slug_wins(self):
        self.assertEqual(archive.entry_slug({"url": "https://example.com/x", "slug": "mine"}), "mine")
        self.assertEqual(archive.entry_slug({"url": "https://example.com/x"}), "example-x")

    def test_every_derived_slug_is_a_valid_one(self):
        for url in ("https://www.example.com/papers/Foo_Bar.pdf", "https://example.org/",
                    "https://example.com/" + "word-" * 30, "https://例え.テスト/記事"):
            self.assertRegex(archive.derive_slug(url), archive.SLUG_RE)

    def test_a_manifest_that_is_not_yaml_is_named(self):
        with tempfile.TemporaryDirectory() as d:
            manifest = Path(d) / "manifest.yaml"
            manifest.write_text("- url: [unclosed\n")
            with self.assertRaises(SystemExit), mock.patch.object(archive, "err") as err:
                archive.load_yaml_list(manifest)
        self.assertIn("manifest.yaml: not valid YAML", err.call_args[0][0])

    def test_aliases_must_be_http_urls(self):
        self.assertEqual(archive.entry_aliases({"url": "u", "aliases": ["https://doi.org/1"]}),
                         ["https://doi.org/1"])
        self.assertEqual(archive.entry_aliases({"url": "u"}), [])
        for bad in ("https://doi.org/1", ["doi:10.1/x"], [3]):
            with self.assertRaises(SystemExit), mock.patch.object(archive, "err"):
                archive.entry_aliases({"url": "u", "aliases": bad})


# Each a case where the two normalizers once disagreed (2026-10-04), or a
# boundary of the rules: re-encoding, bare and empty parameters, scheme
# case, old-style arXiv ids, an empty or version-only arXiv id.
PARITY_URLS = (
    "http://e.com/a/?utm_medium=m#sec",
    "https://e.com/s?q=a%20b&utm_source=x",
    "https://e.com/s?q=a+b",
    "https://e.com/s?flag&utm_source=x",
    "https://e.com/s?",
    "https://e.com/s?a=1&&b=2",
    "https://e.com/s?q=%7Euser",
    "HTTP://E.com/a?utm_source=x",
    "https://e.com/x?ref=hn&id=7",
    "https://arxiv.org/abs/hep-th/9901001v2",
    "https://arxiv.org/pdf/math.GT/0309136v1.pdf",
    "https://arxiv.org/pdf/2401.01234v3.pdf",
    "http://arxiv.org/abs/2401.01234",
    "https://arxiv.org/abs/2401.01234?context=cs",
    "https://arxiv.org/abs/2401.01234v2?context=cs",
    "https://arxiv.org/abs/",
    "https://arxiv.org/abs/v2",
    "https://arxiv.org/list/cs.CR/recent",
)


@requires_cabal
class NormalizationParityTests(unittest.TestCase):
    """archive.normalize_url against the build's ArchiveIndex.normalizeUrl
    (`site normalize-url`): removal enforcement, duplicate detection and
    `suggest` here must call the same URLs equal that link annotation does."""

    @classmethod
    def setUpClass(cls):
        cls.binary = str(site_binary())

    def test_tracking_parameters_are_the_builds(self):
        done = subprocess.run([self.binary, "shared-rules"], capture_output=True,
                              text=True, check=True)
        self.assertEqual(sorted(archive.TRACKING_PARAMS),
                         sorted(json.loads(done.stdout)["archive-tracking-params"]))

    def test_normalize_url_is_the_builds(self):
        urls = list(PARITY_URLS) + [f"https://e.com/a?{p}=x&keep=1" for p in sorted(archive.TRACKING_PARAMS)]
        done = subprocess.run([self.binary, "normalize-url"], input="\n".join(urls) + "\n",
                              capture_output=True, text=True, check=True)
        built = done.stdout.splitlines()
        self.assertEqual(len(built), len(urls))
        for url, want in zip(urls, built):
            with self.subTest(url=url):
                self.assertEqual(archive.normalize_url(url), want)


class UrlEquivalenceTests(unittest.TestCase):
    def test_tracking_parameters_go_and_others_stay(self):
        self.assertEqual(archive.strip_tracking("https://e.com/a?utm_source=x&v=2&fbclid=y"),
                         "https://e.com/a?v=2")
        self.assertEqual(archive.strip_tracking("https://e.com/a"), "https://e.com/a")

    def test_normalize_url(self):
        self.assertEqual(archive.normalize_url("http://e.com/a/?utm_medium=m#sec"), "https://e.com/a")
        self.assertEqual(archive.normalize_url("https://arxiv.org/pdf/2401.01234v3.pdf"),
                         "https://arxiv.org/abs/2401.01234")
        self.assertEqual(archive.normalize_url("https://e.com/a?id=7"), "https://e.com/a?id=7")

    def test_arxiv_forms(self):
        forms = archive.arxiv_aliases("https://arxiv.org/abs/2401.01234v2")
        for f in ("https://arxiv.org/abs/2401.01234", "https://arxiv.org/abs/2401.01234v2",
                  "https://arxiv.org/pdf/2401.01234", "https://arxiv.org/pdf/2401.01234v2.pdf"):
            self.assertIn(f, forms)
        self.assertEqual(archive.arxiv_aliases("https://example.com/abs/1"), set())

    def test_url_aliases_fold_scheme_and_slash_but_omit_the_key(self):
        url = "https://e.com/a/?ref=x"
        aliases = archive.url_aliases(url)
        self.assertNotIn(url, aliases)
        for a in ("http://e.com/a/?ref=x", "https://e.com/a/", "https://e.com/a", "http://e.com/a"):
            self.assertIn(a, aliases)

    def test_moved_meaningfully(self):
        self.assertFalse(archive.moved_meaningfully("http://e.com/a/", "https://e.com/a#top"))
        self.assertTrue(archive.moved_meaningfully("https://e.com/a", "https://e.com/b"))

    def test_wayback_raw_url(self):
        self.assertEqual(archive.wayback_raw_url("https://web.archive.org/web/20200101000000/https://e.com/"),
                         "https://web.archive.org/web/20200101000000id_/https://e.com/")
        self.assertEqual(archive.wayback_raw_url("https://e.com/"), "https://e.com/")


class LinkRotHysteresisTests(unittest.TestCase):
    def fail_streak(self, n, every=7):
        state = {"status": "live", "status-since": DAY.isoformat()}
        for i in range(n):
            state = archive.next_state(state, "fail", None, days(i * every))
        return state

    def test_one_failure_is_only_an_error(self):
        s = self.fail_streak(1)
        self.assertEqual((s["status"], s["consecutive-failures"]), ("error", 1))

    def test_rotting_needs_enough_failures_and_enough_days(self):
        self.assertEqual(self.fail_streak(archive.ROT_FAILS)["status"], "rotted")
        quick = self.fail_streak(archive.ROT_FAILS, every=1)
        self.assertEqual(quick["status"], "error", "three failures in two days are not rot")
        self.assertEqual(self.fail_streak(archive.ROT_FAILS - 1, every=30)["status"], "error")

    def test_the_streak_starts_at_its_first_failure(self):
        s = self.fail_streak(2)
        self.assertEqual(s["status-since"], DAY.isoformat())

    def test_recovery_is_immediate(self):
        s = archive.next_state(self.fail_streak(5), "ok", None, days(60))
        self.assertEqual((s["status"], s["consecutive-failures"], s["status-since"]),
                         ("live", 0, days(60).isoformat()))

    def test_a_live_link_keeps_its_since_date(self):
        s = archive.next_state({"status": "live", "status-since": "2025-01-01"}, "ok", None, DAY)
        self.assertEqual(s["status-since"], "2025-01-01")

    def test_moved_records_where_to(self):
        s = archive.next_state({"status": "live"}, "moved", "https://e.com/b", DAY)
        self.assertEqual((s["status"], s["new-url"]), ("moved", "https://e.com/b"))


class BibCitationTests(unittest.TestCase):
    BIB = """
@comment{ url = {https://ignored.example/} }
@article{Smith2020,
  title = {A},
  doi = {10.1000/xyz},
  url = {https://journal.example/a},
}
@inproceedings{Doe2021,
  doi = "10.1000/abc",
}
@misc{Local,
  url = {file:///tmp/x},
}
@book{NoLink, title = {B}}
"""

    def test_url_wins_doi_resolves_and_the_rest_is_skipped(self):
        self.assertEqual(archive.bib_citations(self.BIB),
                         [("Smith2020", "https://journal.example/a"),
                          ("Doe2021", "https://doi.org/10.1000/abc")])


class ManifestPreScanTests(unittest.TestCase):
    """Manifest errors are refused before any fetch."""

    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix="archive-manifest-"))
        self.addCleanup(shutil.rmtree, self.tmp)
        (self.tmp / "removed.yaml").write_text("[]\n")
        patches = [mock.patch.object(archive, "REMOVED", self.tmp / "removed.yaml"),
                   mock.patch.object(archive, "err")]
        for p in patches:
            p.start()
            self.addCleanup(p.stop)

    def refused(self, entries):
        import yaml
        manifest = self.tmp / "manifest.yaml"
        manifest.write_text(yaml.safe_dump(entries))
        with mock.patch.object(archive, "MANIFEST", manifest), \
             mock.patch.object(archive, "fetch_pdf", side_effect=AssertionError("fetched")), \
             mock.patch.object(archive, "fetch_html", side_effect=AssertionError("fetched")):
            with self.assertRaises(SystemExit):
                archive.cmd_fetch()

    def test_two_hosts_deriving_one_slug(self):
        self.refused([{"url": "https://alice.github.io/"}, {"url": "https://bob.github.io/"}])

    def test_a_slug_override_that_leaves_archive(self):
        self.refused([{"url": "https://example.com/x", "slug": "../x"}])

    def test_the_live_manifest_passes(self):
        import yaml
        entries = yaml.safe_load((archive.MANIFEST).read_text()) or []
        slugs = [archive.entry_slug(e) for e in entries if e.get("url")]
        self.assertEqual(len(slugs), len(set(slugs)))
        for slug in slugs:
            self.assertRegex(slug, archive.SLUG_RE)


@unittest.skipUnless(HAVE_BS4, "beautifulsoup4 not installed")
class SnapshotTests(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix="archive-"))
        self.addCleanup(shutil.rmtree, self.tmp)

    def page(self, body, head=""):
        p = self.tmp / "snapshot.html"
        p.write_text(f"<html><head>{head}</head><body>{body}</body></html>", encoding="utf-8")
        return p

    def test_grades(self):
        prose = "<p>" + "Words of an archived article. " * 20 + "</p>"
        self.assertEqual(archive.classify_snapshot(self.page(prose)), "ok")
        self.assertEqual(archive.classify_snapshot(self.page(prose + '<img src="https://cdn.example/a.png">')),
                         "degraded")
        self.assertEqual(archive.classify_snapshot(self.page(prose + '<img data-src="/a.png">')), "degraded")
        self.assertEqual(archive.classify_snapshot(self.page("<div id=app></div><script>" + "x" * 500 + "</script>")),
                         "js-required")

    def test_noarchive_meta(self):
        self.assertTrue(archive.body_noarchive(self.page("", '<meta name="robots" content="noindex, NOARCHIVE">')))
        self.assertFalse(archive.body_noarchive(self.page("", '<meta name="robots" content="noindex">')))


if __name__ == "__main__":
    unittest.main()
