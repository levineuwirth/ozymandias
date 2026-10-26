#!/usr/bin/env python3
"""tools/sitediff.py reports every difference between two built sites that
no allowlist entry permits, and nothing it is told never to compare.

Run with: ``python3 -m unittest tests.test_sitediff``.
"""

from __future__ import annotations

import contextlib
import io
import tempfile
import unittest
from pathlib import Path
from tests._helpers import load_tool

sitediff = load_tool("sitediff.py", register=True)

PAGE = ('<html><head><title>T</title><link rel="stylesheet" href="/css/a.css">'
        '</head><body><p>Body</p></body></html>\n')


class SiteDiff(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.base, self.new = self.root / "base", self.root / "new"
        for site in (self.base, self.new):
            self.write(site, "index.html", PAGE)
            self.write(site, "css/a.css", "p{color:red}")
            self.write(site, "images/x.png", b"\x89PNG\x00\x01")

    def write(self, site: Path, rel: str, data: str | bytes) -> None:
        path = site / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        if isinstance(data, str):
            path.write_text(data, encoding="utf-8")
        else:
            path.write_bytes(data)

    def allow(self, text: str) -> list:
        path = self.root / "allow.yaml"
        path.write_text(text, encoding="utf-8")
        return sitediff.load_allowlist(path)

    def run_main(self, *args: str) -> tuple[int, str]:
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            code = sitediff.main([str(self.base), str(self.new), *args])
        return code, out.getvalue()

    def test_identical_sites_are_clean(self):
        result = sitediff.compare(self.base, self.new)
        self.assertTrue(result.clean)
        self.assertEqual((result.base_count, result.new_count), (3, 3))
        self.assertEqual(self.run_main()[0], 0)

    def test_changed_added_and_removed_files_are_reported(self):
        self.write(self.new, "css/a.css", "p{color:blue}")
        self.write(self.new, "only-new.html", PAGE)
        self.write(self.base, "only-base.html", PAGE)
        result = sitediff.compare(self.base, self.new)
        self.assertEqual(sorted(result.not_permitted), [
            ("added", "only-new.html"), ("changed", "css/a.css"), ("removed", "only-base.html")])
        code, out = self.run_main()
        self.assertEqual(code, 1)
        self.assertIn("- p{color:red}", out)
        self.assertIn("+ p{color:blue}", out)

    def test_a_binary_change_is_a_change(self):
        self.write(self.new, "images/x.png", b"\x89PNG\x00\x02")
        self.assertEqual(sitediff.compare(self.base, self.new).not_permitted,
                         [("changed", "images/x.png")])

    def test_sidecars_index_and_telemetry_are_never_compared(self):
        for rel in ("index.html.gz", "index.html.br", "index.html.sig",
                    "pagefind/fragment/x.pf_fragment", "build/index.html", "build/time.txt"):
            self.write(self.new, rel, "only here")
        self.assertTrue(sitediff.compare(self.base, self.new).clean)

    def test_a_directory_named_build_below_the_root_is_compared(self):
        self.write(self.new, "essays/build/index.html", PAGE)
        self.assertEqual(sitediff.compare(self.base, self.new).not_permitted,
                         [("added", "essays/build/index.html")])

    def test_the_source_mirror_only_on_request(self):
        self.write(self.new, "source/build/Site.hs", "module Site")
        self.assertTrue(sitediff.compare(self.base, self.new).clean)
        self.assertEqual(sitediff.compare(self.base, self.new, include_source=True).not_permitted,
                         [("added", "source/build/Site.hs")])

    def test_body_only_ignores_the_head_of_pages(self):
        self.write(self.new, "index.html", PAGE.replace("/css/a.css", "/css/b.css"))
        self.assertFalse(sitediff.compare(self.base, self.new).clean)
        self.assertTrue(sitediff.compare(self.base, self.new, body_only=True).clean)

    def test_body_only_still_sees_the_body(self):
        self.write(self.new, "index.html", PAGE.replace("<p>Body</p>", "<p>Body!</p>"))
        self.assertEqual(sitediff.compare(self.base, self.new, body_only=True).not_permitted,
                         [("changed", "index.html")])

    def test_body_only_compares_other_files_whole(self):
        self.write(self.new, "css/a.css", "p{color:blue}")
        self.assertEqual(sitediff.compare(self.base, self.new, body_only=True).not_permitted,
                         [("changed", "css/a.css")])

    def test_a_path_entry_permits_a_change(self):
        self.write(self.new, "css/a.css", "p{color:blue}")
        entries = self.allow('- path: "css/*.css"\n  reason: palette\n')
        result = sitediff.compare(self.base, self.new, entries=entries)
        self.assertTrue(result.clean)
        self.assertEqual(result.permitted, [("changed", "css/a.css")])
        self.assertEqual(entries[0].used, 1)

    def test_star_matches_across_directories(self):
        self.write(self.new, "a/b/c.html", PAGE)
        entries = self.allow('- path: "*.html"\n  change: added\n  reason: new pages\n')
        self.assertTrue(sitediff.compare(self.base, self.new, entries=entries).clean)

    def test_the_change_kind_limits_an_entry(self):
        self.write(self.new, "css/a.css", "p{color:blue}")
        entries = self.allow('- path: "css/*"\n  change: added\n  reason: new sheets\n')
        self.assertEqual(sitediff.compare(self.base, self.new, entries=entries).not_permitted,
                         [("changed", "css/a.css")])

    def test_a_pattern_entry_permits_differences_inside_its_matches_only(self):
        tagged = PAGE.replace("</head>", '<meta name="author" content="A"></head>')
        self.write(self.new, "index.html", tagged)
        entries = self.allow("- path: \"*.html\"\n  pattern: '<meta name=\"author\" content=\"[^\"]*\">'\n"
                             "  reason: identity meta tag\n")
        self.assertTrue(sitediff.compare(self.base, self.new, entries=entries).clean)
        self.write(self.new, "index.html", tagged.replace("<p>Body</p>", "<p>Other</p>"))
        self.assertEqual(sitediff.compare(self.base, self.new, entries=entries).not_permitted,
                         [("changed", "index.html")])

    def test_pattern_entries_cover_one_file_together(self):
        changed = PAGE.replace("<title>T</title>", "<title>T2</title>").replace("Body", "Corps")
        self.write(self.new, "index.html", changed)
        entries = self.allow("- path: \"*.html\"\n  pattern: '<title>[^<]*</title>'\n  reason: titles\n"
                             "- path: \"*.html\"\n  pattern: '(Body|Corps)'\n  reason: wording\n")
        self.assertTrue(sitediff.compare(self.base, self.new, entries=entries).clean)
        self.assertEqual([e.used for e in entries], [1, 1])

    def test_an_entry_needed_by_no_difference_stays_unused(self):
        self.write(self.new, "index.html", PAGE.replace("Body", "Corps"))
        entries = self.allow("- path: \"*.html\"\n  pattern: '<title>[^<]*</title>'\n  reason: titles\n"
                             "- path: \"*.html\"\n  pattern: '(Body|Corps)'\n  reason: wording\n")
        self.assertTrue(sitediff.compare(self.base, self.new, entries=entries).clean)
        self.assertEqual([e.used for e in entries], [0, 1])

    def test_patterns_match_the_original_bytes_not_each_others_leftovers(self):
        # Removing X from "aXb" leaves "ab", which the second pattern would
        # then remove: the unmatched a and b must still count as a change.
        self.write(self.base, "index.html", "<body>aXb</body>")
        self.write(self.new, "index.html", "<body></body>")
        entries = self.allow("- path: \"*.html\"\n  pattern: 'X'\n  reason: x\n"
                             "- path: \"*.html\"\n  pattern: 'ab'\n  reason: ab\n")
        self.assertEqual(sitediff.compare(self.base, self.new, entries=entries).not_permitted,
                         [("changed", "index.html")])

    def test_matching_unchanged_content_is_not_covering_a_change(self):
        # Only the author changes. The title entry matches, on both sides, a
        # title that did not change: it covered nothing, so --strict fails.
        # The meta tag and the title are adjacent, so their spans touch.
        page = '<head><meta name="author" content="A"><title>T</title></head><body></body>\n'
        self.write(self.base, "index.html", page)
        self.write(self.new, "index.html", page.replace('content="A"', 'content="B"'))
        path = self.root / "allow.yaml"
        path.write_text(
            "- path: \"*.html\"\n  pattern: '<meta name=\"author\" content=\"[^\"]*\">'\n  reason: author\n"
            "- path: \"*.html\"\n  pattern: '<meta [^>]*>'\n  reason: any meta tag\n"
            "- path: \"*.html\"\n  pattern: '<title>[^<]*</title>'\n  reason: titles\n",
            encoding="utf-8")
        entries = sitediff.load_allowlist(path)
        result = sitediff.compare(self.base, self.new, entries=entries)
        self.assertTrue(result.clean)
        self.assertEqual([e.used for e in entries], [1, 1, 0])
        code, out = self.run_main("--allow", str(path), "--strict")
        self.assertEqual(code, 1)
        self.assertIn("unused:", out)

    def test_a_match_beside_an_inserted_one_is_not_covering_it(self):
        # The new meta tag lands right against the unchanged title: the two
        # spans touch, and only the inserted one is a change.
        self.write(self.base, "index.html", "<head><title>T</title></head>")
        self.write(self.new, "index.html", '<head><meta name="a"><title>T</title></head>')
        entries = self.allow("- path: \"*.html\"\n  pattern: '<meta [^>]*>'\n  reason: meta\n"
                             "- path: \"*.html\"\n  pattern: '<title>[^<]*</title>'\n  reason: titles\n")
        self.assertTrue(sitediff.compare(self.base, self.new, entries=entries).clean)
        self.assertEqual([e.used for e in entries], [1, 0])

    def test_overlapping_entries_are_named_for_the_change_they_share(self):
        self.write(self.base, "index.html", '<body><meta name="author" content="A"></body>')
        self.write(self.new, "index.html", '<body><meta name="author" content="B"></body>')
        entries = self.allow(
            "- path: \"*.html\"\n  pattern: '<meta name=\"author\" content=\"[^\"]*\">'\n  reason: author\n"
            "- path: \"*.html\"\n  pattern: '<meta [^>]*>'\n  reason: any meta tag\n")
        result = sitediff.compare(self.base, self.new, entries=entries)
        self.assertTrue(result.clean)
        self.assertEqual(result.shared, [("index.html", (entries[0].source, entries[1].source))])

    def test_a_moved_match_is_a_change_inside_the_matches(self):
        self.write(self.base, "index.html", "<body>aXa</body>")
        self.write(self.new, "index.html", "<body>aaX</body>")
        entries = self.allow("- path: \"*.html\"\n  pattern: 'X'\n  reason: x\n")
        self.assertTrue(sitediff.compare(self.base, self.new, entries=entries).clean)
        self.assertEqual(entries[0].used, 1)

    def test_strict_fails_on_an_unused_entry(self):
        path = self.root / "allow.yaml"
        path.write_text('- path: "css/*.css"\n  reason: palette\n', encoding="utf-8")
        code, out = self.run_main("--allow", str(path))
        self.assertEqual(code, 0)
        self.assertIn("unused:", out)
        self.assertEqual(self.run_main("--allow", str(path), "--strict")[0], 1)

    def test_malformed_allowlists_are_refused(self):
        bad = {
            '- path: "*.html"\n': "`reason` is required",
            '- reason: r\n': "`path` is required",
            '- path: "*"\n  reason: r\n  change: moved\n': "`change` must be one of",
            "- path: \"*\"\n  reason: r\n  change: added\n  pattern: 'x'\n": "changed files only",
            "- path: \"*\"\n  reason: r\n  pattern: '('\n": "bad pattern",
            '- path: "*"\n  reason: r\n  why: w\n': "unknown keys",
            'path: "*"\n': "expected a list",
        }
        for text, message in bad.items():
            with self.subTest(text=text):
                with self.assertRaisesRegex(ValueError, message):
                    self.allow(text)

    def test_a_usage_error_exits_2(self):
        with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as raised:
            sitediff.main([str(self.base), str(self.root / "missing")])
        self.assertEqual(raised.exception.code, 2)


if __name__ == "__main__":
    unittest.main()
