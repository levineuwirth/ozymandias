#!/usr/bin/env python3
"""Tests for tools/check-site.py — the post-build artifact gate.

Every case builds a tiny synthetic ``_site`` in a temporary directory, so
nothing here reads or writes the real build output.  The baseline fixture is
deliberately *clean*: each test then introduces exactly one defect and
asserts that the gate notices it and nothing else changes verdict.

Run with: ``make test`` (or ``python3 -m unittest tests.test_check_site``).
"""

from __future__ import annotations

import io
import os
import tempfile
import unittest
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path
from tests._helpers import load_tool

# check-site.py is a hyphenated script, not an importable module name, so it
# is loaded by path rather than by `import`.

check_site = load_tool("check-site.py")


FEED = """<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <title>Test</title>
  <id>https://example.org/feed.xml</id>
  <updated>2026-09-05T00:00:00Z</updated>
  <entry>
    <title>One</title>
    <id>https://example.org/one/</id>
    <updated>2026-09-05T00:00:00Z</updated>
  </entry>
</feed>
"""

SITEMAP = """<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
  <url><loc>https://example.org/</loc></url>
</urlset>
"""

PAGE = """<!doctype html>
<html><body>
<main id="content">
<p>Ordinary prose with a <a href="/essays/one/">link</a>.</p>
<img src="/images/a.jpg" alt="a">
</main>
</body></html>
"""


class SiteFixture:
    """A minimal, passing ``_site`` that each test perturbs."""

    def __init__(self, root: Path) -> None:
        self.root = root
        self.write("index.html", PAGE)
        self.write("essays/one/index.html", "<!doctype html><title>One</title>")
        self.write("404.html", "<!doctype html><title>Not found</title>")
        self.write("feed.xml", FEED)
        self.write("music/feed.xml", FEED)
        self.write("sitemap.xml", SITEMAP)
        self.write("images/a.jpg", "jpeg-bytes")
        self.write("images/a.webp", "webp-bytes")

    def write(self, rel: str, text: str) -> Path:
        path = self.root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")
        return path


class CheckSiteTestCase(unittest.TestCase):
    def setUp(self) -> None:
        self._tmp = tempfile.TemporaryDirectory(prefix="check-site-test-")
        self.addCleanup(self._tmp.cleanup)
        self.root = Path(self._tmp.name) / "_site"
        self.root.mkdir()
        self.site = SiteFixture(self.root)

    def run_gate(self, *extra: str) -> tuple[int, str]:
        """Run the gate over the fixture, returning (exit code, output)."""
        out, err = io.StringIO(), io.StringIO()
        with redirect_stdout(out), redirect_stderr(err):
            code = check_site.main([str(self.root), *extra])
        return code, out.getvalue() + err.getvalue()

    # -- baseline ----------------------------------------------------------

    def test_clean_site_passes(self):
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)
        self.assertIn("check-site: OK", output)

    def test_missing_directory_is_usage_error(self):
        out, err = io.StringIO(), io.StringIO()
        with redirect_stdout(out), redirect_stderr(err):
            code = check_site.main([str(self.root / "nope")])
        self.assertEqual(code, 2)

    # -- S01: publication boundary ----------------------------------------

    def test_logs_and_interrupted_files_fail_including_transport_sidecars(self):
        for name in ("build.log", "snapshot.partial", "build.log.br", "build.log.gz.sig"):
            with self.subTest(name=name):
                path = self.site.write("essays/one/" + name, "private build output")
                try:
                    code, output = self.run_gate()
                    self.assertEqual(code, 1, output)
                    self.assertIn(name, output)
                finally:
                    path.unlink()

    def test_private_note_fails(self):
        self.site.write("essays/private.local.html", PAGE)
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("essays/private.local.html", output)

    def test_credential_shaped_file_fails(self):
        self.site.write("deploy.key", "not-a-real-key")
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("deploy.key", output)

    def test_planning_checklist_fails_even_under_source_mirror(self):
        self.site.write("source/checklist.md", "# local planning notes")
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("source/checklist.md", output)

    def test_compressed_sidecar_of_private_file_fails(self):
        # compress-assets.sh would make one of these for any published file.
        self.site.write("notes.local.md.gz", "compressed")
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("notes.local.md.gz", output)

    def test_pycache_directory_fails(self):
        self.site.write("source/tools/__pycache__/embed.cpython-313.pyc", "x")
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("__pycache__", output)

    def test_editor_and_temporary_debris_fails(self):
        self.site.write("essays/one.html~", PAGE)
        self.site.write("images/b.webp.part", "partial")
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("one.html~", output)
        self.assertIn("b.webp.part", output)

    # -- B08: drafts -------------------------------------------------------

    def test_drafts_directory_fails(self):
        self.site.write("drafts/essays/wip.html", PAGE)
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("drafts/", output)

    def test_draft_href_in_corpus_fails(self):
        self.site.write(
            "essays/index.html",
            '<html><body><a href="/drafts/essays/wip/">WIP</a></body></html>',
        )
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("links into /drafts/", output)

    def test_draft_string_in_source_mirror_is_ignored(self):
        # The published Makefile copy legitimately mentions _site/drafts.
        self.site.write(
            "source/templates/default.html",
            '<html><body><a href="/drafts/x/">$body$</a></body></html>',
        )
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    # -- B03: render failures ---------------------------------------------

    def test_visualization_error_block_fails(self):
        self.site.write(
            "essays/figure.html",
            '<html><body><div class="viz-error"><strong>Visualization '
            "error:</strong> ModuleNotFoundError</div></body></html>",
        )
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("figure/visualization script failed", output)

    def test_score_error_block_fails(self):
        self.site.write(
            "music/piece.html",
            '<html><body><figure class="score-fragment '
            'score-fragment--error"><div class="score-fragment-error">'
            "Missing score: scores/x.svg</div></figure></body></html>",
        )
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("score fragment missing", output)

    def test_raw_citation_syntax_fails(self):
        self.site.write("essays/one/index.html",
                        '<!doctype html><title>One</title><p>as in '
                        '<span class="citation" data-cites="k">[@k, Theorem 3.1]</span></p>')
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("citation shows its source syntax", output)

    def test_unresolved_figure_reference_fails(self):
        self.site.write(
            "essays/refs.html",
            '<html><body><p>See <a href="#fig-typo">Figure ?</a>.</p>'
            "</body></html>",
        )
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("unresolved figure cross-reference", output)

    def test_resolved_figure_reference_passes(self):
        self.site.write(
            "essays/refs.html",
            '<html><body><figure id="fig-1"></figure>'
            '<p>See <a href="#fig-1">Figure 1</a>.</p></body></html>',
        )
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    # -- missing media -----------------------------------------------------

    def test_missing_img_src_fails(self):
        self.site.write(
            "essays/missing.html",
            '<html><body><img src="/images/gone.jpg" alt=""></body></html>',
        )
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("/images/gone.jpg", output)

    def test_missing_srcset_candidate_fails(self):
        self.site.write(
            "essays/missing.html",
            "<html><body><picture>"
            '<source srcset="/images/a.webp 1x, /images/a-2x.webp 2x" '
            'type="image/webp">'
            '<img src="/images/a.jpg" alt=""></picture></body></html>',
        )
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("/images/a-2x.webp", output)

    def test_relative_and_remote_image_urls_resolve(self):
        self.site.write("essays/rel/img.png", "png")
        self.site.write(
            "essays/rel/index.html",
            "<html><body>"
            '<img src="img.png" alt="">'
            '<img src="https://example.org/remote.png" alt="">'
            '<img src="data:image/gif;base64,R0lGOD" alt="">'
            "</body></html>",
        )
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    def test_percent_encoded_image_path_resolves(self):
        self.site.write("images/a b.jpg", "jpeg")
        self.site.write(
            "essays/enc.html",
            '<html><body><img src="/images/a%20b.jpg" alt=""></body></html>',
        )
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    # -- links and anchors -------------------------------------------------

    def link_page(self, body: str, rel: str = "essays/links.html") -> None:
        self.site.write(rel, f"<html><body>{body}</body></html>")

    def test_link_to_a_missing_page_fails(self):
        # The 2026-10-01 near miss: a published page linking to an essay
        # still in drafts.
        self.link_page('<a href="/essays/eighty-witnesses/">companion</a>')
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("link-targets", output)
        self.assertIn("/essays/eighty-witnesses/", output)

    def test_links_resolve_the_way_nginx_try_files_does(self):
        self.site.write("about.html", "<html></html>")
        self.link_page(
            '<a href="/essays/one/">dir</a>'
            '<a href="/essays/one">dir without slash</a>'
            '<a href="/essays/one/index.html">explicit</a>'
            '<a href="/about">extensionless</a>'
            '<a href="../essays/one/">relative</a>'
            '<a href="/">root</a>'
        )
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    def test_missing_stylesheet_and_script_fail(self):
        self.link_page(
            '<link rel="stylesheet" href="/css/gone.css">'
            '<script src="/js/gone.js"></script>'
        )
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("<link> /css/gone.css", output)
        self.assertIn("<script> /js/gone.js", output)

    def test_external_pseudo_scheme_and_server_routes_are_skipped(self):
        self.link_page(
            '<a href="https://example.org/x">ext</a>'
            '<a href="//cdn.example.org/x.js">protocol-relative</a>'
            '<a href="mailto:a@example.org">mail</a>'
            '<a href="javascript:void(0)">js</a>'
            '<link rel="icon" href="data:image/svg+xml,%3Csvg%3E">'
            '<a href="/proxy/arxiv/abs/2401.00001">proxied</a>'
            '<a href="#">empty fragment</a><a href="#top">top</a>'
        )
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    def test_query_entities_and_percent_encoding_resolve(self):
        self.site.write("files/a b.pdf", "pdf")
        self.link_page(
            '<a href="/essays/one/?x=1&amp;y=2">query</a>'
            '<a href="/files/a%20b.pdf">encoded</a>'
        )
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    def test_signature_links_pass_before_signing(self):
        # The gate runs before sign-site.sh writes any .sig.
        self.link_page('<a href="./links.html.sig">sig</a>'
                       '<a href="/essays/one/index.html.sig">sig</a>')
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    def test_signature_of_a_missing_file_fails(self):
        self.link_page('<a href="/essays/gone.html.sig">sig</a>')
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("/essays/gone.html.sig", output)

    def test_link_climbing_out_of_the_site_fails(self):
        self.link_page('<a href="../../../etc/passwd">out</a>')
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("../../../etc/passwd", output)

    def test_data_attributes_are_not_links(self):
        self.link_page('<a href="/essays/one/" data-href="#nothing" '
                       'data-src="/not/a/file">x</a>')
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    def test_missing_same_page_fragment_fails(self):
        # The citation defect: a bibliography back-link to the first
        # citation's anchor, which the page never emitted.
        self.link_page(
            '<sup id="cite-back-1-2">[1]</sup>'
            '<a class="ref-num" href="#cite-back-1">↩</a>'
        )
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("no #cite-back-1 on this page", output)

    def test_missing_fragment_on_another_page_fails(self):
        self.link_page('<a href="/essays/one/#thm-1">Theorem 1</a>')
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("no #thm-1 on essays/one/index.html", output)

    def test_fragments_match_ids_and_named_anchors(self):
        self.site.write(
            "essays/one/index.html",
            '<html><body><div id="thm-1"></div><a name="legacy"></a>'
            '<span id="caf&#233;"></span></body></html>',
        )
        self.link_page(
            '<a href="/essays/one/#thm-1">id</a>'
            '<a href="/essays/one/#legacy">name</a>'
            '<a href="/essays/one/#caf%C3%A9">encoded</a>'
            '<a href="/essays/one/#:~:text=One">text fragment</a>'
        )
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    def test_fragment_on_a_non_html_target_is_not_checked(self):
        self.site.write("papers/p.pdf", "pdf")
        self.link_page('<a href="/papers/p.pdf#page=3">page 3</a>')
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    def test_source_mirror_templates_are_not_link_checked(self):
        self.site.write(
            "source/templates/essay.html",
            '<html><body><a href="$url$">$title$</a>'
            '<a href="#cite-back-1">x</a></body></html>',
        )
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    # -- alt text ---------------------------------------------------------

    def test_img_without_alt_fails(self):
        # What Pandoc writes for ![](x.png): a screen reader reads the
        # file name.
        self.link_page('<p><img src="/images/a.jpg" loading="lazy" /></p>')
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("img-alt", output)
        self.assertIn("{.decorative}", output)

    def test_empty_bare_and_described_alt_pass(self):
        self.link_page(
            '<img src="/images/a.jpg" alt="">'
            '<img src="/images/a.jpg" alt decoding="async">'
            '<img src="/images/a.jpg" alt="A described image">'
            '<picture><source srcset="/images/a.webp" type="image/webp">'
            '<img src="/images/a.jpg" alt=""></picture>'
        )
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    def test_img_in_a_comment_or_script_is_not_an_image(self):
        # A template comment inside a loop ships its literal "<img>" once
        # per item; a script may build markup in a string.
        self.link_page(
            '<!-- one <img> serves all three grid modes -->'
            '<script>const t = "<img src=x>";</script>'
        )
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    # -- plugin embeds ----------------------------------------------------

    def test_pdf_used_as_image_fails(self):
        # What Pandoc writes for ![caption](figures/x.pdf).
        self.site.write("essays/one/figures/x.pdf", "%PDF-1.4")
        self.link_page('<figure><embed src="/essays/one/figures/x.pdf" /></figure>')
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("plugin-embeds", output)
        self.assertIn("/essays/one/figures/x.pdf", output)

    def test_object_fails(self):
        self.link_page('<object data="/images/a.jpg" type="image/jpeg"></object>')
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("plugin-embeds", output)

    def test_embed_in_a_comment_or_script_is_not_an_embed(self):
        self.link_page(
            '<!-- no <embed> here -->'
            '<script>const t = "<object data=x>";</script>'
        )
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    # -- score pages ------------------------------------------------------

    def score_reader(self, *pages: int) -> None:
        listed = ",".join(f"/music/w/scores/page-{i}.svg" for i in pages)
        self.site.write("music/w/score/index.html", f'<div data-pages="{listed}"></div>')

    def test_published_page_the_reader_does_not_list_fails(self):
        self.score_reader(1, 2)
        for i in (1, 2, 3):
            self.site.write(f"music/w/scores/page-{i}.svg", "<svg/>")
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("page-3.svg is published but not in the reader", output)

    def test_listed_page_that_is_missing_fails(self):
        self.score_reader(1, 2)
        self.site.write("music/w/scores/page-1.svg", "<svg/>")
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("lists page-2.svg, which is missing", output)

    def test_reader_and_pages_agree(self):
        self.score_reader(1, 2)
        for i in (1, 2):
            self.site.write(f"music/w/scores/page-{i}.svg", "<svg/>")
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    # -- photo GPS --------------------------------------------------------

    @staticmethod
    def jpeg(*ifd0_tags: int, endian: str = "<") -> bytes:
        """SOI, an APP1 Exif segment whose IFD0 holds the given tags, EOI."""
        import struct
        order = b"II" if endian == "<" else b"MM"
        entries = b"".join(struct.pack(endian + "HHII", t, 4, 1, 0) for t in ifd0_tags)
        tiff = order + struct.pack(endian + "HI", 42, 8) + struct.pack(endian + "H", len(ifd0_tags)) \
            + entries + struct.pack(endian + "I", 0)
        body = b"Exif\0\0" + tiff
        return b"\xff\xd8" + b"\xff\xe1" + struct.pack(">H", len(body) + 2) + body + b"\xff\xd9"

    def write_bytes(self, rel: str, data: bytes) -> None:
        path = self.root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(data)

    def test_photo_with_gps_fails(self):
        for endian in ("<", ">"):
            with self.subTest(endian=endian):
                self.write_bytes("photography/s/a.jpg", self.jpeg(0x010F, 0x8825, endian=endian))
                code, output = self.run_gate()
                self.assertEqual(code, 1)
                self.assertIn("photo-gps", output)

    def test_photo_without_gps_passes(self):
        self.write_bytes("photography/s/a.jpg", self.jpeg(0x010F, 0x0112))
        self.write_bytes("photography/s/b.jpg", b"\xff\xd8\xff\xd9")
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    # -- search model -----------------------------------------------------

    def test_semantic_search_without_its_model_fails(self):
        self.site.write("js/semantic-search.js", "// search")
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("search-model", output)
        self.assertIn("download-model.sh", output)

    def test_semantic_search_with_its_model_passes(self):
        self.site.write("js/semantic-search.js", "// search")
        for rel in check_site.SEARCH_MODEL_FILES:
            self.site.write(f"models/{rel}", "x")
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    # -- feeds and sitemap -------------------------------------------------

    def test_non_rfc3339_feed_updated_fails(self):
        self.site.write("music/feed.xml", FEED.replace(
            "<updated>2026-09-05T00:00:00Z</updated>",
            "<updated>Unknown</updated>",
            1,
        ))
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("not RFC 3339", output)

    def test_malformed_feed_fails(self):
        self.site.write("feed.xml", "<feed><unclosed>")
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("not well-formed", output)

    def test_missing_feed_fails(self):
        os.unlink(self.root / "music" / "feed.xml")
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("feed:music/feed.xml", output)

    def test_malformed_sitemap_fails(self):
        self.site.write("sitemap.xml", "<urlset><url>")
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("sitemap.xml is not well-formed", output)

    def test_offset_timezone_updated_is_accepted(self):
        self.site.write("feed.xml", FEED.replace(
            "2026-09-05T00:00:00Z", "2026-09-05T00:00:00+02:00"
        ))
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    # -- 404 ---------------------------------------------------------------

    def test_missing_404_fails_by_default(self):
        os.unlink(self.root / "404.html")
        code, output = self.run_gate()
        self.assertEqual(code, 1)
        self.assertIn("404.html is missing", output)

    def test_missing_404_is_a_warning_with_flag(self):
        os.unlink(self.root / "404.html")
        code, output = self.run_gate("--allow-missing-404")
        self.assertEqual(code, 0, output)
        self.assertIn("WARNINGS", output)
        self.assertIn("404.html is missing", output)

    # -- duplicate ids (warning only) --------------------------------------

    def test_duplicate_ids_warn_but_do_not_fail(self):
        self.site.write(
            "build/index.html",
            '<html><body><div id="content"><main id="content">x</main>'
            "</div></body></html>",
        )
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)
        self.assertIn("duplicate-ids", output)
        self.assertIn("content", output)

    # -- webp --------------------------------------------------------------

    def test_zero_webp_warns_by_default(self):
        os.unlink(self.root / "images" / "a.webp")
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)
        self.assertIn("missing .webp companion", output)

    def test_zero_webp_fails_with_require_webp(self):
        os.unlink(self.root / "images" / "a.webp")
        code, output = self.run_gate("--require-webp")
        self.assertEqual(code, 1)
        self.assertIn("missing .webp companion", output)

    def test_a_webp_that_is_no_jpegs_companion_does_not_count(self):
        # A score thumbnail is WebP but no photograph's companion; counting
        # it hid the missing pipeline (2026-10-03).
        os.unlink(self.root / "images" / "a.webp")
        self.site.write("music/piece/scores/thumb.webp", "webp-bytes")
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)
        self.assertIn("missing .webp companion", output)

    def test_no_jpegs_means_no_webp_complaint(self):
        os.unlink(self.root / "images" / "a.webp")
        os.unlink(self.root / "images" / "a.jpg")
        self.site.write("index.html", "<html><body><p>text</p></body></html>")
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)
        self.assertNotIn("missing .webp companion", output)

    def test_a_converted_thumbnail_does_not_hide_a_missing_full_size_webp(self):
        # Pillow can produce variant WebPs even when cwebp is absent.
        # The production gate must still catch the unconverted originals.
        self.site.write("images/a.w480.jpg", "jpeg-bytes")
        self.site.write("images/a.w480.webp", "webp-bytes")
        os.unlink(self.root / "images" / "a.webp")
        code, output = self.run_gate("--require-webp")
        self.assertEqual(code, 1, output)
        self.assertIn("images/a.jpg", output)

    def test_a_png_without_webp_fails_even_when_all_jpegs_have_companions(self):
        self.site.write("images/figure.png", "png-bytes")
        code, output = self.run_gate("--require-webp")
        self.assertEqual(code, 1, output)
        self.assertIn("images/figure.png", output)

    def test_partial_webp_coverage_only_warns_for_an_ordinary_build(self):
        self.site.write("images/missing.jpg", "jpeg-bytes")
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)
        self.assertIn("images/missing.jpg", output)

    # -- vendored subtrees are not first-party ------------------------------

    def test_vendored_pdfjs_is_not_scanned(self):
        self.site.write(
            "pdfjs/web/viewer.html",
            '<html><body><img src="images/toolbarButton.png" alt="">'
            '<div id="viewer"></div><div id="viewer"></div></body></html>',
        )
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)

    def test_archive_snapshot_is_not_scanned(self):
        self.site.write(
            "archive/x/snapshot.html",
            '<html><body><img src="/gone-upstream.png" alt=""></body></html>',
        )
        code, output = self.run_gate()
        self.assertEqual(code, 0, output)


if __name__ == "__main__":
    unittest.main()
