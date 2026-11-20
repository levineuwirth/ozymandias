from __future__ import annotations

import argparse
import contextlib
import io
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from tests._helpers import engine_path, load_tool, requires_cabal, script_env, write_site_config


import_content = load_tool("import-content.py", register=True)


@contextlib.contextmanager
def site_at(root: Path):
    """The importer at work on the site at `root` and no other. It finds
    its site at $SITE_ROOT before the working directory (sitelib.site_root),
    so with SITE_ROOT exported, as `make test` and a site's own runs have
    it, a test that only changed directory read that site's routes and
    wrote its pages into that site's content/."""
    with contextlib.chdir(root), mock.patch.dict(os.environ, {"SITE_ROOT": str(root)}):
        yield


def args_for(**overrides: object) -> argparse.Namespace:
    values: dict[str, object] = {
        "type": "page",
        "title_prefix": None,
        "date": None,
        "tags": None,
        "author": None,
        "field": [],
        "collection": None,
        "overwrite": False,
    }
    values.update(overrides)
    return argparse.Namespace(**values)


class ImportContentTests(unittest.TestCase):
    def test_author_override_uses_authors_list(self) -> None:
        doc = import_content.Document(
            "Title\n\nBody", {"authors": ["Previous Author"]}
        )

        import_content.apply_schema(
            [doc], args_for(author="Ada Lovelace")
        )

        self.assertEqual(doc.meta["authors"], ["Ada Lovelace"])

    def test_duplicate_output_destinations_are_rejected(self) -> None:
        docs = [
            import_content.Document("First", {"title": "Same"}),
            import_content.Document("Second", {"title": "Same"}),
        ]

        with self.assertRaisesRegex(
            import_content.ContentImportError,
            "duplicate output destination content/same.md",
        ):
            import_content.assemble_file_entries(docs, args_for())

    def test_explicit_source_slugs_determine_output_paths(self) -> None:
        docs = [
            import_content.Document(
                "First", {"title": "Same", "slug": "source-one"}
            ),
            import_content.Document(
                "Second", {"title": "Same", "slug": "source-two"}
            ),
        ]

        entries = import_content.assemble_file_entries(docs, args_for())

        self.assertEqual(
            {path.as_posix() for path, _content, _label in entries},
            {"content/source-one.md", "content/source-two.md"},
        )

    # A page collection is checked against `site shared-rules`.
    @requires_cabal
    def test_collections_support_every_content_type(self) -> None:
        # A collection's address is under its section's, which is the
        # directory's name unless the site this runs for gives another.
        address = import_content.sitelib.section_address
        expected = {
            "essay": (
                "content/essays/cycle",
                f"/{address('essays')}/cycle/",
            ),
            "blog": (
                "content/blog/cycle",
                f"/{address('blog')}/cycle/",
            ),
            "fiction": (
                "content/fiction/cycle",
                f"/{address('fiction')}/cycle/",
            ),
            "poetry": (
                "content/poetry/cycle",
                f"/{address('poetry')}/cycle/",
            ),
            "page": (
                "content/cycle",
                "/cycle/",
            ),
        }

        for content_type, (directory, collection_url) in expected.items():
            with self.subTest(content_type=content_type):
                doc = import_content.Document("Body", {"title": "Piece"})
                entries = import_content.assemble_file_entries(
                    [doc],
                    args_for(type=content_type, collection="Cycle"),
                )

                self.assertEqual(
                    {path.as_posix() for path, _content, _label in entries},
                    {f"{directory}/piece.md", f"{directory}/index.md"},
                )
                self.assertEqual(
                    doc.meta["collection-url"], collection_url
                )

    @requires_cabal
    def test_page_collection_rejects_reserved_section_slug(self) -> None:
        docs = [import_content.Document("Body", {"title": "Piece"})]

        # bibliography-meta was missing from both lists until 2026-10-04.
        for collection in ("Fiction", "Bibliography Meta"):
            with self.subTest(collection=collection), self.assertRaisesRegex(
                import_content.ContentImportError,
                "conflicts with a reserved content directory",
            ):
                import_content.assemble_file_entries(
                    docs, args_for(type="page", collection=collection)
                )

    @requires_cabal
    def test_collection_refused_where_its_url_space_is_taken(self) -> None:
        # A tag page already lives at /notes/; a `notes` collection would
        # write notes/index.html over it, which the build refuses
        # (build/RouteCheck.hs). Refused before anything is written.
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "data").mkdir()
            write_site_config(root)
            essay = root / "content" / "essays" / "a.md"
            essay.parent.mkdir(parents=True)
            essay.write_text("---\ntitle: A\ndate: 2026-01-01\ntags: [notes]\n---\n\nBody.\n",
                             encoding="utf-8")
            source = root / "source.txt"
            source.write_text("Title\n\nBody\n", encoding="utf-8")
            stderr = io.StringIO()

            with site_at(root), contextlib.redirect_stdout(io.StringIO()), \
                    contextlib.redirect_stderr(stderr):
                refused = import_content.main([str(source), "--collection", "Notes"])
                accepted = import_content.main([str(source), "--collection", "Fresh"])

            self.assertEqual(refused, 2)
            self.assertIn("collection 'notes' would share /notes/ with existing pages: "
                          "/notes/index.html (notes/index.html)", stderr.getvalue())
            self.assertFalse((root / "content" / "notes").exists())
            self.assertEqual(accepted, 0, stderr.getvalue())
            self.assertTrue((root / "content" / "fresh" / "index.md").exists())

    def test_a_test_s_site_is_its_own_whatever_site_root_says(self) -> None:
        with tempfile.TemporaryDirectory() as directory, tempfile.TemporaryDirectory() as other:
            with mock.patch.dict(os.environ, {"SITE_ROOT": other}), site_at(Path(directory)):
                self.assertEqual(import_content.sitelib.site_root(), Path(directory).resolve())
            self.assertEqual(os.listdir(other), [])

    def test_a_section_the_site_does_not_list_takes_no_import(self) -> None:
        # The build publishes nothing from an unlisted section's directory
        # and stops at a file in it, so the importer refuses first.
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            write_site_config(root, sections=[{"dir": "blog", "address": "notebook", "kind": "Note",
                                               "dingbat": None}])
            source = root / "source.txt"
            source.write_text("A Poem\n\nA line.\n", encoding="utf-8")
            stderr = io.StringIO()
            with site_at(root), contextlib.redirect_stdout(io.StringIO()), \
                    contextlib.redirect_stderr(stderr):
                refused = import_content.main([str(source), "--type", "poetry", "--date", "2026-01-02"])
                dry = import_content.main([str(source), "--type", "poetry", "--date", "2026-01-02",
                                           "--dry-run"])
                accepted = import_content.main([str(source), "--type", "blog", "--date", "2026-01-02"])
            self.assertEqual((refused, dry), (2, 2))
            self.assertIn("error: site.yaml lists no poetry section", stderr.getvalue())
            self.assertFalse((root / "content" / "poetry").exists())
            self.assertEqual(accepted, 0, stderr.getvalue())
            self.assertEqual([p.name for p in (root / "content" / "blog").iterdir()], ["a-poem.md"])

    def test_the_poetry_importer_refuses_a_site_without_the_section(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "sonnets.txt"
            source.write_text("I\n\nFrom fairest creatures we desire increase,\n"
                              "That thereby beauty's rose might never die,\n", encoding="utf-8")
            command = [sys.executable, engine_path("tools/import-poetry.py"), str(source),
                       "--poet", "A Poet", "--collection", "Sonnets", "--date", "1609"]
            for sections, status in (([{"dir": "blog", "kind": "Post", "dingbat": None}], 1),
                                     ([{"dir": "poetry", "kind": "Poem", "dingbat": None}], 0)):
                with self.subTest(sections=sections):
                    write_site_config(root, sections=sections)
                    done = subprocess.run(command, cwd=root, env=script_env(SITE_ROOT=str(root)),
                                          capture_output=True, text=True, timeout=60)
                    self.assertEqual(done.returncode, status, done.stdout + done.stderr)
                    self.assertEqual((root / "content" / "poetry").exists(), status == 0)
                    if status:
                        self.assertIn("site.yaml lists no poetry section", done.stderr)

    def test_writing_types_require_valid_iso_dates(self) -> None:
        with self.assertRaisesRegex(
            import_content.ContentImportError,
            "a date in YYYY-MM-DD format is required",
        ):
            import_content.apply_schema(
                [import_content.Document("Body", {"title": "Story"})],
                args_for(type="fiction"),
            )

        with self.assertRaisesRegex(
            import_content.ContentImportError, "invalid date"
        ):
            import_content.apply_schema(
                [import_content.Document("Body", {"title": "Story"})],
                args_for(type="fiction", date="2026-02-30"),
            )

    def test_opening_prose_is_not_consumed_as_a_title(self) -> None:
        body = "This is the opening sentence.\nThe paragraph continues here."
        doc = import_content.Document(body)

        import_content.apply_schema([doc], args_for())

        self.assertEqual(doc.meta["title"], "Untitled")
        self.assertEqual(doc.body, body)
        self.assertEqual(
            doc.meta["abstract"],
            "This is the opening sentence. The paragraph continues here.",
        )

    def test_invalid_regex_exits_without_writing(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "source.txt"
            source.write_text("Title\n\nBody\n", encoding="utf-8")
            stdout = io.StringIO()
            stderr = io.StringIO()

            with site_at(root), contextlib.redirect_stdout(stdout), \
                    contextlib.redirect_stderr(stderr):
                status = import_content.main([
                    str(source), "--splitter", "regex", "--regex", "(",
                ])

            self.assertEqual(status, 2)
            self.assertIn("error: invalid regex pattern", stderr.getvalue())
            self.assertFalse((root / "content").exists())

    def test_consecutive_headings_keep_the_nonempty_section_title(self) -> None:
        docs = import_content.split_heading_1(
            import_content.Document("# First\n# Second\nSecond body"), {}
        )

        self.assertEqual(len(docs), 1)
        self.assertEqual(docs[0].meta, {"number": 1, "title": "Second"})
        self.assertEqual(docs[0].body, "Second body")

    def test_page_break_handles_form_feed_and_preserves_no_match(self) -> None:
        split = import_content.split_page_break(
            import_content.Document("first\fsecond"), {}
        )
        untouched = import_content.Document("Body only")

        self.assertEqual(
            [(doc.meta, doc.body) for doc in split],
            [({"number": 1}, "first"), ({"number": 2}, "second")],
        )
        self.assertEqual(
            import_content.split_page_break(untouched, {}), [untouched]
        )

    def test_frontmatter_must_be_a_yaml_mapping(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "source.md"
            source.write_text(
                "---\nOrdinary paragraph.\n---\nThis survives.\n",
                encoding="utf-8",
            )

            with site_at(root), self.assertRaisesRegex(
                import_content.ContentImportError,
                "frontmatter must be a YAML mapping",
            ):
                import_content.read_file_per_document(["source.md"])

    def test_structured_fields_are_validated(self) -> None:
        with self.assertRaisesRegex(
            import_content.ContentImportError,
            "json entry 0 field 'body' must be a string",
        ):
            import_content._documents_from_records(
                [{"title": "Bad", "body": 42}], Path("source.json"), "json"
            )

        with self.assertRaisesRegex(
            import_content.ContentImportError,
            "field 'number' must be a positive integer",
        ):
            import_content.apply_schema(
                [
                    import_content.Document(
                        "Body", {"title": "Bad", "number": "1"}
                    )
                ],
                args_for(),
            )

    def test_collection_index_frontmatter_is_valid_yaml(self) -> None:
        doc = import_content.Document(
            "Body",
            {
                "title": "Piece",
                "date": "2026-07-17",
                "tags": ["poetry"],
                "poet": "Ada Lovelace",
            },
        )

        rendered = import_content.generate_collection_index(
            [doc], "Essays: 2026"
        )
        metadata = import_content.yaml.safe_load(rendered.split("---", 2)[1])

        self.assertEqual(metadata["title"], "Essays: 2026")
        self.assertEqual(metadata["tags"], ["poetry"])

    def test_abstract_respects_maximum_length_without_spaces(self) -> None:
        abstract = import_content.auto_abstract("x" * 250, max_chars=200)

        self.assertEqual(len(abstract), 200)
        self.assertTrue(abstract.endswith(" …"))


if __name__ == "__main__":
    unittest.main()
