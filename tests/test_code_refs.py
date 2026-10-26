#!/usr/bin/env python3
"""Tests for tools/code-refs.py — no network.

Run with: ``python3 -m unittest tests.test_code_refs``.
"""

from __future__ import annotations

import json
import os
import tempfile
import unittest
from contextlib import redirect_stderr, redirect_stdout
from io import StringIO
from pathlib import Path
from unittest import mock
from tests._helpers import load_tool, requires_cabal

code_refs = load_tool("code-refs.py")

SHA = "3508ef1d63d004686e85373f6100689ae94b922a"


def parse(url: str):
    m = code_refs.URL_RE.search(url)
    return code_refs.parse_link(m) if m else None


class ParseLink(unittest.TestCase):
    def test_pinned_blob(self):
        link = parse(f"https://github.com/o/r/blob/{SHA}/analysis/a.md#results")
        self.assertEqual(link["kind"], "blob")
        self.assertEqual(link["ref"], SHA)
        self.assertEqual(link["path"], "analysis/a.md")
        self.assertTrue(link["pinned"])
        # The fragment selects within the snapshot; it is not part of the key.
        self.assertEqual(link["url"], f"https://github.com/o/r/blob/{SHA}/analysis/a.md")

    def test_branch_tree_is_unpinned(self):
        link = parse("https://github.com/o/r/tree/weight-split-model")
        self.assertEqual((link["kind"], link["ref"], link["path"]), ("tree", "weight-split-model", ""))
        self.assertFalse(link["pinned"])

    def test_short_commit(self):
        link = parse("https://github.com/o/r/commit/f426f97")
        self.assertEqual((link["kind"], link["ref"]), ("commit", "f426f97"))
        self.assertTrue(link["pinned"])

    def test_commit_by_branch_rejected(self):
        self.assertIsNone(parse("https://github.com/o/r/commit/main"))

    def test_blob_without_path_rejected(self):
        self.assertIsNone(parse(f"https://github.com/o/r/blob/{SHA}"))

    def test_traversal_rejected(self):
        self.assertIsNone(parse(f"https://github.com/o/r/blob/{SHA}/a/../../x"))
        self.assertIsNone(parse(f"https://github.com/o/r/blob/{SHA}/a/%2e%2e/x"))

    def test_trailing_punctuation_stripped(self):
        link = parse(f"see https://github.com/o/r/tree/{SHA}/profiler.")
        self.assertEqual(link["path"], "profiler")

    def test_bare_repo_not_matched(self):
        self.assertIsNone(parse("https://github.com/o/r"))


# Discovery asks the generator (`site list-links`, build/PageScan.hs).
@requires_cabal
class Discover(unittest.TestCase):
    def test_markdown_links_deduplicated(self):
        with tempfile.TemporaryDirectory() as d:
            (Path(d) / "a.md").write_text(
                f"[x](https://github.com/o/r/blob/{SHA}/f.py#L3-L9) and "
                f"[y](https://github.com/o/r/blob/{SHA}/f.py#L20)\n"
            )
            links = code_refs.discover_links(Path(d))
        self.assertEqual(list(links), [f"https://github.com/o/r/blob/{SHA}/f.py"])

    def test_only_links_are_discovered(self):
        # A URL in code or in prose is not a link, so no tag would use its
        # snapshot; the old regex scan of the raw Markdown took both.
        with tempfile.TemporaryDirectory() as d:
            (Path(d) / "a.md").write_text(
                f"[x](https://github.com/o/r/blob/{SHA}/link.py)\n\n"
                f"`https://github.com/o/r/blob/{SHA}/inline.py`\n\n"
                f"```\nhttps://github.com/o/r/blob/{SHA}/block.py\n```\n\n"
                f"See https://github.com/o/r/blob/{SHA}/prose.py for more.\n"
                f"<https://github.com/o/r/blob/{SHA}/autolink.py>\n"
            )
            found = {u.rsplit("/", 1)[-1] for u in code_refs.discover_links(Path(d))}
        self.assertEqual(found, {"link.py", "autolink.py"})


@requires_cabal
class Eligibility(unittest.TestCase):
    """Everything under code-refs/ is published, so only links on pages the
    site publishes may be snapshotted (audit X1, T07)."""

    def discover(self, files: dict[str, str]) -> set[str]:
        with tempfile.TemporaryDirectory() as d:
            root = Path(d)
            for rel, text in files.items():
                (root / rel).parent.mkdir(parents=True, exist_ok=True)
                (root / rel).write_text(text)
            return {u.rsplit("/", 1)[-1] for u in code_refs.discover_links(root)}

    def link(self, name: str) -> str:
        return f"[x](https://github.com/o/r/blob/{SHA}/{name})\n"

    def test_only_published_pages_are_scanned(self):
        found = self.discover({
            "essays/pub.md": "---\ntitle: P\n---\n" + self.link("public.py"),
            "drafts/essay/index.md": self.link("drafts.py"),
            "essays/notes.local.md": self.link("local.py"),
            "essays/plan.draft.md": self.link("draftname.py"),
            "essays/wip.md": "---\ntitle: W\ndraft: true\n---\n" + self.link("flagged.py"),
            "poetry/coll/index.md": "---\ndraft: yes\n---\n",
            "poetry/coll/poem.md": self.link("in-draft-collection.py"),
            "poetry/open/index.md": "---\ndraft: false\n---\n",
            "poetry/open/poem.md": self.link("open-collection.py"),
        })
        self.assertEqual(found, {"public.py", "open-collection.py", "in-draft-collection.py"})

    def test_yaml_drafts_cannot_create_public_snapshots(self):
        files = {
            "essays/public.md": self.link("public.py"),
            "essays/hidden/index.md": "---\ndraft: true\n---\n",
            "essays/hidden/notes.md": self.link("hidden-child.py"),
        }
        for index, flag in enumerate(('"draft": true', 'draft:\n  true', 'draft: >-\n  true')):
            files[f"essays/draft{index}.md"] = f"---\n{flag}\n...\n" + self.link(f"draft{index}.py")
        self.assertEqual(self.discover(files), {"public.py"})

    def test_draft_word_in_body_is_not_a_flag(self):
        found = self.discover({
            "essays/a.md": "---\ntitle: A\n---\ndraft: true\n" + self.link("a.py"),
        })
        self.assertEqual(found, {"a.py"})

    def test_private_provider_names_and_directories_are_not_scanned(self):
        found = self.discover({
            "essays/public.md": self.link("public.py"),
            "essays/credentials-notes.md": self.link("credential.py"),
            "essays/id_rsa-notes.md": self.link("key.py"),
            "essays/credentials-private/index.md": self.link("nested.py"),
            "essays/__pycache__/notes.md": self.link("cache.py"),
            "essays/notes.local.md/index.md": self.link("private-dir.py"),
        })
        self.assertEqual(found, {"public.py"})

    def test_publication_name_lists_also_guard_code_ref_discovery(self):
        from tests.test_gitignore import samples

        files = {"essays/public.md": self.link("public.py")}
        for index, (_, name) in enumerate(samples()):
            files[f"essays/{name}/index.md"] = self.link(f"dir{index}.py")
            if name.endswith(".md"):
                files[f"poetry/{name}"] = self.link(f"file{index}.py")
        self.assertEqual(self.discover(files), {"public.py"})


class Fetch(unittest.TestCase):
    """cmd_fetch against a temporary store, with the network stubbed."""

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.content = self.root / "content"
        self.store = self.root / "code-refs"
        for name, value in (("CONTENT_DIR", self.content), ("STORE_DIR", self.store),
                            ("INDEX_PATH", self.store / "index.json")):
            p = mock.patch.object(code_refs, name, value)
            p.start()
            self.addCleanup(p.stop)
        code_refs._visibility.clear()
        (self.content / "essays").mkdir(parents=True)

    def page(self, text: str) -> None:
        (self.content / "essays" / "a.md").write_text(text)

    def fake_snapshot(self, sha: str):
        def snap(link):
            code_refs.refuse_private(link["owner"], link["repo"])
            d = code_refs.sha_dir(link["owner"], link["repo"], sha)
            (d / "tree").mkdir(parents=True, exist_ok=True)
            (d / "commit.json").write_text("{}")
            (d / "tree" / "_root.json").write_text("{}")
            return {"kind": "tree", "host": "github", "owner": link["owner"],
                    "repo": link["repo"], "ref": link["ref"], "sha": sha, "path": "",
                    "pinned": link["pinned"],
                    "src": code_refs.public(d / "tree" / "_root.json"),
                    "commit": code_refs.public(d / "commit.json"),
                    "date": "2026-09-30T00:00:00Z", "fetched": "2026-10-02"}
        return snap

    def fetch(self, snap) -> str:
        out = StringIO()
        with mock.patch.object(code_refs, "snapshot", snap), redirect_stdout(out), redirect_stderr(out):
            code_refs.cmd_fetch(None)
        return out.getvalue()

    def test_unmoved_branch_keeps_its_entry(self):
        # T02: the same commit on another day must not rewrite index.json.
        self.page("[t](https://github.com/o/r/tree/main)\n")
        self.fetch(self.fake_snapshot("a" * 40))
        index = json.loads((self.store / "index.json").read_text())
        url = "https://github.com/o/r/tree/main"
        index[url]["fetched"] = "2026-09-01"
        (self.store / "index.json").write_text(json.dumps(index))
        before = (self.store / "index.json").read_bytes()
        self.fetch(self.fake_snapshot("a" * 40))
        self.assertEqual((self.store / "index.json").read_bytes(), before)

    def test_moved_branch_drops_the_old_commit(self):
        self.page("[t](https://github.com/o/r/tree/main)\n")
        self.fetch(self.fake_snapshot("a" * 40))
        self.fetch(self.fake_snapshot("b" * 40))
        self.assertFalse((self.store / "github/o/r" / ("a" * 40)).exists())
        self.assertTrue((self.store / "github/o/r" / ("b" * 40) / "commit.json").exists())

    def test_debris_is_collected_after_a_clean_fetch(self):
        self.page("[t](https://github.com/o/r/tree/main)\n")
        stray = self.store / "github/o/r" / ("a" * 40) / "blob/x.py.txt.partial"
        stray.parent.mkdir(parents=True)
        stray.write_text("half")
        self.fetch(self.fake_snapshot("a" * 40))
        self.assertFalse(stray.exists())

    def test_failure_keeps_previous_snapshots(self):
        self.page("[t](https://github.com/o/r/tree/main)\n[u](https://github.com/o/s/tree/main)\n")
        self.fetch(self.fake_snapshot("a" * 40))
        def flaky(link):
            if link["repo"] == "s":
                raise code_refs.FetchError("offline")
            return self.fake_snapshot("b" * 40)(link)
        self.fetch(flaky)
        # The failed link keeps its entry, and nothing is collected.
        self.assertTrue((self.store / "github/o/s" / ("a" * 40) / "commit.json").exists())
        self.assertTrue((self.store / "github/o/r" / ("a" * 40) / "commit.json").exists())

    def test_private_repository_refused_with_a_token(self):
        self.page("[t](https://github.com/o/secret/tree/main)\n[u](https://github.com/o/pub/tree/main)\n")
        def api(path):
            return {"private": path.endswith("/secret")}
        with mock.patch.dict(os.environ, {"GITHUB_TOKEN": "t"}), \
             mock.patch.object(code_refs, "api_json", api):
            out = self.fetch(self.fake_snapshot("a" * 40))
        index = json.loads((self.store / "index.json").read_text())
        self.assertEqual(list(index), ["https://github.com/o/pub/tree/main"])
        self.assertFalse((self.store / "github/o/secret").exists())
        self.assertIn("private", out)

    def test_cached_private_repositories_are_removed_even_if_another_fetch_fails(self):
        for ref in ("main", "a" * 40):
            with self.subTest(ref=ref):
                code_refs._visibility.clear()
                self.page(f"[t](https://github.com/o/secret/tree/{ref})\n")
                with mock.patch.dict(os.environ, {"GITHUB_TOKEN": ""}):
                    self.fetch(self.fake_snapshot("a" * 40))
                self.page(f"[t](https://github.com/o/secret/tree/{ref})\n"
                          "[u](https://github.com/o/offline/tree/main)\n")

                def api(path):
                    if path.endswith("/secret"):
                        return {"private": True}
                    raise code_refs.FetchError("offline")

                with mock.patch.dict(os.environ, {"GITHUB_TOKEN": "t"}), \
                     mock.patch.object(code_refs, "api_json", api):
                    self.fetch(self.fake_snapshot("a" * 40))
                index = code_refs.load_index()
                self.assertFalse(any(e["repo"] == "secret" for e in index.values()))
                self.assertFalse((self.store / "github/o/secret").exists())


class CommitSnapshots(unittest.TestCase):
    """ensure_commit against a temporary store, with the API stubbed."""

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        p = mock.patch.object(code_refs, "STORE_DIR", Path(tmp.name))
        p.start()
        self.addCleanup(p.stop)

    def answer(self, files: int):
        return {"sha": SHA, "parents": [],
                "commit": {"message": "m", "author": {"name": "a"},
                           "committer": {"date": "2026-10-01T00:00:00Z"}},
                "stats": {"total": files, "additions": files, "deletions": 0},
                "files": [{"filename": f"f{i}", "status": "added", "additions": 1,
                           "deletions": 0} for i in range(files)]}

    def stored(self) -> dict:
        return json.loads((code_refs.sha_dir("o", "r", SHA) / "commit.json").read_text())

    def test_an_incomplete_answer_does_not_replace_a_full_snapshot(self):
        # GitHub sometimes answers a very large commit before computing its
        # diff; a branch link asks again every build.
        with mock.patch.object(code_refs, "api_json", lambda _: self.answer(3)):
            code_refs.ensure_commit("o", "r", "main")
        with mock.patch.object(code_refs, "api_json", lambda _: self.answer(0)):
            commit = code_refs.ensure_commit("o", "r", "main")
        self.assertEqual(commit["files_total"], 3)
        self.assertEqual(self.stored()["files_total"], 3)

    def test_an_unchanged_snapshot_is_not_rewritten(self):
        with mock.patch.object(code_refs, "api_json", lambda _: self.answer(2)):
            code_refs.ensure_commit("o", "r", "main")
            path = code_refs.sha_dir("o", "r", SHA) / "commit.json"
            os.utime(path, (1, 1))
            code_refs.ensure_commit("o", "r", "main")
        self.assertEqual(path.stat().st_mtime, 1)


class RepositoryStore(unittest.TestCase):
    """The committed store: every file is referenced, every reference
    exists, and every entry comes from a page the site publishes."""

    def test_store_matches_index(self):
        index = code_refs.load_index()
        refs = {e[k][len(code_refs.PUBLIC_PREFIX):] for e in index.values() for k in ("src", "commit")}
        files = {p.relative_to(code_refs.STORE_DIR).as_posix()
                 for p in (code_refs.STORE_DIR / "github").rglob("*") if p.is_file()}
        self.assertEqual(sorted(files - refs), [], "unreferenced snapshot files (run tools/code-refs.py gc)")
        self.assertEqual(sorted(refs - files), [], "index references missing files")

    def test_every_entry_comes_from_a_published_page(self):
        index = code_refs.load_index()
        published = code_refs.discover_links()
        self.assertEqual(sorted(set(index) - set(published)), [],
                         "snapshots for links no published page contains")


if __name__ == "__main__":
    unittest.main()
