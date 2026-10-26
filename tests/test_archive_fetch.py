"""tools/archive.py's downloads and its one-entry refresh.

The PDF and HTML fetchers share one download (size cap, noarchive header,
which failures leave a Wayback fallback open); a local HTTP server stands in
for the network. A refresh re-snapshots its own entry only: it used to run
the whole fetch, so another entry's missing artifact (a fatal error there)
rolled the refresh back."""

import http.server
import json
import tempfile
import threading
import unittest
from pathlib import Path
from unittest import mock

import yaml
from tests._helpers import load_tool

archive = load_tool("archive.py")
# Its User-Agent names the site, from site.yaml through the generator,
# which these tests do without.
archive.user_agent = lambda: "archive-test"

BODY = b"%PDF-1.4 small\n" * 10


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path == "/missing.pdf":
            self.send_error(404)
            return
        self.send_response(200)
        if self.path == "/noarchive.pdf":
            self.send_header("X-Robots-Tag", "noarchive")
        self.send_header("Content-Type", "application/pdf")
        self.end_headers()
        self.wfile.write(BODY * (50 if self.path == "/big.pdf" else 1))

    def log_message(self, *_):
        pass


class Downloads(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        threading.Thread(target=cls.server.serve_forever, daemon=True).start()
        cls.base = f"http://127.0.0.1:{cls.server.server_address[1]}"

    @classmethod
    def tearDownClass(cls):
        cls.server.shutdown()
        cls.server.server_close()

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.dir = Path(tmp.name)
        for name, value in (("err", lambda *_: None), ("SIZE_CAP", len(BODY) * 10)):
            p = mock.patch.object(archive, name, value)
            p.start()
            self.addCleanup(p.stop)

    def fetch(self, path):
        dest = self.dir / "document.pdf"
        return archive.fetch_pdf(self.base + path, dest), dest

    def test_ok(self):
        result, dest = self.fetch("/a.pdf")
        self.assertEqual((result, dest.read_bytes()), ("ok", BODY))

    def test_a_missing_document_is_dead_so_wayback_may_step_in(self):
        self.assertEqual(self.fetch("/missing.pdf")[0], "dead")

    def test_noarchive_and_the_cap_are_skips_a_fallback_must_not_circumvent(self):
        for path in ("/noarchive.pdf", "/big.pdf"):
            with self.subTest(path=path):
                result, dest = self.fetch(path)
                self.assertEqual(result, "skip")
                self.assertFalse(dest.exists())

    def test_a_failed_rename_is_a_skip_and_leaves_no_partial(self):
        # The final rename once sat inside the download's error handling;
        # moved out of it, a failure there raised and left the .part file.
        (self.dir / "document.pdf").write_bytes(b"previous")
        with mock.patch.object(Path, "replace", side_effect=OSError("disk full")):
            result, dest = self.fetch("/a.pdf")
        self.assertEqual(result, "skip")
        self.assertEqual(dest.read_bytes(), b"previous")
        self.assertEqual(list(self.dir.iterdir()), [dest])

    def test_no_debris(self):
        for path in ("/a.pdf", "/missing.pdf", "/big.pdf", "/noarchive.pdf"):
            self.fetch(path)
            (self.dir / "document.pdf").unlink(missing_ok=True)
        self.assertEqual(list(self.dir.iterdir()), [])


class Refresh(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        arch = self.root / "archive"
        arch.mkdir()
        (arch / "removed.yaml").write_text("[]\n")
        (arch / "manifest.yaml").write_text(yaml.safe_dump([
            {"url": "https://a.example/a.pdf", "slug": "a", "type": "pdf"},
            {"url": "https://b.example/b.pdf", "slug": "b", "type": "pdf"},
        ]))
        # b's provenance is committed but its artifact is gone: fatal for a
        # full fetch, and nothing to do with refreshing a.
        (arch / "b").mkdir()
        (arch / "b" / "PROVENANCE.json").write_text(json.dumps(
            {"url": "https://b.example/b.pdf", "type": "pdf", "artifact": "document.pdf",
             "sha256": "0" * 64}))
        self.index = self.root / "archive-index.json"
        self.index.write_text(json.dumps({"https://b.example/b.pdf": {"slug": "b"}}))

        def fake_fetch(url, dest):
            dest.write_bytes(BODY)
            return "ok"
        for name, value in (("ARCHIVE_DIR", arch), ("MANIFEST", arch / "manifest.yaml"),
                            ("REMOVED", arch / "removed.yaml"), ("INDEX_OUT", self.index),
                            ("fetch_pdf", fake_fetch), ("extract_text_pdf",
                            lambda pdf, txt: txt.write_text("text")),
                            ("err", lambda *_: None), ("log", lambda *_: None)):
            p = mock.patch.object(archive, name, value)
            p.start()
            self.addCleanup(p.stop)
        self.arch = arch

    def test_refresh_touches_only_its_own_entry(self):
        self.assertEqual(archive.cmd_refresh(["a"]), 0)
        prov = json.loads((self.arch / "a" / "PROVENANCE.json").read_text())
        self.assertEqual(prov["sha256"], archive.sha256_of(self.arch / "a" / "document.pdf"))
        index = json.loads(self.index.read_text())
        self.assertEqual(index["https://a.example/a.pdf"]["slug"], "a")
        self.assertIn("https://b.example/b.pdf", index)      # left as it was
        self.assertFalse((self.arch / "b" / "document.pdf").exists())

    def test_refresh_drops_the_records_its_slug_had_under_an_old_url(self):
        # The manifest's URL for `a` lost a tracking parameter and an
        # alias. The old record normalizes like the new URL, so the build's
        # manifest filter keeps it, and the dropped alias would go on
        # resolving to `a` unless the refresh removes it.
        self.index.write_text(json.dumps({
            "https://a.example/a.pdf?utm_source=old": {
                "slug": "a", "aliases": ["https://doi.org/10.1/dropped"]},
            "https://b.example/b.pdf": {"slug": "b"},
        }))
        self.assertEqual(archive.cmd_refresh(["a"]), 0)
        index = json.loads(self.index.read_text())
        self.assertEqual(sorted(index), ["https://a.example/a.pdf", "https://b.example/b.pdf"])
        self.assertNotIn("https://doi.org/10.1/dropped",
                         [a for r in index.values() for a in r.get("aliases", [])])

    def test_a_failed_refresh_restores_the_index(self):
        before = self.index.read_text()
        with mock.patch.object(archive, "fetch_pdf", lambda url, dest: "dead"):
            self.assertEqual(archive.cmd_refresh(["a"]), 1)
        self.assertEqual(self.index.read_text(), before)
        self.assertFalse((self.arch / "a").exists())


if __name__ == "__main__":
    unittest.main()
