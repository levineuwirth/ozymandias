"""A fresh font cache must use the same bytes as the reviewed rebuild.

Each source is an https download (a project's pinned revision, a
foundry's) or a file in the site, and each is pinned by its SHA-256; the
bytes are checked before use, whatever the source."""
import hashlib
import io
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch
from tests._helpers import load_tool
from tests.target import FONTS, NO_FONTS

# The tool reads the site's fonts.yaml as it loads.
fonts = load_tool("subset-fonts.py") if FONTS.is_file() else None


@unittest.skipIf(fonts is None, NO_FONTS)
class FontSourceTests(unittest.TestCase):
    def test_every_source_is_https_or_the_site_s_and_pinned(self):
        urls = {s[1] for s in fonts.FONTS} | {s[1] for s in fonts.LICENCES.values()}
        for _, faces, _, reference in fonts.FALLBACK_SETS:
            urls.update(reference(face[0])[0] for face in faces)
        for url in urls:
            with self.subTest(source=url):
                if fonts.is_download(url):
                    self.assertRegex(url, r"^https://[^/]+/")
                else:
                    self.assertTrue((fonts.SITE_ROOT / url).is_file(), f"{url} is not a file in the site")
                self.assertRegex(fonts.SOURCE_HASHES[url], r"^[0-9a-f]{64}$")

    def test_a_source_is_https_or_a_directory_in_the_site(self):
        upstreams = {"foundry": "https://fonts.example/v2", "given": "fonts/source",
                     "plain": "http://fonts.example", "outside": "../elsewhere", "rooted": "/usr/share/fonts"}
        with patch.dict(fonts.CONFIG, {"upstreams": upstreams}):
            self.assertEqual(fonts.source("foundry:A Face/Regular.otf"), "https://fonts.example/v2/A%20Face/Regular.otf")
            self.assertEqual(fonts.source("given:A Face/Regular.otf"), "fonts/source/A Face/Regular.otf")
            for spec in ("plain:a.otf", "outside:a.otf", "rooted:a.otf", "given:../../a.otf"):
                with self.subTest(spec=spec), self.assertRaises(SystemExit):
                    fonts.source(spec)

    def test_a_file_in_the_site_is_read_where_it_is_and_checked(self):
        with tempfile.TemporaryDirectory() as tmp, patch.object(fonts, "SITE_ROOT", Path(tmp)), \
                tempfile.TemporaryDirectory() as cache, patch.object(fonts, "CACHE", Path(cache)):
            (Path(tmp) / "fonts/source").mkdir(parents=True)
            (Path(tmp) / "fonts/source/a.otf").write_bytes(b"the font")
            good = hashlib.sha256(b"the font").hexdigest()
            with patch.dict(fonts.SOURCE_HASHES, {"fonts/source/a.otf": good}):
                self.assertEqual(fonts.fetch("fonts/source/a.otf"), b"the font")
            with patch.dict(fonts.SOURCE_HASHES, {"fonts/source/a.otf": "0" * 64}):
                with self.assertRaisesRegex(ValueError, "checksum"):
                    fonts.fetch("fonts/source/a.otf")
            self.assertFalse(any(Path(cache).rglob("*")), "a file in the site is not cached")

    def test_corrupt_download_is_not_cached(self):
        with tempfile.TemporaryDirectory() as tmp, patch.object(fonts, "CACHE", Path(tmp)):
            with patch.object(fonts.urllib.request, "urlopen", return_value=io.BytesIO(b"truncated")):
                with self.assertRaisesRegex(ValueError, "checksum"):
                    fonts.fetch(fonts.FONTS[0][1])
            self.assertFalse(any(p.is_file() for p in Path(tmp).rglob("*")))

    def test_corrupt_cache_is_not_used(self):
        url = fonts.FONTS[0][1]
        with tempfile.TemporaryDirectory() as tmp, patch.object(fonts, "CACHE", Path(tmp)):
            cached = Path(tmp) / hashlib.sha256(url.encode()).hexdigest()[:16] / url.rsplit("/", 1)[1]
            cached.parent.mkdir(parents=True)
            cached.write_bytes(b"truncated")
            with self.assertRaisesRegex(ValueError, "checksum"):
                fonts.fetch(url)
