"""A fresh font cache must use the same bytes as the reviewed rebuild."""
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
    def test_all_downloads_have_immutable_revisions_and_checksums(self):
        urls = {s[1] for s in fonts.FONTS} | {s[1] for s in fonts.LICENCES.values()}
        for _, faces, _, reference in fonts.FALLBACK_SETS:
            urls.update(reference(face[0])[0] for face in faces)
        for url in urls:
            self.assertRegex(url, r"githubusercontent.com/[^/]+/[^/]+/[0-9a-f]{40}/")
            self.assertRegex(fonts.SOURCE_HASHES[url], r"^[0-9a-f]{64}$")

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
