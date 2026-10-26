"""Every source link the build emits has the /source/ copy its popup fetches.

build/Filters/SourceRefs.hs wraps a repo path in prose as a source-ref link,
and the source-preview rule in build/Site.hs copies files to /source/<path>;
both read Filters.SourceRefs.sourcePreviewGlobs (until 2026-10-04 each kept
its own copy of the whitelist). A path wrapped but not copied is a popup
that 404s on hover. Reads the built site."""

import re
import unittest

from tests._helpers import SITE_ROOT

SITE = SITE_ROOT / "_site"
SOURCE_PATH_RE = re.compile(r'data-source-path="([^"]+)"')


@unittest.skipUnless((SITE / "source").is_dir(), "no _site — run `make build`")
class SourceRefTargets(unittest.TestCase):
    def test_every_wrapped_path_is_served(self) -> None:
        wrapped: dict[str, str] = {}
        for page in sorted(SITE.rglob("*.html")):
            if SITE / "source" in page.parents:
                continue
            for path in SOURCE_PATH_RE.findall(page.read_text(encoding="utf-8")):
                wrapped.setdefault(path, str(page.relative_to(SITE)))
        self.assertTrue(wrapped, "no source-ref links found")
        missing = [f"{path} (on {page})" for path, page in sorted(wrapped.items())
                   if not (SITE / "source" / path).is_file()]
        self.assertEqual(missing, [])


if __name__ == "__main__":
    unittest.main()
