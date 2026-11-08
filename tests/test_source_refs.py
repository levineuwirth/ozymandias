"""Every source link the build emits has the /source/ copy its popup fetches,
and /source/ holds only the site's own files.

build/Filters/SourceRefs.hs wraps a repo path in prose as a source-ref link,
and the source-preview rule in build/Site.hs copies files to /source/<path>;
both read site.yaml's source-preview (until 2026-10-04 each kept its own
copy of the whitelist, and until A9f it was the engine's, publishing the
engine's files too). A path wrapped but not copied is a popup that 404s on
hover. Reads the built site; a site without the source-refs switch has
neither."""

import re
import unittest

from tests._helpers import SITE_ROOT
from tests.switch_checks import SIDECAR
from tests.target import requires_switch

SITE = SITE_ROOT / "_site"
SOURCE_PATH_RE = re.compile(r'data-source-path="([^"]+)"')


@requires_switch("source-refs")
@unittest.skipUnless((SITE / "index.html").is_file(), "no _site — run `make build`")
class SourceRefTargets(unittest.TestCase):
    def test_every_wrapped_path_is_served(self) -> None:
        wrapped: dict[str, str] = {}
        for page in sorted(SITE.rglob("*.html")):
            if SITE / "source" in page.parents:
                continue
            for path in SOURCE_PATH_RE.findall(page.read_text(encoding="utf-8")):
                wrapped.setdefault(path, str(page.relative_to(SITE)))
        # A site may publish no source files, or never mention them in prose.
        # The fixture switch checks require positive examples where promised.
        missing = [f"{path} (on {page})" for path, page in sorted(wrapped.items())
                   if not (SITE / "source" / path).is_file()]
        self.assertEqual(missing, [])

    def test_only_the_site_s_own_files_are_published(self) -> None:
        # The engine's files are its repository's: a copy here would claim
        # them for the site, and go stale against the engine it builds with.
        source = SITE / "source"
        published = [p.relative_to(source).as_posix() for p in source.rglob("*") if p.is_file()]
        foreign = [p for p in published if not (SITE_ROOT / SIDECAR.sub("", p)).is_file()]
        self.assertEqual(foreign, [])


if __name__ == "__main__":
    unittest.main()
