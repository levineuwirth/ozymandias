"""The font preloads a reader's browser gets, on the built site: test_fonts holds
site.yaml's list to the stylesheet, and test_fixture_site holds any page to the
list. Apart from test_fonts because it needs a build, which the GitHub workflow
has none of, and a test there must run (ci.py).
"""

import re
import unittest

from tests._helpers import SITE_ROOT
from tests.target import site_yaml

HOME = SITE_ROOT / "_site" / "index.html"


@unittest.skipUnless(HOME.is_file(), "no _site: run make build")
class BuiltFonts(unittest.TestCase):
    def test_the_built_home_page_preloads_what_site_yaml_names(self):
        emitted = re.findall(r'<link rel="preload" href="([^"]+)" as="font"', HOME.read_text(encoding="utf-8"))
        self.assertEqual(["/" + href.lstrip("./") for href in emitted], site_yaml().get("font-preloads") or [])


if __name__ == "__main__":
    unittest.main()
