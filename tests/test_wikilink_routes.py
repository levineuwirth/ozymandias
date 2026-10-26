"""Wikilinks resolve a bare slug without Hakyll's route table, so the slugs
whose page is a directory index are derived from the taxonomy
(build/Sections.hs: the section landings in site.yaml, the generated
indexes and the portal tags). A slug missing there links to <slug>.html,
which 404s. This keeps the derivation complete against every
"<slug>/index.html" route written in build/."""

import re
import unittest

import yaml

from tests._helpers import SITE_ROOT, engine_path

BUILD = engine_path("build")


def generated_indexes() -> set[str]:
    src = (BUILD / "Sections.hs").read_text(encoding="utf-8")
    body = re.search(r"^generatedIndexes = \[(.*?)\]", src, re.M | re.S).group(1)
    return set(re.findall(r'"([^"]+)"', body))


def section_landings() -> set[str]:
    config = yaml.safe_load((SITE_ROOT / "site.yaml").read_text(encoding="utf-8"))
    return {s["dir"] for s in config["sections"]}


def directory_routes() -> set[str]:
    found = set()
    for hs in BUILD.rglob("*.hs"):
        found |= set(re.findall(r'"([a-z-]+)/index\.html"', hs.read_text(encoding="utf-8")))
    return found


class DirectoryRouteSlugs(unittest.TestCase):
    def test_sources_were_read(self):
        self.assertIn("build", directory_routes())
        self.assertIn("build", generated_indexes())
        self.assertTrue(section_landings())

    def test_every_directory_route_is_derived(self):
        missing = directory_routes() - generated_indexes() - section_landings()
        self.assertEqual(missing, set(),
                         "a generated index belongs in generatedIndexes in build/Sections.hs; "
                         "a section landing in site.yaml's sections")


if __name__ == "__main__":
    unittest.main()
