"""Wikilinks resolve a bare slug without Hakyll's route table, so the slugs
whose page is a directory index are derived from the taxonomy
(build/Sections.hs: the section landings in site.yaml, the generated
indexes whose switches are on and the portal tags). A slug missing there links to <slug>.html,
which 404s. This keeps the derivation complete against every
"<slug>/index.html" route written in build/."""

import re
import unittest

import yaml

from tests._helpers import SITE_ROOT, engine_path

BUILD = engine_path("build")


def generated_indexes() -> dict[str, str]:
    """Sections.generatedIndexPages: each generated index, and the switch
    that builds it."""
    src = (BUILD / "Sections.hs").read_text(encoding="utf-8")
    body = re.search(r"^generatedIndexPages =(.*?)^\S", src, re.M | re.S).group(1)
    return dict(re.findall(r'\("([^"]+)",\s*"([^"]+)"\)', body))


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
        missing = directory_routes() - set(generated_indexes()) - section_landings()
        self.assertEqual(missing, set(),
                         "a generated index belongs in generatedIndexPages in build/Sections.hs; "
                         "a section landing in site.yaml's sections")

    def test_each_generated_index_is_its_switch_s_route(self):
        # So it is a directory slug only when it is built.
        registry = yaml.safe_load(engine_path("switches.yaml").read_text(encoding="utf-8"))
        for index, switch in generated_indexes().items():
            with self.subTest(index=index):
                self.assertIn(f"/{index}/", registry[switch].get("routes") or [])


if __name__ == "__main__":
    unittest.main()
