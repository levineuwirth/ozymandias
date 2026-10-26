"""The search page's filters against the generator's epistemic vocabularies.

`site shared-rules` prints build/Marks.hs's lists. search-filters.js keeps
its own copy of the ordinal scales, and content/search.md numbers a button
for each value: a filter by button index matches a page only while all three
agree. The JS copy once lacked `local` and `low` (fixed 2026-10-04), so a
filter at or above either matched nothing it should have."""

import json
import re
import subprocess
import unittest
from tests._helpers import SITE_ROOT, engine_path, requires_cabal, site_binary

SEARCH_FILTERS_JS = engine_path("static/js/search-filters.js")
SEARCH_MD = SITE_ROOT / "content" / "search.md"
SEARCH_META = SITE_ROOT / "_site" / "data" / "epistemic-meta.json"



def js_scales() -> dict[str, list[str]]:
    text = SEARCH_FILTERS_JS.read_text(encoding="utf-8")
    m = re.search(r"var SCALES = \{(.*?)\};", text, re.S)
    if not m:
        raise AssertionError("search-filters.js: no `var SCALES = {...};`")
    return {name: re.findall(r"'([^']*)'", values)
            for name, values in re.findall(r"(\w+):\s*\[([^\]]*)\]", m.group(1))}


def ordinal_buttons() -> dict[str, list[tuple[int, str]]]:
    text = SEARCH_MD.read_text(encoding="utf-8")
    buttons: dict[str, list[tuple[int, str]]] = {}
    for field, index, label in re.findall(
            r'<button class="[^"]*filter-ordinal-btn[^"]*" data-field="([^"]+)"'
            r' data-index="(\d+)">([^<]*)</button>', text):
        buttons.setdefault(field, []).append((int(index), label))
    return buttons


@requires_cabal
class EpistemicVocabularyTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        out = subprocess.run([str(site_binary()), "shared-rules"],
                             capture_output=True, text=True, check=True)
        cls.vocab = json.loads(out.stdout)["epistemic-vocabulary"]

    def test_search_scales_are_the_generators(self) -> None:
        scales = js_scales()
        self.assertEqual(sorted(scales), ["novelty", "practicality", "scope", "stability"])
        for field, values in scales.items():
            with self.subTest(field=field):
                self.assertEqual(values, self.vocab[field])

    def test_each_button_is_its_values_index(self) -> None:
        buttons = ordinal_buttons()
        self.assertEqual(sorted(buttons), sorted(js_scales()))
        for field, found in buttons.items():
            with self.subTest(field=field):
                self.assertEqual(found, list(enumerate(self.vocab[field])))

    def test_peer_status_default_is_listed(self) -> None:
        # Contexts.peerStatusField treats "unreviewed" as the default and
        # validates against the same list.
        self.assertEqual(self.vocab["peer-status"][0], "unreviewed")


    @unittest.skipUnless(SEARCH_META.is_file(), "no _site — run `make build`")
    def test_indexed_values_are_filterable(self) -> None:
        # Every page the footer gives a stability (those with a status) must
        # carry it in the index: read from front matter, which no page sets,
        # it was missing from all of them, and the stability filter hid
        # every page. And each ordinal value must be on its scale, or no
        # filter at any level can match it.
        meta = json.loads(SEARCH_META.read_text(encoding="utf-8"))
        self.assertTrue(meta)
        scales = js_scales()
        bad = []
        for url, fields in sorted(meta.items()):
            if "status" in fields and "stability" not in fields:
                bad.append(f"{url}: status but no stability")
            for field in scales:
                if field in fields and fields[field].lower() not in self.vocab[field]:
                    bad.append(f"{url}: {field} {fields[field]!r}")
        self.assertEqual(bad, [])


if __name__ == "__main__":
    unittest.main()
