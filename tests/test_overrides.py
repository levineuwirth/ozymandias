"""The site's overrides of engine files (overrides.yaml), held to the
engine's contract (engine/templates/contract.yaml) by
tools/override-review.py.

tools/check-identity.py already fails when a declared override's site or
engine file is missing, or a site file shadows an engine file without an
entry. This is the review: each entry records the blob id of the engine
file the override was last checked against, so an engine change fails here
until someone has looked at it (`override-review.py` shows the diff),
brought the override along if it needs it, and recorded the new id
(`--accept`). And the contract, which no review clears: each template
override is an override point, renders with its fixture context, and keeps
the hooks the engine's scripts and styles need.
"""
import unittest

from tests._helpers import SITE_ROOT, load_tool, requires_cabal, site_binary

review = load_tool("override-review.py", "override_review")


class OverrideTests(unittest.TestCase):
    def test_each_override_was_reviewed_against_the_engine_file_it_replaces(self):
        unreviewed = [f"{e['site']}: reviewed {e['reviewed']}, engine now {current or 'gone'}"
                      for e, current in review.unreviewed(SITE_ROOT)]
        self.assertEqual(unreviewed, [])

    def test_each_template_override_is_an_override_point(self):
        points = set(review.contract()["templates"])
        self.assertEqual([e["site"] for e in review.manifest(SITE_ROOT)
                          if e["site"].startswith("templates/") and e["site"] not in points], [])

    @requires_cabal
    def test_each_template_override_keeps_the_contract(self):
        self.assertEqual(review.hook_problems(SITE_ROOT, str(site_binary())), [])


if __name__ == "__main__":
    unittest.main()
