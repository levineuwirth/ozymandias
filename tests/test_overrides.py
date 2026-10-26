"""The site's overrides of engine files (overrides.yaml).

tools/check-identity.py already fails when a declared override's site or
engine file is missing, or a site file shadows an engine file without an
entry. This is the review: each entry records the blob id of the engine
file the override was last checked against, so an engine change fails here
until someone has looked at it (git diff <reviewed> <current>), brought the
override along if it needs it, and recorded the new id.
"""
import hashlib
import unittest

import yaml

from tests._helpers import SITE_ROOT, engine_path


def blob_id(data: bytes) -> str:
    """The id Git gives a file's contents (git hash-object)."""
    return hashlib.sha1(b"blob %d\0" % len(data) + data).hexdigest()


class OverrideTests(unittest.TestCase):
    def test_each_override_was_reviewed_against_the_engine_file_it_replaces(self):
        entries = yaml.safe_load((SITE_ROOT / "overrides.yaml").read_text()) or []
        unreviewed = [f"{e['site']}: reviewed {e['reviewed']}, engine now {current}"
                      for e in entries
                      if (current := blob_id(engine_path(e["engine"]).read_bytes())) != e["reviewed"]]
        self.assertEqual(unreviewed, [])


if __name__ == "__main__":
    unittest.main()
