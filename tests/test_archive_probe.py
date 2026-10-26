"""tools/archive.py's link-rot probe (audit C03).

A live link marked rotted is swapped for the local copy on every page that
cites it, so the probe must not count a server's quirk as a failure.
"""

import io
import unittest
import urllib.error
from unittest import mock
from tests._helpers import load_tool

archive = load_tool("archive.py", "archive_tool")
# Its User-Agent names the site, from site.yaml through the generator,
# which these tests do without.
archive.user_agent = lambda: "archive-test"

URL = "https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.203.pdf"


class Response(io.BytesIO):
    def __init__(self, url):
        super().__init__(b"%")
        self.url = url

    def geturl(self):
        return self.url


def server(head, get):
    """A urlopen that answers HEAD and GET as given: a status code to raise,
    or None to succeed."""
    def urlopen(req, timeout=None):
        code = head if req.get_method() == "HEAD" else get
        if code is not None:
            raise urllib.error.HTTPError(req.full_url, code, "", {}, None)
        return Response(req.full_url)
    return urlopen


class ProbeTests(unittest.TestCase):
    def probe(self, head, get):
        with mock.patch.object(archive.urllib.request, "urlopen", server(head, get)):
            return archive.probe_url(URL)

    def test_a_404_to_head_is_confirmed_with_get(self):
        # nvlpubs.nist.gov: 404 to HEAD, 206 to a ranged GET.
        self.assertEqual(self.probe(404, None), ("ok", None))

    def test_head_refusals_still_fall_back(self):
        for code in (403, 405, 501):
            with self.subTest(code=code):
                self.assertEqual(self.probe(code, None), ("ok", None))

    def test_a_link_that_fails_both_ways_fails(self):
        self.assertEqual(self.probe(404, 404), ("fail", None))
        self.assertEqual(self.probe(410, 500), ("fail", None))

    def test_a_working_head_needs_no_get(self):
        self.assertEqual(self.probe(None, 500), ("ok", None))


if __name__ == "__main__":
    unittest.main()
