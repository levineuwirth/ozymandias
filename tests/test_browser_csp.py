"""Every route under the enforcing CSP, in Chromium and Firefox.

tools/browser/csp_run.py visits the 43 routes in tools/browser/lib.py (every
page type and the 404 page), scrolls through each, exercises what the policy governs (math,
both search tabs, the photography map, link popups, the PDF.js viewer
with its thumbnails and print output, archive snapshots, the score reader, the lightbox), and records CSP
violations, page errors and responses. Here it runs against
tools/browser/serve.py, the production vhost's emulation, with the
enforcing `add_header Content-Security-Policy` line from
nginx/security-headers.conf, and these tests assert on what it recorded.
This was the check behind promoting that policy (2026-10-09), and is the
one to rerun before changing it.

It needs the network: popups, map tiles and semantic search's
transformers.js come from their real origins, as in production. Popups are
hovered so that their requests meet the policy; whether each provider
shows its popup is for a popup test, not this sweep.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_csp -v
"""

from __future__ import annotations

import json
import re
import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, SITE_ROOT, check_site, enforcing_csp, harness_routes,
                            require_playwright, requires_browser, run_harness, site_server)
from tests.target import route, routes

ROUTES = harness_routes()
NOT_FOUND = route("not-found")   # its own document is meant to 404

# Each browser's routes are split across csp_run.py processes (SHARDS
# below), all running at once; the whole sweep gets SWEEP_SECONDS.
# Chromium's sweep costs about twice Firefox's per route, so it has more
# shards. Routes are dealt longest first to the least loaded shard, by what
# each cost last time (recorded below; evenly the first time): the fixed
# thirds left one Chromium shard 222 s against others' 82-160 s.
SHARDS = {"chromium": 4, "firefox": 2}
COSTS = SITE_ROOT / ".browser-runs" / "csp-route-secs.json"


def shards(browser: str, names: list[str]) -> list[list[str]]:
    try:
        cost = json.loads(COSTS.read_text()).get(browser, {})
    except (OSError, ValueError):
        cost = {}
    load, dealt = [0.0] * SHARDS[browser], [[] for _ in range(SHARDS[browser])]
    for name in sorted(names, key=lambda r: -cost.get(r, 10.0)):
        i = load.index(min(load))
        dealt[i].append(name)
        load[i] += cost.get(name, 10.0)
    return dealt
SWEEP_SECONDS = 1800

# The 404 route is the only one whose document is not a 200.
STATUS = {"404": 404}

# Violations the policy is meant to produce, by route.
EXPECTED_VIOLATIONS = {
    # transformers.js tests once whether eval works and carries on without
    # it; WebAssembly ('wasm-unsafe-eval') is all semantic search needs
    # (nginx/security-headers.conf).
    "search": lambda v: (v.get("effective") == "script-src" and v.get("blocked") == "eval"
                         and (v.get("source") or "").startswith(
                             "https://cdn.jsdelivr.net/npm/@xenova/transformers@")),
}


@requires_browser
class CspSweep(unittest.TestCase):
    results: dict[str, dict[str, dict]]

    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-csp-")))
        out = tmp / "runs"
        base = cls.enterClassContext(site_server(tmp, enforcing_csp()))
        names = list(ROUTES)
        port = base.rsplit(":", 1)[1]
        # Online: this sweep is the one meant to meet the live origins.
        dealt = {b: shards(b, names) for b in BROWSERS}
        run_harness(cls, base, out, [["csp_run.py", browser, port, f"{browser}-{i}.json", *routes]
                                     for browser in BROWSERS for i, routes in enumerate(dealt[browser])],
                    offline=False, seconds=SWEEP_SECONDS)
        cls.results = {b: {} for b in BROWSERS}
        for browser in BROWSERS:
            for i in range(SHARDS[browser]):
                cls.results[browser].update(json.loads((out / f"{browser}-{i}.json").read_text()))
        # What each route cost, for the next sweep's shards.
        COSTS.parent.mkdir(exist_ok=True)
        COSTS.write_text(json.dumps({b: {r: rec.get("secs") or 0 for r, rec in cls.results[b].items()}
                                     for b in BROWSERS}, indent=1, sort_keys=True) + "\n")

    def visited(self) -> list[tuple[str, str, dict]]:
        """(browser, route, record) for each route visited in each browser;
        test_every_route_loads reports the others."""
        return [(b, route, self.results[b][route])
                for b in BROWSERS for route in ROUTES if route in self.results[b]]

    def test_every_route_loads(self) -> None:
        for browser in BROWSERS:
            for route, path in ROUTES.items():
                with self.subTest(browser=browser, route=route):
                    rec = self.results[browser].get(route)
                    self.assertIsNotNone(rec, "not visited")
                    self.assertNotIn("goto_err", rec)
                    self.assertEqual(rec["status"], STATUS.get(route, 200), path)
                    self.assertNotIn("features_err", rec)

    def test_no_csp_violations(self) -> None:
        for browser, route, rec in self.visited():
            with self.subTest(browser=browser, route=route):
                expected = EXPECTED_VIOLATIONS.get(route, lambda v: False)
                self.assertEqual([v for v in rec["csp"] if not expected(v)], [])

    def test_no_page_errors(self) -> None:
        for browser, route, rec in self.visited():
            with self.subTest(browser=browser, route=route):
                self.assertEqual(rec["pageerrors"], [])
                self.assertEqual(rec["jserrs"], [])

    def test_no_elements_fail_to_load(self) -> None:
        """An img, script or stylesheet that fires `error`: blocked by the
        policy, missing, or cleared with src=''."""
        for browser, route, rec in self.visited():
            with self.subTest(browser=browser, route=route):
                self.assertEqual(rec["reserrs"], [])

    def test_no_failed_same_origin_responses(self) -> None:
        for browser, route, rec in self.visited():
            with self.subTest(browser=browser, route=route):
                bad = [r for r in rec["bad_status"] if r["url"].startswith("http://127.0.0.1:")
                       # serve.py has no upstream for the popup proxy, and the
                       # 404 route's own document is meant to be one.
                       and "/proxy/" not in r["url"]
                       and not (ROUTES[route] == NOT_FOUND and r["url"].endswith(NOT_FOUND))]
                self.assertEqual(bad, [])

    def test_images_math_and_transclusions_render(self) -> None:
        for browser, route, rec in self.visited():
            f = rec.get("features")
            if f is None:
                continue  # features_err, reported by test_every_route_loads
            with self.subTest(browser=browser, route=route):
                self.assertEqual(f["imgs_broken"], [])
                self.assertEqual(f["katex_err"], 0)
                self.assertGreaterEqual(f["katex"], f["math_raw"], "math left unrendered")
                self.assertEqual(f["transclude_unfilled"], 0)

    def test_features(self) -> None:
        """What the Promotion note in nginx/security-headers.conf says to
        re-test, in each browser, on the pages routes.yaml names for each
        (its probes)."""
        probe = routes().probe
        for browser in BROWSERS:
            got = {route: rec.get("features", {}) for route, rec in self.results[browser].items()}

            def check(feature: str, route: str):
                return self.subTest(browser=browser, feature=feature, route=route)

            for route in probe("search"):
                with check("keyword search", route):
                    self.assertGreater(got[route]["search"].get("keyword_results", 0), 0,
                                       got[route]["search"])
                with check("semantic search", route):
                    self.assertGreater(got[route]["search"].get("semantic_results", 0), 0,
                                       got[route]["search"])
            for route in probe("map"):
                with check("map tiles and markers", route):
                    m = got[route]["map"]
                    self.assertGreater(m["tilesLoaded"], 0, m)
                    self.assertGreater(m["markers"], 0, m)
                    self.assertIsNone(m["err"])
            for route in probe("pdf-viewer"):
                pdf = got[route]["pdf"]
                with check("PDF.js viewer", route):
                    self.assertGreater(pdf["canvases"], 0, pdf)
                    self.assertGreater(pdf["text"], 0, pdf)
                    self.assertIsNone(pdf["err"])
                with check("PDF.js thumbnails (blob: images)", route):
                    self.assertNotIn("thumbs_err", pdf)
                    self.assertGreater(pdf["thumbs_loaded"], 0, pdf)
                with check("PDF.js print output (blob: images)", route):
                    self.assertNotIn("print_err", pdf)
                    self.assertEqual(pdf["print_loaded"], pdf["pages"], pdf)
            for route in probe("archive-snapshot"):
                with check("archive snapshot", route):
                    frames = [f["url"] for f in got[route]["iframe"]]
                    self.assertTrue(any(u.endswith(ROUTES[route] + "snapshot.html") for u in frames), frames)
            for route in probe("score"):
                with check("score pages", route):
                    self.assertGreater(got[route]["score"]["stage"]["svgs"], 0)
                with check("score audio and turning", route):
                    score = got[route]["score"]
                    self.assertNotIn("audio_err", score)
                    self.assertEqual(score["audio"]["pressed"], "true", score["audio"])
                    self.assertNotIn("next_err", score)
                    before, after = (re.fullmatch(r"(\d+) / (\d+)", score[k] or "")
                                     for k in ("before_next", "after_next"))
                    self.assertTrue(before and after, score)
                    self.assertEqual(int(after[1]), int(before[1]) + 1, score)
            for route in probe("lightbox"):
                with check("lightbox", route):
                    self.assertTrue(got[route].get("lightbox_img"), got[route])
                    self.assertTrue(got[route].get("lightbox", {}).get("visible"), got[route])
            for route in probe("slideshow"):
                with check("slideshow", route):
                    s = got[route]
                    self.assertTrue(s.get("slideshow_btn"), s)
                    self.assertNotIn("slideshow_err", s)
                    self.assertTrue(s["slideshow"]["open"] and s["slideshow"]["ok"], s["slideshow"])
            for route in probe("commonplace"):
                with check("commonplace view toggle", route):
                    self.assertNotIn("cp_err", got[route])
                    self.assertEqual(got[route]["cp"], {"chrono": True, "themed": False})
            for route in probe("composition"):
                with check("composition images", route):
                    c = got[route]["composition"]
                    self.assertTrue(c["thumb"] and c["thumb"]["loaded"], c)
                    self.assertTrue(c["thumb"]["src"].endswith("/scores/thumb.webp"), c)
                    self.assertEqual(c["broken"], [])
            for route in probe("transclusion"):
                with check("transclusion", route):
                    loaded = [t["cls"] for t in got[route]["transclude_after"]]
                    self.assertTrue(loaded and all("transclude--loaded" in c for c in loaded), loaded)
            for route in probe("math"):
                with check("math", route):
                    self.assertGreater(got[route]["katex"], 0)


if __name__ == "__main__":
    unittest.main()
