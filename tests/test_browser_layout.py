"""Layout: sideways scrolling on every route; reduced motion, layout shift
and page weight on the pages the harness lists for them.

The tools/browser report scripts run offline against serve.py, which serves
the .br/.gz sidecars as nginx does, and these tests assert on what they
recorded, once each report holds a record for every page and variant asked
of it:

- overflow_run.py, on every route, in Chromium and Firefox at 320, 375,
  768 and 1440px, and in Chromium at 320 and 1440px with the settings
  panel's smallest and largest text (17 and 29px): no page may scroll
  sideways (WCAG 1.4.10 asks for 320px). Known exceptions are recorded in
  tests/browser-baseline/overflow.json, those at a chosen size with it
  ("pdfjs|320|29px").
- motion_run.py, on 14 pages (its MROUTES), in both browsers, with reduced
  motion asked for by the system or by the site's own setting: no
  animation, no transition, no smooth scrolling, and the slideshow does
  not start by itself.
- perf_run.py, on 21 pages (its PROUTES), in Chromium at 1440 and 375px:
  cumulative layout shift under 0.1; each page's requests and transferred
  bytes within 15% of tests/browser-baseline/budgets.json (a page that
  grows must be recorded; one that shrinks need not be); and the origins
  other than the site's own that each page asks for, as recorded in
  outside.json (offline, those requests are refused, so the bytes are
  the site's own).

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_layout -v
    UPDATE_BROWSER_BASELINE=1 RUN_BROWSER_TESTS=1 ...   # record what is found
"""

from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, UPDATE_BASELINE, assert_complete, baseline, check_site,
                            harness_constant, harness_routes, require_playwright, requires_browser,
                            run_harness, site_server)

ROUTES = harness_routes()
OVERFLOW_WIDTHS = harness_constant("overflow_run.py", "WIDTHS")
OVERFLOW_HALVES = (OVERFLOW_WIDTHS[:len(OVERFLOW_WIDTHS) // 2], OVERFLOW_WIDTHS[len(OVERFLOW_WIDTHS) // 2:])
MOTION_ROUTES = harness_constant("motion_run.py", "MROUTES")
MOTION_MODES = harness_constant("motion_run.py", "MODES")
PERF_ROUTES = harness_constant("perf_run.py", "PROUTES")
WIDTHS = (1440, 375)
TEXT_SIZES = (17, 29)       # the settings panel's ends (lnUtils.TEXT_SIZE)
TEXT_SIZE_WIDTHS = (320, 1440)
GROWTH = 1.15          # a page may grow this much before it fails its budget
SLACK = {"requests": 2, "kB": 10}   # and this much on a small page
CLS_GOOD = 0.1         # web.dev's line for a good cumulative layout shift


@requires_browser
class Layout(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-layout-")))
        out = tmp / "runs"
        base = cls.enterClassContext(site_server(tmp, compress=True))
        run_harness(cls, base, out,
                    # Each browser's widths in two halves, run side by side
                    # (one run of all four was the module's slowest job).
                    [["overflow_run.py", b, f"overflow-{b}-{i}.json", "--widths=" + ",".join(map(str, ws))]
                     for b in BROWSERS for i, ws in enumerate(OVERFLOW_HALVES)]
                    + [["overflow_run.py", "chromium", f"overflow-{px}px.json", f"--text-size={px}",
                        "--widths=" + ",".join(map(str, TEXT_SIZE_WIDTHS))] for px in TEXT_SIZES]
                    + [["motion_run.py", b, f"motion-{b}.json"] for b in BROWSERS]
                    + [["perf_run.py", f"perf-{w}.json", str(w)] for w in WIDTHS])
        load = lambda name: json.loads((out / name).read_text())
        # (browser, text size or None for the default) -> report
        cls.overflow = {(b, None): {k: v for i in range(len(OVERFLOW_HALVES))
                                    for k, v in load(f"overflow-{b}-{i}.json").items()}
                        for b in BROWSERS}
        cls.overflow.update({("chromium", px): load(f"overflow-{px}px.json") for px in TEXT_SIZES})
        cls.motion = {b: load(f"motion-{b}.json") for b in BROWSERS}
        cls.perf = {w: load(f"perf-{w}.json") for w in WIDTHS}

    def test_no_sideways_scrolling(self) -> None:
        def label(key: str, px) -> str:
            return key if px is None else f"{key}|{px}px"

        for (browser, px), report in self.overflow.items():
            widths = OVERFLOW_WIDTHS if px is None else TEXT_SIZE_WIDTHS
            assert_complete(self, report, {f"{r}|{w}" for r in ROUTES for w in widths},
                            f"overflow {browser} {label('', px)}")
        found = sorted({label(key, px) for (_, px), report in self.overflow.items()
                        for key, r in report.items() if r.get("over")})
        known = baseline("overflow", found)
        if UPDATE_BASELINE:
            return
        for (browser, px), report in sorted(self.overflow.items(), key=str):
            for key, r in sorted(report.items()):
                with self.subTest(browser=browser, text_size=px or "default", page=key):
                    self.assertNotIn("error", r)
                    if label(key, px) not in known:
                        self.assertFalse(r["over"], f"{r['sw']}px wide in {r['cw']}px: "
                                                    f"{[o['el'] for o in r['offenders']]}")
        with self.subTest(part="known overflows still overflow"):
            self.assertEqual([k for k in known if k not in found], [],
                             "fixed: record it (UPDATE_BROWSER_BASELINE=1)")

    def test_reduced_motion(self) -> None:
        for browser in BROWSERS:
            assert_complete(self, self.motion[browser],
                            {f"{r}|{m}" for r in MOTION_ROUTES for m in MOTION_MODES}, f"motion {browser}")
        for browser in BROWSERS:
            for key, r in sorted(self.motion[browser].items()):
                if key.endswith("|none"):
                    continue
                with self.subTest(browser=browser, page=key):
                    self.assertNotIn("error", r)
                    self.assertEqual(r["anim"], [], "animations")
                    self.assertEqual(r["nTrans"], 0, f"transitions: {r['trans']}")
                    self.assertEqual(r["scrollBehavior"], "auto")
                    self.assertFalse(r.get("smooth"), "smooth scrolling")
                    self.assertFalse(r.get("webAnims"), "script animations")
                    if key.startswith("photo-series|"):
                        self.assertIsNotNone(r["slideshow"], "no slideshow button")
                        self.assertIn("Play slideshow", r["slideshow"],
                                      "the slideshow started by itself")

    def assert_perf_complete(self) -> None:
        for width in WIDTHS:
            assert_complete(self, self.perf[width], PERF_ROUTES, f"perf {width}")

    def test_layout_shift(self) -> None:
        self.assert_perf_complete()
        for width in WIDTHS:
            for route, r in sorted(self.perf[width].items()):
                with self.subTest(width=width, route=route):
                    self.assertLess(r["cls"], CLS_GOOD)

    def test_page_weight(self) -> None:
        self.assert_perf_complete()
        found = {f"{route}|{w}": {"requests": r["requests"], "kB": r["kB"]}
                 for w in WIDTHS for route, r in self.perf[w].items()}
        budgets = baseline("budgets", found)
        if UPDATE_BASELINE:
            return
        for key, now in sorted(found.items()):
            with self.subTest(page=key):
                self.assertIn(key, budgets, "no budget: record one (UPDATE_BROWSER_BASELINE=1)")
                for measure in ("requests", "kB"):
                    allowed = max(budgets[key][measure] * GROWTH,
                                  budgets[key][measure] + SLACK[measure])
                    self.assertLessEqual(now[measure], allowed,
                                         f"{measure}: {now[measure]}, budget {budgets[key][measure]}")

    def test_outside_origins(self) -> None:
        # The page-weight test used to check that no bytes came from
        # outside, which offline they never can: the requests are refused.
        # What a page asks for is what tells.
        self.assert_perf_complete()
        found = {}
        for width in WIDTHS:
            for route, r in self.perf[width].items():
                found.setdefault(route, set()).update(r["outside"])
        known = baseline("outside", {r: sorted(o) for r, o in sorted(found.items()) if o})
        if UPDATE_BASELINE:
            return
        for route in sorted(PERF_ROUTES):
            with self.subTest(route=route):
                now, was = found[route], set(known.get(route, []))
                self.assertEqual(sorted(now - was), [], "asks a new origin")
                self.assertEqual(sorted(was - now), [], "no longer asked: record it "
                                                        "(UPDATE_BROWSER_BASELINE=1)")


if __name__ == "__main__":
    unittest.main()
