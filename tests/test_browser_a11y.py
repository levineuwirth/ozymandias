"""Accessibility and the keyboard, every route, against the known issues.

tools/browser/axe_run.py runs axe-core (the version tools/browser/
axe-version records; WCAG 2.0-2.2 A and AA rules and best practices) on
every route but PDF.js's viewer (a vendored application, not the site's
markup; axe_run.py's SKIP), at two widths in the light and dark themes,
and in the light theme with the settings panel's smallest and largest
text (17 and 29px), in Chromium; tools/browser/kbd_run.py walks the first forty Tab stops of
the 16 pages a reader moves through by keyboard (its KROUTES), at two
widths, in Chromium and Firefox. Both run offline against serve.py, and
each report must hold a record for every page and variant asked of it.

axe findings are recorded per route and rule, as the most elements any
width or theme showed (tests/browser-baseline/axe.json). A rule on a
route where it is not recorded, or on more elements than recorded, fails:
a new problem. So does a recorded one that is gone or smaller: a fix,
whose record should follow it. The keyboard has no known issues: every
page's first stop is a skip link that lands in its target, and no stop is
hidden, aria-hidden, inert, invisible, clipped (inside a collapsed
container), out of view, or without a visible change.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_a11y -v
    UPDATE_BROWSER_BASELINE=1 RUN_BROWSER_TESTS=1 ...   # record what is found
"""

from __future__ import annotations

import json
import tempfile
import unittest
from collections import defaultdict
from pathlib import Path

from tests._browser import (BROWSERS, UPDATE_BASELINE, assert_complete, baseline, check_site,
                            harness_constant, harness_routes, require_axe, require_playwright,
                            requires_browser, run_harness, site_server)

ROUTES = harness_routes()
SHARDS = 3
THEMES = ("light", "dark")
VIEWPORTS = ("1440x1000", "375x812")
AXE_ROUTES = [r for r in ROUTES if r not in harness_constant("axe_run.py", "SKIP")]
TEXT_SIZES = (17, 29)       # the settings panel's ends (lnUtils.TEXT_SIZE)
KBD_ROUTES = harness_constant("kbd_run.py", "KROUTES")
KBD_WIDTHS = [w for w, _ in harness_constant("kbd_run.py", "VIEWPORTS")]


@requires_browser
class Accessibility(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        require_axe()
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-a11y-")))
        out = tmp / "runs"
        base = cls.enterClassContext(site_server(tmp))
        names = list(ROUTES)
        run_harness(cls, base, out,
                    [["axe_run.py", "chromium", f"axe-{i}.json", ",".join(THEMES),
                      ",".join(VIEWPORTS), *names[i::SHARDS]] for i in range(SHARDS)]
                    + [["axe_run.py", "chromium", f"axe-{px}px-{i}.json", "light", ",".join(VIEWPORTS),
                        f"--text-size={px}", *names[i::2]] for px in TEXT_SIZES for i in range(2)]
                    + [["kbd_run.py", b, f"kbd-{b}.json"] for b in BROWSERS])
        cls.axe = {}
        for i in range(SHARDS):
            cls.axe.update(json.loads((out / f"axe-{i}.json").read_text()))
        # Each a variant of its route like a width or a theme: route|vp|theme|size.
        cls.axe_sizes = {px: {k: v for i in range(2)
                              for k, v in json.loads((out / f"axe-{px}px-{i}.json").read_text()).items()}
                         for px in TEXT_SIZES}
        for px, report in cls.axe_sizes.items():
            cls.axe.update({f"{key}|{px}px": result for key, result in report.items()})
        cls.kbd = {b: json.loads((out / f"kbd-{b}.json").read_text()) for b in BROWSERS}

    def test_axe_against_the_known_issues(self) -> None:
        assert_complete(self, self.axe, {f"{r}|{vp}|{t}" for r in AXE_ROUTES
                                         for vp in VIEWPORTS for t in THEMES}
                        | {f"{r}|{vp}|light|{px}px" for r in AXE_ROUTES
                           for vp in VIEWPORTS for px in TEXT_SIZES}, "axe")
        found = defaultdict(dict)    # route -> rule -> most elements in any variant
        detail = {}                  # (route, rule) -> what axe said, for the message
        for key, result in self.axe.items():
            route = key.split("|")[0]
            self.assertIsInstance(result, list, f"{key}: {result}")
            for v in result:
                if v["count"] > found[route].get(v["id"], 0):
                    found[route][v["id"]] = v["count"]
                    detail[route, v["id"]] = (key, v["impact"], v["help"],
                                              [n["target"] for n in v["nodes"]])
        known = baseline("axe", {r: dict(sorted(rules.items())) for r, rules in sorted(found.items())})
        if UPDATE_BASELINE:
            return
        for route in sorted(set(found) | set(known)):
            for rule in sorted(set(found.get(route, {})) | set(known.get(route, {}))):
                now, was = found.get(route, {}).get(rule, 0), known.get(route, {}).get(rule, 0)
                with self.subTest(route=route, rule=rule):
                    if now > was:
                        key, impact, help_, targets = detail[route, rule]
                        self.fail(f"{impact}: {help_} — {now} element(s), {was} known "
                                  f"(at {key}): {targets}")
                    self.assertEqual(now, was, "fewer than recorded: record the fix "
                                               "(UPDATE_BROWSER_BASELINE=1)")

    def test_keyboard(self) -> None:
        for browser in BROWSERS:
            assert_complete(self, self.kbd[browser],
                            {f"{r}|{w}" for r in KBD_ROUTES for w in KBD_WIDTHS}, f"kbd {browser}")
        for browser in BROWSERS:
            for key, rec in self.kbd[browser].items():
                with self.subTest(browser=browser, page=key):
                    self.assertNotIn("err", rec)
                    skip = rec["skip"]
                    self.assertTrue((skip.get("href") or "").startswith("#"), f"first stop: {skip}")
                    self.assertTrue(skip.get("target_exists"), skip)
                    self.assertTrue(skip.get("after", {}).get("inTarget"),
                                    f"Tab after the skip link left its target: {skip}")
                    bad = []
                    for s in rec["stops"]:
                        if s.get("body"):
                            continue
                        why = [w for w, hit in (
                            ("aria-hidden", s.get("ariaHidden")), ("hidden", s.get("hidden")),
                            ("inert", s.get("inert")), ("invisible", s.get("visible") is False),
                            ("clipped", s.get("clipped")),
                            ("out of view", not s.get("inView")),
                            ("no visible focus", not s.get("indicator_diff"))) if hit]
                        if why:
                            bad.append((s.get("desc"), s.get("name"), why))
                    self.assertEqual(bad, [])


if __name__ == "__main__":
    unittest.main()
