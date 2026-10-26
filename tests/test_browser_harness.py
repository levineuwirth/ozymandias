"""The browser harness's own probes and guards, on cases whose answer is
known.

tests/test_browser_a11y.py fails a Tab stop that tools/browser/kbd_run.py
finds clipped: inside a box that clips its overflow and leaves none of it
showing. Which boxes clip an element depends on its positioning, and a
probe that guesses wrong either fails stops a reader can see (it called
every stop on the score reader clipped) or passes ones nobody can (a
fixed control inside a transformed, collapsed box). Each case here says
what the probe must answer, and the browser's own hit test at the
element's centre must agree, so the expectation cannot be wrong either.

A run against the live site (csp_run.py --base, prod_smoke.py) must send
no CSP report: the production report log is the evidence for the policy.
Aborting /csp-report kept Chromium's reports back, but Firefox sent its
own where routing never sees them. lib.suppress_reports strips the report
endpoint from each document's policies instead; here a page that breaks
the enforcing policy, served with its report log, shows that without the
guard reports arrive, and with it none does, while the violation is still
recorded and the policy still enforced.

In Chromium and Firefox.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_harness -v
"""

from __future__ import annotations

import importlib.util
import os
import tempfile
import time
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, HARNESS, check_site, enforcing_csp, harness_constant,
                            require_playwright, requires_browser, site_server)

# name: (markup, clipped). Each holds one focusable, #t-<name>; the fixed
# ones sit apart, so no case covers another's centre.
CASES = {
    "collapsed box": ('<div class="shut"><a id="t-collapsed-box" href="#x">a</a></div>', True),
    "open box": ('<div class="open"><a id="t-open-box" href="#x">a</a></div>', False),
    "fixed, viewport block": (
        '<div class="shut"><button id="t-fixed-viewport-block" class="fix" style="left: 20px">b</button></div>',
        False),
    "fixed, the box transformed": (
        '<div class="shut" style="transform: translateZ(0)">'
        '<button id="t-fixed-the-box-transformed" class="fix" style="left: 140px">b</button></div>', True),
    "fixed, transformed inside the box": (
        '<div class="shut"><div style="transform: translateX(0)">'
        '<button id="t-fixed-transformed-inside-the-box" class="fix" style="left: 260px">b</button></div></div>',
        True),
    "fixed, the box filtered": (
        '<div class="shut" style="filter: blur(0)">'
        '<button id="t-fixed-the-box-filtered" class="fix" style="left: 380px">b</button></div>', True),
    "fixed, the box contained": (
        '<div class="shut" style="contain: paint">'
        '<button id="t-fixed-the-box-contained" class="fix" style="left: 500px">b</button></div>', True),
    "absolute, the box positioned": (
        '<div class="shut" style="position: relative">'
        '<a id="t-absolute-the-box-positioned" class="abs" href="#x">a</a></div>', True),
    "absolute, its block above the box": (
        '<div style="position: relative; height: 40px"><div class="shut">'
        '<a id="t-absolute-its-block-above-the-box" class="abs" href="#x">a</a></div></div>', False),
}

PAGE = """<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Probe fixture</title>
<style>
  body {{ margin: 0; padding: 400px 20px 20px; }}
  .shut {{ height: 0; overflow: hidden; margin: 20px 0; }}
  .open {{ overflow: hidden; margin: 20px 0; }}
  .fix {{ position: fixed; top: 300px; width: 100px; height: 30px; }}
  .abs {{ position: absolute; top: 0; left: 0; }}
</style></head><body>
{cases}
</body></html>
"""

# What the browser shows at the element's centre: the element, or another.
HIT = """el => { const r = el.getBoundingClientRect();
    const at = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);
    return !!at && (at === el || el.contains(at)); }"""


def target(name: str) -> str:
    return "#t-" + name.replace(",", "").replace(" ", "-")


@requires_browser
class FocusProbe(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import sync_playwright
        cls.info = harness_constant("kbd_run.py", "FOCUS_INFO")
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-harness-")))
        (tmp / "fixtures").mkdir()
        (tmp / "fixtures" / "probe.html").write_text(
            PAGE.format(cases="\n".join(markup for markup, _ in CASES.values())), encoding="utf-8")
        cls.base = cls.enterClassContext(site_server(tmp, fixtures=tmp / "fixtures"))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def test_clipped(self) -> None:
        for browser in BROWSERS:
            page = self.browsers[browser].new_page(viewport={"width": 1000, "height": 800})
            self.addCleanup(page.close)
            page.goto(self.base + "/__fixture/probe.html", wait_until="load")
            for name, (_, clipped) in CASES.items():
                with self.subTest(browser=browser, case=name):
                    el = page.locator(target(name))
                    el.focus()
                    self.assertEqual(el.evaluate(HIT), not clipped, "the fixture's own premise")
                    got = page.evaluate(self.info)
                    self.assertEqual(bool(got["clipped"]), clipped, got)



# Breaks the enforcing policy (script-src: no inline script), and says so
# if the policy failed to stop it.
VIOLATING = """<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Report fixture</title></head><body>
<p>An inline script, which the policy refuses.</p>
<script>window.__ran = 1;</script>
</body></html>
"""


@requires_browser
class ReportGuard(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import sync_playwright
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-guard-")))
        (tmp / "fixtures").mkdir()
        (tmp / "fixtures" / "violating.html").write_text(VIOLATING, encoding="utf-8")
        cls.log = tmp / "csp-reports.jsonl"
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp(), tmp / "fixtures"))
        # lib makes its output directories on import: in tmp, not the repo.
        saved = os.environ.get("BROWSER_OUT")
        os.environ["BROWSER_OUT"] = str(tmp / "out")
        try:
            spec = importlib.util.spec_from_file_location("browser_lib", HARNESS / "lib.py")
            cls.lib = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(cls.lib)
        finally:
            if saved is None:
                del os.environ["BROWSER_OUT"]
            else:
                os.environ["BROWSER_OUT"] = saved
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def reports(self) -> int:
        if not self.log.exists():
            return 0
        return sum('"csp-report"' in line for line in self.log.read_text().splitlines())

    def visit(self, browser: str, guarded: bool) -> dict:
        context = self.browsers[browser].new_context()
        self.addCleanup(context.close)
        context.add_init_script(self.lib.CSP_INIT)
        if guarded:
            self.lib.suppress_reports(context)
        page = context.new_page()
        page.goto(self.base + "/__fixture/violating.html", wait_until="load")
        page.wait_for_timeout(300)
        return {"violations": page.evaluate("window.__cspv.length"),
                "ran": page.evaluate("window.__ran === 1")}

    def wait_for(self, more_than: int, seconds: float) -> int:
        for _ in range(int(seconds * 10)):
            if self.reports() > more_than:
                break
            time.sleep(0.1)
        return self.reports()

    def test_no_report_leaves_a_guarded_browser(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser, guard="none: the fixture reports"):
                before = self.reports()
                seen = self.visit(browser, guarded=False)
                self.assertGreater(seen["violations"], 0)
                self.assertFalse(seen["ran"], "the policy did not stop the inline script")
                self.assertGreater(self.wait_for(before, 10), before, "no report reached the log")
            with self.subTest(browser=browser, guard="suppress_reports"):
                before = self.reports()
                seen = self.visit(browser, guarded=True)
                self.assertGreater(seen["violations"], 0, "the violation went unrecorded")
                self.assertFalse(seen["ran"], "the guard dropped the policy, not its report endpoint")
                # Firefox sends its reports a moment later: give it time.
                self.assertEqual(self.wait_for(before, 4), before, "a report reached the log")


if __name__ == "__main__":
    unittest.main()
