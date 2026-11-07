"""Reading across a deploy, in Chromium and Firefox, with a warm cache.

A reader may hold a page from before a deploy: in a cache, or in a tab left
open, whose scripts go on loading more of their own version on demand. So the
first-party JS and CSS are published under a version in the path
(EngineFiles.staticRules), cached as nginx says (nginx/static-assets.conf), and a
deploy leaves earlier versions on the server (tools/asset-versions.py).

Two builds of the fixture site, as far apart as a namespace rename makes them:
every script of the second calls the first's by other names, so a page that
mixes the two loses what those scripts do. The document root goes from one to
the other by the deploy's own rsync, and the server is the one the other browser
tests use.

Checked: a page opened after the deploy, with scripts and stylesheets cached
from before, runs only the new version; a tab open across the deploy goes on
loading its own; a reader during the deploy, with the new version on its way and
then some pages of it arrived, gets whole pages of either; and the same deploy
without versions does mix them, silently (the scripts are written to survive their
utilities missing): the now page's relative date is gone. That is why any of this
exists, and how this test knows it can see that failure. A page ahead of its
version, which the order of the deploy's transfers prevents, has a control too.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_deploy -v
"""

from __future__ import annotations

import json
import re
import shutil
import subprocess
import sys
import tempfile
import unittest
import urllib.request
from pathlib import Path

from tests import fixture_variants
from tests._browser import BROWSERS, enforcing_csp, require_playwright, requires_browser, site_server
from tests._helpers import ENGINE, SITE_ROOT, engine_path, requires_cabal, script_env, site_binary

TOOL = engine_path("tools/asset-versions.py")
ESSAY = "/essays/long/"
HOME = "/"
NOW = "/current.html"     # loads now.js, which the home page does not
# The scripts' shared names, as utils.js defines them; the second build's.
PREFIX = re.search(r"global\.(\w+)Utils\s*=", engine_path("static/js/utils.js").read_text(encoding="utf-8")).group(1)
OLD, NEW = f"{PREFIX}Utils", "renamedUtils"
RELATIVE_DATE = "document.querySelector('.now-stamp-relative')?.textContent ?? ''"


def rename_namespace(site: Path) -> None:
    """The second build: every script a site override, under the new names."""
    scripts = engine_path("static/js")
    for script in scripts.rglob("*.js"):
        renamed = re.sub(rf"\b{PREFIX}(?=[A-Z])", "renamed", script.read_text(encoding="utf-8"))
        target = site / "static/js" / script.relative_to(scripts)
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(renamed, encoding="utf-8")


def build(site: Path, change=None) -> Path:
    """The full fixture site, with `change`; its _site."""
    full = next(b for b in fixture_variants.builds() if b.name == "full")
    fixture_variants.copy_site(full, site, ENGINE)
    (site / "Makefile").write_text("include engine/engine.mk\n")
    if change:
        change(site)
    git = ["git", "-c", "user.email=fixture@example.org", "-c", "user.name=Fixture",
           "-c", "commit.gpgsign=false", "-c", "core.hooksPath=/dev/null"]
    for args in (["init", "-q"], ["add", "-A"], ["commit", "-qm", "the fixture site"]):
        subprocess.run(git + args, cwd=site, check=True, capture_output=True)
    binary = str(site_binary())
    env = script_env(SITE_ENV="production", SITE_ROOT=str(site), SITE_BINARY=binary)
    for args in ([binary, "footer-data"], [binary, "build"],
                 [str(engine_path("tools/pagefind-index.sh")), "_site"],
                 ["make", "--no-print-directory", f"PY={sys.executable}", "stamp-build-time"]):
        done = subprocess.run(args, cwd=site, env=env, capture_output=True, text=True, timeout=300)
        if done.returncode:
            raise AssertionError(f"{args}: {done.stdout[-2000:]}{done.stderr[-2000:]}")
    return site / "_site"


def unversioned(site: Path, into: Path) -> Path:
    """`site` as it was published before versions."""
    shutil.copytree(site, into)
    versioned = next((into / "assets").iterdir())
    root = f"assets/{versioned.name}/"
    for top in ("js", "css"):
        shutil.move(versioned / top, into / top)
    shutil.rmtree(into / "assets")
    for page in into.rglob("*.html"):
        html = page.read_text(encoding="utf-8")
        page.write_text(html.replace(f"/{root.rstrip('/')}", "").replace(root, ""), encoding="utf-8")
    return into


def nginx_cache_control(block: str) -> str:
    conf = (SITE_ROOT / "nginx/static-assets.conf").read_text(encoding="utf-8")
    found = re.search(re.escape(block) + r'\s*\{.*?add_header Cache-Control "([^"]+)"', conf, re.S)
    if not found:
        raise AssertionError(f"static-assets.conf has no Cache-Control under {block}")
    return found.group(1)


@requires_cabal
@requires_browser
class AcrossADeploy(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        require_playwright()
        from playwright.sync_api import sync_playwright
        cls.tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-deploy-")))
        cls.before = build(cls.tmp / "before")
        cls.after = build(cls.tmp / "after", rename_namespace)
        cls.docroot = cls.tmp / "www"
        cls.docroot.mkdir()
        cls.ledger = cls.tmp / "deployed-assets.json"
        cls.deploy(cls.before)
        cls.csp = enforcing_csp()
        cls.base = cls.enterClassContext(site_server(cls.tmp, cls.csp, root=cls.docroot, cache=True))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    @classmethod
    def deploy(cls, site: Path) -> None:
        """The deploy's own steps (Makefile: deploy-rsync-inplace)."""
        def tool(command: str) -> str:
            return subprocess.run([sys.executable, str(TOOL), command, str(site), "--ledger", str(cls.ledger)],
                                  check=True, capture_output=True, text=True).stdout
        rules = cls.tmp / "deploy-filter.txt"
        rules.write_text(tool("filter"))
        tool("attempt")
        cls.send_assets(site)
        subprocess.run(["rsync", "-a", "--delete", f"--filter=merge {rules}", f"{site}/", f"{cls.docroot}/"],
                       check=True, capture_output=True, text=True)
        tool("deployed")

    @classmethod
    def send_assets(cls, site: Path) -> None:
        """The first of the deploy's two transfers: its version, before any page."""
        subprocess.run(["rsync", "-aR", f"{site}/./assets/{cls.version(site)}/", f"{cls.docroot}/"],
                       check=True, capture_output=True, text=True)

    @staticmethod
    def version(site: Path) -> str:
        return subprocess.run([sys.executable, str(TOOL), "current", str(site)],
                              check=True, capture_output=True, text=True).stdout.strip()

    def setUp(self) -> None:
        self.deploy(self.before)

    def served(self, log: Path, start: int = 0) -> list[str]:
        """What the server has been asked for, since `start` lines of its log."""
        lines = log.read_text(encoding="utf-8").splitlines()[start:]
        return [entry["path"] for entry in map(json.loads, lines) if entry.get("kind") == "get"]

    def missing(self, log: Path, start: int = 0) -> list[str]:
        """What the server could not find since `start` lines of its log, fonts
        aside, which the fixture site has none of. Only the server can say: under
        nosniff, Chromium reports no response at all for a stylesheet answered
        with the 404 page."""
        lines = log.read_text(encoding="utf-8").splitlines()[start:]
        return [entry["path"] for entry in map(json.loads, lines)
                if entry.get("kind") == "404" and "/fonts/" not in entry["path"]]

    def assert_untroubled(self, trouble: list[str]) -> None:
        self.assertEqual(trouble + self.missing(self.log, self.opened), [])

    @property
    def log(self) -> Path:
        return self.tmp / "csp-reports.jsonl"

    def lines(self) -> int:
        return len(self.log.read_text(encoding="utf-8").splitlines())

    def open(self, browser: str):
        """A reader: a page in a context of its own, whose cache lasts as long
        as it does, and what has gone wrong on it."""
        context = self.browsers[browser].new_context()
        self.addCleanup(context.close)
        page = context.new_page()
        trouble: list[str] = []
        page.on("pageerror", lambda e: trouble.append(f"error: {e}"))
        # The fixture site has no font files.
        page.on("response", lambda r: trouble.append(f"{r.status}: {r.url}")
                if r.status >= 400 and r.url.startswith(self.base) and "/fonts/" not in r.url else None)
        self.opened = self.lines()
        return page, trouble

    def test_the_server_caches_as_nginx_says(self) -> None:
        version = self.version(self.before)
        with urllib.request.urlopen(f"{self.base}/assets/{version}/js/utils.js") as answer:
            self.assertEqual(answer.headers["Cache-Control"], nginx_cache_control("location ^~ /assets/"))

    def test_a_cached_script_is_not_asked_for_again(self) -> None:
        # What "warm" means below, or the rest shows nothing.
        version = self.version(self.before)
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, trouble = self.open(browser)
                start = self.lines()
                page.goto(self.base + HOME, wait_until="load")
                page.goto(self.base + ESSAY, wait_until="load")
                asked = self.served(self.log, start)
                self.assertEqual(asked.count(f"/assets/{version}/js/utils.js"), 1, asked)
                self.assertEqual(asked.count(f"/assets/{version}/css/base.css"), 1, asked)
                self.assert_untroubled(trouble)

    def test_a_page_opened_after_the_deploy_runs_only_the_new_version(self) -> None:
        old, new = self.version(self.before), self.version(self.after)
        self.assertNotEqual(old, new)
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                self.deploy(self.before)
                page, trouble = self.open(browser)
                page.goto(self.base + HOME, wait_until="load")
                page.goto(self.base + ESSAY, wait_until="load")
                self.assertTrue(page.evaluate(f"typeof window.{OLD} === 'object'"))
                self.deploy(self.after)
                start = self.lines()
                page.goto(self.base + ESSAY, wait_until="load")
                page.goto(self.base + HOME, wait_until="load")
                self.assertEqual(page.evaluate(f"typeof window.{OLD}"), "undefined")
                self.assertEqual(page.evaluate(f"typeof window.{NEW}"), "object")
                asked = self.served(self.log, start)
                self.assertIn(f"/assets/{new}/js/utils.js", asked)
                self.assertEqual([p for p in asked if p.startswith(f"/assets/{old}/")], [])
                self.assert_untroubled(trouble)

    def test_a_script_new_to_the_cache_finds_the_utilities_it_was_built_with(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                self.deploy(self.before)
                page, trouble = self.open(browser)
                page.goto(self.base + HOME, wait_until="load")
                self.deploy(self.after)
                page.goto(self.base + NOW, wait_until="load")
                page.wait_for_function(f"({RELATIVE_DATE}) !== ''")
                self.assert_untroubled(trouble)

    def test_a_tab_open_across_the_deploy_goes_on_loading_its_own_version(self) -> None:
        old, new = self.version(self.before), self.version(self.after)
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                self.deploy(self.before)
                page, trouble = self.open(browser)
                page.goto(self.base + ESSAY, wait_until="load")
                self.assertEqual(page.evaluate("typeof window.Prism"), "undefined",
                                 "the essay loaded Prism, so nothing is left to load on demand")
                self.deploy(self.after)
                self.assertTrue((self.docroot / f"assets/{new}/js/prism.min.js").is_file())
                start = self.lines()
                # What a transclusion brings late: a code block on a page with none.
                highlighted = page.evaluate("""async () => {
                    const box = document.createElement('div');
                    box.innerHTML = '<pre><code class="language-python">import this</code></pre>';
                    document.body.append(box);
                    await window.PREFIXHighlightCode(box);
                    return box.querySelector('code').querySelectorAll('.token').length;
                }""".replace("PREFIX", PREFIX))
                self.assertGreater(highlighted, 0)
                asked = self.served(self.log, start)
                self.assertIn(f"/assets/{old}/js/prism.min.js", asked)
                self.assertIn(f"/assets/{old}/css/syntax.css", asked)
                self.assertEqual([p for p in asked if p.startswith(f"/assets/{new}/")], [])
                self.assert_untroubled(trouble)

    def start_over(self) -> None:
        """A server that has only ever had the first build."""
        for entry in self.docroot.iterdir():
            if entry.is_dir():
                shutil.rmtree(entry)
            else:
                entry.unlink()
        self.ledger.unlink(missing_ok=True)
        self.deploy(self.before)

    def test_a_reader_during_the_deploy_gets_whole_pages_of_either_version(self) -> None:
        # rsync sends a directory's files before its subdirectories, so a deploy
        # that stops partway leaves the new home page beside old essays.
        old, new = self.version(self.before), self.version(self.after)
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                self.start_over()
                self.assertFalse((self.docroot / f"assets/{new}").exists())
                page, trouble = self.open(browser)
                page.goto(self.base + ESSAY, wait_until="load")
                self.send_assets(self.after)
                start = self.lines()
                page.goto(self.base + ESSAY, wait_until="load")
                self.assertEqual(page.evaluate(f"typeof window.{OLD}"), "object")
                self.assertEqual([p for p in self.served(self.log, start) if p.startswith(f"/assets/{new}/")], [])
                shutil.copy2(self.after / "index.html", self.docroot / "index.html")
                for path, name, other in ((HOME, NEW, OLD), (ESSAY, OLD, NEW), (HOME, NEW, OLD)):
                    page.goto(self.base + path, wait_until="load")
                    self.assertEqual(page.evaluate(f"typeof window.{name}"), "object", path)
                    self.assertEqual(page.evaluate(f"typeof window.{other}"), "undefined", path)
                self.assertTrue((self.docroot / f"assets/{old}/js/utils.js").is_file())
                self.assert_untroubled(trouble)
                # A reader with nothing cached asks the server for all of both.
                page, trouble = self.open(browser)
                for path, name in ((ESSAY, OLD), (HOME, NEW)):
                    page.goto(self.base + path, wait_until="load")
                    self.assertEqual(page.evaluate(f"typeof window.{name}"), "object", path)
                self.assert_untroubled(trouble)

    def test_a_page_ahead_of_its_version_is_what_going_first_prevents(self) -> None:
        # The control: the same moment, had the pages gone first.
        new = self.version(self.after)
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                tmp = self.tmp / f"ahead-{browser}"
                docroot = tmp / "www"
                docroot.mkdir(parents=True)
                subprocess.run(["rsync", "-a", "--delete", f"{self.before}/", f"{docroot}/"],
                               check=True, capture_output=True, text=True)
                shutil.copy2(self.after / "index.html", docroot / "index.html")
                with site_server(tmp, self.csp, root=docroot, cache=True) as base:
                    context = self.browsers[browser].new_context()
                    self.addCleanup(context.close)
                    page = context.new_page()
                    page.goto(base + HOME, wait_until="load")
                    missing = self.missing(tmp / "csp-reports.jsonl")
                    self.assertIn(f"/assets/{new}/css/base.css", missing)
                    self.assertIn(f"/assets/{new}/js/utils.js", missing)
                    self.assertEqual(page.evaluate(f"typeof window.{NEW}"), "undefined")

    def test_without_versions_the_same_deploy_mixes_them(self) -> None:
        tmp = self.tmp / "control"
        tmp.mkdir(exist_ok=True)
        before = unversioned(self.before, tmp / "before")
        after = unversioned(self.after, tmp / "after")
        docroot = tmp / "www"

        def deploy(site: Path) -> None:
            subprocess.run(["rsync", "-a", "--delete", f"{site}/", f"{docroot}/"],
                           check=True, capture_output=True, text=True)

        for browser in BROWSERS:
            with self.subTest(browser=browser):
                deploy(before)
                with site_server(tmp, self.csp, root=docroot, cache=True) as base:
                    context = self.browsers[browser].new_context()
                    self.addCleanup(context.close)
                    page = context.new_page()
                    # Before the deploy, the page has its date.
                    page.goto(base + NOW, wait_until="load")
                    page.wait_for_function(f"({RELATIVE_DATE}) !== ''")
                    context.close()
                    context = self.browsers[browser].new_context()
                    self.addCleanup(context.close)
                    page = context.new_page()
                    page.goto(base + HOME, wait_until="load")
                    deploy(after)
                    # now.js is new; the utilities it looks for are still fresh
                    # in the cache, under their old name.
                    page.goto(base + NOW, wait_until="load")
                    page.wait_for_timeout(500)
                    self.assertEqual(page.evaluate(f"typeof window.{OLD}"), "object")
                    self.assertEqual(page.evaluate(f"typeof window.{NEW}"), "undefined")
                    self.assertEqual(page.evaluate(RELATIVE_DATE), "")


if __name__ == "__main__":
    unittest.main()
