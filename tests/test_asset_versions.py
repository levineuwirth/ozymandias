#!/usr/bin/env python3
"""tools/asset-versions.py: which versions of the first-party JS and CSS a
deploy leaves on the server, by real rsync into a local directory, as the
Makefile's deploy runs it.
"""

import datetime as dt
import json
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

from tests._helpers import engine_path, script_env

TOOL = engine_path("tools/asset-versions.py")
DAY = dt.date(2026, 1, 1)


def day(n: int) -> str:
    return (DAY + dt.timedelta(days=n)).isoformat()


class Case(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.site = self.root / "_site"
        self.ledger = self.root / "data/deployed-assets.json"

    def tool(self, command: str, on: int = 0, site: Path | None = None) -> subprocess.CompletedProcess:
        return subprocess.run([sys.executable, str(TOOL), command, str(site or self.site),
                               "--ledger", str(self.ledger), "--today", day(on)],
                              capture_output=True, text=True, env=script_env())

    def build(self, version: str, *also: str) -> None:
        """A site as a build leaves it: VERSION's files, and ALSO's."""
        shutil.rmtree(self.site, ignore_errors=True)
        self.site.mkdir()
        (self.site / "index.html").write_text(f'<head><meta name="ozy:assets" content="/assets/{version}"></head>')
        for rel in (f"assets/{version}/js/a.js", f"assets/{version}/css/a.css", *also):
            (self.site / rel).parent.mkdir(parents=True, exist_ok=True)
            (self.site / rel).write_text(version)


class Current(Case):
    def test_it_is_what_the_home_page_names(self):
        self.build("abc123")
        done = self.tool("current")
        self.assertEqual((done.returncode, done.stdout), (0, "abc123\n"))

    def test_a_home_page_naming_none_is_refused(self):
        self.site.mkdir()
        (self.site / "index.html").write_text("<head></head>")
        for command in ("current", "prune", "retained", "filter", "attempt", "deployed"):
            with self.subTest(command=command):
                done = self.tool(command)
                self.assertEqual(done.returncode, 1, done.stdout)
                self.assertIn("names no version", done.stderr)


class Prune(Case):
    def test_it_leaves_only_this_build_s_version(self):
        self.build("bbb", "assets/aaa/js/a.js", "js/a.js", "css/a.css", "images/kept.png")
        done = self.tool("prune")
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(sorted(p.name for p in (self.site / "assets").iterdir()), ["bbb"])
        self.assertFalse((self.site / "js").exists())
        self.assertFalse((self.site / "css").exists())
        self.assertTrue((self.site / "images/kept.png").exists())

    def test_a_clean_site_is_left_alone(self):
        self.build("bbb")
        done = self.tool("prune")
        self.assertEqual((done.returncode, done.stdout), (0, ""))
        self.assertTrue((self.site / "assets/bbb/js/a.js").exists())


class Ledger(Case):
    def state(self) -> dict:
        return json.loads(self.ledger.read_text())

    def retained(self, on: int) -> list[str]:
        return self.tool("retained", on).stdout.split()

    def attempt(self, version: str, on: int) -> None:
        self.build(version)
        done = self.tool("attempt", on)
        self.assertEqual(done.returncode, 0, done.stderr)

    def go_live(self, version: str, on: int) -> None:
        self.attempt(version, on)
        done = self.tool("deployed", on)
        self.assertEqual(done.returncode, 0, done.stderr)

    def test_before_any_deploy_only_the_legacy_paths_are_kept(self):
        self.build("aaa")
        self.assertEqual(self.retained(0), ["js", "css"])
        self.assertFalse(self.ledger.exists(), "asking wrote the ledger")

    def test_the_clock_starts_when_a_version_is_replaced_however_long_it_was_live(self):
        self.go_live("aaa", 0)
        self.go_live("bbb", 365)
        self.assertEqual(self.state()["replaced"], {"aaa": day(365)})
        self.assertIn("assets/aaa", self.retained(365 + 90))
        self.assertNotIn("assets/aaa", self.retained(365 + 91))

    def test_the_legacy_paths_are_kept_for_as_long_from_the_first_deploy(self):
        self.go_live("aaa", 0)
        self.go_live("bbb", 30)
        self.assertEqual(self.state()["legacy"], day(0))
        self.assertIn("js", self.retained(90))
        self.assertIn("css", self.retained(90))
        self.assertNotIn("js", self.retained(91))
        self.assertNotIn("css", self.retained(91))

    def test_the_version_going_live_needs_no_rule(self):
        self.go_live("aaa", 0)
        self.assertNotIn("assets/aaa", self.retained(1))

    def test_a_deploy_of_the_same_version_starts_no_clock(self):
        self.go_live("aaa", 0)
        self.go_live("aaa", 200)
        self.assertEqual(self.state()["replaced"], {})
        self.assertEqual(self.state()["live"], "aaa")

    def test_a_version_coming_back_stops_its_clock_and_starts_the_other_s(self):
        self.go_live("aaa", 0)
        self.go_live("bbb", 10)
        self.go_live("aaa", 20)
        self.assertEqual(self.state()["replaced"], {"bbb": day(20)})
        self.assertEqual(self.state()["live"], "aaa")

    def test_what_has_had_its_days_leaves_the_ledger(self):
        self.go_live("aaa", 0)
        self.go_live("bbb", 10)
        self.go_live("ccc", 200)
        self.assertEqual(self.state()["replaced"], {"bbb": day(200)})

    def test_retained_sees_the_live_version_before_it_is_recorded_as_replaced(self):
        self.go_live("aaa", 0)
        self.build("bbb")
        self.assertEqual(self.retained(1), ["assets/aaa", "js", "css"])

    def test_an_attempt_moves_nothing(self):
        self.go_live("aaa", 0)
        self.attempt("bbb", 1)
        self.assertEqual(self.state(), {"live": "aaa", "attempted": ["bbb"], "replaced": {}, "legacy": day(0)})

    def test_a_failed_deploy_costs_the_live_version_none_of_its_days(self):
        self.go_live("aaa", 0)
        self.attempt("bbb", 1)
        # aaa has been serving all this time; bbb may have been, in part.
        self.build("ccc")
        self.assertEqual(self.retained(92), ["assets/aaa", "assets/bbb"])
        self.go_live("ccc", 92)
        self.assertEqual(self.state()["replaced"], {"aaa": day(92), "bbb": day(92)})
        self.assertEqual(self.state()["attempted"], [])
        self.assertEqual(self.retained(92 + 90), ["assets/aaa", "assets/bbb"])
        self.assertEqual(self.retained(92 + 91), [])

    def test_a_failed_first_deploy_starts_no_clock_on_the_legacy_paths(self):
        self.attempt("aaa", 0)
        self.assertIsNone(self.state()["legacy"])
        self.assertEqual(self.retained(200), ["js", "css"])
        self.go_live("aaa", 200)
        self.assertEqual(self.state()["legacy"], day(200))
        self.assertEqual(self.retained(200 + 90), ["js", "css"])
        self.assertEqual(self.retained(200 + 91), [])

    def test_trying_again_finishes_what_failed(self):
        self.go_live("aaa", 0)
        self.attempt("bbb", 1)
        self.attempt("bbb", 2)
        self.assertEqual(self.state()["attempted"], ["bbb"])
        self.go_live("bbb", 3)
        self.assertEqual(self.state(), {"live": "bbb", "attempted": [], "replaced": {"aaa": day(3)}, "legacy": day(0)})

    def test_going_back_after_a_failed_deploy_starts_the_failed_version_s_clock(self):
        self.go_live("aaa", 0)
        self.attempt("bbb", 1)
        self.attempt("aaa", 2)
        self.assertEqual(self.state()["attempted"], ["bbb"], "what is live needs no attempt")
        self.go_live("aaa", 5)
        self.assertEqual(self.state(), {"live": "aaa", "attempted": [], "replaced": {"bbb": day(5)}, "legacy": day(0)})

    def test_an_attempted_version_stays_though_its_earlier_days_have_run_out(self):
        self.go_live("aaa", 0)
        self.go_live("bbb", 10)
        self.attempt("aaa", 200)
        self.build("ccc")
        self.assertIn("assets/aaa", self.retained(300))
        self.go_live("ccc", 300)
        self.assertEqual(self.state()["replaced"], {"aaa": day(300), "bbb": day(300)})

    def test_filter_is_rsync_rules_for_the_same_paths(self):
        self.go_live("aaa", 0)
        self.build("bbb")
        self.assertEqual(self.tool("filter", 1).stdout.splitlines(),
                         ["P /assets/aaa/***", "P /js/***", "P /css/***"])


@unittest.skipUnless(shutil.which("rsync"), "rsync not on PATH")
class Deploys(Case):
    """Each is the transfer `make deploy` runs, into a local directory."""

    def setUp(self):
        super().setUp()
        self.server = self.root / "www"
        self.server.mkdir()
        for rel in ("index.html", "stale.txt", "js/old.js", "js/seasons/old.js", "css/old.css"):
            (self.server / rel).parent.mkdir(parents=True, exist_ok=True)
            (self.server / rel).write_text("before")

    def transfer(self, version: str, on: int) -> None:
        """A deploy as far as its transfer, which then fails to tell anyone."""
        self.build(version)
        rules = self.root / "deploy-filter.txt"
        rules.write_text(self.tool("filter", on).stdout)
        self.assertEqual(self.tool("attempt", on).returncode, 0)
        subprocess.run(["rsync", "-aR", f"{self.site}/./assets/{version}/", f"{self.server}/"],
                       check=True, capture_output=True, text=True)
        subprocess.run(["rsync", "-a", "--delete", f"--filter=merge {rules}", f"{self.site}/", f"{self.server}/"],
                       check=True, capture_output=True, text=True)

    def deploy(self, version: str, on: int) -> None:
        self.transfer(version, on)
        self.assertEqual(self.tool("deployed", on).returncode, 0)

    def on_server(self) -> set[str]:
        return {str(p.relative_to(self.server)) for p in self.server.rglob("*") if p.is_file()}

    def has(self, version: str) -> bool:
        return (self.server / f"assets/{version}/js/a.js").is_file() \
            and (self.server / f"assets/{version}/css/a.css").is_file()

    def test_earlier_versions_and_the_legacy_paths_stay_for_their_days_and_then_go(self):
        self.deploy("aaa", 0)
        self.assertEqual(self.on_server(), {"index.html", "js/old.js", "js/seasons/old.js", "css/old.css",
                                            "assets/aaa/js/a.js", "assets/aaa/css/a.css"})
        self.deploy("bbb", 10)
        self.assertTrue(self.has("aaa") and self.has("bbb"))
        self.assertEqual((self.server / "assets/aaa/js/a.js").read_text(), "aaa")
        self.deploy("ccc", 50)
        self.assertTrue(self.has("aaa") and self.has("bbb") and self.has("ccc"))
        self.assertTrue((self.server / "js/old.js").exists())
        # 91 days from the first deploy, the legacy paths; aaa, replaced on
        # day 10, has another nine.
        self.deploy("ccc", 91)
        self.assertFalse((self.server / "js").exists())
        self.assertFalse((self.server / "css").exists())
        self.assertTrue(self.has("aaa"))
        self.deploy("ccc", 100)
        self.assertTrue(self.has("aaa"))
        self.deploy("ccc", 101)
        self.assertFalse(self.has("aaa"))
        self.assertTrue(self.has("bbb") and self.has("ccc"))
        self.deploy("ddd", 141)
        self.assertFalse(self.has("bbb"))
        self.assertTrue(self.has("ccc") and self.has("ddd"))

    def test_a_deploy_that_never_transferred_costs_the_live_version_none_of_its_days(self):
        self.deploy("aaa", 0)
        self.build("bbb")
        self.assertEqual(self.tool("attempt", 1).returncode, 0)
        self.deploy("ccc", 92)
        self.assertTrue(self.has("aaa") and self.has("ccc"))
        self.deploy("ccc", 92 + 90)
        self.assertTrue(self.has("aaa"))
        self.deploy("ccc", 92 + 91)
        self.assertFalse(self.has("aaa"))

    def test_a_deploy_whose_end_went_unnoted_keeps_both_versions(self):
        self.deploy("aaa", 0)
        self.transfer("bbb", 1)
        self.deploy("ccc", 92)
        self.assertTrue(self.has("aaa") and self.has("bbb") and self.has("ccc"))
        self.deploy("ccc", 92 + 91)
        self.assertFalse(self.has("aaa") or self.has("bbb"))

    def test_a_lost_ledger_loses_the_earlier_versions_at_the_next_deploy_but_not_the_legacy_paths(self):
        self.deploy("aaa", 0)
        self.ledger.unlink()
        self.deploy("bbb", 1)
        self.assertFalse(self.has("aaa"))
        self.assertTrue((self.server / "js/old.js").exists())


if __name__ == "__main__":
    unittest.main()
