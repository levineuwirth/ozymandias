"""The engine's fixture site (engine/fixture-site/): a small site under
another identity, built from a copy committed in a scratch directory (the
build reads Git for dates and /build/), with engine/ linked in, as a site of
the engine's has it.

It is built once per profile (profiles.yaml: full, every switch on; minimal,
the partner's site) and once per variant (tests/fixture_variants.py), each
with its switches written into the copy's site.yaml. Every build must pass
the artifact gate, and every switch's checks (tests/switch_checks.py) run on
every build, as the build has the switch; the coverage test fails unless
each switch is built both ways.

The gate's test needs the engine's downloads (DOWNLOADS), which a fresh
checkout lacks, so `make test-clean` and the GitHub workflow never run it:
only a machine that has them checks that the fixture passes the gate.
"""

import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
import unittest.mock
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

import yaml

from tests import fixture_variants, switch_checks
from tests._helpers import ENGINE, engine_path, load_tool, requires_cabal, script_env, site_binary

FIXTURE = engine_path("fixture-site")
REGISTRY = engine_path("switches.yaml")
# The engine's downloads, gitignored: a fresh checkout has none, and the
# gate fails a site without them (the map's Leaflet, search's model).
DOWNLOADS = ("static/pdfjs/web/viewer.html", "static/leaflet/leaflet.js",
             "static/models/all-MiniLM-L6-v2/config.json")
review = load_tool("override-review.py", "override_review")


def registered_switches() -> set[str]:
    return set(yaml.safe_load(REGISTRY.read_text(encoding="utf-8")) or {})


class FixtureFiles(unittest.TestCase):
    def test_its_pages_and_roles_are_checked(self):
        routes = load_tool("browser/routes.py", "fixture_routes").Routes(FIXTURE)
        self.assertTrue(routes.pages)

    def test_its_override_was_reviewed_against_the_engine_file(self):
        self.assertTrue(review.manifest(FIXTURE))
        self.assertTrue(all((FIXTURE / e["site"]).is_file() for e in review.manifest(FIXTURE)))
        self.assertEqual(review.unreviewed(FIXTURE), [])

    @requires_cabal
    def test_its_override_keeps_the_contract(self):
        self.assertEqual(review.hook_problems(FIXTURE, str(site_binary())), [])


class Profiles(unittest.TestCase):
    def test_each_profile_names_every_switch(self):
        for name, profile in fixture_variants.profiles().items():
            with self.subTest(profile=name):
                self.assertEqual(set(profile["switches"]), registered_switches())
                self.assertTrue(all(isinstance(v, bool) for v in profile["switches"].values()))
                self.assertLessEqual(set(profile), {"switches", "without"})

    def test_variants_turn_off_what_needs_what_they_turn_off(self):
        terms = {"a": {}, "b": {"needs": ["a"]}, "c": {"needs-any": ["a", "d"]}, "d": {}}
        self.assertEqual(fixture_variants.settle({"a": False, "b": True, "c": True, "d": False}, terms),
                         {"a": False, "b": False, "c": False, "d": False})
        self.assertEqual(fixture_variants.settle({"a": False, "b": True, "c": True, "d": True}, terms),
                         {"a": False, "b": False, "c": True, "d": True})

    def test_the_builds_are_derived_from_the_registry(self):
        terms = {"a": {}, "b": {"needs": ["a"]}, "c": {"needs-any": ["a", "d"]}, "d": {}}
        profiles = {"full": {"switches": dict.fromkeys(terms, True)},
                    "minimal": {"switches": {"a": True, "b": True, "c": False, "d": False},
                                "without": ["x/"]}}
        with unittest.mock.patch.object(fixture_variants, "registry", return_value=terms), \
                unittest.mock.patch.object(fixture_variants, "profiles", return_value=profiles):
            found = {b.name: (b.switches, b.without) for b in fixture_variants.builds()}
        self.assertEqual(found["minimal-without-a"], ({"a": False, "b": False, "c": False, "d": False}, ("x/",)))
        self.assertEqual(found["minimal-without-b"][0], {"a": True, "b": False, "c": False, "d": False})
        self.assertEqual(found["c-by-a"][0], {"a": True, "b": True, "c": True, "d": False})
        self.assertEqual(found["c-by-d"][0], {"a": False, "b": False, "c": True, "d": True})
        self.assertEqual(set(found), {"full", "minimal", "minimal-without-a", "minimal-without-b",
                                      "c-by-a", "c-by-d"})


class Coverage(unittest.TestCase):
    def test_every_switch_is_built_both_ways_and_checked(self):
        builds = fixture_variants.builds()
        for name in sorted(registered_switches()):
            with self.subTest(switch=name):
                self.assertIn(name, switch_checks.CHECKS, f"{name} has no checks in tests/switch_checks.py")
                self.assertTrue(any(b.switches[name] for b in builds), f"no fixture build has {name} on")
                self.assertTrue(any(not b.switches[name] for b in builds), f"no fixture build has {name} off")
        self.assertLessEqual(set(switch_checks.CHECKS), registered_switches())


def build_site(build: fixture_variants.Build, site: Path, binary: str) -> dict:
    """One build in a copy of its own (`site`): the generator reads its
    switches first; with the archive on, the fetch stage runs as `make
    build` runs it (offline: the fixture's snapshot is committed); then the
    build, the search index and the gate. Each stage's result, by name."""
    shutil.copytree(FIXTURE, site)
    for rel in build.without:
        shutil.rmtree(site / rel) if (site / rel).is_dir() else (site / rel).unlink()
    config = yaml.safe_load((site / "site.yaml").read_text(encoding="utf-8"))
    config["switches"] = build.switches
    config["sections"] = fixture_variants.listed_sections(config["sections"], build.switches)
    # JSON, which YAML reads as written: a dumped string "y" would come back
    # as a boolean.
    (site / "site.yaml").write_text(json.dumps(config), encoding="utf-8")
    (site / "engine").symlink_to(ENGINE)
    # The commit ignores the user's signing and hooks, which could fail it.
    git = ["git", "-c", "user.email=fixture@example.org", "-c", "user.name=Fixture",
           "-c", "commit.gpgsign=false", "-c", "core.hooksPath=/dev/null"]
    for args in (["init", "-q"], ["add", "-A"], ["commit", "-qm", "the fixture site"]):
        subprocess.run(git + args, cwd=site, check=True, capture_output=True)
    env = script_env(SITE_ENV="production", SITE_ROOT=str(site), SITE_BINARY=binary)

    def run(*args, timeout=600):
        return subprocess.run(list(args), cwd=site, env=env, capture_output=True, text=True, timeout=timeout)

    runs = {"config": run(binary, "config")}
    if runs["config"].returncode:
        return runs
    if build.switches.get("archive"):
        runs["fetch"] = run(sys.executable, str(engine_path("tools/archive.py")), "fetch")
    runs["build"] = run(binary, "build")
    runs["index"] = run(str(engine_path("tools/pagefind-index.sh")), "_site", timeout=300)
    runs["gate"] = run(shutil.which("python3"), str(engine_path("tools/check-site.py")), "_site", timeout=300)
    return runs


@requires_cabal
@unittest.skipUnless(shutil.which("pagefind"), "pagefind not on PATH")
class FixtureBuilds(unittest.TestCase):
    """Every profile and variant, built at once: they share nothing, and one
    each is a build per switch the minimal profile has on (FIXTURE_JOBS at
    a time, 4 by default)."""

    @classmethod
    def setUpClass(cls):
        tmp = tempfile.TemporaryDirectory(prefix="fixture-site-")
        cls.addClassCleanup(tmp.cleanup)
        cls.builds = fixture_variants.builds()
        cls.sites = {b.name: Path(tmp.name) / b.name for b in cls.builds}
        binary = str(site_binary())
        with ThreadPoolExecutor(max_workers=int(os.environ.get("FIXTURE_JOBS") or 4)) as pool:
            done = {b.name: pool.submit(build_site, b, cls.sites[b.name], binary) for b in cls.builds}
        cls.runs = {name: future.result() for name, future in done.items()}

    def refused(self, name: str) -> None:
        """Fail a build the generator refused, with its reason."""
        runs = self.runs[name]
        if "build" not in runs:
            self.fail(f"the generator refused its switches: {runs['config'].stderr}")

    def test_the_generator_accepts_each_build_s_switches(self):
        for build in self.builds:
            with self.subTest(build=build.name):
                config = self.runs[build.name]["config"]
                self.assertEqual(config.returncode, 0, config.stderr)

    def test_each_build_builds(self):
        for build in self.builds:
            with self.subTest(build=build.name):
                self.refused(build.name)
                runs = self.runs[build.name]
                for stage in ("fetch", "build", "index"):
                    if stage in runs:
                        done = runs[stage]
                        self.assertEqual(done.returncode, 0, f"{stage}: {done.stdout[-3000:]}{done.stderr}")
                self.assertNotIn("[ERROR]", runs["build"].stdout)

    @unittest.skipUnless(all(engine_path(p).is_file() for p in DOWNLOADS),
                         "no engine downloads (make download-pdfjs download-leaflet download-model)")
    def test_the_artifact_gate_passes(self):
        for build in self.builds:
            with self.subTest(build=build.name):
                self.refused(build.name)
                gate = self.runs[build.name]["gate"]
                self.assertEqual(gate.returncode, 0, gate.stdout[-5000:] + gate.stderr)

    def test_each_switch_does_what_it_says_both_ways(self):
        for build in self.builds:
            for name, state in sorted(build.switches.items()):
                with self.subTest(build=build.name, switch=name, on=state):
                    on, off = switch_checks.CHECKS[name]
                    (on if state else off)(self.sites[build.name])

    def test_a_switch_s_assets_ship_only_with_it(self):
        # A download a fresh checkout lacks (PDF.js) has nothing to ship.
        for build in self.builds:
            site = self.sites[build.name] / "_site"
            for name, spec in sorted(fixture_variants.registry().items()):
                for glob in spec.get("assets") or []:
                    with self.subTest(build=build.name, switch=name, asset=glob):
                        if build.switches[name] and list(engine_path("static").glob(glob)):
                            self.assertTrue(list(site.glob(glob)), f"{glob} was not published")
                        else:
                            self.assertEqual(list(site.glob(glob)), [], f"{glob} was published")

    def turned_off(self, name: str) -> Path:
        """The full build's copy, its inputs and the data its build left
        (not its _site or cache), built again with switch `name` off and
        the switches that need it settled: what a site that turns a feature
        off has."""
        site = self.sites["full"].with_name(f"full-{name}-turned-off")
        shutil.copytree(self.sites["full"], site, symlinks=True, ignore=shutil.ignore_patterns("_site", "_cache"))
        config = yaml.safe_load((site / "site.yaml").read_text(encoding="utf-8"))
        config["switches"] = fixture_variants.settle(dict(config["switches"], **{name: False}),
                                                     fixture_variants.registry())
        config["sections"] = fixture_variants.listed_sections(config["sections"], config["switches"])
        (site / "site.yaml").write_text(json.dumps(config), encoding="utf-8")
        binary = str(site_binary())
        env = script_env(SITE_ENV="production", SITE_ROOT=str(site), SITE_BINARY=binary)
        done = subprocess.run([binary, "build"], cwd=site, env=env, capture_output=True, text=True, timeout=600)
        self.assertEqual(done.returncode, 0, done.stdout[-3000:] + done.stderr)
        return site

    def test_a_site_that_turns_the_archive_off_keeps_none_of_it(self):
        # The leftover index (data/archive-index.json, from the full
        # build's fetch) must not mark links to /archive/ pages no longer
        # built.
        self.assertTrue((self.sites["full"] / "data/archive-index.json").is_file())
        switch_checks.archive_off(self.turned_off("archive"))

    def test_a_site_that_turns_code_off_has_neither_highlighting_nor_copy_buttons(self):
        switch_checks.code_off(self.turned_off("code"))

    def test_a_site_that_turns_math_off_keeps_its_tex_as_text(self):
        switch_checks.math_off(self.turned_off("math"))

    def test_a_site_that_turns_viz_off_runs_no_figure_and_loads_no_vega(self):
        # The essay still sets `viz: true`.
        switch_checks.viz_off(self.turned_off("viz"))

    def test_a_site_that_turns_the_pdf_viewer_off_links_to_its_pdfs(self):
        switch_checks.pdf_viewer_off(self.turned_off("pdf-viewer"))

    def test_a_site_that_turns_source_refs_off_links_no_source(self):
        # The essay still has an authored .source-ref link.
        switch_checks.source_refs_off(self.turned_off("source-refs"))

    def test_a_site_that_turns_code_refs_off_tags_no_link(self):
        # Its snapshots and their index are still in code-refs/.
        site = self.turned_off("code-refs")
        self.assertTrue((site / "code-refs/index.json").is_file())
        switch_checks.code_refs_off(site)

    def test_a_site_that_turns_music_off_keeps_none_of_it(self):
        # Its works still in content/music/: no work, reader or score of
        # theirs is built, nor listed anywhere.
        site = self.turned_off("music")
        self.assertTrue((site / "content/music" / switch_checks.PIECE / "index.md").is_file())
        switch_checks.music_off(site)

    def test_every_page_its_routes_name_is_built(self):
        for name in ("minimal", "full"):
            with self.subTest(build=name):
                site = self.sites[name]
                routes = load_tool("browser/routes.py", "fixture_routes").Routes(site)
                not_found = routes.role("not-found")   # its own document is meant to 404
                missing = [path for path in routes.pages.values() if path != not_found
                           and not (site / "_site" / path.lstrip("/")
                                    / ("index.html" if path.endswith("/") else "")).is_file()]
                self.assertEqual(missing, [])

    def test_its_override_stands_in_for_the_engine_s(self):
        essay = (self.sites["minimal"] / "_site/essays/long/index.html").read_text(encoding="utf-8")
        self.assertIn("fixture-ornament", essay)


if __name__ == "__main__":
    unittest.main()
