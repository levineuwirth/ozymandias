"""The site under test (tests/target.py), and what engine/tests/ci.py takes
from it: the site's own modules and suites, and the skips its switches
explain."""

import importlib.util
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from tests import target
from tests._helpers import engine_path, load_tool


def load_ci():
    spec = importlib.util.spec_from_file_location("ci", engine_path("tests/ci.py"))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


ci = load_ci()


class Switches(unittest.TestCase):
    def test_a_switch_is_the_site_s_setting_over_the_registry_s_default(self):
        self.assertTrue(target.switch_on("archive", {"switches": {"archive": True}}))
        self.assertFalse(target.switch_on("archive", {"switches": {"archive": False}}))
        self.assertFalse(target.switch_on("archive", {}))   # the engine's default
        with self.assertRaises(KeyError):
            target.switch_on("a-switch-the-registry-lacks", {})
        with self.assertRaises(ValueError):
            target.switches({"switches": {"a-switch-the-registry-lacks": True}})
        with self.assertRaises(ValueError):
            target.switches({"switches": {"archive": "yes"}})

    def test_a_test_of_a_feature_turned_off_skips_saying_which(self):
        class Probe(unittest.TestCase):
            @target.requires_switch("archive")
            def test_archive(self):
                pass

        with mock.patch.object(target, "switches", return_value={"archive": False}):
            result = unittest.TestResult()
            Probe("test_archive").run(result)
        self.assertEqual([reason for _, reason in result.skipped], ["switch off: archive"])
        with mock.patch.object(target, "switches", return_value={"archive": True}):
            result = unittest.TestResult()
            Probe("test_archive").run(result)
        self.assertEqual((result.skipped, result.testsRun), ([], 1))

    def test_switches_come_from_site_yaml_not_the_generator(self):
        # CI, where ci.py runs, has no generator.
        with mock.patch.object(target, "site_binary", side_effect=AssertionError("generator asked")):
            self.assertIsInstance(target.switches(), dict)
            self.assertIsInstance(target.switch_on("archive"), bool)

    def test_ci_accepts_only_the_skips_the_site_explains(self):
        # A switch the registry lacks explains nothing (b).
        skipped = [("a", "switch off: archive"), ("b", "switch off: no-such-switch"), ("c", "node not installed"),
                   ("d", target.NO_FONTS)]
        with tempfile.TemporaryDirectory() as tmp:
            fonts = Path(tmp) / "fonts.yaml"
            with mock.patch.object(target, "FONTS", fonts):
                self.assertEqual(ci.unexpected_skips(skipped, {"switches": {"archive": False}}),
                                 skipped[1:3])
                fonts.write_text("families: {}\n")
                self.assertEqual(ci.unexpected_skips(skipped, {"switches": {"archive": False}}),
                                 skipped[1:])


class EngineFile(unittest.TestCase):
    def test_the_site_s_own_file_wins(self):
        with tempfile.TemporaryDirectory() as tmp:
            site = Path(tmp)
            with mock.patch.object(target, "SITE_ROOT", site):
                self.assertEqual(target.engine_file("templates/page.html"),
                                 engine_path("templates/page.html"))
                (site / "templates").mkdir()
                (site / "templates/page.html").write_text("")
                self.assertEqual(target.engine_file("templates/page.html"), site / "templates/page.html")


class SiteCi(unittest.TestCase):
    def test_the_site_lists_its_own_modules_and_suites(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            self.assertEqual(ci.site_ci(root), {"modules": [], "suites": []})
            (root / "tests").mkdir()
            (root / ci.SITE_CI).write_text("modules: [test_a]\nsuites: [cv]\n")
            self.assertEqual(ci.site_ci(root), {"modules": ["test_a"], "suites": ["cv"]})
            for bad in ("modules: test_a\n", "extra: []\n", "suites: [1]\n"):
                (root / ci.SITE_CI).write_text(bad)
                with self.assertRaises(ValueError):
                    ci.site_ci(root)

    def test_suites_are_checked_before_anything_runs(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for suite in ("cv", "notes"):
                (root / suite / "tests").mkdir(parents=True)
                (root / suite / "tests/test_own.py").write_text("")
            (root / "cv/tests/test_cv.py").write_text("")
            self.assertEqual(ci.suite_problems(root, ["cv"]), [])
            self.assertEqual(ci.suite_problems(root, ["cv", "notes", "gone"]),
                             ["notes/tests/test_own.py has the name of cv/tests/test_own.py",
                              f"{ci.SITE_CI} names gone/, which has no tests/"])

    def test_the_engine_lists_none_of_the_site_s(self):
        site = ci.site_ci(target.SITE_ROOT)["modules"]
        self.assertEqual(sorted(set(ci.MODULES) & set(site)), [])
        for name in ci.MODULES:
            self.assertTrue(engine_path(f"tests/{name}.py").is_file(), name)


class Routes(unittest.TestCase):
    """The site's tests/routes.yaml, as the harness and the browser tests read it."""

    def test_the_site_s_routes_hold(self):
        routes = target.routes()
        self.assertTrue(routes.pages)
        for role in routes.roles:
            self.assertTrue(target.route(role).startswith("/"), role)

    def test_the_sweeps_cover_every_kind_of_page_the_site_has(self):
        # Plan 7.3.3: so axe's and overflow's coverage cannot shrink quietly.
        routes = target.routes()
        self.assertEqual(routes.uncovered(target.switches()), [])
        self.assertEqual(routes.misnamed(target.built, target.switches()), [])

    def scratch_routes(self, tmp: str, kinds: str = "{home: home}", pages: str = "{home: /}"):
        """A scratch site's Routes, and every switch off."""
        site = Path(tmp)
        (site / "tests").mkdir()
        (site / "site.yaml").write_text("{}")
        (site / "tests/routes.yaml").write_text(f"pages: {pages}\nkinds: {kinds}\n")
        module = load_tool("browser/routes.py", "site_routes_kinds")
        return site, module.Routes(site), {k.switch: False for k in module.KINDS.values() if k.switch}

    def test_a_kind_the_site_has_must_be_named_and_one_it_has_off_not(self):
        with tempfile.TemporaryDirectory() as tmp:
            pages, kinds = "{home: /, m: /music/}", "{home: home, music-index: m}"
            site, routes, switches = self.scratch_routes(tmp, kinds, pages)
            problems = routes.uncovered(switches)
            self.assertIn("music-index: named, but the music switch is off", problems)
            self.assertEqual([p for p in routes.uncovered(switches, shared=True) if p.startswith("music-index:")], [])
            self.assertEqual([p for p in problems if p.startswith("essay:")], [], "no essay, so no kind to name")

    def test_a_collection_s_entries_are_its_section_s_kind_and_its_landing_is_not(self):
        # The generator's patterns, its exclusions among them.
        for section, kind in (("poetry", "poem"), ("fiction", "story"), ("blog", "post")):
            with self.subTest(section=section), tempfile.TemporaryDirectory() as tmp:
                site, routes, switches = self.scratch_routes(tmp)
                def named() -> list[str]:
                    return [p for p in routes.uncovered(switches) if p.startswith(f"{kind}:")]

                landing = site / "content" / section / "a-set/index.md"
                landing.parent.mkdir(parents=True)
                landing.write_text("---\ntitle: A set\n---\n")
                self.assertEqual(named(), [], "a landing alone is not an entry")
                entry = landing.with_name("first.md")
                entry.write_text("---\ntitle: First\ndraft: true\n---\n")
                self.assertEqual(named(), [], "a draft alone is not an entry")
                entry.write_text("---\ntitle: First\n---\n")
                self.assertNotEqual(named(), [], "only a nested entry, and no page named")

    def problems(self, kinds: str, pages: str, built: dict[str, str] | None, off: tuple[str, ...] = ()) -> list[str]:
        """`misnamed` for a scratch site with every switch on but `off`, and an
        essay, so an author's kind is one it has; with `built`'s pages by path,
        or no build at all."""
        with tempfile.TemporaryDirectory() as tmp:
            site, routes, switches = self.scratch_routes(tmp, kinds, pages)
            (site / "content/essays").mkdir(parents=True)
            (site / "content/essays/a.md").write_text("---\ntitle: A\n---\n")
            switches = {name: name not in off for name in switches}

            def where(path: str) -> Path:
                file = site / "built.html"
                if built is None:
                    file.unlink(missing_ok=True)
                else:
                    file.write_text(built.get(path, ""))
                return file

            return routes.misnamed(where, switches)

    def test_a_kind_is_held_to_its_own_route_and_not_another_s(self):
        # Substitutions that once passed: an author's kind on a section's
        # index, the by-year index's on one year's page; with a build and
        # without.
        built = {"/essays/": 'class="essay-list" class="page-title"', "/photography/by-year/2024/": "photography-by-year-back",
                 "/stats/": 'class="essay-frontmatter" id="activity"', "/links.html": 'class="page-title"'}
        pages = "{index: /essays/, year: /photography/by-year/2024/, stats: /stats/, links: /links.html}"
        for kinds, problem in (("{author: index}", "author: /essays/ "),
                               ("{photo-years: year}", "photo-years: /photography/by-year/2024/ "),
                               ("{essay: stats}", "essay: /stats/ "),
                               ("{page: index}", "page: /essays/ "),
                               ("{poem: index}", "poem: /essays/ ")):
            for build in (built, None):
                with self.subTest(kinds=kinds, build=build is not None):
                    found = self.problems(kinds, pages, build)
                    self.assertTrue([p for p in found if p.startswith(problem)], found)

    def test_a_path_is_kept_only_by_a_kind_the_site_has(self):
        # With stats-page off a tag `stats` has /stats/ (test_route_check), and
        # likewise each generated index and namespace: the kinds that yield a
        # path to these are free to have it once their switch is off.
        for path, switch in (("/stats/", "stats-page"), ("/build/", "build-page"), ("/archive/", "archive"),
                             ("/bibliography/", "bibliography-index"), ("/authors/a/", "authors"),
                             ("/photography/by-year/", "photography")):
            for kind, mark in (("tag", 'class="item-card-list" class="page-title"'),
                               ("essay", 'class="essay-frontmatter" class="page-title"'),
                               ("page", 'class="page-title"')):
                for build in ({path: mark}, None):
                    with self.subTest(path=path, kind=kind, build=build is not None):
                        pages, kinds = f"{{p: {path}}}", f"{{{kind}: p}}"
                        self.assertEqual(self.problems(kinds, pages, build, off=(switch,)), [])
                        self.assertTrue(self.problems(kinds, pages, build), "the switch on keeps the path")

    def test_names_and_paths_resolve_and_strays_are_refused(self):
        with tempfile.TemporaryDirectory() as tmp:
            site = Path(tmp)
            (site / "tests").mkdir()
            routes_yaml = site / "tests/routes.yaml"
            routes_yaml.write_text("pages: {home: /, '404': /gone}\nkeyboard: [home]\n"
                                   "footer: [home, /other.html]\nprobes: {math: [home]}\n"
                                   "roles: {not-found: '404', essay: /e.html, score: /music/w/}\n")
            routes = load_tool("browser/routes.py", "site_routes_probe").Routes(site)
            self.assertEqual([routes.path(e) for e in routes.names("footer")], ["/", "/other.html"])
            self.assertEqual((routes.role("not-found"), routes.role("essay")), ("/gone", "/e.html"))
            self.assertEqual((routes.probe("math"), routes.probe("map")), (["home"], []))
            for stray, message in (
                    ("pages: {home: /}\nfooter: [essay]\n", "no such page"),
                    ("pages: {home: /}\nkeyboard: [/x/]\n", "no such page"),     # a sweep names pages
                    ("pages: {home: /}\nprobes: {math: [/]}\n", "no such page"), # so does a probe
                    ("pages: {home: /}\nprobes: {popup: [home]}\n", "no such probe"),
                    ("pages: {home: /}\nroles: {score: /music/w}\n", "must end in /"),
                    ("pages: {home: index.html}\n", "not a path"),
                    ("pages: {home: /}\nextra: []\n", "unknown")):
                routes_yaml.write_text(stray)
                with self.assertRaisesRegex(ValueError, message, msg=stray):
                    type(routes)(site)

    def test_a_page_s_built_file(self):
        self.assertEqual(target.built("/essays/e/"), target.SITE / "essays/e/index.html")
        self.assertEqual(target.built("/essays/e.html"), target.SITE / "essays/e.html")


if __name__ == "__main__":
    unittest.main()
