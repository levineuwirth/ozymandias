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
    def test_a_feature_is_on_unless_the_config_turns_it_off(self):
        settings = {"switches": {"math": False, "music": True}}
        self.assertFalse(target.switch_on("math", settings))
        self.assertTrue(target.switch_on("music", settings))
        self.assertTrue(target.switch_on("viz", settings))
        self.assertTrue(target.switch_on("math", {}))

    def test_a_test_of_a_feature_turned_off_skips_saying_which(self):
        class Probe(unittest.TestCase):
            @target.requires_switch("math")
            def test_math(self):
                pass

        with mock.patch.object(target, "switches", return_value={"math": False}):
            result = unittest.TestResult()
            Probe("test_math").run(result)
        self.assertEqual([reason for _, reason in result.skipped], ["switch off: math"])
        with mock.patch.object(target, "switches", return_value={}):
            result = unittest.TestResult()
            Probe("test_math").run(result)
        self.assertEqual((result.skipped, result.testsRun), ([], 1))

    def test_switches_come_from_site_yaml_not_the_generator(self):
        # CI, where ci.py runs, has no generator.
        with mock.patch.object(target, "site_binary", side_effect=AssertionError("generator asked")):
            self.assertIsInstance(target.switches(), dict)
            self.assertTrue(target.switch_on("a-feature-no-site-turns-off"))

    def test_ci_accepts_only_the_skips_the_site_explains(self):
        skipped = [("a", "switch off: math"), ("b", "switch off: viz"), ("c", "node not installed"),
                   ("d", target.NO_FONTS)]
        with tempfile.TemporaryDirectory() as tmp:
            fonts = Path(tmp) / "fonts.yaml"
            with mock.patch.object(target, "FONTS", fonts):
                self.assertEqual(ci.unexpected_skips(skipped, {"switches": {"math": False}}),
                                 skipped[1:3])
                fonts.write_text("families: {}\n")
                self.assertEqual(ci.unexpected_skips(skipped, {"switches": {"math": False}}),
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
