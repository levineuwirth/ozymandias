"""The site under test (tests/target.py), and what engine/tests/ci.py takes
from it: the site's own modules and suites, and the skips its switches
explain."""

import importlib.util
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from tests import target
from tests._helpers import engine_path


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

        with mock.patch.object(target, "config", return_value={"switches": {"math": False}}):
            result = unittest.TestResult()
            Probe("test_math").run(result)
        self.assertEqual([reason for _, reason in result.skipped], ["switch off: math"])
        with mock.patch.object(target, "config", return_value={}):
            result = unittest.TestResult()
            Probe("test_math").run(result)
        self.assertEqual((result.skipped, result.testsRun), ([], 1))

    def test_ci_accepts_only_the_skips_the_config_explains(self):
        skipped = [("a", "switch off: math"), ("b", "switch off: viz"), ("c", "node not installed")]
        self.assertEqual(ci.unexpected_skips(skipped, {"switches": {"math": False}}), skipped[1:])


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

    def test_the_engine_lists_none_of_the_site_s(self):
        site = ci.site_ci(target.SITE_ROOT)["modules"]
        self.assertEqual(sorted(set(ci.MODULES) & set(site)), [])
        for name in ci.MODULES:
            self.assertTrue(engine_path(f"tests/{name}.py").is_file(), name)


if __name__ == "__main__":
    unittest.main()
