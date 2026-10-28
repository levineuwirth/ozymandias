"""The engine's switch registry (engine/switches.yaml) and the places that
ask it. The generator refuses a malformed registry and a site naming a
switch it lacks (test_config); here, without the generator: the registry's
shape, and that every switch the engine's code asks about is registered (a
misspelt one would otherwise read as off, or stop a build only once
reached) and every registered switch gates something.
"""

import re
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

import yaml

from tests._helpers import ENGINE, engine_path, script_env

REGISTRY = engine_path("switches.yaml")
KINDS = {"capability", "generated", "interface"}
# How each kind of engine file asks about a switch.
ASKS = {
    "build": ("*.hs", re.compile(r'switchOn\b[^"\n]*"([a-z][a-z0-9-]*)"')),
    "engine.mk": ("engine.mk", re.compile(r"(?:switch_on|with_switch),([a-z][a-z0-9-]*)")),
    "tools": ("*.py", re.compile(r'switch_on\(\s*"([a-z][a-z0-9-]*)"')),
    "tests": ("*.py", re.compile(r'(?:switch_on|requires_switch)\(\s*"([a-z][a-z0-9-]*)"')),
}
# Templates cannot ask yet: no context gives a switch field, and a missing
# field in $if$ reads as false, so a template gate would be off for every
# site. The first one brings the field (Config) and its pattern here.
# The tests of the mechanism itself, which ask about unregistered names on
# purpose.
MECHANISM = {"test_switches.py", "test_target.py", "test_config.py"}


def registry() -> dict:
    return yaml.safe_load(REGISTRY.read_text(encoding="utf-8"))


def asked() -> dict[str, set[str]]:
    """Each switch name the engine's files ask about, with the files."""
    found: dict[str, set[str]] = {}
    for where, (glob, pattern) in ASKS.items():
        root = ENGINE / where
        paths = [root] if root.is_file() else root.rglob(glob)
        for path in paths:
            if "fixture-site" in path.parts or path.name in MECHANISM:
                continue
            for name in pattern.findall(path.read_text(encoding="utf-8")):
                found.setdefault(name, set()).add(str(path.relative_to(ENGINE)))
    return found


class RegistryTests(unittest.TestCase):
    def test_each_switch_has_a_kind_a_default_and_what_it_covers(self):
        terms = registry()
        self.assertTrue(terms)
        for name, spec in terms.items():
            with self.subTest(switch=name):
                self.assertRegex(name, r"^[a-z][a-z0-9-]*$")
                self.assertLessEqual(set(spec), {"kind", "default", "needs", "needs-any", "about"})
                self.assertIn(spec["kind"], KINDS)
                self.assertIsInstance(spec["default"], bool)
                self.assertTrue(isinstance(spec["about"], str) and spec["about"].strip())
                for other in (spec.get("needs") or []) + (spec.get("needs-any") or []):
                    self.assertIn(other, terms)
                    self.assertNotEqual(other, name)

    def test_every_switch_asked_about_is_registered(self):
        terms = registry()
        unknown = {name: sorted(paths) for name, paths in asked().items() if name not in terms}
        self.assertEqual(unknown, {})

    def test_every_registered_switch_gates_something(self):
        used = asked()
        self.assertEqual([name for name in registry() if name not in used], [])


class MakeTests(unittest.TestCase):
    """engine.mk's switch_on and with_switch, in a scratch site that
    includes it, with tools/sitelib.py resolving."""

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        (self.root / "engine").symlink_to(ENGINE)
        (self.root / "Makefile").write_text("include engine/engine.mk\n")
        self.set_archive(False)

    def set_archive(self, on: bool) -> None:
        (self.root / "site.yaml").write_text(yaml.safe_dump({"switches": {"archive": on}}))

    def make(self, *args):
        return subprocess.run(["make", "--no-print-directory", f"PY={sys.executable}", *args],
                              cwd=self.root, env=script_env(), capture_output=True, text=True, timeout=60)

    def test_a_target_of_a_switch_turned_off_says_so(self):
        done = self.make("archive-suggest")
        self.assertEqual((done.returncode, done.stdout.strip()),
                         (0, "archive-suggest: the archive switch is off for this site"), done.stderr)

    def test_with_switch_runs_the_recipe_only_with_the_switch_on(self):
        probe = "--eval=probe: ; @$(call with_switch,archive,echo ran)"
        self.assertEqual(self.make(probe, "probe").stdout.strip(), "probe: the archive switch is off for this site")
        self.set_archive(True)
        self.assertEqual(self.make(probe, "probe").stdout.strip(), "ran")

    def test_a_switch_that_cannot_be_read_stops_the_recipe(self):
        # A misspelt switch, or a site.yaml naming one the registry lacks,
        # must not read as off.
        done = self.make("--eval=probe: ; @if $(call switch_on,archve); then echo on; else echo off; fi", "probe")
        self.assertEqual(done.returncode, 2, done.stdout + done.stderr)
        self.assertNotIn("off", done.stdout)
        (self.root / "site.yaml").write_text("switches: {archve: true}\n")
        self.assertEqual(self.make("archive-suggest").returncode, 2)


if __name__ == "__main__":
    unittest.main()
