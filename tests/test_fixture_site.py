"""The engine's fixture site (engine/fixture-site/): a small site with the
minimal profile's features, under another identity, that builds and passes
the artifact gate. It is copied to a scratch directory and committed there
(the build reads Git for dates and /build/), with engine/ linked in, as a
site of the engine's has it.

Its temporary-inputs.yaml lists what it carries only because an engine rule
is still unconditional, each with the switch that will remove the need; an
entry fails here once its switch exists while the input is still present.

The gate's test needs the engine's downloads (DOWNLOADS), which a fresh
checkout lacks, so `make test-clean` and the GitHub workflow never run it:
only a machine that has them checks that the fixture passes the gate.
"""

import hashlib
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

import yaml

from tests._helpers import ENGINE, engine_path, load_tool, requires_cabal, script_env, site_binary

FIXTURE = engine_path("fixture-site")
# A8's switch registry: the switches that exist (none before it).
REGISTRY = engine_path("switches.yaml")
# The engine's downloads, gitignored: a fresh checkout has none, and the
# gate fails a site without them (the map's Leaflet, search's model).
DOWNLOADS = ("static/pdfjs/web/viewer.html", "static/leaflet/leaflet.js",
             "static/models/all-MiniLM-L6-v2/config.json")


def registered_switches() -> set[str]:
    return set(yaml.safe_load(REGISTRY.read_text(encoding="utf-8")) or {}) if REGISTRY.is_file() else set()


def blob_id(data: bytes) -> str:
    return hashlib.sha1(b"blob %d\0" % len(data) + data).hexdigest()


class FixtureFiles(unittest.TestCase):
    def test_temporary_inputs_are_there_until_their_switch(self):
        entries = yaml.safe_load((FIXTURE / "temporary-inputs.yaml").read_text(encoding="utf-8"))
        self.assertTrue(entries)
        config = yaml.safe_load((FIXTURE / "site.yaml").read_text(encoding="utf-8"))
        switches = registered_switches()
        for entry in entries:
            with self.subTest(entry=entry):
                self.assertLessEqual(set(entry), {"path", "section", "switch", "why"})
                self.assertTrue(entry["switch"] and entry["why"])
                self.assertTrue((FIXTURE / entry["path"]).is_file())
                if "section" in entry:
                    self.assertIn(entry["section"], [s["dir"] for s in config["sections"]])
                self.assertNotIn(entry["switch"], switches,
                                 f"{entry['switch']} exists: remove {entry['path']} and its entry")

    def test_its_pages_and_roles_are_checked(self):
        routes = load_tool("browser/routes.py", "fixture_routes").Routes(FIXTURE)
        self.assertTrue(routes.pages)

    def test_its_override_was_reviewed_against_the_engine_file(self):
        for entry in yaml.safe_load((FIXTURE / "overrides.yaml").read_text(encoding="utf-8")):
            with self.subTest(override=entry["site"]):
                self.assertTrue((FIXTURE / entry["site"]).is_file())
                self.assertEqual(blob_id(engine_path(entry["engine"]).read_bytes()), entry["reviewed"])


@requires_cabal
@unittest.skipUnless(shutil.which("pagefind"), "pagefind not on PATH")
class FixtureBuild(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        tmp = tempfile.TemporaryDirectory(prefix="fixture-site-")
        cls.addClassCleanup(tmp.cleanup)
        cls.site = Path(tmp.name) / "site"
        shutil.copytree(FIXTURE, cls.site)
        (cls.site / "engine").symlink_to(ENGINE)
        # The commit ignores the user's signing and hooks, which could fail it.
        git = ["git", "-c", "user.email=fixture@example.org", "-c", "user.name=Fixture",
               "-c", "commit.gpgsign=false", "-c", "core.hooksPath=/dev/null"]
        for args in (["init", "-q"], ["add", "-A"], ["commit", "-qm", "the fixture site"]):
            subprocess.run(git + args, cwd=cls.site, check=True, capture_output=True)
        binary = str(site_binary())
        env = script_env(SITE_ENV="production", SITE_ROOT=str(cls.site), SITE_BINARY=binary)
        cls.build = subprocess.run([binary, "build"], cwd=cls.site, env=env,
                                   capture_output=True, text=True, timeout=600)
        cls.index = subprocess.run([str(engine_path("tools/pagefind-index.sh")), "_site"], cwd=cls.site,
                                   env=env, capture_output=True, text=True, timeout=300)
        cls.gate = subprocess.run([shutil.which("python3"), str(engine_path("tools/check-site.py")), "_site"],
                                  cwd=cls.site, env=env, capture_output=True, text=True, timeout=300)

    def test_it_builds(self):
        self.assertEqual(self.build.returncode, 0, self.build.stdout[-3000:] + self.build.stderr)
        self.assertNotIn("[ERROR]", self.build.stdout)
        self.assertEqual(self.index.returncode, 0, self.index.stdout + self.index.stderr)

    @unittest.skipUnless(all(engine_path(p).is_file() for p in DOWNLOADS),
                         "no engine downloads (make download-pdfjs download-leaflet download-model)")
    def test_the_artifact_gate_passes(self):
        self.assertEqual(self.gate.returncode, 0, self.gate.stdout[-5000:] + self.gate.stderr)

    def test_every_page_its_routes_name_is_built(self):
        routes = load_tool("browser/routes.py", "fixture_routes").Routes(self.site)
        not_found = routes.role("not-found")   # its own document is meant to 404
        missing = [path for path in routes.pages.values() if path != not_found
                   and not (self.site / "_site" / path.lstrip("/") / ("index.html" if path.endswith("/") else ""))
                   .is_file()]
        self.assertEqual(missing, [])

    def test_its_override_stands_in_for_the_engine_s(self):
        essay = (self.site / "_site/essays/long/index.html").read_text(encoding="utf-8")
        self.assertIn("fixture-ornament", essay)


if __name__ == "__main__":
    unittest.main()
