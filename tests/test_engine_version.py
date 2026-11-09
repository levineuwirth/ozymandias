"""A build runs only with the engine sources its generator was compiled from.

The generator embeds a digest of its sources as it compiles (build/
EngineVersion.hs, build/SourceHash.hs), prints it as `site version`, and
`site build`, `rebuild` and `watch` refuse to run when the engine/ beside
them holds other sources (plan §3.7). So a stale binary can't build a site
from a newer engine's templates and scripts, or an older one's."""

import os
import re
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import ENGINE, SITE_ROOT, requires_cabal, script_env, site_binary, write_site_config


@requires_cabal
class EngineVersion(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.binary = str(site_binary())

    def run_site(self, root: Path, *args: str) -> subprocess.CompletedProcess:
        return subprocess.run([self.binary, *args], cwd=root, env=script_env(),
                              capture_output=True, text=True, timeout=120)

    def test_the_generator_names_the_sources_it_was_compiled_from(self):
        done = self.run_site(ENGINE, "version")
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertRegex(done.stdout.strip(), r"^ozymandias sources [0-9a-f]{64}$")

    def test_a_build_refuses_other_sources_and_writes_nothing(self):
        with tempfile.TemporaryDirectory(prefix="engine-version-") as tmp:
            root = Path(tmp)
            write_site_config(root)
            shutil.copytree(ENGINE / "build", root / "engine/build")
            shutil.copy2(ENGINE / "ozymandias.cabal", root / "engine/ozymandias.cabal")
            shutil.copy2(ENGINE / "cabal.project.freeze", root / "engine/cabal.project.freeze")
            # The same sources: past the check (the scratch site then stops
            # for want of templates, as a site without the engine's would).
            done = self.run_site(root, "build")
            self.assertNotIn("compiled from other sources", done.stderr)
            changes = (("cabal.project.freeze", lambda text: text.replace("any.base ==4.18.2.1", "any.base ==4.18.2.0")),
                       ("build/Utils.hs", lambda text: text + "\n-- edited\n"),
                       ("ozymandias.cabal", lambda text: text.replace("0.1.0.0", "0.1.0.1", 1)),
                       ("build/Added.hs", lambda _: "module Added where\n"))
            for rel, change in changes:
                path = root / "engine" / rel
                before = path.read_text() if path.exists() else None
                path.write_text(change(before))
                for command in ("build", "rebuild", "watch"):
                    with self.subTest(changed=rel, command=command):
                        shutil.rmtree(root / "_site", ignore_errors=True)
                        done = self.run_site(root, command)
                        self.assertNotEqual(done.returncode, 0)
                        self.assertIn("compiled from other sources than engine/ holds", done.stderr)
                        self.assertFalse((root / "_site").exists())
                if before is None:
                    path.unlink()
                else:
                    path.write_text(before)

    def test_project_compiler_settings_are_part_of_the_match(self):
        with tempfile.TemporaryDirectory(prefix="engine-project-") as tmp:
            root = Path(tmp)
            write_site_config(root)
            shutil.copytree(ENGINE / "build", root / "engine/build")
            for name in ("ozymandias.cabal", "cabal.project.freeze"):
                shutil.copy2(ENGINE / name, root / "engine" / name)
            shutil.copy2(SITE_ROOT / "cabal.project", root / "cabal.project")
            (root / "cabal.project.freeze").symlink_to("engine/cabal.project.freeze")
            self.assertNotIn("compiled from other sources", self.run_site(root, "build").stderr)
            for name, text in (("cabal.project.local", "package ozymandias\n  ghc-options: -O0\n"),
                               ("cabal.project", "packages: engine\nwith-compiler: another-ghc\n")):
                with self.subTest(file=name):
                    path = root / name
                    before = path.read_bytes() if path.exists() else None
                    path.write_text(text)
                    done = self.run_site(root, "build")
                    self.assertNotEqual(done.returncode, 0)
                    self.assertIn("compiled from other sources", done.stderr)
                    path.unlink() if before is None else path.write_bytes(before)

    def test_a_tree_without_the_engine_s_sources_is_not_compared(self):
        # The tests' scratch sites take the engine's registry and nothing else.
        with tempfile.TemporaryDirectory(prefix="engine-version-") as tmp:
            root = Path(tmp)
            write_site_config(root)
            done = self.run_site(root, "build")
            self.assertNotIn("compiled from other sources", done.stderr)

    def test_other_commands_don_t_compare(self):
        # The tools ask the generator last built (sitelib.site_config) without
        # compiling it: they may run while the engine is being edited.
        with tempfile.TemporaryDirectory(prefix="engine-version-") as tmp:
            root = Path(tmp)
            write_site_config(root)
            shutil.copytree(ENGINE / "build", root / "engine/build")
            (root / "engine/ozymandias.cabal").write_text("edited\n")
            done = self.run_site(root, "config")
            self.assertEqual(done.returncode, 0, done.stderr)
            self.assertTrue(re.match(r"\{", done.stdout))


if __name__ == "__main__":
    unittest.main()
