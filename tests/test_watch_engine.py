"""Real watch sessions must use new compiled rules and re-evaluate overrides."""
import os
import shutil
import signal
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path
from tests import fixture_variants
from tests._helpers import ENGINE, SITE_ROOT, requires_cabal, script_env


@requires_cabal
class WatchEngine(unittest.TestCase):
    def test_compiler_edits_and_file_changes_restart_under_the_lock(self):
        with tempfile.TemporaryDirectory(prefix="watch-engine-") as tmp:
            root = Path(tmp)
            engine, site = root / "ozymandias", root / "site"
            shutil.copytree(ENGINE, engine, ignore=shutil.ignore_patterns(
                "static", "tests", "fixture-site", "dist-newstyle", "__pycache__", ".git"))
            (engine / "static").symlink_to(ENGINE / "static")
            build = next(b for b in fixture_variants.builds() if b.name == "minimal")
            fixture_variants.copy_site(build, site, engine)
            (site / "cabal.project").write_text((SITE_ROOT / "cabal.project").read_text())
            (site / "cabal.project.freeze").symlink_to("engine/cabal.project.freeze")
            for args in (("init", "-q"), ("add", "."), ("commit", "-qm", "fixture")):
                subprocess.run(["git", "-c", "user.name=Fixture", "-c", "user.email=test@example.org",
                                "-c", "commit.gpgsign=false", *args], cwd=site, check=True,
                               capture_output=True)
            logpath = root / "watch.log"
            env = script_env(SITE_ENV="production", WATCH_ENGINE_INTERVAL="0.1")
            command = ["bash", str(engine / "tools/with-lock.sh"), "data/.site-build.lock", sys.executable,
                       str(engine / "tools/watch-engine.py"), "--", "cabal", "run", "-v0", "site", "--",
                       "watch", "--no-server"]
            with logpath.open("w") as log:
                proc = subprocess.Popen(command, cwd=site, env=env, stdout=log, stderr=subprocess.STDOUT,
                                        start_new_session=True)
                def until(predicate):
                    deadline = time.monotonic() + 240
                    while time.monotonic() < deadline:
                        if proc.poll() is not None:
                            self.fail(logpath.read_text()[-6000:])
                        if predicate():
                            return
                        time.sleep(.2)
                    self.fail("watch timed out:\n" + logpath.read_text()[-6000:])
                def contains(rel, text):
                    path = site / "_site" / rel
                    return path.exists() and text in path.read_text()
                try:
                    until(lambda: contains("robots.txt", "Disallow: /proxy/"))
                    source = engine / "build/Site.hs"
                    source.write_text(source.read_text().replace("Disallow: /proxy/", "Disallow: /watch-test/"))
                    until(lambda: contains("robots.txt", "Disallow: /watch-test/"))
                    locked = subprocess.run(["bash", str(engine / "tools/with-lock.sh"),
                                             "data/.site-build.lock", "true"], cwd=site,
                                            env=env, capture_output=True)
                    self.assertNotEqual(locked.returncode, 0)
                    head = engine / "templates/partials/head.html"
                    original = head.read_text()
                    freeze = engine / "cabal.project.freeze"
                    freeze.write_text(freeze.read_text() + "\n-- watch dependency change\n")
                    head.write_text(original + '\n<meta name="watch-freeze" content="yes">\n')
                    until(lambda: contains("index.html", 'name="watch-freeze"'))
                    (site / "cabal.project.local").write_text(
                        "package ozymandias\n  ghc-options: -fno-ignore-asserts\n")
                    head.write_text(original + '\n<meta name="watch-project" content="yes">\n')
                    until(lambda: contains("index.html", 'name="watch-project"'))
                    extra = engine / "templates/partials/watch-extra.html"
                    extra.write_text('<meta name="watch-added" content="yes">\n')
                    head.write_text(original + '\n$partial("templates/partials/watch-extra.html")$\n')
                    until(lambda: contains("index.html", 'name="watch-added"'))
                    own = site / "templates/partials/head.html"
                    own.write_text(original + '\n<meta name="watch-override" content="yes">\n')
                    until(lambda: contains("index.html", 'name="watch-override"'))
                    head.write_text(original + '\n<meta name="watch-restored" content="yes">\n')
                    extra.unlink()
                    own.unlink()
                    until(lambda: contains("index.html", 'name="watch-restored"'))
                    self.assertFalse(contains("index.html", 'name="watch-override"'))
                    self.assertFalse(contains("index.html", 'name="watch-added"'))
                finally:
                    if proc.poll() is None:
                        os.killpg(proc.pid, signal.SIGINT)
                        try:
                            proc.wait(timeout=10)
                        except subprocess.TimeoutExpired:
                            os.killpg(proc.pid, signal.SIGKILL)
                            proc.wait()
