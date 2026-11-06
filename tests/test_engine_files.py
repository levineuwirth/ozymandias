"""The engine's files inside a site's build (build/EngineFiles.hs).

A site is built from its own directory with the engine at engine/. Each
engine file stands in for the path it has without that prefix, and a site
file at that path overrides it: templates by alias, static files by route,
raw reads through one resolver. These probes run the real rules over a
scratch site.
"""
import os
from pathlib import Path
import re
import shutil
import struct
import subprocess
import tempfile
import unittest

from tests._helpers import ENGINE, SITE_ROOT, engine_path, requires_cabal, script_env, site_binary

PROBE = '''{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forM_, (>=>))
import EngineFiles
import Compilers (essayCompiler, photographyCompiler)
import Config (configRules)
import Hakyll
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \\args -> case args of
    "resolve" : paths -> mapM_ (resolveFile >=> putStrLn) paths
    "existing" : paths -> mapM_ (existingFile >=> print) paths
    "files" : dirs -> mapM_ (filesIn >=> mapM_ putStrLn) dirs
    _ -> hakyll $ do
        twins <- engineTwins
        staticRules twins []
        templateRules twins
        configRules
        dims <- makePatternDependency (staticImageDimensions .&&. hasNoVersion)
        rulesExtraDependencies [dims] $ match "content/essays/*.md" $ do
            route $ setExtension "html"
            compile essayCompiler
        match "content/photography/*.md" $ do
            route $ setExtension "html"
            compile photographyCompiler
        forM_ ["plain.html", "shadowed.html"] $ \\name ->
            create [fromFilePath name] $ do
                route idRoute
                compile $ makeItem ("" :: String)
                    >>= loadAndApplyTemplate (fromFilePath ("templates/" ++ name)) defaultContext
        create ["stamped.html"] $ do
            route idRoute
            compile $ makeItem ("$assets$" :: String) >>= applyAsTemplate assetsField
'''


@requires_cabal
class EngineFilesTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        tmp = tempfile.TemporaryDirectory(prefix="engine-files-probe-")
        cls.addClassCleanup(tmp.cleanup)
        build = Path(tmp.name)
        (build / "Probe.hs").write_text(PROBE)
        cls.binary = build / "probe"
        done = subprocess.run(["cabal", "exec", "--", "ghc", "-v0", f"-i{engine_path('build')}",
                               "-outputdir", str(build), str(build / "Probe.hs"),
                               "-o", str(cls.binary)], cwd=ENGINE, env=script_env(),
                              capture_output=True, text=True, timeout=180)
        if done.returncode:
            raise AssertionError(done.stderr)

    def setUp(self):
        tmp = tempfile.TemporaryDirectory(prefix="engine-files-")
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        # The probe renders a page through each.
        self.write("engine/templates/plain.html", "engine plain")
        self.write("engine/templates/shadowed.html", "engine shadowed")

    def write(self, rel, text, mtime=None):
        path = self.root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)
        if mtime is not None:
            os.utime(path, (mtime, mtime))
        return path

    def run_probe(self, *args):
        done = subprocess.run([str(self.binary), *args], cwd=self.root, env=script_env(),
                              capture_output=True, text=True, timeout=60)
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        return done.stdout

    def built(self, rel):
        return (self.root / "_site" / rel).read_text()

    def versions(self):
        return {p.name for p in (self.root / "_site/assets").iterdir()}

    def version(self):
        """The version the build's pages name; its files are under it."""
        stamped = self.built("stamped.html")
        self.assertTrue(stamped.startswith("/assets/"), stamped)
        return stamped.removeprefix("/assets/")

    def asset(self, rel):
        return self.built(f"assets/{self.version()}/{rel}")

    def test_engine_templates_answer_to_their_site_names_and_site_templates_win(self):
        self.write("engine/templates/plain.html",
                   '<!-- an editor\'s note -->engine plain, $partial("templates/partials/p.html")$')
        self.write("engine/templates/partials/p.html", "engine partial")
        self.write("templates/shadowed.html", 'site shadowed, $partial("templates/partials/p.html")$')
        self.run_probe("build")
        self.assertEqual(self.built("plain.html"), "engine plain, engine partial")
        self.assertEqual(self.built("shadowed.html"), "site shadowed, engine partial")
        # A site partial reaches the engine's templates that include it.
        self.write("templates/partials/p.html", "site partial")
        self.run_probe("build")
        self.assertEqual(self.built("plain.html"), "engine plain, site partial")
        self.assertEqual(self.built("shadowed.html"), "site shadowed, site partial")

    def test_template_edits_and_new_overrides_rebuild_without_a_clean(self):
        self.run_probe("build")
        stamp = (self.root / "engine/templates/plain.html").stat().st_mtime
        self.write("engine/templates/plain.html", "engine plain, edited", stamp + 2)
        self.run_probe("build")
        self.assertEqual(self.built("plain.html"), "engine plain, edited")
        # An override older than the engine's file still wins: it is a new
        # identifier, so it is out of date whatever its mtime.
        self.write("templates/plain.html", "site plain", stamp - 100)
        self.run_probe("build")
        self.assertEqual(self.built("plain.html"), "site plain")
        # Removing it brings back an alias whose stored body is the site's
        # and whose dependencies have not changed, so it needs the clean
        # build that tools/build-freshness.sh forces when the overrides
        # change (tests/test_build_freshness.py).
        (self.root / "templates/plain.html").unlink()
        self.run_probe("clean")
        self.run_probe("build")
        self.assertEqual(self.built("plain.html"), "engine plain, edited")

    def test_engine_static_files_are_served_as_the_site_s_and_site_files_win(self):
        self.write("engine/static/css/base.css", "a {\n  color: red;\n}\n")
        self.write("engine/static/js/engine.js", "engine();\n")
        self.write("engine/static/js/shared.js", "engine();\n")
        self.write("static/js/shared.js", "site();\n")
        self.write("engine/static/images/a.png.dims.yaml", "width: 1\n")
        self.write("static/css/site.css", "b { color: blue; }\n")
        self.run_probe("build")
        self.assertEqual(self.asset("css/base.css"), "a{color:red}")
        self.assertEqual(self.asset("css/site.css"), "b{color:blue}")
        self.assertEqual(self.asset("js/engine.js"), "engine();\n")
        self.assertEqual(self.asset("js/shared.js"), "site();\n")
        self.assertFalse((self.root / "_site/images/a.png.dims.yaml").exists())
        self.assertFalse((self.root / "_site/engine").exists())
        # First-party JS and CSS are only under their version.
        self.assertFalse((self.root / "_site/js").exists())
        self.assertFalse((self.root / "_site/css").exists())

    def test_the_version_follows_the_files_and_every_file_and_page_follow_it(self):
        self.write("engine/static/css/base.css", "a {\n  color: red;\n}\n", mtime=1000)
        self.write("engine/static/js/engine.js", "one();\n", mtime=1000)
        self.write("engine/static/css/seasons/dusk.css", "b {\n  color: blue;\n}\n", mtime=1000)
        self.write("engine/static/images/a.txt", "one", mtime=1000)
        self.run_probe("build")
        first = self.version()
        self.assertEqual(self.versions(), {first})
        self.assertRegex(first, r"^[0-9a-f]{12}$")
        # A file of another kind, and no change at all, leave it.
        self.write("engine/static/images/a.txt", "two", mtime=2000)
        self.run_probe("build")
        self.assertEqual(self.version(), first)
        # An edit moves every file, not only that one, and the page with them;
        # what is left of the old version is untouched here (engine.mk prunes).
        self.write("engine/static/js/engine.js", "two();\n", mtime=3000)
        self.run_probe("build")
        second = self.version()
        self.assertEqual(self.versions(), {first, second})
        self.assertEqual(self.asset("js/engine.js"), "two();\n")
        self.assertEqual(self.asset("css/base.css"), "a{color:red}")
        # Below the top directory a stylesheet is copied, not compressed.
        self.assertEqual(self.asset("css/seasons/dusk.css"), "b {\n  color: blue;\n}\n")
        self.assertEqual(self.built(f"assets/{first}/js/engine.js"), "one();\n")
        # A site's own file counts, and an override of an engine file.
        self.write("static/css/site.css", "c{}", mtime=4000)
        self.run_probe("build")
        third = self.version()
        self.assertNotIn(third, (first, second))
        self.assertEqual(self.asset("css/site.css"), "c{}")
        self.write("static/js/engine.js", "site();\n", mtime=5000)
        self.run_probe("build")
        fourth = self.version()
        self.assertNotIn(fourth, (first, second, third))
        self.assertEqual(self.asset("js/engine.js"), "site();\n")
        # So does a file's going, and its coming back restores the version.
        (self.root / "static/css/site.css").unlink()
        self.run_probe("build")
        fifth = self.version()
        self.assertNotIn(fifth, (first, second, third, fourth))
        self.assertFalse((self.root / f"_site/assets/{fifth}/css/site.css").exists())
        self.write("static/css/site.css", "c{}", mtime=6000)
        self.run_probe("build")
        self.assertEqual(self.version(), fourth)

    def test_template_migration_cleans_then_engine_edits_rebuild(self):
        # Exercise the real freshness decision and alias rules together.
        from tests.test_build_freshness import BuildFreshnessTestCase
        fixture = BuildFreshnessTestCase()
        fixture.binary = site_binary()
        self.addCleanup(fixture.doCleanups)
        fixture.setUp()
        self.root = fixture.root
        # The probe has no need for the helper's dummy essay or minimal config.
        (self.root / "content/essays/one.md").unlink()
        shutil.copy2(SITE_ROOT / "site.yaml", self.root / "site.yaml")
        self.write("templates/plain.html", "before move")
        self.write("templates/shadowed.html", "unchanged")
        fixture.git("add", "content", "site.yaml", "templates")
        fixture.git("commit", "-q", "-m", "probe inputs")
        self.run_probe("build")
        fixture.run_script("stamp")
        (self.root / "engine/templates").mkdir(parents=True)
        (self.root / "templates/plain.html").rename(self.root / "engine/templates/plain.html")
        self.assertIn("build rules changed", fixture.check())
        self.assertTrue(fixture.cleaned())
        self.run_probe("clean")
        self.run_probe("build")
        fixture.run_script("stamp")
        (self.root / "CLEANED").unlink()
        self.assertEqual(self.built("plain.html"), "before move")
        stamp = (self.root / "engine/templates/plain.html").stat().st_mtime
        self.write("engine/templates/plain.html", "after engine edit", stamp + 2)
        self.assertIn("incremental build OK", fixture.check())
        self.run_probe("build")
        self.assertEqual(self.built("plain.html"), "after engine edit")

    def image_pages(self):
        shutil.copy2(SITE_ROOT / "site.yaml", self.root / "site.yaml")
        # Its switches resolve over the engine's registry, where a site has it.
        (self.root / "engine").mkdir(exist_ok=True)
        (self.root / "engine/switches.yaml").symlink_to(engine_path("switches.yaml"))
        pages = ("content/essays/image.md", "content/photography/image.md")
        for page in pages:
            self.write(page, "![Standalone](/images/probe.jpg)\n\n"
                       "Inline ![inline](/images/probe.jpg) image.\n\n"
                       "![Local](local.jpg)\n")
            self.write(str(Path(page).parent / "local.jpg.dims.yaml"), "width: 33\nheight: 22\n")
        self.write("static/images/probe.jpg", "image fixture")
        self.write("static/images/probe.webp", "webp fixture")
        self.write("static/images/probe.jpg.dims.yaml", "width: 640\nheight: 480\n")
        return [str(Path(page).with_suffix(".html")) for page in pages]

    def test_image_rendering_survives_move_and_site_sidecars_win(self):
        pages = self.image_pages()
        self.run_probe("build")
        before = [self.built(page) for page in pages]
        for html in before:
            self.assertEqual(html.count('<picture>'), 2)
            self.assertEqual(html.count('width="640"'), 2)
            self.assertIn('width="33"', html)
        (self.root / "static").rename(self.root / "engine/static")
        self.run_probe("clean")
        self.run_probe("build")
        self.assertEqual([self.built(page) for page in pages], before)
        self.write("static/images/probe.jpg.dims.yaml", "width: 320\nheight: 240\n")
        self.run_probe("build")
        for page in pages:
            self.assertEqual(self.built(page).count('width="320"'), 2)
        # Shadowed engine sidecars must not make the dependency pattern
        # disagree with Hakyll's claimed identifiers on every build.
        log = self.run_probe("build")
        for page in pages:
            self.assertNotIn("updated " + str(Path(page).with_suffix(".md")), log)

    def test_engine_sidecar_edits_rebuild_essay_and_photo(self):
        pages = self.image_pages()
        (self.root / "static").rename(self.root / "engine/static")
        self.run_probe("build")
        sidecar = self.root / "engine/static/images/probe.jpg.dims.yaml"
        self.write(str(sidecar.relative_to(self.root)), "width: 800\nheight: 600\n",
                   sidecar.stat().st_mtime + 2)
        self.run_probe("build")
        for page in pages:
            self.assertEqual(self.built(page).count('width="800"'), 2)

    def test_raw_reads_take_the_site_file_then_the_engine_s(self):
        self.write("data/both.csl", "site")
        self.write("engine/data/both.csl", "engine")
        self.write("engine/data/engine.csl", "engine")
        self.assertEqual(self.run_probe("resolve", "data/both.csl", "data/engine.csl", "data/none.csl")
                         .splitlines(), ["data/both.csl", "engine/data/engine.csl", "data/none.csl"])
        self.assertEqual(self.run_probe("existing", "data/engine.csl", "data/none.csl").splitlines(),
                         ['Just "engine/data/engine.csl"', "Nothing"])

    def test_a_directory_lists_the_site_s_and_the_engine_s_entries_once(self):
        for rel in ("build/Site.hs", "engine/build/Site.hs", "engine/build/Main.hs",
                    "engine/static/css/base.css"):
            self.write(rel, "")
        self.assertEqual(self.run_probe("files", "build", "static/css", "static/js").splitlines(),
                         ["build/Site.hs", "engine/build/Main.hs", "engine/static/css/base.css"])


@requires_cabal
class ProviderWalkTests(unittest.TestCase):
    def test_the_engine_s_demonstration_site_and_build_directories_are_not_walked(self):
        refused = ["engine/fixture-site/content/a.md", "engine/dist-newstyle/cache/plan.json",
                   "engine/fixture-site/static/x.css"]
        walked = ["engine/templates/page.html", "engine/static/css/base.css",
                  "engine/tests/test_a.py", "content/essays/a.md"]
        done = subprocess.run([str(site_binary()), "private-paths"], cwd=ENGINE,
                              input="\n".join(refused + walked) + "\n",
                              capture_output=True, text=True, check=True)
        self.assertEqual(done.stdout.splitlines(), refused)



@requires_cabal
class RefreezeTests(unittest.TestCase):
    def test_refreeze_verifies_a_package_below_the_site_root(self):
        with tempfile.TemporaryDirectory(prefix="refreeze-engine-") as tmp:
            root = Path(tmp)
            engine = root / "engine"
            engine.mkdir()
            # A real, minimal Cabal package: no network or external libraries.
            (root / "cabal.project").write_text(
                "packages: engine\nactive-repositories: :none\n")
            (engine / "example-engine.cabal").write_text(
                "cabal-version: 3.0\nname: example-engine\nversion: 0.1.0.0\n"
                "build-type: Simple\nexecutable site\n  main-is: Main.hs\n"
                "  build-depends: base\n  default-language: Haskell2010\n")
            (engine / "Main.hs").write_text('main :: IO ()\nmain = putStrLn "built"\n')
            done = subprocess.run(["bash", str(engine_path("tools/refreeze.sh"))],
                                  cwd=root, env=script_env(), capture_output=True,
                                  text=True, timeout=120)
            self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
            self.assertTrue((root / "cabal.project.freeze").is_file())
            binary = subprocess.check_output(["cabal", "list-bin", "-v0", "exe:site"],
                                             cwd=root, env=script_env(), text=True).strip()
            self.assertEqual(subprocess.check_output([binary], text=True).strip(), "built")


# A root-relative image the engine's templates, styles or scripts name.
# PDF.js, Leaflet and the search model are downloaded with their own files.
IMAGE_URL = re.compile(r"""(?:["'(]|\$site-url\$)(/[\w./-]+\.(?:svg|png|ico|jpe?g|webp|gif))""")
DOWNLOADED = ("/pdfjs/", "/leaflet/", "/models/")


class EngineDefaultsTests(unittest.TestCase):
    """A site brings its own icons, logo, preview card and ornaments
    (overrides.yaml); the engine has a neutral default for each, so a site
    that brings none still builds."""

    def test_every_image_the_engine_names_is_its_own(self):
        named = set()
        for directory in ("templates", "static/css", "static/js"):
            for path in engine_path(directory).rglob("*"):
                if path.is_file():
                    named.update(url for url in IMAGE_URL.findall(path.read_text(errors="replace"))
                                 if not url.startswith(DOWNLOADED))
        self.assertGreater(len(named), 40)
        static = engine_path("static")
        self.assertEqual(sorted(u for u in named if not (static / u.lstrip("/")).is_file()), [])

    def test_each_image_is_the_size_the_engine_declares(self):
        head = engine_path("templates/partials/head.html").read_text()
        declared = {}
        for tag in re.findall(r"<link [^>]*>", head):
            href, sizes = re.search(r'href="(/[^"]+\.png)"', tag), re.search(r'sizes="(\d+)x(\d+)"', tag)
            if href and sizes:
                declared[href[1]] = (int(sizes[1]), int(sizes[2]))
        card = re.search(r'property="og:image" content="\$site-url\$(/[^"]+)"', head)[1]
        declared[card] = tuple(int(re.search(rf'property="og:image:{side}" content="(\d+)"', head)[1])
                               for side in ("width", "height"))
        manifest = engine_path("templates/site.webmanifest").read_text()
        for src, width, height in re.findall(r'"src": "([^"]+)",\s*"sizes": "(\d+)x(\d+)"', manifest):
            declared[src] = (int(width), int(height))
        self.assertEqual(len(declared), 5)
        for url, size in declared.items():
            # The engine's default, and the site's own where it has one.
            rel = url.lstrip("/")
            images = [engine_path("static") / rel]
            if (SITE_ROOT / "static" / rel).is_file():
                images.append(SITE_ROOT / "static" / rel)
            for image in dict.fromkeys(images):
                with self.subTest(image=image):
                    header = image.read_bytes()[:24]
                    self.assertEqual(header[:8], b"\x89PNG\r\n\x1a\n")
                    self.assertEqual(struct.unpack(">II", header[16:24]), size)


if __name__ == "__main__":
    unittest.main()
