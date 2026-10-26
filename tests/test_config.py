"""The real config CLI, its consumers, and Hakyll dependency invalidation."""

import json
import os
import re
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from xml.etree import ElementTree

from tests._helpers import (ENGINE, engine_path, requires_cabal, script_env, site_binary,
                            write_site_config)


def standalone(source, route, **settings):
    page = {"source": source, "route": route, "template": "page.html", "context": "page",
            "scores": False, "dingbat": None, "flags": []}
    return page | settings


@requires_cabal
class ConfigTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.binary = site_binary()

    def setUp(self):
        tmp = tempfile.TemporaryDirectory(prefix="site-config-")
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.config = write_site_config(self.root)

    def write_config(self):
        (self.root / "site.yaml").write_text(json.dumps(self.config), encoding="utf-8")

    def run_site(self, *args):
        return subprocess.run([str(self.binary), *args], cwd=self.root, env=script_env(),
                              capture_output=True, text=True, timeout=120)

    def test_config_json_uses_this_sites_identity(self):
        done = self.run_site("config")
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(json.loads(done.stdout), self.config)
        self.assertFalse((self.root / "_cache").exists())
        self.assertFalse((self.root / "data").exists())

    def test_make_values_preserve_literal_metacharacters(self):
        # A make include must not expand dollars, lose text after '#', or
        # consume its terminator when a value ends in a backslash.
        self.config["title"] = "Élodie # $5 $(error expanded) \\"
        self.write_config()
        done = self.run_site("config", "--make")
        self.assertEqual(done.returncode, 0, done.stderr)
        (self.root / "config.mk").write_text(done.stdout)
        (self.root / "Makefile").write_text(
            "include config.mk\n"
            "$(file >title.txt,$(SITE_TITLE))\n"
            "$(file >config.json,$(SITE_CONFIG_JSON))\n"
            "all:;@:\n")
        made = subprocess.run(["make", "--no-print-directory"], cwd=self.root,
                              capture_output=True, text=True)
        self.assertEqual(made.returncode, 0, made.stderr)
        self.assertEqual((self.root / "title.txt").read_text(), self.config["title"] + "\n")
        self.assertEqual(json.loads((self.root / "config.json").read_text()), self.config)

    def test_missing_and_malformed_config_fail(self):
        path = self.root / "site.yaml"
        for text in ("[", "{}", "title: [not, a, string]"):
            with self.subTest(text=text):
                path.write_text(text)
                done = self.run_site("config")
                self.assertNotEqual(done.returncode, 0)
                self.assertIn("site.yaml", done.stderr)
        path.unlink()
        self.assertNotEqual(self.run_site("config").returncode, 0)

    def test_invalid_settings_are_rejected(self):
        feed = {"path": "feed.xml", "title": "Example", "description": "Posts",
                "sections": ["essays"], "advertise": True, "limit": 30}
        section = self.config["sections"][0]
        portal = self.config["portals"][0]
        link = self.config["nav-primary"][0]
        page = {"source": "content/me/index.md", "route": "me.html", "template": "essay.html",
                "context": "essay", "scores": True, "dingbat": None, "flags": []}
        bad = [
            {"titel": "misspelled"}, {"author": ""}, {"title": "one\ntwo"},
            {"url": "example.org"}, {"url": "https://example.org/"},
            {"host": "example.org/path"}, {"code-license": "../outside"},
            {"manifest": self.config["manifest"] | {"unknown": "value"}},
            {"manifest": self.config["manifest"] | {"name": ""}},
            {"manifest": self.config["manifest"] | {"description": "one\ntwo"}},
            {"signing-fingerprint": "one\ntwo"},
            {"host": "other.example"}, {"host": "www."}, {"host": "www.other.example"},
            {"feeds": [feed, feed]},
            *[{"feeds": [dict(feed, **change)]} for change in (
                {"path": "../feed.xml"}, {"path": "/feed.xml"}, {"limit": 0},
                {"sections": []}, {"sections": ["essays", "essays"]},
                {"sections": ["unknown"]}, {"sections": ["essays", "photography"]},
                {"adveritse": True}, {"advertise": "yes"},
            )],
            {"sections": None}, {"portals": None}, {"nav-primary": None}, {"library": None},
            *[{"sections": [dict(section, **change)] + self.config["sections"][1:]} for change in (
                {"dir": "Essays"}, {"dir": "essays/old"}, {"dir": "-essays"}, {"dir": ""},
                {"kind": ""}, {"dingbat": "unknown"}, {"landing": "no"}, {"order": 1},
            )],
            {"sections": [section, section]},
            *[{"portals": [dict(portal, **change)]} for change in (
                {"tag": "notes/sub"}, {"tag": "Notes"}, {"name": ""}, {"order": 1},
            )],
            {"portals": [portal, portal]},
            *[{"nav-primary": [dict(link, **change)]} for change in (
                {"href": "javascript:alert(1)"}, {"href": "//example.net/"}, {"href": "/a b"},
                {"href": "notes.html"}, {"label": ""}, {"title": "x"},
            )],
            *[{"library": self.config["library"] | change} for change in (
                {"shelf-cap": 0}, {"shelf-cap": 6}, {"description": "one\ntwo"},
                {"see-also": [dict(link, href="relative.html")]}, {"shelves": 3},
            )],
            {"standalone": None},
            *[{"standalone": [dict(page, **change)]} for change in (
                {"source": "pages/me.md"}, {"source": "content/a/b/c.md"}, {"source": "content/me.txt"},
                {"source": "content/../me.md"}, {"source": "content/*.md"}, {"source": "content/index.md"},
                {"route": "/me.html"}, {"route": "me"}, {"template": "../essay.html"},
                {"template": "essay.txt"}, {"context": "blog"}, {"scores": True, "source": "content/me.md"},
                {"dingbat": "unknown"}, {"flags": ["Not A Slug"]}, {"order": 1},
            )],
            {"standalone": [page, page]},
            {"standalone": [page, dict(page, source="content/other/index.md")]},
            {"standalone": [dict(page, source="content/essays/me.md", scores=False)]},
            {"standalone": [dict(page, source="content/tag-meta/me.md", scores=False)]},
        ]
        original = self.config.copy()
        for overrides in bad:
            with self.subTest(overrides=overrides):
                self.config = original | overrides
                self.write_config()
                done = self.run_site("config")
                self.assertNotEqual(done.returncode, 0, done.stdout)
                self.assertIn("site.yaml", done.stderr)

    def test_taxonomy_rejects_declarations_the_routes_cannot_honor(self):
        original = self.config["sections"]
        cases = [
            (original + [{"dir": "notes", "kind": "Note"}], "unsupported section directory"),
            # A page beside its directory is a standalone page now.
            (original + [{"dir": "memento-mori", "kind": "Page"}], "unsupported section directory"),
            ([dict(s, landing=False) if s["dir"] == "essays" else s
              for s in original], "unknown configuration keys"),
            *[([s for s in original if s["dir"] != omitted["dir"]], "must include")
              for omitted in original],
        ]
        for sections, error in cases:
            with self.subTest(sections=sections):
                self.config["sections"] = sections
                self.write_config()
                # Refuse at rule construction, before content can be dropped
                # or a wikilink can be directed to a nonexistent landing.
                done = self.run_site("list-routes")
                self.assertNotEqual(done.returncode, 0)
                self.assertIn(error, done.stderr)

    def test_taxonomy_lists_derive_from_sections_and_portals(self):
        self.config["sections"] = [dict(s, kind="Custom label", dingbat="trefoil")
                                   for s in reversed(self.config["sections"])]
        self.config["portals"] = [{"name": "Walks", "tag": "walks"}]
        self.config["standalone"] = [
            standalone("content/memento-mori/index.md", "memento-mori.html"),
            standalone("content/people/team.md", "people/team/index.html"),
            standalone("content/studio.md", "studio/index.html")]
        self.write_config()
        done = self.run_site("shared-rules")
        self.assertEqual(done.returncode, 0, done.stderr)
        rules = json.loads(done.stdout)
        reserved, owned, slugs = (set(rules[k]) for k in
                                  ("reserved-sections", "section-owned-tags", "directory-slugs"))
        self.assertEqual(rules["reserved-sections"], sorted(reserved))
        # A standalone page's directory is reserved; a route under a
        # directory claims that namespace from the tags.
        self.assertTrue({"essays", "memento-mori", "people"} <= reserved)
        self.assertIn("essays", owned & slugs)
        self.assertIn("people", owned - slugs)
        # A standalone page routed to a directory index is linked there.
        self.assertTrue({"studio", "people/team"} <= slugs - reserved)
        self.assertIn("studio", owned)
        self.assertNotIn("memento-mori", owned | slugs)
        self.assertIn("walks", slugs - owned - reserved)
        self.assertFalse({"research", "cv", "me"} & (reserved | owned | slugs))

        tagged = subprocess.run([str(self.binary), "expand-tags"], cwd=self.root, env=script_env(),
                                input="essays\nwalks\nessays/field\n", capture_output=True,
                                text=True, timeout=60)
        self.assertEqual(tagged.returncode, 0, tagged.stderr)
        self.assertEqual(tagged.stdout.split(), ["walks", "essays/field"])
        (self.root / "data").mkdir()
        page = self.root / "content/essays/page.md"
        page.parent.mkdir(parents=True)
        page.write_text("---\ntitle: Page\ndate: 2026-01-01\ntags: [walks]\n---\n"
                        "[[Walks]], [[Essays]], [[Studio]] and [[Memento Mori]]\n")
        memento = self.root / "content/memento-mori/index.md"
        memento.parent.mkdir()
        memento.write_text("---\ntitle: Memento\n---\nBody\n")
        notes = self.root / "content/notes/a.md"
        notes.parent.mkdir()
        notes.write_text("---\ntitle: Note\n---\nBody\n")
        team = self.root / "content/people/team.md"
        team.parent.mkdir()
        team.write_text("---\ntitle: Team\n---\nBody\n")
        (self.root / "content/studio.md").write_text("---\ntitle: Studio\n---\nBody\n")
        links = self.run_site("list-links", "content")
        self.assertEqual(links.returncode, 0, links.stderr)
        self.assertEqual(links.stdout.split(), ["/essays/", "/memento-mori.html", "/studio/", "/walks/"])
        # Transclusion resolves the same slugs.
        fixture = self.root / "transclusions.md"
        fixture.write_text("---\ntitle: Fixture\n---\n{{studio}}\n\n{{people/team}}\n")
        rendered = self.run_site("render-fixture", str(fixture))
        self.assertEqual(rendered.returncode, 0, rendered.stderr)
        self.assertEqual(re.findall(r'data-src="([^"]*)"', rendered.stdout), ["/studio/", "/people/team/"])
        done = self.run_site("list-routes")
        self.assertEqual(done.returncode, 0, done.stderr)
        routes = {line.split("\t")[0] for line in done.stdout.splitlines()}
        self.assertIn("notes/a.html", routes)  # ordinary collections still compile
        self.assertTrue({"memento-mori.html", "people/team/index.html", "studio/index.html"} <= routes)
        for url in links.stdout.split():
            self.assertIn(url.lstrip("/") + ("index.html" if url.endswith("/") else ""), routes)

    def test_tools_ask_the_generator_about_the_site_they_work_on(self):
        # Run from elsewhere with SITE_ROOT, a tool reads that site's
        # configuration, not the working directory's: another site's, or
        # none at all.
        self.config["portals"] = [{"name": "Walks", "tag": "walks"}]
        self.write_config()
        page = self.root / "content/essays/a.md"
        page.parent.mkdir(parents=True)
        page.write_text("---\ntitle: A\n---\nSee [code](https://github.com/o/r/blob/main/a.py).\n")
        (self.root / "data").mkdir()
        other, empty = self.root / "other", self.root / "empty"
        empty.mkdir()
        other.mkdir()
        write_site_config(other, portals=[{"name": "Notes", "tag": "notes"}])
        tools = engine_path("tools/sitelib.py").parent
        probe = (f"import json, sys, importlib.util; sys.path.insert(0, {str(tools)!r})\n"
                 "import shared_rules\n"
                 f"spec = importlib.util.spec_from_file_location('code_refs', {str(tools / 'code-refs.py')!r})\n"
                 "code_refs = importlib.util.module_from_spec(spec); spec.loader.exec_module(code_refs)\n"
                 "print(json.dumps([shared_rules.shared_rules()['directory-slugs'],\n"
                 "                  sorted(code_refs.discover_links())]))\n")
        for elsewhere in (other, empty):
            with self.subTest(cwd=elsewhere.name):
                done = subprocess.run([sys.executable, "-c", probe], cwd=elsewhere,
                                      capture_output=True, text=True, timeout=60,
                                      env=script_env(SITE_ROOT=str(self.root),
                                                     SITE_BINARY=str(self.binary)))
                self.assertEqual(done.returncode, 0, done.stderr)
                slugs, links = json.loads(done.stdout)
                self.assertIn("walks", slugs)
                self.assertNotIn("notes", slugs)
                self.assertEqual(links, ["https://github.com/o/r/blob/main/a.py"])

    def test_unclaimed_pages_in_a_standalone_directory_are_refused(self):
        self.config["standalone"] = [standalone("content/people/team.md", "people/team/index.html")]
        self.write_config()
        (self.root / "data").mkdir()
        people = self.root / "content/people"
        people.mkdir(parents=True)
        for name in ("team.md", "other.md"):
            (people / name).write_text("---\ntitle: Page\n---\nBody\n")
        # The directory is reserved from page collections, so other.md would
        # otherwise be compiled by no rule at all.
        done = self.run_site("list-routes")
        self.assertNotEqual(done.returncode, 0)
        self.assertIn("no standalone entry for content/people/other.md", done.stderr)
        (people / "other.md").unlink()
        done = self.run_site("list-routes")
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertIn("people/team/index.html\tcontent/people/team.md", done.stdout.splitlines())

    def test_url_may_be_the_www_alias_of_host(self):
        self.config["url"] = "https://www.example.org"
        self.write_config()
        done = self.run_site("config")
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(json.loads(done.stdout)["url"], "https://www.example.org")

    def test_feed_routes_and_default_author_follow_config(self):
        (self.root / "data").mkdir()
        page = self.root / "content/essays/example.md"
        page.parent.mkdir(parents=True)
        page.write_text("---\ntitle: Example\ndate: 2026-01-01\n---\nText.\n")
        self.config["feeds"] = [
            {"path": "updates.xml", "title": "Updates", "description": "Posts",
             "sections": ["essays"], "limit": 2},
            {"path": "photos.xml", "title": "Photos", "description": "Pictures",
             "sections": ["photography"], "limit": 5},
        ]
        self.write_config()
        done = self.run_site("list-routes")
        self.assertEqual(done.returncode, 0, done.stderr)
        routes = {line.split("\t")[0] for line in done.stdout.splitlines()}
        self.assertTrue({"updates.xml", "photos.xml", "authors/example-author/index.html"} <= routes)
        self.assertTrue({"feed.xml", "music/feed.xml", "photography/feed.xml"}.isdisjoint(routes))

    def test_link_filters_use_configured_hosts_and_repository(self):
        # The host may be named bare or with www., and url may use either:
        # links to both forms are the site's own in every combination.
        original = self.config.copy()
        for url, host in (("https://example.org", "example.org"),
                          ("https://www.example.org", "www.example.org"),
                          ("https://example.org", "www.example.org"),
                          ("https://www.example.org", "example.org")):
            with self.subTest(url=url, host=host):
                self.config = original | {"url": url, "host": host}
                self.check_link_filters()

    def check_link_filters(self):
        (self.root / "Makefile").write_text("all:;@:\n")
        (self.root / "probe.md").write_text(
            "[own](https://example.org/page)\n\n"
            "[www](https://www.example.org/page)\n\n"
            "[lookalike](https://example.org.attacker.invalid/page)\n\n"
            "[forge](https://git.example.org/author/other)\n\n"
            "[source](https://git.example.org/author/site/src/branch/main/Makefile)\n\n"
            "[configured](https://git.example.org/author/site/raw/branch/next/docs/Makefile)\n\n"
            "`Makefile`\n")
        self.config["source-branch"] = "next/docs"
        self.write_config()
        done = self.run_site("render-fixture", "probe.md")
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(done.stdout.count('class="link-external"'), 2)
        self.assertIn('data-link-icon="forgejo"', done.stdout)
        self.assertEqual(done.stdout.count('class="source-ref"'), 3)
        self.assertIn('href="https://git.example.org/author/site/src/branch/next/docs/Makefile"', done.stdout)
        self.assertNotIn("levineuwirth", done.stdout)


class FeedRenderingTests(unittest.TestCase):
    def test_every_feed_goes_through_render_site_feed(self):
        # An empty feed is valid, and configured text is escaped, only
        # through Config.renderSiteFeed; a rule calling Hakyll's renderers
        # directly would write <updated>Unknown or unescaped settings.
        build = engine_path("build")
        for path in sorted(build.rglob("*.hs")):
            if path.name == "Config.hs":
                continue
            with self.subTest(path=path.relative_to(build.parent)):
                text = path.read_text(encoding="utf-8")
                self.assertNotRegex(text, r"\brender(Atom|Rss)\b")


PROBE = '''{-# LANGUAGE OverloadedStrings #-}
import Config
import Hakyll
main :: IO ()
main = hakyll $ do
    configRules
    match "posts/*" $ do
        route $ setExtension "html"
        compile getResourceBody
    let ctx = bodyField "description" <> dateField "updated" "%Y-%m-%dT%H:%M:%SZ" <> defaultContext
    create ["empty.xml"] $ do
        route idRoute
        compile $ do
            (cfg, spec) <- loadFeed "empty.xml"
            renderSiteFeed cfg spec ctx []
    create ["full.xml"] $ do
        route idRoute
        compile $ do
            (cfg, spec) <- loadFeed "full.xml"
            renderSiteFeed cfg spec ctx =<< loadAll "posts/*"
'''

ATOM = "{http://www.w3.org/2005/Atom}"


@requires_cabal
class FeedProbeTests(unittest.TestCase):
    """Config.renderSiteFeed, driven by a small Hakyll program built against
    the real Config module."""

    @classmethod
    def setUpClass(cls):
        tmp = tempfile.TemporaryDirectory(prefix="config-feed-probe-")
        cls.addClassCleanup(tmp.cleanup)
        build = Path(tmp.name)
        (build / "Probe.hs").write_text(PROBE)
        cls.binary = build / "probe"
        compiled = subprocess.run(
            ["cabal", "exec", "--", "ghc", "-v0", f"-i{engine_path('build')}", "-outputdir", str(build),
             str(build / "Probe.hs"), "-o", str(cls.binary)], cwd=ENGINE, env=script_env(),
            capture_output=True, text=True, timeout=180)
        if compiled.returncode:
            raise AssertionError(compiled.stderr)

    def render(self, title: str = "Full", **config) -> dict[str, str]:
        with tempfile.TemporaryDirectory(prefix="config-feed-") as tmp:
            root = Path(tmp)
            feed = {"description": "Posts", "sections": ["essays"], "limit": None}
            write_site_config(root, feeds=[dict(feed, path="empty.xml", title="Empty"),
                                           dict(feed, path="full.xml", title=title)], **config)
            (root / "posts").mkdir()
            (root / "posts/a.md").write_text("---\ntitle: A\ndate: 2026-01-02\n---\nText.\n")
            done = subprocess.run([str(self.binary), "build"], cwd=root, env=script_env(),
                                  capture_output=True, text=True, timeout=60)
            self.assertEqual(done.returncode, 0, done.stderr)
            return {name: (root / "_site" / name).read_text(encoding="utf-8")
                    for name in ("empty.xml", "full.xml")}

    def test_an_empty_feed_has_a_valid_updated_and_a_populated_one_keeps_its_date(self):
        feeds = self.render()
        updated = re.compile(r"<updated>([^<]*)</updated>")
        self.assertRegex(updated.search(feeds["empty.xml"]).group(1),
                         r"^\d{4}-\d\d-\d\dT\d\d:\d\d:\d\dZ$")
        self.assertNotIn("Unknown", feeds["empty.xml"])
        self.assertEqual(updated.search(feeds["full.xml"]).group(1), "2026-01-02T00:00:00Z")

    def test_configured_text_is_escaped_and_parses_back_to_itself(self):
        title = "Art & photography <in> \"quotes\" and 'apostrophes'"
        author, email = "Alice & Bob <Studio>", "alice&bob@example.org"
        feeds = self.render(title=title, author=author, email=email)
        for name, expected in (("full.xml", title), ("empty.xml", "Empty")):
            with self.subTest(feed=name):
                root = ElementTree.fromstring(feeds[name])
                self.assertEqual(root.findtext(f"{ATOM}title"), expected)
                self.assertEqual(root.findtext(f"{ATOM}author/{ATOM}name"), author)
                self.assertEqual(root.findtext(f"{ATOM}author/{ATOM}email"), email)
                self.assertEqual(root.find(f"{ATOM}link[@rel='self']").get("href"),
                                 f"https://example.org/{name}")


@requires_cabal
class ConfigDependencyTests(unittest.TestCase):
    def test_config_edit_recompiles_a_cached_context_consumer(self):
        # Compile a small Hakyll consumer of the actual Config module. It
        # bypasses build-freshness.sh, proving loadConfig itself tracks YAML.
        with tempfile.TemporaryDirectory(prefix="config-dependency-") as tmp:
            root = Path(tmp)
            write_site_config(root)
            (root / "page.txt").write_text("$site-title$")
            driver = root / "Probe.hs"
            driver.write_text('''{-# LANGUAGE OverloadedStrings #-}
import Config
import Hakyll
main :: IO ()
main = hakyll $ do
    configRules
    match "page.txt" $ do
        route $ setExtension "html"
        compile $ getResourceBody >>= applyAsTemplate (configField "site-title" siteTitle)
''')
            binary = root / "probe"
            compiled = subprocess.run(
                ["cabal", "exec", "--", "ghc", "-v0", f"-i{engine_path('build')}", "-outputdir", str(root),
                 str(driver), "-o", str(binary)], cwd=ENGINE, env=script_env(),
                capture_output=True, text=True, timeout=120)
            self.assertEqual(compiled.returncode, 0, compiled.stderr)
            for title in ("Example Site", "Changed Site"):
                if title == "Changed Site":
                    previous = (root / "site.yaml").stat().st_mtime
                    write_site_config(root, title=title)
                    os.utime(root / "site.yaml", (previous + 2, previous + 2))
                done = subprocess.run([str(binary), "build"], cwd=root, env=script_env(),
                                      capture_output=True, text=True, timeout=60)
                self.assertEqual(done.returncode, 0, done.stderr)
                self.assertEqual((root / "_site/page.html").read_text(), title)


if __name__ == "__main__":
    unittest.main()
