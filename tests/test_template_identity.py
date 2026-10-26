"""Real templates and the web manifest render alternate identity and
navigation, and track edits."""
import html
import json
import os
import re
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

from tests._helpers import (ENGINE, SITE_ROOT, copy_engine_tree, engine_path, load_tool, requires_cabal,
                            script_env, site_binary, write_site_config)


@requires_cabal
class TemplateIdentityTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        tmp = tempfile.TemporaryDirectory(prefix="template-identity-probe-")
        cls.addClassCleanup(tmp.cleanup)
        build = Path(tmp.name)
        (build / "Probe.hs").write_text('''{-# LANGUAGE OverloadedStrings #-}
import Config
import Contexts (contentKindField)
import Dingbat (dingbatField)
import qualified Patterns as P
import qualified Tags as T
import Hakyll
import Utils (stripHtmlComments)
import Control.Monad (forM_)
main = hakyll $ do
    cfg <- preprocess readSiteConfig
    configRules
    tags <- T.buildAllTags cfg
    match "content/essays/*.md" $ compile getResourceBody
    manifestRules
    match "templates/**" $ compile $ do
        item <- getResourceBody
        tpl <- compileTemplateItem (fmap stripHtmlComments item)
        pure (itemSetBody tpl item)
    let ctx = identityContext <> navigationContext <> constField "home" "true" <> constField "title" "Piece"
              <> constField "url" "/probe" <> constField "canonical-url" "/probe"
              <> bodyField "body"
    forM_ ["partials/head.html", "partials/footer.html", "partials/nav.html",
           "partials/archive-removal-notice.html", "partials/ornament.html",
           "404.html", "score-reader-default.html"] $ \\name ->
        create [fromFilePath name] $ do
            route idRoute
            compile $ makeItem ("" :: String) >>= loadAndApplyTemplate (fromFilePath ("templates/" ++ name)) ctx
    create ["essays/card.html"] $ do
        route idRoute
        compile $ makeItem ("" :: String) >>= loadAndApplyTemplate "templates/partials/item-card.html"
            (contentKindField <> constField "title" "Example" <> constField "canonical-url" "/essays/example/"
             <> constField "date-iso" "2026-01-01" <> constField "date-display" "January 1")
    forM_ ["essays/ornament.html", "essays/profile.html", "essays/plain.html", "memento.html",
           "elsewhere.html"] $ \\name ->
        create [fromFilePath name] $ do
            route idRoute
            compile $ makeItem ("$dingbat$" :: String) >>= applyAsTemplate dingbatField
    -- The backlinks pass compiles each page as version "links".
    create ["link-graph.txt"] $ do
        route idRoute
        compile $ makeItem (show (matches (P.allContent cfg)
                                    (setVersion (Just "links") "content/memento/index.md")))
    create ["see-also.html"] $ do
        route idRoute
        compile $ makeItem ("" :: String) >>= loadAndApplyTemplate "templates/tag-index.html"
            (T.seeAlsoContext [(portalName p, portalTag p) | p <- sitePortals cfg] tags "first"
             <> constField "title" "First")
''')
        cls.binary = build / "probe"
        done = subprocess.run(["cabal", "exec", "--", "ghc", "-v0", f"-i{engine_path('build')}",
                               "-outputdir", str(build), str(build / "Probe.hs"),
                               "-o", str(cls.binary)], cwd=ENGINE, env=script_env(),
                              capture_output=True, text=True, timeout=180)
        if done.returncode:
            raise AssertionError(done.stderr)

    def test_identity_escaping_advertisements_manifest_and_incremental_changes(self):
        with tempfile.TemporaryDirectory(prefix="template-identity-") as tmp:
            root = Path(tmp)
            copy_engine_tree("templates", root / "templates")
            title = 'A & B <Studio> "quoted"'
            manifest = {"name": title, "short-name": 'A "B"', "description": "Art & photos",
                        "theme-color": "#123456", "background-color": "#abcdef"}
            feed = {"path": "news.xml", "title": title, "description": "News", "sections": ["essays"]}
            portals = [{"name": "Zines", "tag": "zines"}, {"name": "Art & <Craft>", "tag": "art"},
                       {"name": "beekeeping", "tag": "bees"}]
            nav = [{"label": "Home", "href": "/"}, {"label": "Q&A", "href": "/q.html?a=1&b=2"}]
            config = dict(title=title, email="a&b@example.org", portals=portals, **{
                "nav-primary": nav,
                "contact-email": "contact&us@example.org", "signing-fingerprint": "ABC DEF",
                "manifest": manifest, "feeds": [feed, dict(feed, path="hidden.xml", advertise=False)]})
            write_site_config(root, **config)
            for changed in (False, True):
                if changed:
                    before = (root / "site.yaml").stat().st_mtime
                    config["title"] = "Changed identity"
                    config["manifest"] = manifest | {"name": "Changed manifest"}
                    config["feeds"] = [dict(feed, **{"advertise-title": "Changed advertisement"})]
                    config["portals"] = portals[:1] + [{"name": "Archery", "tag": "archery"}]
                    write_site_config(root, **config)
                    os.utime(root / "site.yaml", (before + 2, before + 2))
                done = subprocess.run([str(self.binary), "build"], cwd=root, env=script_env(),
                                      capture_output=True, text=True, timeout=60)
                self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
                def read(name):
                    return (root / "_site" / name).read_text()
                head = read("partials/head.html")
                self.assertIn(f'<title>{html.escape(config["title"])}</title>', head)
                self.assertIn('href="/news.xml"', head)
                self.assertNotIn('hidden.xml', head)
                self.assertEqual(head.count('rel="alternate"'), 1)
                if changed:
                    self.assertIn('title="Changed advertisement"', head)
                else:
                    self.assertIn(f'title="{html.escape(title)}"', head)
                self.assertIn("mailto:a&amp;b@example.org", read("404.html"))
                self.assertIn("contact&amp;us@example.org", read("partials/archive-removal-notice.html"))
                self.assertIn("Ed25519 signing subkey ABC DEF", read("partials/footer.html"))
                self.assertIn(f'Piece — Score — {html.escape(config["title"])}', read("score-reader-default.html"))
                parsed = json.loads(read("site.webmanifest"))
                for key, value in config["manifest"].items():
                    self.assertEqual(parsed[key.replace("-", "_")], value)
                rendered = read("partials/nav.html")
                def links(row):
                    return re.findall(r'<a href="([^"]*)">([^<]*)</a>',
                                      re.search(row + r'">(.*?)</div>', rendered, re.S).group(1))
                self.assertEqual(links("nav-primary"), [(html.escape(l["href"]), html.escape(l["label"]))
                                                        for l in config["nav-primary"]])
                self.assertEqual(links("nav-portals"), [(f"/{p['tag']}/", html.escape(p["name"])) for p in
                                                        sorted(config["portals"], key=lambda p: p["name"].lower())])

    def test_taxonomy_labels_remain_text_in_cards_and_see_also(self):
        with tempfile.TemporaryDirectory(prefix="taxonomy-labels-") as tmp:
            root = Path(tmp)
            copy_engine_tree("templates", root / "templates")
            config = write_site_config(root)
            kind = 'Study <Draft> & "notes"'
            portal = "Art & <Craft> 'work'"
            config["sections"][0]["kind"] = kind
            config["portals"] = [{"name": "First", "tag": "first"},
                                 {"name": portal, "tag": "second"}]
            (root / "site.yaml").write_text(json.dumps(config))
            page = root / "content/essays/example.md"
            page.parent.mkdir(parents=True)
            page.write_text("---\ntitle: Example\ntags: [first, second]\n---\nBody.\n")
            done = subprocess.run([str(self.binary), "build"], cwd=root, env=script_env(),
                                  capture_output=True, text=True, timeout=60)
            self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
            card = (root / "_site/essays/card.html").read_text()
            see_also = (root / "_site/see-also.html").read_text()
            kind_text = re.search(r'<span class="item-card-kind">([^<]*)</span>', card)
            portal_text = re.search(r'<a class="see-also-name" href="/second/">([^<]*)</a>', see_also)
            self.assertIsNotNone(kind_text, card)
            self.assertIsNotNone(portal_text, see_also)
            self.assertEqual(html.unescape(kind_text.group(1)), kind)
            self.assertEqual(html.unescape(portal_text.group(1)), portal)

    def test_standalone_pages_take_their_ornament_and_join_the_link_graph(self):
        with tempfile.TemporaryDirectory(prefix="ornaments-") as tmp:
            root = Path(tmp)
            copy_engine_tree("templates", root / "templates")
            config = write_site_config(root)
            config["sections"][0]["dingbat"] = "fleuron"  # essays
            page = {"template": "essay.html", "context": "essay", "scores": False, "flags": []}
            # A standalone page's own ornament is more specific than its
            # section's; without one it takes the section's.
            config["standalone"] = [
                page | {"source": "content/memento/index.md", "route": "memento.html", "dingbat": "memento"},
                page | {"source": "content/profile.md", "route": "essays/profile.html", "dingbat": "memento"},
                page | {"source": "content/plain.md", "route": "essays/plain.html", "dingbat": None}]
            (root / "site.yaml").write_text(json.dumps(config))
            done = subprocess.run([str(self.binary), "build"], cwd=root, env=script_env(),
                                  capture_output=True, text=True, timeout=60)
            self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
            expected = {"essays/ornament.html": "fleuron", "essays/profile.html": "memento",
                        "essays/plain.html": "fleuron", "memento.html": "memento",
                        "elsewhere.html": "asterism"}
            ornaments = {name: (root / "_site" / name).read_text() for name in expected}
            self.assertEqual(ornaments, expected)
            self.assertEqual((root / "_site/link-graph.txt").read_text(), "True")

    def test_the_engine_defaults_carry_the_site_s_identity_and_no_other(self):
        # The engine's own footer, not-found page and ornament, which this
        # site overrides (overrides.yaml), so its build never renders them.
        # None may carry this site's identity (tools/check-identity.py).
        config = json.loads(subprocess.check_output([str(site_binary()), "config"],
                                                    cwd=SITE_ROOT, text=True))
        checker = load_tool("check-identity.py")
        identity = checker.denylist(config, checker.load_policy(SITE_ROOT)["fixed-strings"]
                                    + checker.font_names(SITE_ROOT))
        with tempfile.TemporaryDirectory(prefix="engine-defaults-") as tmp:
            root = Path(tmp)
            shutil.copytree(engine_path("templates"), root / "templates")
            write_site_config(root, title="Atelier & Co", host="atelier.example",
                              url="https://atelier.example", email="studio@atelier.example",
                              **{"code-license": "Apache-2.0"})
            done = subprocess.run([str(self.binary), "build"], cwd=root, env=script_env(),
                                  capture_output=True, text=True, timeout=60)
            self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
            pages = {name: (root / "_site" / name).read_text()
                     for name in ("partials/footer.html", "404.html", "partials/ornament.html")}
            self.assertIn('<a href="/">atelier.example</a>', pages["partials/footer.html"])
            self.assertIn(">Apache-2.0</a>", pages["partials/footer.html"])
            self.assertIn("Atelier &amp; Co", pages["404.html"])
            self.assertIn('href="mailto:studio@atelier.example"', pages["404.html"])
            self.assertIn('class="footer-ornament"', pages["partials/ornament.html"])
            for name, page in pages.items():
                self.assertEqual([t for t in identity if t.lower() in page.lower()], [], name)
                self.assertNotIn("<!--", page, name)
