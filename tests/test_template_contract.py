"""The override contract rendered (engine/contract.yaml): the
engine's own templates keep it, and `site render-template` renders a
template as the build would, with the fixture context and nothing else,
so the hook check sees what an override does.
"""
import itertools
import json
import re
import subprocess
import tempfile
import unittest
from pathlib import Path

import yaml

from tests._helpers import engine_path, load_tool, requires_cabal, site_binary

review = load_tool("override-review.py", "override_review")


@requires_cabal
class TemplateContractTests(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.binary = str(site_binary())
        self.terms = review.contract()

    def override(self, path: str, text: str) -> None:
        (self.root / path).parent.mkdir(parents=True, exist_ok=True)
        (self.root / path).write_text(text)
        manifest = self.root / "overrides.yaml"
        entries = manifest.read_text() if manifest.exists() else ""
        manifest.write_text(entries + f"- site: {path}\n  engine: {path}\n  reviewed: {'0' * 40}\n")

    def test_the_engine_s_own_templates_keep_it(self):
        for template in self.terms["templates"]:
            with self.subTest(template=template):
                self.assertEqual(review.template_problems(self.root, template, self.terms, self.binary), [])

    def test_an_override_that_drops_a_hook_is_reported(self):
        self.override("templates/partials/footer.html", "<footer><p>$site-host$</p></footer>\n")
        self.override("templates/partials/ornament.html", '<hr class="footer-ornament">\n')
        self.assertEqual(review.hook_problems(self.root, self.binary),
                         ["templates/partials/footer.html: lacks footer[data-pagefind-ignore] at its top level"])

    def test_a_footer_inside_another_element_is_reported(self):
        # The page shell's styles reach only its direct child footer.
        self.override("templates/partials/footer.html",
                      '<div class="foot"><footer data-pagefind-ignore>$site-host$</footer></div>\n')
        self.assertEqual(review.hook_problems(self.root, self.binary), [
            "templates/partials/footer.html: lacks footer at its top level,"
            " footer[data-pagefind-ignore] at its top level"])

    def test_an_override_using_a_field_the_contract_does_not_give_is_reported(self):
        # Outside $if$ the render fails, as the build would; inside, it
        # would render as if false, so the scan names it.
        self.override("templates/partials/footer.html",
                      "<footer data-pagefind-ignore>$if(build-info)$<b>$build-info$</b>$endif$</footer>\n")
        self.override("templates/404.html", "<p>$site-motto$</p>\n")
        problems = review.hook_problems(self.root, self.binary)
        self.assertEqual(problems[0], "templates/partials/footer.html: uses build-info, which the contract"
                                      " does not give it")
        self.assertEqual(problems[1], "templates/404.html: uses site-motto, which the contract does not give it")
        self.assertTrue(problems[2].startswith("templates/404.html: does not render with its fixture context"))
        self.assertIn("site-motto", problems[2])
        self.assertEqual(len(problems), 3)

    def test_the_site_s_partials_an_override_includes_are_scanned_too(self):
        self.override("templates/partials/footer.html",
                      '<footer data-pagefind-ignore>$partial("templates/partials/extra.html")$'
                      '$partial("templates/partials/ornament.html")$</footer>\n')
        (self.root / "templates/partials/extra.html").write_text(
            '$if(build-info)$<b>$build-info$</b>$endif$$partial("templates/partials/more.html")$')
        (self.root / "templates/partials/more.html").write_text("$if(site-motto)$x$endif$")
        self.assertEqual(review.hook_problems(self.root, self.binary), [
            "templates/partials/footer.html: uses build-info (in templates/partials/extra.html),"
            " which the contract does not give it",
            "templates/partials/footer.html: uses site-motto (in templates/partials/more.html),"
            " which the contract does not give it"])

    def test_the_identity_group_is_the_engine_s_identity_fields(self):
        # `site identity-fields` names what Config.identityContext gives; a
        # field the contract kept after the engine dropped it would let an
        # override's $if$ on it pass here and read false on the site.
        printed = subprocess.run([self.binary, "identity-fields"], capture_output=True, text=True, check=True)
        engine = {line.split()[0]: set(line.split()[1:]) for line in printed.stdout.splitlines()}
        identity = self.terms["contexts"]["identity"]
        contract = {name: {key for item in value for key in item} if isinstance(value, list) else set()
                    for name, value in identity.items()}
        self.assertEqual(contract, engine)

    def test_the_switches_group_names_registered_switches_on(self):
        # A misspelt one would give a field no site has.
        registry = yaml.safe_load(engine_path("switches.yaml").read_text(encoding="utf-8"))
        switches = self.terms["contexts"]["switches"]
        self.assertTrue(switches)
        self.assertEqual({name.removeprefix("switch-") for name in switches} - set(registry), set())
        self.assertTrue(all(name.startswith("switch-") and value is True for name, value in switches.items()))

    def test_the_engine_footer_separates_what_each_switch_adds(self):
        # Its right part holds the build page's link, the build's time and
        # the signature link, each behind its switch: whichever of them a
        # site has, the signature link is set off from what comes before
        # it, and nothing else is.
        build = '<a href="/build/" class="footer-build-link"'
        stamp = '<span class="footer-build-time" data-build-time></span>'
        sig = '<a href="/essays/example/.sig" class="footer-sig-link"'
        base = self.terms["contexts"]["identity"] | self.terms["contexts"]["page"]
        for on in itertools.product((False, True), repeat=3):
            switches = dict(zip(("switch-build-page", "switch-footer-build-time", "switch-signature-link"), on))
            with self.subTest(**{name.removeprefix("switch-"): value for name, value in switches.items()}):
                done = review.render(self.root, "templates/partials/footer.html", base | switches, self.binary)
                self.assertEqual(done.returncode, 0, done.stderr)
                right = re.search(r'<div class="footer-right">(.*?)</div>', done.stdout, re.S).group(1)
                for (name, value), mark in zip(switches.items(), (build, stamp, sig)):
                    self.assertEqual(mark in right, value, f"{name} {value}: {right}")
                if on[2]:
                    before = right[:right.index(sig)]
                    self.assertEqual(before.endswith(" · "), on[0] or on[1], right)
                self.assertNotIn("</a><a", right)
                self.assertNotIn("</span><a", right)

    def test_an_override_of_a_template_the_contract_does_not_name_is_reported(self):
        self.override("templates/partials/toc.html", "<nav></nav>\n")
        self.assertEqual(review.hook_problems(self.root, self.binary),
                         ["templates/partials/toc.html: not an override point (engine/contract.yaml)"])

    def test_a_page_and_its_partials_overridden_together_keep_it(self):
        # The landing with the engine's own controls and a card of the site's
        # that names the site and its page: the controls ask, in $if$, for
        # is-series and photography-view-map, which the landing does not
        # give, and the card is given in the landing's loop what it is
        # alone (a photograph's context has the site's fields), as the build
        # gives it.
        engine = Path(review.ENGINE)
        for path in ("templates/photography-index.html", "templates/partials/photography-controls.html"):
            self.override(path, (engine / path).read_text(encoding="utf-8"))
        card = (engine / "templates/partials/photo-card.html").read_text(encoding="utf-8")
        self.override("templates/partials/photo-card.html",
                      card.replace('<span class="photo-card-title">$title$</span>',
                                   '<span class="photo-card-title">$title$ · $site-title$ · $url$</span>'))
        self.assertEqual(review.hook_problems(self.root, self.binary), [])

    def test_an_overridden_partial_renders_in_each_page_that_includes_it(self):
        # The controls' own context has the map's and a series' flags; the
        # landing gives neither, so a branch only it takes is rendered there.
        self.override("templates/partials/photography-controls.html",
                      '<nav><a class="mode-btn" data-mode="grid" href="/photography/">Grid</a>'
                      '$if(photography-view-map)$<b>Map</b>$else$<i>$series-frame-count$</i>$endif$</nav>\n')
        problems = review.hook_problems(self.root, self.binary)
        self.assertEqual(len(problems), 1, problems)
        self.assertTrue(problems[0].startswith(
            "templates/photography-index.html, the engine's, with the site's"
            " templates/partials/photography-controls.html: does not render"), problems)
        self.assertIn("series-frame-count", problems[0])

    def test_an_overridden_partial_keeps_its_hooks_in_each_page_that_includes_it(self):
        # Views on a series and the map, and on the landing only the
        # slideshow's button: rendered alone, with every flag, it has them.
        self.override("templates/partials/photography-controls.html",
                      '<nav>$if(is-series)$<a class="mode-btn" data-mode="grid" href="/photography/">Grid</a>'
                      '$else$$if(photography-view-map)$<a class="mode-btn" data-mode="grid" href="/photography/">'
                      'Grid</a>$else$<button class="mode-btn" type="button" data-mode="slideshow">Slideshow'
                      '</button>$endif$$endif$</nav>\n')
        self.assertEqual(review.hook_problems(self.root, self.binary), [
            "templates/photography-index.html, the engine's, with the site's"
            " templates/partials/photography-controls.html: lacks"
            " .mode-btn[data-mode]:not([data-mode=slideshow]) (templates/partials/photography-controls.html's)"])

    def test_the_engine_s_own_partials_keep_their_hooks_in_each_page(self):
        # Each override-point partial, the engine's own copy as the site's:
        # every page that includes it keeps its hooks.
        engine = Path(review.ENGINE)
        for path in self.terms["templates"]:
            if path.startswith("templates/partials/"):
                self.override(path, (engine / path).read_text(encoding="utf-8"))
        self.assertEqual(review.hook_problems(self.root, self.binary), [])

    def test_the_slideshow_s_button_is_not_a_view(self):
        self.override("templates/partials/photography-controls.html",
                      '<div>$if(is-series)$<button class="mode-btn" type="button"'
                      ' data-mode="slideshow">Slideshow</button>$endif$</div>\n')
        view = ".mode-btn[data-mode]:not([data-mode=slideshow])"
        controls = "templates/partials/photography-controls.html"
        self.assertEqual(review.hook_problems(self.root, self.binary),
                         [f"{controls}: lacks {view}"] +
                         [f"templates/{page}.html, the engine's, with the site's {controls}: lacks {view} ({controls}'s)"
                          for page in ("photography-series", "photography-index", "photography-map")])

    def test_a_photograph_s_page_stays_in_keyword_search(self):
        engine = Path(review.ENGINE) / "templates/photography.html"
        text = engine.read_text(encoding="utf-8")
        self.assertIn("data-pagefind-body", text)
        self.override("templates/photography.html", text.replace("data-pagefind-body", "data-kept"))
        self.assertEqual(review.hook_problems(self.root, self.binary),
                         ["templates/photography.html: lacks [data-pagefind-body]"])

    def test_plain_is_given_where_the_engine_gives_it(self):
        # A photograph's context has plain, in a list of them too; a link's
        # (photo-links) has only its name and URL, and the build stops on it.
        self.override("templates/partials/photo-card.html",
                      '<li class="photo-card"><a class="photo-card-link" href="$canonical-url$">'
                      '<img class="photo-card-img" alt="$plain(title)$"></a></li>\n')
        self.override("templates/photography.html",
                      '<main id="markdownBody"><img data-lightbox="true" alt="$plain(title)$">'
                      '$if(photo-links)$$for(photo-links)$<a title="$plain(link-name)$">$link-name$</a>'
                      '$endfor$$endif$</main>\n')
        problems = review.hook_problems(self.root, self.binary)
        self.assertEqual(len(problems), 1, problems)
        self.assertTrue(problems[0].startswith("templates/photography.html: does not render"), problems)
        self.assertIn("plain", problems[0])

    def test_render_template_gives_the_context_s_fields_and_no_others(self):
        (self.root / "templates").mkdir()
        (self.root / "templates/t.html").write_text(
            "[$a$]$if(on)$[on]$endif$$if(off)$[off]$endif$$if(absent)$[absent]$endif$"
            '$for(items)$<$name$>$sep$,$endfor$$partial("templates/p.html")$')
        (self.root / "templates/p.html").write_text("{$a$}")
        context = {"a": "x &amp; y", "on": True, "off": False, "items": [{"name": "i"}, {"name": "j"}]}
        done = review.render(self.root, "templates/t.html", context, self.binary)
        self.assertEqual((done.returncode, done.stdout), (0, "[x &amp; y][on]<i>,<j>{x &amp; y}"), done.stderr)

        work = self.root / "work"
        work.mkdir()
        (self.root / "context.json").write_text(json.dumps({"on": True}))
        failed = subprocess.run([self.binary, "render-template", "templates/t.html", "context.json", str(work)],
                                cwd=self.root, capture_output=True, text=True)
        self.assertEqual(failed.returncode, 1)
        self.assertIn("Missing field 'a'", failed.stderr)

    def test_plain_is_the_engine_s_at_the_top_and_in_items_that_say_so(self):
        # Every photography template writes $plain(title)$ into attributes; a
        # loop's items have it as the engine's lists of pages do ($plain),
        # and not as its lists of links do.
        (self.root / "templates").mkdir()
        (self.root / "templates/t.html").write_text(
            '<i title="$plain(t)$"></i>$for(items)$<b title="$plain(t)$"></b>$endfor$')
        context = {"t": "<em>A</em> \"q\"", "items": [{"t": "B &amp; c", "$plain": True}]}
        done = review.render(self.root, "templates/t.html", context, self.binary)
        self.assertEqual((done.returncode, done.stdout),
                         (0, '<i title="A &quot;q&quot;"></i><b title="B &amp;amp; c"></b>'), done.stderr)
        del context["items"][0]["$plain"]
        done = review.render(self.root, "templates/t.html", context, self.binary)
        self.assertEqual(done.returncode, 1)
        self.assertIn("plain", done.stderr)
        self.assertEqual(review.template_fields('$plain(alt)$$if(x)$$endif$'), {"alt", "x"})


if __name__ == "__main__":
    unittest.main()
