"""The override contract rendered (engine/templates/contract.yaml): the
engine's own templates keep it, and `site render-template` renders a
template as the build would, with the fixture context and nothing else,
so the hook check sees what an override does.
"""
import json
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import load_tool, requires_cabal, site_binary

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
                         ["templates/partials/footer.html: lacks footer[data-pagefind-ignore]"])

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

    def test_an_override_of_a_template_the_contract_does_not_name_is_reported(self):
        self.override("templates/partials/toc.html", "<nav></nav>\n")
        self.assertEqual(review.hook_problems(self.root, self.binary),
                         ["templates/partials/toc.html: not an override point (engine/templates/contract.yaml)"])

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


if __name__ == "__main__":
    unittest.main()
