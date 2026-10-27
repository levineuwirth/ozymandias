"""tools/override-review.py without the generator: the contract's own
soundness (engine/templates/contract.yaml), hook selectors and field scans,
and the review: what changed under an override, and recording it.
"""
import io
import subprocess
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from unittest import mock

from tests._helpers import load_tool

review = load_tool("override-review.py", "override_review")


class ContractTests(unittest.TestCase):
    def test_the_contract_is_sound(self):
        self.assertEqual(review.contract_problems(review.contract()), [])

    def test_a_hook_no_file_it_names_binds_is_reported(self):
        terms = review.contract()
        terms["templates"]["templates/partials/footer.html"]["hooks"].append(
            {"selector": ".no-such-hook", "by": ["static/js/nav.js"], "why": "none"})
        self.assertEqual(review.contract_problems(terms),
                         ["templates/partials/footer.html: .no-such-hook: static/js/nav.js does not name it"])

    def test_a_hook_s_tag_or_part_of_a_longer_name_does_not_count_as_naming_it(self):
        terms = review.contract()
        optional = terms["templates"]["templates/partials/footer.html"]["optional"]
        optional.append({"selector": "a.footer-sig-link", "by": ["static/js/nav.js"]})
        optional.append({"selector": ".footer-orn", "by": ["static/css/components.css"]})
        self.assertEqual(review.contract_problems(terms), [
            "templates/partials/footer.html: a.footer-sig-link: static/js/nav.js does not name it",
            "templates/partials/footer.html: .footer-orn: static/css/components.css does not name it",
        ])

    def test_entries_must_name_engine_templates_groups_and_hooks(self):
        terms = review.contract()
        terms["templates"]["templates/no-such.html"] = {"hooks": []}
        terms["templates"]["templates/404.html"]["context"] = ["identity", "no-such-group"]
        del terms["templates"]["templates/partials/ornament.html"]["hooks"]
        self.assertEqual(review.contract_problems(terms), [
            "templates/404.html: no context group no-such-group",
            "templates/partials/ornament.html: an entry has context, hooks (perhaps []) and optional",
            "templates/no-such.html: not an engine template",
        ])


class SelectorTests(unittest.TestCase):
    HTML = '<footer data-pagefind-ignore="all"><button class="a footer-totop" id="up">x</button><hr class="b"/></footer>'

    def test_a_hook_matches_by_tag_class_id_and_attribute(self):
        for selector in ("footer", "footer[data-pagefind-ignore]", "[data-pagefind-ignore=all]",
                         "button.footer-totop", ".a.footer-totop#up", "#up", "hr.b"):
            with self.subTest(selector=selector):
                self.assertTrue(review.matches(self.HTML, selector))

    def test_each_part_must_hold_on_one_element(self):
        for selector in ("div.footer-totop", "footer.footer-totop", "[data-pagefind-ignore=none]",
                         "button#down", ".footer", "[data-build-time]"):
            with self.subTest(selector=selector):
                self.assertFalse(review.matches(self.HTML, selector))

    def test_a_selector_beyond_a_compound_one_is_refused(self):
        for selector in ("", "footer > button", "a:hover", ".a .b"):
            with self.subTest(selector=selector), self.assertRaises(ValueError):
                review.selector_parts(selector)


class FieldTests(unittest.TestCase):
    def test_a_template_s_fields_are_its_uses_loops_and_conditions(self):
        text = ('<!-- $not-this$ --><a href="$url$">$if(title)$$title$$else$x$endif$</a>'
                '$for(site-feeds)$$feed-url$$sep$, $endfor$$-if(flag)-$y$endif$'
                '$partial("templates/partials/x.html")$ costs $$5 $$$size$')
        self.assertEqual(review.template_fields(text), {"url", "title", "site-feeds", "feed-url", "flag", "size"})

    def test_a_context_gives_its_loops_item_fields(self):
        self.assertEqual(review.field_names({"a": "x", "b": [{"c": "y"}], "d": True}), {"a", "b", "c", "d"})


class ReviewTests(unittest.TestCase):
    """The review against a scratch engine in a repository of its own."""

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.engine = Path(tmp.name) / "engine"
        self.root = Path(tmp.name) / "site"
        (self.engine / "templates/partials").mkdir(parents=True)
        (self.root / "templates/partials").mkdir(parents=True)
        self.footer = self.engine / "templates/partials/footer.html"
        self.footer.write_text("<footer>\n<p>one</p>\n</footer>\n")
        (self.engine / "templates/partials/ornament.html").write_text("<hr>\n")
        git = ["git", "-c", "user.email=t@example.org", "-c", "user.name=T", "-c", "commit.gpgsign=false",
               "-c", "core.hooksPath=/dev/null"]
        for args in (["init", "-q"], ["add", "-A"], ["commit", "-qm", "engine"]):
            subprocess.run(git + args, cwd=self.engine, check=True, capture_output=True)
        self.reviewed = review.blob_id(self.footer.read_bytes())
        ornament = review.blob_id(b"<hr>\n")
        self.manifest = (
            "# The site's overrides.\n"
            f"- site: templates/partials/footer.html\n  engine: templates/partials/footer.html\n"
            f"  reviewed: {self.reviewed}\n  note: its own\n"
            f"- site: templates/partials/ornament.html\n  engine: templates/partials/ornament.html\n"
            f"  reviewed: {ornament}\n")
        (self.root / "overrides.yaml").write_text(self.manifest)
        for patch in (mock.patch.object(review, "ENGINE", self.engine),
                      mock.patch.object(review, "SITE_ROOT", self.root)):
            patch.start()
            self.addCleanup(patch.stop)

    def run_tool(self, *args):
        out = io.StringIO()
        with redirect_stdout(out):
            code = review.main(list(args))
        return code, out.getvalue()

    def test_an_engine_change_shows_its_diff_until_accepted(self):
        self.assertEqual(review.unreviewed(self.root), [])
        self.footer.write_text("<footer>\n<p>two</p>\n</footer>\n")
        current = review.blob_id(self.footer.read_bytes())
        [(entry, now)] = review.unreviewed(self.root)
        self.assertEqual((entry["site"], now), ("templates/partials/footer.html", current))
        code, output = self.run_tool()
        self.assertEqual(code, 0)
        self.assertIn(f"engine/templates/partials/footer.html was {self.reviewed}, is {current}", output)
        self.assertIn("-<p>one</p>\n+<p>two</p>", output)

        code, output = self.run_tool("--accept", "templates/partials/footer.html")
        self.assertEqual(code, 0, output)
        self.assertEqual(review.unreviewed(self.root), [])
        # Only that entry's id changed; the comment and the other entry stand.
        self.assertEqual((self.root / "overrides.yaml").read_text(),
                         self.manifest.replace(self.reviewed, current))

    def test_a_gone_engine_file_is_unreviewed_and_cannot_be_accepted(self):
        self.footer.unlink()
        [(entry, now)] = review.unreviewed(self.root)
        self.assertIsNone(now)
        self.assertIn("is gone", review.upstream_diff(entry))
        with redirect_stdout(io.StringIO()), mock.patch("sys.stderr", io.StringIO()) as err:
            self.assertEqual(review.main(["--accept", "templates/partials/footer.html"]), 1)
        self.assertIn("No such file", err.getvalue())
        self.assertEqual((self.root / "overrides.yaml").read_text(), self.manifest)

    def test_only_declared_overrides_are_reviewed(self):
        with mock.patch("sys.stderr", io.StringIO()) as err:
            self.assertEqual(review.main(["--accept", "templates/page.html"]), 2)
            self.assertEqual(review.main(["--accept"]), 2)
        self.assertIn("not in overrides.yaml: templates/page.html", err.getvalue())
        self.assertEqual((self.root / "overrides.yaml").read_text(), self.manifest)

    def test_a_reviewed_blob_the_engine_never_held_is_said_so(self):
        entry = {"site": "x", "engine": "templates/partials/footer.html", "reviewed": "0" * 40}
        self.assertIn("not in the engine's repository", review.upstream_diff(entry))


if __name__ == "__main__":
    unittest.main()
