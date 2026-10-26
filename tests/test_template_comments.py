"""Template comments stay in the templates (Utils.stripHtmlComments).

They are notes for whoever edits a template, and a comment in a partial
rendered inside a loop shipped once per item: about 750 copies of
photo-card's on the photography pages, 1.3 MB across the site. The build
drops them as each template compiles; `site strip-template-comments` runs
the same function on stdin.
"""

from __future__ import annotations

import subprocess
import unittest
from pathlib import Path

from tests._helpers import SITE_ROOT, engine_path, requires_cabal, script_env, site_binary


@requires_cabal
class TemplateCommentTests(unittest.TestCase):
    def strip(self, text: str) -> str:
        done = subprocess.run([str(site_binary()), "strip-template-comments"], input=text,
                              env=script_env(), capture_output=True, text=True, timeout=60)
        self.assertEqual(done.returncode, 0, done.stderr)
        return done.stdout

    def test_a_comment_on_lines_of_its_own_goes_with_them(self) -> None:
        self.assertEqual(self.strip("<ul>\n    <!-- a note\n         on two lines -->\n    <li>x</li>\n</ul>\n"),
                         "<ul>\n    <li>x</li>\n</ul>\n")
        self.assertEqual(self.strip("<!-- first line -->\n<p>x</p>\n"), "<p>x</p>\n")

    def test_a_comment_beside_markup_goes_alone(self) -> None:
        self.assertEqual(self.strip("<li>x</li> <!-- why --> <li>y</li>\n"),
                         "<li>x</li>  <li>y</li>\n")
        self.assertEqual(self.strip("<p>$title$<!-- t --></p>\n"), "<p>$title$</p>\n")

    def test_an_unterminated_comment_is_left_alone(self) -> None:
        self.assertEqual(self.strip("<p>x</p>\n<!-- never closed\n"), "<p>x</p>\n<!-- never closed\n")

    def test_no_template_keeps_a_comment(self) -> None:
        # Every comment a template has today is closed and is removed.
        # The engine's templates and the site's.
        for root in dict.fromkeys((engine_path("templates"), SITE_ROOT / "templates")):
            for path in sorted(root.rglob("*.html")):
                with self.subTest(template=path.relative_to(root.parent).as_posix()):
                    self.assertNotIn("<!--", self.strip(path.read_text(encoding="utf-8")))


if __name__ == "__main__":
    unittest.main()
