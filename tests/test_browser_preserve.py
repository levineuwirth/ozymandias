"""Phase B's preservation harness, in a browser: a change to one CSS rule is
what it reports, and the same site twice is nothing.

tools/browser/preserve.py over a scratch site of one page and one stylesheet
(its tests/routes.yaml names the page): recorded, recorded again, then with
one rule changed. The real sites' records are the harness's work, not this
test's; this is that it sees a computed style and the pixels it makes."""

import os
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

from tests._browser import requires_browser
from tests._helpers import engine_path, script_env, load_tool

TOOL = engine_path("tools/browser/preserve.py")
PAGE = """<!doctype html><html lang="en"><head><meta charset="utf-8"><title>One</title>
<link rel="stylesheet" href="/style.css"></head>
<body><p>A paragraph with <a href="/elsewhere" class="out">a link</a>.</p></body></html>
"""
STYLE = "p { color: rgb(20, 20, 20); }\na.out::after { content: '>'; margin-left: 2px; }\n"


@requires_browser
class Preserve(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory(prefix="preserve-browser-")
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.site = self.root / "site"
        (self.site / "_site").mkdir(parents=True)
        (self.site / "tests").mkdir()
        (self.site / "site.yaml").write_text("{}\n")
        (self.site / "tests/routes.yaml").write_text("pages:\n  home: /\n")
        (self.site / "_site/index.html").write_text(PAGE)
        (self.site / "_site/style.css").write_text(STYLE)

    def run_tool(self, *args) -> subprocess.CompletedProcess:
        return subprocess.run([sys.executable, str(TOOL), *args], cwd=self.site,
                              env=script_env(SITE_ROOT=str(self.site), BROWSER_JOBS="1"),
                              capture_output=True, text=True, timeout=600)

    def snapshot(self, name: str, *options: str) -> Path:
        done = self.run_tool("snapshot", str(self.root / name), *options)
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        return self.root / name

    def test_default_is_rendered_css_and_has_its_own_record_at_either_endpoint(self):
        preserve = load_tool("browser/preserve.py", "preserve_browser_test")
        built = self.site / "_site"
        for script in ("utils.js", "theme.js"):
            (built / script).write_text(engine_path("static/js/" + script).read_text())
        (built / "index.html").write_text(PAGE.replace('</head>',
            '<link rel="stylesheet" href="/theme.css">'
            '<link rel="stylesheet" href="/site.css">'
            '<script src="/utils.js"></script><script src="/theme.js"></script></head>'))
        (built / "style.css").write_text(STYLE + """
            :root { --text-size-default: 23px; --text-size: var(--text-size-default); }
            html { font-size: var(--text-size); }
        """)
        (built / "theme.css").write_text("""
            :root { --text-size-default: 21px; }
            /* Old default: --text-size-default: 17px */
        """)
        # Poison the source files too: snapshot must use the served build,
        # not whatever CSS happens to be in the working tree today.
        source = self.site / "static/css"
        source.mkdir(parents=True)
        (source / "theme.css").write_text((built / "theme.css").read_text())
        (source / "site.css").write_text(":root { --text-size-default: 25px; }")
        for size in (25, 17, 29):
            with self.subTest(default=size):
                (built / "site.css").write_text(f":root {{ --text-size-default: {size}px; }}")
                snapshot = self.snapshot(f"size-{size}", "--shots=0")
                meta = json.loads((snapshot / "meta.json").read_text())
                self.assertEqual(meta["sizes"], [17, "default", 29])
                for theme in ("light", "dark", "cappuccino"):
                    for label, expected in ((17, 17), ("default", size), (29, 29)):
                        path = snapshot / "styles/home" / f"{theme}-{label}.json.gz"
                        styles = preserve.resolved(path, meta["properties"])
                        self.assertEqual(styles["html"][0]["font-size"], f"{expected}px")
                done = self.run_tool("compare", str(snapshot), str(snapshot))
                self.assertEqual(done.returncode, 0, done.stdout + done.stderr)

    def test_one_changed_rule_is_what_it_reports(self):
        first, again = self.snapshot("first"), self.snapshot("again")
        same = self.run_tool("compare", str(first), str(again))
        self.assertEqual(same.returncode, 0, same.stdout)
        (self.site / "_site/style.css").write_text(STYLE.replace("20, 20, 20", "200, 20, 20")
                                                    .replace("margin-left: 2px", "margin-left: 3px"))
        changed = self.snapshot("changed")
        done = self.run_tool("compare", str(first), str(changed), "--report", str(self.root / "report"))
        self.assertEqual(done.returncode, 1, done.stdout)
        out = done.stdout
        self.assertIn("html>body:1>p:1 color: 'rgb(20, 20, 20)' -> 'rgb(200, 20, 20)'", out)
        self.assertIn("html>body:1>p:1>a:1::after margin-left: '2px' -> '3px'", out)
        self.assertIn("shots/home/light-320.png: pixels differ", out)
        self.assertIn("shots/home/dark-print.png: pixels differ", out)
        # Nothing but those: the link's own color is inherited from the
        # paragraph, and changes with it.
        self.assertNotIn("display", out)
        self.assertTrue(any((self.root / "report/shots/home").iterdir()))

    def test_faint_phone_and_print_colours_are_caught_by_styles(self):
        sheet = self.site / "_site/style.css"
        sheet.write_text(STYLE + "body { background: rgb(100, 100, 100); }\n")
        first = self.snapshot("first")
        sheet.write_text(sheet.read_text() +
                         "@media (max-width: 400px), print { body { background: rgb(101, 100, 100); } }\n")
        changed = self.snapshot("changed")
        done = self.run_tool("compare", str(first), str(changed))
        self.assertEqual(done.returncode, 1, done.stdout + done.stderr)
        for theme in ("light", "dark", "cappuccino"):
            for width in (320, "print"):
                self.assertIn(f"home/{theme}-view-{width}.json.gz: html>body:1 background-color: "
                              "'rgb(100, 100, 100)' -> 'rgb(101, 100, 100)'", done.stdout)
        # The tolerance still ignores faint pixels. The new style records
        # must be the reason the default comparison refuses this change.
        self.assertNotIn("shots/", done.stdout)
        self.assertNotIn("view-768", done.stdout)
        self.assertNotIn("view-1280", done.stdout)
        self.assertNotIn("view-1600", done.stdout)
        self.assertIn("preserve: 6 difference(s)", done.stdout)


if __name__ == "__main__":
    unittest.main()
