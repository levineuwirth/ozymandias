"""The languages a work's code is written in (site.yaml's `languages`,
projects.yml's and an essay's `languages:`; Vita.languagesField,
Vita.renderProjects), in whole builds of a small scratch site:

  · a project's languages stand under its title on the projects page, by
    the registry's names, in its order;
  · an essay shows its own languages, else its project's (the project whose
    `essay` is the page, flat or a directory's index), else none;
  · a key the registry does not have fails the build, naming it.

    python -m unittest tests.test_languages -v
"""

from __future__ import annotations

import re
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import engine_path, requires_cabal, script_env, site_binary, write_site_config

PROJECTS = """projects:
  - id: kernel
    name: A Kernel
    essay: /essays/kernel/
    languages: [c, asm]
    start: May 2025
    description: A small kernel.
  - id: tool
    name: A Tool
    essay: /essays/tool.html
    languages: [rust]
    start: June 2025
    description: A tool.
  - id: plain
    name: Plain
    start: July 2025
    description: No languages.
"""


def page(path: Path, front: str, body: str = "Body.") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(f"---\n{front}\n---\n\n{body}\n", encoding="utf-8")


def scratch(root: Path, projects: str = PROJECTS) -> None:
    (root / "engine").symlink_to(engine_path(""))
    page(root / "content/index.md", "title: Home\nhome: true", "Home.")
    page(root / "content/cv/projects.md", "title: Projects", "Projects.")
    (root / "templates").mkdir()
    (root / "templates/projects.html").write_text(
        '<main id="markdownBody"><h1 class="page-title">$title$</h1>$vita-projects-html$</main>\n',
        encoding="utf-8")
    (root / "yaml-source/data").mkdir(parents=True)
    (root / "yaml-source/data/projects.yml").write_text(projects, encoding="utf-8")
    page(root / "content/essays/kernel/index.md", "title: The Kernel\ndate: 2026-01-01")
    page(root / "content/essays/tool.md", "title: The Tool\ndate: 2026-01-02")
    page(root / "content/essays/own.md", "title: Its Own\ndate: 2026-01-03\nlanguages: [python, rust]")
    page(root / "content/essays/none.md", "title: None\ndate: 2026-01-04")
    write_site_config(
        root,
        languages={"asm": "x86-64 assembly", "c": "C", "python": "Python", "rust": "Rust"},
        switches={"vita": True},
        standalone=[{"source": "content/cv/projects.md", "route": "cv/projects/index.html",
                     "template": "projects.html", "context": "projects", "scores": False,
                     "dingbat": None, "flags": []}])


def build(root: Path) -> subprocess.CompletedProcess:
    return subprocess.run([str(site_binary()), "rebuild"], cwd=root,
                          env=script_env(SITE_ENV="production", SITE_THREADS="1"),
                          capture_output=True, text=True, timeout=240)


def languages(html: str) -> list[str]:
    """The essay's languages row, one element (accessibility checkers count
    each element outside a landmark), split into its names."""
    row = re.search(r'<div class="meta-row meta-languages">\s*(.*?)\s*</div>', html, re.S)
    return [name.strip() for name in row.group(1).split("·")] if row else []


@requires_cabal
class Languages(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        tmp = tempfile.TemporaryDirectory(prefix="languages-")
        cls.addClassCleanup(tmp.cleanup)
        cls.root = Path(tmp.name)
        scratch(cls.root)
        cls.done = build(cls.root)

    def read(self, rel: str) -> str:
        return (self.root / "_site" / rel).read_text(encoding="utf-8")

    def test_the_build_succeeds(self):
        self.assertEqual(self.done.returncode, 0, self.done.stdout[-3000:] + self.done.stderr)

    def test_a_project_s_languages_stand_under_its_title(self):
        projects = self.read("cv/projects/index.html")
        self.assertEqual(re.findall(r'class="vita-languages">([^<]*)<', projects),
                         ["C · x86-64 assembly", "Rust"])
        plain = projects[projects.index("Plain"):]
        self.assertNotIn("vita-languages", plain[:plain.index("</li>")])

    def test_an_essay_shows_its_own_or_its_project_s(self):
        self.assertEqual(languages(self.read("essays/kernel/index.html")), ["C", "x86-64 assembly"])
        self.assertEqual(languages(self.read("essays/tool.html")), ["Rust"])
        self.assertEqual(languages(self.read("essays/own.html")), ["Python", "Rust"])
        none = self.read("essays/none.html")
        self.assertEqual(languages(none), [])
        self.assertNotIn("meta-languages", none)


@requires_cabal
class UnknownLanguage(unittest.TestCase):
    def test_a_key_not_in_the_registry_fails_the_build(self):
        with tempfile.TemporaryDirectory(prefix="languages-unknown-") as tmp:
            root = Path(tmp)
            scratch(root, PROJECTS.replace("[rust]", "[rust, cobol]"))
            done = build(root)
            self.assertNotEqual(done.returncode, 0)
            self.assertIn('"cobol" is not in site.yaml\'s languages', done.stdout + done.stderr)


if __name__ == "__main__":
    unittest.main()
