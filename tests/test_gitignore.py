#!/usr/bin/env python3
"""Every name the site refuses to publish is also ignored by git.

`make build` commits content/ wholesale and `make deploy` pushes it, so a
private file the build would never serve (build/Site.hs `neverPublish`,
tools/check-site.py PRIVATE_FILE_GLOBS) still reached the public mirror if
.gitignore missed it (audit D03). Both lists are read from their sources,
so a rule added to either without a matching ignore rule fails here.

Run with: ``python3 -m unittest tests.test_gitignore``.
"""

from __future__ import annotations

import re
import shutil
import subprocess
import unittest
from tests._helpers import SITE_ROOT, engine_path, load_tool, requires_cabal, site_binary
SAMPLE_DIR = "content/essays/sample"


def _check_site():
    return load_tool("check-site.py")


def never_publish() -> dict[str, list[str]]:
    """The suffixes, prefixes, infixes and exact names in build/Site.hs
    `neverPublish`."""
    src = engine_path("build/Site.hs").read_text(encoding="utf-8")
    lists = {}
    for name in ("suffixes", "prefixes", "infixes", "exactNames"):
        m = re.search(rf"^\s*{name}\s*=\s*(.*?)\]", src, re.MULTILINE | re.DOTALL)
        assert m, f"neverPublish {name} not found in build/Site.hs"
        body = re.sub(r"--[^\n]*", "", m.group(1))
        lists[name] = re.findall(r'"([^"]*)"', body)
    return lists


def build_samples() -> list[tuple[str, str]]:
    """(rule, sample file name) for every `neverPublish` rule."""
    np = never_publish()
    out = [(f"suffix {s}", "sample.md~" if s == "~" else f"sample{s}") for s in np["suffixes"]]
    out += [(f"prefix {p}", f"{p}sample") for p in np["prefixes"]]
    out += [(f"infix {i}", f"sample{i}sample") for i in np["infixes"]]
    out += [(f"name {n}", n) for n in np["exactNames"]]
    return out


def gate_samples() -> list[tuple[str, str]]:
    """(rule, sample file name) for every check-site glob."""
    return [(f"check-site {g}", g.replace("*", "sample")) for g in _check_site().PRIVATE_FILE_GLOBS]


def samples() -> list[tuple[str, str]]:
    """(rule, sample file name) for every rule."""
    return build_samples() + gate_samples()


@unittest.skipUnless(shutil.which("git"), "git not on PATH")
class PrivateNamesAreIgnored(unittest.TestCase):
    def test_lists_were_read(self):
        np = never_publish()
        self.assertIn(".local.md", np["suffixes"])
        self.assertIn("credentials", np["prefixes"])
        self.assertIn(".draft.", np["infixes"])
        self.assertIn(".netrc", np["exactNames"])

    def test_every_private_name_is_ignored(self):
        names = samples()
        paths = [f"{SAMPLE_DIR}/{name}" for _, name in names]
        # --no-index: judge the rules alone, as the content/ snapshot's
        # `git add` would, whether or not a file exists or is tracked.
        done = subprocess.run(
            ["git", "check-ignore", "--no-index", "--stdin"],
            cwd=SITE_ROOT, input="\n".join(paths) + "\n", capture_output=True, text=True,
        )
        ignored = set(done.stdout.split())
        missing = [f"{rule} ({name})" for (rule, name), path in zip(names, paths)
                   if path not in ignored]
        self.assertEqual(missing, [], "add a .gitignore rule for each")

    def test_the_artifact_gate_knows_every_name_the_build_refuses(self):
        # check-site is the last gate before publishing; a name the build's
        # boundary refuses but check-site does not know would ship from any
        # path that bypasses that boundary. The lists had drifted: nine
        # names (id_rsa*, .netrc, *.swo, ...) were missing from check-site.
        import fnmatch
        globs = _check_site().PRIVATE_FILE_GLOBS
        missing = [f"{rule} ({name})" for rule, name in build_samples()
                   if not any(fnmatch.fnmatch(name, g) for g in globs)]
        self.assertEqual(missing, [], "add each to PRIVATE_FILE_GLOBS in tools/check-site.py")

    @requires_cabal
    def test_the_build_refuses_every_name_the_artifact_gate_does(self):
        # The other direction, asked of the build itself (`site
        # private-paths`, Site.refusedPath): checklist.md, *.local.html and
        # *.draft.* were known only to check-site until 2026-10-04, so the
        # build rendered such a file and the gate then failed the deploy.
        names = [name for _, name in gate_samples()]
        paths = ([f"{SAMPLE_DIR}/{n}" for n in names]
                 + [f"{SAMPLE_DIR}/{n}/index.md" for n in names])
        done = subprocess.run([str(site_binary()), "private-paths"], cwd=SITE_ROOT,
                              input="\n".join(paths) + "\n", capture_output=True,
                              text=True, check=True)
        self.assertEqual(sorted(set(paths) - set(done.stdout.splitlines())), [],
                         "add each to neverPublish in build/Site.hs")

    def test_ordinary_content_is_not_ignored(self):
        for name in ("index.md", "figure.svg", "data.csv", "notes.md"):
            done = subprocess.run(
                ["git", "check-ignore", "--no-index", "-q", f"{SAMPLE_DIR}/{name}"], cwd=SITE_ROOT
            )
            self.assertEqual(done.returncode, 1, f"{name} would be ignored")


if __name__ == "__main__":
    unittest.main()
