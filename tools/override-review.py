#!/usr/bin/env python3
"""A site's overrides of engine files, held to the engine's contract.

The site's overrides.yaml records, for each file it puts in place of an
engine file at the same path, the Git blob id of the engine file it was
last reviewed against. engine/contract.yaml declares the
templates a site may override, the fixture context each is rendered with,
and the hooks the engine's scripts and styles need in it.

    override-review.py                    what changed under each unreviewed override
    override-review.py PATH...            the same for these
    override-review.py --accept PATH...   record each engine file's blob id now
    override-review.py --check            render each template override, look for its hooks

Accepting clears only the review. The hook check (which `make test` runs
too, engine/tests/test_overrides.py) and the browser tests must pass on
their own.
"""
from __future__ import annotations

import argparse
import difflib
import hashlib
import json
import re
import subprocess
import sys
import tempfile
from html.parser import HTMLParser
from pathlib import Path

import yaml

sys.path.insert(0, str(Path(__file__).resolve().parent))
from sitelib import ENGINE, SITE_ROOT, site_binary

MANIFEST = "overrides.yaml"
CONTRACT = ENGINE / "contract.yaml"
# A hook: a tag, then classes, an id and attributes (a value optional).
SELECTOR = re.compile(r"([a-z][a-z0-9-]*)?((?:\.[\w-]+|#[\w-]+|\[[\w-]+(?:=[\w-]+)?\])*)")
SIMPLE = re.compile(r"\.([\w-]+)|#([\w-]+)|\[([\w-]+)(?:=([\w-]+))?\]")
# What a template's $...$ uses a field by: name, if(name), for(name).
FIELD = re.compile(r"(?:if|for)\(([\w-]+)\)|([a-z][\w-]*)")
KEYWORDS = {"else", "endif", "endfor", "sep"}
PARTIAL = re.compile(r'partial\("([^"]+)"\)')
# Elements HTML never closes, so nothing after one is inside it.
VOID = {"area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "track", "wbr"}


def blob_id(data: bytes) -> str:
    """The id Git gives a file's contents (git hash-object)."""
    return hashlib.sha1(b"blob %d\0" % len(data) + data).hexdigest()


def manifest(root: Path) -> list[dict]:
    path = root / MANIFEST
    return (yaml.safe_load(path.read_text(encoding="utf-8")) if path.is_file() else None) or []


def contract() -> dict:
    return yaml.safe_load(CONTRACT.read_text(encoding="utf-8"))


# ---------------------------------------------------------------------------
# The review
# ---------------------------------------------------------------------------

def unreviewed(root: Path) -> list[tuple[dict, str | None]]:
    """Each override whose engine file is not the one it was reviewed
    against, with that file's blob id now (None when it is gone)."""
    changed = []
    for entry in manifest(root):
        engine = ENGINE / entry["engine"]
        current = blob_id(engine.read_bytes()) if engine.is_file() else None
        if current != entry["reviewed"]:
            changed.append((entry, current))
    return changed


def upstream_diff(entry: dict) -> str:
    """What changed in the engine file since the review: the reviewed blob,
    from the engine's repository, against the file as it is."""
    engine = ENGINE / entry["engine"]
    if not engine.is_file():
        return f"engine/{entry['engine']} is gone: the override replaces nothing now.\n"
    reviewed = subprocess.run(["git", "cat-file", "blob", entry["reviewed"]], cwd=ENGINE, capture_output=True)
    if reviewed.returncode != 0:
        return f"The reviewed blob {entry['reviewed']} is not in the engine's repository.\n"
    try:
        old, new = reviewed.stdout.decode("utf-8"), engine.read_bytes().decode("utf-8")
    except UnicodeDecodeError:
        return f"Binary file: {entry['reviewed']} -> {blob_id(engine.read_bytes())}.\n"
    return "".join(difflib.unified_diff(old.splitlines(keepends=True), new.splitlines(keepends=True),
                                        f"reviewed ({entry['reviewed'][:12]})", f"engine/{entry['engine']}"))


def accept(root: Path, path: str) -> str:
    """Record the engine file's blob id now as reviewed for the override at
    `path`, editing only that entry's `reviewed` line. Returns the id."""
    entry = next(e for e in manifest(root) if e["site"] == path)
    current = blob_id((ENGINE / entry["engine"]).read_bytes())
    document = root / MANIFEST
    text = document.read_text(encoding="utf-8")
    line = re.compile(rf"^(- site: ['\"]?{re.escape(path)}['\"]?\n(?:  .*\n)*?  reviewed: )[0-9a-f]{{40}}$", re.M)
    edited, count = line.subn(rf"\g<1>{current}", text)
    after = yaml.safe_load(edited)
    if count != 1 or [e for e in after if e["site"] != path] != [e for e in manifest(root) if e["site"] != path]:
        raise ValueError(f"{MANIFEST}: cannot find {path}'s reviewed line to update")
    document.write_text(edited, encoding="utf-8")
    return current


# ---------------------------------------------------------------------------
# The hook check
# ---------------------------------------------------------------------------

def selector_parts(selector: str) -> tuple[str | None, list[tuple[str, str, str | None]]]:
    """A hook's tag and its classes, id and attributes, as (kind, name,
    value) with kind "class", "id" or "attr"."""
    whole = SELECTOR.fullmatch(selector)
    if not whole or not selector:
        raise ValueError(f"not a hook selector: {selector!r}")
    parts = []
    for cls, ident, attr, value in SIMPLE.findall(whole.group(2)):
        parts.append(("class", cls, None) if cls else ("id", ident, None) if ident else ("attr", attr, value or None))
    return whole.group(1), parts


class _Elements(HTMLParser):
    """Each element's tag, attributes and depth (0: at the top level)."""

    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.found: list[tuple[str, dict[str, str], int]] = []
        self.depth = 0

    def handle_starttag(self, tag, attrs):
        self.handle_startendtag(tag, attrs)
        if tag not in VOID:
            self.depth += 1

    def handle_startendtag(self, tag, attrs):
        self.found.append((tag, {k: v or "" for k, v in attrs}, self.depth))

    def handle_endtag(self, tag):
        if tag not in VOID:
            self.depth = max(self.depth - 1, 0)


def matches(html: str, selector: str, root: bool = False) -> bool:
    """Whether some element of `html` matches the hook `selector`; with
    `root`, one at the top level, which the engine places as it is (the
    footer, a direct child of the page shell)."""
    tag, parts = selector_parts(selector)
    parser = _Elements()
    parser.feed(html)
    parser.close()

    def element_matches(element_tag: str, attrs: dict[str, str]) -> bool:
        if tag and element_tag != tag:
            return False
        for kind, name, value in parts:
            if kind == "class" and name not in attrs.get("class", "").split():
                return False
            if kind == "id" and attrs.get("id") != name:
                return False
            if kind == "attr" and (name not in attrs or (value is not None and attrs[name] != value)):
                return False
        return True

    return any(element_matches(t, a) for t, a, depth in parser.found if not root or depth == 0)


def context(terms: dict, template: str) -> dict:
    """The fixture context `template` is rendered with: its groups' fields."""
    fields: dict = {}
    for group in terms["templates"][template].get("context") or []:
        fields.update(terms["contexts"][group])
    return fields


def field_names(value) -> set[str]:
    """Every field a context gives, at any depth (a loop's items' too)."""
    if isinstance(value, dict):
        return set(value) | {n for v in value.values() for n in field_names(v)}
    if isinstance(value, list):
        return {n for v in value for n in field_names(v)}
    return set()


def template_uses(text: str) -> tuple[set[str], set[str]]:
    """The fields a template's text uses and the partials it includes, read
    as Hakyll reads it: each $...$ in turn, $$ a literal dollar, trimming
    dashes aside; its comments are skipped, as the build strips them."""
    text = re.sub(r"<!--.*?-->", "", text, flags=re.S)
    names, partials, at = set(), set(), 0
    while (start := text.find("$", at)) != -1:
        if text.startswith("$$", start):
            at = start + 2
            continue
        end = text.find("$", start + 1)
        if end == -1:
            break
        token = text[start + 1:end].strip("-")
        if use := FIELD.fullmatch(token):
            names.add(use.group(1) or use.group(2))
        elif include := PARTIAL.fullmatch(token):
            partials.add(include.group(1))
        at = end + 1
    return names - KEYWORDS, partials


def template_fields(text: str) -> set[str]:
    """The fields alone ('template_uses')."""
    return template_uses(text)[0]


def site_fields(root: Path, template: str) -> dict[str, set[str]]:
    """The fields `template` uses, by file: its own (the site's, else the
    engine's) and those of each partial of the site's it includes, in
    turn; the engine's partials are the engine's to keep to the contract."""
    found: dict[str, set[str]] = {}
    queue = [template]
    while queue:
        path = queue.pop(0)
        source = root / path if (root / path).is_file() else ENGINE / path if path == template else None
        if path in found or source is None:
            continue
        found[path], partials = template_uses(source.read_text(encoding="utf-8"))
        queue += sorted(partials)
    return found


def render(root: Path, template: str, fields: dict, binary: str) -> subprocess.CompletedProcess:
    """`site render-template` for `template` as `root` resolves it, its own
    templates over the engine's (this one, wherever `root` is)."""
    with tempfile.TemporaryDirectory(prefix="render-template-") as tmp:
        scratch = Path(tmp)
        (scratch / "root").mkdir()
        (scratch / "work").mkdir()
        if (root / "templates").is_dir():
            (scratch / "root" / "templates").symlink_to((root / "templates").resolve())
        (scratch / "root" / "engine").symlink_to(ENGINE)
        (scratch / "context.json").write_text(json.dumps(fields), encoding="utf-8")
        return subprocess.run([binary, "render-template", template, str(scratch / "context.json"),
                               str(scratch / "work")], cwd=scratch / "root", capture_output=True, text=True)


def template_problems(root: Path, template: str, terms: dict, binary: str) -> list[str]:
    """Why `template`, as `root` has it, breaks the contract: a field the
    contract does not give, a failed render, a hook missing from it."""
    fields = context(terms, template)
    problems = []
    for path, used in site_fields(root, template).items():
        if undeclared := sorted(used - field_names(fields)):
            where = "" if path == template else f" (in {path})"
            problems.append(f"{template}: uses {', '.join(undeclared)}{where}, which the contract does not give it")
    done = render(root, template, fields, binary)
    if done.returncode != 0:
        return problems + [f"{template}: does not render with its fixture context: {done.stderr.strip()}"]
    missing = [h["selector"] + (" at its top level" if h.get("root") else "")
               for h in terms["templates"][template].get("hooks") or []
               if not matches(done.stdout, h["selector"], root=bool(h.get("root")))]
    if missing:
        problems.append(f"{template}: lacks {', '.join(missing)}")
    return problems


def hook_problems(root: Path, binary: str) -> list[str]:
    """Each of the site's template overrides against the contract: it must
    be an override point, render with its fixture context and keep its
    hooks."""
    terms = contract()
    problems = []
    for entry in manifest(root):
        path = entry["site"]
        if not path.startswith("templates/"):
            continue
        if path not in terms["templates"]:
            problems.append(f"{path}: not an override point (engine/contract.yaml)")
            continue
        problems += template_problems(root, path, terms, binary)
    return problems


def contract_problems(terms: dict) -> list[str]:
    """What is wrong with the contract itself: its shape, a template the
    engine lacks, a hook no file it names binds to."""
    problems = []
    if set(terms) != {"contexts", "templates"}:
        return [f"contract: has {sorted(terms)}, not contexts and templates"]
    for template, entry in terms["templates"].items():
        if not template.startswith("templates/") or not (ENGINE / template).is_file():
            problems.append(f"{template}: not an engine template")
        if not isinstance(entry, dict) or set(entry) - {"context", "hooks", "optional"} or "hooks" not in entry:
            problems.append(f"{template}: an entry has context, hooks (perhaps []) and optional")
            continue
        problems += [f"{template}: no context group {g}" for g in entry.get("context") or []
                     if g not in terms["contexts"]]
        for hook in (entry["hooks"] or []) + (entry.get("optional") or []):
            if set(hook) - {"selector", "by", "why", "root"} or not hook.get("by") or \
                    not isinstance(hook.get("root", False), bool):
                problems.append(f"{template}: a hook has a selector, the files that bind it (by), why,"
                                " and perhaps root: true")
                continue
            try:
                tag, parts = selector_parts(hook["selector"])
            except ValueError as error:
                problems.append(f"{template}: {error}")
                continue
            # What a file binding the hook names: its classes, id and
            # attributes, or its tag when it has nothing else; whole.
            names = {name for _, name, _ in parts} or {tag}
            named = re.compile("|".join(rf"(?<![\w-]){re.escape(n)}(?![\w-])" for n in names))
            for by in hook["by"]:
                path = ENGINE / by
                if not path.is_file():
                    problems.append(f"{template}: {hook['selector']}: no engine file {by}")
                elif not named.search(path.read_text(encoding="utf-8")):
                    problems.append(f"{template}: {hook['selector']}: {by} does not name it")
    return problems


# ---------------------------------------------------------------------------

def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Review a site's overrides of engine files.")
    parser.add_argument("paths", nargs="*", help="overrides, by their site path (default: each unreviewed one)")
    parser.add_argument("--accept", action="store_true",
                        help="record each engine file's blob id now as the one reviewed")
    parser.add_argument("--check", action="store_true",
                        help="render each template override with its fixture context and look for its hooks")
    args = parser.parse_args(argv)
    root = SITE_ROOT

    if args.check:
        problems = hook_problems(root, site_binary())
        for problem in problems:
            print(f"override-review: {problem}")
        print(f"override-review: {len(problems)} problem(s) in the template overrides")
        return 1 if problems else 0

    known = {e["site"] for e in manifest(root)}
    unknown = [p for p in args.paths if p not in known]
    if unknown:
        print(f"override-review: not in {MANIFEST}: {', '.join(unknown)}", file=sys.stderr)
        return 2
    if args.accept:
        if not args.paths:
            print("override-review: --accept needs the overrides to accept", file=sys.stderr)
            return 2
        for path in args.paths:
            try:
                print(f"override-review: {path}: reviewed against {accept(root, path)}")
            except (OSError, ValueError) as error:
                print(f"override-review: {path}: {error}", file=sys.stderr)
                return 1
        print("override-review: the hook check (--check, make test) and the browser tests still apply")
        return 0

    pending = [(e, c) for e, c in unreviewed(root) if not args.paths or e["site"] in args.paths]
    for entry, current in pending:
        print(f"== {entry['site']}: engine/{entry['engine']} was {entry['reviewed']}, is {current or 'gone'}")
        print(upstream_diff(entry))
    if pending:
        print("Bring each override along if it needs it, then record the review:\n"
              "    engine/tools/override-review.py --accept <path>")
    else:
        print("override-review: every override was reviewed against its engine file as it is")
    return 0


if __name__ == "__main__":
    sys.exit(main())
