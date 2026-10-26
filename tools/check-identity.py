#!/usr/bin/env python3
"""Check the engine's files against a site's identity.

Configuration comes from `site config`. The site-owned migration policy lists
legacy names, the ln namespace patterns, the site's files and directories,
and the engine's vendored code; it is not part of the engine. Every file Git
sees has an owner: the engine owns what is under engine/, named by its path
there, which scans and allowances use; the site owns the rest, as the policy
names it. A site file that overrides an engine file at the same path
(overrides.yaml) is the site's, and the engine's is the one under engine/.
Allowances cover individual occurrences. Stable IDs survive reviewed location
changes; every recorded revision must be a subset of the previous revision.
There is deliberately no command to refresh or regenerate allowances.
"""
from __future__ import annotations

from pathlib import Path
import re
import subprocess
import sys

import yaml

sys.path.insert(0, str(Path(__file__).resolve().parent))
from sitelib import SITE_ROOT, site_config

ENGINE = "engine/"
ALLOWANCES = "tests/identity-allowances.yaml"
POLICY = "tests/identity-policy.yaml"
OVERRIDES = "overrides.yaml"
FONTS = "tools/fonts.yaml"


def font_names(root: Path) -> list[str]:
    """The names the site's fonts.yaml gives its font subsets in place of a
    reserved one: the site's own, so identity."""
    path = root / FONTS
    document = yaml.safe_load(path.read_text()) if path.exists() else {}
    families = (document or {}).get("families") or {}
    return [spec["rename"]["to"] for spec in families.values() if "rename" in spec]


def denylist(config: dict, fixed: list[str]) -> list[str]:
    terms = set(fixed)
    for key in ("title", "author", "author-sort", "host", "forge-host", "email",
                "contact-email", "source-repo", "signing-fingerprint"):
        if value := config.get(key):
            terms.add(value)
    host = config["host"].removeprefix("www.")
    terms.update((host, host.split(".")[0]))
    if fingerprint := config.get("signing-fingerprint"):
        terms.add(fingerprint.replace(" ", ""))
    if any(not isinstance(t, str) or not t for t in terms):
        raise ValueError("denylist entries must be nonempty strings")
    return sorted(terms, key=lambda t: (-len(t), t))


def scanners(terms: list[str], patterns: list[str]) -> list[re.Pattern]:
    # Terms are literal and case-insensitive; patterns (the ln namespace) are
    # case-sensitive, so the namespace's names are identity and a variable
    # named `ln` is not.
    result = [re.compile("|".join(re.escape(t) for t in terms), re.IGNORECASE)] if terms else []
    for pattern in patterns:
        try:
            compiled = re.compile(pattern)
        except re.error as error:
            raise ValueError(f"invalid identity pattern {pattern!r}: {error}") from None
        if compiled.fullmatch(""):
            raise ValueError(f"identity pattern matches the empty string: {pattern!r}")
        result.append(compiled)
    return result


def located(root: Path, path: str) -> Path:
    """An engine file by its path in the engine."""
    return root / ENGINE / path


def occurrences(root: Path, paths: list[str], terms: list[str],
                patterns: list[str] = ()) -> list[dict]:
    # Prefer the longest at a position: an email/domain/stem is one occurrence,
    # not three overlapping allowances. Columns distinguish repeats on a line.
    compiled = scanners(terms, list(patterns))
    found = []
    for name in paths:
        for line, text in enumerate(located(root, name).read_text(encoding="utf-8", errors="replace").splitlines(), 1):
            matches = sorted((m.start(), -m.end(), m.group())
                             for scanner in compiled for m in scanner.finditer(text) if m.end() > m.start())
            end = 0
            for start, negative_end, string in matches:
                if start >= end:
                    found.append(dict(file=name, line=line, column=start + 1, string=string))
                    end = -negative_end
    return found


def entries(document: dict) -> dict[str, dict]:
    if not isinstance(document, dict) or set(document) != {"bootstrap-parent", "initial-count", "entries"}:
        raise ValueError("invalid allowance manifest fields")
    result = {}
    if type(document["initial-count"]) is not int or document["initial-count"] < 0:
        raise ValueError("initial-count must be a nonnegative integer")
    for entry in document["entries"]:
        if set(entry) != {"id", "file", "line", "column", "string"}:
            raise ValueError("invalid allowance entry fields")
        if not isinstance(entry["id"], str) or entry["id"] in result:
            raise ValueError("allowance IDs must be unique strings")
        if any(type(entry[k]) is not int or entry[k] < 1 for k in ("line", "column")):
            raise ValueError("allowance locations must be positive integers")
        result[entry["id"]] = entry
    if len(result) > document["initial-count"]:
        raise ValueError("allowance count exceeds initial-count")
    return result


def check_transition(previous: dict, current: dict) -> None:
    before, after = entries(previous), entries(current)
    for key in ("initial-count", "bootstrap-parent"):
        if previous[key] != current[key]:
            raise ValueError(f"frozen {key} changed")
    for ident, entry in after.items():
        if ident not in before or before[ident]["string"] != entry["string"]:
            raise ValueError(f"new or changed allowance: {ident}")


def check_history(root: Path, current: dict) -> None:
    def git(*args):
        return subprocess.check_output(["git", *args], cwd=root, text=True).strip()
    if git("rev-parse", "--is-shallow-repository") == "true":
        raise ValueError("identity freeze requires full Git history, not a shallow checkout")
    # Check historical transitions too: after a commit, HEAD alone would let
    # that commit's newly added allowances approve themselves.
    revisions = git("rev-list", "--reverse", "HEAD", "--", ALLOWANCES).splitlines()
    previous = None
    for revision in revisions:
        recorded = yaml.safe_load(git("show", f"{revision}:{ALLOWANCES}"))
        entries(recorded)
        if previous is not None:
            check_transition(previous, recorded)
        elif len(recorded["entries"]) != recorded["initial-count"]:
            raise ValueError("initial allowance population does not match initial-count")
        previous = recorded
    if previous is None:
        if git("rev-parse", "HEAD") != current["bootstrap-parent"]:
            raise ValueError("no frozen allowance history; bootstrap is permitted only at its recorded parent")
        if len(current["entries"]) != current["initial-count"]:
            raise ValueError("initial allowance population does not match initial-count")
    else:
        check_transition(previous, current)


def check_occurrences(found: list[dict], document: dict) -> list[str]:
    allowed = entries(document)
    def location(e):
        return tuple(e[k] for k in ("file", "line", "column", "string"))
    remaining = {location(e) for e in found}
    errors = []
    for ident, entry in allowed.items():
        key = location(entry)
        if key not in remaining:
            errors.append(f"stale or duplicate allowance {ident}: {key}")
        else:
            remaining.remove(key)
    errors.extend(f"unallowed identity: {key}" for key in sorted(remaining))
    return errors


def load_policy(root: Path) -> dict:
    policy = yaml.safe_load((root / POLICY).read_text())
    keys = {"fixed-strings", "fixed-patterns", "site-files", "vendored-files"}
    if not isinstance(policy, dict) or set(policy) != keys:
        raise ValueError(f"identity policy must have exactly {sorted(keys)}")
    for key in keys:
        if not isinstance(policy[key], list) or not all(isinstance(e, str) and e for e in policy[key]):
            raise ValueError(f"identity policy {key} must be a list of nonempty strings")
    return policy


def overridden(root: Path) -> set[str]:
    """Paths where the site's overrides.yaml says a site file overrides the
    engine's: the site's at that path, the engine's under engine/."""
    path = root / OVERRIDES
    document = yaml.safe_load(path.read_text()) if path.exists() else []
    # The engine lets a site file override only the engine file at its own
    # path, so an entry naming two paths can only be a mistake.
    if not isinstance(document, list) or not all(
            isinstance(e, dict) and isinstance(e.get("site"), str) and e.get("engine") == e["site"]
            for e in document):
        raise ValueError(f"{OVERRIDES} must be a list of entries with a site path and the same engine path")
    return {e["site"] for e in document}


def visible_files(directory: Path) -> set[str]:
    # What Git sees under `directory`, by paths relative to it: tracked files
    # plus untracked ones not ignored, so a new file needs an owner before it
    # can be committed, while caches, downloads and build products (ignored)
    # need none. A tracked file deleted in the working copy is gone. Run in
    # engine/, this is the engine's repository's view, whether engine/ is in
    # the site's (Phase A) or a checkout of its own.
    listing = subprocess.check_output(
        ["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"], cwd=directory)
    return {p for p in listing.decode().split("\0") if p and (directory / p).is_file()}


def inventory(root: Path, policy: dict) -> list[str]:
    """The engine's files to scan, once every file has its one owner.

    The engine owns what Git sees under engine/; its vendored code, by
    vendored-files, is not scanned. The site owns the rest, each file named
    in site-files or inside a site directory there (an entry ending in /),
    and those entries must exist. A site file at an engine file's path
    overrides it, which overrides.yaml must declare and site-files name
    exactly; the engine's copy must then exist too.
    """
    site, vendored = policy["site-files"], policy["vendored-files"]
    files = {e for e in site if not e.endswith("/")}
    directories = [e for e in site if e.endswith("/")]
    engine = visible_files(root / ENGINE)
    seen = {p for p in visible_files(root) if not p.startswith(ENGINE)}
    overrides = overridden(root)
    paired = overrides & files & engine
    unclassified = sorted(p for p in seen if p not in files and not any(p.startswith(d) for d in directories))
    conflicting = sorted({e for e in site + vendored if (site + vendored).count(e) > 1}
                         | ((seen & engine) - paired)
                         | (overrides - files))
    missing = sorted((files - seen)
                     | {d for d in directories if not any(p.startswith(d) for p in seen)}
                     | {ENGINE + p for p in (overrides & files) - engine})
    if unclassified or missing or conflicting:
        raise ValueError(f"inventory: unclassified={unclassified}, missing={missing}, conflicting={conflicting}")
    return sorted(p for p in engine
                  if not any(p == v or (v.endswith("/") and p.startswith(v)) for v in vendored))


def check(root: Path, config: dict) -> tuple[int, int]:
    policy = load_policy(root)
    paths = inventory(root, policy)
    document = yaml.safe_load((root / ALLOWANCES).read_text())
    check_history(root, document)
    found = occurrences(root, paths, denylist(config, policy["fixed-strings"] + font_names(root)),
                        policy["fixed-patterns"])
    errors = check_occurrences(found, document)
    if errors:
        raise ValueError("\n".join(errors))
    return len(paths), len(found)


def main() -> int:
    try:
        paths, count = check(SITE_ROOT, site_config(build=True))
        print(f"identity: {paths} engine files; {count} frozen allowances remain")
        return 0
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        print(f"identity: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
