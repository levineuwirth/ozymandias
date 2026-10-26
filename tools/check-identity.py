#!/usr/bin/env python3
"""Check the designated engine sources against a site's identity.

Configuration comes from `site config`. The site-owned migration policy lists
legacy names, the ln namespace patterns, and the files and directories staying
in the site; it is not part of the engine. Every file Git sees has an owner.
An engine path names a file by its path in the engine, found under engine/
once A6 moves it there and at the root until then; scans and allowances use
that name, so a move is not a relocation. A site file that overrides an
engine file at the same path (overrides.yaml) is the site's, and the engine's
is the one under engine/. Allowances cover individual
occurrences. Stable IDs survive reviewed location
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
PATHS = "tests/engine-paths.txt"
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
    """An engine path where its file is: under engine/ once moved, else at
    the root."""
    moved = root / ENGINE / path
    return moved if moved.exists() else root / path


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


def visible_files(root: Path) -> set[str]:
    # What Git sees: tracked files plus untracked ones not ignored, so a new
    # file needs an owner before it can be committed, while caches, downloads
    # and build products (ignored) need none. A tracked file deleted in the
    # working copy is gone.
    listing = subprocess.check_output(
        ["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"], cwd=root)
    return {p for p in listing.decode().split("\0") if p and (root / p).is_file()}


def inventory(root: Path, policy: dict) -> list[str]:
    paths = [s for line in (root / PATHS).read_text().splitlines()
             if (s := line.strip()) and not s.startswith("#")]
    if len(set(paths)) != len(paths):
        raise ValueError("duplicate engine paths")
    if any(p.endswith("/") for p in paths):
        raise ValueError("engine paths name files, not directories")
    # Every file in the repository needs an owner: an engine path, a site or
    # vendored file, or a site or vendored directory (an entry ending in /).
    # An engine path inside a site directory is the engine's. Under engine/,
    # only engine paths and vendored files belong, by their path there, and
    # each engine path is in one place, unless a site file overrides it: then
    # it is both an engine path and a site file, and the engine's is under
    # engine/. Engine and site entries must exist; vendored files are
    # downloaded and need not.
    owned = policy["site-files"] + policy["vendored-files"]
    files = [e for e in owned if not e.endswith("/")]
    directories = [e for e in owned if e.endswith("/")]
    listed = paths + files + directories
    candidates = visible_files(root)
    overrides = overridden(root)
    paired = overrides & set(paths) & set(policy["site-files"])
    conflicting = sorted({e for e in listed if listed.count(e) > (2 if e in paired else 1)}
                         | {p for p in paths if p in candidates and ENGINE + p in candidates
                            and p not in paired}
                         | (overrides - paired))
    exact = set(paths) | set(files)
    vendored = policy["vendored-files"]

    def owner(path: str) -> bool:
        if path.startswith(ENGINE):
            rel = path.removeprefix(ENGINE)
            return rel in paths or any(rel == v or (v.endswith("/") and rel.startswith(v))
                                       for v in vendored)
        return path in exact or any(path.startswith(d) for d in directories)

    unclassified = sorted(p for p in candidates if not owner(p))
    site = policy["site-files"]
    missing = sorted({p for p in paths if p not in candidates and ENGINE + p not in candidates}
                     | {ENGINE + p for p in paired if ENGINE + p not in candidates}
                     | ({e for e in site if not e.endswith("/")} - candidates)
                     | {d for d in site if d.endswith("/") and not any(p.startswith(d) for p in candidates)})
    if unclassified or missing or conflicting:
        raise ValueError(f"engine inventory: unclassified={unclassified}, missing={missing}, conflicting={conflicting}")
    return paths


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
        paths, count = check(SITE_ROOT, site_config())
        print(f"identity: {paths} engine files; {count} frozen allowances remain")
        return 0
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        print(f"identity: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
