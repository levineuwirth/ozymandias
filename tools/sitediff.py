#!/usr/bin/env python3
"""Compare two built sites, for a change that must leave the output alone
or change only what it says it changes.

    tools/sitediff.py BASE NEW [--body-only] [--allow FILE]... [--strict]
                      [--include-source] [--show N] [--limit N]

BASE and NEW are two `_site` trees. Keep the baseline with

    cp -a --reflink=auto _site <dir>

and never with hard links (`cp -al`, `rsync --link-dest`): Hakyll rewrites
its outputs in place, so the next build would rewrite the baseline too.

Exit status: 0 when every difference is permitted; 1 when one is not, or,
with --strict, when an allowlist entry covered nothing; 2 on a usage error.

Never compared:
  *.br *.gz *.sig  compression and signature sidecars, derived from files
                   that are compared
  pagefind/        the keyword index, whose fragments hash page content that
                   is compared directly
  build/           build telemetry: the build time, durations, code sizes
  source/          the source mirror, which changes whenever code does;
                   --include-source compares it

--body-only compares each HTML page without its <head>; other files are
still compared whole. It is the Phase B rule: stylesheet links may change,
page bodies may not.

An allowlist is a YAML list of the changes a step permits:

    - path: "essays/*.html"   # fnmatch on the path under the site root;
                              # * also matches across directories
      pattern: '<meta name="author" content="[^"]*">'
                              # optional, for changed files only: the two
                              # files may differ only inside matches of this
                              # regular expression (bytes, UTF-8)
      change: changed         # optional: changed, added, removed or any
      reason: identity meta tags (A9)

A difference is permitted when entries cover it; several pattern entries
can cover one file together. Every pattern is matched against the original
bytes, and the union of the matches is removed from both files, which must
then be equal. An entry covers a change only where the two files differ
inside its matches: matching unchanged text covers nothing. A change inside
the matches of more than one entry counts for each, and is reported as
shared, so a redundant entry shows. An entry that covered nothing is
reported, and --strict makes that an error, for a step whose list means
"exactly these changes".
"""

from __future__ import annotations

import argparse
import difflib
import fnmatch
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path

SIDECARS = (".br", ".gz", ".sig")
NEVER_COMPARED = ("pagefind", "build")
TEXT_SUFFIXES = frozenset({
    ".html", ".xml", ".json", ".txt", ".css", ".js", ".svg", ".webmanifest",
    ".csv", ".tsv", ".md", ".yaml", ".yml", ".bib", ".asc",
})
CHANGES = ("changed", "added", "removed", "any")
HEAD = re.compile(rb"<head(?:\s[^>]*)?>.*?</head\s*>", re.I | re.S)


@dataclass
class Entry:
    source: str
    path: str
    reason: str
    pattern: re.Pattern[bytes] | None = None
    change: str = "any"
    used: int = 0

    def covers(self, rel: str, kind: str) -> bool:
        return self.change in (kind, "any") and fnmatch.fnmatchcase(rel, self.path)


@dataclass
class Result:
    base_count: int
    new_count: int
    not_permitted: list[tuple[str, str]] = field(default_factory=list)
    permitted: list[tuple[str, str]] = field(default_factory=list)
    # A change that more than one entry covered: (path, the entries' sources).
    shared: list[tuple[str, tuple[str, ...]]] = field(default_factory=list)

    @property
    def clean(self) -> bool:
        return not self.not_permitted


def load_allowlist(path: Path) -> list[Entry]:
    import yaml

    data = yaml.safe_load(path.read_text(encoding="utf-8"))
    if data is None:
        return []
    if not isinstance(data, list):
        raise ValueError(f"{path}: expected a list of entries")
    entries = []
    for number, raw in enumerate(data, 1):
        where = f"{path}: entry {number}"
        if not isinstance(raw, dict):
            raise ValueError(f"{where}: expected a mapping")
        unknown = sorted(set(raw) - {"path", "pattern", "change", "reason"})
        if unknown:
            raise ValueError(f"{where}: unknown keys {unknown}")
        glob, reason = raw.get("path"), raw.get("reason")
        if not isinstance(glob, str) or not glob:
            raise ValueError(f"{where}: `path` is required")
        if not isinstance(reason, str) or not reason.strip():
            raise ValueError(f"{where}: `reason` is required")
        change = raw.get("change", "any")
        if change not in CHANGES:
            raise ValueError(f"{where}: `change` must be one of {', '.join(CHANGES)}")
        pattern = raw.get("pattern")
        compiled = None
        if pattern is not None:
            if not isinstance(pattern, str) or not pattern:
                raise ValueError(f"{where}: `pattern` must be a non-empty string")
            if change in ("added", "removed"):
                raise ValueError(f"{where}: `pattern` applies to changed files only")
            try:
                compiled = re.compile(pattern.encode("utf-8"))
            except re.error as e:
                raise ValueError(f"{where}: bad pattern: {e}") from None
        entries.append(Entry(f"{path} entry {number}", glob, reason.strip(), compiled, change))
    return entries


def site_files(root: Path, include_source: bool) -> set[str]:
    skipped = set(NEVER_COMPARED) | (set() if include_source else {"source"})
    found = set()
    for p in root.rglob("*"):
        if not p.is_file() or p.name.endswith(SIDECARS):
            continue
        rel = p.relative_to(root)
        if rel.parts[0] in skipped:
            continue
        found.add(rel.as_posix())
    return found


def same_bytes(a: Path, b: Path) -> bool:
    if a.stat().st_size != b.stat().st_size:
        return False
    with a.open("rb") as fa, b.open("rb") as fb:
        while True:
            x, y = fa.read(1 << 20), fb.read(1 << 20)
            if x != y:
                return False
            if not x:
                return True


def body(data: bytes) -> bytes:
    return HEAD.sub(b"", data, count=1)


Removed = dict[int, list[tuple[bytes, frozenset[int]]]]


def split(data: bytes, entries: list[Entry]) -> tuple[bytes, Removed]:
    """`data` without every entry's matches, and what was removed.

    Every pattern is matched against the original bytes, never against what
    another pattern left: removing one match must not create another. Spans
    that overlap are merged; spans that merely touch stay apart, so text an
    entry matched is never credited to its neighbor's change. The result
    maps each offset in the remaining bytes to the spans removed there, in
    order, each with the indexes of the entries whose matches make it up."""
    found = sorted((m.start(), m.end(), i) for i, e in enumerate(entries)
                   for m in e.pattern.finditer(data) if m.end() > m.start())
    merged: list[tuple[int, int, set[int]]] = []
    for start, end, i in found:
        if merged and start < merged[-1][1]:
            s, e, ids = merged[-1]
            merged[-1] = (s, max(e, end), ids | {i})
        else:
            merged.append((start, end, {i}))
    kept: list[bytes] = []
    removed: Removed = {}
    pos = length = 0
    for start, end, ids in merged:
        kept.append(data[pos:start])
        length += start - pos
        removed.setdefault(length, []).append((data[start:end], frozenset(ids)))
        pos = end
    kept.append(data[pos:])
    return b"".join(kept), removed


def covering(rel: str, a: bytes, b: bytes, entries: list[Entry]
             ) -> tuple[list[Entry], list[tuple[Entry, ...]]] | None:
    """The entries that permit a changed file, with any change that several
    entries covered together; None when the file is not permitted.

    With pattern entries, the two files must be equal once the matches are
    removed. The removed spans then line up by their offset in what remains,
    and the spans at one offset are aligned in order. A span both sides
    removed unchanged covers nothing; the entries behind every other span,
    on either side, covered a change."""
    applicable = [e for e in entries if e.covers(rel, "changed")]
    whole = [e for e in applicable if e.pattern is None]
    if whole:
        return whole[:1], []
    patterned = [e for e in applicable if e.pattern is not None]
    if not patterned:
        return None
    kept_a, removed_a = split(a, patterned)
    kept_b, removed_b = split(b, patterned)
    if kept_a != kept_b:
        return None
    used: set[int] = set()
    shared: set[frozenset[int]] = set()
    for offset in removed_a.keys() | removed_b.keys():
        spans_a, spans_b = removed_a.get(offset, []), removed_b.get(offset, [])
        bytes_a, bytes_b = [s for s, _ in spans_a], [s for s, _ in spans_b]
        if bytes_a == bytes_b:
            continue
        matcher = difflib.SequenceMatcher(None, bytes_a, bytes_b, autojunk=False)
        for tag, i1, i2, j1, j2 in matcher.get_opcodes():
            if tag == "equal":
                continue
            for _, ids in spans_a[i1:i2] + spans_b[j1:j2]:
                used |= ids
                if len(ids) > 1:
                    shared.add(ids)
    return ([patterned[i] for i in sorted(used)],
            sorted(tuple(patterned[i] for i in sorted(ids)) for ids in shared))


def compare(base: Path, new: Path, *, body_only: bool = False,
            entries: list[Entry] | None = None, include_source: bool = False) -> Result:
    entries = entries or []
    a_files, b_files = site_files(base, include_source), site_files(new, include_source)
    result = Result(len(a_files), len(b_files))

    def judge(rel: str, kind: str, a: bytes = b"", b: bytes = b"") -> None:
        if kind == "changed":
            found = covering(rel, a, b, entries)
        else:
            found = next((([e], []) for e in entries if e.pattern is None and e.covers(rel, kind)), None)
        if found is None:
            result.not_permitted.append((kind, rel))
            return
        users, shared = found
        for e in users:
            e.used += 1
        result.permitted.append((kind, rel))
        result.shared += [(rel, tuple(e.source for e in group)) for group in shared]

    for rel in sorted(a_files - b_files):
        judge(rel, "removed")
    for rel in sorted(b_files - a_files):
        judge(rel, "added")
    for rel in sorted(a_files & b_files):
        pa, pb = base / rel, new / rel
        if same_bytes(pa, pb):
            continue
        a, b = pa.read_bytes(), pb.read_bytes()
        if body_only and rel.endswith(".html"):
            a, b = body(a), body(b)
            if a == b:
                continue
        judge(rel, "changed", a, b)
    return result


def excerpt(a: bytes, b: bytes, width: int = 120) -> list[str]:
    i = next((n for n, (x, y) in enumerate(zip(a, b)) if x != y), min(len(a), len(b)))
    line = a.count(b"\n", 0, i) + 1
    lo, hi = max(0, i - width // 2), i + width // 2

    def window(data: bytes) -> str:
        return data[lo:hi].decode("utf-8", "replace").replace("\n", "⏎")

    return [f"first difference at byte {i} (line {line})", f"- {window(a)}", f"+ {window(b)}"]


def describe(base: Path, new: Path, rel: str, body_only: bool, show: int) -> list[str]:
    a, b = (base / rel).read_bytes(), (new / rel).read_bytes()
    if body_only and rel.endswith(".html"):
        a, b = body(a), body(b)
    lines = excerpt(a, b)
    if show and Path(rel).suffix in TEXT_SUFFIXES and len(a) + len(b) < 4_000_000:
        al = a.decode("utf-8", "replace").splitlines()
        bl = b.decode("utf-8", "replace").splitlines()
        changed = [l for l in difflib.unified_diff(al, bl, lineterm="", n=0)
                   if l[:1] in "+-" and not l.startswith(("+++", "---"))]
        lines += [l[:200] for l in changed[:show]]
        if len(changed) > show:
            lines.append(f"… {len(changed) - show} more changed lines")
    return lines


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("base", type=Path)
    parser.add_argument("new", type=Path)
    parser.add_argument("--body-only", action="store_true",
                        help="compare HTML pages without their <head>")
    parser.add_argument("--allow", type=Path, action="append", default=[],
                        help="YAML allowlist of permitted changes (repeatable)")
    parser.add_argument("--strict", action="store_true",
                        help="fail when an allowlist entry covered nothing")
    parser.add_argument("--include-source", action="store_true",
                        help="compare the source/ mirror too")
    parser.add_argument("--show", type=int, default=6, metavar="N",
                        help="changed lines shown per text file (default 6)")
    parser.add_argument("--limit", type=int, default=25, metavar="N",
                        help="differences listed per kind (default 25)")
    args = parser.parse_args(argv)

    for d in (args.base, args.new):
        if not d.is_dir():
            parser.error(f"{d}: not a directory")
    try:
        entries = [e for path in args.allow for e in load_allowlist(path)]
    except (OSError, ValueError) as e:
        parser.error(str(e))

    result = compare(args.base, args.new, body_only=args.body_only,
                     entries=entries, include_source=args.include_source)
    mode = "body-only" if args.body_only else "whole-file"
    print(f"sitediff: {args.base} → {args.new} ({mode})")
    print(f"files: {result.base_count} base, {result.new_count} new; "
          f"{len(result.permitted)} permitted differences, {len(result.not_permitted)} not permitted")

    marks = {"changed": "~", "added": "+", "removed": "-"}
    for kind in ("removed", "added", "changed"):
        found = [rel for k, rel in result.not_permitted if k == kind]
        for rel in found[:args.limit]:
            print(f"  {marks[kind]} {kind}: {rel}")
            if kind == "changed":
                for line in describe(args.base, args.new, rel, args.body_only, args.show):
                    print(f"      {line}")
        if len(found) > args.limit:
            print(f"  … {len(found) - args.limit} more {kind}")

    for e in entries:
        if e.used:
            print(f"  permitted by {e.source} ({e.reason}): {e.used}")
    for rel, sources in result.shared[:args.limit]:
        print(f"  shared: {rel}: one change covered by {' and '.join(sources)}")
    if len(result.shared) > args.limit:
        print(f"  … {len(result.shared) - args.limit} more shared")
    unused = [e for e in entries if not e.used]
    for e in unused:
        print(f"  unused: {e.source} ({e.reason})")

    if not result.clean:
        return 1
    if args.strict and unused:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
