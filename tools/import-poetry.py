#!/usr/bin/env python3
"""
import-poetry.py — Import a poetry collection from a Project Gutenberg plain-text file.

Produces:
  content/poetry/{collection-slug}/index.md          Collection index page
  content/poetry/{collection-slug}/{poem-slug}.md    One file per poem

Usage:
  python tools/import-poetry.py gutenberg.txt \\
      --poet "William Shakespeare" \\
      --collection "Sonnets" \\
      --date 1609 \\
      --title-prefix "Sonnet" \\
      --tags poetry,english \\
      [--slug shakespeare-sonnets] \\
      [--interactive] \\
      [--dry-run] \\
      [--overwrite]

The --title-prefix controls per-poem title generation:
  "Sonnet" → "Sonnet 1", "Sonnet 2", ..., slug "sonnet-1", "sonnet-2"
  If omitted, defaults to the singular of --collection (strips trailing 's').
"""

import argparse
import re
import sys
from pathlib import Path
from typing import Optional

sys.path.insert(0, str(Path(__file__).resolve().parent))
from front_matter import scalar, tags_field  # noqa: E402
from sitelib import SITE_ROOT, slugify  # noqa: E402

POETRY_DIR  = SITE_ROOT / "content" / "poetry"

# ---------------------------------------------------------------------------
# Roman numeral conversion
# ---------------------------------------------------------------------------

_ROMAN_VALS = [
    ("M", 1000), ("CM", 900), ("D", 500), ("CD", 400),
    ("C",  100), ("XC",  90), ("L",  50), ("XL",  40),
    ("X",   10), ("IX",   9), ("V",   5), ("IV",   4), ("I", 1),
]

def roman_to_int(s: str) -> Optional[int]:
    s = s.upper().strip()
    if not s:
        return None
    i, result = 0, 0
    for numeral, value in _ROMAN_VALS:
        while i < len(s) and s[i : i + len(numeral)] == numeral:
            result += value
            i += len(numeral)
    return result if i == len(s) and result > 0 else None

# Matches a line that is *solely* a Roman numeral (with optional period/trailing space).
# Anchored; leading/trailing whitespace stripped by caller.
_ROMAN_RE = re.compile(
    r"^(M{0,4}(?:CM|CD|D?C{0,3})(?:XC|XL|L?X{0,3})(?:IX|IV|V?I{0,3}))\.?$",
    re.IGNORECASE,
)

# ---------------------------------------------------------------------------
# Gutenberg parsing
# ---------------------------------------------------------------------------

_START_RE = re.compile(r"\*\*\* START OF THE PROJECT GUTENBERG", re.IGNORECASE)
_END_RE   = re.compile(r"\*\*\* END OF THE PROJECT GUTENBERG",   re.IGNORECASE)

def strip_gutenberg(text: str) -> tuple[str, str]:
    """Return (header, body) where body is the text between the PG markers."""
    lines = text.splitlines()
    start = 0
    end   = len(lines)
    for i, line in enumerate(lines):
        if _START_RE.search(line):
            start = i + 1
            break
    for i, line in enumerate(lines):
        if _END_RE.search(line):
            end = i
            break
    header = "\n".join(lines[:start])
    body   = "\n".join(lines[start:end])
    return header, body

# ---------------------------------------------------------------------------
# Poem splitting
# ---------------------------------------------------------------------------

def split_poems(body: str) -> list[dict]:
    """
    Split body text into individual poems using Roman-numeral headings as
    boundaries. Returns a list of dicts:
        { number: int, roman: str, lines: list[str] }

    Lines are raw — call normalize_stanzas() before writing.
    """
    lines = body.splitlines()
    poems: list[dict] = []
    current: Optional[dict] = None

    for line in lines:
        stripped = line.strip()
        m = _ROMAN_RE.match(stripped)
        if m and stripped:  # empty stripped means blank line, not a heading
            number = roman_to_int(m.group(1))
            if number is not None:
                if current is not None and _has_content(current["lines"]):
                    poems.append(current)
                current = {"number": number, "roman": m.group(1).upper(), "lines": []}
                continue
        if current is not None:
            current["lines"].append(line)

    if current is not None and _has_content(current["lines"]):
        poems.append(current)

    return poems

def _has_content(lines: list[str], min_words: int = 4) -> bool:
    text = " ".join(l.strip() for l in lines if l.strip())
    return len(text.split()) >= min_words

# ---------------------------------------------------------------------------
# Stanza normalization
# ---------------------------------------------------------------------------

def normalize_stanzas(raw: list[str]) -> list[str]:
    """
    Strip common indentation, remove leading/trailing blank lines, collapse
    runs of more than one blank line to a single blank line (stanza break).
    """
    lines = [l.rstrip() for l in raw]

    # Trim leading/trailing blank lines
    while lines and not lines[0].strip():
        lines.pop(0)
    while lines and not lines[-1].strip():
        lines.pop()

    # Determine and strip common leading whitespace on content lines
    content = [l for l in lines if l.strip()]
    if content:
        indent = min(len(l) - len(l.lstrip()) for l in content)
        lines = [l[indent:] if len(l) >= indent else l for l in lines]

    # Collapse multiple consecutive blank lines to one
    out: list[str] = []
    prev_blank = False
    for l in lines:
        blank = not l.strip()
        if blank and prev_blank:
            continue
        out.append(l)
        prev_blank = blank

    # Final trim
    while out and not out[0].strip():
        out.pop(0)
    while out and not out[-1].strip():
        out.pop()

    return out

def first_content_line(lines: list[str]) -> str:
    for l in lines:
        if l.strip():
            return l.strip()
    return ""

# ---------------------------------------------------------------------------
# File generation
# ---------------------------------------------------------------------------

def make_poem_file(
    poem:            dict,
    title_prefix:    str,
    poet:            str,
    collection:      str,
    collection_slug: str,
    date:            str,
    tags:            list[str],
) -> tuple[str, str]:
    """Return (filename_stem, markdown_content)."""
    title     = f"{title_prefix} {poem['number']}"
    slug      = slugify(title)
    norm      = normalize_stanzas(poem["lines"])
    abstract  = first_content_line(norm)
    col_url   = f"/poetry/{collection_slug}/"

    fm = f"""\
---
title: {scalar(title)}
number: {poem['number']}
poet: {scalar(poet)}
collection: {scalar(collection)}
collection-url: {col_url}
date: {date}
{tags_field(tags)}
abstract: {scalar(abstract)}
---

"""
    content = fm + "\n".join(norm) + "\n"
    return slug, content

def make_collection_index(
    collection:      str,
    poet:            str,
    date:            str,
    tags:            list[str],
    collection_slug: str,
    title_prefix:    str,
    poems:           list[dict],
) -> str:
    count      = len(poems)
    abstract   = f"{count} poem{'s' if count != 1 else ''}"

    poem_links = "\n".join(
        f"- [{title_prefix} {p['number']}](./{slugify(title_prefix + ' ' + str(p['number']))}.html)"
        for p in sorted(poems, key=lambda p: p["number"])
    )

    return f"""\
---
title: {scalar(collection)}
poet: {scalar(poet)}
date: {date}
{tags_field(tags)}
abstract: {scalar(abstract)}
---

*{poet}* · {date}

{poem_links}
"""

# ---------------------------------------------------------------------------
# Interactive review
# ---------------------------------------------------------------------------

def interactive_review(poems: list[dict], title_prefix: str) -> list[dict]:
    approved: list[dict] = []
    total = len(poems)
    for idx, poem in enumerate(poems, 1):
        title   = f"{title_prefix} {poem['number']}"
        preview = first_content_line(normalize_stanzas(poem["lines"]))
        n_lines = sum(1 for l in poem["lines"] if l.strip())

        print(f"\n{'─' * 60}")
        print(f"  [{idx}/{total}]  {poem['roman']}.  →  {title}")
        print(f"  First line : {preview}")
        print(f"  Body lines : {n_lines}")
        print()
        resp = input("  [Enter] include   s skip   q quit: ").strip().lower()
        if resp == "q":
            print("Stopped at user request.")
            break
        elif resp == "s":
            print(f"  Skipped {title}.")
            continue
        approved.append(poem)

    return approved

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(
        description="Import a Gutenberg poetry collection into content/poetry/."
    )
    parser.add_argument("source",            help="Path to the Gutenberg .txt file")
    parser.add_argument("--poet",            required=True, help='e.g. "William Shakespeare"')
    parser.add_argument("--collection",      required=True, help='e.g. "Sonnets"')
    parser.add_argument("--date",            required=True, help="Publication year, e.g. 1609")
    parser.add_argument("--title-prefix",    help='Per-poem title prefix, e.g. "Sonnet". Defaults to singular of --collection.')
    parser.add_argument("--tags",            default="poetry", help="Comma-separated tags (default: poetry)")
    parser.add_argument("--slug",            help="Override collection directory slug")
    parser.add_argument("--interactive",     action="store_true", help="Review each poem before writing")
    parser.add_argument("--dry-run",         action="store_true", help="Show what would be written; write nothing")
    parser.add_argument("--overwrite",       action="store_true", help="Overwrite existing files")
    args = parser.parse_args()

    source = Path(args.source)
    if not source.exists():
        print(f"error: file not found: {source}", file=sys.stderr)
        sys.exit(1)

    # Validate publication year — flows into YAML unchanged so it must be
    # well-formed and within a sane range.
    try:
        year = int(args.date)
    except ValueError:
        print(
            f"error: --date must be an integer year, got {args.date!r}",
            file=sys.stderr,
        )
        sys.exit(1)
    if not (1 <= year <= 2100):
        print(
            f"error: --date {year} out of plausible range (1..2100)",
            file=sys.stderr,
        )
        sys.exit(1)

    # Defaults
    title_prefix     = args.title_prefix or args.collection.rstrip("s")
    if not title_prefix.strip():
        print(
            "error: title prefix is empty (check --title-prefix or --collection)",
            file=sys.stderr,
        )
        sys.exit(1)
    collection_slug  = args.slug or slugify(f"{args.poet}-{args.collection}")
    if not collection_slug or collection_slug == "-":
        print(
            f"error: collection slug is empty "
            f"(check --slug, --collection={args.collection!r}, --poet={args.poet!r})",
            file=sys.stderr,
        )
        sys.exit(1)
    tags             = [t.strip() for t in args.tags.split(",")]
    out_dir          = POETRY_DIR / collection_slug

    text = source.read_text(encoding="utf-8", errors="replace")
    _header, body = strip_gutenberg(text)

    if not body.strip():
        print("warning: Gutenberg markers not found — treating entire file as body", file=sys.stderr)
        body = text

    poems = split_poems(body)

    if not poems:
        print("No poems detected. The file may not use Roman-numeral headings.", file=sys.stderr)
        print("First 50 lines of body:", file=sys.stderr)
        for ln in body.splitlines()[:50]:
            print(f"  {repr(ln)}", file=sys.stderr)
        sys.exit(1)

    print(f"Detected {len(poems)} poems  ·  collection: {args.collection}  ·  poet: {args.poet}")

    if args.interactive:
        poems = interactive_review(poems, title_prefix)
        print(f"\n{len(poems)} poem(s) approved for import.")

    if not poems:
        print("Nothing to write.")
        return

    # Build file map
    files: dict[Path, str] = {}
    for poem in poems:
        slug, content = make_poem_file(
            poem, title_prefix, args.poet, args.collection,
            collection_slug, args.date, tags,
        )
        files[out_dir / f"{slug}.md"] = content

    files[out_dir / "index.md"] = make_collection_index(
        args.collection, args.poet, args.date, tags,
        collection_slug, title_prefix, poems,
    )

    # Dry run
    if args.dry_run:
        print(f"\nDry run — {len(files)} file(s) → {out_dir.relative_to(SITE_ROOT)}/")
        for path in sorted(files):
            marker = " (exists)" if path.exists() else ""
            print(f"  {path.name}{marker}")
        print(f"\nSample — first poem:\n{'─'*60}")
        first_content = next(v for k, v in files.items() if k.name != "index.md")
        print(first_content[:800])
        return

    # Write
    out_dir.mkdir(parents=True, exist_ok=True)
    written = skipped = 0
    for path, content in sorted(files.items()):
        if path.exists() and not args.overwrite:
            print(f"  skip  {path.name}")
            skipped += 1
        else:
            path.write_text(content, encoding="utf-8", errors="strict")
            print(f"  write {path.name}")
            written += 1

    print(f"\n{written} written, {skipped} skipped → {out_dir.relative_to(SITE_ROOT)}/")
    if not args.dry_run:
        print("Next: make clean && make build")

if __name__ == "__main__":
    main()
