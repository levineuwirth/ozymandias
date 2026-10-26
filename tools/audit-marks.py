#!/usr/bin/env python3
"""Audit frontmatter marks (monograms + epistemic figures).

Walks ``content/**/*.md``, resolves each piece's monogram candidate
path, checks whether ``mark.svg`` exists and whether ``status:`` is
set, validates the epistemic-figure fields against the vocabularies
in ``build/Marks.hs``, and emits a table plus corpus-wide coverage
percentages. Output is pure ASCII so it pipes / scrolls cleanly.

The field validation exists because the generator degrades silently:
``Marks.validate`` maps any off-vocabulary value to ``Nothing``, which
is indistinguishable from an absent field, and an absent axis turns
the figure's closed polygon into an open polyline. A one-word typo
(``scope: moderate``, borrowed from the novelty / practicality scales)
therefore opens the figure with no warning anywhere in the build.

Run as::

    make audit-marks

or directly via::

    .venv/bin/python engine/tools/audit-marks.py

Exit code is always 0; this is a report tool, not a gate.

The dual-form path resolver matches ``build/Marks.hs``:

  * ``content/essays/foo.md``       -> ``content/essays/foo.mark.svg``
  * ``content/essays/foo/index.md`` -> ``content/essays/foo/mark.svg``

Photography is excluded: visual content doesn't carry monograms or
epistemic figures by design (see PHOTOGRAPHY.md).
"""

from __future__ import annotations

import sys
from dataclasses import dataclass
from pathlib import Path

import yaml

sys.path.insert(0, str(Path(__file__).resolve().parent))
import shared_rules  # noqa: E402
import front_matter  # noqa: E402
import sitelib  # noqa: E402
CONTENT_ROOT = sitelib.SITE_ROOT / "content"

# Sections that ship marks by design — these get a coverage line in
# the summary even when empty (so a regression is visible). Other
# sections appear in the summary only when they contain pieces.
PRIMARY_SECTIONS = ("essays", "blog", "poetry", "fiction", "music")

# Excluded entirely: visual content (PHOTOGRAPHY.md), in-progress
# drafts, and the per-portal tag-meta sidecar tree (which is metadata
# infrastructure, not authored pieces).
SKIPPED_DIRS = ("photography", "drafts", "tag-meta")


# Epistemic-figure field vocabularies, from the generator itself
# (build/Marks.hs, through tools/shared_rules.py). The generator silently
# drops any value not on them, so a hand copy here that drifted would stop
# catching exactly the class of bug this audit was added for.
def enum_fields() -> dict[str, list[str]]:
    return shared_rules.shared_rules()["epistemic-vocabulary"]

# Numeric axes, as (low, high) inclusive bounds. Same silent-drop
# hazard: build/Marks.hs reads these with ``readMaybe``, so a
# non-integer vanishes, and an out-of-range value puts the vertex
# outside the roundel instead.
INT_FIELDS: dict[str, tuple[int, int]] = {
    "confidence": (0, 100),
    "importance": (1, 5),
    "evidence": (1, 5),
}

# ``confidence`` alone accepts a non-numeric escape hatch: a formal
# proof opts out of a credence rather than claiming 100 (MARKS.md 4.3).
CONFIDENCE_WORDS = ("proved", "proven")


def field_problems(fm: dict) -> list[str]:
    """Report epistemic fields whose values the generator would drop.

    One human-readable string per bad field. An absent field is not a
    problem here -- MARKS.md 3.3 makes partial figures legal -- only a
    field that is *present and unusable*."""
    problems: list[str] = []

    for name, allowed in enum_fields().items():
        if name not in fm or fm[name] is None:
            continue
        value = str(fm[name]).strip().lower()
        if value and value not in allowed:
            problems.append(f"{name}: {fm[name]!s} (want {'|'.join(allowed)})")

    for name, (low, high) in INT_FIELDS.items():
        if name not in fm or fm[name] is None:
            continue
        raw = str(fm[name]).strip()
        if name == "confidence" and raw.lower() in CONFIDENCE_WORDS:
            continue
        try:
            value = int(raw)
        except ValueError:
            problems.append(f"{name}: {raw} (want an integer {low}-{high})")
            continue
        if not low <= value <= high:
            problems.append(f"{name}: {raw} (want {low}-{high})")

    return problems


@dataclass
class AuditRow:
    """One row of audit output for a single source file."""

    path: Path
    section: str
    has_monogram: bool
    has_status: bool
    problems: list[str]

    @property
    def suggestion(self) -> str:
        actions = []
        if not self.has_monogram:
            actions.append("add mark.svg")
        if not self.has_status:
            actions.append("set status:")
        if self.problems:
            actions.append(f"fix {len(self.problems)} field value(s)")
        return ", ".join(actions)


def parse_frontmatter(md_path: Path) -> dict:
    """Extract the YAML frontmatter block from a Markdown file.

    Returns an empty dict on parse failure or when no frontmatter is
    present. Errors are non-fatal — the audit reports what it can."""
    try:
        text = md_path.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return {}
    try:
        return front_matter.load(text)
    except yaml.YAMLError:
        return {}


def monogram_path(md_path: Path) -> Path:
    """Resolve the candidate ``mark.svg`` path for a Markdown source.

    Mirrors ``Marks.monogramCandidates`` in build/Marks.hs."""
    if md_path.name == "index.md":
        return md_path.parent / "mark.svg"
    return md_path.with_suffix(".mark.svg")


def section_of(path: Path) -> str:
    """Bucket a content path under its top-level section name.

    Returns ``"standalone"`` for files directly under ``content/``."""
    rel = path.relative_to(CONTENT_ROOT)
    if len(rel.parts) == 1:
        return "standalone"
    return rel.parts[0]


def collect() -> list[AuditRow]:
    """Walk content/ and return one AuditRow per published source file."""
    rows: list[AuditRow] = []

    for md_path in CONTENT_ROOT.rglob("*.md"):
        rel = md_path.relative_to(CONTENT_ROOT)
        if rel.parts and rel.parts[0] in SKIPPED_DIRS:
            continue

        # Skip tag-meta sidecars (they're not authored pages).
        if md_path.name == "_tag-meta.md":
            continue

        fm = parse_frontmatter(md_path)
        rows.append(
            AuditRow(
                path=md_path,
                section=section_of(md_path),
                has_monogram=monogram_path(md_path).is_file(),
                has_status="status" in fm and bool(str(fm["status"]).strip()),
                problems=field_problems(fm),
            )
        )

    rows.sort(
        key=lambda r: (
            r.section != "standalone",  # standalone last
            r.section,
            not r.has_status,
            not r.has_monogram,
            str(r.path),
        )
    )
    return rows


def fmt_check(present: bool) -> str:
    return "OK" if present else "--"


def render_table(rows: list[AuditRow]) -> None:
    if not rows:
        print("No content files found under content/.")
        return

    path_w = max(len(str(r.path)) for r in rows)
    path_w = min(path_w, 60)  # cap so suggestions stay on the same line

    header = (
        f"{'PATH':<{path_w}}  {'MONO':<5} {'EPIS':<5} {'FLDS':<5}  SUGGESTION"
    )
    print(header)
    print("-" * len(header))

    current_section = None
    for r in rows:
        if r.section != current_section:
            current_section = r.section
            print(f"\n# {current_section}")

        path_str = str(r.path)
        if len(path_str) > path_w:
            path_str = path_str[: path_w - 1] + "..."
        print(
            f"{path_str:<{path_w}}  "
            f"{fmt_check(r.has_monogram):<5} "
            f"{fmt_check(r.has_status):<5} "
            f"{fmt_check(not r.problems):<5}  "
            f"{r.suggestion}"
        )


def render_problems(rows: list[AuditRow]) -> None:
    """List every unusable field value, with its file and the vocabulary.

    Separate from the table because the table can only afford a
    yes / no column, and the value itself is the whole point: the
    generator gives no other signal that it threw the value away."""
    flagged = [r for r in rows if r.problems]
    if not flagged:
        return

    print()
    print("# Field values the figure generator would discard")
    print("-" * 60)
    for r in flagged:
        print(str(r.path))
        for problem in r.problems:
            print(f"    {problem}")


def render_summary(rows: list[AuditRow]) -> None:
    print()
    print("# Coverage")
    print("-" * 60)

    by_section: dict[str, list[AuditRow]] = {}
    for r in rows:
        by_section.setdefault(r.section, []).append(r)

    def line(label: str, group: list[AuditRow]) -> None:
        n = len(group)
        if n == 0:
            return
        m = sum(1 for r in group if r.has_monogram)
        e = sum(1 for r in group if r.has_status)
        f = sum(1 for r in group if not r.problems)
        print(
            f"{label:<14} {n:>3} pieces  "
            f"monogram {m:>3}/{n:<3} ({m * 100 // n:>3}%)  "
            f"epistemic {e:>3}/{n:<3} ({e * 100 // n:>3}%)  "
            f"fields {f:>3}/{n:<3} ({f * 100 // n:>3}%)"
        )

    rendered: set[str] = set()
    for section in PRIMARY_SECTIONS:
        if section in by_section:
            line(section, by_section[section])
            rendered.add(section)

    other_sections = sorted(s for s in by_section if s not in rendered)
    for section in other_sections:
        line(section, by_section[section])

    print("-" * 60)
    line("total", rows)


def main() -> int:
    if not CONTENT_ROOT.is_dir():
        print(f"error: {CONTENT_ROOT}/ not found (run from the site's root, or set SITE_ROOT)",
              file=sys.stderr)
        return 1

    rows = collect()
    render_table(rows)
    render_problems(rows)
    render_summary(rows)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
