#!/usr/bin/env python3
"""viz-provenance.py — checksum the data behind each figure.

An essay that argues from benchmark numbers should let a reader check them.
The CSVs under `figures/data/` already ship to the site (the asset rule in
build/Site.hs copies everything beside a directory essay), so a reader can
download them — but nothing says whether the file they downloaded is the one
the chart was drawn from, and nothing records where the numbers came from.

This writes a PROVENANCE.json next to each `figures/data/` directory:

    {
      "generated_by": "tools/viz-provenance.py",
      "files": {
        "kem_level.csv": {
          "sha256": "…",
          "bytes": 654,
          "rows": 3,
          "source": "TODO: the command or run that produced this"
        }
      }
    }

`source` is deliberately not guessed. Only the author knows which benchmark
run a file came from, so the tool seeds it with a TODO and then preserves
whatever the author writes there across regenerations.

Two subcommands:

    write   create or update PROVENANCE.json (keeps existing `source` text)
    check   verify every recorded checksum still matches; exit 1 if not

`check` is the one worth wiring into a build: a CSV edited without
regenerating provenance means the published checksum no longer describes the
published file.

Usage:  uv run python tools/viz-provenance.py {write,check} [PATH ...]
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import sitelib  # noqa: E402

SITE_ROOT = sitelib.SITE_ROOT
MANIFEST = "PROVENANCE.json"
SOURCE_TODO = "TODO: the command or run that produced this"


def data_dirs(paths: list[str]) -> list[Path]:
    """Every `figures/data/` directory under content/, or those given."""
    if paths:
        return [Path(p).resolve() for p in paths]
    return sorted(
        d for d in (SITE_ROOT / "content").rglob("figures/data") if d.is_dir()
    )


def digest(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as fh:
        for chunk in iter(lambda: fh.read(65536), b""):
            h.update(chunk)
    return h.hexdigest()


def row_count(path: Path) -> int | None:
    try:
        with path.open(newline="") as fh:
            return sum(1 for _ in csv.DictReader(fh))
    except (UnicodeDecodeError, csv.Error):
        return None


def describe(path: Path) -> dict:
    entry = {"sha256": digest(path), "bytes": path.stat().st_size}
    rows = row_count(path)
    if rows is not None:
        entry["rows"] = rows
    return entry


def load(manifest: Path) -> dict:
    if not manifest.is_file():
        return {}
    try:
        return json.loads(manifest.read_text())
    except json.JSONDecodeError:
        return {}


def cmd_write(dirs: list[Path]) -> int:
    for d in dirs:
        manifest = d / MANIFEST
        previous = load(manifest).get("files", {})
        files = {}
        for csv_path in sorted(d.glob("*.csv")):
            entry = describe(csv_path)
            # Carry the author's own note forward; never overwrite it.
            entry["source"] = previous.get(csv_path.name, {}).get(
                "source", SOURCE_TODO
            )
            files[csv_path.name] = entry
        if not files:
            continue
        manifest.write_text(
            json.dumps(
                {"generated_by": "tools/viz-provenance.py", "files": files},
                indent=2,
                sort_keys=True,
            )
            + "\n"
        )
        todo = sum(1 for e in files.values() if e["source"] == SOURCE_TODO)
        rel = os.path.relpath(manifest, SITE_ROOT)
        note = f"  ({todo} still needing a source note)" if todo else ""
        print(f"wrote {rel}: {len(files)} file(s){note}")
    return 0


def cmd_check(dirs: list[Path]) -> int:
    problems = 0
    for d in dirs:
        manifest = d / MANIFEST
        rel = os.path.relpath(d, SITE_ROOT)
        if not manifest.is_file():
            print(f"{rel}: no {MANIFEST} — run `make viz-provenance`")
            problems += 1
            continue
        recorded = load(manifest).get("files", {})
        present = {p.name for p in d.glob("*.csv")}
        for name in sorted(present - set(recorded)):
            print(f"{rel}/{name}: not recorded in {MANIFEST}")
            problems += 1
        for name in sorted(set(recorded) - present):
            print(f"{rel}/{name}: recorded but missing from disk")
            problems += 1
        for name in sorted(present & set(recorded)):
            actual = digest(d / name)
            if actual != recorded[name].get("sha256"):
                print(
                    f"{rel}/{name}: checksum changed — the published "
                    f"provenance no longer describes this file"
                )
                problems += 1
    print(f"\nviz-provenance: {len(dirs)} directory(ies), {problems} problem(s)")
    return 1 if problems else 0


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("command", choices=("write", "check"))
    ap.add_argument("paths", nargs="*", help="figures/data dirs (default: all)")
    args = ap.parse_args()

    dirs = data_dirs(args.paths)
    if not dirs:
        print("viz-provenance: no figures/data directories found")
        return 0
    return cmd_write(dirs) if args.command == "write" else cmd_check(dirs)


if __name__ == "__main__":
    sys.exit(main())
