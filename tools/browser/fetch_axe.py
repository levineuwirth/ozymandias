#!/usr/bin/env python3
"""Fetch axe-core for the accessibility tests: the version and SHA-256 that
tools/browser/axe-version records, to tools/browser/axe.min.js (gitignored).

Does nothing when the file there already has that checksum; otherwise
downloads dist/axe.min.js from jsDelivr, refuses it unless the checksum
matches, and replaces the file. Run by `make test-browser`.

    python3 tools/browser/fetch_axe.py
"""

from __future__ import annotations

import hashlib
import sys
import urllib.request
from pathlib import Path

# sitelib, in the engine's tools/ above this harness, whatever SITE_ROOT
# and the working directory are.
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import sitelib  # noqa: E402

# Vendored: axe-core goes beside the record it is checked against, wherever
# A6 has put that.
VERSION_FILE = sitelib.engine_path("tools/browser/axe-version")
TARGET = VERSION_FILE.parent / "axe.min.js"
URL = "https://cdn.jsdelivr.net/npm/axe-core@{version}/axe.min.js"


def recorded() -> tuple[str, str]:
    """(version, sha256) from axe-version: its one line that is not a comment."""
    lines = [l.split() for l in VERSION_FILE.read_text(encoding="utf-8").splitlines()
             if l.strip() and not l.lstrip().startswith("#")]
    if len(lines) != 1 or len(lines[0]) != 2:
        raise SystemExit(f"fetch_axe: {VERSION_FILE.name} must hold one line: <version> <sha256>")
    return lines[0][0], lines[0][1].lower()


def sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def main() -> int:
    version, want = recorded()
    if TARGET.is_file() and sha256(TARGET.read_bytes()) == want:
        return 0
    url = URL.format(version=version)
    with urllib.request.urlopen(url, timeout=60) as response:
        data = response.read()
    got = sha256(data)
    if got != want:
        print(f"fetch_axe: {url} has SHA-256 {got}, not the recorded {want}; not installed",
              file=sys.stderr)
        return 1
    tmp = TARGET.with_suffix(".js.tmp")
    tmp.write_bytes(data)
    tmp.replace(TARGET)
    print(f"fetch_axe: axe-core {version} installed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
