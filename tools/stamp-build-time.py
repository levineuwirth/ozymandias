#!/usr/bin/env python3
"""Write the site-wide build time to _site/build/time.txt.

Every page's footer shows when the site was last built, the same text on
every page. static/js/nav.js reads it from this one file into the empty
<span data-build-time> that templates/partials/footer.html ships.

It used to be stamped into every page: Hakyll renders a page only when its
dependencies change, so a time rendered at compile time froze on reused
pages, and this script rewrote the span in every HTML file after each
build instead. That made every build rewrite and recompress ~500 pages
whose content had not changed, and every deploy re-sign and re-send them
with their compressed copies (boot profile, 2026-10-06). Pages now stay
byte-identical until their content changes.

Format: "Monday, October 6th, 2026 13:03:33", UTC, as the footers showed
it before.
"""
from __future__ import annotations

import sys
from datetime import datetime, timezone
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import sitelib  # noqa: E402


def ordinal_suffix(day: int) -> str:
    if 11 <= day <= 13:
        return "th"
    return {1: "st", 2: "nd", 3: "rd"}.get(day % 10, "th")


def format_time(now: datetime) -> str:
    return (
        f"{now.strftime('%A, %B')} "
        f"{now.day}{ordinal_suffix(now.day)}, "
        f"{now.strftime('%Y %H:%M:%S')}"
    )


def main(root: str) -> int:
    site = Path(root)
    if not site.is_dir():
        print(f"stamp-build-time: {root} not found", file=sys.stderr)
        return 1
    target = site / "build" / "time.txt"
    target.parent.mkdir(parents=True, exist_ok=True)
    stamp = format_time(datetime.now(timezone.utc))
    sitelib.atomic_write_text(target, stamp + "\n")
    print(f"stamp-build-time: {stamp} -> {target}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1] if len(sys.argv) > 1 else "_site"))
