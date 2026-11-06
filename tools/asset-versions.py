#!/usr/bin/env python3
"""The versions of a site's first-party JS and CSS (/assets/<hash>/, written by
the generator: EngineFiles.staticRules), across builds and deploys.

A page names the version it was built with, and a reader may hold that page
long after the next deploy: in a cache, or in a tab left open, whose scripts
go on loading more of their own version on demand. So a deploy must leave
earlier versions on the server, which rsync --delete would remove.

    asset-versions.py current [SITE]    the version SITE (_site) was built with
    asset-versions.py prune [SITE]      remove the others from SITE
    asset-versions.py record [SITE]     SITE's version goes live today
    asset-versions.py retained [SITE]   what else to keep on the server, a path each
    asset-versions.py filter [SITE]     the same, as rsync filter rules

data/deployed-assets.json holds the version live on the server and the day each
earlier one was replaced. An earlier version is kept for KEEP_DAYS from that
day, however long it was live. The paths assets lived at before they were
versioned (LEGACY), which tabs open across the first such deploy still load
from, are kept the same way from that deploy.

A deploy records before its transfer. If the transfer then fails, a version
that never arrived is kept for nothing and a clock starts early; the other order
could lose a version that had arrived, at the next deploy.

The ledger is local, as a deploy is: lost, the server's earlier versions go at
the next deploy, and tabs that old lose their on-demand loads. It costs no extra
connection to the server: everything goes through the deploy's one rsync.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import re
import shutil
import sys
from pathlib import Path

KEEP_DAYS = 90
LEGACY = ("js", "css")
LEDGER = Path("data/deployed-assets.json")
META = re.compile(r'<meta name="ozy:assets" content="/assets/([0-9a-f]+)">')


def current(site: Path) -> str:
    """The version SITE's home page names."""
    found = META.search((site / "index.html").read_text(encoding="utf-8"))
    if not found:
        raise SystemExit(f"asset-versions: {site}/index.html names no version")
    return found.group(1)


def prune(site: Path) -> list[str]:
    """Remove from SITE what a build no longer publishes: the generator never
    deletes an output, and the deploy decides what stays on the server."""
    keep = current(site)
    gone = [p for p in sorted((site / "assets").iterdir()) if p.name != keep]
    gone += [site / name for name in LEGACY if (site / name).exists()]
    for path in gone:
        shutil.rmtree(path)
    return [str(path.relative_to(site)) for path in gone]


def load(ledger: Path) -> dict:
    if not ledger.is_file():
        return {"live": None, "replaced": {}, "legacy": None}
    return json.loads(ledger.read_text(encoding="utf-8"))


def fresh(day: str, today: dt.date) -> bool:
    return (today - dt.date.fromisoformat(day)).days <= KEEP_DAYS


def retained(ledger: Path, today: dt.date, building: str) -> list[str]:
    """Paths on the server to keep beyond the version `building`. Before the
    first deploy that records a version, the legacy paths are still in use."""
    state = load(ledger)
    versions = {version for version, day in state["replaced"].items() if fresh(day, today)}
    if state["live"]:
        versions.add(state["live"])
    paths = [f"assets/{version}" for version in sorted(versions - {building})]
    if state["legacy"] is None or fresh(state["legacy"], today):
        paths += list(LEGACY)
    return paths


def record(ledger: Path, site: Path, today: dt.date) -> None:
    """SITE's version goes live today, which is the day the one before it was
    replaced. What no longer stays leaves the ledger."""
    state = load(ledger)
    now = current(site)
    day = today.isoformat()
    if state["live"] and state["live"] != now:
        state["replaced"][state["live"]] = day
    state["replaced"] = {v: d for v, d in state["replaced"].items() if v != now and fresh(d, today)}
    state["live"] = now
    state["legacy"] = state["legacy"] or day
    ledger.parent.mkdir(parents=True, exist_ok=True)
    ledger.write_text(json.dumps(state, indent=1, sort_keys=True) + "\n", encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("command", choices=("current", "prune", "record", "retained", "filter"))
    parser.add_argument("site", nargs="?", type=Path, default=Path("_site"))
    parser.add_argument("--ledger", type=Path, default=LEDGER)
    parser.add_argument("--today", type=dt.date.fromisoformat, default=dt.date.today())
    args = parser.parse_args()
    if args.command == "current":
        print(current(args.site))
    elif args.command == "prune":
        for path in prune(args.site):
            print(f"asset-versions: removed {path} from {args.site}")
    elif args.command == "record":
        record(args.ledger, args.site, args.today)
    else:
        for path in retained(args.ledger, args.today, current(args.site)):
            print(path if args.command == "retained" else f"P /{path}/***")
    return 0


if __name__ == "__main__":
    sys.exit(main())
