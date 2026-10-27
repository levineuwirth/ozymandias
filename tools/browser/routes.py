"""The site's pages the browser harness and the engine's browser tests visit:
the site's tests/routes.yaml, read and checked once, for lib.py here and for
engine/tests/target.py.

`pages` names every page the sweeps load, in order; the other lists name the
pages one run visits, `probes` the pages each CSP-sweep interaction runs on,
and `roles` the page a module needs for one thing. An entry in a list or a
role is a page's name or a path ("/..."); a probe's is a name, because the
sweep records its results by name.
"""

from __future__ import annotations

from pathlib import Path

import yaml

ROUTES_FILE = "tests/routes.yaml"
LISTS = ("keyboard", "motion", "perf", "axe-skip", "popup-sweep", "footer", "layout-shift")


class Routes:
    def __init__(self, site: Path):
        path = Path(site) / ROUTES_FILE
        document = yaml.safe_load(path.read_text(encoding="utf-8")) or {}
        unknown = set(document) - {"pages", "probes", "roles", *LISTS}
        if unknown:
            raise ValueError(f"{ROUTES_FILE}: unknown {sorted(unknown)}")
        self.pages: dict[str, str] = dict(document.get("pages") or {})
        self.lists = {key: list(document.get(key) or []) for key in LISTS}
        self.probes: dict[str, list[str]] = dict(document.get("probes") or {})
        self.roles: dict[str, str] = dict(document.get("roles") or {})
        problems = [f"page {name!r}: {path!r}" for name, path in self.pages.items()
                    if not (isinstance(name, str) and isinstance(path, str) and path.startswith("/"))]
        for where, entries in [*self.lists.items(), *(("role " + k, [v]) for k, v in self.roles.items())]:
            problems += [f"{where}: {e!r}" for e in entries if not self.known(e)]
        for kind, names in self.probes.items():
            problems += [f"probe {kind}: {n!r}" for n in names if n not in self.pages]
        if problems:
            raise ValueError(f"{ROUTES_FILE}: no such page: {', '.join(problems)}")

    def known(self, entry) -> bool:
        return isinstance(entry, str) and (entry.startswith("/") or entry in self.pages)

    def path(self, entry: str) -> str:
        """A page's path, by its name or as given."""
        return entry if entry.startswith("/") else self.pages[entry]

    def names(self, key: str) -> list[str]:
        """A run's pages, as listed (names or paths)."""
        return self.lists[key]

    def probe(self, kind: str) -> list[str]:
        """The pages a CSP-sweep interaction runs on, by name."""
        return self.probes.get(kind, [])

    def role(self, role: str) -> str:
        """The path of the page a module needs for `role`."""
        return self.path(self.roles[role])
