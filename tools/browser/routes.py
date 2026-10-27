"""The site's pages the browser harness and the engine's browser tests visit:
the site's tests/routes.yaml, read and checked once, for lib.py here and for
engine/tests/target.py.

`pages` names every page the sweeps load, in order; the other lists name the
pages one run visits, `probes` the pages each CSP-sweep interaction runs on,
and `roles` the page a module needs for one thing. The sweeps record their
results by page name, so their lists (keyboard, motion, perf, axe-skip) and
the probes name pages; the manual scripts' lists and the roles may also
give a path ("/..."). A score role's page is a directory: the work's score
reader is at <page>score/.
"""

from __future__ import annotations

from pathlib import Path

import yaml

ROUTES_FILE = "tests/routes.yaml"
NAMED = ("keyboard", "motion", "perf", "axe-skip")
ANY = ("popup-sweep", "footer", "layout-shift")
LISTS = NAMED + ANY
# The CSP sweep's interactions (csp_run.features) and checks (test_browser_csp).
PROBES = ("popups", "search", "map", "slideshow", "lightbox", "score", "pdf-viewer",
          "archive-snapshot", "archive-pdf", "pdf-links", "composition", "transclusion",
          "commonplace", "math")
DIRECTORIES = ("score", "score-other", "score-silent")


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
        problems = [f"page {name!r} is not a path: {path!r}" for name, path in self.pages.items()
                    if not (isinstance(name, str) and isinstance(path, str) and path.startswith("/"))]
        problems += [f"no such probe {kind!r}" for kind in self.probes if kind not in PROBES]
        for key in LISTS:
            problems += [f"{key}: no such page {e!r}" for e in self.lists[key]
                         if not (e in self.pages if key in NAMED else self.known(e))]
        for kind, names in self.probes.items():
            problems += [f"probe {kind}: no such page {n!r}" for n in names if n not in self.pages]
        for role, entry in self.roles.items():
            if not self.known(entry):
                problems.append(f"role {role}: no such page {entry!r}")
            elif role in DIRECTORIES and not self.path(entry).endswith("/"):
                problems.append(f"role {role}: {self.path(entry)} must end in / (its score is <page>score/)")
        if problems:
            raise ValueError(f"{ROUTES_FILE}: {'; '.join(problems)}")

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
