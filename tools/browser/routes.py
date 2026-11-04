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

`kinds` names, from `pages`, one page of each kind the engine builds (KINDS), so
that axe's and the other sweeps' coverage cannot shrink unnoticed on a small
site: `uncovered` says which kinds a site has and does not name, or names and
has off, and `misnamed` which named pages are not of their kind.
"""

from __future__ import annotations

from pathlib import Path
from typing import NamedTuple

import yaml

ROUTES_FILE = "tests/routes.yaml"
NAMED = ("keyboard", "motion", "perf", "axe-skip", "dropcaps")
ANY = ("popup-sweep", "footer", "layout-shift")
LISTS = NAMED + ANY
# The CSP sweep's interactions (csp_run.features) and checks (test_browser_csp).
PROBES = ("popups", "search", "map", "slideshow", "lightbox", "score", "pdf-viewer",
          "archive-snapshot", "archive-pdf", "pdf-links", "composition", "transclusion",
          "commonplace", "math")
DIRECTORIES = ("score", "score-other", "score-silent")


class Kind(NamedTuple):
    switch: str | None          # the switch that builds it; None, every site's
    marker: str | None = None   # what its template puts on every such page
    content: tuple[str, ...] = ()   # globs: a site has it only if one matches


KINDS = {
    "home": Kind(None),
    "essay": Kind(None, 'class="essay-frontmatter"'),
    "page": Kind(None),
    "section-index": Kind(None, 'class="essay-list"'),
    "poem": Kind(None, 'class="reading-mode poetry"', ("content/poetry/*.md",)),
    "story": Kind(None, 'class="reading-mode fiction"', ("content/fiction/*.md",)),
    "post": Kind(None, "essay-frontmatter--blog", ("content/blog/*.md",)),
    "search": Kind(None, 'id="search"'),
    "not-found": Kind(None),
    "new": Kind("new", 'class="item-card-list"'),
    "tag": Kind("tag-pages", 'class="item-card-list"'),
    "author": Kind("authors", 'class="essay-list"'),
    "library": Kind("library", "library-see-also"),
    "bibliography": Kind("bibliography-index", "library-see-also"),
    "keyword": Kind("bibliography-index", 'class="see-also"', ("_site/bibliography/*/index.html",)),
    "photo-index": Kind("photography", 'class="photography-grid"'),
    "photo-series": Kind("photography", "photo-series-frontmatter"),
    "photo-single": Kind("photography", 'class="photo-entry"'),
    "photo-map": Kind("photography", 'id="photography-map"'),
    "photo-contact": Kind("photography", 'class="photography-contact-sheet"'),
    "photo-years": Kind("photography", "photography-by-year-back"),
    "photo-year": Kind("photography", "photography-by-year-back"),
    "music-index": Kind("music", 'class="music-index"'),
    "composition": Kind("music", 'class="composition"'),
    "score-reader": Kind("music", 'id="score-reader-bar"'),
    "archive-index": Kind("archive", "archive-index-header"),
    "archive-entry": Kind("archive", 'class="archive-entry"'),
    "commonplace": Kind("commonplace", "cp-view-toggle"),
    "now": Kind("now", "now-stamp"),
    "vita": Kind("vita"),
    "stats": Kind("stats-page", 'id="activity"'),
    "build": Kind("build-page", "Build Telemetry"),
}


class Routes:
    def __init__(self, site: Path):
        path = Path(site) / ROUTES_FILE
        document = yaml.safe_load(path.read_text(encoding="utf-8")) or {}
        unknown = set(document) - {"pages", "kinds", "probes", "roles", *LISTS}
        if unknown:
            raise ValueError(f"{ROUTES_FILE}: unknown {sorted(unknown)}")
        self.pages: dict[str, str] = dict(document.get("pages") or {})
        self.lists = {key: list(document.get(key) or []) for key in LISTS}
        self.probes: dict[str, list[str]] = dict(document.get("probes") or {})
        self.roles: dict[str, str] = dict(document.get("roles") or {})
        self.kinds: dict[str, str] = dict(document.get("kinds") or {})
        problems = [f"page {name!r} is not a path: {path!r}" for name, path in self.pages.items()
                    if not (isinstance(name, str) and isinstance(path, str) and path.startswith("/"))]
        problems += [f"no such probe {kind!r}" for kind in self.probes if kind not in PROBES]
        problems += [f"no such kind {kind!r}" for kind in self.kinds if kind not in KINDS]
        problems += [f"kind {kind}: no such page {name!r}" for kind, name in self.kinds.items()
                     if name not in self.pages]
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

    def uncovered(self, site: Path, switches: dict[str, bool], shared: bool = False) -> list[str]:
        """What is wrong with `kinds` for a site with `switches`: a kind it has
        that names no page, and (unless these routes are `shared`, the fixture's
        between its profiles) one it names but has off."""
        def has(kind: Kind) -> bool:
            return not kind.content or any(any(Path(site).glob(glob)) for glob in kind.content)

        problems = []
        for name, kind in KINDS.items():
            on = kind.switch is None or switches[kind.switch]
            if on and has(kind) and name not in self.kinds:
                problems.append(f"{name}: the site has this kind of page, and tests/routes.yaml's kinds names none")
            if not on and name in self.kinds and not shared:
                problems.append(f"{name}: named, but the {kind.switch} switch is off")
        return problems

    def misnamed(self, built, switches: dict[str, bool]) -> list[str]:
        """Each named page of a kind the site has on that is not of its kind.
        `built(path)` is the file a URL path serves; one not there is passed
        over (no build to look at)."""
        problems = []
        for name, page in self.kinds.items():
            kind = KINDS[name]
            if kind.marker and (kind.switch is None or switches[kind.switch]):
                file = built(self.pages[page])
                if file.is_file() and kind.marker not in file.read_text(encoding="utf-8"):
                    problems.append(f"{name}: {self.pages[page]} has no {kind.marker}")
        return problems

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
