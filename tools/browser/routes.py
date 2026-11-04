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
has off, and `misnamed` which named pages are not of their kind, and of it
alone.
"""

from __future__ import annotations

import re
from pathlib import Path, PurePosixPath
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
    """A kind of page, as the generator builds it. A page is of this kind if
    its path fits `route` (a regular expression, whole) and it carries `marker`
    from its template; `page` is what is left over (`Routes.kinds_of`). A site
    has the kind if its switch is on and, where a site may have none, a
    published source fits `content` less `exclude` (Patterns.hs's patterns;
    test_route_check holds them to the generator) and sets one of `declares`, or
    site.yaml has a standalone page of `context`, whose route is then the
    kind's."""
    switch: str | None
    route: str | None = None
    marker: str | None = None
    content: tuple[str, ...] = ()
    exclude: tuple[str, ...] = ()
    context: str | None = None
    declares: tuple[str, ...] = ()


def writing(section: str) -> dict:
    """Patterns.hs's poetry, fiction and blog: flat entries, and entries in a
    collection less its landing."""
    return {"content": (f"content/{section}/*.md", f"content/{section}/*/*.md"),
            "exclude": (f"content/{section}/*/index.md",)}


ESSAYS = ("content/essays/*.md", "content/essays/*/*.md")
FRAMES = {"content": ("content/photography/*.md", "content/photography/*/*.md"),
          "exclude": ("content/photography/index.md",)}
# Paths of the engine's own, which a tag cannot have: a generated index, or a
# section's page.
NOT_A_TAG = r"(?!(?:authors|archive|bibliography|music|photography/by-year|photography/map|photography/contact-sheet|stats|build)/)"

KINDS = {
    "home": Kind(None, r"/"),
    "essay": Kind(None, r"/(?!stats/$|build/$).*", 'class="essay-frontmatter"', ESSAYS),
    "page": Kind(None),
    "section-index": Kind(None, r"/(?:essays|poetry|fiction|blog)/"),
    "poem": Kind(None, None, 'class="reading-mode poetry"', **writing("poetry")),
    "story": Kind(None, None, 'class="reading-mode fiction"', **writing("fiction")),
    "post": Kind(None, None, "essay-frontmatter--blog", **writing("blog")),
    "collection": Kind(None, None, 'class="collection-contents"',
                       ("content/poetry/*/index.md", "content/fiction/*/index.md")),
    "search": Kind(None, None, 'id="search"', ("content/search.md",)),
    "not-found": Kind(None),
    "new": Kind("new", r"/new\.html", 'class="item-card-list"'),
    "tag": Kind("tag-pages", rf"/{NOT_A_TAG}.+/", 'class="item-card-list"', ESSAYS + FRAMES["content"]),
    "author": Kind("authors", r"/authors/[^/]+/", 'class="essay-list"',
                   ESSAYS + writing("blog")["content"], writing("blog")["exclude"]),
    "library": Kind("library", r"/library\.html", "library-see-also"),
    "bibliography": Kind("bibliography-index", r"/bibliography/", "library-see-also"),
    "keyword": Kind("bibliography-index", r"/bibliography/[^/]+/", None, ("_site/bibliography/*/index.html",)),
    "photo-index": Kind("photography", r"/photography/", 'class="photography-grid"'),
    "photo-series": Kind("photography", None, "photo-series-frontmatter", ("content/photography/*/*.md",),
                         ("content/photography/*/index.md",)),
    "photo-single": Kind("photography", None, 'class="photo-entry"', **FRAMES),
    "photo-map": Kind("photography", r"/photography/map/", 'id="photography-map"'),
    "photo-contact": Kind("photography", r"/photography/contact-sheet/", 'class="photography-contact-sheet"'),
    "photo-years": Kind("photography", r"/photography/by-year/", "photography-by-year-back"),
    "photo-year": Kind("photography", r"/photography/by-year/[0-9]+/", "photography-by-year-back", **FRAMES),
    "music-index": Kind("music", r"/music/", 'class="music-index"', ("content/music/index.md",)),
    "composition": Kind("music", None, 'class="composition"', ("content/music/*/index.md",)),
    "score-reader": Kind("music", r"/music/[^/]+/score/", 'id="score-reader-bar"', ("content/music/*/index.md",),
                         declares=("score-pages", "score-dir")),
    "archive-index": Kind("archive", r"/archive/", "archive-index-header"),
    "archive-entry": Kind("archive", None, 'class="archive-entry"', ("archive/*/PROVENANCE.json",)),
    "commonplace": Kind("commonplace", r"/commonplace\.html", "cp-view-toggle", ("content/commonplace.md",)),
    "now": Kind("now", None, "now-stamp", context="now"),
    "vita": Kind("vita", context="vita"),
    "stats": Kind("stats-page", r"/stats/", 'id="activity"'),
    "build": Kind("build-page", r"/build/", "Build Telemetry"),
}
# Withholding a directory-form entry withholds its directory (build/Drafts.hs).
DIRECTORY_ENTRIES = ("essays", "music", "photography")
FRONT_MATTER = re.compile(r"\A-{3,}[ \t]*\n(.*?)\n-{3,}[ \t]*(?:\n|\Z)", re.S)


def front_matter(path: Path) -> dict:
    """A Markdown file's front matter, or nothing."""
    if path.suffix != ".md" or not path.is_file():
        return {}
    found = FRONT_MATTER.match(path.read_text(encoding="utf-8", errors="replace"))
    try:
        meta = yaml.safe_load(found.group(1)) if found else None
    except yaml.YAMLError:
        return {}
    return meta if isinstance(meta, dict) else {}


def is_draft(path: Path) -> bool:
    """Whether a Markdown file has draft: true, as Drafts.draftFlag reads it."""
    flag = front_matter(path).get("draft")
    return flag is True or flag == 1 or (isinstance(flag, str) and flag.strip().lower() in ("true", "yes", "on"))


def published(site: Path, rel: Path) -> bool:
    """Whether `rel`, under `site`, is left to publish by a production build."""
    if is_draft(site / rel):
        return False
    parts = rel.parts
    return not (len(parts) == 4 and parts[0] == "content" and parts[1] in DIRECTORY_ENTRIES
                and is_draft(site / "content" / parts[1] / parts[2] / "index.md"))


class Routes:
    def __init__(self, site: Path):
        self.site = Path(site)
        path = self.site / ROUTES_FILE
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

    def standalone(self) -> dict[str, list[str]]:
        """site.yaml's standalone pages' paths, by their context."""
        config = yaml.safe_load((self.site / "site.yaml").read_text(encoding="utf-8")) or {}
        found: dict[str, list[str]] = {}
        for page in config.get("standalone") or []:
            found.setdefault(page["context"], []).append("/" + page["route"].removesuffix("index.html"))
        return found

    def has(self, name: str, switches: dict[str, bool]) -> bool:
        """Whether the site has pages of this kind."""
        kind = KINDS[name]
        if kind.switch is not None and not switches[kind.switch]:
            return False
        if kind.context:
            return kind.context in self.standalone()
        return not kind.content or any(
            published(self.site, rel) and not any(PurePosixPath(rel).match(glob) for glob in kind.exclude)
            and (not kind.declares or any(front_matter(self.site / rel).get(key) for key in kind.declares))
            for glob in kind.content
            for rel in (p.relative_to(self.site) for p in self.site.glob(glob)))

    def fits_path(self, name: str, path: str) -> bool:
        """Whether `path` can be a page of this kind, before looking at one."""
        kind = KINDS[name]
        return ((kind.route is None or re.fullmatch(kind.route, path) is not None)
                and (kind.context is None or path in self.standalone().get(kind.context, [])))

    def kinds_of(self, path: str, html: str) -> set[str]:
        """Every kind a built page fits: all of a kind's route, context and
        mark. `page` is what is left over, for page.html leaves no mark of its
        own but the title every page.html and many others have."""
        found = {name for name, kind in KINDS.items() if name not in ("page", "not-found")
                 and self.fits_path(name, path) and (kind.marker is None or kind.marker in html)
                 and (kind.route or kind.context or kind.marker)}
        if not found and 'class="page-title"' in html:
            found.add("page")
        return found

    def uncovered(self, switches: dict[str, bool], shared: bool = False) -> list[str]:
        """What is wrong with `kinds` for a site with `switches`: a kind it has
        that names no page, and (unless these routes are `shared`, the fixture's
        between its profiles) one it names but has off."""
        problems = []
        for name, kind in KINDS.items():
            if self.has(name, switches) and name not in self.kinds:
                problems.append(f"{name}: the site has this kind of page, and tests/routes.yaml's kinds names none")
            if (not shared and kind.switch is not None and not switches[kind.switch] and name in self.kinds):
                problems.append(f"{name}: named, but the {kind.switch} switch is off")
        return problems

    def misnamed(self, built, switches: dict[str, bool]) -> list[str]:
        """Each named page of a kind the site has on that is not of its kind,
        and of it alone. `built(path)` is the file a URL path serves; without
        one (no build) only its path can be judged."""
        problems = []
        for name, page in self.kinds.items():
            kind = KINDS[name]
            if kind.switch is not None and not switches[kind.switch]:
                continue
            path, file = self.pages[page], built(self.pages[page])
            if name == "not-found":
                if path != self.roles.get("not-found") and page != self.roles.get("not-found") or file.is_file():
                    problems.append(f"not-found: {path} is not the role's path that no page has")
            elif file.is_file():
                found = self.kinds_of(path, file.read_text(encoding="utf-8"))
                if found != {name}:
                    problems.append(f"{name}: {path} is {sorted(found) or 'no kind'}")
            elif not self.fits_path(name, path):
                problems.append(f"{name}: {path} cannot be one")
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
