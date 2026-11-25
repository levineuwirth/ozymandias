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

A role may be null and a probe an empty list: the site has no such page (a
site without essays has no page for `essay`), and the test that needs one is
skipped. A site names every role the engine asks for (ROLES), so one left
out is a mistake, and fails (`role_problems`): a site says what it lacks.
The 404 page it always has. A probe whose feature shows on a kind of page
(PROBE_KINDS) names a page wherever the site has that kind.

`kinds` names, from `pages`, one page of each kind the engine builds (KINDS), so
that axe's and the other sweeps' coverage cannot shrink unnoticed on a small
site: `uncovered` says which kinds a site has and does not name, or names and
has off, and `misnamed` which named pages are not of their kind, and of it
alone.
"""

from __future__ import annotations

import functools
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
# The pages the engine's browser tests and scripts each need for one thing
# (roles), and those no site is without.
ROLES = ("not-found", "essay", "essay-peer-reviewed", "essay-gallery", "essay-figure", "essay-sidenotes",
         "essay-footnotes", "photo", "photo-series", "photo-series-slideshow", "list", "list-other",
         "score", "score-other", "score-silent", "now")
REQUIRED_ROLES = ("not-found",)
# A probe whose feature every site has, and the kind of page it shows on: a
# site with pages of that kind names one for it, and [] only without.
PROBE_KINDS = {"search": "search"}


class NoSuchPage(LookupError):
    """The site's routes.yaml says it has no page for this role (null)."""


class Kind(NamedTuple):
    """A kind of page, as the generator builds it. A page is of this kind if
    its path fits `route` (a regular expression, whole) and it carries `marker`
    from its template, and no kind the site has claims the path (`Routes.claims`);
    `page` is what is left over (`Routes.kinds_of`). A site
    has the kind if its switch is on and, where a site may have none, a
    published source fits `content` less `exclude` (Patterns.hs's patterns;
    test_route_check holds them to the generator) and sets one of `declares`, or
    site.yaml has a standalone page of `context`, whose route is then the
    kind's. A kind `routed_by_source` has the paths its sources route to
    (`Routes.source_paths`) in place of a marker: a photograph's or a
    series' page, whose template a site may override (contract.yaml), so
    that no styling class of the engine's says what it is."""
    switch: str | None
    route: str | None = None
    marker: str | None = None
    content: tuple[str, ...] = ()
    exclude: tuple[str, ...] = ()
    context: str | None = None
    declares: tuple[str, ...] = ()
    routed_by_source: bool = False


def writing(section: str) -> dict:
    """Patterns.hs's poetry, fiction and blog: flat entries, and entries in a
    collection less its landing."""
    return {"content": (f"content/{section}/*.md", f"content/{section}/*/*.md"),
            "exclude": (f"content/{section}/*/index.md",)}


ESSAYS = ("content/essays/*.md", "content/essays/*/*.md")
FRAMES = {"content": ("content/photography/*.md", "content/photography/*/*.md"),
          "exclude": ("content/photography/index.md",)}
KINDS = {
    "home": Kind(None, r"/"),
    "essay": Kind(None, None, 'class="essay-frontmatter"', ESSAYS),
    "page": Kind(None),
    # A section of writing's landing, at the section's address: the route
    # here is for a site that serves each at its directory's name, and
    # Routes.route gives the site's own (site.yaml's sections).
    "section-index": Kind(None, r"/(?:essays|poetry|fiction|blog)/"),
    "poem": Kind(None, None, 'class="reading-mode poetry"', **writing("poetry")),
    "story": Kind(None, None, 'class="reading-mode fiction"', **writing("fiction")),
    "post": Kind(None, None, "essay-frontmatter--blog", **writing("blog")),
    "collection": Kind(None, None, 'class="collection-contents"',
                       ("content/poetry/*/index.md", "content/fiction/*/index.md")),
    "search": Kind(None, None, 'id="search"', ("content/search.md",)),
    "not-found": Kind(None),
    "new": Kind("new", r"/new\.html", 'class="item-card-list"'),
    # Every tag page has its head; a short one shows its pieces in full,
    # without a card list.
    "tag": Kind("tag-pages", r"/.+/", 'class="tag-head"', ESSAYS + FRAMES["content"]),
    "author": Kind("authors", r"/authors/[^/]+/", 'class="essay-list"',
                   ESSAYS + writing("blog")["content"], writing("blog")["exclude"]),
    "library": Kind("library", r"/library\.html", "library-see-also"),
    "bibliography": Kind("bibliography-index", r"/bibliography/", "library-see-also"),
    "keyword": Kind("bibliography-index", r"/bibliography/[^/]+/", None, ("_site/bibliography/*/index.html",)),
    # Photography's templates are a site's to override: its pages are known
    # by their routes, and a photograph's or a series' by its source.
    "photo-index": Kind("photography", r"/photography/"),
    "photo-series": Kind("photography", None, None, ("content/photography/*/*.md",),
                         ("content/photography/*/index.md",), routed_by_source=True),
    "photo-single": Kind("photography", None, None, **FRAMES, routed_by_source=True),
    "photo-map": Kind("photography", r"/photography/map/"),
    "photo-contact": Kind("photography", r"/photography/contact-sheet/"),
    "photo-years": Kind("photography", r"/photography/by-year/"),
    "photo-year": Kind("photography", r"/photography/by-year/[0-9]+/", None, **FRAMES),
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
    # The whole tag map (Tags.tagMapRules); a tag page's own map is part of
    # the tag kind.
    "tag-map": Kind("tag-map", r"/map/", "tag-map-head"),
}
# A tag may have any directory path nothing else claims: its route says nothing
# about it, as no route says nothing about a kind with none.
OPEN_ROUTE = ("tag",)
# Withholding a directory-form entry withholds its directory (build/Drafts.hs).
DIRECTORY_ENTRIES = ("essays", "music", "photography")
# The sections of writing (Config.writingDirs), which a site lists as it has
# them and may serve at an address of its own.
WRITING = ("essays", "blog", "poetry", "fiction")
NOWHERE = r"(?!)"
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
        self.roles: dict[str, str | None] = dict(document.get("roles") or {})
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
            if not isinstance(names, list):
                problems.append(f"probe {kind}: not a list of pages ([] if the site has none)")
                continue
            problems += [f"probe {kind}: no such page {n!r}" for n in names if n not in self.pages]
        for role, entry in self.roles.items():
            if entry is None:
                continue                    # the site has no such page
            if not self.known(entry):
                problems.append(f"role {role}: no such page {entry!r}")
            elif role in DIRECTORIES and not self.path(entry).endswith("/"):
                problems.append(f"role {role}: {self.path(entry)} must end in / (its score is <page>score/)")
        if problems:
            raise ValueError(f"{ROUTES_FILE}: {'; '.join(problems)}")

    @functools.cached_property
    def section_addresses(self) -> list[str]:
        """Where the sections of writing site.yaml lists are served: each
        at its `address`, or at its directory's name."""
        config = yaml.safe_load((self.site / "site.yaml").read_text(encoding="utf-8")) or {}
        return [section.get("address") or section["dir"] for section in config.get("sections") or []
                if section.get("dir") in WRITING]

    def route(self, name: str) -> str | None:
        """A kind's route on this site: the generator's, but a section
        index's, which is at a listed section's address and nowhere on a
        site that lists none."""
        if name != "section-index":
            return KINDS[name].route
        if not self.section_addresses:
            return NOWHERE
        return "/(?:%s)/" % "|".join(re.escape(address) for address in self.section_addresses)

    @functools.cached_property
    def standalone_paths(self) -> dict[str, list[str]]:
        """site.yaml's standalone pages' paths, by their context."""
        config = yaml.safe_load((self.site / "site.yaml").read_text(encoding="utf-8")) or {}
        found: dict[str, list[str]] = {}
        for page in config.get("standalone") or []:
            found.setdefault(page["context"], []).append("/" + page["route"].removesuffix("index.html"))
        return found

    @functools.cached_property
    def source_paths(self) -> dict[str, set[str]]:
        """The paths photography's published sources route to (Photography.hs):
        a flat photograph's <slug>.html, every frame's <directory>/<frame>/,
        whether or not its directory has an index.md, and a directory's
        index.md's <slug>/ (a series when frames are beside it, else a
        photograph)."""
        found: dict[str, set[str]] = {"photo-single": set(), "photo-series": set()}
        root = self.site / "content/photography"
        if not root.is_dir():
            return found
        def shown(md: Path) -> bool:
            return published(self.site, md.relative_to(self.site))
        for md in sorted(root.glob("*.md")):
            if md.name != "index.md" and shown(md):
                found["photo-single"].add(f"/photography/{md.stem}.html")
        framed = set()
        for md in sorted(root.glob("*/*.md")):
            if md.name != "index.md" and shown(md):
                found["photo-single"].add(f"/photography/{md.parent.name}/{md.stem}/")
                framed.add(md.parent.name)
        for index in sorted(root.glob("*/index.md")):
            if shown(index):
                slug = index.parent.name
                found["photo-series" if slug in framed else "photo-single"].add(f"/photography/{slug}/")
        return found

    def sourced(self, path: str, switches: dict[str, bool]) -> str | None:
        """The kind a photography source publishes at `path`, if one does:
        its route is its own, whatever markup a site's template gives it."""
        return next((name for name, kind in KINDS.items() if kind.routed_by_source
                     and (kind.switch is None or switches[kind.switch])
                     and path in self.source_paths.get(name, set())), None)

    def has(self, name: str, switches: dict[str, bool]) -> bool:
        """Whether the site has pages of this kind."""
        kind = KINDS[name]
        if kind.switch is not None and not switches[kind.switch]:
            return False
        if name == "section-index":
            return bool(self.section_addresses)
        if kind.context:
            return kind.context in self.standalone_paths
        if kind.routed_by_source:
            # What its sources publish: a frame with no index.md beside it
            # is a photograph, and makes no series.
            return bool(self.source_paths.get(name))
        return not kind.content or any(
            published(self.site, rel) and not any(PurePosixPath(rel).match(glob) for glob in kind.exclude)
            and (not kind.declares or any(front_matter(self.site / rel).get(key) for key in kind.declares))
            for glob in kind.content
            for rel in (p.relative_to(self.site) for p in self.site.glob(glob)))

    def claims(self, switches: dict[str, bool]) -> list[str]:
        """The kinds with a route or standalone context of their own that this
        site has. Only they keep a path from the other kinds: with stats-page
        off, /stats/ is free for a tag or a standalone essay (RouteCheck refuses
        two pages on one path)."""
        return [name for name, kind in KINDS.items()
                if (self.route(name) or kind.context) and name not in OPEN_ROUTE and self.has(name, switches)]

    def fits_path(self, name: str, path: str, claims: list[str]) -> bool:
        """Whether `path` can be a page of this kind, before looking at one,
        on a site whose `claims` these are."""
        def own(n: str) -> bool:
            k = KINDS[n]
            return ((self.route(n) is None or re.fullmatch(self.route(n), path) is not None)
                    and (k.context is None or path in self.standalone_paths.get(k.context, []))
                    and (not k.routed_by_source or path in self.source_paths.get(n, set())))

        if not own(name):
            return False
        return name in claims or not any(own(other) for other in claims)

    def kinds_of(self, path: str, html: str, switches: dict[str, bool]) -> set[str]:
        """Every kind a built page fits: its switch, route or context, and
        mark, on a path nothing the site has claims from it. `page` is what is
        left over, for page.html leaves no mark of its own but the title every
        page.html and many others have. A path a photography source
        publishes is that source's kind alone (`sourced`)."""
        if sourced := self.sourced(path, switches):
            return {sourced}
        claims = self.claims(switches)
        found = {name for name, kind in KINDS.items() if name not in ("page", "not-found")
                 and (kind.switch is None or switches[kind.switch])
                 and (kind.route or kind.context or kind.marker or kind.routed_by_source)
                 and self.fits_path(name, path, claims) and (kind.marker is None or kind.marker in html)}
        if not found and 'class="page-title"' in html and self.fits_path("page", path, claims):
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
        claims = self.claims(switches)
        for name, page in self.kinds.items():
            kind = KINDS[name]
            if kind.switch is not None and not switches[kind.switch]:
                continue
            path, file = self.pages[page], built(self.pages[page])
            if name == "not-found":
                if path != self.roles.get("not-found") and page != self.roles.get("not-found") or file.is_file():
                    problems.append(f"not-found: {path} is not the role's path that no page has")
            elif file.is_file():
                found = self.kinds_of(path, file.read_text(encoding="utf-8"), switches)
                if found != {name}:
                    problems.append(f"{name}: {path} is {sorted(found) or 'no kind'}")
            elif not self.fits_path(name, path, claims):
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

    def role_problems(self) -> list[str]:
        """What is wrong with `roles` as a whole: a role the engine asks for
        that is not named (a page, or null), one it does not know, and a
        role every site has that is null."""
        return ([f"role {role}: not named (its page, or null if the site has none)" for role in ROLES
                 if role not in self.roles]
                + [f"role {role}: no test asks for it" for role in self.roles if role not in ROLES]
                + [f"role {role}: every site has this page" for role in REQUIRED_ROLES
                   if role in self.roles and self.roles[role] is None])

    def probe_problems(self, switches: dict[str, bool]) -> list[str]:
        """A probe left empty where the site has the kind of page its feature
        shows on (search, on a site with a search page)."""
        return [f"probe {probe}: the site has {kind} pages, and names none"
                for probe, kind in PROBE_KINDS.items() if self.has(kind, switches) and not self.probe(probe)]

    def probe(self, kind: str) -> list[str]:
        """The pages a CSP-sweep interaction runs on, by name."""
        return self.probes.get(kind, [])

    def role(self, role: str) -> str:
        """The path of the page a module needs for `role`. KeyError if
        routes.yaml does not name the role, NoSuchPage if it says the site
        has no such page."""
        entry = self.roles[role]
        if entry is None:
            raise NoSuchPage(f"{ROUTES_FILE}: the site has no page for {role}")
        return self.path(entry)
