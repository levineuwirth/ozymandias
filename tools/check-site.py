#!/usr/bin/env python3
"""check-site.py — post-build artifact gate for a finished ``_site/``.

Why this exists
---------------
``cabal run site -- build`` exits 0 on outcomes that must never reach the
VPS.  Three of them are confirmed defects rather than hypotheticals:

  * **S01** — the Hakyll provider is not Git's ignore policy, so a
    ``*.local.md`` note, a stray ``*.key``, or the local planning
    ``checklist.md`` can be published even though Git refuses to commit it.
  * **B03** — ``Filters.Viz`` and ``Filters.Score`` deliberately turn a
    rendering failure into page *content* (a ``viz-error`` div, a
    ``score-fragment--error`` figure), and ``Filters.FigureRefs`` renders an
    unresolved cross-reference as ``Figure ?``.  All three ship silently.
  * **B08** — a ``SITE_ENV=dev`` value inherited from the environment makes
    the build emit draft routes; deleting ``_site/drafts`` afterwards does
    not remove the links to it that other pages already grew.

This script inspects the *output*, not the exit status of the process that
made it.  It is stdlib-only and reads nothing outside the site directory, so
it can run against an isolated build, a release staging directory, or the
working ``_site``.

Usage
-----
    tools/check-site.py [SITE_DIR] [options]

    --allow-missing-404   downgrade a missing 404.html to a warning
                          (needed until the 404 route lands; see F12)
    --require-webp        fail on any JPEG/PNG missing its WebP companion
    --warn-only           report everything but exit 0 — diagnostic use
                          only, never for a build that will be deployed
    --max-report N        cap the number of detail lines per check
    --quiet               print only the summary and any failures

Exit status is 1 when any error-level check fired, 0 otherwise.  Warnings
never fail the build; they are printed so a human notices the drift.
"""

from __future__ import annotations

import argparse
import fnmatch
import html
import importlib.util
import os
import re
import sys
from urllib.parse import unquote, urlsplit
from xml.etree import ElementTree

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import sitelib  # noqa: E402

# ---------------------------------------------------------------------------
# What must never be published (S01)
# ---------------------------------------------------------------------------

# Matched against the file's basename, after stripping any transport sidecar
# suffix, so a compressed or signed copy of a private file is caught too.
PRIVATE_FILE_GLOBS = (
    "*.local.md",
    "*.local.html",
    "*.draft.*",
    "*.key",
    "*.pem",
    "*.p12",
    "*.pfx",
    ".env*",
    "*.env",
    "*~",
    "*.swp",
    "*.swo",
    "*.pyc",
    "*.pyo",
    "*.tmp",
    "*.part",
    "*.partial",
    "*.log",
    ".DS_Store",
    ".netrc",
    ".npmrc",
    ".pypirc",
    "credentials*",
    "id_rsa*",
    "id_dsa*",
    "id_ecdsa*",
    "id_ed25519*",
    "checklist.md",
)

PRIVATE_DIR_NAMES = frozenset({"__pycache__"})

# compress-assets.sh and sign-site.sh write these alongside every artifact.
SIDECAR_SUFFIXES = (".gz", ".br", ".sig")

# ---------------------------------------------------------------------------
# Subtrees excluded from *page-content* checks
# ---------------------------------------------------------------------------
#
# These are not first-party pages, and scanning them produces only false
# positives:
#
#   source/    the published source mirror — HTML there is Hakyll *templates*
#              ($body$ placeholders, no resolvable image paths) and prose
#              files that legitimately contain the strings we grep for.
#   pdfjs/     Mozilla's vendored viewer.
#   pagefind/  generated search fragments.
#   archive/**/snapshot.html  monolith captures of third-party pages.
#
# The publication check (S01) deliberately does NOT skip these: the whole
# point of that check is that source/checklist.md is published.
CONTENT_SCAN_SKIP_PREFIXES = ("source/", "pdfjs/", "pagefind/")


def skip_for_content_scan(rel: str) -> bool:
    if rel.startswith(CONTENT_SCAN_SKIP_PREFIXES):
        return True
    if rel.startswith("archive/") and os.path.basename(rel) == "snapshot.html":
        return True
    return False


# ---------------------------------------------------------------------------
# Markers of a failed render that still exited 0 (B03)
# ---------------------------------------------------------------------------
#
# Exact strings from build/Filters/Viz.hs:errorBlock,
# build/Filters/Score.hs:errorBlock, and build/Filters/FigureRefs.hs:labelFor.
RENDER_FAILURE_MARKERS = (
    ('class="viz-error"', "figure/visualization script failed"),
    ("score-fragment--error", "score fragment missing or unreadable"),
    ("Figure ?</a>", "unresolved figure cross-reference"),
)

# A citation that reached the page as its source syntax, [@key]: rendered
# without citeproc, as backlink excerpts once were (audit H04). Citeproc's
# own spans hold the rendered label, never "[@".
RAW_CITATION_RE = re.compile(r'class="citation"[^>]*>\[@')

# B08 — a link into the draft tree, wherever it survived the drafts purge.
DRAFT_HREF_RE = re.compile(r"""(?:href|src)=["'](?:[^"']*/)?drafts/""")

IMG_TAG_RE = re.compile(r"<(img|source)\b([^>]*)>", re.IGNORECASE)
ATTR_SRC_RE = re.compile(r"""\bsrc\s*=\s*["']([^"']*)["']""", re.IGNORECASE)
ATTR_SRCSET_RE = re.compile(r"""\bsrcset\s*=\s*["']([^"']*)["']""", re.IGNORECASE)
ID_ATTR_RE = re.compile(r"""\bid\s*=\s*["']([^"']+)["']""")

# ---------------------------------------------------------------------------
# Links and anchors
# ---------------------------------------------------------------------------
#
# image-targets covers <img> and <source>; these cover everything else a page
# loads or a reader follows. A local URL resolves the way the site's nginx server's
# try_files does ($uri, the directory's index.html, $uri.html), and a fragment on an HTML target
# must name an id, or an <a name>, on that page.
#
# Neither check existed until 2026-10-01, when three links to an essay still
# in content/drafts/ would have deployed as 404s with every gate green. The
# first run found two live defects: citation back-links whose first anchor
# was never emitted, and backlink excerpts whose same-page links named
# anchors only their source page had.
LINK_TAG_RE = re.compile(
    r"<(a|area|link|script|iframe|embed|object|video|audio|track)\b([^>]*)>",
    re.IGNORECASE,
)
# The lookbehind keeps data-href= and friends out: they hold values for
# scripts, not URLs the browser fetches.
LINK_ATTR_RE = re.compile(
    r"""(?<![\w-])(?:href|src|data)\s*=\s*["']([^"']*)["']""", re.IGNORECASE
)
ANCHOR_ID_RE = re.compile(r"""(?<![\w-])id\s*=\s*["']([^"']+)["']""")
ANCHOR_NAME_RE = re.compile(
    r"""<a\b[^>]*?(?<![\w-])name\s*=\s*["']([^"']+)["']""", re.IGNORECASE
)

# img-alt: every <img> a reader's browser parses must carry alt. Markdown
# writes `![](x.png)` with none, and a screen reader then reads the file
# name; `{.decorative}` (build/Filters/Images.hs) writes alt="" instead.
# Comments and script/style bodies are not markup a browser renders, and a
# template comment inside a loop repeats its literal "<img>" once per item.
NON_MARKUP_RE = re.compile(
    r"<!--.*?-->|<(script|style)\b[^>]*>.*?</\1\s*>", re.IGNORECASE | re.DOTALL
)
IMG_ALT_RE = re.compile(r"(?<![\w-])alt(?:\s*=|\s|/?>|$)", re.IGNORECASE)

# plugin-embeds: no <embed> or <object> in a page. Pandoc writes one for a
# PDF used as an image (`![](x.pdf)`). The site's framing headers blank it
# today, since every PDF carries X-Frame-Options: DENY, and the CSP's
# object-src 'none' blocks it outright once enforced (audit W01/A02).
# Publish such a figure as SVG; embed a whole PDF with {{pdf:…}}, which
# uses the PDF.js viewer in an iframe.
PLUGIN_EMBED_RE = re.compile(r"<(embed|object)\b([^>]*)>", re.IGNORECASE)

# Answered by nginx locations, not by files in _site (nginx/*.conf).
SERVER_ROUTES = ("/proxy/", "/csp-report")

# Every page's footer links its detached signature, which sign-site.sh writes
# only after this gate has passed (deploy: build, validate, then sign). Such a
# link is sound when the file it signs exists.
SIGNATURE_SUFFIX = ".sig"

# Fragments a browser honours with no matching element: an empty one, the
# HTML spec's "top", and text fragments.
IMPLICIT_FRAGMENTS = ("", "top")

# RFC 3339 date-time, which is the Atom `updated` contract.
RFC3339_RE = re.compile(
    r"^\d{4}-\d{2}-\d{2}[Tt]\d{2}:\d{2}:\d{2}(\.\d+)?([Zz]|[+-]\d{2}:\d{2})$"
)

ATOM_NS = "{http://www.w3.org/2005/Atom}"


# ---------------------------------------------------------------------------
# Reporting
# ---------------------------------------------------------------------------


class Report:
    """Accumulates per-check errors and warnings, then prints one summary."""

    def __init__(self, max_report: int, quiet: bool) -> None:
        self.max_report = max_report
        self.quiet = quiet
        self.errors: list[tuple[str, str]] = []
        self.warnings: list[tuple[str, str]] = []
        self.checks_run: list[str] = []

    def check(self, name: str) -> None:
        self.checks_run.append(name)

    def error(self, name: str, detail: str) -> None:
        self.errors.append((name, detail))

    def warn(self, name: str, detail: str) -> None:
        self.warnings.append((name, detail))

    def fail(self, name: str, detail: str, *, as_error: bool) -> None:
        (self.error if as_error else self.warn)(name, detail)

    def _emit(self, label: str, items: list[tuple[str, str]], stream) -> None:
        if not items:
            return
        print(f"\n{label} ({len(items)}):", file=stream)
        by_check: dict[str, list[str]] = {}
        for name, detail in items:
            by_check.setdefault(name, []).append(detail)
        for name, details in by_check.items():
            print(f"  [{name}] {len(details)}", file=stream)
            for detail in details[: self.max_report]:
                print(f"      {detail}", file=stream)
            if len(details) > self.max_report:
                print(
                    f"      … and {len(details) - self.max_report} more",
                    file=stream,
                )

    def summarise(self, site_dir: str) -> int:
        # Flush first: the summary may go to stderr while the header went to
        # stdout, and an unflushed stdout would print after it in a pipeline.
        sys.stdout.flush()
        stream = sys.stderr if self.errors else sys.stdout
        self._emit("ERRORS", self.errors, stream)
        self._emit("WARNINGS", self.warnings, stream)
        print(
            f"\ncheck-site: {len(self.checks_run)} checks over {site_dir} — "
            f"{len(self.errors)} error(s), {len(self.warnings)} warning(s)",
            file=stream,
        )
        if self.errors:
            print(
                "check-site: FAILED — this build must not be signed or deployed",
                file=sys.stderr,
            )
            return 1
        print("check-site: OK", file=stream)
        return 0


# ---------------------------------------------------------------------------
# Walking helpers
# ---------------------------------------------------------------------------


def strip_sidecar(name: str) -> str:
    while name.endswith(SIDECAR_SUFFIXES):
        name = name.rsplit(".", 1)[0]
    return name


def walk_files(site_dir: str):
    """Yield (abs_path, site_relative_path) for every regular file."""
    for dirpath, dirnames, filenames in os.walk(site_dir):
        dirnames.sort()
        for name in sorted(filenames):
            full = os.path.join(dirpath, name)
            yield full, os.path.relpath(full, site_dir)


def read_text(path: str) -> str:
    with open(path, "r", encoding="utf-8", errors="replace") as handle:
        return handle.read()


# ---------------------------------------------------------------------------
# Checks
# ---------------------------------------------------------------------------


def check_private_files(site_dir: str, report: Report) -> None:
    """S01 — nothing private, temporary, or credential-shaped may ship."""
    report.check("private-files")
    for dirpath, dirnames, filenames in os.walk(site_dir):
        for bad in sorted(set(dirnames) & PRIVATE_DIR_NAMES):
            rel = os.path.relpath(os.path.join(dirpath, bad), site_dir)
            report.error("private-files", f"{rel}/ (private directory)")
        dirnames[:] = sorted(d for d in dirnames if d not in PRIVATE_DIR_NAMES)
        for name in sorted(filenames):
            base = strip_sidecar(name)
            for glob in PRIVATE_FILE_GLOBS:
                if fnmatch.fnmatch(base, glob) or fnmatch.fnmatch(name, glob):
                    rel = os.path.relpath(os.path.join(dirpath, name), site_dir)
                    report.error("private-files", f"{rel} (matches {glob})")
                    break


def check_no_drafts_dir(site_dir: str, report: Report) -> None:
    """B08 — dev drafts must not be present in a production tree."""
    report.check("drafts-dir")
    drafts = os.path.join(site_dir, "drafts")
    if os.path.isdir(drafts):
        count = sum(len(files) for _, _, files in os.walk(drafts))
        report.error("drafts-dir", f"drafts/ exists ({count} files)")


def check_html_corpus(
    site_dir: str,
    html_files: list[tuple[str, str]],
    report: Report,
) -> None:
    """B03 render failures, B08 draft links, duplicate ids, and every local
    image, link and anchor, in one pass."""
    report.check("render-failures")
    report.check("draft-links")
    report.check("duplicate-ids")
    report.check("image-targets")
    report.check("link-targets")
    report.check("link-fragments")
    report.check("img-alt")
    report.check("plugin-embeds")

    anchors = AnchorIndex(site_dir)

    for full, rel in html_files:
        text = read_text(full)
        anchors.add(rel, text)

        for marker, description in RENDER_FAILURE_MARKERS:
            if marker in text:
                report.error("render-failures", f"{rel}: {description}")
        if RAW_CITATION_RE.search(text):
            report.error("render-failures", f"{rel}: a citation shows its source syntax [@…]")

        if DRAFT_HREF_RE.search(text):
            report.error("draft-links", f"{rel}: links into /drafts/")

        seen: dict[str, int] = {}
        for ident in ID_ATTR_RE.findall(text):
            seen[ident] = seen.get(ident, 0) + 1
        dupes = sorted(i for i, n in seen.items() if n > 1)
        if dupes:
            shown = ", ".join(dupes[:5])
            more = "" if len(dupes) <= 5 else f" (+{len(dupes) - 5} more)"
            report.warn("duplicate-ids", f"{rel}: {shown}{more}")

        for missing in missing_image_targets(site_dir, rel, text):
            report.error("image-targets", f"{rel}: missing {missing}")

        check_links(site_dir, rel, text, anchors, report)

        unlabelled = images_without_alt(text)
        if unlabelled:
            more = f" (+{len(unlabelled) - 1} more)" if len(unlabelled) > 1 else ""
            report.error(
                "img-alt",
                f"{rel}: <img src={unlabelled[0]!r}> has no alt{more} — describe "
                f"it, or mark it {{.decorative}} for alt=\"\"",
            )

        embeds = plugin_embeds(text)
        if embeds:
            more = f" (+{len(embeds) - 1} more)" if len(embeds) > 1 else ""
            report.error(
                "plugin-embeds",
                f"{rel}: {embeds[0]}{more} — object-src 'none' blocks it; publish "
                f"a figure as SVG, or embed a PDF with {{{{pdf:…}}}}",
            )


def images_without_alt(text: str) -> list[str]:
    """The src of every rendered <img> that has no alt attribute."""
    markup = NON_MARKUP_RE.sub("", text)
    missing = []
    for _tag, attrs in IMG_TAG_RE.findall(markup):
        if _tag.lower() == "img" and not IMG_ALT_RE.search(attrs):
            src = ATTR_SRC_RE.search(attrs)
            missing.append(src.group(1) if src else "")
    return missing


def plugin_embeds(text: str) -> list[str]:
    """Each rendered <embed>/<object>, as `<tag src>` for the report."""
    markup = NON_MARKUP_RE.sub("", text)
    found = []
    for tag, attrs in PLUGIN_EMBED_RE.findall(markup):
        src = re.search(r"""(?<![\w-])(?:src|data)\s*=\s*["']([^"']*)["']""", attrs)
        found.append(f"<{tag.lower()} {src.group(1) if src else ''}>".replace(" >", ">"))
    return found


class AnchorIndex:
    """The ids and <a name>s of each HTML page, read once and on demand."""

    def __init__(self, site_dir: str) -> None:
        self.site_dir = site_dir
        self._anchors: dict[str, set[str]] = {}

    def add(self, rel: str, text: str) -> None:
        if rel not in self._anchors:
            found = ANCHOR_ID_RE.findall(text) + ANCHOR_NAME_RE.findall(text)
            self._anchors[rel] = {html.unescape(a) for a in found}

    def has(self, rel: str, fragment: str) -> bool:
        if rel not in self._anchors:
            self.add(rel, read_text(os.path.join(self.site_dir, rel)))
        return fragment in self._anchors[rel]


def resolve_target(site_dir: str, page_dir: str, path: str) -> str | None:
    """The site-relative file nginx would serve for a local path, or None.

    Mirrors ``try_files $uri $uri/ $uri.html`` in the site's nginx server,
    where a directory without its slash is first redirected to it. A path
    that climbs out of the site resolves to nothing.
    """
    if path.startswith("/"):
        joined = path.lstrip("/")
    else:
        joined = os.path.join(page_dir, path)
    base = os.path.normpath(joined)
    if base == ".":
        base = ""
    if base.startswith(".."):
        return None
    for candidate in (base, os.path.join(base, "index.html"), base + ".html"):
        if candidate and os.path.isfile(os.path.join(site_dir, candidate)):
            return candidate
    return None


def check_links(
    site_dir: str, rel: str, text: str, anchors: AnchorIndex, report: Report
) -> None:
    """Every local href/src/data a page carries resolves, and so does its
    fragment when the target is a page."""
    page_dir = os.path.dirname(rel)
    seen: set[str] = set()
    for tag, attrs in LINK_TAG_RE.findall(text):
        for raw in LINK_ATTR_RE.findall(attrs):
            url = html.unescape(raw).strip()
            if url in seen:
                continue
            seen.add(url)
            parts = urlsplit(url)
            if parts.scheme or parts.netloc or url.startswith("//"):
                continue  # external, mailto:, data:, javascript:
            path = unquote(parts.path)
            if path.startswith(SERVER_ROUTES):
                continue

            target = resolve_target(site_dir, page_dir, path) if path else rel
            if target is None and path.endswith(SIGNATURE_SUFFIX):
                signed = path[: -len(SIGNATURE_SUFFIX)]
                if resolve_target(site_dir, page_dir, signed) is not None:
                    continue
            if target is None:
                report.error(
                    "link-targets", f"{rel}: <{tag.lower()}> {url} resolves to nothing"
                )
                continue

            fragment = unquote(parts.fragment)
            if (
                fragment in IMPLICIT_FRAGMENTS
                or fragment.startswith(":~:")
                or not target.endswith(".html")
            ):
                continue
            if not anchors.has(target, fragment):
                where = "this page" if target == rel else target
                report.error(
                    "link-fragments", f"{rel}: {url} — no #{fragment} on {where}"
                )


def missing_image_targets(site_dir: str, rel: str, text: str) -> list[str]:
    """Local <img src>/srcset and <source srcset> targets absent from _site."""
    page_dir = os.path.dirname(rel)
    missing: list[str] = []
    seen: set[str] = set()

    for _tag, attrs in IMG_TAG_RE.findall(text):
        candidates: list[str] = []
        src = ATTR_SRC_RE.search(attrs)
        if src:
            candidates.append(src.group(1))
        srcset = ATTR_SRCSET_RE.search(attrs)
        if srcset:
            for entry in srcset.group(1).split(","):
                entry = entry.strip()
                if entry:
                    candidates.append(entry.split()[0])

        for raw in candidates:
            url = raw.strip()
            if not url or url in seen:
                continue
            seen.add(url)
            resolved = resolve_local(site_dir, page_dir, url)
            if resolved is None:
                continue
            if not os.path.exists(resolved):
                missing.append(url)
    return missing


def resolve_local(site_dir: str, page_dir: str, url: str) -> str | None:
    """Map a URL to a path under site_dir, or None when it is not local."""
    if url.startswith(("http://", "https://", "//", "data:", "mailto:", "#")):
        return None
    parts = urlsplit(url)
    if parts.scheme or parts.netloc:
        return None
    path = unquote(parts.path)
    if not path:
        return None
    if path.startswith("/"):
        target = os.path.join(site_dir, path.lstrip("/"))
    else:
        target = os.path.join(site_dir, page_dir, path)
    return os.path.normpath(target)


def site_feeds() -> list[str]:
    """The feeds the site's config lists (`site config`), by their paths
    in the built site."""
    return [feed["path"] for feed in sitelib.site_config()["feeds"]]


def check_feed(site_dir: str, rel: str, report: Report) -> None:
    """F10 — an advertised Atom feed must parse and carry an RFC 3339 date."""
    report.check(f"feed:{rel}")
    path = os.path.join(site_dir, rel)
    if not os.path.exists(path):
        report.error(f"feed:{rel}", "missing")
        return
    try:
        root = ElementTree.parse(path).getroot()
    except ElementTree.ParseError as exc:
        report.error(f"feed:{rel}", f"not well-formed XML: {exc}")
        return

    updates = root.findall(f"{ATOM_NS}updated") + root.findall(
        f".//{ATOM_NS}entry/{ATOM_NS}updated"
    )
    if not updates:
        report.error(f"feed:{rel}", "no <updated> element")
        return
    for element in updates:
        value = (element.text or "").strip()
        if not RFC3339_RE.match(value):
            report.error(f"feed:{rel}", f"<updated> is not RFC 3339: {value!r}")


def check_sitemap(site_dir: str, report: Report) -> None:
    report.check("sitemap")
    path = os.path.join(site_dir, "sitemap.xml")
    if not os.path.exists(path):
        report.error("sitemap", "sitemap.xml missing")
        return
    try:
        ElementTree.parse(path)
    except ElementTree.ParseError as exc:
        report.error("sitemap", f"sitemap.xml is not well-formed: {exc}")


def check_404(site_dir: str, report: Report, *, as_error: bool) -> None:
    """F12 — nginx advertises /404.html; the build must actually emit it."""
    report.check("404-page")
    path = os.path.join(site_dir, "404.html")
    if not os.path.exists(path):
        report.fail(
            "404-page",
            "404.html is missing (nginx's error_page target does not exist)",
            as_error=as_error,
        )
    elif os.path.getsize(path) == 0:
        report.fail("404-page", "404.html is empty", as_error=as_error)


def check_webp(site_dir: str, report: Report, *, as_error: bool) -> None:
    """P02 — the WebP pipeline silently no-ops when cwebp is absent."""
    report.check("webp")
    webps: set[str] = set()
    images: list[str] = []
    for _full, rel in walk_files(site_dir):
        lower = rel.lower()
        if lower.endswith(".webp"):
            webps.add(rel)
        elif lower.endswith((".jpg", ".jpeg", ".png")):
            images.append(rel)
    # Pillow makes photo-variant WebPs even without cwebp. Checking only
    # for zero companions would let those hide unconverted originals.
    # Keep path case: the production filesystem is case-sensitive.
    for rel in sorted(images):
        companion = os.path.splitext(rel)[0] + ".webp"
        if companion not in webps:
            report.fail("webp", f"{rel}: missing .webp companion ({companion}); "
                        "run make convert-images with cwebp installed", as_error=as_error)


def jpeg_gps(path: str) -> bool:
    """Whether a JPEG's EXIF (APP1) carries a GPS IFD pointer (tag 0x8825).
    Reads markers up to the image data only; anything unparseable is
    treated as no EXIF rather than guessed at."""
    import struct
    try:
        with open(path, "rb") as f:
            if f.read(2) != b"\xff\xd8":
                return False
            while True:
                marker = f.read(2)
                if len(marker) < 2 or marker[0] != 0xFF or marker[1] in (0xDA, 0xD9):
                    return False
                (length,) = struct.unpack(">H", f.read(2))
                body = f.read(length - 2)
                if marker[1] != 0xE1 or not body.startswith(b"Exif\0\0"):
                    continue
                tiff = body[6:]
                endian = {b"II": "<", b"MM": ">"}.get(tiff[:2])
                if endian is None:
                    return False
                (ifd,) = struct.unpack(endian + "I", tiff[4:8])
                (count,) = struct.unpack(endian + "H", tiff[ifd:ifd + 2])
                for i in range(count):
                    (tag,) = struct.unpack(endian + "H", tiff[ifd + 2 + 12 * i: ifd + 4 + 12 * i])
                    if tag == 0x8825:
                        return True
                return False
    except (OSError, struct.error, IndexError):
        return False


DATA_PAGES_RE = re.compile(r'''data-pages="([^"]*)"''')


def check_cv_pdfs(site_dir: str, report: Report) -> None:
    """C02 — /cv.pdf and /resume.pdf are built by hand from the CV YAML the
    Vita page reads at every build; warn when they are older than it. Only
    for the output of the site being checked ($SITE_ROOT, else the working
    directory): the record is that checkout's."""
    report.check("cv-pdfs")
    root = os.path.realpath(os.environ.get("SITE_ROOT") or os.getcwd())
    if os.path.realpath(site_dir) not in (os.path.join(root, "_site"), os.path.join(root, "_site-dev")):
        return
    tool = os.path.join(root, "tools", "cv-pdfs.py")
    if not os.path.isfile(tool):
        return   # a site without a CV
    spec = importlib.util.spec_from_file_location("cv_pdfs", tool)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    for problem in module.stale():
        report.warn("cv-pdfs", problem)


def check_score_pages(site_dir: str, report: Report) -> None:
    """M02 — a score reader and its published pages agree. A page file the
    reader does not list is left over from an earlier engraving (or a
    movement withheld since); a listed page with no file is a hole."""
    report.check("score-pages")
    music = os.path.join(site_dir, "music")
    if not os.path.isdir(music):
        return
    for slug in sorted(os.listdir(music)):
        reader = os.path.join(music, slug, "score", "index.html")
        scores = os.path.join(music, slug, "scores")
        if not os.path.isfile(reader):
            continue
        m = DATA_PAGES_RE.search(read_text(reader))
        listed = {p.rsplit("/", 1)[-1] for p in m.group(1).split(",") if p} if m else set()
        present = {f for f in os.listdir(scores)
                   if f.startswith("page-") and f.endswith(".svg")} if os.path.isdir(scores) else set()
        for f in sorted(present - listed):
            report.error("score-pages", f"music/{slug}/scores/{f} is published but not in the reader")
        for f in sorted(listed - present):
            report.error("score-pages", f"music/{slug}/score/ lists {f}, which is missing")


def check_photo_gps(site_dir: str, report: Report) -> None:
    """T03 — a JPEG published with the camera's GPS position. The importers
    strip EXIF, but a failed or interrupted import used to leave the
    unstripped copy in content/, which the build publishes regardless of
    git."""
    report.check("photo-gps")
    for full, rel in walk_files(site_dir):
        if rel.lower().endswith((".jpg", ".jpeg")) and jpeg_gps(full):
            report.error("photo-gps", f"{rel}: carries GPS EXIF — strip it (exiftool -all=)")


# The files tools/download-model.sh fetches, relative to _site/models/.
SEARCH_MODEL_FILES = tuple(
    f"all-MiniLM-L6-v2/{name}"
    for name in ("config.json", "tokenizer.json", "tokenizer_config.json",
                 "special_tokens_map.json", "onnx/model_quantized.onnx")
)


def check_search_model(site_dir: str, report: Report) -> None:
    """D07 — static/models/ is gitignored, so a checkout without it builds a
    site whose semantic search has no model, and the deploy's rsync --delete
    then removes the model from the server."""
    report.check("search-model")
    if not os.path.exists(os.path.join(site_dir, "js", "semantic-search.js")):
        return
    missing = [f for f in SEARCH_MODEL_FILES
               if not os.path.isfile(os.path.join(site_dir, "models", f))]
    if missing:
        report.error(
            "search-model",
            f"semantic search ships without models/{missing[0]}"
            f"{f' (+{len(missing) - 1} more)' if len(missing) > 1 else ''} — "
            f"run engine/tools/download-model.sh",
        )


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        prog="check-site.py",
        description="Reject a finished _site that must not be deployed.",
    )
    parser.add_argument("site_dir", nargs="?", default="_site")
    parser.add_argument(
        "--allow-missing-404",
        action="store_true",
        help="downgrade a missing 404.html from an error to a warning",
    )
    parser.add_argument(
        "--require-webp",
        action="store_true",
        help="fail when any JPEG or PNG lacks its .webp companion",
    )
    parser.add_argument(
        "--warn-only",
        action="store_true",
        help="report every finding but exit 0 (diagnostic use only)",
    )
    parser.add_argument("--max-report", type=int, default=15)
    parser.add_argument("--quiet", action="store_true")
    args = parser.parse_args(argv)

    site_dir = os.path.normpath(args.site_dir)
    if not os.path.isdir(site_dir):
        print(f"check-site: {site_dir} is not a directory", file=sys.stderr)
        return 2

    report = Report(max_report=args.max_report, quiet=args.quiet)

    html_files = [
        (full, rel)
        for full, rel in walk_files(site_dir)
        if rel.endswith(".html") and not skip_for_content_scan(rel)
    ]
    if not args.quiet:
        print(
            f"check-site: scanning {site_dir} "
            f"({len(html_files)} first-party HTML pages)"
        )

    check_private_files(site_dir, report)
    check_no_drafts_dir(site_dir, report)
    check_html_corpus(site_dir, html_files, report)
    for rel in site_feeds():
        check_feed(site_dir, rel, report)
    check_sitemap(site_dir, report)
    check_404(site_dir, report, as_error=not args.allow_missing_404)
    check_webp(site_dir, report, as_error=args.require_webp)
    check_cv_pdfs(site_dir, report)
    check_search_model(site_dir, report)
    check_photo_gps(site_dir, report)
    check_score_pages(site_dir, report)

    status = report.summarise(site_dir)
    if status and args.warn_only:
        print(
            "\ncheck-site: --warn-only — exiting 0 DESPITE the errors above.\n"
            "            This site is not fit to deploy. Use this flag only to\n"
            "            see the whole picture while fixes are still landing.",
            file=sys.stderr,
        )
        return 0
    return status


if __name__ == "__main__":
    sys.exit(main())
