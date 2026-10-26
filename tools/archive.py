#!/usr/bin/env python3
"""
archive.py — Build-time link-archiving tool for the site.

Reads archive/manifest.yaml, fetches any manifest URL that has no local
artifact yet, stores it under archive/<slug>/, extracts readable text,
writes the per-entry archive/<slug>/PROVENANCE.json, and (re)writes the
Hakyll input data/archive-index.json.

Two artifact types:
  * pdf  — downloaded directly, stored as document.pdf, text via pdftotext.
  * html — snapshotted with `monolith` into a single self-contained
           snapshot.html (JavaScript stripped, assets inlined as data
           URIs), a restrictive Content-Security-Policy <meta> injected,
           text extracted with BeautifulSoup.

Subcommands:
  fetch    download missing artifacts, (re)generate sidecars + index;
           an original already dead at first fetch falls back to its most
           recent existing Wayback capture (recorded as `fetched-from`)
  refresh  deliberately re-snapshot a single entry, recording the prior
           SHA in the new PROVENANCE.json's `previous-sha256`
  wayback  submit archived URLs to the Wayback Machine as a second,
           independent copy; backfill the capture URL into PROVENANCE.json
  check    HEAD/GET-probe every manifest URL for link rot, updating
           data/archive-state.json with asymmetric hysteresis
  suggest  print works cited in data/*.bib (url / doi fields) but not in
           the manifest, as manifest-ready lines; never edits the manifest
  gc       delete archive/<slug>/ directories listed in archive/removed.yaml

Failure policy:
  * Integrity errors — a committed artifact whose SHA-256 no longer
    matches PROVENANCE.json, or a slug whose manifest URL has changed —
    print loudly and exit non-zero, halting `make build`.
  * Transient errors — a network failure, an over-cap download, a missing
    `monolith` binary, a manifest entry missing its `url:` — print a
    warning, skip that entry, and exit zero so the build proceeds (the
    entry is retried on the next build).

See ARCHIVE.md for the full design.

Gated on .venv by the Makefile (same convention as embed.py). Non-stdlib
dependencies: PyYAML and beautifulsoup4, both already in pyproject.toml.
External tools: `pdftotext` (poppler) for PDF text, and the `monolith`
binary — vendored at tools/bin/monolith, see tools/monolith-version.txt.
"""

from __future__ import annotations

import datetime
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import urllib.error
import urllib.request
from pathlib import Path
from urllib.parse import quote, urlparse

import yaml

sys.path.insert(0, str(Path(__file__).resolve().parent))
import sitelib  # noqa: E402

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

SITE_ROOT     = sitelib.SITE_ROOT
ARCHIVE_DIR   = SITE_ROOT / "archive"
MANIFEST      = ARCHIVE_DIR / "manifest.yaml"
REMOVED       = ARCHIVE_DIR / "removed.yaml"
INDEX_OUT     = SITE_ROOT / "data" / "archive-index.json"
STATE_OUT     = SITE_ROOT / "data" / "archive-state.json"

ROT_FAILS     = 3       # consecutive failed scans before `rotted` is considered
ROT_DAYS      = 14      # ... and the streak must also span at least this many days

SIZE_CAP      = 25 * 1024 * 1024          # 25 MB per-artifact cap
TIMEOUT       = 60                        # seconds, per network request
WAYBACK_TIMEOUT = 120                     # seconds — Save Page Now is slow

# Per-type on-disk names. The artifact is committed; the .txt is generated
# (gitignored) and regenerated whenever the artifact's SHA-256 changes.
ARTIFACT = {"pdf": "document.pdf", "html": "snapshot.html"}
TEXTFILE = {"pdf": "document.txt", "html": "snapshot.txt"}

# Injected into every HTML snapshot's <head>. Permits exactly what a
# faithful monolith capture needs — inlined images/fonts as data URIs and
# inline styles (as <style> elements and as style="" attributes) — and
# blocks every network fetch and every script a broken or hostile snapshot
# might attempt. Defense-in-depth behind the iframe sandbox; see ARCHIVE.md.
ARCHIVE_CSP = (
    "default-src 'none'; img-src data:; "
    "style-src 'unsafe-inline'; style-src-elem 'unsafe-inline'; "
    "style-src-attr 'unsafe-inline'; font-src data:; "
    "script-src 'none'; object-src 'none'; frame-src 'none'"
)


def check_canary() -> str:
    """Probed before a link-rot scan: the site's own front page (site.yaml).
    Unreachable -> the scan is inconclusive (offline machine, dead DNS) and
    state is left untouched. Guards the unattended timer-run case: three
    offline scans spanning two weeks would otherwise flip every entry to
    `rotted` at once."""
    return sitelib.site_config()["url"] + "/"


def user_agent() -> str:
    """Who is fetching, and whom to write to (site.yaml)."""
    config = sitelib.site_config()
    return f"{config['host']}/archive ({config['contact-email']}; removal requests honored)"


def log(msg: str) -> None:
    print(f"[archive] {msg}")


def err(msg: str) -> None:
    print(f"[archive] ERROR: {msg}", file=sys.stderr)


def atomic_write_json(path: Path, obj) -> None:
    """PROVENANCE.json and the generated index/state files are integrity
    records: an interrupt mid-write must never leave a truncated file that
    the next run parses (or mistakes for corruption), so they are written
    through sitelib.atomic_write_text, durably."""
    sitelib.atomic_write_text(
        path, json.dumps(obj, indent=2, ensure_ascii=False) + "\n")


# ---------------------------------------------------------------------------
# Manifest / removed.yaml
# ---------------------------------------------------------------------------

def load_yaml_list(path: Path) -> list[dict]:
    """Load a YAML file expected to hold a list of mappings. An empty or
    absent file yields an empty list."""
    if not path.exists():
        return []
    try:
        data = yaml.safe_load(path.read_text(encoding="utf-8"))
    except yaml.YAMLError as exc:
        # Named, like the shape errors below, rather than a traceback.
        err(f"{path.name}: not valid YAML: {exc}")
        sys.exit(1)
    if data is None:
        return []
    if not isinstance(data, list):
        err(f"{path.name}: expected a YAML list, got {type(data).__name__}")
        sys.exit(1)
    # Validate items too: a stray scalar line (`- https://example.com`
    # instead of `- url: ...`) would otherwise surface much later as an
    # AttributeError deep inside fetch/wayback/check.
    for i, item in enumerate(data):
        if not isinstance(item, dict):
            err(f"{path.name}: entry {i + 1} is not a mapping "
                f"(got {type(item).__name__}: {item!r}); "
                f"each entry must be `- url: ...`")
            sys.exit(1)
    return data


SLUG_RE = re.compile(r"[a-z0-9][a-z0-9-]{0,63}")


def derive_slug(url: str) -> str:
    """Auto-derive a slug as {domain-label}-{path-tail}, slugified and
    truncated. A manifest `slug:` override is preferred over this."""
    p = urlparse(url)
    host = p.netloc.lower().removeprefix("www.")
    labels = host.split(".")
    domain = labels[-2] if len(labels) >= 2 else (host or "url")
    tail = (p.path.rstrip("/").split("/") or [""])[-1] or "index"
    slug = re.sub(r"[^a-z0-9]+", "-", f"{domain}-{tail}".lower()).strip("-")
    slug = slug[:64].strip("-")
    return slug or hashlib.sha1(url.encode()).hexdigest()[:12]


def entry_slug(entry: dict) -> str:
    slug = entry.get("slug")
    return slug if slug else derive_slug(entry["url"])


def entry_aliases(entry: dict) -> list[str]:
    """The authored `aliases:` list of a manifest entry, validated. An
    alias is an equivalent URL of the same work that no offline
    normalisation can derive — e.g. its DOI form vs. the landing URL the
    artifact was fetched from. Aliases are matching metadata, not
    identity: editing them never requires a refresh (the index is
    rewritten from the manifest on every fetch). Malformed values are
    fatal — a typo'd alias would otherwise silently never match, and the
    resulting affordance gap is invisible."""
    aliases = entry.get("aliases", [])
    if not isinstance(aliases, list) or not all(
            isinstance(a, str) and a.startswith(("http://", "https://"))
            for a in aliases):
        err(f"manifest entry {entry.get('url')!r}: `aliases:` must be a "
            f"list of http(s) URLs, got {aliases!r}")
        sys.exit(1)
    return aliases


# ---------------------------------------------------------------------------
# Hashing / type detection
# ---------------------------------------------------------------------------

def sha256_of(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 16), b""):
            h.update(chunk)
    return h.hexdigest()


def probe_headers(url: str) -> dict[str, str]:
    """Best-effort HEAD request. Returns the response headers as a
    lowercased-key dict, or {} on any failure (some servers reject HEAD)."""
    req = urllib.request.Request(url, method="HEAD",
                                 headers={"User-Agent": user_agent()})
    try:
        with urllib.request.urlopen(req, timeout=TIMEOUT) as resp:
            return {k.lower(): v for k, v in resp.headers.items()}
    except Exception:                                  # noqa: BLE001
        return {}


def probe_headers_get(url: str) -> dict[str, str]:
    """Best-effort ranged GET, returning lowercased-key response headers
    or {} on any failure. Used alongside 'probe_headers' so an
    @X-Robots-Tag: noarchive@ that appears only on GET (some servers omit
    it on HEAD) is still honoured."""
    req = urllib.request.Request(
        url, method="GET",
        headers={"User-Agent": user_agent(), "Range": "bytes=0-0"})
    try:
        with urllib.request.urlopen(req, timeout=TIMEOUT) as resp:
            return {k.lower(): v for k, v in resp.headers.items()}
    except Exception:                                  # noqa: BLE001
        return {}


def detect_type(url: str, override) -> str | None:
    """Resolve an entry's artifact type. A manifest `type:` wins; then the
    URL extension; then a Content-Type probe; HTML is the final default
    (most non-PDF cited URLs are pages). Returns None on a bad override."""
    if override:
        o = str(override).strip().lower()
        if o in ARTIFACT:
            return o
        err(f"{url}: manifest type: {override!r} not recognised "
            f"(expected pdf | html)")
        return None
    path = urlparse(url).path.lower()
    if path.endswith(".pdf"):
        return "pdf"
    if path.endswith((".html", ".htm")):
        return "html"
    ct = (probe_headers(url).get("content-type") or "").lower()
    if "pdf" in ct:
        return "pdf"
    return "html"


# ---------------------------------------------------------------------------
# PDF fetch + text extraction
# ---------------------------------------------------------------------------

def download(url: str, dest: Path, what: str = "",
             cap_hint: str = " (commit deliberately with `git add -f`)") -> tuple[str, str]:
    """Stream `url` into `dest` under the size cap, honouring an
    X-Robots-Tag: noarchive on the response. Returns the result and the URL
    the response came from (after redirects). The result is "ok"; "dead"
    when the document itself could not be retrieved (DNS failure, refused
    connection, timeout, HTTP error status — the cases where a Wayback
    fallback is legitimate); or "skip" for a policy or local failure
    (noarchive, the cap, disk) that a fallback must not circumvent. Only
    "ok" leaves a file. `what` and `cap_hint` complete the cap message."""
    req = urllib.request.Request(url, headers={"User-Agent": user_agent()})
    try:
        with urllib.request.urlopen(req, timeout=TIMEOUT) as resp:
            # X-Robots-Tag: noarchive — honour the archiving-specific
            # directive even though robots.txt itself is not gated.
            robots = (resp.headers.get("X-Robots-Tag") or "").lower()
            if "noarchive" in robots:
                err(f"{url}: response carries X-Robots-Tag: noarchive — skipped")
                return "skip", url
            final = resp.geturl()
            total = 0
            with dest.open("wb") as fh:
                for chunk in iter(lambda: resp.read(1 << 16), b""):
                    total += len(chunk)
                    if total > SIZE_CAP:
                        fh.close()
                        dest.unlink(missing_ok=True)
                        err(f"{url}: {what}exceeds {SIZE_CAP // (1024*1024)} MB cap "
                            f"— skipped{cap_hint}")
                        return "skip", url
                    fh.write(chunk)
        return "ok", final
    except (urllib.error.URLError, TimeoutError) as exc:
        dest.unlink(missing_ok=True)
        err(f"{url}: fetch failed — {exc}")
        return "dead", url             # HTTPError is a URLError subclass
    except Exception as exc:                       # noqa: BLE001 — report any failure
        dest.unlink(missing_ok=True)
        err(f"{url}: fetch failed — {exc}")
        return "skip", url


def fetch_pdf(url: str, dest: Path) -> str:
    """Download a PDF to `dest` ('download' says what the result means). A
    partial or over-cap download leaves no file."""
    tmp = dest.with_suffix(dest.suffix + ".part")
    result, _ = download(url, tmp)
    if result == "ok":
        try:
            tmp.replace(dest)
        except Exception as exc:                   # noqa: BLE001 — a local failure
            tmp.unlink(missing_ok=True)
            err(f"{url}: fetch failed — {exc}")
            return "skip"
    return result


def extract_text_pdf(pdf: Path, txt: Path) -> None:
    """Extract plain text from `pdf` into `txt` via pdftotext. On any
    failure an empty file is written so downstream steps still find it."""
    try:
        # `--` ends option parsing so a slug starting with `-` cannot be
        # mistaken for a pdftotext option.
        subprocess.run(["pdftotext", "-q", "--", str(pdf), str(txt)],
                       check=True)
    except (subprocess.CalledProcessError, FileNotFoundError) as exc:
        err(f"{pdf.name}: pdftotext failed ({exc}); writing empty text sidecar")
        txt.write_text("", encoding="utf-8")


# ---------------------------------------------------------------------------
# HTML snapshot (monolith) + CSP + text extraction + quality classification
# ---------------------------------------------------------------------------

def find_monolith() -> str | None:
    """Locate the monolith binary: $MONOLITH_BIN, then the vendored
    tools/bin/monolith, then $PATH. None if unavailable."""
    env = os.environ.get("MONOLITH_BIN")
    if env and Path(env).is_file():
        return env
    vendored = sitelib.engine_path("tools/bin/monolith")
    if vendored.is_file():
        return str(vendored)
    return shutil.which("monolith")


MONOLITH_VERSION_FILE = sitelib.engine_path("tools/monolith-version.txt")

# Binaries already verified this run — the pin check hashes the binary
# once, not once per snapshot.
_monolith_verified: set[str] = set()


def _pinned_monolith_sha256() -> str | None:
    """Parse the `sha256 = <hex>` line from tools/monolith-version.txt.
    Returns None when the file is missing or unparseable (the caller
    warns and continues — only a *mismatch* is fatal)."""
    try:
        text = MONOLITH_VERSION_FILE.read_text(encoding="utf-8")
    except OSError:
        return None
    m = re.search(r"^\s*sha256\s*=\s*([0-9a-fA-F]{64})\s*$",
                  text, re.MULTILINE)
    return m.group(1).lower() if m else None


def verify_monolith(mono: str) -> None:
    """Integrity gate for the snapshot tool itself: the binary that
    produces committed artifacts must match the SHA-256 pinned in
    tools/monolith-version.txt. A mismatch is an integrity error (print
    loudly, exit non-zero, halt `make build`); a missing or unparseable
    version file is a warning only."""
    if mono in _monolith_verified:
        return
    pinned = _pinned_monolith_sha256()
    if pinned is None:
        print(f"[archive] WARNING: {MONOLITH_VERSION_FILE.name} is missing "
              f"or has no parseable `sha256 = …` line — monolith binary "
              f"integrity NOT verified ({mono})", file=sys.stderr)
        _monolith_verified.add(mono)
        return
    live = sha256_of(Path(mono))
    if live != pinned:
        err(f"monolith binary {mono} fails SHA-256 verification "
            f"(pinned {pinned}, found {live}). The snapshot tool's bytes "
            f"do not match tools/monolith-version.txt — re-vendor the "
            f"binary or update the pin (see that file's instructions).")
        sys.exit(1)
    _monolith_verified.add(mono)


def body_noarchive(path: Path) -> bool:
    """True if the snapshot declares <meta name=robots ... noarchive> —
    the in-document equivalent of the X-Robots-Tag header."""
    from bs4 import BeautifulSoup
    soup = BeautifulSoup(path.read_text(encoding="utf-8", errors="replace"),
                         "html.parser")
    for m in soup.find_all("meta"):
        if (m.get("name") or "").lower() in ("robots", "googlebot"):
            if "noarchive" in (m.get("content") or "").lower():
                return True
    return False


def inject_archive_metas(path: Path) -> None:
    """Insert the archive CSP and a robots `noindex, noarchive` <meta> as
    the first <head> children, dropping any CSP or robots <meta> the
    original shipped: two intersecting CSPs could block resources a
    faithful snapshot legitimately needs, and we own the indexing posture
    for the served snapshot regardless of what the original said."""
    from bs4 import BeautifulSoup
    soup = BeautifulSoup(path.read_text(encoding="utf-8", errors="replace"),
                         "html.parser")
    head = soup.head
    if head is None:
        head = soup.new_tag("head")
        (soup.html if soup.html is not None else soup).insert(0, head)
    for m in list(head.find_all("meta")):
        if (m.get("http-equiv") or "").lower() == "content-security-policy":
            m.decompose()
        elif (m.get("name") or "").lower() == "robots":
            m.decompose()
    # Inserted in reverse so the final head order is CSP first, robots
    # second (deterministic, easy to grep).
    robots = soup.new_tag("meta")
    robots["name"] = "robots"
    robots["content"] = "noindex, noarchive"
    head.insert(0, robots)
    csp = soup.new_tag("meta")
    csp["http-equiv"] = "Content-Security-Policy"
    csp["content"] = ARCHIVE_CSP
    head.insert(0, csp)
    path.write_text(str(soup), encoding="utf-8")


def fetch_html(url: str, dest: Path) -> str:
    """Snapshot an HTML page with monolith into a single self-contained
    file at `dest`, then inject the archive CSP. Returns "ok" on success,
    "dead" when the source document itself could not be retrieved (the
    Wayback-fallback-eligible case), or "skip" for every other failure —
    noarchive directives, the size cap, monolith problems — none of which
    a fallback may circumvent. Every failure path is non-fatal."""
    # Honour directives returned by preliminary probes before performing
    # the document fetch. The full document response is inspected below
    # and is also the exact body passed to monolith; do not let monolith
    # perform a second unobservable fetch of the primary document.
    if any("noarchive" in (h.get("x-robots-tag") or "").lower()
           for h in (probe_headers(url),
                     probe_headers_get(url))):
        err(f"{url}: response carries X-Robots-Tag: noarchive — skipped")
        return "skip"

    mono = find_monolith()
    if mono is None:
        err(f"{url}: monolith not found — vendor the binary at "
            f"engine/tools/bin/monolith (see engine/tools/monolith-version.txt) "
            f"or set $MONOLITH_BIN; HTML snapshot skipped")
        return "skip"
    verify_monolith(mono)

    source = dest.with_suffix(dest.suffix + ".source.part")
    tmp = dest.with_suffix(dest.suffix + ".part")
    result, effective_url = download(url, source, "source HTML ", cap_hint="")
    if result != "ok":
        return result

    if body_noarchive(source):
        source.unlink(missing_ok=True)
        err(f"{url}: response declares <meta name=robots> noarchive — skipped")
        return "skip"

    cmd = [mono, "--no-js", "--ignore-errors", "--quiet",
           "--timeout", str(TIMEOUT), "--user-agent", user_agent(),
           "--base-url", effective_url, "--output", str(tmp), "-"]
    try:
        proc = subprocess.run(cmd, input=source.read_bytes(),
                              capture_output=True, timeout=TIMEOUT * 6)
    except subprocess.TimeoutExpired:
        source.unlink(missing_ok=True)
        tmp.unlink(missing_ok=True)
        err(f"{url}: monolith timed out — skipped")
        return "skip"
    except Exception as exc:                           # noqa: BLE001
        source.unlink(missing_ok=True)
        tmp.unlink(missing_ok=True)
        err(f"{url}: monolith failed to run — {exc}")
        return "skip"
    finally:
        source.unlink(missing_ok=True)

    if proc.returncode != 0:
        tmp.unlink(missing_ok=True)
        output = proc.stderr or proc.stdout or b""
        tail = output.decode("utf-8", errors="replace").strip().splitlines()
        err(f"{url}: monolith exited {proc.returncode} "
            f"({tail[-1] if tail else 'no output'}) — skipped")
        return "skip"
    if not tmp.exists() or tmp.stat().st_size == 0:
        tmp.unlink(missing_ok=True)
        err(f"{url}: monolith produced no output — skipped")
        return "skip"
    if tmp.stat().st_size > SIZE_CAP:
        size_mb = tmp.stat().st_size // (1024 * 1024)
        tmp.unlink(missing_ok=True)
        err(f"{url}: snapshot is {size_mb} MB, over the "
            f"{SIZE_CAP // (1024*1024)} MB cap — skipped "
            f"(commit deliberately with `git add -f`)")
        return "skip"
    inject_archive_metas(tmp)
    tmp.replace(dest)
    return "ok"


def extract_text_html(snapshot: Path, txt: Path) -> None:
    """Extract readable, block-separated text from an HTML snapshot. Block
    boundaries become blank lines so the archive page can render the text
    as paragraphs. On any failure an empty file is written."""
    try:
        from bs4 import BeautifulSoup
        soup = BeautifulSoup(snapshot.read_text(encoding="utf-8",
                                                errors="replace"),
                             "html.parser")
        for tag in soup(["script", "style", "noscript", "template", "head"]):
            tag.decompose()
        blocks = ["h1", "h2", "h3", "h4", "h5", "h6", "p", "li", "blockquote",
                  "pre", "tr", "figcaption", "section", "article", "div",
                  "header", "footer", "ul", "ol", "dl", "dd", "dt", "table",
                  "br", "hr"]
        # Append a NUL after every block element, then split the flattened
        # text on it: each chunk is the text between two block boundaries,
        # i.e. one paragraph. NUL never occurs in real HTML text content.
        sentinel = "\x00"
        for tag in soup.find_all(blocks):
            tag.append(sentinel)
        body = soup.body or soup
        paras = []
        for chunk in body.get_text(" ").split(sentinel):
            words = chunk.split()
            if words:
                paras.append(" ".join(words))
        txt.write_text("\n\n".join(paras) + "\n", encoding="utf-8")
    except Exception as exc:                           # noqa: BLE001
        err(f"{snapshot.name}: HTML text extraction failed ({exc}); "
            f"writing empty text sidecar")
        txt.write_text("", encoding="utf-8")


def classify_snapshot(path: Path) -> str:
    """Heuristic capture-quality grade: 'ok' / 'degraded' / 'js-required'.
    A near-empty snapshot is a JS app shell `--no-js` hollowed out; an
    <img> whose src is still remote (or only lazy-load attrs) is one
    monolith failed to inline. The author reviews the rendered snapshot
    before committing regardless — this only drives an automated flag."""
    try:
        from bs4 import BeautifulSoup
        soup = BeautifulSoup(path.read_text(encoding="utf-8", errors="replace"),
                             "html.parser")
        for tag in soup(["script", "style", "noscript", "template"]):
            tag.decompose()
        body = soup.body or soup
        if len(body.get_text(" ", strip=True)) < 200:
            return "js-required"
        remote = 0
        for img in body.find_all("img"):
            src = (img.get("src") or "").strip()
            if src.startswith(("http://", "https://")):
                remote += 1
            elif not src and (img.get("data-src") or img.get("data-lazy-src")
                              or img.get("srcset")):
                remote += 1
        return "degraded" if remote else "ok"
    except Exception:                                  # noqa: BLE001
        return "degraded"


# ---------------------------------------------------------------------------
# Equivalent-URL aliases
# ---------------------------------------------------------------------------

# Query parameters whose presence/absence is semantically irrelevant — a
# citation written with `?utm_source=…` should match the canonical form.
# Non-tracking parameters (`?v=`, `?id=`, Wayback timestamps) are
# load-bearing and must be preserved.
TRACKING_PARAMS = frozenset({
    "utm_source", "utm_medium", "utm_campaign", "utm_term", "utm_content",
    "fbclid", "gclid", "mc_eid", "mc_cid", "ref", "igshid",
    "_hsenc", "_hsmi", "mkt_tok",
})

# Matches @https://arxiv.org/(abs|pdf)/<id>[v<n>][.pdf]@ — the family of
# forms a single paper has in the wild.
_ARXIV_RE = re.compile(
    r"(https?://arxiv\.org/)(abs|pdf)/([\w.]+?)(v\d+)?(\.pdf)?$"
)


def strip_tracking(url: str) -> str:
    """Remove tracking query parameters, leaving every other parameter in
    place, spelled and ordered exactly as it was. Works on the text, as
    @stripTracking@ in @build/ArchiveIndex.hs@ does: parsing and
    re-encoding the query turned `a%20b` into `a+b` and `flag` into
    `flag=`, so the two sides disagreed about which URLs were the same."""
    path, sep, query = url.partition("?")
    if not sep:
        return url
    kept = [p for p in query.split("&") if p.split("=", 1)[0] not in TRACKING_PARAMS]
    return f"{path}?{'&'.join(kept)}" if kept else path


def arxiv_aliases(url: str) -> set[str]:
    """For an arXiv URL, the set of equivalent forms: abs ↔ pdf, with and
    without version, with and without trailing @.pdf@. Empty for any URL
    that isn't arXiv."""
    m = _ARXIV_RE.match(url)
    if not m:
        return set()
    scheme_host, _kind, paper_id, version, _ext = m.groups()
    out: set[str] = set()
    for kind in ("abs", "pdf"):
        for ver in ("", version or ""):
            tails = (".pdf", "") if kind == "pdf" else ("",)
            for tail in tails:
                out.add(f"{scheme_host}{kind}/{paper_id}{ver}{tail}")
    return out


def url_aliases(url: str) -> list[str]:
    """The equivalent-URL set: tracking parameters stripped, http/https
    folded, trailing slashes tolerated, arXiv abs/pdf/versioned forms
    expanded. The canonical URL itself is omitted (it is the index key)."""
    out: set[str] = {url, strip_tracking(url)}
    for u in list(out):
        if u.startswith("https://"):
            out.add("http://" + u[len("https://"):])
        elif u.startswith("http://"):
            out.add("https://" + u[len("http://"):])
    for u in list(out):
        out.add(u.rstrip("/"))
    for u in list(out):
        out.update(arxiv_aliases(u))
    out.discard(url)
    return sorted(out)


def arxiv_canonical(url: str) -> str:
    """The canonical form of an arXiv URL: @https://arxiv.org/abs/<id>@
    with no version and no @.pdf@. Non-arXiv passes through. Works as the
    Haskell-side @arxivCanonical@ in @build/ArchiveIndex.hs@ does, on
    whatever follows `abs/` or `pdf/`, so an old-style id
    (`hep-th/9901001v2`) is canonicalized too; @_ARXIV_RE@ does not take
    those, and it is only for listing a new-style id's aliases."""
    for prefix in ("https://arxiv.org/", "http://arxiv.org/"):
        if url.startswith(prefix):
            kind, slash, paper = url[len(prefix):].partition("/")
            if kind in ("abs", "pdf") and slash:
                paper = paper.removesuffix(".pdf")
                head, v, version = paper.rpartition("v")
                if v and version.isascii() and version.isdigit():
                    paper = head
                return f"https://arxiv.org/abs/{paper}"
    return url


def normalize_url(url: str) -> str:
    """The canonical form for *matching* — drop fragment, strip tracking,
    fold http→https, arXiv-canonicalise, trim trailing slashes. Mirrors
    @normalizeUrl@ in @build/ArchiveIndex.hs@ so removal enforcement and
    duplicate detection use the same equivalence the link-annotation
    filter uses; tests/test_archive.py runs a table of URLs through both
    (`site normalize-url`)."""
    no_frag = url.split("#", 1)[0]
    clean = strip_tracking(no_frag)
    if clean.startswith("http://"):
        clean = "https://" + clean[len("http://"):]
    canonical = arxiv_canonical(clean)
    return canonical.rstrip("/")


def _is_tracked_and_clean(*paths: Path) -> bool:
    """True if every path is tracked by git AND has no uncommitted
    changes — i.e. its committed bytes are recoverable via @git log -S@
    once a refresh replaces it. False on any git error (uninitialised
    repo, missing git binary, dirty/untracked file)."""
    str_paths = [str(p) for p in paths]
    try:
        for p in str_paths:
            rc = subprocess.run(
                ["git", "ls-files", "--error-unmatch", "--", p],
                cwd=str(SITE_ROOT),
                capture_output=True,
            ).returncode
            if rc != 0:
                return False
        rc = subprocess.run(
            ["git", "diff", "--quiet", "HEAD", "--", *str_paths],
            cwd=str(SITE_ROOT),
            capture_output=True,
        ).returncode
        return rc == 0
    except FileNotFoundError:
        return False


# ---------------------------------------------------------------------------
# fetch subcommand
# ---------------------------------------------------------------------------

_WAYBACK_CAPTURE_RE = re.compile(
    r"^(https?://web\.archive\.org/web/)(\d{4,14})(/.+)$")


def wayback_raw_url(capture: str) -> str:
    """The raw-bytes form of a Wayback capture URL: the `id_` flag after
    the timestamp serves the original response bytes, without the Wayback
    toolbar or link rewriting. An unexpected shape passes through."""
    m = _WAYBACK_CAPTURE_RE.match(capture)
    return f"{m.group(1)}{m.group(2)}id_{m.group(3)}" if m else capture


def fetch_from_wayback(url: str, entry: dict, slug_dir: Path
                       ) -> tuple[str, Path, str, str] | None:
    """Fetch-time fallback for an original that is already dead: pull the
    most recent *existing* Wayback capture (raw bytes via `id_`). Returns
    (atype, artifact_path, capture_url, raw_url), or None when no capture
    exists or the capture fetch itself fails. Lookup only — a dead URL
    cannot be newly captured, so the fallback never creates third-party
    state."""
    capture = wayback_lookup(url)
    if capture is None:
        err(f"{url}: original unreachable and the Wayback Machine has no "
            f"capture — skipped")
        return None
    raw = wayback_raw_url(capture)
    # Wayback replays the original response headers as X-Archive-Orig-*;
    # a preserved noarchive directive is still the publisher's word, and
    # the dead original can no longer be asked directly.
    preserved = (probe_headers(raw).get("x-archive-orig-x-robots-tag")
                 or "").lower()
    if "noarchive" in preserved:
        err(f"{url}: Wayback capture preserves X-Robots-Tag: noarchive "
            f"— skipped")
        return None
    # Re-resolve the artifact type against the raw capture: the original
    # is dead, so its own Content-Type probe degraded to the html
    # default; the capture replays the stored Content-Type.
    atype = detect_type(raw, entry.get("type"))
    if atype is None:
        return None
    art = slug_dir / ARTIFACT[atype]
    log(f"{url}: original dead — fetching Wayback capture {capture}")
    result = fetch_pdf(raw, art) if atype == "pdf" else fetch_html(raw, art)
    if result != "ok":
        err(f"{url}: Wayback capture fetch failed — skipped")
        return None
    return (atype, art, capture, raw)


def removed_canonical_forms() -> set[str]:
    """The takedowns in removed.yaml, in normalised form, so a
    tracking-laden variant cannot bypass one the author already recorded."""
    return {normalize_url(r["url"]) for r in load_yaml_list(REMOVED) if r.get("url")}


def validate_manifest(manifest: list[dict], removed_norms: set[str]) -> None:
    """Refuse a malformed manifest before any fetch I/O (exits on error)."""
    # Pre-scan validation: reject canonical-form duplicates *before* any
    # fetch I/O, so a first colliding entry never gets partially processed
    # while a second's duplicate check halts. The canonical URL and every
    # authored alias must be unique across the whole manifest — a shared
    # form would route one citation under two slugs. (An alias that
    # normalises to its own entry's URL is merely redundant: deduped here,
    # never an error.)
    seen: dict[str, str] = {}
    slugs: dict[str, str] = {}
    for entry in manifest:
        url = entry.get("url")
        if not url:
            continue
        # A slug names a directory under archive/. Two hosts can derive the
        # same one (alice.github.io/ and bob.github.io/ are both
        # "github-index"), and a `slug:` override could climb out of
        # archive/ ("../x"); either is a manifest error, caught before any
        # fetch rather than as a confusing "URL changed" failure later.
        slug = entry_slug(entry)
        if not SLUG_RE.fullmatch(slug):
            err(f"manifest entry {url!r}: slug {slug!r} must be lowercase "
                f"letters, digits and hyphens (at most 64), starting with a "
                f"letter or digit")
            sys.exit(1)
        if slug in slugs:
            err(f"manifest: {url!r} and {slugs[slug]!r} both archive under "
                f"slug {slug!r}; give one of them a `slug:`")
            sys.exit(1)
        slugs[slug] = url
        keys = {normalize_url(url)}
        keys |= {normalize_url(a) for a in entry_aliases(entry)}
        for norm in sorted(keys):
            if norm in seen:
                err(f"manifest: {url!r} and {seen[norm]!r} share the "
                    f"canonical form {norm!r} (directly or via `aliases:`). "
                    f"Drop one or distinguish them; the link archive "
                    f"cannot route both under one slug.")
                sys.exit(1)
            seen[norm] = url
        # A takedown recorded in removed.yaml is enforced against aliases
        # too — re-listing a removed work as an alias of another entry
        # would republish it under that entry's slug. (The canonical URL
        # gets the same check, with fetch-specific wording, in the
        # per-entry loop below.)
        hit = keys & removed_norms
        if hit:
            err(f"manifest entry {url!r}: canonical form {sorted(hit)[0]!r} "
                f"(direct or via `aliases:`) is recorded in "
                f"archive/removed.yaml as a deliberate takedown. To "
                f"re-archive it, remove the corresponding line from "
                f"removed.yaml first.")
            sys.exit(1)


def archive_entry(entry: dict, removed_norms: set[str], *,
                  wayback_fallback: bool = True) -> dict | None:
    """Archive one manifest entry with a `url:`: fetch its artifact if it is
    not present, verify a committed one, refresh its text, write its
    provenance once. Returns its archive-index.json record, or None when it
    was skipped this run (network, cap, undetectable type: retried next
    build). Integrity failures exit."""
    url = entry["url"]
    norm = normalize_url(url)

    # A manifest URL whose canonical form matches a removed entry is a
    # deliberate takedown; never silently re-archive it. The author
    # either removes the line from removed.yaml ("I want it back") or
    # from the manifest.
    if norm in removed_norms:
        err(f"manifest URL {url!r} (canonical {norm!r}) is recorded in "
            f"archive/removed.yaml as a deliberate takedown. To re-archive "
            f"it, remove the corresponding line from removed.yaml first.")
        sys.exit(1)

    slug = entry_slug(entry)
    slug_dir = ARCHIVE_DIR / slug
    prov_path = slug_dir / "PROVENANCE.json"

    # --- resolve the artifact type ------------------------------------
    # An archived entry's type is fixed in PROVENANCE.json; a new entry
    # is detected from the manifest / URL / Content-Type.
    prov = None
    if prov_path.exists():
        prov = json.loads(prov_path.read_text(encoding="utf-8"))
        if prov.get("url") != url:
            err(f"{slug}: manifest URL changed "
                f"({prov.get('url')!r} -> {url!r}). A committed artifact "
                f"is never silently re-fetched; to deliberately "
                f"re-snapshot, run `archive.py refresh {slug}`.")
            sys.exit(1)
        atype = prov.get("type", "pdf")
    else:
        atype = detect_type(url, entry.get("type"))
        if atype is None:
            return None

    art       = slug_dir / ARTIFACT[atype]
    txt       = slug_dir / TEXTFILE[atype]
    txt_stamp = slug_dir / (TEXTFILE[atype] + ".sha256")

    # --- integrity guard (fatal): a committed artifact must verify,
    #     and a lost artifact must not be silently re-fetched. -------
    if prov is not None:
        if art.exists():
            live = sha256_of(art)
            if live != prov.get("sha256"):
                err(f"{slug}: {art.name} SHA-256 mismatch "
                    f"(recorded {prov.get('sha256')}, found {live}) "
                    f"— the committed artifact is corrupt or was replaced")
                sys.exit(1)
        else:
            err(f"{slug}: PROVENANCE.json is committed but {art.name} "
                f"is missing. The committed artifact has been lost; "
                f"restore it from git before rebuilding. A refresh "
                f"requires a present, verified prior snapshot.")
            sys.exit(1)

    # --- fetch the artifact if it is not already present --------------
    wb_capture: str | None = None    # Wayback capture used, if any
    wb_raw: str | None = None        # ... and its raw id_ form
    if not art.exists():
        slug_dir.mkdir(parents=True, exist_ok=True)
        log(f"fetching {url}  [{atype}]")
        result = (fetch_pdf(url, art) if atype == "pdf"
                  else fetch_html(url, art))
        if result == "dead" and wayback_fallback:
            # The original is already gone at first fetch — pull the
            # most recent existing Wayback capture instead. Only for
            # *dead* originals: a noarchive refusal or an over-cap
            # skip must never be circumvented via a third-party copy.
            fb = fetch_from_wayback(url, entry, slug_dir)
            if fb is not None:
                atype, art, wb_capture, wb_raw = fb
                txt       = slug_dir / TEXTFILE[atype]
                txt_stamp = slug_dir / (TEXTFILE[atype] + ".sha256")
                result = "ok"
        if result != "ok":
            return None
    else:
        log(f"{slug}: artifact present, skipping fetch")

    digest = sha256_of(art)

    # --- regenerate text when the artifact changed (or .txt absent) ---
    stale = (not txt.exists()
             or not txt_stamp.exists()
             or txt_stamp.read_text(encoding="utf-8").strip() != digest)
    if stale:
        if atype == "pdf":
            extract_text_pdf(art, txt)
        else:
            extract_text_html(art, txt)
        txt_stamp.write_text(digest + "\n", encoding="utf-8")

    # --- write PROVENANCE.json (once; stable thereafter) --------------
    if prov is None:
        quality = "ok" if atype == "pdf" else classify_snapshot(art)
        prov = {
            "url": url,
            "slug": slug,
            "title": entry.get("title") or slug,
            "type": atype,
            "artifact": ARTIFACT[atype],
            "sha256": digest,
            "previous-sha256": None,
            "bytes": art.stat().st_size,
            "archived": datetime.date.today().isoformat(),
            "source-date": entry.get("source-date"),
            "snapshot-quality": quality,
            # When the fallback fired, the capture is already known —
            # cmd_wayback (which targets `wayback: null`) skips it,
            # correctly: a dead original cannot be re-submitted.
            "wayback": wb_capture,
        }
        if wb_raw is not None:
            # The snapshot's bytes came from the Wayback capture, not
            # the (dead) original. Recorded so the provenance never
            # implies a first-hand fetch that did not happen.
            prov["fetched-from"] = wb_raw
        atomic_write_json(prov_path, prov)
        origin = " via Wayback" if wb_raw else ""
        log(f"{slug}: archived{origin} [{atype}, {quality}] "
            f"({prov['bytes']} bytes)")

    # --- contribute to the Hakyll index -------------------------------
    # Generated equivalents of the canonical URL, plus the authored
    # `aliases:` (each with its own generated equivalents) — so a DOI
    # alias's http:// form matches just like the canonical's would.
    alias_set = set(url_aliases(url))
    for authored in entry_aliases(entry):
        alias_set.add(authored)
        alias_set.update(url_aliases(authored))
    alias_set.discard(url)
    return {
        "slug": slug,
        "type": prov.get("type", atype),
        "title": prov.get("title", slug),
        "aliases": sorted(alias_set),
    }


def cmd_fetch(wayback_fallback: bool = True) -> int:
    manifest = load_yaml_list(MANIFEST)
    removed_norms = removed_canonical_forms()
    validate_manifest(manifest, removed_norms)

    index: dict[str, dict] = {}
    skipped = 0
    for entry in manifest:
        url = entry.get("url")
        if not url:
            err("manifest entry without a `url:` — skipped")
            skipped += 1
            continue
        record = archive_entry(entry, removed_norms, wayback_fallback=wayback_fallback)
        if record is None:
            skipped += 1
        else:
            index[url] = record

    # archive-index.json is always rewritten to mirror the manifest exactly.
    atomic_write_json(INDEX_OUT, index)
    log(f"wrote {INDEX_OUT.relative_to(SITE_ROOT)} ({len(index)} entries)")

    if skipped:
        err(f"{skipped} entr{'y' if skipped == 1 else 'ies'} skipped "
            f"(network / cap / missing url) — retried next build")
    return 0


# ---------------------------------------------------------------------------
# refresh subcommand — deliberate re-snapshot of one entry
# ---------------------------------------------------------------------------

def cmd_refresh(argv: list[str]) -> int:
    """Deliberately re-snapshot a single entry.

    Two invariants:

      * The prior snapshot is *recoverable* — refresh refuses to replace
        an artifact whose committed bytes git does not have, so the
        recorded @previous-sha256@ always points at something
        retrievable via @git log -S@. Commit the current snapshot first.

      * The replacement is *atomic across every exit path* — slug dir and
        @data/archive-index.json@ are both staged aside; any failure
        (transient fetch error, fatal integrity exit, exception,
        interruption) restores both. We never end up with no snapshot
        and never leave the index pointing at a discarded state.

    The only way an @archive.py@ invocation replaces a committed artifact
    — @cmd_fetch@ itself refuses to."""
    if not argv:
        err("refresh: pass a slug "
            "(e.g. `archive.py refresh nist-fips-203`)")
        return 2
    slug = argv[0]

    manifest = load_yaml_list(MANIFEST)
    entry = next((e for e in manifest
                  if e.get("url") and entry_slug(e) == slug), None)
    if entry is None:
        err(f"refresh: {slug!r} is not in archive/manifest.yaml")
        return 2

    slug_dir = ARCHIVE_DIR / slug
    prov_path = slug_dir / "PROVENANCE.json"
    prev_sha: str | None = None
    if prov_path.exists():
        try:
            prev = json.loads(prov_path.read_text(encoding="utf-8"))
            prev_sha = prev.get("sha256")
            prev_art_name = prev.get("artifact") or ""
            prev_artifact = slug_dir / prev_art_name
        except Exception as exc:                       # noqa: BLE001
            err(f"refresh: cannot parse prior provenance for {slug}: {exc}")
            return 2
        # The prior snapshot must be committed and clean — otherwise
        # `previous-sha256` would point at bytes git can no longer give
        # back, breaking the auditable replacement contract. The empty-
        # artifact guard matters: without it prev_artifact would be the
        # slug directory itself, which exists() accepts and sha256_of
        # then crashes on with IsADirectoryError.
        if not prev_sha or not prev_art_name or not prev_artifact.is_file():
            err(f"refresh: prior snapshot for {slug} is incomplete; restore "
                f"its artifact and provenance before replacing it.")
            return 2
        live_sha = sha256_of(prev_artifact)
        if live_sha != prev_sha:
            err(f"refresh: prior snapshot for {slug} fails SHA-256 "
                f"verification (recorded {prev_sha}, found {live_sha}); "
                f"refusing to replace unverifiable bytes.")
            return 2
        if not _is_tracked_and_clean(prov_path, prev_artifact):
            err(f"refresh: the prior snapshot for {slug} "
                f"(archive/{slug}/{{PROVENANCE.json, "
                f"{prev_artifact.name}}}) has uncommitted changes or is "
                f"not tracked in git. Commit the current snapshot first "
                f"— otherwise its bytes cannot be recovered via "
                f"`git log -S` once replaced.")
            return 2

    # Stage the old snapshot AND the current archive-index.json aside —
    # the refresh rewrites the index's record, so a failed refresh
    # must roll both back.
    backup: Path | None = None
    if slug_dir.exists():
        backup = slug_dir.with_name(slug + ".refresh-backup")
        if backup.exists():
            err(f"refresh: recovery directory {backup.name} already exists; "
                f"resolve it before starting another refresh.")
            return 2
        slug_dir.rename(backup)
        log(f"refresh: staged old archive/{slug}/ aside as {backup.name}")

    index_existed = INDEX_OUT.exists()
    index_backup: Path | None = None
    if index_existed:
        index_backup = INDEX_OUT.with_suffix(".json.refresh-backup")
        if index_backup.exists():
            if backup is not None:
                backup.rename(slug_dir)
            err(f"refresh: recovery file {index_backup.name} already exists; "
                f"resolve it before starting another refresh.")
            return 2
        shutil.copy2(INDEX_OUT, index_backup)

    succeeded = False
    try:
        # This entry alone: a full fetch would also fetch every other
        # entry's missing artifact, and an unrelated entry's integrity
        # failure would roll this refresh back. The manifest is still
        # checked whole, as a fetch would.
        removed_norms = removed_canonical_forms()
        validate_manifest(manifest, removed_norms)
        # No Wayback fallback during a refresh: the author asked for a
        # fresh first-hand snapshot. If the original turns out to be dead,
        # the right outcome is "refresh fails, prior snapshot restored" —
        # not a silent downgrade of a committed first-hand snapshot to
        # third-party bytes. Adopting a Wayback copy stays a deliberate
        # act (a new entry whose original is already dead).
        record = archive_entry(entry, removed_norms, wayback_fallback=False)
        if record is not None:
            index = (json.loads(INDEX_OUT.read_text(encoding="utf-8"))
                     if INDEX_OUT.exists() else {})
            # Drop every record this slug had first: after a change of the
            # manifest URL its old record is keyed by the old URL, and its
            # aliases would go on resolving to the slug. Other entries stay.
            index = {u: r for u, r in index.items()
                     if not (isinstance(r, dict) and r.get("slug") == slug)}
            index[entry["url"]] = record
            atomic_write_json(INDEX_OUT, index)

        # Success requires a new PROVENANCE.json *and* its declared
        # artifact on disk.
        if record is not None and prov_path.exists():
            try:
                new_prov = json.loads(prov_path.read_text(encoding="utf-8"))
                art_name = new_prov.get("artifact", "")
                if art_name and (slug_dir / art_name).exists():
                    if prev_sha:
                        new_prov["previous-sha256"] = prev_sha
                        atomic_write_json(prov_path, new_prov)
                        log(f"refresh: recorded previous-sha256 "
                            f"{prev_sha[:12]}…")
                    succeeded = True
            except Exception:                              # noqa: BLE001
                succeeded = False
    finally:
        # Runs on every exit path — normal return, exception, SystemExit
        # from an integrity failure, KeyboardInterrupt. We always end with either a
        # complete new snapshot or the prior one restored, never neither.
        if succeeded:
            if backup is not None:
                shutil.rmtree(backup)
            if index_backup is not None:
                index_backup.unlink()
            log(f"refresh: {slug} re-snapshotted")
        else:
            if slug_dir.exists():
                shutil.rmtree(slug_dir)
            if backup is not None:
                backup.rename(slug_dir)
            if index_backup is not None:
                shutil.move(str(index_backup), str(INDEX_OUT))
            elif not index_existed:
                INDEX_OUT.unlink(missing_ok=True)
            err(f"refresh: re-snapshot of {slug} failed; the prior "
                f"snapshot has been restored.")

    return 0 if succeeded else 1


# ---------------------------------------------------------------------------
# wayback subcommand
# ---------------------------------------------------------------------------

def wayback_save(url: str) -> None:
    """Trigger a fresh Wayback capture via Save Page Now. Best-effort: any
    outcome is tolerated — the resulting URL is read back via the
    availability API (which also surfaces a pre-existing capture)."""
    # Quote only what can't appear raw in a request line (spaces,
    # control chars); URL structure (:/?&=#) passes through so Save
    # Page Now sees the original URL shape.
    req = urllib.request.Request(
        "https://web.archive.org/save/" + quote(url, safe=":/?&=#"),
        headers={"User-Agent": user_agent()})
    try:
        with urllib.request.urlopen(req, timeout=WAYBACK_TIMEOUT):
            pass
    except Exception as exc:                           # noqa: BLE001
        log(f"wayback: save request for {url} did not complete ({exc})")


def wayback_lookup(url: str) -> str | None:
    """Return the most recent Wayback Machine capture URL for `url`, or
    None if there is no capture (or the availability API is unreachable)."""
    api = ("https://archive.org/wayback/available?url="
           + quote(url, safe=""))
    req = urllib.request.Request(api, headers={"User-Agent": user_agent()})
    try:
        with urllib.request.urlopen(req, timeout=TIMEOUT) as resp:
            data = json.loads(resp.read().decode("utf-8"))
    except Exception as exc:                           # noqa: BLE001
        err(f"wayback: availability lookup failed for {url} ({exc})")
        return None
    snap = (data.get("archived_snapshots") or {}).get("closest") or {}
    if snap.get("available") and snap.get("url"):
        return snap["url"]
    return None


def cmd_wayback() -> int:
    """Submit every archived URL whose PROVENANCE.json has no `wayback`
    capture yet to the Wayback Machine, then backfill the returned capture
    URL. Never on the critical path of a build — a separate target. Always
    exits 0: a capture that does not come through is simply retried next
    run. URLs recorded in removed.yaml are skipped — a deliberate takedown
    must not be re-published to a third-party archive even if its manifest
    line is still present during the documented eviction sequence.
    """
    manifest = load_yaml_list(MANIFEST)
    removed_norms = removed_canonical_forms()
    backfilled = pending = 0

    for entry in manifest:
        url = entry.get("url")
        if not url or normalize_url(url) in removed_norms:
            continue
        slug = entry_slug(entry)
        prov_path = ARCHIVE_DIR / slug / "PROVENANCE.json"
        if not prov_path.exists():
            continue                       # not fetched yet — run `fetch` first
        prov = json.loads(prov_path.read_text(encoding="utf-8"))
        if prov.get("wayback"):
            continue                       # already has a capture recorded

        log(f"wayback: submitting {url}")
        wayback_save(url)
        capture = wayback_lookup(url)
        if capture:
            prov["wayback"] = capture
            atomic_write_json(prov_path, prov)
            log(f"{slug}: wayback -> {capture}")
            backfilled += 1
        else:
            log(f"{slug}: no Wayback capture available yet — retried next run")
            pending += 1

    log(f"wayback: {backfilled} backfilled, {pending} pending")
    return 0


# ---------------------------------------------------------------------------
# check subcommand — link-rot detection
# ---------------------------------------------------------------------------

def moved_meaningfully(orig: str, final: str) -> bool:
    """True if `final` (where the request actually landed after redirects)
    differs from `orig` by more than an http/https fold or a trailing slash
    — i.e. a real relocation, not benign canonicalisation."""
    def norm(u: str) -> str:
        u = u.split("#", 1)[0]
        if u.startswith("http://"):
            u = "https://" + u[len("http://"):]
        return u.rstrip("/")
    return norm(orig) != norm(final)


def probe_url(url: str) -> tuple[str, str | None]:
    """Probe a URL for reachability. Returns @(result, new_url)@ where
    result is 'ok' | 'moved' | 'fail'. HEAD first; any HEAD failure is
    confirmed with a ranged GET before it counts. Servers answer HEAD
    wrongly in more ways than 403/405/501: nvlpubs.nist.gov returns 404 to
    HEAD and 206 to GET for a live document, which was on course to be
    marked rotted and replaced by the local copy (audit C03)."""
    for method in ("HEAD", "GET"):
        headers = {"User-Agent": user_agent()}
        if method == "GET":
            headers["Range"] = "bytes=0-0"
        req = urllib.request.Request(url, method=method, headers=headers)
        try:
            with urllib.request.urlopen(req, timeout=TIMEOUT) as resp:
                final = resp.geturl()
                if moved_meaningfully(url, final):
                    return ("moved", final)
                return ("ok", None)
        except Exception as exc:               # noqa: BLE001 — HTTP error or network failure
            if isinstance(exc, urllib.error.HTTPError):
                exc.close()                    # it holds the response open
            if method == "HEAD":
                continue                       # confirm with GET
            return ("fail", None)
    return ("fail", None)


def next_state(prev: dict, result: str, new_url: str | None,
               today: datetime.date) -> dict:
    """Fold a probe result into an entry's state with asymmetric
    hysteresis. Recovery is immediate: one 'ok' returns straight to
    'live'. Rotting is slow: 'rotted' needs ROT_FAILS consecutive failures
    spanning at least ROT_DAYS days; below that the status is the
    inconclusive 'error'."""
    iso         = today.isoformat()
    prev_status = prev.get("status", "live")
    prev_cf     = prev.get("consecutive-failures", 0)
    prev_since  = prev.get("status-since", iso)

    if result == "ok":
        return {"status": "live", "checked": iso,
                "consecutive-failures": 0,
                "status-since": prev_since if prev_status == "live" else iso}

    if result == "moved":
        rec = {"status": "moved", "checked": iso,
               "consecutive-failures": 0,
               "status-since": prev_since if prev_status == "moved" else iso}
        if new_url:
            rec["new-url"] = new_url
        return rec

    # result == "fail" — increment the streak; 'status-since' marks its start.
    cf = prev_cf + 1
    streak_since = prev_since if prev_status in ("error", "rotted") else iso
    span = (today - datetime.date.fromisoformat(streak_since)).days
    status = "rotted" if (cf >= ROT_FAILS and span >= ROT_DAYS) else "error"
    return {"status": status, "checked": iso,
            "consecutive-failures": cf, "status-since": streak_since}


def cmd_check() -> int:
    """Probe every manifest URL and rewrite data/archive-state.json. The
    new state mirrors the manifest exactly (entries for dropped URLs are
    discarded). A slow network job — never on a build's critical path;
    always exits 0, since a probe failure is the signal, not an error.
    URLs listed in removed.yaml are skipped — the link-rot scanner should
    not keep probing a deliberately-removed work."""
    manifest = load_yaml_list(MANIFEST)
    removed_norms = removed_canonical_forms()
    old = {}
    if STATE_OUT.exists():
        try:
            old = json.loads(STATE_OUT.read_text(encoding="utf-8"))
        except Exception:                                  # noqa: BLE001
            old = {}

    to_probe = [e["url"] for e in manifest
                if e.get("url") and normalize_url(e["url"]) not in removed_norms]

    # Offline guard: when the canary is unreachable, no probe result below
    # is evidence about the *targets* — record nothing rather than a
    # spurious `fail` against every entry. Exit 0: an offline laptop is
    # not a unit failure; the scan simply retries on the next timer fire.
    if to_probe and probe_url(canary := check_canary())[0] == "fail":
        log(f"check: canary {canary} unreachable — offline or DNS "
            f"down; scan inconclusive, state left untouched")
        return 0

    today = datetime.date.today()
    state: dict[str, dict] = {}
    tally = {"live": 0, "moved": 0, "error": 0, "rotted": 0}

    for url in to_probe:
        result, new_url = probe_url(url)
        rec = next_state(old.get(url, {}), result, new_url, today)
        state[url] = rec
        tally[rec["status"]] = tally.get(rec["status"], 0) + 1
        note = f" -> {new_url}" if new_url else ""
        log(f"check: {url}  [{rec['status']}]{note}")

    atomic_write_json(STATE_OUT, state)
    log(f"check: {tally['live']} live, {tally['moved']} moved, "
        f"{tally['error']} error, {tally['rotted']} rotted "
        f"-> {STATE_OUT.relative_to(SITE_ROOT)}")
    return 0


# ---------------------------------------------------------------------------
# gc subcommand
# ---------------------------------------------------------------------------

def cmd_gc(ignore_orphans: bool) -> int:
    manifest = load_yaml_list(MANIFEST)
    removed = load_yaml_list(REMOVED)

    manifest_slugs = {entry_slug(e) for e in manifest if e.get("url")}
    removed_slugs = {r["slug"] for r in removed if r.get("slug")}

    if not ARCHIVE_DIR.exists():
        log("no archive/ directory — nothing to GC")
        return 0

    deleted = 0
    orphans: list[str] = []
    for child in sorted(ARCHIVE_DIR.iterdir()):
        if not child.is_dir():
            continue
        slug = child.name
        if slug in removed_slugs:
            shutil.rmtree(child)
            log(f"gc: removed archive/{slug}/ (in removed.yaml)")
            deleted += 1
        elif slug not in manifest_slugs:
            orphans.append(slug)

    for slug in orphans:
        err(f"gc: archive/{slug}/ is not in manifest.yaml and not in "
            f"removed.yaml — left intact. If you meant to evict it, add it "
            f"to removed.yaml first; if it is stale (a branch switch, a "
            f"rename), delete the directory by hand.")

    log(f"gc: {deleted} director{'y' if deleted == 1 else 'ies'} removed")
    if orphans and not ignore_orphans:
        err(f"gc: {len(orphans)} orphan(s) present — "
            f"resolve them or re-run with --ignore-orphans")
        return 1
    return 0


# ---------------------------------------------------------------------------
# suggest subcommand
# ---------------------------------------------------------------------------

_BIB_ENTRY_RE = re.compile(r"@(\w+)\s*\{\s*([^,\s{}]+)\s*,")
_BIB_FIELD_RE = re.compile(
    r"""^\s*(url|doi)\s*=\s*[{"]\s*([^}"]+?)\s*["}]""",
    re.IGNORECASE | re.MULTILINE)


def bib_citations(text: str) -> list[tuple[str, str]]:
    """(citation-key, URL) pairs from one .bib file's text. Per entry the
    `url` field wins; a DOI-only entry resolves to https://doi.org/{doi}.
    @comment/@string/@preamble blocks carry no citation and are skipped."""
    out: list[tuple[str, str]] = []
    entries = list(_BIB_ENTRY_RE.finditer(text))
    for i, m in enumerate(entries):
        kind, key = m.group(1).lower(), m.group(2)
        if kind in ("comment", "string", "preamble"):
            continue
        end = entries[i + 1].start() if i + 1 < len(entries) else len(text)
        fields = {f.group(1).lower(): f.group(2)
                  for f in _BIB_FIELD_RE.finditer(text[m.end():end])}
        url = fields.get("url")
        if not url and fields.get("doi"):
            url = "https://doi.org/" + fields["doi"]
        if url and url.startswith(("http://", "https://")):
            out.append((key, url))
    return out


def cmd_suggest() -> int:
    """Print works cited in data/*.bib but absent from the manifest, as
    manifest-ready lines. Read-only by design: tools never write
    manifest.yaml — the author reviews and copies lines by hand, so the
    manifest stays the *identity* of the archive, not a .bib cache."""
    manifest = load_yaml_list(MANIFEST)
    removed = load_yaml_list(REMOVED)
    covered = {normalize_url(e["url"])
               for e in manifest + removed if e.get("url")}
    for e in manifest:
        covered.update(normalize_url(a) for a in entry_aliases(e))

    bib_files = sorted((SITE_ROOT / "data").glob("*.bib"))
    if not bib_files:
        log("suggest: no data/*.bib files to scan")
        return 0

    # normalized URL -> first-seen verbatim URL + every citing entry, so
    # one work cited from three papers prints once, with all three citers.
    suggestions: dict[str, dict] = {}
    for bib in bib_files:
        for key, url in bib_citations(bib.read_text(encoding="utf-8")):
            norm = normalize_url(url)
            if norm in covered:
                continue
            s = suggestions.setdefault(norm, {"url": url, "cited": []})
            s["cited"].append(f"{bib.name}:{key}")

    scanned = ", ".join(b.name for b in bib_files)
    if not suggestions:
        log(f"suggest: nothing to add — every URL cited in {scanned} is "
            f"already archived or deliberately removed")
        return 0

    log(f"suggest: {len(suggestions)} cited work(s) not in the manifest "
        f"(scanned {scanned})")
    print()
    print("# Cited but not archived. Review each one, then copy the")
    print("# entries you want into archive/manifest.yaml.")
    for s in suggestions.values():
        print()
        for citer in s["cited"]:
            print(f"# cited by {citer}")
        print(f'- url: "{s["url"]}"')
    return 0


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def main(argv: list[str]) -> int:
    cmd = argv[0] if argv else "fetch"
    if cmd == "fetch":
        return cmd_fetch()
    if cmd == "refresh":
        return cmd_refresh(argv[1:])
    if cmd == "wayback":
        return cmd_wayback()
    if cmd == "check":
        return cmd_check()
    if cmd == "suggest":
        return cmd_suggest()
    if cmd == "gc":
        return cmd_gc(ignore_orphans="--ignore-orphans" in argv[1:])
    err(f"unknown subcommand {cmd!r} "
        f"(expected: fetch | refresh | wayback | check | suggest | gc)")
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
