#!/usr/bin/env python3
"""code-refs.py — build-time snapshots of the GitHub code that prose links to.

A link such as

    https://github.com/<owner>/<repo>/blob/<sha>/analysis/results.md

used to get the same hover popup as a link to the bare repository: the
repository's description and its star count, fetched live from
api.github.com. This tool fetches what the link actually points at, once,
and stores it in the site so the popup can show it from the same origin.

    fetch   Scan the published pages under content/ for GitHub blob /
            tree / commit links, fetch any snapshot that is missing, and
            rewrite code-refs/index.json. When every link resolved, end
            with gc, so the store holds exactly what the index references.
            Wired into `make build` and `make deploy`'s preflight.
    gc      Delete snapshots that no current link references
            (--dry-run lists them instead).

Everything under code-refs/ is published, so only links on pages the site
publishes are snapshotted: never content/drafts/, never a private name the
build refuses to publish (*.local.md, *.draft.md), never a page marked
`draft: true` or one inside a collection whose index.md is. With
GITHUB_TOKEN set (which can read private repositories), a private
repository is refused outright and its previous snapshot dropped.

What gets stored, under code-refs/github/<owner>/<repo>/<sha>/:

    commit.json          message, author, date, diffstat, changed files —
                         fetched for every commit any link resolves to, so
                         blob and tree popups can say which revision (and
                         when) they show
    blob/<path>.txt      the file's raw bytes. The .txt suffix is load-
                         bearing: an .html or .svg from someone else's
                         repository must never be served as a document
                         from this origin.
    tree/<path>.json     the directory listing, plus the README's opening
                         (tree/_root.json for the repository root)

Pinned links (a hex commit id in the URL) are immutable: they are fetched
once and never again, which is why the store is tracked in Git rather than
regenerated — it is also the only copy left if the upstream history is
rewritten or the repository disappears. Branch links (…/tree/main) are
re-resolved on every run and snapshotted at the commit the branch points
to at build time; the popup says "as of" that date.

Network failures are warnings, never build failures. A link whose fetch
failed keeps its previous snapshot if it had one, and simply has no code
popup otherwise (the next build retries).

Unauthenticated, the GitHub REST API allows 60 requests an hour. A pinned
link costs at most two requests, ever; raw file contents come from
raw.githubusercontent.com, which is not metered the same way. Set
GITHUB_TOKEN in the environment to raise the limit.

Limits (deliberate): only github.com; a branch name is taken to be the
first path segment after blob/ or tree/, so branch names containing '/'
are not recognised; files over MAX_BLOB_BYTES and binary files are skipped.
"""

from __future__ import annotations

import argparse
import datetime as dt
import http.client
import json
import os
import re
import shutil
import subprocess
import sys
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import sitelib  # noqa: E402
SITE_ROOT = sitelib.SITE_ROOT
CONTENT_DIR = SITE_ROOT / "content"
STORE_DIR = SITE_ROOT / "code-refs"
INDEX_PATH = STORE_DIR / "index.json"
PUBLIC_PREFIX = "/code-refs/"

MAX_BLOB_BYTES = 1_000_000
MAX_COMMIT_FILES = 100
MAX_README_CHARS = 4000
TIMEOUT = 20

# Owner and repository name as GitHub permits them; the rest of the path is
# taken up to the first character that cannot belong to a Markdown link
# target. The fragment is not captured — it selects within the snapshot
# (a line range or a heading) and is the popup's business, not the store's.
URL_RE = re.compile(
    r"https://github\.com/"
    r"(?P<owner>[A-Za-z0-9](?:[A-Za-z0-9-]{0,38}))/"
    r"(?P<repo>[A-Za-z0-9._-]+)/"
    r"(?P<kind>blob|tree|commit)/"
    r"(?P<rest>[^\s)\]>\"'#?]+)"
)
FULL_SHA_RE = re.compile(r"^[0-9a-f]{40}$")
SHORT_SHA_RE = re.compile(r"^[0-9a-f]{7,39}$")
README_RE = re.compile(r"^readme(\.(md|markdown|txt|rst))?$", re.IGNORECASE)


# ---------------------------------------------------------------------------
# Link discovery
# ---------------------------------------------------------------------------

def canonical_url(owner: str, repo: str, kind: str, rest: str) -> str:
    return f"https://github.com/{owner}/{repo}/{kind}/{rest}"


def parse_link(match: re.Match) -> dict | None:
    """Split a matched URL into its parts, or None if it is not usable."""
    owner, repo, kind = match["owner"], match["repo"], match["kind"]
    rest = match["rest"].rstrip(".,;:").rstrip("/")
    if not rest:
        return None
    ref, _, path = rest.partition("/")
    path = urllib.parse.unquote(path)
    # The path becomes a filesystem path under the store; refuse anything
    # that could step outside it.
    if path and any(seg in ("", ".", "..") for seg in path.split("/")):
        return None
    pinned = bool(FULL_SHA_RE.match(ref) or SHORT_SHA_RE.match(ref))
    if kind == "commit" and (path or not pinned):
        return None
    elif kind == "blob" and not path:
        return None
    return {
        "url": canonical_url(owner, repo, kind, rest),
        "kind": kind,
        "owner": owner,
        "repo": repo,
        "ref": ref,
        "path": path,
        "pinned": pinned,
    }


def discover_links(content_dir: Path | None = None) -> dict[str, dict]:
    """The GitHub code links on the published pages, as the build reads
    them (`site list-links`, build/PageScan.hs): the pages it publishes,
    and only real links, so a URL in a code block or in prose is not
    snapshotted for a tag that would never use it."""
    content_dir = content_dir or CONTENT_DIR
    targets = subprocess.check_output(
        [sitelib.site_binary(), "list-links", str(content_dir.resolve())],
        cwd=SITE_ROOT, text=True)
    links: dict[str, dict] = {}
    for target in targets.splitlines():
        m = URL_RE.match(target)
        link = parse_link(m) if m else None
        if link:
            links.setdefault(link["url"], link)
    return links


# ---------------------------------------------------------------------------
# HTTP
# ---------------------------------------------------------------------------

class FetchError(Exception):
    pass


class PrivateRepository(FetchError):
    """Readable only with the token: never snapshotted, never kept."""


def http_get(url: str, *, api: bool) -> bytes:
    headers = {"User-Agent": f"{sitelib.site_config()['host']} code-refs"}
    if api:
        headers["Accept"] = "application/vnd.github+json"
        token = os.environ.get("GITHUB_TOKEN")
        if token:
            headers["Authorization"] = f"Bearer {token}"
    req = urllib.request.Request(url, headers=headers)
    try:
        with urllib.request.urlopen(req, timeout=TIMEOUT) as resp:
            # The size cap is for file contents only. An API response is
            # read whole: a commit with large patches runs past a megabyte,
            # and a truncated body is unparseable JSON, refetched (and
            # failed) on every build.
            return resp.read(MAX_BLOB_BYTES + 1) if not api else resp.read()
    except urllib.error.HTTPError as e:
        hint = ""
        if e.code == 429 or (e.code == 403 and e.headers.get("x-ratelimit-remaining") == "0"):
            hint = " (API rate limit; set GITHUB_TOKEN)"
        raise FetchError(f"HTTP {e.code} for {url}{hint}") from None
    except (urllib.error.URLError, TimeoutError, OSError, http.client.HTTPException) as e:
        # HTTPException covers IncompleteRead, which is not an OSError and
        # would otherwise escape as a traceback and fail the build.
        raise FetchError(f"{url}: {e!r}") from None


def api_json(path: str):
    return json.loads(http_get("https://api.github.com" + path, api=True))


def quote_path(path: str) -> str:
    return urllib.parse.quote(path, safe="/")


# ---------------------------------------------------------------------------
# Snapshots
# ---------------------------------------------------------------------------

def sha_dir(owner: str, repo: str, sha: str) -> Path:
    return STORE_DIR / "github" / owner / repo / sha


def public(path: Path) -> str:
    return PUBLIC_PREFIX + path.relative_to(STORE_DIR).as_posix()


def write_json(path: Path, obj) -> None:
    # Unchanged bytes are left alone: every file here is copied into the
    # site, and a rewrite moves its mtime, which recopies, recompresses and
    # re-signs it.
    sitelib.atomic_write_bytes(
        path, (json.dumps(obj, indent=1, ensure_ascii=False) + "\n").encode(),
        skip_if_unchanged=True)


def ensure_commit(owner: str, repo: str, ref: str) -> dict:
    """Commit metadata for `ref`, from the store when the ref is a full SHA
    already fetched; otherwise from the API (which also resolves short SHAs
    and branch names to the full SHA)."""
    if FULL_SHA_RE.match(ref):
        cached = sha_dir(owner, repo, ref) / "commit.json"
        if cached.exists():
            return json.loads(cached.read_text())
    # per_page bounds the file list (and its patches) the API returns.
    data = api_json(
        f"/repos/{owner}/{repo}/commits/{urllib.parse.quote(ref, safe='')}"
        f"?per_page={MAX_COMMIT_FILES}"
    )
    files = data.get("files") or []
    commit = {
        "sha": data["sha"],
        "message": data["commit"]["message"],
        "author": (data["commit"].get("author") or {}).get("name", ""),
        "date": (data["commit"].get("committer") or {}).get("date", ""),
        "parents": [p["sha"] for p in data.get("parents", [])],
        "stats": data.get("stats") or {},
        "files_total": len(files),
        "files": [
            {k: f.get(k) for k in ("filename", "status", "additions", "deletions")}
            for f in files[:MAX_COMMIT_FILES]
        ],
    }
    path = sha_dir(owner, repo, commit["sha"]) / "commit.json"
    # A commit is immutable, but GitHub's answer about it is not: for a very
    # large commit the API sometimes answers before it has computed the
    # diff, with no files and zero stats. A branch link resolves again on
    # every build, so that answer used to replace the stored snapshot (and
    # the next build put the full one back). Only an answer listing at
    # least as many files replaces what is stored.
    try:
        stored = json.loads(path.read_text())
    except (OSError, ValueError):
        stored = None
    if isinstance(stored, dict) and stored.get("files_total", 0) > commit["files_total"]:
        return stored
    write_json(path, commit)
    return commit


def ensure_blob(owner: str, repo: str, sha: str, path: str) -> Path:
    dest = sha_dir(owner, repo, sha) / "blob" / (path + ".txt")
    if dest.exists():
        return dest
    raw = http_get(
        f"https://raw.githubusercontent.com/{owner}/{repo}/{sha}/{quote_path(path)}",
        api=False,
    )
    if len(raw) > MAX_BLOB_BYTES:
        raise FetchError(f"{path}: larger than {MAX_BLOB_BYTES} bytes, skipped")
    if b"\0" in raw[:8192]:
        raise FetchError(f"{path}: binary file, skipped")
    try:
        raw.decode("utf-8")
    except UnicodeDecodeError:
        raise FetchError(f"{path}: not UTF-8 text, skipped") from None
    sitelib.atomic_write_bytes(dest, raw)
    return dest


def ensure_tree(owner: str, repo: str, sha: str, path: str) -> Path:
    dest = sha_dir(owner, repo, sha) / "tree" / ((path or "_root") + ".json")
    if dest.exists():
        return dest
    api_path = f"/repos/{owner}/{repo}/contents"
    if path:
        api_path += "/" + quote_path(path)
    listing = api_json(api_path + "?ref=" + sha)
    if not isinstance(listing, list):
        raise FetchError(f"{path or '/'}: not a directory at {sha[:7]}")
    entries = sorted(
        ({"name": e["name"], "type": e["type"], "size": e.get("size", 0)} for e in listing),
        key=lambda e: (e["type"] != "dir", e["name"].lower()),
    )
    readme = None
    for e in entries:
        if e["type"] == "file" and README_RE.match(e["name"]):
            rpath = f"{path}/{e['name']}" if path else e["name"]
            try:
                text = http_get(
                    f"https://raw.githubusercontent.com/{owner}/{repo}/{sha}/{quote_path(rpath)}",
                    api=False,
                ).decode("utf-8", errors="replace")
                readme = {"name": e["name"], "text": text[:MAX_README_CHARS]}
            except FetchError as err:
                warn(f"README for {path or '/'}: {err}")
            break
    write_json(dest, {"path": path, "entries": entries, "readme": readme})
    return dest


_visibility: dict[tuple[str, str], bool] = {}


def refuse_private(owner: str, repo: str) -> None:
    """With GITHUB_TOKEN set the API can read private repositories, and
    their snapshots would be published like any other. Without a token a
    private repository is a 404 already, so no check is needed."""
    if not os.environ.get("GITHUB_TOKEN"):
        return
    key = (owner, repo)
    if key not in _visibility:
        _visibility[key] = bool(api_json(f"/repos/{owner}/{repo}").get("private"))
    if _visibility[key]:
        raise PrivateRepository(f"{owner}/{repo} is private; not snapshotted")


def snapshot(link: dict) -> dict:
    """Fetch (or find) the snapshot for one link; return its index entry."""
    owner, repo = link["owner"], link["repo"]
    refuse_private(owner, repo)
    commit = ensure_commit(owner, repo, link["ref"])
    sha = commit["sha"]
    kind = link["kind"]
    if kind == "blob":
        src = ensure_blob(owner, repo, sha, link["path"])
    elif kind == "tree":
        src = ensure_tree(owner, repo, sha, link["path"])
    else:
        src = sha_dir(owner, repo, sha) / "commit.json"
    return {
        "kind": kind,
        "host": "github",
        "owner": owner,
        "repo": repo,
        "ref": link["ref"],
        "sha": sha,
        "path": link["path"],
        "pinned": link["pinned"],
        "src": public(src),
        "commit": public(sha_dir(owner, repo, sha) / "commit.json"),
        "date": commit.get("date", ""),
        "fetched": dt.date.today().isoformat(),
    }


def snapshot_present(entry: dict) -> bool:
    return all(
        (STORE_DIR / entry[k][len(PUBLIC_PREFIX):]).exists() for k in ("src", "commit")
    )


# ---------------------------------------------------------------------------
# Commands
# ---------------------------------------------------------------------------

def warn(msg: str) -> None:
    print(f"code-refs: warning: {msg}", file=sys.stderr)


def load_index() -> dict:
    try:
        return json.loads(INDEX_PATH.read_text())
    except (OSError, ValueError):
        return {}


def cmd_fetch(_args) -> int:
    links = discover_links()
    old = load_index()
    new: dict[str, dict] = {}
    fetched = failed = 0
    private_repositories: set[tuple[str, str]] = set()
    for url, link in sorted(links.items()):
        prev = old.get(url)
        try:
            # A cached commit is immutable, but its repository's visibility
            # is not. Check before the pinned-snapshot fast path too.
            refuse_private(link["owner"], link["repo"])
            if prev and prev.get("pinned") and snapshot_present(prev):
                new[url] = prev
                continue
            entry = snapshot(link)
        except PrivateRepository as e:
            failed += 1
            private_repositories.add((link["owner"], link["repo"]))
            warn(str(e))
            continue
        except (FetchError, KeyError, ValueError) as e:
            failed += 1
            warn(str(e))
            if prev and snapshot_present(prev):
                new[url] = prev
            continue
        # A branch link re-resolves every run. When it still names the same
        # commit, keep the entry as it was, `fetched` included: rewriting
        # the date alone left index.json modified after every build day.
        if prev and same_snapshot(prev, entry) and snapshot_present(prev):
            new[url] = prev
        else:
            new[url] = entry
            fetched += 1
    if new != old:
        STORE_DIR.mkdir(exist_ok=True)
        write_json(INDEX_PATH, dict(sorted(new.items())))
    # Refusing an index entry is insufficient: every file in this store is
    # published. Remove confirmed-private repositories even when an unrelated
    # network failure prevents general garbage collection below.
    for owner, repo in sorted(private_repositories):
        directory = STORE_DIR / "github" / owner / repo
        if directory.exists():
            shutil.rmtree(directory)
            print(f"code-refs: removed private repository snapshot {owner}/{repo}")
    print(
        f"code-refs: {len(new)} linked snapshots"
        f" ({fetched} fetched, {failed} failed, {len(links) - len(new)} without a snapshot)"
    )
    # Everything under code-refs/ is published. Once every link has
    # resolved, drop what no link references any more (a moved branch's
    # old commit, a link taken out of a page, debris from an interrupted
    # write). After a failure the previous snapshots stay for the retry.
    if failed == 0:
        collect_garbage(dry_run=False)
    return 0


def same_snapshot(a: dict, b: dict) -> bool:
    return {k: v for k, v in a.items() if k != "fetched"} == {
        k: v for k, v in b.items() if k != "fetched"
    }


def cmd_gc(args) -> int:
    collect_garbage(dry_run=args.dry_run)
    return 0


def collect_garbage(*, dry_run: bool) -> None:
    index = load_index()
    keep = {
        (STORE_DIR / e[k][len(PUBLIC_PREFIX):]).resolve()
        for e in index.values() for k in ("src", "commit")
    }
    gh = STORE_DIR / "github"
    doomed = [p for p in gh.rglob("*") if p.is_file() and p.resolve() not in keep] if gh.exists() else []
    for p in doomed:
        print(("would remove " if dry_run else "removed ") + str(p.relative_to(STORE_DIR.parent)))
        if not dry_run:
            p.unlink()
    if not dry_run and gh.exists():
        for d in sorted((d for d in gh.rglob("*") if d.is_dir()), key=lambda d: -len(d.parts)):
            if not any(d.iterdir()):
                d.rmdir()


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    sub = ap.add_subparsers(dest="cmd", required=True)
    sub.add_parser("fetch", help="snapshot every linked GitHub blob/tree/commit")
    gc = sub.add_parser("gc", help="delete snapshots no link references")
    gc.add_argument("--dry-run", action="store_true")
    args = ap.parse_args(argv)
    return {"fetch": cmd_fetch, "gc": cmd_gc}[args.cmd](args)


if __name__ == "__main__":
    sys.exit(main())
