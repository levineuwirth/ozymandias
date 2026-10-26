#!/usr/bin/env python3
"""
embed.py — Build-time embedding pipeline.

Produces two outputs from _site/**/*.html:

  data/similar-links.json       Page-level similarity (for "Related" footer section)
  data/semantic-index.bin       Paragraph vectors as raw Float32 array (N × PARA_DIM)
  data/semantic-meta.json       Paragraph metadata: [{url, title, heading, excerpt}]

Two models, one process:

  * Pages use nomic-embed-text-v1.5 (768 dims) — build-time only, never
    shipped to the browser. Chosen for its well-separated cosine scores on
    small corpora, which keeps the MIN_SCORE gate meaningful so every essay
    reliably gets a "Related" footer section.

  * Paragraphs use all-MiniLM-L6-v2 (384 dims) — must match what the
    browser runs via transformers.js (static/js/semantic-search.js) since
    query vectors are dotted against the shipped index.

Called by `make build` when .venv exists. Failures are non-fatal.

Staleness: both passes are content-hash cached (data/embed-cache-*.npz),
so an unchanged site re-embeds nothing and loads no model. Extraction is
cached too (data/embed-cache-extract.json): a page is parsed only when
its bytes, footer build time aside, or the extractor changed. A content
hash rather than an mtime, which cannot see changed content behind an
unchanged mtime (the compression cache's lesson, audit X2).

Cost of a fully-warm run: every page is read and hashed, none parsed,
and torch / sentence-transformers are imported lazily only when a cache
miss actually requires embedding. Parsing every page with BeautifulSoup
had been nearly all of a no-change run (about 3.5 of 3.9 s).
"""

import hashlib
import json
import re
import sys
import zipfile
from pathlib import Path

import faiss
import numpy as np
from bs4 import BeautifulSoup

sys.path.insert(0, str(Path(__file__).resolve().parent))
import sitelib  # noqa: E402

# torch + sentence-transformers cost seconds of import time alone, so
# they are imported lazily inside the cache-miss branches below. A
# fully-warm run (no misses) never pays it.

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

SITE_ROOT      = sitelib.SITE_ROOT
SITE_DIR       = SITE_ROOT / "_site"
SIMILAR_OUT    = SITE_ROOT / "data" / "similar-links.json"
SEMANTIC_BIN   = SITE_ROOT / "data" / "semantic-index.bin"
SEMANTIC_META  = SITE_ROOT / "data" / "semantic-meta.json"
# Content-addressed caches, one per pass. Keyed by sha256 of the (prefixed)
# input text; invalidated wholesale on model name/revision/dim change.
# Gitignored — build artifacts, not source. Survive `make clean`.
PAGE_CACHE     = SITE_ROOT / "data" / "embed-cache-pages.npz"
PARA_CACHE     = SITE_ROOT / "data" / "embed-cache-paragraphs.npz"
# What extract_document returned for each page, keyed by the page's path
# and the hash of its bytes with the footer build time masked, under a
# fingerprint of this file and the parser (see extract_site).
EXTRACT_CACHE  = SITE_ROOT / "data" / "embed-cache-extract.json"

# Two models, deliberately split:
#
#   PARA_MODEL — embeds paragraphs for data/semantic-index.bin. This index
#   is fetched by the browser at /search/ and ranked against query vectors
#   computed client-side. The client (static/js/semantic-search.js) embeds
#   queries with MiniLM-L6-v2 via transformers.js, so the build-time model
#   must match exactly — both the architecture and the embedding dimension
#   are part of the wire contract.
#
#   PAGE_MODEL — embeds full pages for data/similar-links.json. This file
#   is consumed only at Hakyll-build time (SimilarLinks.hs) and never
#   shipped to the browser, so it is free to use a different, stronger
#   model. nomic-embed-text-v1.5 produces well-separated cosine scores on
#   small corpora (top neighbours at 0.7–0.9 instead of MiniLM's compressed
#   0.1–0.3), so the MIN_SCORE gate below is meaningful and every essay
#   reliably gets a "Related" footer section.
#
# Both pins are deliberate. Bump only when validating and re-run a full
# embed pass to refresh the corresponding output files.

PARA_MODEL_NAME     = "sentence-transformers/all-MiniLM-L6-v2"
PARA_MODEL_REVISION = "c9745ed1d9f207416be6d2e6f8de32d1f16199bf"
PARA_DIM            = 384

PAGE_MODEL_NAME     = "nomic-ai/nomic-embed-text-v1.5"
PAGE_MODEL_REVISION = "e9b6763023c676ca8431644204f50c2b100d9aab"
# The weights repo above declares its modeling code via auto_map in a
# SEPARATE repo (nomic-ai/nomic-bert-2048), which `revision=` does NOT
# pin — without this second pin, trust_remote_code executes whatever is
# at that repo's head at build time.
PAGE_MODEL_CODE_REVISION = "7710840340a098cfb869c4f65e87cf2b1b70caca"
PAGE_DIM            = 768
# Nomic requires task-prefixed input. Documents (corpus side) get
# "search_document: "; queries would get "search_query: ". similar-links
# only ever embeds documents, so the prefix is constant here.
PAGE_PREFIX         = "search_document: "

TOP_N          = 5      # similar-links: neighbours per page
MIN_SCORE      = 0.30   # similar-links: discard weak matches
MIN_PARA_CHARS = 80     # semantic: skip very short paragraphs
MAX_PARA_CHARS = 1000   # semantic: truncate before embedding

# /archive/ is the archive index — a list page that would dominate every
# entry's "Related" set; the individual /archive/<slug>/ pages stay in.
EXCLUDE_URLS = {"/search/", "/build/", "/404.html", "/feed.xml",
                "/music/feed.xml", "/archive/"}

# Whole subtrees kept out of the corpus. /source/ is the repository code
# mirror — source files, not content; left in, they pollute every page's
# "Related" set and semantic search (e.g. a template file surfacing as a
# neighbour, titled with its unrendered "$title$" placeholder).
# /drafts/ exists in _site only when a SITE_ENV=dev build (make dev /
# make watch) has written into the shared output dir. `make build` purges
# it before embedding, but a watch session running in parallel with a
# direct embed.py invocation can still race drafts into _site — draft
# text must never reach the public search index or Related sections.
EXCLUDE_PREFIXES = ("/source/", "/drafts/")

# Pages whose <body data-portal> are portal/landing pages — they aggregate
# excerpts from many entries and would otherwise dominate every page's
# "Related" set with high but uninformative scores. default.html sets the
# attribute when any of the `list-page`, `portal`, or `home` template flags
# is true, so adding `constField "portal" "true"` to a Hakyll rule (or
# `portal: true` to a content file's frontmatter) is enough to exclude it.
PORTAL_BODY_ATTR = "data-portal"

# lxml parses ~2x faster than html.parser and produces byte-identical
# extracted text on this corpus (verified across every page: identical
# page texts and paragraph lists, hence identical content hashes — the
# embed caches survive the switch). Fall back gracefully when absent.
try:
    import lxml  # noqa: F401
    HTML_PARSER = "lxml"
except ImportError:
    HTML_PARSER = "html.parser"


def atomic_write_bytes(path: Path, data: bytes) -> None:
    """Identical bytes are left alone: Hakyll judges an input changed by its
    mtime, so rewriting an unchanged similar-links.json made every page that
    reads it recompile, and re-copied, recompressed and re-signed the
    semantic pair, on every build (audit X3/D05)."""
    sitelib.atomic_write_bytes(path, data, skip_if_unchanged=True)


def atomic_write_text(path: Path, text: str) -> None:
    atomic_write_bytes(path, text.encode("utf-8"))


# ---------------------------------------------------------------------------
# Page-embedding cache
# ---------------------------------------------------------------------------
#
# Loading the nomic model and embedding 26 pages on CPU takes ~3 minutes
# every `make build`. Pages rarely change between builds — usually one
# essay is edited and everything else is identical. This cache stores
# one nomic vector per page content hash so unchanged pages are reused
# verbatim and only edited/new pages are re-embedded. A fully-warm cache
# skips the model load entirely.

def content_hash(text: str) -> str:
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def load_vec_cache(path: Path, model: str, revision: str,
                   dim: int) -> dict[str, np.ndarray]:
    """Load {hash: vector} from disk. Returns an empty dict if the cache
    is absent, unreadable, or pinned to a different model — in those
    cases save_vec_cache() will overwrite the stale file on next save."""
    if not path.exists():
        return {}
    try:
        npz = np.load(path, allow_pickle=False)
        if (npz["model"].item()    != model or
            npz["revision"].item() != revision or
            int(npz["dim"].item()) != dim):
            return {}
        hashes  = npz["hashes"]
        vectors = npz["vectors"]
        if vectors.shape != (len(hashes), dim):
            return {}
        return {h.item(): vectors[i] for i, h in enumerate(hashes)}
    except (OSError, KeyError, ValueError, EOFError,
            zipfile.BadZipFile) as e:
        print(f"embed.py: cache {path.name} unreadable ({e}) — discarding",
              file=sys.stderr)
        return {}


def save_vec_cache(path: Path, model: str, revision: str, dim: int,
                   cache: dict[str, np.ndarray]) -> None:
    """Atomically persist {hash: vector}. Empty cache writes an empty
    file so a subsequent load returns {} cleanly (instead of falling
    through to the "no file" path)."""
    if cache:
        hashes  = np.array(list(cache.keys()))
        vectors = np.stack(list(cache.values())).astype(np.float32)
    else:
        hashes  = np.array([], dtype="U64")
        vectors = np.zeros((0, dim), dtype=np.float32)
    # Pass an open file handle, not a path: np.savez_compressed appends
    # ".npz" to bare paths, which would mangle the temporary's name.
    with sitelib.atomic_path(path) as tmp, open(tmp, "wb") as f:
        np.savez_compressed(
            f,
            model=model,
            revision=revision,
            dim=dim,
            hashes=hashes,
            vectors=vectors,
        )


STRIP_SELECTORS = [
    "nav", "footer", "#toc", ".link-popup", "script", "style",
    ".page-meta-footer", ".metadata", "[data-pagefind-ignore]",
    # An archive page's Referenced by and Related: what every other page
    # keeps inside the stripped .page-meta-footer. Other pages' sentences
    # and titles are not this page's content; embedding them fed each
    # archive page's Related list back into its own embedding, and, since
    # footers come from files written between the compile passes
    # (build/FooterData.hs), re-embedded the page a build after any change.
    "section.archive-backlinks", "section.archive-related",
    # Inline-SVG metadata (matplotlib emits creator boilerplate and,
    # historically, a per-run <dc:date> timestamp). None of it is page
    # content, and the timestamp made every recompile a cache miss.
    "svg metadata",
]

# ---------------------------------------------------------------------------
# HTML parsing helpers
# ---------------------------------------------------------------------------

def _url_from_path(html_path: Path) -> str:
    rel = html_path.relative_to(SITE_DIR)
    if rel.name == "index.html":
        parent = str(rel.parent)
        if parent in (".", ""):
            return "/"
        return "/" + parent + "/"
    return "/" + str(rel)

def _clean_soup(soup: BeautifulSoup) -> None:
    for sel in STRIP_SELECTORS:
        for el in soup.select(sel):
            el.decompose()

def _title(soup: BeautifulSoup, url: str) -> str:
    h1 = soup.find("h1")
    if h1:
        return h1.get_text(" ", strip=True)
    tag = soup.find("title")
    raw = tag.get_text(" ", strip=True) if tag else url
    return re.split(r"\s+[—–-]\s+", raw)[0].strip()

# ---------------------------------------------------------------------------
# Extraction — pages (similar-links) + paragraphs (semantic search)
# ---------------------------------------------------------------------------

def _excluded(url: str) -> bool:
    return url in EXCLUDE_URLS or url.startswith(EXCLUDE_PREFIXES)


def extract_document(html_path: Path) -> tuple[dict | None, list[dict]]:
    """Read and parse a page ONCE; return (page, paragraphs).

    Replaces the former extract_page / extract_paragraphs pair, which
    each re-read and re-parsed the same file. page is None (and
    paragraphs empty) for excluded / portal / non-content pages — the
    paragraph pass only ever ran when the page pass succeeded, and that
    behaviour is preserved here.
    """
    url = _url_from_path(html_path)
    if _excluded(url):
        return None, []
    return _extract(url, html_path.read_text(encoding="utf-8", errors="replace"))


def _extract(url: str, raw: str) -> tuple[dict | None, list[dict]]:
    soup = BeautifulSoup(raw, HTML_PARSER)

    body_tag = soup.body
    if body_tag is not None and body_tag.has_attr(PORTAL_BODY_ATTR):
        return None, []
    body = soup.select_one("#markdownBody")
    if body is None:
        return None, []

    title = _title(soup, url)
    _clean_soup(soup)

    text = re.sub(r"\s+", " ", body.get_text(" ", strip=True)).strip()
    if len(text) < 100:
        return None, []

    page = {"url": url, "title": title, "text": text}

    paras   = []
    heading = title  # track current section heading

    for el in body.find_all(["h1", "h2", "h3", "h4", "p", "li", "blockquote"]):
        if el.name in ("h1", "h2", "h3", "h4"):
            heading = el.get_text(" ", strip=True)
            continue
        # The notes list at the end of the body is each note's only copy
        # (build/Filters/Sidenotes.hs). A note's paragraphs and lists are
        # passages of their own; the item around them would repeat them.
        if "footnote-item" in (el.get("class") or []):
            continue
        ptext = re.sub(r"\s+", " ", el.get_text(" ", strip=True)).strip()
        if len(ptext) < MIN_PARA_CHARS:
            continue
        paras.append({
            "url":     url,
            "title":   title,
            # After the last section, but not of it.
            "heading": "Notes" if el.find_parent("section", class_="footnotes") else heading,
            "excerpt": ptext[:200] + ("…" if len(ptext) > 200 else ""),
            "text":    ptext[:MAX_PARA_CHARS],
        })

    return page, paras

# The footer's site-wide build time. Pages ship this span empty and the
# browser fills it in (since 2026-10-06); before that it was stamped into
# every page after every build. It is outside #markdownBody, so it never
# reaches the extracted text; masked so a stamped page and an empty one
# hash alike.
BUILD_TIME_SPAN = re.compile(
    rb'(<span class="footer-build-time" data-build-time>)[^<]*(</span>)')


def extractor_fingerprint() -> str:
    """Everything besides a page's bytes that decides its extraction:
    this file (the selectors, limits and code) and the parser's versions.
    Any change re-extracts every page once."""
    import bs4
    import soupsieve
    h = hashlib.sha256(Path(__file__).read_bytes())
    h.update(f"{HTML_PARSER} bs4 {bs4.__version__} soupsieve {soupsieve.__version__}".encode())
    if HTML_PARSER == "lxml":
        from lxml import etree
        h.update(f" lxml {etree.LXML_VERSION}".encode())
    return h.hexdigest()


def load_extract_cache(path: Path, fingerprint: str) -> dict:
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return {}
    if not isinstance(data, dict) or data.get("fingerprint") != fingerprint:
        return {}
    pages = data.get("pages")
    return pages if isinstance(pages, dict) else {}


def extract_site(cache_path: Path = EXTRACT_CACHE) -> tuple[list[dict], list[dict], int]:
    """Every indexable page under SITE_DIR and its paragraphs, in path
    order, as extract_document gives them, and how many pages had to be
    parsed. A page whose bytes (footer build time aside) and extractor are
    unchanged since the last run is taken from the cache, which keeps
    only the pages present now."""
    fingerprint = extractor_fingerprint()
    cached = load_extract_cache(cache_path, fingerprint)
    fresh: dict[str, dict] = {}
    pages: list[dict] = []
    paragraphs: list[dict] = []
    parsed = 0
    for html in sorted(SITE_DIR.rglob("*.html")):
        url = _url_from_path(html)
        if _excluded(url):
            continue
        raw = html.read_bytes()
        key = hashlib.sha256(BUILD_TIME_SPAN.sub(rb"\1\2", raw)).hexdigest()
        rel = html.relative_to(SITE_DIR).as_posix()
        hit = cached.get(rel)
        if isinstance(hit, dict) and hit.get("key") == key:
            page, paras = hit["page"], hit["paras"]
        else:
            # As read_text would: invalid bytes replaced, newlines universal.
            text = raw.decode("utf-8", errors="replace").replace("\r\n", "\n").replace("\r", "\n")
            page, paras = _extract(url, text)
            parsed += 1
        fresh[rel] = {"key": key, "page": page, "paras": paras}
        if page is not None:
            pages.append(page)
            paragraphs.extend(paras)
    atomic_write_text(cache_path, json.dumps({"fingerprint": fingerprint, "pages": fresh},
                                             ensure_ascii=False))
    return pages, paragraphs, parsed


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> int:
    if not SITE_DIR.exists():
        print("embed.py: _site/ not found — skipping", file=sys.stderr)
        return 0

    # --- Extract pages + paragraphs in one pass ---
    print("embed.py: extracting pages…")
    pages, paragraphs, parsed = extract_site()
    print(f"embed.py: extraction: {parsed} page(s) parsed, the rest unchanged "
          f"({len(pages)} indexable)")

    if not pages:
        print("embed.py: no indexable pages found", file=sys.stderr)
        return 0

    # --- Similar-links (page level, nomic, content-hash cached) ---
    cache       = load_vec_cache(PAGE_CACHE, PAGE_MODEL_NAME,
                                 PAGE_MODEL_REVISION, PAGE_DIM)
    page_inputs = [PAGE_PREFIX + p["text"] for p in pages]
    hashes      = [content_hash(t) for t in page_inputs]
    miss_idxs   = [i for i, h in enumerate(hashes) if h not in cache]

    print(f"embed.py: pages: {len(pages) - len(miss_idxs)} cached / "
          f"{len(miss_idxs)} to embed")

    if miss_idxs:
        print(f"embed.py: loading {PAGE_MODEL_NAME}@{PAGE_MODEL_REVISION[:8]}…")
        # Lazy: pulls in torch; only paid when something must be embedded.
        from sentence_transformers import SentenceTransformer
        page_model = SentenceTransformer(
            PAGE_MODEL_NAME, revision=PAGE_MODEL_REVISION, trust_remote_code=True,
            # code_revision pins the auto_map modeling repo; it must reach
            # both AutoConfig and AutoModel.from_pretrained.
            model_kwargs={"code_revision": PAGE_MODEL_CODE_REVISION},
            config_kwargs={"code_revision": PAGE_MODEL_CODE_REVISION},
        )
        new_vecs = page_model.encode(
            [page_inputs[i] for i in miss_idxs],
            normalize_embeddings=True,
            show_progress_bar=True,
            batch_size=8,
        ).astype(np.float32)
        for i, vec in zip(miss_idxs, new_vecs):
            cache[hashes[i]] = vec
        # Drop the model before loading MiniLM below; sentence-transformers
        # holds the full weight tensor in RAM until GC runs.
        del page_model

    # Assemble page_vecs in the original pages[] order.
    page_vecs = np.stack([cache[h] for h in hashes]).astype(np.float32)

    # Prune the cache to only currently-present hashes so a deleted page
    # doesn't keep its vector around forever. Then persist.
    save_vec_cache(PAGE_CACHE, PAGE_MODEL_NAME, PAGE_MODEL_REVISION,
                   PAGE_DIM, {h: cache[h] for h in hashes})

    index = faiss.IndexFlatIP(page_vecs.shape[1])
    index.add(page_vecs)
    scores_all, indices_all = index.search(page_vecs, TOP_N + 1)

    similar: dict[str, list] = {}
    for i, page in enumerate(pages):
        neighbours = []
        for rank in range(TOP_N + 1):
            j, score = int(indices_all[i, rank]), float(scores_all[i, rank])
            if j == i or score < MIN_SCORE:
                continue
            neighbours.append({"url": pages[j]["url"], "title": pages[j]["title"],
                                "score": round(score, 4)})
            if len(neighbours) == TOP_N:
                break
        if neighbours:
            similar[page["url"]] = neighbours

    atomic_write_text(SIMILAR_OUT, json.dumps(similar, ensure_ascii=False, indent=2))
    print(f"embed.py: wrote {len(similar)} similar-links entries")

    # --- Semantic index (paragraph level, MiniLM, content-hash cached) ---
    if not paragraphs:
        print("embed.py: no paragraphs extracted — skipping semantic index")
        return 0

    pcache      = load_vec_cache(PARA_CACHE, PARA_MODEL_NAME,
                                 PARA_MODEL_REVISION, PARA_DIM)
    para_inputs = [p["text"] for p in paragraphs]
    para_hashes = [content_hash(t) for t in para_inputs]
    para_miss   = [i for i, h in enumerate(para_hashes) if h not in pcache]

    print(f"embed.py: paragraphs: {len(paragraphs) - len(para_miss)} cached / "
          f"{len(para_miss)} to embed")

    if para_miss:
        print(f"embed.py: loading {PARA_MODEL_NAME}@{PARA_MODEL_REVISION[:8]}…")
        # Lazy: pulls in torch; only paid when something must be embedded.
        from sentence_transformers import SentenceTransformer
        para_model = SentenceTransformer(PARA_MODEL_NAME,
                                         revision=PARA_MODEL_REVISION)
        new_para_vecs = para_model.encode(
            [para_inputs[i] for i in para_miss],
            normalize_embeddings=True,
            show_progress_bar=True,
            batch_size=64,
        ).astype(np.float32)
        for i, vec in zip(para_miss, new_para_vecs):
            pcache[para_hashes[i]] = vec
        del para_model

    # Assemble in original paragraph order; prune + persist the cache.
    para_vecs = np.stack([pcache[h] for h in para_hashes]).astype(np.float32)
    save_vec_cache(PARA_CACHE, PARA_MODEL_NAME, PARA_MODEL_REVISION,
                   PARA_DIM, {h: pcache[h] for h in para_hashes})

    atomic_write_bytes(SEMANTIC_BIN, para_vecs.tobytes())

    meta = [{"url": p["url"], "title": p["title"],
             "heading": p["heading"], "excerpt": p["excerpt"]}
            for p in paragraphs]
    atomic_write_text(SEMANTIC_META, json.dumps(meta, ensure_ascii=False))

    print(f"embed.py: wrote {len(paragraphs)} paragraphs to semantic index "
          f"({SEMANTIC_BIN.stat().st_size // 1024} KB)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
