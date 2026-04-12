#!/usr/bin/env python3
"""
embed.py — Build-time embedding pipeline.

Produces two outputs from _site/**/*.html:

  data/similar-links.json       Page-level similarity (for "Related" footer section)
  data/semantic-index.bin       Paragraph vectors as raw Float32 array (N × DIM)
  data/semantic-meta.json       Paragraph metadata: [{url, title, heading, excerpt}]

Both use all-MiniLM-L6-v2 (384 dims) — the same model shipped to the browser
via transformers.js for query-time semantic search.

Called by `make build` when .venv exists. Failures are non-fatal.
Staleness check: skips if all output files are newer than every HTML in _site/.
"""

import json
import re
import sys
from pathlib import Path

import faiss
import numpy as np
from bs4 import BeautifulSoup
from sentence_transformers import SentenceTransformer

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

REPO_ROOT      = Path(__file__).parent.parent
SITE_DIR       = REPO_ROOT / "_site"
SIMILAR_OUT    = REPO_ROOT / "data" / "similar-links.json"
SEMANTIC_BIN   = REPO_ROOT / "data" / "semantic-index.bin"
SEMANTIC_META  = REPO_ROOT / "data" / "semantic-meta.json"

MODEL_NAME     = "all-MiniLM-L6-v2"
DIM            = 384

TOP_N          = 5      # similar-links: neighbours per page
MIN_SCORE      = 0.30   # similar-links: discard weak matches
MIN_PARA_CHARS = 80     # semantic: skip very short paragraphs
MAX_PARA_CHARS = 1000   # semantic: truncate before embedding

EXCLUDE_URLS = {"/search/", "/build/", "/404.html", "/feed.xml", "/music/feed.xml"}

STRIP_SELECTORS = [
    "nav", "footer", "#toc", ".link-popup", "script", "style",
    ".page-meta-footer", ".metadata", "[data-pagefind-ignore]",
]

# ---------------------------------------------------------------------------
# Staleness check
# ---------------------------------------------------------------------------

def needs_update() -> bool:
    outputs = [SIMILAR_OUT, SEMANTIC_BIN, SEMANTIC_META]
    if not all(p.exists() for p in outputs):
        return True
    oldest = min(p.stat().st_mtime for p in outputs)
    return any(html.stat().st_mtime > oldest for html in SITE_DIR.rglob("*.html"))

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
# Page-level extraction  (for similar-links)
# ---------------------------------------------------------------------------

def extract_page(html_path: Path) -> dict | None:
    raw  = html_path.read_text(encoding="utf-8", errors="replace")
    soup = BeautifulSoup(raw, "html.parser")
    url  = _url_from_path(html_path)

    if url in EXCLUDE_URLS:
        return None
    body = soup.select_one("#markdownBody")
    if body is None:
        return None

    title = _title(soup, url)
    _clean_soup(soup)

    text = re.sub(r"\s+", " ", body.get_text(" ", strip=True)).strip()
    if len(text) < 100:
        return None

    return {"url": url, "title": title, "text": text}

# ---------------------------------------------------------------------------
# Paragraph-level extraction  (for semantic search)
# ---------------------------------------------------------------------------

def extract_paragraphs(html_path: Path, url: str, title: str) -> list[dict]:
    raw  = html_path.read_text(encoding="utf-8", errors="replace")
    soup = BeautifulSoup(raw, "html.parser")
    body = soup.select_one("#markdownBody")
    if body is None:
        return []

    _clean_soup(soup)

    paras    = []
    heading  = title  # track current section heading

    for el in body.find_all(["h1", "h2", "h3", "h4", "p", "li", "blockquote"]):
        if el.name in ("h1", "h2", "h3", "h4"):
            heading = el.get_text(" ", strip=True)
            continue
        text = re.sub(r"\s+", " ", el.get_text(" ", strip=True)).strip()
        if len(text) < MIN_PARA_CHARS:
            continue
        paras.append({
            "url":     url,
            "title":   title,
            "heading": heading,
            "excerpt": text[:200] + ("…" if len(text) > 200 else ""),
            "text":    text[:MAX_PARA_CHARS],
        })

    return paras

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> int:
    if not SITE_DIR.exists():
        print("embed.py: _site/ not found — skipping", file=sys.stderr)
        return 0

    if not needs_update():
        print("embed.py: all outputs up to date — skipping")
        return 0

    # --- Extract pages + paragraphs in one pass ---
    print("embed.py: extracting pages…")
    pages = []
    paragraphs = []

    for html in sorted(SITE_DIR.rglob("*.html")):
        page = extract_page(html)
        if page is None:
            continue
        pages.append(page)
        paragraphs.extend(extract_paragraphs(html, page["url"], page["title"]))

    if not pages:
        print("embed.py: no indexable pages found", file=sys.stderr)
        return 0

    # --- Load model once for both tasks ---
    print(f"embed.py: loading {MODEL_NAME}…")
    model = SentenceTransformer(MODEL_NAME)

    # --- Similar-links (page level) ---
    print(f"embed.py: embedding {len(pages)} pages…")
    page_vecs = model.encode(
        [p["text"] for p in pages],
        normalize_embeddings=True,
        show_progress_bar=True,
        batch_size=64,
    ).astype(np.float32)

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

    SIMILAR_OUT.parent.mkdir(parents=True, exist_ok=True)
    SIMILAR_OUT.write_text(json.dumps(similar, ensure_ascii=False, indent=2))
    print(f"embed.py: wrote {len(similar)} similar-links entries")

    # --- Semantic index (paragraph level) ---
    if not paragraphs:
        print("embed.py: no paragraphs extracted — skipping semantic index")
        return 0

    print(f"embed.py: embedding {len(paragraphs)} paragraphs…")
    para_vecs = model.encode(
        [p["text"] for p in paragraphs],
        normalize_embeddings=True,
        show_progress_bar=True,
        batch_size=64,
    ).astype(np.float32)

    SEMANTIC_BIN.write_bytes(para_vecs.tobytes())

    meta = [{"url": p["url"], "title": p["title"],
             "heading": p["heading"], "excerpt": p["excerpt"]}
            for p in paragraphs]
    SEMANTIC_META.write_text(json.dumps(meta, ensure_ascii=False))

    print(f"embed.py: wrote {len(paragraphs)} paragraphs to semantic index "
          f"({SEMANTIC_BIN.stat().st_size // 1024} KB)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
