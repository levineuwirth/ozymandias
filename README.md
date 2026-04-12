# Ozymandias

A full-featured static site framework built with [Hakyll](https://jaspervdj.be/hakyll/) and [Pandoc](https://pandoc.org/). Designed for long-form writing, research, music, and creative work.

## What's included

- **Sidenotes** — footnotes render in the margin on wide screens, inline on mobile.
- **Epistemic profiles** — tag essays with confidence, evidence quality, importance, and stability; readers see a compact credibility signal before committing to read.
- **Backlinks** — two-pass wikilink resolution with automatic backlink sections.
- **Score reader** — swipeable SVG score viewer for music compositions.
- **Typography** — dropcaps, smallcaps auto-detection, abbreviation tooltips, old-style figures.
- **Math** — KaTeX rendering for inline and display equations.
- **Citations** — Pandoc citeproc with Chicago Notes; bibliography and further-reading sections.
- **Search** — Pagefind client-side full-text search.
- **Semantic search** — optional embedding pipeline (sentence-transformers + FAISS) for "similar links."
- **Settings** — dark mode, text size, focus mode, reduce motion.
- **Wikilinks** — `[[Page Name]]` and `[[Page Name|display text]]` syntax.
- **Atom feeds** — site-wide and per-section (e.g., music-only).
- **Library** — configurable portal taxonomy that groups content by tag hierarchy.
- **Version history** — git-derived stability heuristic with manual history annotations.
- **Reading mode** — dedicated layout for poetry and fiction.
- **GPG signing** — optional per-page detached signatures.

## Quickstart

```sh
# Clone and enter the repo
git clone <your-fork-url> my-site && cd my-site

# Edit your identity and navigation
$EDITOR site.yaml

# Build and serve locally (requires GHC 9.6+, cabal, pagefind)
make dev
```

`make dev` builds with drafts visible and starts a local server on `:8000`.
For production: `make build` (one-shot build into `_site/`).

## Prerequisites

- **GHC 9.6+** and **cabal-install** — for the Haskell build pipeline.
- **Pagefind** — client-side search index (`npm i -g pagefind` or via your package manager).
- **cwebp** (optional) — for automatic WebP image conversion (`pacman -S libwebp` / `apt install webp`).
- **Python 3.12+ and uv** (optional) — for the embedding pipeline (`uv sync` to set up).

## Configuration

All site identity, navigation, and taxonomy live in `site.yaml`:

```yaml
site-name:   "My Site"
site-url:    "https://example.com"
author-name: "Your Name"

nav:
  - { href: "/",             label: "Home"    }
  - { href: "/library.html", label: "Library" }

portals:
  - { slug: "writing", name: "Writing" }
  - { slug: "code",    name: "Code"    }
```

See the comments in `site.yaml` for the full schema.

## Project structure

```
build/          Haskell source — Hakyll rules, Pandoc filters, compilers
templates/      Hakyll HTML templates and partials
static/         CSS, JS, fonts, images (copied to _site/)
content/        Markdown source — essays, blog, poetry, fiction, music
data/           Bibliography, annotations, citation style
tools/          Shell/Python build-time utilities
site.yaml       Site-wide configuration
Makefile        Build, deploy, dev targets
```

## Content types

| Type        | Path                             | Template        |
|:------------|:---------------------------------|:----------------|
| Essay       | `content/essays/*.md`            | essay.html      |
| Blog post   | `content/blog/*.md`              | blog-post.html  |
| Poetry      | `content/poetry/*.md`            | reading.html    |
| Fiction     | `content/fiction/*.md`            | reading.html    |
| Composition | `content/music/<slug>/index.md`  | composition.html|
| Page        | `content/*.md`                   | page.html       |

## Deployment

The included `make deploy` target:
1. Runs `make clean && make build && make sign`
2. Rsyncs `_site/` to a VPS (configured via `.env`)
3. Pushes to the git remote

Set `VPS_USER`, `VPS_HOST`, and `VPS_PATH` in `.env` (gitignored).

## License

The framework code (everything outside `content/`) is [MIT](LICENSE). The demo content under `content/` is public domain. Your own content is yours — add whatever license you choose.

---

*"My name is Ozymandias, King of Kings; / Look on my Works, ye Mighty, and despair!"* — the name is a reminder that all frameworks are temporary, but the writing you put in them might not be.
