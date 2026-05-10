---
title: Colophon
abstract: How this site is built.
---

This site is built with Ozymandias, a static site framework using Hakyll, Pandoc, and a custom pipeline of Haskell filters and JavaScript components.

## Stack

- **Build engine:** Haskell + [Hakyll](https://jaspervdj.be/hakyll/).
- **Markdown processing:** [Pandoc](https://pandoc.org/) with custom AST filters for sidenotes, dropcaps, smallcaps, wikilinks, math, code highlighting, and more.
- **Typography:** Spectral (body), Fira Sans (headings and UI), JetBrains Mono (code).
- **Search:** [Pagefind](https://pagefind.app/) for client-side full-text search.
- **Citations:** Pandoc citeproc with the Chicago Notes style.
- **Score reader:** Inline SVG score rendering for music compositions.

Configuration lives in `site.yaml` at the project root.

## Living documents

Every essay carries a small *Stability* signal in its metadata block. It answers the question "how settled is this piece?", and is computed from the page's git history rather than declared by hand:

- **volatile** — solo commit, or less than two weeks old.
- **revising** — under six commits and under three months old.
- **fairly stable** — under sixteen commits, or under one year old.
- **stable** — under thirty-one commits, or under two years old.
- **established** — anything beyond.

The classification is intentionally conservative: a fast burst of revisions early in a piece's life reads as *volatile* until enough calendar time has passed to demonstrate the text has settled. The *Last reviewed* date in the page footer is the most recent commit touching the file.

You can pin a page's stability by adding its source path to `IGNORE.txt` in the project root; the file is cleared after every successful build, so pins are one-shot. Authors may also override the auto-calculation with a `stability:` and `last-reviewed:` value in frontmatter.
