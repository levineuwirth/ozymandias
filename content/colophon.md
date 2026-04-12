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
