---
title: A Feature Tour
date: 2026-04-12
abstract: A brief tour of the typography, structural, and analytic features available in an Ozymandias site.
tags: [writing, notes]
status: Stable
confidence: 80
importance: 3
evidence: 4
scope: average
novelty: low
practicality: high
confidence-history: [70, 80]
history:
  - date: "2026-04-12"
    note: Initial demo essay
---

This essay exists to give you something to look at on a fresh build and to demonstrate the major features of the pipeline at a glance. Feel free to delete it once you have written your own.

## Typography

The body typeface is Spectral, a screen-first serif with old-style figures (2026, 1984), genuine italic *like this*, and **bold weights**. Standard ligatures are active: *first*, *fifty*, *ffle*. Headings are set in Fira Sans Semibold; code is JetBrains Mono.

Inline code looks like `make build`. Common abbreviations --- HTML, CSS, JSON, NASA, MIT --- are automatically set in smallcaps by the typography filter.

## Sidenotes

Footnotes in your Markdown become sidebar notes on wide screens.[^margin] On narrow screens they fall back to numbered footnotes at the bottom. No special syntax is required beyond standard Pandoc footnotes.

[^margin]: This is a sidenote. It lives in the margin on wide displays and folds into the flow on mobile.

A second sidenote for demonstration.[^second] The numbering and positioning are handled automatically.

[^second]: Sidenotes can contain *inline formatting*, `code`, and even math like $x^2$.

## Mathematics

Inline math like $e^{i\pi} + 1 = 0$ uses KaTeX. Display equations render centered:

$$\int_{-\infty}^{\infty} e^{-x^2}\,dx = \sqrt{\pi}$$

The quadratic formula solves $ax^2 + bx + c = 0$:

$$x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}$$

## Code

Fenced code blocks with language annotations get syntax highlighting:

```haskell
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"
```

```python
def greet(name: str) -> str:
    return f"Hello, {name}!"
```

## Wikilinks

Link between pages by title using double brackets: [[About]] resolves to the about page. Use pipe syntax for custom link text: [[Colophon|the colophon page]].

## Citations

Citations use Pandoc's `[@key]` syntax and resolve against `data/bibliography.bib`. For instance, Knuth's classic text on typesetting[@knuth1984] or Shannon's foundational paper on information theory[@shannon1948]. The bibliography appears at the bottom of the essay.

## Epistemic profile

The frontmatter of this essay declares `status`, `confidence`, `importance`, `evidence`, and related fields. These render as a compact metadata block at the top of the page, giving readers a credibility signal before they commit to reading.

## Tables

| Feature        | Status   |
|:---------------|:---------|
| Typography     | complete |
| Sidenotes      | complete |
| Math (KaTeX)   | complete |
| Code           | complete |
| Citations      | complete |
| Wikilinks      | complete |
| Backlinks      | complete |
| Score reader   | complete |

## Further features

- **Backlinks:** if other essays link to this page, they appear in a Backlinks section at the bottom.
- **Similar links:** enable the embedding pipeline (`uv sync && make build`) and semantically similar essays appear in Further Reading.
- **Dark mode:** use the settings toggle in the nav bar.
- **Score reader:** add SVG score pages to a composition directory; see `content/music/demo-piece/` for an example.
