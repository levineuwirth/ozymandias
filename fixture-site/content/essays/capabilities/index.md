---
title: Capabilities
date: 2026-02-10
abstract: >-
  The full profile's essay: what a site has only with its switches on, each
  in a section of its own, from $\pi$ on.
tags: [notes]
viz: true
---

## Math

Euler's identity inline, $e^{i\pi} + 1 = 0$, and a sum as a display:

$$\sum_{k=0}^{n} \binom{n}{k} = 2^n$$

## Code

A block in a named language:

```python
def double(n):
    return 2 * n
```

## Figures

A static figure, whose caption has math:

::: {.figure script="figures/line.py" caption="A line drawn in black, and $y = x$."}
:::

An interactive one, which loads Vega:

::: {.visualization script="figures/chart.py" caption="Three points, joined."}
:::

## Source references

Inline code naming a file the site publishes, `data/bibliography.bib`, links
to it, and so does [its link to the repository](https://git.example.org/author/site/src/branch/main/data/bibliography.bib),
and one an author marked, [the same file](https://git.example.org/author/site/src/branch/main/data/bibliography.bib){.source-ref data-source-path="data/bibliography.bib"}.

## A paper

[The fixture paper](/essays/capabilities/paper.pdf) is cited too [@delta2026],
and embedded from its first page:

{{pdf:/essays/capabilities/paper.pdf#1}}

## A score fragment

The motif the [[Short Page]] mentions, as the music switch inlines it.

::: {.score-fragment score-name="Motif" score-caption="A two-bar motif."}
![](figures/motif.svg)
:::
