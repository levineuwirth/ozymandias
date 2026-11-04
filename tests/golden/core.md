---
title: "Golden fixture: the core"
bibliography: fixture.bib
further-reading:
  - alpha2001
  - gamma2020
figure-numbering: true
---

# Opening section

Plain prose with "smart quotes" -- an en dash --- an em dash, and an
ellipsis... Abbreviations: e.g. this, i.e. that (cf. the other), and one
ending a line, e.g.
here, and one with a comma, i.e., like so. Then
here. NASA and
HTML go to small caps, and so does [explicit]{.smallcaps} text.

A cited claim [@alpha2001] and a cluster [@delta2015; @alpha2001], then the
first book again [@alpha2001, p. 4]. The book is also listed for further
reading, which is the case that once cost its first citation its anchor.

A note.^[A sidenote with a [link](https://en.wikipedia.org/wiki/Footnote).]
A second note.^[The second note cites [@delta2015].]

## Links

- external with an icon: [Wikipedia](https://en.wikipedia.org/wiki/Typography)
- external without one: [Example](https://example.net/page)
- internal: [the colophon](/colophon.html), and [a section](#links)
- mail: [write](mailto:someone@example.org)
- wikilinks: [[Golden Page]] and [[Golden Page|shown differently]]

## What "consumed" means, e.g. for this

A heading with quotation marks and an abbreviation Typography wraps in
`<abbr>`: the TOC entry must keep both (audit H03).

## Figures

![A plate with a WebP companion and recorded dimensions.](plate.png){#fig-plate}

An inline image ![a small square](plain.png) without a WebP companion, and
one with: ![the "plate" in 'quotes', e.g. this one](plate.png).

A decorative one, ![](plain.png){.decorative}, and one in a picture, ![](plate.png){.decorative}.

As [](#fig-plate) shows, and as [the plate](#fig-plate) says in other words.

::: aftermatter
Closing matter.
:::
