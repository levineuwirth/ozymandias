# Paper port prototype: figure regions

A prototype of one stage of the planned paper-porting tool. It takes each
figure of a LaTeX paper from the paper's own built PDF, as SVG, by a region
the build records. It is kept so the engine tool can start from tested code.
It is not wired into the engine, and the engine tool replaces it. The
design and its reviews are in the agents' vault, `Passus/Paper port tool…`
(2026-11-17 and 2026-11-18). The latest pass is `extraction failures`.

## How it works

1. **Marks.** `ozyport.sty` is injected into the paper's unchanged build:
   `latexmk -usepretex='\RequirePackage{ozyport}' -jobname=main`, with this
   directory on `TEXINPUTS`.
   - For each `figure` and `figure*`, it records the absolute page and the
     position where the float box begins, where its caption begins and ends,
     and where it ends. It also records `\hsize` at each point.
   - The marks go to `main.ozyfig`.
   - Tables get no marks. A mark in horizontal mode rides on `\vadjust`.
2. **Inventory.** `inventory.py main.tex` lists the figures the source
   declares, with label and caption count. It is the independent reference
   the marks must match. It reads text, not TeX; the engine tool's source
   stage replaces it.
3. **Extraction.** `extract.py <paper> <out>` takes `A/main.pdf` (plain
   build), `B/` (marked build and `main.ozyfig`) and `inventory.json`.
   - It checks everything first.
   - It crops each region from the marked build with pypdf, then converts it
     with `pdftocairo -svg`.
   - It moves the SVGs into `<out>` in one rename, only if nothing was
     refused.
   - Any refusal exits 1 and leaves `<out>` absent. The docstring lists the
     refusals.
   - The picture comparison it prints (librsvg against poppler) is a
     diagnostic, not a check.

## Running

```sh
./make-fixtures.sh                     # builds fixtures/ from the papers (about a minute)
uv run --no-project --with pypdf --with pillow python -I test_extract.py
```

`make-fixtures.sh` regenerates everything the tests need, from the papers'
sources:
- It reads `PROOF_BROKER` (default `~/Repos/research/proof-broker/paper`) and
  `SIMD` (default `~/Repos/research/where-simd-helps/paper`).
- It copies each source without build outputs and builds it twice in
  parallel (plain and marked).
- It writes `inventory.json` and a `SOURCE` file with the commit, whether
  the working tree was clean, a digest of the copied source, the page count,
  and the figures declared and marked.

`fixtures/` is never committed. `FIXTURES` points the tests elsewhere, and
`EXTRACT` runs the same tests against another extractor.

The tests name proof-broker's two figures by label (`fig:pipeline`,
`fig:grid`) and expect six figures in SIMD. They also rely on figure 1's
caption line falling 14 pt below its `capbegin` mark. If a paper changes
those, the tests must follow.

## Fixtures as tested (2026-11-18)

| paper | commit | working tree | source digest | pages | figures |
|---|---|---|---|---|---|
| proof-broker | `0a806cb9` | uncommitted changes, copied as they were | `e75f4ea04e9cc620` | 15 | 2 |
| where-simd-helps | `00ced380` | clean | `51007c014e5bbc06` | 7 | 6 |

A fixture whose digest differs was built from different sources. The tests
may still hold, but the counts above are what they were written against.
Natural Order's arXiv-prep paper (43 pages, 3 included-PDF figures) was also
run through the extractor, but it is not a fixture.

## Tested dependencies

- Python 3.13.15, through uv 0.13.8
- pypdf 7.1.0
- Pillow 13.0.0 and 11.3.0. The engine pins `<12`, so the extractor reads
  pixels with `tobytes()`: `getdata()` is deprecated, and
  `get_flattened_data()` does not exist before 12.
- poppler 26.10.0 (`pdftocairo`, `pdftotext -bbox`, `pdfinfo`) with cairo
  1.18.6. The extractor excuses exactly one failure of this pair: SIGABRT,
  with the whole stderr equal to the `_cairo_hash_table_destroy` assertion
  (it is in `extract.py`). That excuse goes when the bug is fixed.
- librsvg 2.63.4 (`rsvg-convert`), for the diagnostic only
- TeX Live 2026: pdfTeX 1.40.29, LaTeX2e 2025-11-01, latexmk 4.87.
  `ozyport.sty` needs a kernel with the hook system and
  `\ReadonlyShipoutCounter` (2020-10 or later), and pdfTeX's `\pdfsavepos`.
- rsync 3.5.2, for the fixture recipe

## Known limits

- A caption above its figure has a region rule but no real paper tested it.
- Subfigures are untested. More than one `\caption` in a figure refuses.
- Content drawn outside the float box (negative margins, overlays) would be
  cut. The edge test sees words, not strokes.
- A figure set without a `figure` environment (`\captionof`) is neither
  marked nor inventoried.
- Only pdfTeX is supported (`\pdfsavepos`).
