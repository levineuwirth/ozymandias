"""
viz_theme.py — Shared matplotlib setup for levineuwirth.org figures.

Usage in a figure script:

    import sys
    sys.path.insert(0, 'tools')   # relative to project root (where cabal runs)
    from viz_theme import apply_monochrome, save_svg

    apply_monochrome()

    import matplotlib.pyplot as plt
    fig, ax = plt.subplots()
    ax.plot([1, 2, 3], [4, 5, 6])
    ax.set_xlabel("x")
    ax.set_ylabel("y")

    save_svg(fig)  # writes SVG to stdout; Viz.hs captures it

Design constraints
------------------
- Use pure black (#000000) for all drawn elements (lines, markers, text,
  spines, ticks).  Filters.Viz.processColors replaces these with
  `currentColor` so the SVG adapts to light/dark mode via CSS.
- Use transparent backgrounds (figure and axes).  The page background
  shows through, so the figure integrates cleanly in both modes.
- For greyscale fills (bars, areas), use values in the range #333–#ccc.
  These do NOT get replaced by processColors, so choose mid-greys that
  remain legible in both light (#faf8f4) and dark (#121212) contexts.
- For multi-series charts, distinguish series by linestyle (solid, dashed,
  dotted, dash-dot) rather than colour.

Font note: matplotlib's SVG output uses the font names configured here, but
those fonts are not available in the browser SVG renderer — the browser falls
back to its default serif.  Do not rely on font metrics for sizing.
"""

import csv
import sys
import io
import os
import matplotlib as mpl
import matplotlib.pyplot as plt

# Greyscale linestyle cycle for multi-series charts.
# Each entry: (color, linestyle) — all black, distinguished by dash pattern.
LINESTYLE_CYCLE = [
    {'color': '#000000', 'linestyle': 'solid'},
    {'color': '#000000', 'linestyle': 'dashed'},
    {'color': '#000000', 'linestyle': 'dotted'},
    {'color': '#000000', 'linestyle': (0, (5, 2, 1, 2))},  # dash-dot
    {'color': '#555555', 'linestyle': 'solid'},
    {'color': '#555555', 'linestyle': 'dashed'},
]


def load_csv(name):
    """Read a CSV from the calling script's own ``data/`` directory.

    Returns a list of dicts, one per row, exactly as ``csv.DictReader``
    would.

    Figure scripts are executed from the project root, so a bare relative
    path does not resolve to anything near the script. Every script in the
    corpus worked around that by hardcoding the full path from the root::

        filepath = "content/essays/where-does-simd-help-post-quantum-cryptography/figures/data/kem_level.csv"

    which silently breaks every figure in an essay the moment the essay is
    renamed or moved. Resolving against ``__file__`` instead makes a figure
    survive the move, and shortens the call to ``load_csv("kem_level.csv")``.
    """
    caller = sys._getframe(1).f_globals.get("__file__")
    if caller is None:                      # interactive use
        base = os.getcwd()
    else:
        base = os.path.dirname(os.path.abspath(caller))
    path = os.path.join(base, "data", name)
    with open(path, newline="") as fh:
        return list(csv.DictReader(fh))


def apply_monochrome():
    """Configure matplotlib for monochrome, transparent, dark-mode-safe output.

    Call this before creating any figures.  All element colours are set to
    pure black (#000000) so Filters.Viz.processColors can replace them with
    CSS currentColor.  Backgrounds are transparent.
    """
    mpl.rcParams.update({
        # Transparent backgrounds — CSS page background shows through.
        'figure.facecolor':  'none',
        'axes.facecolor':    'none',
        'savefig.facecolor': 'none',
        'savefig.edgecolor': 'none',

        # All text and structural elements: pure black → currentColor.
        'text.color':        'black',
        'axes.labelcolor':   'black',
        'axes.edgecolor':    'black',
        'xtick.color':       'black',
        'ytick.color':       'black',

        # Grid: mid-grey, stays legible in both modes (not replaced).
        'axes.grid':         False,
        'grid.color':        '#cccccc',
        'grid.linewidth':    0.6,

        # Lines and patches: black → currentColor.
        'lines.color':       'black',
        'patch.edgecolor':   'black',

        # Legend: no box frame; background transparent.
        'legend.frameon':    False,
        'legend.facecolor':  'none',
        'legend.edgecolor':  'none',

        # Use linestyle cycle instead of colour cycle for series distinction.
        'axes.prop_cycle': mpl.cycler(
            color=[c['color'] for c in LINESTYLE_CYCLE],
            linestyle=[c['linestyle'] for c in LINESTYLE_CYCLE],
        ),
    })


def save_svg(fig, tight=True, salt=None, alt=None, desc=None):
    """Write *fig* as SVG to stdout and close it.

    Hakyll's Viz filter captures stdout and inlines the SVG.

    Parameters
    ----------
    fig : matplotlib.figure.Figure
    tight : bool
        If True (default), call fig.tight_layout() before saving.
    salt : str, optional
        Overrides the svg.hashsalt derived from the script filename. Only
        needed if two figures on one page would otherwise share a salt.
    alt : str
        One-sentence accessible name for the chart, announced in place of
        it. Say what kind of chart it is and what it plots -- a screen
        reader user gets this and the caption, nothing else. Omitting it
        warns on stderr during the build.
    desc : str, optional
        Longer description for readers who cannot see the figure: the shape
        of the data and the finding it carries, not a restatement of the
        caption.
    """
    if tight:
        fig.tight_layout()

    # svg.hashsalt defaults to None, which makes matplotlib seed the ids it
    # generates for <clipPath> and marker <defs> from a *random* value per
    # process. Those ids then differ on every single build:
    #
    #   <g clip-path="url(#p4949e963db)">   build N
    #   <g clip-path="url(#p208e5f457e)">   build N+1
    #
    # ...which is enough to make every figure, and so every page embedding
    # one, byte-different each time even when nothing about the data or the
    # code changed. Pinning the salt makes the ids a pure function of the
    # figure's content, which is what the metadata Date=None below is also
    # reaching for.
    #
    # The salt is the script's filename rather than a site-wide constant for
    # two reasons: it is stable across machines and checkouts (an absolute
    # path is not), and it keeps figures distinguishable. A shared salt makes
    # the id a function of content alone, so two figures with identical clip
    # geometry on the same page would generate the same id — and a <use> or
    # url(#...) reference resolves to whichever copy the document happens to
    # hold first.
    if salt is None:
        salt = os.path.basename(sys.argv[0]) or 'viz'
    mpl.rcParams['svg.hashsalt'] = salt

    buf = io.StringIO()
    # metadata Date=None: matplotlib otherwise stamps <dc:date> with the
    # generation time, making every recompile's SVG (and thus the page's
    # HTML) differ. That churned the embedding cache each build: new text
    # hash -> re-embed -> similar-links scores shift -> page recompiles
    # next build -> new timestamp -> repeat. Reproducible output breaks
    # the loop at the source.
    fig.savefig(buf, format='svg', bbox_inches='tight', transparent=True,
                metadata={'Date': None})
    plt.close(fig)
    sys.stdout.write(_annotate(buf.getvalue(), alt, desc))


def _annotate(svg, alt, desc):
    """Give the SVG root an accessible name and description.

    Inline SVG is exposed to assistive technology as a graphic only if it
    says so: without role="img" the whole chart is either ignored or read
    out as a meaningless pile of <path> elements. `aria-label` carries the
    name rather than `aria-labelledby` pointing at a <title>, because the
    label then needs no id -- and ids inside a figure are rewritten by
    Filters.Viz to keep several figures from colliding on one page.

    <title> and <desc> are emitted as well: <title> is what a browser shows
    as a tooltip, and both are what a reader gets when the SVG is opened on
    its own, outside the page.
    """
    if not alt:
        print("viz_theme: save_svg() called without alt= — the figure will "
              "be invisible to screen readers", file=sys.stderr)
        return svg

    start = svg.find("<svg")
    if start == -1:
        return svg
    end = svg.find(">", start)
    if end == -1:
        return svg

    head, tail = svg[:end], svg[end + 1:]
    nodes = "<title>{}</title>".format(_xml(alt))
    if desc:
        nodes += "<desc>{}</desc>".format(_xml(desc))
    return '{} role="img" aria-label="{}">{}{}'.format(
        head, _xml(alt), nodes, tail
    )


def _xml(text):
    """Escape text for an XML attribute or text node."""
    return (text.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace('"', "&quot;"))
