"""
viz_theme.py — Shared matplotlib setup for figures in Ozymandias sites.

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

import sys
import io
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


def save_svg(fig, tight=True):
    """Write *fig* as SVG to stdout and close it.

    Hakyll's Viz filter captures stdout and inlines the SVG.

    Parameters
    ----------
    fig : matplotlib.figure.Figure
    tight : bool
        If True (default), call fig.tight_layout() before saving.
    """
    if tight:
        fig.tight_layout()
    buf = io.StringIO()
    fig.savefig(buf, format='svg', bbox_inches='tight', transparent=True)
    plt.close(fig)
    sys.stdout.write(buf.getvalue())
