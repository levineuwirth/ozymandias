#!/usr/bin/env bash
# Subsets and converts TTF system fonts to WOFF2 for web use.
# Requires: fonttools (pyftsubset) — `pip install fonttools brotli`
# Output: static/fonts/

set -euo pipefail

OUTDIR="$(dirname "$0")/../static/fonts"
mkdir -p "$OUTDIR"

SPECTRAL="/usr/share/fonts/ttf-spectral"
FIRA="/usr/share/fonts/TTF"
JBM="/usr/share/fonts/TTF"

# Unicode range: Latin, Latin Extended-A, General Punctuation, common symbols
UNICODES="U+0000-00FF,U+0131,U+0152-0153,U+02BB-02BC,U+02C6,U+02DA,U+02DC,U+2000-206F,U+2074,U+20AC,U+2122,U+2191,U+2193,U+2212,U+2215,U+FEFF,U+FFFD"

SPECTRAL_FEATURES="liga,dlig,smcp,c2sc,onum,lnum,pnum,tnum,frac,ordn,sups,subs,ss01,ss02,ss03,ss04,ss05,kern"
FIRA_FEATURES="smcp,liga,kern"
JBM_FEATURES="liga,kern,calt"

subset() {
    local src="$1" dest="$2" features="$3"
    echo "  → $(basename "$dest")"
    pyftsubset "$src" \
        --output-file="$dest" \
        --flavor=woff2 \
        --layout-features="$features" \
        --unicodes="$UNICODES" \
        --no-hinting \
        --desubroutinize
}

echo "Spectral..."
subset "$SPECTRAL/Spectral-Regular.ttf"         "$OUTDIR/spectral-regular.woff2"          "$SPECTRAL_FEATURES"
subset "$SPECTRAL/Spectral-Italic.ttf"          "$OUTDIR/spectral-italic.woff2"           "$SPECTRAL_FEATURES"
subset "$SPECTRAL/Spectral-SemiBold.ttf"        "$OUTDIR/spectral-semibold.woff2"         "$SPECTRAL_FEATURES"
subset "$SPECTRAL/Spectral-SemiBoldItalic.ttf"  "$OUTDIR/spectral-semibold-italic.woff2"  "$SPECTRAL_FEATURES"
subset "$SPECTRAL/Spectral-Bold.ttf"            "$OUTDIR/spectral-bold.woff2"             "$SPECTRAL_FEATURES"
subset "$SPECTRAL/Spectral-BoldItalic.ttf"      "$OUTDIR/spectral-bold-italic.woff2"      "$SPECTRAL_FEATURES"

echo "Fira Sans..."
subset "$FIRA/FiraSans-Regular.ttf"             "$OUTDIR/fira-sans-regular.woff2"         "$FIRA_FEATURES"
subset "$FIRA/FiraSans-SemiBold.ttf"            "$OUTDIR/fira-sans-semibold.woff2"        "$FIRA_FEATURES"

echo "JetBrains Mono..."
subset "$JBM/JetBrainsMono-Regular.ttf"         "$OUTDIR/jetbrains-mono-regular.woff2"    "$JBM_FEATURES"
subset "$JBM/JetBrainsMono-Italic.ttf"          "$OUTDIR/jetbrains-mono-italic.woff2"     "$JBM_FEATURES"

echo ""
echo "Done. Output:"
ls -lh "$OUTDIR"/*.woff2
