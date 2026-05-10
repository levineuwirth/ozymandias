#!/usr/bin/env bash
# subset-fonts.sh — Subset and convert TTF system fonts to WOFF2 for web use.
#
# Requires: fonttools (pyftsubset) — `pip install fonttools brotli`
# Output:   static/fonts/
#
# ─── Font directories ────────────────────────────────────────────────────────
# Override any of these via the matching env var when your distro lays the
# fonts out differently. The script tries each candidate path in order and
# fails with a clear message if none resolve.
#
#   SPECTRAL_DIR  Spectral-{Regular,Italic,SemiBold,SemiBoldItalic,Bold,BoldItalic}.ttf
#   FIRA_DIR      FiraSans-{Regular,SemiBold}.ttf
#   JBM_DIR       JetBrainsMono-{Regular,Italic}.ttf
#
# Common locations:
#   Arch:         /usr/share/fonts/ttf-spectral, /usr/share/fonts/TTF
#   Debian:       /usr/share/fonts/truetype/spectral, /usr/share/fonts/truetype/firasans,
#                 /usr/share/fonts/truetype/jetbrains-mono
#   macOS (brew): /opt/homebrew/share/fonts, ~/Library/Fonts
#   Manual:       wherever you unzipped the upstream releases

set -euo pipefail

OUTDIR="$(dirname "$0")/../static/fonts"
mkdir -p "$OUTDIR"

# ---------------------------------------------------------------------------
# Locate font directories (env var override > candidate list > error)
# ---------------------------------------------------------------------------

# Resolve a font dir by checking, in order:
#   1. The override env var (passed by name), if non-empty
#   2. Each candidate path
# Sets the global RESOLVED_DIR on success; exits with a descriptive message
# on failure. Sets the global rather than echoing because `exit 1` inside a
# `$(...)` substitution only exits the subshell, not the script.
resolve_font_dir() {
    local label="$1"      # human-readable name, e.g. "Spectral"
    local env_var="$2"    # env var name, e.g. "SPECTRAL_DIR"
    shift 2
    local candidates=("$@")

    # Indirection: ${!env_var} reads the env var named in $env_var.
    if [ -n "${!env_var:-}" ]; then
        if [ ! -d "${!env_var}" ]; then
            echo "Error: $env_var=${!env_var} is not a directory." >&2
            exit 1
        fi
        RESOLVED_DIR="${!env_var}"
        return
    fi

    for dir in "${candidates[@]}"; do
        if [ -d "$dir" ]; then
            RESOLVED_DIR="$dir"
            return
        fi
    done

    echo "Error: $label fonts not found in any of:" >&2
    printf '  %s\n' "${candidates[@]}" >&2
    echo "Set $env_var to point at the directory containing the .ttf files." >&2
    exit 1
}

resolve_font_dir "Spectral" "SPECTRAL_DIR" \
    /usr/share/fonts/ttf-spectral \
    /usr/share/fonts/truetype/spectral \
    /opt/homebrew/share/fonts \
    "$HOME/Library/Fonts"
SPECTRAL="$RESOLVED_DIR"

resolve_font_dir "Fira Sans" "FIRA_DIR" \
    /usr/share/fonts/TTF \
    /usr/share/fonts/truetype/firasans \
    /usr/share/fonts/truetype/fira-sans \
    /opt/homebrew/share/fonts \
    "$HOME/Library/Fonts"
FIRA="$RESOLVED_DIR"

resolve_font_dir "JetBrains Mono" "JBM_DIR" \
    /usr/share/fonts/TTF \
    /usr/share/fonts/truetype/jetbrains-mono \
    /usr/share/fonts/truetype/jetbrainsmono \
    /opt/homebrew/share/fonts \
    "$HOME/Library/Fonts"
JBM="$RESOLVED_DIR"

# ---------------------------------------------------------------------------
# Subset configuration
# ---------------------------------------------------------------------------

# Unicode range: Latin, Latin Extended-A, General Punctuation, common symbols
UNICODES="U+0000-00FF,U+0131,U+0152-0153,U+02BB-02BC,U+02C6,U+02DA,U+02DC,U+2000-206F,U+2074,U+20AC,U+2122,U+2191,U+2193,U+2212,U+2215,U+FEFF,U+FFFD"

SPECTRAL_FEATURES="liga,dlig,smcp,c2sc,onum,lnum,pnum,tnum,frac,ordn,sups,subs,ss01,ss02,ss03,ss04,ss05,kern"
FIRA_FEATURES="smcp,liga,kern"
JBM_FEATURES="liga,kern,calt"

# ---------------------------------------------------------------------------
# Subset helper — fails fast if a source file is missing
# ---------------------------------------------------------------------------

subset() {
    local src="$1" dest="$2" features="$3"
    if [ ! -f "$src" ]; then
        echo "Error: source font not found: $src" >&2
        echo "  Adjust the directory env var (SPECTRAL_DIR / FIRA_DIR / JBM_DIR)" >&2
        echo "  or install the corresponding font package." >&2
        exit 1
    fi
    echo "  → $(basename "$dest")"
    pyftsubset "$src" \
        --output-file="$dest" \
        --flavor=woff2 \
        --layout-features="$features" \
        --unicodes="$UNICODES" \
        --no-hinting \
        --desubroutinize
}

# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------

echo "Spectral... ($SPECTRAL)"
subset "$SPECTRAL/Spectral-Regular.ttf"         "$OUTDIR/spectral-regular.woff2"          "$SPECTRAL_FEATURES"
subset "$SPECTRAL/Spectral-Italic.ttf"          "$OUTDIR/spectral-italic.woff2"           "$SPECTRAL_FEATURES"
subset "$SPECTRAL/Spectral-SemiBold.ttf"        "$OUTDIR/spectral-semibold.woff2"         "$SPECTRAL_FEATURES"
subset "$SPECTRAL/Spectral-SemiBoldItalic.ttf"  "$OUTDIR/spectral-semibold-italic.woff2"  "$SPECTRAL_FEATURES"
subset "$SPECTRAL/Spectral-Bold.ttf"            "$OUTDIR/spectral-bold.woff2"             "$SPECTRAL_FEATURES"
subset "$SPECTRAL/Spectral-BoldItalic.ttf"      "$OUTDIR/spectral-bold-italic.woff2"      "$SPECTRAL_FEATURES"

echo "Fira Sans... ($FIRA)"
subset "$FIRA/FiraSans-Regular.ttf"             "$OUTDIR/fira-sans-regular.woff2"         "$FIRA_FEATURES"
subset "$FIRA/FiraSans-SemiBold.ttf"            "$OUTDIR/fira-sans-semibold.woff2"        "$FIRA_FEATURES"

echo "JetBrains Mono... ($JBM)"
subset "$JBM/JetBrainsMono-Regular.ttf"         "$OUTDIR/jetbrains-mono-regular.woff2"    "$JBM_FEATURES"
subset "$JBM/JetBrainsMono-Italic.ttf"          "$OUTDIR/jetbrains-mono-italic.woff2"     "$JBM_FEATURES"

echo ""
echo "Done. Output:"
ls -lh "$OUTDIR"/*.woff2
