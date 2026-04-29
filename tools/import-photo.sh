#!/usr/bin/env bash
# import-photo.sh — Author-facing import workflow for photography entries.
#
# Given a path to an original photograph and a target slug, this script:
#
#   1. Creates content/photography/<slug>/.
#   2. Resizes the original to ≤2400px on the long edge, JPEG quality 85,
#      sRGB. EXIF is preserved at this step so the extractor can read it.
#   3. Runs tools/extract-exif.py to produce the {photo}.exif.yaml sidecar.
#   4. Strips EXIF from the delivered JPEG (the sidecar now holds the
#      metadata; the file shipped to viewers carries no GPS, no serial
#      numbers, no Lightroom edit history).
#   5. Runs tools/extract-palette.py to produce the {photo}.palette.yaml
#      sidecar.
#   6. Scaffolds an index.md frontmatter stub ready for editing.
#
# Usage:
#   tools/import-photo.sh <original-path> <slug> [--title "Title"]
#
# Examples:
#   tools/import-photo.sh ~/Photos/IMG_4421.jpg reykjavik-rooftops
#   tools/import-photo.sh ~/Photos/IMG_4421.jpg reykjavik-rooftops --title "Reykjavík Rooftops"
#
# Requirements:
#   * ImageMagick (`magick`) for resize / strip / colorspace conversion
#   * uv + .venv (Pillow + colorthief + pyyaml) for sidecar extraction
#
# Originals are NEVER copied into the repo verbatim — only the resized
# delivery JPEG. Per PHOTOGRAPHY.md, originals live outside source
# control (your local archive, NAS, or backup).

set -euo pipefail

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

if [ "$#" -lt 2 ]; then
    cat <<EOF >&2
Usage: $(basename "$0") <original-path> <slug> [--title "Title"]

Imports a photograph into content/photography/<slug>/, producing:
  photo.jpg                  resized, sRGB, EXIF-stripped (delivery copy)
  photo.jpg.exif.yaml        extracted EXIF metadata (sidecar)
  photo.jpg.palette.yaml     5-color palette (sidecar)
  index.md                   frontmatter stub ready for editing
EOF
    exit 1
fi

ORIGINAL="$1"
SLUG="$2"
shift 2

TITLE=""
while [ "$#" -gt 0 ]; do
    case "$1" in
        --title)
            TITLE="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
    esac
done

if [ ! -f "$ORIGINAL" ]; then
    echo "import-photo: original not found: $ORIGINAL" >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Tool availability checks
# ---------------------------------------------------------------------------

if ! command -v magick >/dev/null 2>&1; then
    echo "import-photo: ImageMagick ('magick') is required but not installed." >&2
    echo "  Arch:   pacman -S imagemagick" >&2
    echo "  Debian: apt install imagemagick" >&2
    exit 1
fi

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

if [ ! -d "$REPO_ROOT/.venv" ]; then
    echo "import-photo: .venv not found at $REPO_ROOT/.venv" >&2
    echo "  Run: uv sync" >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Layout
# ---------------------------------------------------------------------------

ENTRY_DIR="$REPO_ROOT/content/photography/$SLUG"
TARGET="$ENTRY_DIR/photo.jpg"
EXIF_SIDECAR="$TARGET.exif.yaml"
PALETTE_SIDECAR="$TARGET.palette.yaml"
INDEX_MD="$ENTRY_DIR/index.md"

if [ -e "$ENTRY_DIR" ]; then
    echo "import-photo: $ENTRY_DIR already exists. Refusing to overwrite." >&2
    echo "  Either choose a new slug or remove the existing entry first." >&2
    exit 1
fi

mkdir -p "$ENTRY_DIR"

# ---------------------------------------------------------------------------
# Step 1: resize + colorspace, EXIF preserved (so the extractor can read it)
# ---------------------------------------------------------------------------

echo "import-photo: resizing to ≤2400px JPEG q85 sRGB → $TARGET"
magick "$ORIGINAL" \
    -auto-orient \
    -resize '2400x2400>' \
    -colorspace sRGB \
    -quality 85 \
    "$TARGET"
chmod 644 "$TARGET"

# ---------------------------------------------------------------------------
# Step 2: extract EXIF (reads from the resized file, which still has EXIF)
# ---------------------------------------------------------------------------

echo "import-photo: extracting EXIF sidecar..."
( cd "$REPO_ROOT" && uv run python tools/extract-exif.py ) || true

if [ ! -f "$EXIF_SIDECAR" ]; then
    # Empty sidecar so the consuming Hakyll field has something to read
    # (an absent sidecar is also handled, but a present-but-empty file
    # signals "extraction was attempted" — useful for film scans where
    # there's intentionally no EXIF to find).
    echo '{}' > "$EXIF_SIDECAR"
fi

# ---------------------------------------------------------------------------
# Step 3: strip EXIF from the delivered JPEG (sidecar already has it)
# ---------------------------------------------------------------------------

echo "import-photo: stripping EXIF from delivered file..."
magick mogrify -strip "$TARGET"

# ---------------------------------------------------------------------------
# Step 4: extract palette (does its own walk; idempotent on already-done photos)
# ---------------------------------------------------------------------------

echo "import-photo: extracting palette sidecar..."
( cd "$REPO_ROOT" && uv run python tools/extract-palette.py ) || true

# ---------------------------------------------------------------------------
# Step 5: scaffold index.md
# ---------------------------------------------------------------------------

if [ -z "$TITLE" ]; then
    TITLE="$(echo "$SLUG" | tr '-' ' ' | awk '{
        for(i=1;i<=NF;i++) $i=toupper(substr($i,1,1)) substr($i,2);
        print
    }')"
fi

TODAY="$(date -u +%Y-%m-%d)"

# Probe the resized file's pixel dimensions so we can suggest an
# orientation; the author can override in frontmatter.
DIMS="$(magick identify -format '%w %h' "$TARGET")"
WIDTH="${DIMS%% *}"
HEIGHT="${DIMS##* }"

if [ "$WIDTH" -gt "$HEIGHT" ]; then
    ORIENTATION="landscape"
elif [ "$HEIGHT" -gt "$WIDTH" ]; then
    ORIENTATION="portrait"
else
    ORIENTATION="square"
fi

cat > "$INDEX_MD" <<EOF
---
title: "$TITLE"
date: $TODAY
abstract: >
  TODO — short caption for this photograph.
tags: [photography]
photo: photo.jpg
orientation: $ORIENTATION
# license: "CC BY-SA 4.0"   # uncomment + set; canonical URL auto-resolves
# location: ""              # human-readable, e.g. "Reykjavík, Iceland"
# camera, lens, exposure are auto-filled from the EXIF sidecar — only
# add them here to override what was extracted.
---

EOF
chmod 644 "$INDEX_MD"

echo
echo "import-photo: done."
echo "  Entry:   $INDEX_MD"
echo "  Photo:   $TARGET ($WIDTH × $HEIGHT, $ORIENTATION)"
echo "  Sidecars: $(basename "$EXIF_SIDECAR"), $(basename "$PALETTE_SIDECAR")"
echo
echo "Next: edit $INDEX_MD to fill in title / abstract / tags, then 'make dev'."
