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
#                         [--series <series-slug>] [--tags a,b,c]
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
Usage: $(basename "$0") <original-path> <slug> [--title "T"] [--series <s>] [--tags a,b]

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

if [[ ! "$SLUG" =~ ^[a-z0-9-]+$ ]]; then
    echo "import-photo: invalid slug '$SLUG' (must be lowercase a-z, 0-9, hyphens only)" >&2
    exit 1
fi

TITLE=""
SERIES=""
EXTRA_TAGS=""
while [ "$#" -gt 0 ]; do
    case "$1" in
        --title)
            TITLE="$2"
            shift 2
            ;;
        --series)
            SERIES="$2"
            shift 2
            ;;
        --tags)
            EXTRA_TAGS="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
    esac
done

if [ -n "$SERIES" ] && [[ ! "$SERIES" =~ ^[a-z0-9-]+$ ]]; then
    echo "import-photo: invalid series '$SERIES' (lowercase a-z, 0-9, hyphens)" >&2
    exit 1
fi

if [ ! -f "$ORIGINAL" ]; then
    echo "import-photo: original not found: $ORIGINAL" >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Tool availability checks
# ---------------------------------------------------------------------------

if ! command -v exiftool >/dev/null 2>&1; then
    echo "import-photo: exiftool is required (it strips EXIF from the delivered file)." >&2
    echo "  Arch:   pacman -S perl-image-exiftool" >&2
    echo "  Debian: apt install libimage-exiftool-perl" >&2
    exit 1
fi

if ! command -v magick >/dev/null 2>&1; then
    echo "import-photo: ImageMagick ('magick') is required but not installed." >&2
    echo "  Arch:   pacman -S imagemagick" >&2
    echo "  Debian: apt install imagemagick" >&2
    exit 1
fi

# shellcheck source=tools/site-paths.sh
source "$(dirname "$0")/site-paths.sh"

if [ ! -d "$SITE_ROOT/.venv" ]; then
    echo "import-photo: .venv not found at $SITE_ROOT/.venv" >&2
    echo "  Run: make venv" >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Layout
# ---------------------------------------------------------------------------

# Two shapes, per PHOTOGRAPHY.md:
#
#   standalone   content/photography/<slug>/index.md  + photo.jpg
#   series child content/photography/<series>/<slug>.md + <slug>.jpg
#
# The build derives the image URL as /photography/<dir>/<photo>, where <dir>
# is the containing directory either way — so a series child keeps its image
# beside its markdown rather than in a subdirectory of its own. A directory
# becomes a series automatically once it holds sibling .md files next to
# index.md; nothing has to be declared.
if [ -n "$SERIES" ]; then
    ENTRY_DIR="$SITE_ROOT/content/photography/$SERIES"
    TARGET="$ENTRY_DIR/$SLUG.jpg"
    INDEX_MD="$ENTRY_DIR/$SLUG.md"
else
    ENTRY_DIR="$SITE_ROOT/content/photography/$SLUG"
    TARGET="$ENTRY_DIR/photo.jpg"
    INDEX_MD="$ENTRY_DIR/index.md"
fi

EXIF_SIDECAR="$TARGET.exif.yaml"
PALETTE_SIDECAR="$TARGET.palette.yaml"

# Refuse to clobber an existing entry. For a series the directory is expected
# to exist and be added to, so only the entry's own files are guarded.
if [ -e "$INDEX_MD" ] || [ -e "$TARGET" ]; then
    echo "import-photo: $INDEX_MD or $TARGET already exists. Refusing to overwrite." >&2
    echo "  Either choose a new slug or remove the existing entry first." >&2
    exit 1
fi
if [ -z "$SERIES" ] && [ -e "$ENTRY_DIR" ]; then
    echo "import-photo: $ENTRY_DIR already exists. Refusing to overwrite." >&2
    exit 1
fi

# The namespace guard (scaffold-photos.py --check), on the tags as they will
# be written, before anything is copied.
SERIES="$SERIES" TAGS="$EXTRA_TAGS" SLUGS="$SLUG" \
    "$SITE_ROOT/.venv/bin/python" "$(engine_path tools/scaffold-photos.py)" --check || exit 2

mkdir -p "$ENTRY_DIR"

# ---------------------------------------------------------------------------
# Step 1: resize + colorspace, EXIF preserved (so the extractor can read it)
# ---------------------------------------------------------------------------

# From here until the strip in step 3 succeeds, $TARGET carries the camera's
# full EXIF (GPS, serial numbers) inside content/, which the build publishes
# whether or not git tracks it. Any exit before then — a failure, or Ctrl-C
# during extraction — removes it (audit T03); a retry then starts clean
# instead of refusing an "existing" entry.
trap 'rm -f -- "$TARGET"' EXIT INT TERM

echo "import-photo: resizing to ≤2400px JPEG q85 sRGB → $TARGET"
magick "$ORIGINAL" \
    -auto-orient \
    -resize '2400x2400>' \
    -colorspace sRGB \
    -quality 85 \
    "$TARGET" \
    || { echo "import-photo: magick resize failed for $ORIGINAL → $TARGET" >&2; exit 1; }
chmod 644 "$TARGET"

# ---------------------------------------------------------------------------
# Step 2: extract EXIF (reads from the resized file, which still has EXIF)
# ---------------------------------------------------------------------------

echo "import-photo: extracting EXIF sidecar..."
( cd "$SITE_ROOT" && .venv/bin/python "$(engine_path tools/extract-exif.py)" "$TARGET" ) || true

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
exiftool -q -all= --icc_profile:all -overwrite_original "$TARGET" \
    || {
        # The copy under content/ still carries full EXIF (GPS, serial
        # numbers); the Makefile's `git add content/` could auto-commit
        # and publish it. Remove it before bailing out.
        rm -f -- "$TARGET"
        echo "import-photo: exiftool -all= failed for $TARGET (EXIF NOT stripped); deleted the copied target so the EXIF-laden JPEG cannot be auto-committed" >&2
        exit 1
    }
trap - EXIT INT TERM

# Stripping rewrites the JPEG, so the delivered file is now NEWER than the
# sidecar we just extracted from it. extract-exif.py treats
# `image mtime > sidecar mtime` as stale, so the very next `make build`
# would re-extract from the stripped file and overwrite real metadata with
# width/height alone — silently, because the build still succeeds. Making
# the sidecar the newer file closes that window.
touch "$EXIF_SIDECAR"

# ---------------------------------------------------------------------------
# Step 4: extract palette (does its own walk; idempotent on already-done photos)
# ---------------------------------------------------------------------------

echo "import-photo: extracting palette sidecar..."
( cd "$SITE_ROOT" && .venv/bin/python "$(engine_path tools/extract-palette.py)" "$TARGET" ) || true

# ---------------------------------------------------------------------------
# Step 4b: responsive delivery variants (P01)
#
# The .w480 / .w960 / .w1440 siblings the card grids, contact sheet and map
# tooltips actually load. `make build` regenerates whatever is missing, so
# this is only a convenience — it means the frame you just imported renders
# at the right weight in `make dev` without a full build first.
#
# The generator is named this ONE file rather than the whole section, and
# is non-fatal: a failure here costs an author a heavier preview, never an
# import.
# ---------------------------------------------------------------------------

echo "import-photo: generating responsive variants..."
( cd "$SITE_ROOT" && .venv/bin/python "$(engine_path tools/generate-thumbnails.py)" "$TARGET" ) \
    || echo "import-photo: variant generation failed (make build will retry)" >&2

# ---------------------------------------------------------------------------
# Step 5: scaffold index.md (and the series landing, the first time)
#
# tools/scaffold-photos.py writes it, from a one-row plan, exactly as a bulk
# import (tools/import-photos.sh) does: the title (from the slug when
# --title is absent), today's date, the tags (--tags filed beneath
# photography/ unless they contain a slash: the series says which trip a
# frame belongs to, the tags what it is of), the photo, the orientation,
# and the camera fields from the EXIF sidecar.
#
# Those camera fields are carried into the frontmatter because the sidecar
# is gitignored and is regenerated from the delivered JPEG, which this
# script has just stripped: on any other machine regeneration yields width
# and height and nothing else. Frontmatter is tracked and authoritative
# (PHOTOGRAPHY.md). `geo` is deliberately NOT carried across: the sidecar
# holds coordinates at full precision, Hakyll rounds them at render time,
# and writing them into frontmatter would commit exact positions to a
# public repository around that gate.
# ---------------------------------------------------------------------------

# The entry goes as arguments, not a plan row: a title may hold a tab or a
# newline, which a tab-separated row would cut short or split.
SERIES="$SERIES" TAGS="$EXTRA_TAGS" LOCATION="" \
    "$SITE_ROOT/.venv/bin/python" "$(engine_path tools/scaffold-photos.py)" \
        --entry "$INDEX_MD" "$TARGET" "$SLUG" "$TITLE" \
    || { echo "import-photo: could not write $INDEX_MD" >&2; exit 1; }

GEO_PRESENT=""
if [ -f "$EXIF_SIDECAR" ] && grep -q '^geo:' "$EXIF_SIDECAR" 2>/dev/null; then
    GEO_PRESENT="yes"
fi
DIMS="$(magick identify -format '%w × %h' "$TARGET")"

echo
echo "import-photo: done."
echo "  Entry:   $INDEX_MD"
echo "  Photo:   $TARGET ($DIMS)"
echo "  Sidecars: $(basename "$EXIF_SIDECAR"), $(basename "$PALETTE_SIDECAR")"
echo
if [ -n "$GEO_PRESENT" ]; then
    echo
    echo "  NOTE: this photograph carries GPS coordinates in its EXIF."
    echo "  They are in the sidecar and were NOT written to frontmatter, and"
    echo "  the delivered JPEG has been stripped, so nothing is published yet."
    echo "  Add geo: + geo-precision: to $INDEX_MD only if you want it mapped."
fi

echo
echo "Next: edit $INDEX_MD to fill in title / abstract / tags, then 'make dev'."
