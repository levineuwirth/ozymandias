#!/usr/bin/env bash
# download-pdfjs.sh — Vendor Mozilla's prebuilt PDF.js viewer into static/pdfjs/.
#
# The Haskell link filter (build/Filters/Links.hs) rewrites every root-relative
# .pdf link to open through /pdfjs/web/viewer.html, so this viewer must be
# present in static/ for the site build to produce working PDF links.
#
# Runs on every `make build`; the extracted viewer is gitignored (~18 MB).
#
# What "already installed" means
# ------------------------------
# It used to mean "web/viewer.html exists". That hid two failures:
#
#   * Bumping PDFJS_VERSION did nothing. The old viewer stayed in place and
#     the site kept shipping it, with no message saying so.
#   * An extraction interrupted after viewer.html was written looked
#     complete forever, leaving a viewer whose pdf.worker.mjs is missing.
#
# So the installed copy now carries a .version stamp, and a run is a no-op
# only when that stamp matches the requested pin AND every file in
# REQUIRED_FILES is present and non-empty. Anything else reinstalls.
#
# The reinstall is staged: the archive is extracted into a sibling
# directory, stripped, and inventory-checked there, and only a staging tree
# that passes is renamed over the live one. A `rename(2)` within static/ is
# atomic, so a concurrent site build sees either the old complete viewer or
# the new complete viewer, never a half-written one.
#
# To bump the pinned version, set PDFJS_VERSION and re-run with
# ALLOW_UNPINNED=1, then add the line it prints to
# tools/pdfjs-checksums.sha256. Without a pinned line the archive is
# refused (tools/pin-check.sh).
#
# Test-only overrides:
#   PDFJS_DIR           install somewhere other than static/pdfjs
#   PDFJS_ARCHIVE_PATH  use a local zip instead of downloading

set -euo pipefail

# shellcheck source=tools/site-paths.sh
source "$(dirname "$0")/site-paths.sh"
no_site_copy pdfjs
PDFJS_DIR="${PDFJS_DIR:-$(engine_path static)/pdfjs}"
CHECKSUMS="${PDFJS_CHECKSUMS:-$(engine_path tools/pdfjs-checksums.sha256)}"
# shellcheck source=tools/pin-check.sh
source "$(engine_path tools/pin-check.sh)"

PDFJS_VERSION="${PDFJS_VERSION:-5.6.205}"
ARCHIVE="pdfjs-${PDFJS_VERSION}-dist.zip"
URL="https://github.com/mozilla/pdf.js/releases/download/v${PDFJS_VERSION}/${ARCHIVE}"

VERSION_FILE="$PDFJS_DIR/.version"

# Everything the site actually loads at runtime. viewer.html pulls
# viewer.mjs and viewer.css; viewer.mjs imports build/pdf.mjs, which spawns
# build/pdf.worker.mjs. Missing any one of them is a broken PDF page, so
# any one missing means "not installed".
REQUIRED_FILES=(
    "web/viewer.html"
    "web/viewer.mjs"
    "web/viewer.css"
    "build/pdf.mjs"
    "build/pdf.worker.mjs"
    "LICENSE"
)

# Every required file present and non-empty under $1.
inventory_ok() {
    local root="$1" missing=0 rel
    for rel in "${REQUIRED_FILES[@]}"; do
        if [ ! -s "$root/$rel" ]; then
            echo "pdfjs: missing or empty $rel" >&2
            missing=$((missing + 1))
        fi
    done
    [ "$missing" -eq 0 ]
}

installed_version() {
    [ -f "$VERSION_FILE" ] && cat "$VERSION_FILE" || echo ""
}

# The version PDF.js itself reports, read out of the bundle. Used only to
# adopt an installation made before .version existed, so that introducing
# the stamp does not force everyone into one gratuitous 18 MB re-download.
detected_version() {
    [ -f "$PDFJS_DIR/build/pdf.mjs" ] || return 0
    grep -aoE 'version = "[0-9]+\.[0-9]+\.[0-9]+"' "$PDFJS_DIR/build/pdf.mjs" \
        | head -1 \
        | sed -E 's/.*"([^"]+)".*/\1/'
}

# --- Is a reinstall needed? -------------------------------------------------

have="$(installed_version)"
if [ -z "$have" ] && inventory_ok "$PDFJS_DIR" 2>/dev/null; then
    detected="$(detected_version)"
    if [ -n "$detected" ]; then
        echo "pdfjs: adopting unstamped installation (detected $detected)"
        printf '%s\n' "$detected" > "$VERSION_FILE"
        have="$detected"
    fi
fi
if [ "$have" = "$PDFJS_VERSION" ] && inventory_ok "$PDFJS_DIR" 2>/dev/null; then
    echo "pdfjs: $PDFJS_VERSION already installed and complete (skipping)"
    exit 0
fi

if [ -e "$PDFJS_DIR" ]; then
    if [ -n "$have" ] && [ "$have" != "$PDFJS_VERSION" ]; then
        echo "pdfjs: installed $have != requested $PDFJS_VERSION — reinstalling"
    else
        echo "pdfjs: installation absent, unstamped, or incomplete — reinstalling"
    fi
fi

command -v unzip >/dev/null 2>&1 || {
    echo "download-pdfjs: unzip not found — install it (pacman -S unzip / apt install unzip)" >&2
    exit 1
}

# --- Fetch ------------------------------------------------------------------

tmpdir=$(mktemp -d)
# The staging tree must be a sibling of the install path so the final `mv`
# is a same-filesystem rename, not a cross-device copy that can be
# interrupted halfway.
mkdir -p "$(dirname "$PDFJS_DIR")"
stagedir=$(mktemp -d "$(dirname "$PDFJS_DIR")/.pdfjs-stage.XXXXXX")
trap 'rm -rf "$tmpdir" "$stagedir"' EXIT

if [ -n "${PDFJS_ARCHIVE_PATH:-}" ]; then
    echo "pdfjs: using local archive $PDFJS_ARCHIVE_PATH"
    cp "$PDFJS_ARCHIVE_PATH" "$tmpdir/$ARCHIVE"
else
    echo "pdfjs: downloading $ARCHIVE"
    curl -fsSL --progress-bar "$URL" -o "$tmpdir/$ARCHIVE"
fi

pin_verify "$CHECKSUMS" "$ARCHIVE" "$tmpdir/$ARCHIVE" pdfjs || exit 1
echo "pdfjs: sha256 verified"

# --- Stage ------------------------------------------------------------------

echo "pdfjs: staging in $stagedir"
unzip -q -o "$tmpdir/$ARCHIVE" -d "$stagedir"

# Strip artifacts that are never needed by site users. Saves ~11 MB on
# disk and in rsync; none are referenced by viewer.html at runtime.
#   *.map                           sourcemaps (devtools-only)
#   web/debugger.mjs, debugger.css  PDF.js developer panel
#   web/compressed.tracemonkey-*.pdf  demo PDF shipped as the viewer's default
echo "pdfjs: stripping unused artifacts"
find "$stagedir" -type f -name '*.map' -delete
rm -f "$stagedir/web/debugger.mjs" "$stagedir/web/debugger.css"
rm -f "$stagedir"/web/compressed.tracemonkey-*.pdf

if ! inventory_ok "$stagedir"; then
    echo "pdfjs: staged extraction is incomplete — refusing to install" >&2
    exit 1
fi
printf '%s\n' "$PDFJS_VERSION" > "$stagedir/.version"

# --- Swap -------------------------------------------------------------------

if [ -e "$PDFJS_DIR" ]; then
    retired="$PDFJS_DIR.retired.$$"
    mv "$PDFJS_DIR" "$retired"
    if ! mv "$stagedir" "$PDFJS_DIR"; then
        # Put the previous install back rather than leaving nothing there.
        mv "$retired" "$PDFJS_DIR"
        echo "pdfjs: install failed — previous copy restored" >&2
        exit 1
    fi
    rm -rf "$retired"
else
    mv "$stagedir" "$PDFJS_DIR"
fi
# $stagedir no longer exists; keep the trap from complaining about it.
stagedir="$tmpdir/already-moved"

echo "pdfjs: installed $PDFJS_VERSION at $PDFJS_DIR"
echo "       Run 'make build' to include it in _site/."
