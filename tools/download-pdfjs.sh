#!/usr/bin/env bash
# download-pdfjs.sh — Vendor Mozilla's prebuilt PDF.js viewer into static/pdfjs/.
#
# The Haskell link filter (build/Filters/Links.hs) rewrites every root-relative
# .pdf link to open through /pdfjs/web/viewer.html, so this viewer must be
# present in static/ for the site build to produce working PDF links.
#
# Run once before deploying. The extracted viewer is gitignored (~18 MB
# uncompressed); re-running is safe — the script skips when the viewer
# already exists.
#
# To bump the pinned version, set PDFJS_VERSION, re-run, then update
# tools/pdfjs-checksums.sha256 with the new archive SHA-256.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
PDFJS_DIR="$REPO_ROOT/static/pdfjs"
CHECKSUMS="$REPO_ROOT/tools/pdfjs-checksums.sha256"

PDFJS_VERSION="${PDFJS_VERSION:-5.6.205}"
ARCHIVE="pdfjs-${PDFJS_VERSION}-dist.zip"
URL="https://github.com/mozilla/pdf.js/releases/download/v${PDFJS_VERSION}/${ARCHIVE}"

if [ -f "$PDFJS_DIR/web/viewer.html" ]; then
    echo "pdfjs: already present at $PDFJS_DIR/web/viewer.html (skipping)"
    exit 0
fi

command -v unzip >/dev/null 2>&1 || {
    echo "download-pdfjs: unzip not found — install it (pacman -S unzip / apt install unzip)" >&2
    exit 1
}

tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

echo "pdfjs: downloading $ARCHIVE"
curl -fsSL --progress-bar "$URL" -o "$tmpdir/$ARCHIVE"

if [ -f "$CHECKSUMS" ]; then
    want=$(awk -v p="$ARCHIVE" '$2 == p { print $1; exit }' "$CHECKSUMS")
    if [ -n "$want" ]; then
        got=$(sha256sum "$tmpdir/$ARCHIVE" | awk '{ print $1 }')
        if [ "$got" != "$want" ]; then
            echo "pdfjs: sha256 mismatch for $ARCHIVE" >&2
            echo "       expected $want" >&2
            echo "       got      $got" >&2
            exit 1
        fi
        echo "pdfjs: sha256 verified"
    else
        echo "pdfjs: no pinned checksum for $ARCHIVE in $CHECKSUMS — skipping verification" >&2
    fi
else
    echo "pdfjs: $CHECKSUMS not found — skipping sha256 verification" >&2
fi

mkdir -p "$PDFJS_DIR"
echo "pdfjs: extracting to $PDFJS_DIR"
unzip -q -o "$tmpdir/$ARCHIVE" -d "$PDFJS_DIR"

# Strip artifacts that are never needed by site users. Saves ~11 MB on
# disk and in rsync; none are referenced by viewer.html at runtime.
#   *.map                           sourcemaps (devtools-only)
#   web/debugger.mjs, debugger.css  PDF.js developer panel
#   web/compressed.tracemonkey-*.pdf  demo PDF shipped as the viewer's default
echo "pdfjs: stripping unused artifacts"
find "$PDFJS_DIR" -type f -name '*.map' -delete
rm -f "$PDFJS_DIR/web/debugger.mjs" "$PDFJS_DIR/web/debugger.css"
rm -f "$PDFJS_DIR"/web/compressed.tracemonkey-*.pdf

echo "pdfjs: done. static/pdfjs/web/viewer.html is ready."
echo "       Run 'make build' to include it in _site/."
