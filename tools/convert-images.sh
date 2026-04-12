#!/usr/bin/env bash
# convert-images.sh — Produce WebP companions for every local raster image.
#
# Walks static/ and content/ for JPEG and PNG files and calls cwebp to produce
# a .webp file alongside each one.  Existing .webp files are skipped (safe to
# re-run).  If cwebp is not found the script exits 0 so the build continues.
#
# Requires: cwebp (libwebp) — pacman -S libwebp  /  apt install webp
#
# Quality: -q 85 is a good default for photographic content.  For images that
# are already highly compressed, -lossless avoids further degradation.

set -euo pipefail

if ! command -v cwebp >/dev/null 2>&1; then
    echo "convert-images: cwebp not found — skipping WebP conversion." >&2
    echo "  Install: pacman -S libwebp  (or: apt install webp)" >&2
    exit 0
fi

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

converted=0
skipped=0

while IFS= read -r -d '' img; do
    webp="${img%.*}.webp"
    if [ -f "$webp" ] && [ ! "$img" -nt "$webp" ]; then
        skipped=$((skipped + 1))
    else
        echo "  webp  ${img#"$REPO_ROOT/"}"
        cwebp -quiet -q 85 "$img" -o "$webp"
        converted=$((converted + 1))
    fi
done < <(find "$REPO_ROOT/static" "$REPO_ROOT/content" \
              \( -name "*.jpg" -o -name "*.jpeg" -o -name "*.png" \) \
              -print0 2>/dev/null)

echo "convert-images: ${converted} converted, ${skipped} already present."
