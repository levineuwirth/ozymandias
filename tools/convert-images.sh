#!/usr/bin/env bash
# convert-images.sh — Produce WebP companions for every local raster image.
#
# Walks static/ and content/ for JPEG and PNG files and calls cwebp to produce
# a .webp file alongside each one.  Existing .webp files are skipped (safe to
# re-run).  If cwebp is not found the script exits 0 so the build continues.
#
# Photography's .w480/.w960/.w1440 variants arrive with WebP siblings that
# tools/generate-thumbnails.py encoded from the source pixels (it runs first
# in `make build`); they are newer than their JPEG, so this skips them. It
# covers the full-size delivery files and every other image.
#
# Requires: cwebp — Arch ships it in libwebp-utils (NOT libwebp, which
#   is library-only). Debian/Ubuntu ship it in the webp package.
#   Install: pacman -S libwebp-utils  /  apt install webp
#
# Quality: -q 85 is a good default for photographic content.  For images that
# are already highly compressed, -lossless avoids further degradation.

set -euo pipefail

if ! command -v cwebp >/dev/null 2>&1; then
    # Exit 0 deliberately: build/Filters/Images.hs only emits a
    # <source type="image/webp"> when the .webp file actually exists, so a
    # build without cwebp produces a correct site — just a heavier one.
    #
    # But "correct and silently 3x heavier" is exactly the failure the
    # audit found shipped: zero WebP files site-wide, 375 JPEGs, and a
    # one-line note nobody read. So the notice is loud, and the production
    # knob is `tools/check-site.py --require-webp`, which fails on any
    # JPEG/PNG missing its WebP companion, including partial conversion.
    cat >&2 <<'WARN'

  ==============================================================
   WARNING: cwebp not found — full-size WebP conversion skipped.
  ==============================================================

   Existing WebPs and photo variants made by Pillow remain usable.
   Images without WebP companions fall back to JPEG/PNG. The site
   stays usable, but those images ship more bytes.

   Install the converter:

     Arch    pacman -S libwebp-utils     <- NOT libwebp; that
                                            package is the
                                            library only and
                                            has no cwebp binary
     Debian  apt install webp
     macOS   brew install webp

   Then re-run `make build`. To make this a hard failure in a
   production build, run:  engine/tools/check-site.py _site --require-webp
   (`make validate REQUIRE_WEBP=1`).

WARN
    exit 0
fi

# shellcheck source=tools/site-paths.sh
source "$(dirname "$0")/site-paths.sh"
# The site's images and content, and the engine's images once they move.
roots=("$SITE_ROOT/static" "$SITE_ROOT/content")
[ "$(engine_path static)" = "$SITE_ROOT/static" ] || roots+=("$(engine_path static)")

converted=0
skipped=0

while IFS= read -r -d '' img; do
    webp="${img%.*}.webp"
    if [ -f "$webp" ] && [ ! "$img" -nt "$webp" ]; then
        skipped=$((skipped + 1))
    else
        echo "  webp  ${img#"$SITE_ROOT/"}"
        # Write to a temp name then move: an interrupted cwebp would
        # otherwise leave a truncated .webp that is newer than its
        # source, which the staleness gate above then skips forever.
        cwebp -quiet -q 85 "$img" -o "$webp.part"
        mv "$webp.part" "$webp"
        converted=$((converted + 1))
    fi
done < <(find "${roots[@]}" \
              \( -name "*.jpg" -o -name "*.jpeg" -o -name "*.png" \) \
              -print0 2>/dev/null)

echo "convert-images: ${converted} converted, ${skipped} already present."
