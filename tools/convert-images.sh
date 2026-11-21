#!/usr/bin/env bash
# convert-images.sh — Produce WebP companions for every local raster image.
#
# Walks static/ and content/ for JPEG and PNG files and calls cwebp to produce
# a .webp file alongside each one.  Existing .webp files are skipped (safe to
# re-run).  If cwebp is not found the script exits 0 so the build continues.
# An extension counts in either case (IMG_0042.JPG, as a camera writes it):
# build/Filters/Images.hs and tools/check-site.py look for its companion too.
#
# One companion, one image. A page shows map.png, map.JPG or map.gif with
# map.webp (build/Filters/Images.hs), so two of them beside each other have
# one companion between them and a page showing either would be served
# whichever was converted last: a draft's picture on a published page, for
# one. The same holds across static/ and the engine's static/, which are
# served as one tree with the site's file over the engine's: the site's
# images/color.png and the engine's images/color.JPG are both shown with
# /images/color.webp. The script refuses such images, naming them, before it
# converts anything, with or without cwebp, and removes the companion they
# share: nothing says whose pixels it holds, and it would outlast a rename.
# It also stops, and says so, when it cannot list the images at all.
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

# shellcheck source=tools/site-paths.sh
source "$(dirname "$0")/site-paths.sh"
# The site's images and content, and the engine's images once they move.
site_static="$SITE_ROOT/static"
engine_static="$(engine_path static)"
candidates=("$site_static" "$SITE_ROOT/content")
[ "$engine_static" = "$site_static" ] || candidates+=("$engine_static")
roots=()
for root in "${candidates[@]}"; do
    [ ! -d "$root" ] || roots+=("$root")
done

# A path as the site's author knows it.
show() {
    printf '%s' "${1#"$SITE_ROOT/"}"
}

# Every image a page may show with a WebP companion, in one order on every
# machine. The list is whole or the script stops: a walk that failed halfway
# would pass the check below for want of the files it did not reach.
list="$(mktemp)"
trap 'rm -f "$list"' EXIT
if [ "${#roots[@]}" -gt 0 ]; then
    find "${roots[@]}" \
         \( -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.png" -o -iname "*.gif" \) \
         -print0 > "$list" \
        || { echo "convert-images: could not list the images (find failed); nothing was checked or converted." >&2; exit 1; }
    LC_ALL=C sort -z -o "$list" "$list" \
        || { echo "convert-images: could not list the images (sort failed); nothing was checked or converted." >&2; exit 1; }
fi

# Which images would be shown with each companion. A companion is known by
# where it is served: beside its image under content/, and at one address
# for both static trees. A directory may be named as an image is
# (album.JPG) and is not one; a link to an image is.
images=()                # what cwebp reads: the JPEGs and PNGs
declare -A shown_with    # companion -> its images, a newline between them
shared=()                # the companions with more than one
while IFS= read -r -d '' img; do
    [[ -f "$img" ]] || continue
    case "${img##*.}" in
        [gG][iI][fF]) ;;     # shown with a companion, never given one here
        *) images+=("$img") ;;
    esac
    companion="${img%.*}.webp"
    case "$img" in
        "$engine_static"/*)
            rel="${img#"$engine_static"/}"
            # A site file of the same name is served in this one's place.
            [[ "$engine_static" == "$site_static" || ! -f "$site_static/$rel" ]] || continue
            companion="static:${rel%.*}.webp" ;;
        "$site_static"/*)
            rel="${img#"$site_static"/}"
            companion="static:${rel%.*}.webp" ;;
    esac
    if [[ -z "${shown_with[$companion]+set}" ]]; then
        shown_with[$companion]=$img
    else
        [[ "${shown_with[$companion]}" == *$'\n'* ]] || shared+=("$companion")
        shown_with[$companion]+=$'\n'$img
    fi
done < "$list"

for companion in ${shared[@]+"${shared[@]}"}; do
    mapfile -t sharing <<< "${shown_with[$companion]}"
    names="$(show "${sharing[0]}")"
    for ((i = 1; i < ${#sharing[@]}; i++)); do
        if ((i == ${#sharing[@]} - 1)); then names+=" and "; else names+=", "; fi
        names+="$(show "${sharing[i]}")"
    done
    # The files that are, or would become, the companion they are shown with.
    case "$companion" in
        static:*)
            rel="${companion#static:}"
            files=("$site_static/$rel")
            [ "$engine_static" = "$site_static" ] || files+=("$engine_static/$rel")
            what="the WebP companion served at /$rel" ;;
        *)
            files=("$companion")
            what="one WebP companion, $(show "$companion")" ;;
    esac
    echo "convert-images: $names share $what: a page showing one would be served another's pixels." >&2
    for file in "${files[@]}"; do
        [ -e "$file" ] || [ -L "$file" ] || continue
        if rm -f -- "$file"; then
            echo "  Removed $(show "$file"), which may hold any of theirs; the next build makes it again." >&2
        else
            echo "  Could not remove $(show "$file"), which may hold any of theirs: delete it." >&2
        fi
    done
    echo "  Rename all but one of the images." >&2
done
[ "${#shared[@]}" -eq 0 ] || exit 1

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

converted=0
skipped=0

for img in ${images[@]+"${images[@]}"}; do
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
done

echo "convert-images: ${converted} converted, ${skipped} already present."
