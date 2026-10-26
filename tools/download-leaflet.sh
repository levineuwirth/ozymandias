#!/usr/bin/env bash
# download-leaflet.sh — Vendor Leaflet + leaflet.markercluster into static/leaflet/.
#
# The /photography/map/ page (build/Photography.hs photographyMapRules)
# loads Leaflet from /leaflet/ to render a map of geo-tagged photos.
#
# Scope of this vendoring: the *library* — JS, CSS, and marker sprites —
# is served from this origin, so no third party sees a request for it and
# no CDN outage can break the page's scripting. The *map itself* is not
# offline. Tiles still come from Carto:
#
#   static/js/photography-map.js:
#     https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png
#
# so a visitor loading /photography/map/ makes third-party requests to
# basemaps.cartocdn.com for every tile, and the map renders empty if that
# host is unreachable or blocked. Vendoring the library removes one
# dependency, not the dependency. Serving tiles from this origin would
# mean self-hosting a tile set — deliberately out of scope.
#
# Run once before deploying. The vendored copy is gitignored
# (~150 KB total); re-running is safe — files that already exist AND
# match their pinned checksum are skipped; anything missing or
# mismatched is re-fetched.
#
# To bump the pinned versions, set LEAFLET_VERSION / MARKERCLUSTER_VERSION
# and re-run with ALLOW_UNPINNED=1, then add the lines it prints to
# tools/leaflet-checksums.sha256. Without a pinned line a file is refused
# (tools/pin-check.sh).

set -euo pipefail

# shellcheck source=tools/site-paths.sh
source "$(dirname "$0")/site-paths.sh"
no_site_copy leaflet
LEAFLET_DIR="$(engine_path static)/leaflet"
CHECKSUMS="$(engine_path tools/leaflet-checksums.sha256)"
# shellcheck source=tools/pin-check.sh
source "$(engine_path tools/pin-check.sh)"

LEAFLET_VERSION="${LEAFLET_VERSION:-1.9.4}"
MARKERCLUSTER_VERSION="${MARKERCLUSTER_VERSION:-1.5.3}"

UNPKG_LEAFLET="https://unpkg.com/leaflet@${LEAFLET_VERSION}/dist"
UNPKG_MC="https://unpkg.com/leaflet.markercluster@${MARKERCLUSTER_VERSION}/dist"

# Files to vendor: (URL_BASE LOCAL_PATH SOURCE_FILENAME PIN_KEY)
# PIN_KEY is the version-prefixed name in the checksum file so future
# version bumps don't accidentally accept files matching an older hash.
files_to_fetch=(
    "$UNPKG_LEAFLET|leaflet.js|leaflet-${LEAFLET_VERSION}-leaflet.js"
    "$UNPKG_LEAFLET|leaflet.css|leaflet-${LEAFLET_VERSION}-leaflet.css"
    "$UNPKG_LEAFLET/images|images/marker-icon.png|leaflet-${LEAFLET_VERSION}-marker-icon.png"
    "$UNPKG_LEAFLET/images|images/marker-icon-2x.png|leaflet-${LEAFLET_VERSION}-marker-icon-2x.png"
    "$UNPKG_LEAFLET/images|images/marker-shadow.png|leaflet-${LEAFLET_VERSION}-marker-shadow.png"
    "$UNPKG_MC|leaflet.markercluster.js|leaflet.markercluster-${MARKERCLUSTER_VERSION}-leaflet.markercluster.js"
    "$UNPKG_MC|MarkerCluster.css|leaflet.markercluster-${MARKERCLUSTER_VERSION}-MarkerCluster.css"
    "$UNPKG_MC|MarkerCluster.Default.css|leaflet.markercluster-${MARKERCLUSTER_VERSION}-MarkerCluster.Default.css"
)

mkdir -p "$LEAFLET_DIR/images"

# Per-file skip: existing files are skipped only after re-verifying
# their checksum, so a partial or tampered file from an interrupted
# earlier run can never be silently accepted. Downloads land in a
# .part temp and are only moved into place after verification — a
# failed verification leaves nothing at the final path.
for entry in "${files_to_fetch[@]}"; do
    IFS='|' read -r url_base local_path pin_key <<<"$entry"
    src_name="${local_path##*/}"
    target="$LEAFLET_DIR/$local_path"
    mkdir -p "$(dirname "$target")"

    if [ -f "$target" ]; then
        if pin_verify "$CHECKSUMS" "$pin_key" "$target" leaflet; then
            echo "leaflet: $local_path present and verified (skipping)"
            continue
        fi
        echo "leaflet: $local_path failed verification — re-fetching" >&2
        rm -f "$target"
    fi

    echo "leaflet: fetching $local_path ($pin_key)"
    tmp="$target.part"
    curl -fsSL --progress-bar "$url_base/$src_name" -o "$tmp"
    if ! pin_verify "$CHECKSUMS" "$pin_key" "$tmp" leaflet; then
        rm -f "$tmp"
        echo "leaflet: refusing to vendor unverified $local_path" >&2
        exit 1
    fi
    mv "$tmp" "$target"
done

echo "leaflet: vendored to $LEAFLET_DIR"
