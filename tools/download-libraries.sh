#!/usr/bin/env bash
# download-libraries.sh — Vendor the browser libraries the site loaded from
# jsDelivr, so they are served from this origin.
#
#   tools/download-libraries.sh transformers   # semantic search
#   tools/download-libraries.sh vega           # interactive figures
#
# transformers: transformers.js 2.17.2 (static/js/semantic-search.js imports
#   it from /transformers/) and the two single-threaded builds of the ONNX
#   runtime it carries, which it fetches from the same directory
#   (env.backends.onnx.wasm.wasmPaths). A browser takes the SIMD build, or
#   the plain one where SIMD is missing. The threaded builds are not
#   vendored: they need cross-origin isolation, which the site does not
#   have, and semantic-search.js asks for one thread.
# vega: Vega 5.33.1, Vega-Lite 5.23.0 and Vega-Embed 6.29.0
#   (templates/partials/head.html, on pages with `viz: true`). Kept on the
#   5/5/6 line deliberately: vega 6 / vega-lite 6 / vega-embed 7 are a spec
#   migration for every authored figure, not a pin. These are the bytes the
#   template's old Subresource Integrity hashes named.
#
# Files are gitignored and land in static/<group>/, which the build serves
# at /<group>/. Re-running is safe: a present file is kept only after its
# checksum is checked again; anything missing or mismatched is fetched to a
# .part file and moved into place only once verified.
#
# To bump a version, change it below, run once with ALLOW_UNPINNED=1, check
# the new files, and add the lines it prints to
# tools/library-checksums.sha256 (tools/pin-check.sh). Their licenses are in
# static/licenses/ and named in the site's THIRD-PARTY.md.

set -euo pipefail

# shellcheck source=tools/site-paths.sh
source "$(dirname "$0")/site-paths.sh"
CHECKSUMS="$(engine_path tools/library-checksums.sha256)"
# shellcheck source=tools/pin-check.sh
source "$(engine_path tools/pin-check.sh)"

TRANSFORMERS_VERSION=2.17.2
VEGA_VERSION=5.33.1
VEGA_LITE_VERSION=5.23.0
VEGA_EMBED_VERSION=6.29.0
NPM=https://cdn.jsdelivr.net/npm

group="${1:-}"
case "$group" in
    transformers)
        base="$NPM/@xenova/transformers@$TRANSFORMERS_VERSION/dist"
        files=(
            "$base/transformers.min.js|transformers.min.js|transformers-$TRANSFORMERS_VERSION-transformers.min.js"
            "$base/ort-wasm-simd.wasm|ort-wasm-simd.wasm|transformers-$TRANSFORMERS_VERSION-ort-wasm-simd.wasm"
            "$base/ort-wasm.wasm|ort-wasm.wasm|transformers-$TRANSFORMERS_VERSION-ort-wasm.wasm"
        ) ;;
    vega)
        files=(
            "$NPM/vega@$VEGA_VERSION/build/vega.min.js|vega.min.js|vega-$VEGA_VERSION-vega.min.js"
            "$NPM/vega-lite@$VEGA_LITE_VERSION/build/vega-lite.min.js|vega-lite.min.js|vega-lite-$VEGA_LITE_VERSION-vega-lite.min.js"
            "$NPM/vega-embed@$VEGA_EMBED_VERSION/build/vega-embed.min.js|vega-embed.min.js|vega-embed-$VEGA_EMBED_VERSION-vega-embed.min.js"
        ) ;;
    *)
        echo "usage: $0 transformers|vega" >&2
        exit 2 ;;
esac

no_site_copy "$group"
dir="$(engine_path static)/$group"
mkdir -p "$dir"

for entry in "${files[@]}"; do
    IFS='|' read -r url name key <<<"$entry"
    target="$dir/$name"
    if [ -f "$target" ]; then
        if pin_verify "$CHECKSUMS" "$key" "$target" "$group"; then
            echo "$group: $name present and verified (skipping)"
            continue
        fi
        echo "$group: $name failed verification — re-fetching" >&2
        rm -f "$target"
    fi
    echo "$group: fetching $name ($key)"
    curl -fsSL --progress-bar "$url" -o "$target.part"
    if ! pin_verify "$CHECKSUMS" "$key" "$target.part" "$group"; then
        rm -f "$target.part"
        echo "$group: refusing to vendor unverified $name" >&2
        exit 1
    fi
    mv "$target.part" "$target"
done

echo "$group: vendored to $dir"
