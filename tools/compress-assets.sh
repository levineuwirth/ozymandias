#!/usr/bin/env bash
# compress-assets.sh — Generate .gz (and .br, if brotli is installed) sidecars
# for compressible text assets in _site/.
#
# Pairs with nginx `gzip_static on` / `brotli_static on`: nginx serves the
# pre-compressed file when the client advertises a matching Accept-Encoding,
# so each build pays the compression cost once (at brotli -q 11) instead of
# the server paying it on every request.
#
# Only files >= MIN_SIZE bytes are compressed — below that, the compression
# framing overhead can exceed the savings.
#
# Sidecars come from a cache keyed by the SHA-256 of the source's bytes
# ($COMPRESS_CACHE, data/.compress-cache by default; it survives `site --
# clean`). A source whose bytes were compressed before, in this build or any
# earlier one, costs a hash and a reflink copy; only new bytes go through
# brotli -q 11. Before, reuse was decided by mtimes: the brotli CLI copies its
# input's mtime to its output, so every .br looked stale and all 342 MB were
# recompressed on every build (audit X2/D01), and no mtime rule can see
# changed content behind an unchanged mtime. A hash sees both. Entries unused
# for 30 days are pruned.
#
# Three ways a sidecar can go stale, all handled here because nginx serves
# it in preference to the source whenever the client sends the matching
# Accept-Encoding — a stale sidecar is a *wrong response*, not a slow one:
#
#   1. The source shrank below MIN_SIZE. The old sidecar is not overwritten
#      (nothing is compressed), so it keeps decoding to the previous, larger
#      content. Deleted here.
#   2. The source was deleted. Its sidecars are orphans that nginx will
#      still happily serve. Swept here, but only when the stripped name has
#      one of the compressible extensions below, so an intentionally
#      published foo.tar.gz is never touched.
#   3. The compressor was interrupted or produced garbage. Every sidecar
#      this run writes is decompressed and `cmp`-ed against its source
#      before it is moved into place.
#
# Usage:
#   ./tools/compress-assets.sh              # compress _site/
#   ./tools/compress-assets.sh path/to/dir  # compress a specific directory
#
# MIN_SIZE is overridable (bytes); the directory argument is what tests use
# to run this against a scratch tree.

set -euo pipefail

SITE_DIR="${1:-_site}"
MIN_SIZE="${MIN_SIZE:-1024}"  # bytes
COMPRESS_CACHE="${COMPRESS_CACHE:-$(dirname "$SITE_DIR")/data/.compress-cache}"

if [[ ! "$MIN_SIZE" =~ ^[0-9]+$ ]]; then
    echo "compress-assets: MIN_SIZE must be a positive integer (got '$MIN_SIZE')" >&2
    exit 1
fi

if [ ! -d "$SITE_DIR" ]; then
    echo "compress-assets: directory '$SITE_DIR' not found" >&2
    exit 1
fi

have_brotli=0
if command -v brotli >/dev/null 2>&1; then
    have_brotli=1
else
    echo "compress-assets: brotli not found — generating gzip only" >&2
    echo "                 (install: pacman -S brotli  /  apt install brotli)" >&2
fi

# Export for subshells invoked by xargs.
export MIN_SIZE
export have_brotli
export COMPRESS_CACHE
mkdir -p "$COMPRESS_CACHE"

compress_one() {
    local src="$1"
    local size
    size=$(stat -c '%s' "$src" 2>/dev/null || stat -f '%z' "$src")

    if [ "$size" -lt "$MIN_SIZE" ]; then
        # Below the threshold nothing is written — so any sidecar sitting
        # here is from a previous, larger version of this file and would
        # decode to content the source no longer has. Remove it.
        for stale in "$src.gz" "$src.br"; do
            if [ -f "$stale" ]; then
                echo "  drop  ${stale} (source below MIN_SIZE)" >&2
                rm -f "$stale"
            fi
        done
        rm -f "$src.gz.tmp" "$src.br.tmp"
        return
    fi

    # One cache entry per (content, encoding). A miss compresses into the
    # cache and proves the result round-trips; a hit is trusted, since the
    # entry was proven against these exact bytes when it was made.
    local h kind cached tmp
    h=$(sha256sum "$src" | cut -c1-64)
    for kind in gz br; do
        if [ "$kind" = br ] && [ "$have_brotli" != 1 ]; then
            # A previously built sidecar may describe older source bytes.
            # nginx prefers it, so gzip-only builds must remove it.
            rm -f "$src.br" "$src.br.tmp"
            continue
        fi
        cached="$COMPRESS_CACHE/${h:0:2}/$h.$kind"
        if [ -f "$cached" ]; then
            touch "$cached"                     # recently used: kept by the prune
        else
            mkdir -p "${cached%/*}"
            tmp="$cached.tmp.$BASHPID"
            case "$kind" in
                # -n: no name or mtime in the header, so the bytes depend on content alone
                gz) gzip -9 -n -c "$src" > "$tmp" ;;
                br) brotli -Z -c "$src" > "$tmp" ;;
            esac || { rm -f "$tmp"; return 1; }
            local decode=(gzip -dc)
            [ "$kind" = br ] && decode=(brotli -dc)
            if ! "${decode[@]}" "$tmp" | cmp -s - "$src"; then
                rm -f "$tmp"
                echo "compress-assets: $kind sidecar for $src did not round-trip" >&2
                return 1
            fi
            mv -f "$tmp" "$cached"
        fi
        # Replace the sidecar only when its bytes differ, so an unchanged
        # sidecar keeps its mtime and rsync leaves it alone.
        if ! cmp -s "$cached" "$src.$kind"; then
            cp --reflink=auto "$cached" "$src.$kind.tmp" && mv -f "$src.$kind.tmp" "$src.$kind" \
                || { rm -f "$src.$kind.tmp"; return 1; }
        fi
    done
}
export -f compress_one

# Extensions worth compressing. Images (png/jpg/webp) and PDFs are already
# compressed; fonts (woff2) are zstd/brotli internally — don't re-wrap.
# onnx: the 23 MB search model brotlis to 15.7 MB, fetched on the first
# semantic query; ico and pdf.js's .ftl locale files shrink by 70-80 %
# (audit O17). Kept in one place because the orphan sweep below has to
# agree with it exactly: a suffix this list does not claim must never be
# deleted.
COMPRESSIBLE_EXTS=(html css js mjs json svg xml txt wasm onnx ico ftl)

find_predicates=()
for ext in "${COMPRESSIBLE_EXTS[@]}"; do
    if [ "${#find_predicates[@]}" -gt 0 ]; then
        find_predicates+=(-o)
    fi
    find_predicates+=(-name "*.$ext")
done

# --- Sweep 1: orphaned and interrupted sidecars ----------------------------
#
# A source deleted since the last run leaves foo.html.gz behind, and nginx
# will still serve it to any client advertising gzip. Delete a sidecar only
# when its stripped name has a compressible extension, so a deliberately
# published archive (foo.tar.gz) survives.
orphans=0
while IFS= read -r -d '' sidecar; do
    src="${sidecar%.*}"
    [ -e "$src" ] && continue
    ext="${src##*.}"
    for known in "${COMPRESSIBLE_EXTS[@]}"; do
        if [ "$ext" = "$known" ]; then
            echo "  drop  $sidecar (source no longer exists)" >&2
            rm -f "$sidecar"
            orphans=$((orphans + 1))
            break
        fi
    done
done < <(find "$SITE_DIR" -type f \( -name '*.gz' -o -name '*.br' \) -print0)

# Debris from an interrupted earlier run. These are never served (nginx
# looks for exactly .gz/.br) but they do get rsynced to the VPS.
while IFS= read -r -d '' debris; do
    rm -f "$debris"
done < <(find "$SITE_DIR" -type f \( -name '*.gz.tmp' -o -name '*.br.tmp' \) -print0)

# --- Sweep 2: compress ------------------------------------------------------
if ! find "$SITE_DIR" -type f \( "${find_predicates[@]}" \) \
        -not -name '*.gz' \
        -not -name '*.br' \
        -print0 \
    | xargs -0 -P "$(nproc 2>/dev/null || echo 4)" -I {} bash -c 'compress_one "$@"' _ {}
then
    echo "compress-assets: one or more sidecars failed to verify — aborting" >&2
    exit 1
fi

# Cache entries no build has used for 30 days, and debris from interrupted runs.
find "$COMPRESS_CACHE" -type f \( -name '*.gz' -o -name '*.br' \) -mtime +30 -delete 2>/dev/null || true
find "$COMPRESS_CACHE" -type f -name '*.tmp.*' -mmin +60 -delete 2>/dev/null || true

echo "compress-assets: sidecars written under $SITE_DIR/ ($orphans orphan(s) removed)"
