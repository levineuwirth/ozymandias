#!/usr/bin/env bash
# download-model.sh — Download the quantized ONNX model for client-side semantic search.
#
# Downloads Xenova/all-MiniLM-L6-v2 (quantized ONNX, ~22 MB total) from HuggingFace
# into static/models/all-MiniLM-L6-v2/ so it can be served same-origin, keeping the
# nginx CSP to 'self' for connect-src.
#
# Run once before deploying. Files are gitignored (binary artifacts).
# Re-running is safe — existing files are skipped.
#
# Supply chain hardening: each downloaded file is verified against
# tools/model-checksums.sha256 if that file is present. The checksums
# file is committed to git so a HuggingFace compromise (or a
# transparently rebuilt model) cannot silently land in the deployed
# site. To pin a new model version, run `tools/download-model.sh`,
# verify the model out-of-band, then commit the generated checksums:
#
#     (cd static/models/all-MiniLM-L6-v2 && \
#      sha256sum config.json tokenizer.json tokenizer_config.json \
#                special_tokens_map.json onnx/model_quantized.onnx) \
#      > tools/model-checksums.sha256

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
MODEL_DIR="$REPO_ROOT/static/models/all-MiniLM-L6-v2"
BASE_URL="https://huggingface.co/Xenova/all-MiniLM-L6-v2/resolve/main"
CHECKSUMS="$REPO_ROOT/tools/model-checksums.sha256"

mkdir -p "$MODEL_DIR/onnx"

# Look up the expected SHA-256 for a relative path, or empty string if
# no checksums file is present (or the path isn't pinned yet).
expected_sha() {
    local rel="$1"
    if [ ! -f "$CHECKSUMS" ]; then
        echo ""
        return
    fi
    # Match lines of the form "<sha>  <relative-path>"
    awk -v p="$rel" '$2 == p { print $1; exit }' "$CHECKSUMS"
}

verify_sha() {
    local rel="$1" dst="$2"
    local want
    want=$(expected_sha "$rel")
    if [ -z "$want" ]; then
        return
    fi
    local got
    got=$(sha256sum "$dst" | awk '{ print $1 }')
    if [ "$got" != "$want" ]; then
        echo "  ERROR sha256 mismatch for $rel" >&2
        echo "        expected $want" >&2
        echo "        got      $got" >&2
        rm -f "$dst"
        exit 1
    fi
    echo "  ok    sha256 verified for $rel"
}

fetch() {
    local src="$1" dst="$2"
    if [ -f "$dst" ]; then
        echo "  skip  $src (already present)"
        verify_sha "$src" "$dst"
        return
    fi
    echo "  fetch $src"
    curl -fsSL --progress-bar "$BASE_URL/$src" -o "$dst"
    verify_sha "$src" "$dst"
}

if [ ! -f "$CHECKSUMS" ]; then
    echo "Note: $CHECKSUMS not found — downloads will not be SHA-verified." >&2
    echo "      See the comment in tools/download-model.sh to pin checksums." >&2
fi

echo "Downloading all-MiniLM-L6-v2 to $MODEL_DIR ..."

fetch "config.json"                  "$MODEL_DIR/config.json"
fetch "tokenizer.json"               "$MODEL_DIR/tokenizer.json"
fetch "tokenizer_config.json"        "$MODEL_DIR/tokenizer_config.json"
fetch "special_tokens_map.json"      "$MODEL_DIR/special_tokens_map.json"
fetch "onnx/model_quantized.onnx"    "$MODEL_DIR/onnx/model_quantized.onnx"

echo "Done. static/models/all-MiniLM-L6-v2/ is ready."
echo "Run 'make build' to copy the model files into _site/."
