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
# Supply chain hardening: each downloaded file must match its line in
# tools/model-checksums.sha256 (tools/pin-check.sh), and a file with no
# line is refused. The checksums file is committed to git so a HuggingFace
# compromise (or a transparently rebuilt model) cannot silently land in the
# deployed site. To pin a new model version, run
# `ALLOW_UNPINNED=1 tools/download-model.sh`, verify the model
# out-of-band, and commit the lines it prints.

set -euo pipefail

# shellcheck source=tools/site-paths.sh
source "$(dirname "$0")/site-paths.sh"
no_site_copy models
MODEL_DIR="$(engine_path static)/models/all-MiniLM-L6-v2"
BASE_URL="https://huggingface.co/Xenova/all-MiniLM-L6-v2/resolve/main"
CHECKSUMS="$(engine_path tools/model-checksums.sha256)"
# shellcheck source=tools/pin-check.sh
source "$(engine_path tools/pin-check.sh)"

mkdir -p "$MODEL_DIR/onnx"

verify_sha() {
    local rel="$1" dst="$2"
    if ! pin_verify "$CHECKSUMS" "$rel" "$dst" model; then
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
    # Download to a temp name and move into place only after
    # verification: an interrupted curl must never leave a partial
    # file at the final path, where the present-file skip would accept it.
    curl -fsSL --progress-bar "$BASE_URL/$src" -o "$dst.part"
    verify_sha "$src" "$dst.part"
    mv "$dst.part" "$dst"
}

echo "Downloading all-MiniLM-L6-v2 to $MODEL_DIR ..."

fetch "config.json"                  "$MODEL_DIR/config.json"
fetch "tokenizer.json"               "$MODEL_DIR/tokenizer.json"
fetch "tokenizer_config.json"        "$MODEL_DIR/tokenizer_config.json"
fetch "special_tokens_map.json"      "$MODEL_DIR/special_tokens_map.json"
fetch "onnx/model_quantized.onnx"    "$MODEL_DIR/onnx/model_quantized.onnx"

echo "Done. static/models/all-MiniLM-L6-v2/ is ready."
echo "Run 'make build' to copy the model files into _site/."
