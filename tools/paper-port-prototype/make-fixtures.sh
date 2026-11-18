#!/bin/bash
# Regenerate the prototype's test fixtures from the papers' sources.
#
#   make-fixtures.sh [dest]        (default: fixtures/ beside this script)
#
# For each paper: copy its source (no build outputs) to <dest>/<name>/src,
# build it plain (A/) and with ozyport.sty's marks injected (B/), and write
# the source's figure inventory (inventory.json) and provenance (SOURCE).
# The paper's own files are never changed: the marks come from
# `latexmk -usepretex`, with this directory on TEXINPUTS.
#
# Sources default to the author's checkouts; override with
#   PROOF_BROKER=<paper dir> SIMD=<paper dir> make-fixtures.sh
set -euo pipefail
HERE=$(dirname "$(readlink -f "$0")")
DEST=$(readlink -f "${1:-$HERE/fixtures}")
PROOF_BROKER=${PROOF_BROKER:-$HOME/Repos/research/proof-broker/paper}
SIMD=${SIMD:-$HOME/Repos/research/where-simd-helps/paper}
LATEXMK=(latexmk -pdf -interaction=nonstopmode -halt-on-error -jobname=main)

fixture() {
    local name=$1 src=$2 out=$DEST/$1
    [ -f "$src/main.tex" ] || { echo "$name: no main.tex in $src" >&2; exit 1; }
    rm -rf "$out"
    mkdir -p "$out"
    # Sources only: the paper's build directory, review and check outputs,
    # and LaTeX's by-products stay behind.
    rsync -a --exclude build/ --exclude reviews/ --exclude checks/ --exclude .git \
        --exclude 'main.aux' --exclude 'main.bbl' --exclude 'main.blg' --exclude 'main.fdb_latexmk' \
        --exclude 'main.fls' --exclude 'main.log' --exclude 'main.out' --exclude 'main.pdf' \
        --exclude '*.ozyfig' --exclude arxiv/ --exclude arxiv.tar.gz \
        "$src/" "$out/src/"
    cp -r "$out/src" "$out/A"
    cp -r "$out/src" "$out/B"
    (cd "$out/A" && nice "${LATEXMK[@]}" main.tex >build.out 2>&1) &
    (cd "$out/B" && TEXINPUTS="$HERE:" nice "${LATEXMK[@]}" -usepretex='\RequirePackage{ozyport}' main.tex >build.out 2>&1) &
    wait
    for v in A B; do
        [ -f "$out/$v/main.pdf" ] || { echo "$name: the $v build failed; see $out/$v/build.out" >&2; exit 1; }
    done
    python3 -I "$HERE/inventory.py" "$out/src/main.tex" >"$out/inventory.json"
    {
        echo "source: $src"
        git -C "$src" log -1 --format='commit: %H %cd' -- . 2>/dev/null || echo "commit: (not a git checkout)"
        if [ -n "$(git -C "$src" status --porcelain -- . 2>/dev/null)" ]; then
            echo "working tree: uncommitted changes (the copy is of the working tree)"
        else
            echo "working tree: clean"
        fi
        echo "source digest: $(cd "$out/src" && find . -type f -print0 | sort -z | xargs -0 sha256sum | sha256sum | cut -c1-16)"
        echo "pages: $(pdfinfo "$out/A/main.pdf" | awk '/^Pages:/ {print $2}')"
        echo "figures declared: $(python3 -c 'import json,sys; print(len(json.load(open(sys.argv[1]))["figures"]))' "$out/inventory.json")"
        echo "figures marked: $(cut -d' ' -f1 "$out/B/main.ozyfig" | sort -u | grep -c .)"
    } >"$out/SOURCE"
    echo "$name:"; sed 's/^/  /' "$out/SOURCE"
}

fixture proof-broker "$PROOF_BROKER"
fixture simd "$SIMD"
