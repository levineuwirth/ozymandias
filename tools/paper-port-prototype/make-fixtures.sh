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
#
# Before anything is deleted, every output directory is checked against
# every source tree: neither may contain the other. An existing output is
# replaced only if it holds this recipe's marker file; anything else there
# is refused and left alone. A fixture is complete only when both builds
# exited 0 and its marks name the figures its source declares.
set -euo pipefail
HERE=$(dirname "$(readlink -f "$0")")
DEST=$(realpath -m -- "${1:-$HERE/fixtures}")
MARKER=.ozyport-fixture
MARKER_TEXT="made by tools/paper-port-prototype/make-fixtures.sh"
LATEXMK=(latexmk -pdf -interaction=nonstopmode -halt-on-error -jobname=main)
NAMES=(proof-broker simd)
declare -A SRC=(
    [proof-broker]=${PROOF_BROKER:-$HOME/Repos/research/proof-broker/paper}
    [simd]=${SIMD:-$HOME/Repos/research/where-simd-helps/paper}
)

die() { echo "make-fixtures: $*" >&2; exit 1; }
# within A B: A is B or lies under it (both resolved, symlinks followed).
within() { [[ "$1/" == "$2/"* ]]; }

# 1. Every check, before anything is touched.
for name in "${NAMES[@]}"; do
    [ -f "${SRC[$name]}/main.tex" ] || die "$name: no main.tex in ${SRC[$name]}"
    SRC[$name]=$(realpath -e -- "${SRC[$name]}")
done
for name in "${NAMES[@]}"; do
    out="$DEST/$name"
    for other in "${NAMES[@]}"; do
        src=${SRC[$other]}
        within "$out" "$src" && die "the output $out lies inside the source $src"
        within "$src" "$out" && die "the output $out contains the source $src"
    done
    within "$HERE" "$out" && die "the output $out contains this recipe"
    if [ -e "$out" ] || [ -L "$out" ]; then
        [ -d "$out" ] && [ ! -L "$out" ] && [ "$(cat "$out/$MARKER" 2>/dev/null)" = "$MARKER_TEXT" ] \
            || die "$out exists and is not a fixture this recipe made; move it away first"
    fi
done

# 2. Build.
fixture() {
    local name=$1 src=${SRC[$1]} out=$DEST/$1
    rm -rf -- "$out"
    mkdir -p -- "$out"
    echo "$MARKER_TEXT" >"$out/$MARKER"
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
    local plain=$!
    (cd "$out/B" && TEXINPUTS="$HERE:" nice "${LATEXMK[@]}" -usepretex='\RequirePackage{ozyport}' main.tex >build.out 2>&1) &
    local marked=$!
    local a=0 b=0
    wait "$plain" || a=$?
    wait "$marked" || b=$?
    [ "$a" = 0 ] || die "$name: the plain build exited $a; see $out/A/build.out"
    [ "$b" = 0 ] || die "$name: the marked build exited $b; see $out/B/build.out"
    for v in A B; do
        [ -f "$out/$v/main.pdf" ] || die "$name: the $v build wrote no PDF; see $out/$v/build.out"
    done
    [ -f "$out/B/main.ozyfig" ] || die "$name: the marked build wrote no marks"
    python3 -I "$HERE/inventory.py" "$out/src/main.tex" >"$out/inventory.json"
    local declared marked_n
    declared=$(python3 -c 'import json,sys; print(len(json.load(open(sys.argv[1]))["figures"]))' "$out/inventory.json")
    marked_n=$(cut -d' ' -f1 "$out/B/main.ozyfig" | sort -u | grep -c . || true)
    [ "$declared" = "$marked_n" ] || die "$name: the source declares $declared figures, the build marked $marked_n"
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
        echo "figures declared: $declared"
        echo "figures marked: $marked_n"
    } >"$out/SOURCE"
    echo "$name:"; sed 's/^/  /' "$out/SOURCE"
}

for name in "${NAMES[@]}"; do
    fixture "$name"
done
