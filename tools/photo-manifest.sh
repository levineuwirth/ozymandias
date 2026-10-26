#!/usr/bin/env bash
#
# photo-manifest.sh — draft a manifest for tools/import-photos.sh.
#
# The batch importer takes a tab-separated manifest, which is the right input
# for photographs gathered over years from scattered directories — but only if
# something else writes the first draft. This does that: it finds the images,
# reads every capture date in ONE exiftool call, orders them chronologically,
# and emits a line per photograph with a date-stamped slug and an empty title.
#
# The output is meant to be edited: delete the lines you don't want, fill in
# the titles you do. Nothing is imported and nothing is touched — it only
# reads, and writes to stdout.
#
# Usage:
#   tools/photo-manifest.sh <dir> --prefix undergrad > /tmp/undergrad.tsv
#   $EDITOR /tmp/undergrad.tsv
#   tools/import-photos.sh --manifest /tmp/undergrad.tsv --series undergrad
#
# Options:
#   --prefix S     slug prefix (required): S-YYYYMMDD-NN
#   --recursive    descend into subdirectories
#
# Slugs are date-stamped rather than sequential because they become URLs and
# ought to survive the set changing: deleting a line from the middle of the
# manifest must not renumber everything after it. A photograph keeps its slug
# no matter what else you decide to keep.
set -uo pipefail

DIR=""; PREFIX=""; RECURSIVE=false
while [ "$#" -gt 0 ]; do
    case "$1" in
        --prefix)    PREFIX="$2"; shift 2 ;;
        --recursive) RECURSIVE=true; shift ;;
        -h|--help)   sed -n '2,30p' "$0"; exit 0 ;;
        -*)          echo "unknown option: $1" >&2; exit 2 ;;
        *)           DIR="$1"; shift ;;
    esac
done

[ -n "$DIR" ]    || { echo "photo-manifest: need a directory" >&2; exit 2; }
[ -d "$DIR" ]    || { echo "photo-manifest: no such directory: $DIR" >&2; exit 2; }
[ -n "$PREFIX" ] || { echo "photo-manifest: --prefix is required" >&2; exit 2; }
[[ "$PREFIX" =~ ^[a-z0-9-]+$ ]] || { echo "photo-manifest: invalid prefix '$PREFIX'" >&2; exit 2; }
command -v exiftool >/dev/null 2>&1 || { echo "photo-manifest: exiftool required" >&2; exit 1; }

depth=(); $RECURSIVE || depth=(-maxdepth 1)
mapfile -d '' -t FILES < <(
    find "$DIR" "${depth[@]}" -type f \
        \( -iname '*.jpg' -o -iname '*.jpeg' -o -iname '*.png' -o -iname '*.heic' \) \
        -print0 | sort -z
)

if [ "${#FILES[@]}" -eq 0 ]; then
    echo "photo-manifest: no images found in $DIR" >&2
    exit 1
fi

# One exiftool call for the whole directory. Per-file it would be ~0.3 s each,
# which is the entire reason import-photos.sh exists.
exiftool -q -json -DateTimeOriginal -CreateDate -FileModifyDate "${FILES[@]}" \
  | PREFIX="$PREFIX" python3 -c '
import json, os, sys, collections

rows = json.load(sys.stdin)
prefix = os.environ["PREFIX"]

def when(r):
    # DateTimeOriginal is the shutter. CreateDate is a decent second. File
    # mtime is a last resort and often lies — a copied file carries the date
    # of the copy — but an approximate order beats no order.
    for key in ("DateTimeOriginal", "CreateDate", "FileModifyDate"):
        v = r.get(key)
        if isinstance(v, str) and len(v) >= 10 and v[:4].isdigit():
            return v[:10].replace(":", "")
    return "00000000"

rows.sort(key=lambda r: (when(r), r.get("SourceFile", "")))

seq = collections.Counter()
print("# path\tslug\ttitle   — delete unwanted lines, fill in titles")
print(f"# {len(rows)} photographs, in capture order")
for r in rows:
    src = r.get("SourceFile")
    if not src:
        continue
    day = when(r)
    seq[day] += 1
    print(f"{src}\t{prefix}-{day}-{seq[day]:02d}\t")
'
