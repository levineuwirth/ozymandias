#!/usr/bin/env bash
#
# import-photos.sh — bulk import, for when import-photo.sh's per-file cost
# stops being noise.
#
# The single-file tool spends roughly 0.9 s per photograph, and most of that
# is process startup rather than work: exiftool is Perl and costs ~0.3 s to
# launch, and it is launched twice per image. Twenty photographs cost 1.47 s
# as twenty exiftool calls and 0.12 s as one. This script therefore does the
# same work in a different shape — every stage that can see the whole batch
# at once does:
#
#   resize      N parallel ImageMagick calls   (irreducible; genuine work)
#   extract     1 call                          (was N)
#   strip       1 call                          (was N)
#   palette     1 call                          (was N)
#   scaffold    1 call                          (was N)
#
# so a batch costs N + 4 processes instead of ~6N.
#
# Input is a manifest rather than flags, because photographs gathered over
# years live in scattered directories under names like IMG_4421.JPG that say
# nothing. One line per photograph, tab-separated:
#
#   /path/to/original.jpg <TAB> slug <TAB> Title
#
# Title is optional. Blank lines and lines beginning with # are ignored.
#
# Usage:
#   tools/import-photos.sh --manifest FILE [--series SLUG] [--tags a,b]
#                          [--location "Copenhagen, Denmark"] [--jobs N]
#                          [--execute]
#
# Dry run by default: it validates everything and prints the plan without
# touching the repository. Nothing is written until --execute.
#
# Deliberately NOT handled: geo. Coordinates stay a per-photograph decision,
# because a batch flag is exactly how a home address ends up on a public map.
set -uo pipefail

# shellcheck source=tools/site-paths.sh
source "$(dirname "$0")/site-paths.sh"
MANIFEST=""; SERIES=""; TAGS=""; LOCATION=""; JOBS=4; EXECUTE=false

while [ "$#" -gt 0 ]; do
    case "$1" in
        --manifest) MANIFEST="$2"; shift 2 ;;
        --series)   SERIES="$2";   shift 2 ;;
        --tags)     TAGS="$2";     shift 2 ;;
        --location) LOCATION="$2"; shift 2 ;;
        --jobs)     JOBS="$2";     shift 2 ;;
        --execute)  EXECUTE=true;  shift ;;
        -h|--help)  sed -n '2,40p' "$0"; exit 0 ;;
        *) echo "unknown argument: $1" >&2; exit 2 ;;
    esac
done

[ -n "$MANIFEST" ] || { echo "import-photos: --manifest is required" >&2; exit 2; }
[ -f "$MANIFEST" ] || { echo "import-photos: no such manifest: $MANIFEST" >&2; exit 2; }
if [ -n "$SERIES" ] && [[ ! "$SERIES" =~ ^[a-z0-9-]+$ ]]; then
    echo "import-photos: invalid series '$SERIES'" >&2; exit 2
fi
for tool in magick exiftool; do
    command -v "$tool" >/dev/null 2>&1 || { echo "import-photos: $tool is required" >&2; exit 1; }
done
[ -x "$SITE_ROOT/.venv/bin/python" ] || { echo "import-photos: .venv missing — run make venv" >&2; exit 1; }

# ---------------------------------------------------------------------------
# Namespace guard.
#
# Series and tags share one URL space: a series lives at /photography/<slug>/
# and a tag index at /photography/<tag>/. Tagging Danish frames `denmark` while
# a `denmark` series exists makes both claim the same route, and the build
# refuses to run (build/RouteCheck.hs) — which names the conflict, but only
# after the photos are copied and the entries written.
#
# Cheaper to refuse here, where the offending tag is still on the command line.
# ---------------------------------------------------------------------------
# The check is scaffold-photos.py --check, which judges the tags as they
# will be written; a check here in the shell split "street art" into two
# words that were never the tag written ("streetart"). It runs once the
# manifest is read, since a photograph's slug can collide too.

# ---------------------------------------------------------------------------
# Parse and validate the whole manifest before writing anything. A batch that
# fails halfway leaves a half-imported series that has to be unpicked by hand.
# ---------------------------------------------------------------------------
# Empty, not merely declared: under `set -u` a declared but unset array has
# no length, and a manifest whose every line was refused crashed on it.
SRCS=() SLUGS=() TITLES=() TARGETS=() MDS=()
errors=0
seen=""

while IFS=$'\t' read -r src slug title || [ -n "${src:-}" ]; do
    case "${src:-}" in ''|'#'*) continue ;; esac
    src="${src%$'\r'}"; slug="${slug:-}"; title="${title:-}"

    if [ -z "$slug" ]; then
        echo "  no slug for: $src" >&2; errors=$((errors+1)); continue
    fi
    if [[ ! "$slug" =~ ^[a-z0-9-]+$ ]]; then
        echo "  invalid slug '$slug' (lowercase a-z, 0-9, hyphens)" >&2; errors=$((errors+1)); continue
    fi
    if [ ! -f "$src" ]; then
        echo "  missing original: $src" >&2; errors=$((errors+1)); continue
    fi
    case " $seen " in *" $slug "*) echo "  duplicate slug in manifest: $slug" >&2; errors=$((errors+1)); continue ;; esac
    seen="$seen $slug"

    if [ -n "$SERIES" ]; then
        dir="$SITE_ROOT/content/photography/$SERIES"
        target="$dir/$slug.jpg"; md="$dir/$slug.md"
    else
        dir="$SITE_ROOT/content/photography/$slug"
        target="$dir/photo.jpg"; md="$dir/index.md"
    fi
    if [ -e "$md" ] || [ -e "$target" ]; then
        echo "  already exists: $slug" >&2; errors=$((errors+1)); continue
    fi

    SRCS+=("$src"); SLUGS+=("$slug"); TITLES+=("$title")
    TARGETS+=("$target"); MDS+=("$md")
done < "$MANIFEST"

n=${#SRCS[@]}
if [ "$errors" -gt 0 ]; then
    echo "import-photos: $errors problem(s) in the manifest — nothing imported." >&2
    exit 1
fi
[ "$n" -gt 0 ] || { echo "import-photos: manifest is empty" >&2; exit 1; }
SERIES="$SERIES" TAGS="$TAGS" SLUGS="${SLUGS[*]}" \
    "$SITE_ROOT/.venv/bin/python" "$(engine_path tools/scaffold-photos.py)" --check || exit 2

echo "import-photos: $n photograph(s)${SERIES:+ into series '$SERIES'}"
if ! $EXECUTE; then
    echo "### DRY RUN — nothing written. Pass --execute to import. ###"
    for ((i = 0; i < n && i < 5; i++)); do
        printf '  %s  ->  %s\n' "$(basename "${SRCS[$i]}")" "${MDS[$i]#$SITE_ROOT/}"
    done
    [ "$n" -gt 5 ] && echo "  … and $((n - 5)) more"
    exit 0
fi

mkdir -p "$(dirname "${TARGETS[0]}")"

# The resized copies keep the camera's EXIF (GPS, serial numbers) until step 3
# strips it, and content/ is published whether or not git tracks a file. Any
# exit before the strip succeeds — a failed resize, a failed extraction,
# Ctrl-C — removes every copy (audit T03).
trap 'rm -f -- "${TARGETS[@]}"' EXIT INT TERM

# ---------------------------------------------------------------------------
# 1. Resize, in parallel. The only stage that is real per-image work.
# ---------------------------------------------------------------------------
# vipsthumbnail when it is available, ImageMagick otherwise. libvips asks the
# JPEG decoder for a reduced-scale read instead of decoding in full and then
# resampling. The win is real but modest at these sizes — 0.181 s against
# 0.234 s — because 4608px to 2400px is under a factor of two, and
# shrink-on-load works in halves; it would be dramatic for thumbnails.
# Verified equivalent on output that matters: same dimensions, same sRGB, EXIF
# intact for the extraction step that follows, and about 12% smaller files.
if command -v vipsthumbnail >/dev/null 2>&1; then
    RESIZER=vips
    echo "  resizing ($JOBS parallel, libvips)…"
else
    RESIZER=magick
    echo "  resizing ($JOBS parallel, ImageMagick)…"
fi

for ((i = 0; i < n; i++)); do
    printf '%s\0%s\0' "${SRCS[$i]}" "${TARGETS[$i]}"
done | RESIZER="$RESIZER" xargs -0 -n 2 -P "$JOBS" bash -c '
    if [ "$RESIZER" = vips ]; then
        # vipsthumbnail honours the orientation tag by default, matching
        # ImageMagick -auto-orient. Its module warnings on stderr concern
        # openslide and poppler, neither of which has anything to do with
        # JPEG, so they are dropped rather than alarming anyone.
        # ">": shrink only, as the magick branch below and import-photo.sh
        # do. Without it vipsthumbnail also enlarges a smaller original.
        vipsthumbnail "$0" --size "2400x2400>" -o "$1[Q=85]" 2>/dev/null \
            || { echo "  resize FAILED: $0" >&2; exit 0; }
    else
        magick "$0" -auto-orient -resize "2400x2400>" -colorspace sRGB -quality 85 "$1" \
            || { echo "  resize FAILED: $0" >&2; exit 0; }
    fi
    chmod 644 "$1"
'
missing=0
for t in "${TARGETS[@]}"; do [ -f "$t" ] || missing=$((missing+1)); done
if [ "$missing" -gt 0 ]; then
    echo "import-photos: $missing resize(s) failed — aborting before metadata." >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# 2. EXIF, one call. Reads the resized copies, which still carry their tags.
# ---------------------------------------------------------------------------
echo "  extracting EXIF (1 call)…"
( cd "$SITE_ROOT" && .venv/bin/python "$(engine_path tools/extract-exif.py)" "${TARGETS[@]}" ) >/dev/null || true

# ---------------------------------------------------------------------------
# 3. Strip, one call. exiftool rewrites the container without re-encoding, so
#    the delivered photograph is not put through a second lossy generation
#    merely to remove its metadata.
# ---------------------------------------------------------------------------
echo "  stripping EXIF (1 call)…"
if ! exiftool -q -all= --icc_profile:all -overwrite_original "${TARGETS[@]}"; then
    echo "import-photos: strip failed — removing delivery files so no EXIF-laden" >&2
    echo "  JPEG can be committed by the auto-snapshot in \`make build\`." >&2
    rm -f -- "${TARGETS[@]}"
    exit 1
fi
trap - EXIT INT TERM

# Stripping rewrote each file, making it newer than the sidecar extracted from
# it; extract-exif.py would then treat those sidecars as stale and overwrite
# real metadata with width and height alone on the next build.
for t in "${TARGETS[@]}"; do touch "$t.exif.yaml" 2>/dev/null || true; done

# ---------------------------------------------------------------------------
# 4. Palette, one call.
# ---------------------------------------------------------------------------
# Palette extraction is the one metadata stage that is genuinely CPU-bound —
# k-means over every pixel, ~0.27 s per photograph — so batching it into a
# single process saves nothing. It is split across $JOBS processes instead.
# Interpreter startup is ~0.02 s, so a handful of extra ones costs nothing
# against the parallelism they buy.
echo "  extracting palettes ($JOBS parallel)…"
pal_pids=()
for ((j = 0; j < JOBS; j++)); do
    chunk=()
    for ((k = j; k < n; k += JOBS)); do chunk+=("${TARGETS[$k]}"); done
    [ ${#chunk[@]} -eq 0 ] && continue
    ( cd "$SITE_ROOT" && .venv/bin/python "$(engine_path tools/extract-palette.py)" "${chunk[@]}" ) >/dev/null 2>&1 &
    pal_pids+=("$!")
done
for pid in "${pal_pids[@]}"; do wait "$pid" || true; done

# ---------------------------------------------------------------------------
# 5. Scaffold every entry, and the series landing if this is the first
#    import into one, in one Python process.
# ---------------------------------------------------------------------------
echo "  writing entries (1 call)…"
plan="$(mktemp)"; trap 'rm -f "$plan"' EXIT
for ((i = 0; i < n; i++)); do
    printf '%s\t%s\t%s\t%s\n' "${MDS[$i]}" "${TARGETS[$i]}" "${SLUGS[$i]}" "${TITLES[$i]}"
done > "$plan"

SERIES="$SERIES" TAGS="$TAGS" LOCATION="$LOCATION" \
    "$SITE_ROOT/.venv/bin/python" "$(engine_path tools/scaffold-photos.py)" "$plan" || exit 1

echo "import-photos: $n imported."
echo "Next: titles and captions are placeholders — edit, then 'make dev'."
