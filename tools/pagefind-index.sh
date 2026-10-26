#!/usr/bin/env bash
# pagefind-index.sh — build the keyword search index into <site>/pagefind,
# replacing only the files whose content changed.
#
#   tools/pagefind-index.sh [site-dir]     (Stage 4 of `make build`)
#
# Pagefind never clears its output directory, so the build used to delete
# it and index afresh. Its output is deterministic, but every file then got
# a new mtime: each deploy re-sent all ~450 index files and their sidecars,
# and readers re-downloaded unchanged ones, since nginx's ETag is the mtime.
# The index is written to a scratch directory instead and copied over by
# content (-c): an unchanged file keeps its mtime, a changed one is
# replaced, and one Pagefind no longer writes is deleted, as the old
# fragments of a removed page must be. The .gz/.br sidecars are
# compress-assets.sh's (it refreshes and sweeps them), so they are left out.
set -euo pipefail

site=${1:-_site}
[ -d "$site" ] || { echo "pagefind-index: $site not found" >&2; exit 1; }

staging=$(mktemp -d "${TMPDIR:-/tmp}/pagefind-index.XXXXXX")
trap 'rm -rf "$staging"' EXIT

pagefind --site "$site" --output-path "$staging/pagefind"
rsync -rc --delete --exclude='*.gz' --exclude='*.br' "$staging/pagefind/" "$site/pagefind/"
