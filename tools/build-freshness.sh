#!/usr/bin/env bash
# build-freshness.sh — decide when an incremental Hakyll build would be
# WRONG and a full rebuild (`cabal run site -- clean`) is required, and
# record the build-state stamps that decision reads.
#
# Why this exists: `make deploy` used to run `make clean` unconditionally,
# which forced a from-scratch rebuild (and a full rsync re-scan) on every
# deploy. Incremental builds are safe for ordinary content edits — Hakyll
# tracks content, templates, and matched data files — but three things sit
# outside its dependency graph:
#
#   1. The rules themselves. build/**/*.hs and the cabal metadata are not
#      tracked: after editing them, unchanged pages keep their old
#      rendering forever. Any change here forces a clean.
#   2. Deletions/renames. Hakyll never removes outputs for deleted
#      sources, and deploy mirrors _site with rsync --delete — so without
#      a clean, deleted pages would stay live forever. A D/R under
#      content/ static/ data/ archive/ code-refs/ (committed since the last successful
#      build, or sitting uncommitted in the worktree) forces a clean.
#   2b. Route-defining metadata. A route can also disappear without any
#      file being deleted: drop the last `tags: [x]` entry naming a tag and
#      /x/ still sits in _site, where rsync --delete keeps it because it
#      still exists locally. That is a *modification*, invisible to (2).
#      So the fields that generate routes — tags, author(s), collection,
#      series, status — are extracted from the frontmatter of every changed
#      content/**/*.md and data/*.yaml and compared against the version the
#      last build saw. Any change to metadata that previously existed forces
#      a clean; a file that had none before does not, because every route it
#      now generates is new and an incremental build creates those correctly.
#   2c. The draft flag. `draft: true` withholds the page (build/Drafts.hs),
#      and Hakyll never deletes the output of a file it stops seeing, so a
#      changed set of withheld files forces a clean. The generator reads
#      YAML once per scan; snapshot discovery uses the same boundary. The
#      recorded hash describes the last built worktree, including uncommitted
#      or untracked flags, rather than assuming that worktree matched HEAD.
#   3. Compile-time clocks. Stability labels (build/Stability.hs), /now/'s
#      relative timestamp, and archive link-rot annotations are computed
#      via untracked IO and freeze on pages that never recompile. A full
#      rebuild at least every MAX_FULL_REBUILD_AGE_DAYS (default 7)
#      bounds the drift.
#
# Usage (both are wired into the `build` target in the Makefile):
#   build-freshness.sh check   after the content auto-commit, before
#                              `cabal run site -- build`; may run clean
#   build-freshness.sh stamp   immediately after a successful site build
#
# State lives in data/ (gitignored): rules-hash.txt, last-build-commit.txt,
# last-full-rebuild.txt, unpublished-hash.txt, .full-rebuild-pending. Deleting any of them is
# safe — the next build rebuilds from scratch and re-seeds them.
#
# KNOWN LIMITS (deliberate, bounded by the 7-day full rebuild):
#   * Route-defining metadata is only tracked for the fields in
#     ROUTE_FIELDS below, and only in content/**/*.md frontmatter and
#     data/*.yaml. A route generated from some other field, or from a
#     source this scan does not read, is not covered.
#   * archive/manifest.yaml entry removal is not parsed. The documented
#     takedown path (`make archive-gc`) deletes archive/<slug>/, which the
#     D/R check above now sees because archive/ is in its pathspec.
#   * The comparison is over the whole extracted metadata block, not per
#     entry, so it cannot distinguish "added a tag" from "renamed one".
#     Editing an existing `tags:` line therefore cleans. Only a file that
#     carried NO route metadata before is treated as purely additive. That
#     errs toward correctness at the cost of the occasional full rebuild.
#
# STATE_DIR / CACHE_DIR / CLEAN_CMD are overridable for tests only.

set -euo pipefail
# shellcheck source=tools/site-paths.sh
source "$(dirname "$0")/site-paths.sh"
cd "$SITE_ROOT"

STATE_DIR="${STATE_DIR:-data}"
CACHE_DIR="${CACHE_DIR:-_cache}"
CLEAN_CMD="${CLEAN_CMD:-cabal run site -- clean}"
MAX_AGE_DAYS="${MAX_FULL_REBUILD_AGE_DAYS:-7}"

RULES_HASH_FILE="$STATE_DIR/rules-hash.txt"
LAST_BUILD_COMMIT_FILE="$STATE_DIR/last-build-commit.txt"
LAST_FULL_REBUILD_FILE="$STATE_DIR/last-full-rebuild.txt"
PENDING_MARKER="$STATE_DIR/.full-rebuild-pending"
DRAFT_HASH_FILE="$STATE_DIR/unpublished-hash.txt"

# Everything that changes the compiled site generator: the Haskell rules
# and the cabal metadata (dependency-bound moves change behaviour too),
# plus site.yaml: identity and feed settings can change route shape.
# Template contents are absent — Hakyll tracks those itself. The
# engine's files are digested by their path in the engine, so moving them
# under engine/ (A6) changes nothing here.
#
# Also the overrides and template aliases: which file or compiler wins is
# settled as the rules are evaluated (build/EngineFiles.hs), so adding or
# removing one needs a clean build. A moved template also needs its old
# resource identifier recompiled as an alias to acquire the new dependency.
rules_hash() {
    local build cabal
    build=$(engine_path build)
    cabal=$(engine_path levineuwirth.cabal)
    {
        (cd "$build/.." && find build -name '*.hs' -print0 | sort -z | xargs -0 sha256sum)
        sha256sum cabal.project cabal.project.freeze
        (cd "$(dirname "$cabal")" && sha256sum "$(basename "$cabal")")
        sha256sum site.yaml
        file_rules
    } | sha256sum | cut -d' ' -f1
}

# Site overrides and unshadowed engine templates (alias identifiers). The
# engine's tests and demonstration site, build directories and dot
# entries are no site's inputs, and are not looked at.
file_rules() {
    [ -d engine ] || return 0
    (cd engine && find . -mindepth 1 \( -name '.*' -o -name __pycache__ -o -name dist-newstyle \
                                       -o -name fixture-site -o -path ./tests \) -prune \
                    -o \( -type f -o -type l \) -print) |
        sed 's|^\./||' | LC_ALL=C sort |
        while IFS= read -r file; do
            if [ -e "$file" ] || [ -L "$file" ]; then
                printf 'override %s\n' "$file"
            elif [[ "$file" == templates/* ]]; then
                printf 'template-alias %s\n' "$file"
            fi
        done
}

# Paths whose deletion/rename removes a published route. archive/ is here
# because an evicted snapshot leaves archive/<slug>/index.html behind;
# code-refs/ for the same reason (`tools/code-refs.py gc`); the engine's
# static files are served as the site's are.
DIFF_PATHS=(content/ static/ data/ archive/ code-refs/ engine/static/)

# Frontmatter fields that generate routes of their own (tag indexes, author
# pages, collection and series listings, epistemic-status listings).
ROUTE_FIELDS='tags|author|authors|collection|series|status'

# Read a document on stdin; print only its route-defining metadata.
#
# A file beginning with `---` is Markdown with YAML frontmatter, and only
# that block is scanned — so a body line reading "status: shipped" inside a
# code fence can never masquerade as metadata. A file that does not begin
# with `---` (data/*.yaml) is scanned whole. Indented continuation lines
# following a matched key are kept, which is what carries the block-sequence
# form (`tags:` then `  - graph-theory`) as well as the inline
# `tags: [a, b]` form.
route_fields() {
    awk -v fields="^($ROUTE_FIELDS):" '
        NR == 1 {
            if ($0 == "---") { inside = 1; delimited = 1; next }
            inside = 1
        }
        delimited && inside && $0 == "---" { exit }
        inside {
            if ($0 ~ fields)            { cur = 1; print; next }
            if (cur && $0 ~ /^[ \t]/)   { print; next }
            cur = 0
        }
    '
}

# The route_fields digest of a file, either at a git revision or in the
# worktree. Prints the empty-input digest when the file does not exist
# there, which is how an addition is distinguished from a change.
route_fields_hash() {
    local rev="$1" path="$2"
    if [ "$rev" = "WORKTREE" ]; then
        if [ -f "$path" ]; then
            route_fields < "$path" | sha256sum | cut -d' ' -f1
        else
            printf '' | sha256sum | cut -d' ' -f1
        fi
    else
        if git cat-file -e "$rev:$path" 2>/dev/null; then
            git show "$rev:$path" | route_fields | sha256sum | cut -d' ' -f1
        else
            printf '' | sha256sum | cut -d' ' -f1
        fi
    fi
}

# Which changed files are worth the frontmatter comparison: Markdown under
# content/ and YAML directly under data/. Both the committed range (when a
# last-build commit is on record) and the uncommitted worktree diff feed in.
route_field_candidates() {
    local base="$1"
    {
        if [ -n "$base" ]; then
            git diff --name-only -M "$base"..HEAD -- "${DIFF_PATHS[@]}"
        fi
        git diff --name-only HEAD -- "${DIFF_PATHS[@]}"
    } | grep -E '^(content/.*\.md|data/[^/]*\.yaml)$' | sort -u
}

# Any changed file whose previously-recorded route metadata no longer
# matches. Prints one path per offender.
changed_route_fields() {
    local base="$1" empty file old new
    empty=$(printf '' | sha256sum | cut -d' ' -f1)
    while IFS= read -r file; do
        [ -n "$file" ] || continue
        old=$(route_fields_hash "${base:-HEAD}" "$file")
        new=$(route_fields_hash WORKTREE "$file")
        # A file that had no route metadata at all before is purely
        # additive: every route it now generates is new, and an incremental
        # build creates new routes correctly. Anything else may have
        # stranded an old index page.
        if [ "$old" != "$empty" ] && [ "$old" != "$new" ]; then
            printf '%s\n' "$file"
        fi
    done < <(route_field_candidates "$base")
}

check() {
    reasons=()

    if [ ! -f "$RULES_HASH_FILE" ]; then
        reasons+=("no rules-hash on record (first run under freshness tracking)")
    elif [ "$(rules_hash)" != "$(cat "$RULES_HASH_FILE")" ]; then
        reasons+=("build rules changed (build/**/*.hs, cabal metadata, site.yaml, overrides or template aliases)")
    fi

    base=""
    if [ -f "$LAST_BUILD_COMMIT_FILE" ]; then
        last_commit=$(cat "$LAST_BUILD_COMMIT_FILE")
        if git cat-file -e "$last_commit^{commit}" 2>/dev/null; then
            base="$last_commit"
            if [ -n "$(git diff --name-status -M --diff-filter=DR "$last_commit"..HEAD -- "${DIFF_PATHS[@]}")" ]; then
                reasons+=("content deleted/renamed since last build (${last_commit:0:12})")
            fi
        else
            reasons+=("last-build commit $last_commit unknown (rewritten history?)")
        fi
    fi

    # Uncommitted deletions (e.g. a static/ file removed but not yet
    # committed — the auto-commit only covers content/). This re-fires on
    # every build until the deletion is committed; that is deliberate.
    if [ -n "$(git diff --name-status --diff-filter=DR HEAD -- "${DIFF_PATHS[@]}")" ]; then
        reasons+=("uncommitted deletions in the worktree")
    fi

    # Route-defining metadata changed inside a file that still exists: a
    # removed tag / author / collection / series / status leaves its old
    # generated index in _site, which rsync --delete then preserves.
    changed_meta=$(changed_route_fields "$base")
    if [ -n "$changed_meta" ]; then
        first=$(printf '%s\n' "$changed_meta" | head -3 | tr '\n' ' ')
        count=$(printf '%s\n' "$changed_meta" | grep -c .)
        reasons+=("route metadata changed in $count file(s): ${first%% }")
    fi

    # Compare the actual last-built boundary, including untracked flags
    # and photo stems. A git revision cannot represent a SKIP_SNAPSHOT build.
    draft_hash=$(python3 "$(engine_path tools/unpublished.py)" hash)
    if [ ! -f "$DRAFT_HASH_FILE" ] || [ "$draft_hash" != "$(cat "$DRAFT_HASH_FILE")" ]; then
        reasons+=("draft flag changed (withheld pages or assets differ from last build)")
    fi

    now=$(date +%s)
    last_full=$(cat "$LAST_FULL_REBUILD_FILE" 2>/dev/null || echo 0)
    case "$last_full" in ''|*[!0-9]*) last_full=0 ;; esac
    if [ "$last_full" -eq 0 ]; then
        reasons+=("no full rebuild on record")
    elif [ $(( now - last_full )) -gt $(( MAX_AGE_DAYS * 86400 )) ]; then
        reasons+=("last full rebuild older than $MAX_AGE_DAYS days (compile-time clocks drift)")
    fi

    if [ "${#reasons[@]}" -gt 0 ]; then
        echo "build-freshness: full rebuild required:" >&2
        printf '  - %s\n' "${reasons[@]}" >&2
        $CLEAN_CMD
    else
        echo "build-freshness: incremental build OK" >&2
    fi

    # A missing cache dir means the coming build is a full rebuild —
    # whether we just cleaned or the user ran `make clean` themselves.
    # Remember that so `stamp` records the full-rebuild time only once
    # the build actually succeeds.
    if [ ! -d "$CACHE_DIR" ]; then
        mkdir -p "$STATE_DIR"
        touch "$PENDING_MARKER"
    fi
}

stamp() {
    mkdir -p "$STATE_DIR"
    python3 "$(engine_path tools/unpublished.py)" hash > "$DRAFT_HASH_FILE.tmp"
    mv "$DRAFT_HASH_FILE.tmp" "$DRAFT_HASH_FILE"
    rules_hash > "$RULES_HASH_FILE"
    git rev-parse HEAD > "$LAST_BUILD_COMMIT_FILE"
    if [ -f "$PENDING_MARKER" ]; then
        date +%s > "$LAST_FULL_REBUILD_FILE"
        rm -f "$PENDING_MARKER"
    fi
}

case "${1:-}" in
    check) check ;;
    stamp) stamp ;;
    *) echo "usage: $0 {check|stamp}" >&2; exit 2 ;;
esac
