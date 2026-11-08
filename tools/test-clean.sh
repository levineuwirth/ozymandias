#!/usr/bin/env bash
# test-clean.sh — run the test suite on HEAD in a fresh clone.
#
#   make test-clean           KEEP=1 keeps the clone afterwards
#
# What a commit needs but only this working tree supplies passes `make test`
# here and fails in any other checkout: an untracked fixture, a gitignored
# file a test reads, a variable the shell happens to export. October 2026
# had both kinds: the golden image's dimension sidecar was never committed
# (f3018fb), and deploy variables leaked into the tests (40190c8).
#
# So the suite runs on HEAD as committed (uncommitted changes are not
# tested), in a scratch clone, under a minimal environment: HOME, PATH, the
# locale, TMPDIR and the toolchain's locations, nothing else. The generator
# is compiled from scratch in the clone against the shared cabal store, so
# a module missing from the cabal file fails here too. This checkout's
# .venv is borrowed (the Python environment is not what this checks), and
# tests that need a built _site skip, as they would in any fresh checkout.
#
# The engine is cloned too once it is a repository of its own, the site's
# engine a link to it (A10b): both at HEAD as committed, side by side, so
# that the clone's Makefile links the engine's clone as ../ozymandias.
# Until then it is the site's engine/ and comes with the site.
set -euo pipefail

repo=$(git rev-parse --show-toplevel)
engine=$(git -C "$repo/engine" rev-parse --show-toplevel 2>/dev/null || true)
if [ -n "$engine" ] && [ "$(cd "$engine" && pwd -P)" = "$(cd "$repo" && pwd -P)" ]; then
    engine=""
fi
work=$(mktemp -d "${TMPDIR:-/tmp}/test-clean.XXXXXX")
status=1
cleanup() {
    if [ "$status" -ne 0 ] || [ "${KEEP:-0}" = 1 ]; then
        echo "test-clean: the clones are kept in $work"
    else
        rm -rf "$work"
    fi
}
trap cleanup EXIT

git clone --quiet "$repo" "$work/repo"
if [ -d "$repo/.venv" ]; then ln -s "$repo/.venv" "$work/repo/.venv"; fi
echo "test-clean: $(git -C "$work/repo" log -1 --format='%h %s')"
if [ -n "$engine" ]; then
    git clone --quiet "$engine" "$work/ozymandias"
    echo "test-clean: engine $(git -C "$work/ozymandias" log -1 --format='%h %s')"
else
    echo "test-clean: engine in this repository"
fi

keep=(HOME PATH LANG LC_ALL TMPDIR CABAL_DIR GHCUP_INSTALL_BASE_PREFIX
      XDG_CONFIG_HOME XDG_DATA_HOME XDG_STATE_HOME XDG_CACHE_HOME)
clean_env=()
for var in "${keep[@]}"; do
    if [ -n "${!var:-}" ]; then clean_env+=("$var=${!var}"); fi
done

env -i "${clean_env[@]}" make --no-print-directory -C "$work/repo" test

# Then the GitHub workflow's selection, as it runs there: no built site and no
# Haskell toolchain, where ci.py fails on any skip. A test that needs a build
# passes everywhere else and is only caught here (c9bd151).
if [ -x "$work/repo/.venv/bin/python" ]; then
    ci_path=$(printf '%s' "$PATH" | tr ':' '\n' | grep -v -e ghcup -e cabal | paste -sd: -)
    (cd "$work/repo" && env -i "${clean_env[@]}" PATH="$ci_path" .venv/bin/python engine/tests/ci.py)
else
    echo "test-clean: no .venv to borrow, so the CI selection did not run" >&2
fi
status=0
