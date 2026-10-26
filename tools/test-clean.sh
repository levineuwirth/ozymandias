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
set -euo pipefail

repo=$(git rev-parse --show-toplevel)
work=$(mktemp -d "${TMPDIR:-/tmp}/test-clean.XXXXXX")
status=1
cleanup() {
    if [ "$status" -ne 0 ] || [ "${KEEP:-0}" = 1 ]; then
        echo "test-clean: the clone is kept at $work/repo"
    else
        rm -rf "$work"
    fi
}
trap cleanup EXIT

git clone --quiet "$repo" "$work/repo"
if [ -d "$repo/.venv" ]; then ln -s "$repo/.venv" "$work/repo/.venv"; fi
echo "test-clean: $(git -C "$work/repo" log -1 --format='%h %s')"

keep=(HOME PATH LANG LC_ALL TMPDIR CABAL_DIR GHCUP_INSTALL_BASE_PREFIX
      XDG_CONFIG_HOME XDG_DATA_HOME XDG_STATE_HOME XDG_CACHE_HOME)
clean_env=()
for var in "${keep[@]}"; do
    if [ -n "${!var:-}" ]; then clean_env+=("$var=${!var}"); fi
done

env -i "${clean_env[@]}" make --no-print-directory -C "$work/repo" test
status=0
