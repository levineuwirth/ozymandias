#!/usr/bin/env bash
# refreeze.sh — Regenerate cabal.project.freeze after a pacman -Syu updates
# Haskell libraries. Run from the site's root, whose cabal.project it is.
set -euo pipefail

# shellcheck source=tools/site-paths.sh
source "$(dirname "$0")/site-paths.sh"
FREEZE="$SITE_ROOT/cabal.project.freeze"

cd "$SITE_ROOT"

# Back up the current freeze and restore it if resolution fails, so an
# unsolvable index never leaves the repo with no freeze file at all
# (recoverable via git, but the script shouldn't depend on that).
BACKUP=""
if [ -f "$FREEZE" ]; then
    BACKUP="$(mktemp "$FREEZE.bak.XXXXXX")"
    cp "$FREEZE" "$BACKUP"
fi
restore_on_failure() {
    if [ -n "$BACKUP" ]; then
        echo "==> Refreeze failed — restoring previous freeze file." >&2
        mv "$BACKUP" "$FREEZE"
    fi
}
trap restore_on_failure ERR

echo "==> Removing stale freeze file..."
rm -f "$FREEZE"

echo "==> Resolving dependencies and writing new freeze file..."
cabal freeze
trap - ERR
[ -n "$BACKUP" ] && rm -f "$BACKUP"

echo "==> Verifying build..."
cabal build exe:site

echo ""
echo "Done. cabal.project.freeze updated."
