#!/usr/bin/env bash
# refreeze.sh — Regenerate the engine's cabal.project.freeze after a pacman
# -Syu updates Haskell libraries. Run from a site's root: its cabal.project
# builds the engine, and its cabal.project.freeze is a link to the engine's,
# so every site that builds one engine commit builds it with the same
# libraries. (A link, not cabal.project's `import:`: cabal 3.16 caches an
# imported file and misses its changes, but watches the freeze beside it.)
set -euo pipefail

# shellcheck source=tools/site-paths.sh
source "$(dirname "$0")/site-paths.sh"
# The engine as the site sees it, where the site's link points.
FREEZE="$SITE_ROOT/engine/cabal.project.freeze"

cd "$SITE_ROOT"

# Back up the current freeze and restore it if resolution fails, so an
# unsolvable index never leaves the engine with no freeze file at all
# (recoverable via git, but the script shouldn't depend on that).
BACKUP=""
if [ -f "$FREEZE" ]; then
    BACKUP="$(mktemp "$FREEZE.bak.XXXXXX")"
    cp "$FREEZE" "$BACKUP"
fi
# A copy of the site's project, which reads no freeze of its own, so the
# resolution is a fresh one; cabal freeze writes the project file's name
# plus .freeze beside it.
PROJECT="$(mktemp cabal.project.refreeze.XXXXXX)"
cleanup() { rm -f "$PROJECT" "$PROJECT.freeze"; }
restore_on_failure() {
    if [ -n "$BACKUP" ]; then
        echo "==> Refreeze failed — restoring previous freeze file." >&2
        mv "$BACKUP" "$FREEZE"
    fi
}
trap cleanup EXIT
trap restore_on_failure ERR
cp cabal.project "$PROJECT"

echo "==> Removing stale freeze file..."
rm -f "$FREEZE"

echo "==> Resolving dependencies and writing new freeze file..."
cabal freeze --project-file="$PROJECT"
mv "$PROJECT.freeze" "$FREEZE"
trap - ERR
[ -n "$BACKUP" ] && rm -f "$BACKUP"

echo "==> Verifying build..."
cabal build exe:site

echo ""
echo "Done. $FREEZE updated."
