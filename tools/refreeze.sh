#!/usr/bin/env bash
# refreeze.sh — Regenerate cabal.project.freeze after a pacman -Syu updates
# Haskell libraries. Run from anywhere inside the repo.
set -euo pipefail

REPO_ROOT="$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
FREEZE="$REPO_ROOT/cabal.project.freeze"

cd "$REPO_ROOT"

echo "==> Removing stale freeze file..."
rm -f "$FREEZE"

echo "==> Resolving dependencies and writing new freeze file..."
cabal freeze

echo "==> Verifying build..."
cabal build

echo ""
echo "Done. cabal.project.freeze updated."
