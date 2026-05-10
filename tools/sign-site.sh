#!/usr/bin/env bash
# sign-site.sh — Detach-sign every HTML file in _site/ with the signing subkey.
#
# Requires the passphrase to be pre-cached via tools/preset-signing-passphrase.sh.
# Produces <file>.html.sig alongside each <file>.html.
#
# Usage (called by `make sign`):
#   ./tools/sign-site.sh

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SITE_YAML="$REPO_ROOT/site.yaml"

GNUPGHOME="${GNUPGHOME:-$HOME/.gnupg-signing}"
SITE_DIR="${1:-_site}"

# Read the master signing-key fingerprint from site.yaml's `gpg-fingerprint`
# field. Single source of truth — same value the templates render in the
# footer's "sig" tooltip. Empty fingerprint means signing isn't configured;
# bail with a helpful message rather than calling gpg with an empty key.
if [ ! -f "$SITE_YAML" ]; then
    echo "Error: $SITE_YAML not found." >&2
    exit 1
fi

SIGNING_KEY="$(awk -F: '
    /^[[:space:]]*gpg-fingerprint[[:space:]]*:/ {
        sub(/^[^:]*:[[:space:]]*/, "")
        gsub(/^["'"'"' ]+|["'"'"' ]+$/, "")
        print
        exit
    }
' "$SITE_YAML")"

if [ -z "$SIGNING_KEY" ]; then
    echo "Error: gpg-fingerprint is empty in $SITE_YAML." >&2
    echo "  Set it to your master signing-key fingerprint, then retry 'make sign'." >&2
    echo "  (Leave it empty to skip signing entirely — and remove 'sign' from your" >&2
    echo "   deploy pipeline accordingly.)" >&2
    exit 1
fi

if [ ! -d "$SITE_DIR" ]; then
    echo "Error: site directory '$SITE_DIR' not found. Run 'make build' first." >&2
    exit 1
fi

# Pre-flight: verify the signing key is available and the passphrase is cached
# by signing /dev/null. If this fails (e.g. passphrase not preset, key missing),
# abort before touching any .sig files.
echo "sign-site: pre-flight check..." >&2
if ! GNUPGHOME="$GNUPGHOME" gpg \
        --homedir "$GNUPGHOME" \
        --batch \
        --yes \
        --detach-sign \
        --armor \
        --local-user "$SIGNING_KEY" \
        --output /dev/null \
        /dev/null 2>/dev/null; then
    echo "" >&2
    echo "ERROR: GPG signing pre-flight failed." >&2
    echo "  The signing key passphrase is probably not cached." >&2
    echo "  Run: ./tools/preset-signing-passphrase.sh" >&2
    echo "  Then retry: make sign  (or  make deploy)" >&2
    exit 1
fi
echo "sign-site: pre-flight OK — signing $SITE_DIR..." >&2

find "$SITE_DIR" -name "*.html" -print0 | xargs -0 -I {} -P $(nproc) \
    gpg --homedir "$GNUPGHOME" \
        --batch \
        --yes \
        --detach-sign \
        --armor \
        --local-user "$SIGNING_KEY" \
        --output "{}.sig" \
        "{}"

count=$(find "$SITE_DIR" -name "*.html" -printf '.' | wc -c)
echo "Signed $count HTML files in $SITE_DIR."
