#!/usr/bin/env bash
# preset-signing-passphrase.sh — Cache the signing subkey passphrase in the
# dedicated signing agent so that automated builds can sign without a prompt.
#
# Run this ONCE in an interactive terminal after system boot (or after the
# agent cache expires).  The passphrase is cached for 24 h (see gpg-agent.conf).
#
# Usage:
#   engine/tools/preset-signing-passphrase.sh
#
# The script will prompt for the passphrase via the terminal (not pinentry).

set -euo pipefail

# shellcheck source=tools/site-paths.sh
source "$(dirname "$0")/site-paths.sh"
GNUPGHOME="${GNUPGHOME:-$HOME/.gnupg-signing}"
GPG_PRESET="/usr/lib/gnupg/gpg-preset-passphrase"

# The agent caches by keygrip: the signing subkey's, found from the
# fingerprint site.yaml records (signing-fingerprint).
FINGERPRINT="$(site_config signing-fingerprint | tr -d ' ')"
KEYGRIP="$(gpg --homedir "$GNUPGHOME" --batch --with-colons --with-keygrip --list-keys "$FINGERPRINT" 2>/dev/null \
    | awk -F: -v fpr="$FINGERPRINT" '$1 == "fpr" { hit = ($10 == fpr) } $1 == "grp" && hit { print $10; exit }' \
    || true)"
if [ -z "$KEYGRIP" ]; then
    echo "Error: no key ${FINGERPRINT:-(no signing-fingerprint)} in $GNUPGHOME" >&2
    exit 1
fi

if [ ! -x "$GPG_PRESET" ]; then
    echo "Error: gpg-preset-passphrase not found at $GPG_PRESET" >&2
    exit 1
fi

# Ensure the agent is running with our config.
GNUPGHOME="$GNUPGHOME" gpg-connect-agent --homedir "$GNUPGHOME" /bye >/dev/null 2>&1 || true

echo -n "Signing subkey passphrase: "
read -rs PASSPHRASE
echo

# printf, not `echo -n`: a passphrase starting with -e/-n/-E would be
# eaten as an echo option.
printf '%s' "$PASSPHRASE" | GNUPGHOME="$GNUPGHOME" "$GPG_PRESET" --homedir "$GNUPGHOME" --preset "$KEYGRIP"

echo "Passphrase cached for keygrip $KEYGRIP (24 h TTL)."
echo "Test: GNUPGHOME=$GNUPGHOME gpg --homedir $GNUPGHOME --batch --detach-sign --armor --output /dev/null /dev/null"
