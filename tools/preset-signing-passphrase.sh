#!/usr/bin/env bash
# preset-signing-passphrase.sh — Cache the signing subkey passphrase in the
# dedicated signing agent so that `make sign` can run without a prompt.
#
# Reads the master signing-key fingerprint from site.yaml's `gpg-fingerprint`
# field, derives the signing subkey's keygrip via `gpg --with-keygrip`, and
# hands the passphrase to gpg-preset-passphrase. Cached for the TTL configured
# in $GNUPGHOME/gpg-agent.conf (typically 24 h via max-cache-ttl).
#
# Run ONCE in an interactive terminal after system boot or after the agent
# cache expires.
#
# Usage:
#   ./tools/preset-signing-passphrase.sh
#
# Environment:
#   GNUPGHOME — defaults to $HOME/.gnupg-signing (matches sign-site.sh).

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SITE_YAML="$REPO_ROOT/site.yaml"

GNUPGHOME="${GNUPGHOME:-$HOME/.gnupg-signing}"

# gpg-preset-passphrase ships in different paths depending on distro.
PRESET_CANDIDATES=(
    /usr/lib/gnupg/gpg-preset-passphrase
    /usr/libexec/gpg-preset-passphrase
    /usr/lib/gnupg2/gpg-preset-passphrase
)
GPG_PRESET=""
for candidate in "${PRESET_CANDIDATES[@]}"; do
    if [ -x "$candidate" ]; then
        GPG_PRESET="$candidate"
        break
    fi
done
if [ -z "$GPG_PRESET" ]; then
    echo "Error: gpg-preset-passphrase not found in any of:" >&2
    printf '  %s\n' "${PRESET_CANDIDATES[@]}" >&2
    echo "Install gnupg's utilities package (e.g. on Debian: apt install gnupg-agent)." >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Read the master fingerprint from site.yaml
# ---------------------------------------------------------------------------

if [ ! -f "$SITE_YAML" ]; then
    echo "Error: $SITE_YAML not found." >&2
    exit 1
fi

# Minimal parser: looks for a line of the form `gpg-fingerprint: "..."` or
# `gpg-fingerprint: ...`. Strips quotes and surrounding whitespace.
FINGERPRINT="$(awk -F: '
    /^[[:space:]]*gpg-fingerprint[[:space:]]*:/ {
        sub(/^[^:]*:[[:space:]]*/, "")
        gsub(/^["'"'"' ]+|["'"'"' ]+$/, "")
        print
        exit
    }
' "$SITE_YAML")"

if [ -z "$FINGERPRINT" ]; then
    echo "Error: gpg-fingerprint is empty in $SITE_YAML." >&2
    echo "  Set it to the 40-character master-key fingerprint of your signing key," >&2
    echo "  then re-run this script." >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Derive the signing subkey's keygrip
# ---------------------------------------------------------------------------
#
# `gpg --with-keygrip --list-secret-keys <fpr>` output groups each subkey
# with its capability flags `[...]` and a `Keygrip = ...` line. We want the
# keygrip of the first subkey whose capability set contains S (sign).

KEYGRIP="$(GNUPGHOME="$GNUPGHOME" gpg \
            --homedir "$GNUPGHOME" \
            --with-keygrip \
            --with-colons \
            --list-secret-keys "$FINGERPRINT" 2>/dev/null \
        | awk -F: '
            $1 == "ssb" && $12 ~ /s/ { want = 1; next }
            want && $1 == "grp"      { print $10; exit }
          ')"

# Fallback: if no signing subkey was found, try the primary key (some users
# sign with the primary instead of a subkey).
if [ -z "$KEYGRIP" ]; then
    KEYGRIP="$(GNUPGHOME="$GNUPGHOME" gpg \
                --homedir "$GNUPGHOME" \
                --with-keygrip \
                --with-colons \
                --list-secret-keys "$FINGERPRINT" 2>/dev/null \
            | awk -F: '
                $1 == "sec" && $12 ~ /s/ { want = 1; next }
                want && $1 == "grp"      { print $10; exit }
              ')"
fi

if [ -z "$KEYGRIP" ]; then
    echo "Error: could not derive a signing keygrip for $FINGERPRINT" >&2
    echo "  Verify the key exists in GNUPGHOME=$GNUPGHOME:" >&2
    echo "    GNUPGHOME=$GNUPGHOME gpg --list-secret-keys $FINGERPRINT" >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Ensure agent is running, then preset
# ---------------------------------------------------------------------------

GNUPGHOME="$GNUPGHOME" gpg-connect-agent --homedir "$GNUPGHOME" /bye >/dev/null 2>&1 || true

echo -n "Signing-key passphrase (will not echo): "
read -rs PASSPHRASE
echo

echo -n "$PASSPHRASE" | GNUPGHOME="$GNUPGHOME" \
    "$GPG_PRESET" --homedir "$GNUPGHOME" --preset "$KEYGRIP"

echo "Passphrase cached for keygrip $KEYGRIP."
echo "Verify with:"
echo "  GNUPGHOME=$GNUPGHOME gpg --homedir $GNUPGHOME --batch --detach-sign --armor --output /dev/null /dev/null"
