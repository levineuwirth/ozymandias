#!/usr/bin/env bash
# sign-site.sh — Detach-sign every HTML file in _site/ with the signing subkey.
#
# Requires the passphrase to be pre-cached via tools/preset-signing-passphrase.sh.
# Produces <file>.html.sig alongside each <file>.html.
#
# Usage (called by `make sign`):
#   ./tools/sign-site.sh [site-dir]
#
# A page whose content has not changed keeps its signature. Each run records,
# in $SIGN_MANIFEST (data/sign-manifest.txt, headed by the key and the site
# directory), the SHA-256 of every page it leaves signed and of that page's
# .sig as it wrote or kept it. A signature is reused only when both still
# match: the page is the content it signed, and the .sig is the file it made
# for it, not one swapped, edited or left from another page. Anything else
# is signed again. A content hash, not an mtime: an mtime rule
# misses changed content behind an unchanged mtime, as the compression cache
# found (audit X2). Until 2026-10-06 every page changed on every build (the
# footer time was stamped into each), so every deploy re-signed and re-sent
# all ~500. SIGN_ALL=1 signs every page. Coverage is unaffected either way —
# the post-sign check below still requires a non-empty .sig for every .html
# before the script exits 0.

set -euo pipefail

# shellcheck source=tools/site-paths.sh
source "$(dirname "$0")/site-paths.sh"
GNUPGHOME="${GNUPGHOME:-$HOME/.gnupg-signing}"
SITE_DIR="${1:-_site}"
# The tests sign with a throwaway key; production uses the subkey site.yaml
# records (signing-fingerprint).
SIGNING_KEY="${SIGNING_KEY:-$(site_config signing-fingerprint | tr -d ' ')}"
if [ -z "$SIGNING_KEY" ]; then
    echo "Error: no signing key; site.yaml has no signing-fingerprint." >&2
    exit 1
fi
SIGN_MANIFEST="${SIGN_MANIFEST:-${SITE_ROOT:-.}/data/sign-manifest.txt}"

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
    echo "  Run: engine/tools/preset-signing-passphrase.sh" >&2
    echo "  Then retry: make sign  (or  make deploy)" >&2
    exit 1
fi
echo "sign-site: pre-flight OK — signing $SITE_DIR..." >&2

# Sign sequentially through a single gpg-agent: parallel signing causes
# pinentry/IPC races where individual signs fail silently while xargs
# still exits 0. Atomic write via .tmp + mv avoids leaving a truncated
# .sig if the script is interrupted mid-write.
sign_one() {
    local html="$1"
    local sig="${html}.sig"
    local tmp="${sig}.tmp"
    if ! gpg --homedir "$GNUPGHOME" \
             --batch \
             --yes \
             --detach-sign \
             --armor \
             --local-user "$SIGNING_KEY" \
             --output "$tmp" \
             "$html"; then
        rm -f "$tmp"
        echo "sign-site: FAILED to sign $html" >&2
        return 1
    fi
    mv -f "$tmp" "$sig"
}

# The hashes recorded when the current signatures were made, if they were
# made with this key for this site directory.
# Lines are "<page sha256>  <.sig sha256>  <path>"; v2 added the .sig hash,
# so an older manifest matches nothing and every page is signed once.
header="v2 key $SIGNING_KEY site $(cd "$SITE_DIR" && pwd)"
declare -A signed=()
if [ "${SIGN_ALL:-0}" != "1" ] && [ -f "$SIGN_MANIFEST" ] \
   && [ "$(head -n 1 "$SIGN_MANIFEST")" = "$header" ]; then
    while IFS= read -r line; do
        signed["${line:132}"]="${line:0:130}"
    done < <(tail -n +2 "$SIGN_MANIFEST")
fi

# Written beside the manifest and moved into place only once every page is
# signed: an interrupted run records nothing, and the next one re-signs.
mkdir -p "$(dirname "$SIGN_MANIFEST")"
manifest_tmp="$SIGN_MANIFEST.tmp.$$"
pages_tmp="$SIGN_MANIFEST.pages.$$"
sigs_tmp="$SIGN_MANIFEST.sigs.$$"
trap 'rm -f "$manifest_tmp" "$pages_tmp" "$sigs_tmp"' EXIT
printf '%s\n' "$header" > "$manifest_tmp"

# Every page and every existing .sig hashed up front, each into a file, so a
# page that cannot be read stops the run here: read through a process
# substitution, a failed sha256sum went unseen, and the page was left out
# of the manifest with its old signature in place.
if ! (cd "$SITE_DIR" && find . -name "*.html" -print0 | xargs -0 -r sha256sum) > "$pages_tmp"; then
    echo "sign-site: could not hash every page in $SITE_DIR; nothing signed" >&2
    exit 1
fi
if ! (cd "$SITE_DIR" && find . -name "*.html.sig" -print0 | xargs -0 -r sha256sum) > "$sigs_tmp"; then
    echo "sign-site: could not hash every signature in $SITE_DIR; nothing signed" >&2
    exit 1
fi
declare -A sig_now=()
while IFS= read -r line; do
    rel=${line:66}
    sig_now["${rel#./}"]="${line:0:64}"
done < "$sigs_tmp"

count=0
skipped=0
while IFS= read -r line; do
    hash=${line:0:64}
    rel=${line:66}
    rel=${rel#./}
    html="$SITE_DIR/$rel"
    sig_hash=${sig_now[$rel.sig]:-}
    if [ -s "${html}.sig" ] && [ -n "$sig_hash" ] \
       && [ "${signed[$rel]:-}" = "$hash  $sig_hash" ]; then
        skipped=$((skipped + 1))
    else
        sign_one "$html"
        sig_hash=$(sha256sum "${html}.sig") || exit 1
        sig_hash=${sig_hash:0:64}
        count=$((count + 1))
    fi
    printf '%s  %s  %s\n' "$hash" "$sig_hash" "$rel" >> "$manifest_tmp"
done < "$pages_tmp"
mv -f "$manifest_tmp" "$SIGN_MANIFEST"

# Post-sign manifest verification: every .html must have a non-empty
# matching .sig. This catches any per-file failure that slipped through
# (set -e bails on first failure inside the loop, but a manual --output
# write to a directory containing a stale .sig from a prior run could
# look "successful" otherwise).
missing=0
while IFS= read -r -d '' html; do
    if [ ! -s "${html}.sig" ]; then
        echo "sign-site: missing/empty signature for $html" >&2
        missing=$((missing + 1))
    fi
done < <(find "$SITE_DIR" -name "*.html" -print0)

if [ "$missing" -ne 0 ]; then
    echo "sign-site: $missing HTML files lack signatures — aborting" >&2
    exit 1
fi

if [ "$skipped" -gt 0 ]; then
    echo "Signed $count HTML files in $SITE_DIR ($skipped unchanged, reused)."
else
    echo "Signed $count HTML files in $SITE_DIR."
fi
