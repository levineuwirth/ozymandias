#!/bin/bash
#
# pin-check.sh — the SHA-256 check the vendoring scripts share
# (download-pdfjs.sh, download-leaflet.sh, download-model.sh). Sourced, not
# run. Each script keeps a checksum file of `<sha256>  <key>` lines; the key
# names the version too (pdfjs-5.6.205-dist.zip), so a version bump cannot
# match an older hash.
#
# The scripts used to skip verification, with a warning, when the checksum
# file or the key's line was missing, and so vendored an unverified file
# into the site. A missing pin now fails like a mismatch. To bump a
# version, run the script once with ALLOW_UNPINNED=1: it vendors the new
# files and prints the lines to add to the checksum file (cleanup pass,
# 2026-10-05).

# pin_verify <checksums> <key> <file> <label>
#   0 if <file> matches the SHA-256 pinned for <key>, or if nothing is pinned
#   for <key> and ALLOW_UNPINNED=1 (then the line to pin is printed);
#   1 otherwise, saying why on stderr, prefixed with <label>.
pin_verify() {
    local checksums=$1 key=$2 file=$3 label=$4 want="" got
    got=$(sha256sum "$file" | awk '{ print $1 }')
    if [ -f "$checksums" ]; then
        want=$(awk -v k="$key" '$2 == k { print $1; exit }' "$checksums")
    fi
    if [ -z "$want" ]; then
        if [ "${ALLOW_UNPINNED:-0}" = 1 ]; then
            echo "$label: $key is not pinned; ALLOW_UNPINNED=1, so vendoring it. Pin it with" >&2
            echo "$label:   echo '$got  $key' >> $checksums" >&2
            return 0
        fi
        echo "$label: no SHA-256 pinned for $key in $checksums; refusing to vendor it" >&2
        echo "$label: (to bump a version, rerun with ALLOW_UNPINNED=1 and pin what it prints)" >&2
        return 1
    fi
    if [ "$got" != "$want" ]; then
        echo "$label: sha256 mismatch for $key" >&2
        echo "$label:   expected $want" >&2
        echo "$label:   got      $got" >&2
        return 1
    fi
}
