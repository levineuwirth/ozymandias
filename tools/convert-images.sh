#!/usr/bin/env bash
# convert-images.sh — a WebP companion beside every local JPEG and PNG.
#
# The work is tools/convert_images.py, which says what it does, what it
# refuses and whose files it leaves alone (the ownership record,
# tools/derived_images.py). This is the name `make` and the documents
# know it by.
#
# Requires: cwebp — Arch ships it in libwebp-utils (NOT libwebp, which
#   is library-only). Debian/Ubuntu ship it in the webp package.
#   Install: pacman -S libwebp-utils  /  apt install webp
set -euo pipefail
exec python3 "$(dirname "$0")/convert_images.py" "$@"
