#!/bin/bash
#
# site-paths.sh — where the site is and where an engine file is, for the
# engine's shell tools, as tools/sitelib.py has them for the Python ones.
# Sourced, not run, from beside the sourcing script:
#
#   source "$(dirname "$0")/site-paths.sh"
#
#   SITE_ROOT          $SITE_ROOT, else the working directory, from which
#                      the Makefile runs every tool
#   engine_path REL    an engine file by its path in the engine: under
#                      engine/ once A6 moves it there, beside these tools,
#                      or, while A6 has moved these tools and not it,
#                      beside the site
#   site_config KEY    one value of the site's site.yaml, as the generator
#                      last built reads and checks it (tools/sitelib.py)
#   no_site_copy DIR   fail if the site still holds the engine's static/DIR
#                      (a download from before the engine took it over):
#                      the build serves a site file over the engine's, so
#                      the old copy would win, unannounced

SITE_ROOT="$(cd "${SITE_ROOT:-.}" && pwd)"
ENGINE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

engine_path() {
    local found
    for found in "$ENGINE/engine/$1" "$ENGINE/$1" "$SITE_ROOT/$1"; do
        if [ -e "$found" ]; then
            printf '%s\n' "$found"
            return
        fi
    done
    printf '%s\n' "$ENGINE/$1"
}

site_config() {
    python3 "$(dirname "${BASH_SOURCE[0]}")/sitelib.py" config "$1"
}

no_site_copy() {
    local engine site="$SITE_ROOT/static/$1"
    engine="$(engine_path static)/$1"
    if [ -e "$site" ] && [ "$site" != "$engine" ]; then
        echo "static/$1: left from before the engine took it over, and the build would serve it over $engine; move it there or delete it" >&2
        return 1
    fi
}
