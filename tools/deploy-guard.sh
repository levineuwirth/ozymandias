#!/usr/bin/env bash
# deploy-guard.sh — refuse an `rsync --delete` that would land in the wrong
# place (audit D04). Called by the Makefile's rsync targets, so it holds
# whether they run under `make deploy` or on their own.
#
#   deploy-guard.sh docroot <VPS_PATH>
#       Validate the document root and print it normalised: surrounding
#       blanks stripped (Make keeps the trailing blank of a commented .env
#       line, `VPS_PATH=/var/www/x  # docroot`) and one trailing slash
#       removed (the typo `/var/www/` is otherwise one directory too high,
#       and is then refused as too short). Refuses
#       an empty, relative or short path, `..`, `//`, and inner whitespace.
#       The Makefile's VPS_DOCROOT derives the same string for any path
#       this accepts.
#
#   deploy-guard.sh target <site-dir> <rsync-destination/>
#       Dry-run the transfer the deploy is about to make and refuse it when
#       the destination has no index.html (it is not the live site: a typo,
#       or a fresh directory — DEPLOY_NEW_DOCROOT=1 for a deliberate first
#       deploy), or when it would delete more than DEPLOY_MAX_DELETE files
#       (default 1000; DEPLOY_ALLOW_DELETE=1 for a deliberate re-layout).
#       Costs one extra rsync connection and deletes nothing.

set -euo pipefail

refuse() { echo "deploy: $*" >&2; exit 1; }

case "${1:-}" in
docroot)
    p="${2-}"
    p="${p#"${p%%[![:space:]]*}"}"       # leading blanks
    p="${p%"${p##*[![:space:]]}"}"       # trailing blanks
    case "$p" in *//*) refuse "VPS_PATH=$p is not a plain path" ;; esac
    p="${p%/}"                            # one trailing slash, as Make's patsubst does
    [ -n "$p" ] || refuse "VPS_PATH is not set (or is only '/') in .env"
    case "$p" in /*) ;; *) refuse "VPS_PATH=$p is not an absolute path" ;; esac
    case "$p" in
        *[[:space:]]*) refuse "VPS_PATH='$p' contains whitespace — a comment on its .env line?" ;;
        *//*|*/../*|*/..|*/./*|*/.) refuse "VPS_PATH=$p is not a plain path" ;;
    esac
    # A docroot is at least three components deep (/var/www/<site>); this
    # also refuses every shallower parent: /var/www, /srv/http, /home/<user>.
    slashes="${p//[!\/]/}"
    [ "${#slashes}" -ge 3 ] || refuse "VPS_PATH=$p has fewer than three components — refusing"
    printf '%s\n' "$p"
    ;;
target)
    site="${2:?site directory}"; dest="${3:?rsync destination}"
    [ -s "$site/index.html" ] || refuse "$site/index.html is missing or empty — refusing to rsync"
    out=$(rsync -an --delete --itemize-changes "$site/" "$dest")
    deletions=$(printf '%s\n' "$out" | grep -c '^\*deleting' || true)
    if printf '%s\n' "$out" | grep -Eq '^[<>]f\+{9,} index\.html$' \
       && [ "${DEPLOY_NEW_DOCROOT:-0}" != 1 ]; then
        refuse "$dest has no index.html, so it is not the live site — refusing" \
               "(DEPLOY_NEW_DOCROOT=1 for a deliberate first deploy)"
    fi
    max="${DEPLOY_MAX_DELETE:-1000}"
    if [ "$deletions" -gt "$max" ] && [ "${DEPLOY_ALLOW_DELETE:-0}" != 1 ]; then
        refuse "rsync would delete $deletions files at $dest (limit $max) — refusing" \
               "(DEPLOY_ALLOW_DELETE=1 if that is intended)"
    fi
    echo "deploy: the transfer to $dest deletes $deletions file(s)"
    ;;
*)
    echo "usage: $0 docroot <VPS_PATH> | target <site-dir> <dest/>" >&2
    exit 2
    ;;
esac
