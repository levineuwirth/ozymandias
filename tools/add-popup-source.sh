#!/usr/bin/env bash
# add-popup-source.sh — Scaffold a new popup provider end-to-end.
#
# Prompts for the handful of facts unique to a new source (name, label,
# a sample URL), then:
#   1. Probes the upstream for CORS support and content-type.
#   2. Prints a PROVIDERS entry stub you paste into static/js/popups.js.
#   3. If CORS-broken, prints + (on confirmation) appends an nginx
#      location block to nginx/popup-proxy.conf.
#   4. Prints a remaining-steps checklist (icon SVG, CSS, CSP comment).
#
# The parse() body is left as a TODO — writing it requires eyes on the
# actual API response, which this tool also fetches and pretty-prints so
# you can inspect the response shape without a second terminal.
#
# Never writes to static/js/popups.js automatically — that file has too
# much surrounding structure to edit blindly. Copy-paste is safer.

set -euo pipefail

# shellcheck source=tools/site-paths.sh
source "$(dirname "$0")/site-paths.sh"
NGINX_CONF="$SITE_ROOT/nginx/popup-proxy.conf"

# ── helpers ──────────────────────────────────────────────────────────

bold()   { printf '\033[1m%s\033[0m\n' "$*"; }
dim()    { printf '\033[2m%s\033[0m\n' "$*"; }
warn()   { printf '\033[33m%s\033[0m\n' "$*" >&2; }
prompt() { local v; read -r -p "$1 " v; printf '%s' "$v"; }

usage() {
    cat <<EOF
Usage: tools/add-popup-source.sh [--help]

Interactive scaffold for a new popups.js provider. Asks a few
questions, probes the upstream, and prints the code you paste into
the provider table (and, if needed, an nginx reverse-proxy block).

Requires: curl, jq (for JSON pretty-printing), xmllint (optional).
EOF
}

[[ "${1:-}" == "--help" || "${1:-}" == "-h" ]] && { usage; exit 0; }

# The site, for the probe's Origin and the proxy's User-Agent.
ORIGIN="$(site_config url)"
HOST="$(site_config host)"
CONTACT="$(site_config contact-email)"

# ── interactive prompts ──────────────────────────────────────────────

bold "── new popup provider ──"
NAME=$(prompt  "slug (lowercase, used as class + data-popup-source key, e.g. 'zenodo'):")
[[ -z "$NAME" ]] && { warn "slug required"; exit 1; }
# The slug is interpolated into nginx directives (location /proxy/$NAME/,
# set \$upstream_$NAME) — validate like import-photo.sh does so a space,
# ';', or '{' can't produce a config that fails to load.
[[ "$NAME" =~ ^[a-z0-9-]+$ ]] || { warn "slug must match ^[a-z0-9-]+\$"; exit 1; }

LABEL=$(prompt "display label (e.g. 'Zenodo'):")
[[ -z "$LABEL" ]] && LABEL="$NAME"

SAMPLE_URL=$(prompt "sample link URL (the kind readers will click):")
[[ -z "$SAMPLE_URL" ]] && { warn "sample URL required"; exit 1; }

API_URL=$(prompt "sample API URL (leave blank if you haven't found it yet):")

# ── CORS + content-type probe ────────────────────────────────────────

echo
bold "── probing upstream ──"

if [[ -n "$API_URL" ]]; then
    HEADERS=$(curl -sSI -H "Origin: $ORIGIN" "$API_URL" 2>&1 || true)
    # grep returns 1 when no match — tolerate that under `set -e` + pipefail.
    CORS=$(printf '%s\n' "$HEADERS" | grep -i 'access-control-allow-origin' | head -1 | tr -d '\r' || true)
    CT=$(printf   '%s\n' "$HEADERS" | grep -i '^content-type'                | head -1 | tr -d '\r' | awk '{print tolower($2)}' || true)

    if [[ -n "$CORS" ]]; then
        echo "  CORS       ✓  $CORS"
        NEEDS_PROXY=0
    else
        warn "  CORS       ✗  none — upstream needs a reverse proxy"
        NEEDS_PROXY=1
    fi

    echo "  Content-Type: ${CT:-unknown}"
    case "$CT" in
        *xml*|*atom*) FETCH_TYPE=xml ;;
        *json*)       FETCH_TYPE=json ;;
        *)            FETCH_TYPE=json; warn "  (unrecognized type — defaulting to json)" ;;
    esac
    echo "  fetchType  → $FETCH_TYPE"
else
    warn "  no API URL supplied; assuming json + CORS-OK (edit by hand if wrong)"
    FETCH_TYPE=json
    NEEDS_PROXY=0
fi

# ── sample response dump (helps write the parser) ────────────────────

if [[ -n "$API_URL" ]]; then
    echo
    bold "── sample response (first ~40 lines) ──"
    RAW=$(curl -sS "$API_URL" 2>&1 || true)
    if [[ "$FETCH_TYPE" == json ]] && command -v jq >/dev/null; then
        printf '%s\n' "$RAW" | jq -C . 2>/dev/null | head -40 || printf '%s\n' "$RAW" | head -40
    elif [[ "$FETCH_TYPE" == xml ]] && command -v xmllint >/dev/null; then
        printf '%s\n' "$RAW" | xmllint --format - 2>/dev/null | head -40 || printf '%s\n' "$RAW" | head -40
    else
        printf '%s\n' "$RAW" | head -40
    fi
fi

# ── proxy prefix + upstream host derivation ──────────────────────────

# UPSTREAM_HOST is derived unconditionally: the no-proxy (direct CORS
# fetch) case is exactly when the host must be added to connect-src, so
# the checklist's CSP reminder below needs it populated either way.
UPSTREAM_HOST=$(printf '%s' "$API_URL" | awk -F/ '{print $3}')
if [[ "$NEEDS_PROXY" -eq 1 ]]; then
    UPSTREAM_PATH=$(printf '%s' "$API_URL" | awk -F/ 'BEGIN{OFS="/"} {$1=""; $2=""; $3=""; print}' | sed 's|^///||')
    PROXY_PATH="/proxy/$NAME/"
    PROXY_API_URL="$PROXY_PATH${UPSTREAM_PATH%%\?*}"
    [[ "$API_URL" == *"?"* ]] && PROXY_API_URL="$PROXY_API_URL?${API_URL#*\?}"
else
    PROXY_API_URL="$API_URL"
fi

# ── PROVIDERS entry stub ─────────────────────────────────────────────

echo
bold "── paste into static/js/popups.js (PROVIDERS array) ──"
dim   "   Insertion order = match priority; pick a spot before less-specific entries."
echo
cat <<EOF
        /* $LABEL — TODO: one-line description. */
        {
            name: '$NAME', label: '$LABEL',
            match: /TODO-regex-for-public-URL/,
            fetchType: '$FETCH_TYPE',
            url: function (ctx) {
                return '${PROXY_API_URL:-TODO-upstream-URL}'; // uses ctx.match / ctx.href
            },
            parse: function (data) {
                // TODO: map response to { title, authors?, meta?, abstract?, stats? }
                if (!data || !data.TITLE_FIELD) return null;
                return {
                    title:    data.TITLE_FIELD,
                    authors:  data.AUTHORS_FIELD || [],
                    abstract: data.ABSTRACT_FIELD || ''
                };
            }
        },
EOF

# ── nginx proxy block ────────────────────────────────────────────────

if [[ "$NEEDS_PROXY" -eq 1 ]]; then
    echo
    bold "── nginx block for $NGINX_CONF ──"
    NGINX_BLOCK=$(cat <<EOF

# ── $LABEL ───────────────────────────────────────────────────────────
# TODO: note the upstream + why it's CORS-broken + cache justification.
location /proxy/$NAME/ {
    set \$upstream_$NAME $UPSTREAM_HOST;
    proxy_pass https://\$upstream_$NAME/;
    proxy_set_header Host \$upstream_$NAME;
    proxy_set_header User-Agent "$HOST popup-proxy ($CONTACT)";
    proxy_ssl_server_name on;

    proxy_cache popup_proxy;
    proxy_cache_valid 200 30d;
    proxy_cache_valid any  5m;
    proxy_cache_use_stale error timeout updating http_500 http_502 http_503 http_504;
    proxy_cache_lock on;
    add_header X-Cache-Status \$upstream_cache_status always;

    add_header Access-Control-Allow-Origin "\$scheme://\$host" always;
}
EOF
)
    printf '%s\n' "$NGINX_BLOCK"

    echo
    ANSWER=$(prompt "append this block to nginx/popup-proxy.conf now? [y/N]")
    if [[ "$ANSWER" =~ ^[Yy] ]]; then
        printf '%s\n' "$NGINX_BLOCK" >> "$NGINX_CONF"
        echo "  appended to $NGINX_CONF"
    else
        echo "  skipped — paste manually when ready"
    fi
fi

# ── remaining-steps checklist ────────────────────────────────────────

echo
bold "── remaining manual steps ──"
cat <<EOF
  1. In static/js/popups.js: paste the PROVIDERS entry into the table
     (it lives after the provider helpers; order = match priority) and
     fill the TODO regex + parse() body.

  2. In static/css/popups.css: add an icon rule (copy any existing
     .popup-source[data-popup-source="…"]::before block and swap in the
     $NAME key + the mask-image path).

  3. Add static/images/link-icons/$NAME.svg — used by both the inline
     link icon and the popup source label. Monochrome SVG, currentColor.

  4. In build/Filters/Links.hs: add a clause to \`domainIcon\` so links
     to this source get data-link-icon="$NAME" at build time.
EOF

if [[ "$NEEDS_PROXY" -eq 0 && -n "$UPSTREAM_HOST" ]]; then
    echo "  5. Add https://$UPSTREAM_HOST to connect-src in"
    echo "     nginx/security-headers.conf (direct CORS fetches are blocked"
    echo "     by CSP otherwise), and mirror it in the popups.js top-comment."
fi

echo
dim "done."
