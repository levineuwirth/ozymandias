#!/usr/bin/env bash
# profile-build-arm.sh — run tools/profile-build.py once, on the next boot,
# before the login screen.
#
#   tools/profile-build-arm.sh             arm: install the unit, set the flag
#   tools/profile-build-arm.sh --status    show what is installed and armed
#   tools/profile-build-arm.sh --print     print the unit it would install
#   tools/profile-build-arm.sh --disarm    remove the flag and the unit
#
# Run it as yourself, not under sudo. The unit has to carry your PATH (cabal
# lives in ~/build/.ghcup/bin, which no system unit would find) and your
# user, so this script writes it from your environment and asks sudo only
# for the two privileged steps: installing it and enabling it.
#
# What arming does. A oneshot system unit is enabled under multi-user.target,
# ordered after network-online.target and BEFORE greetd.service, guarded by
# ConditionPathExists on the flag file. On the next boot it starts as soon
# as the network is up, deletes the flag first (so it runs once per arming,
# even if the profile fails or the machine loses power), and runs the
# profiler as you. greetd, and so the login screen, waits until it finishes
# or until TimeoutStartSec cuts it off. Progress is printed on the console
# and kept in the journal. Every later boot skips the unit, because the flag
# is gone; --disarm removes the unit itself.
#
# Escape hatch: Ctrl+Alt+F2 gives a text console; log in there and run
#   sudo systemctl stop levineuwirth-build-profile
# and the login screen appears.
#
# See BUILD_PROFILE_RUNBOOK.local.md for the whole procedure.

set -euo pipefail

# shellcheck source=tools/site-paths.sh
source "$(dirname "$0")/site-paths.sh"
REPO=$SITE_ROOT
UNIT=levineuwirth-build-profile.service
UNIT_PATH=/etc/systemd/system/$UNIT
FLAG=$REPO/.build-profiles/ARMED
TIMEOUT_MIN=${TIMEOUT_MIN:-90}          # the whole run; greetd waits at most this long
SCENARIO_TIMEOUT_MIN=${SCENARIO_TIMEOUT_MIN:-35}

if [ "$(id -u)" = 0 ]; then
    echo "profile-build-arm: run this as yourself, not as root — it needs your PATH." >&2
    exit 2
fi

# Where your shell moves the toolchain's state, carried like PATH. Without
# CABAL_DIR the 2026-10-06 boot run used cabal's default store, wrote a
# default ~/.config/cabal/config, and the three tests that compile against
# the store failed.
toolchain_env() {
    local var
    for var in CABAL_DIR GHCUP_INSTALL_BASE_PREFIX XDG_CONFIG_HOME XDG_DATA_HOME \
               XDG_STATE_HOME XDG_CACHE_HOME; do
        if [ -n "${!var:-}" ]; then echo "Environment=$var=${!var}"; fi
    done
}

render_unit() {
cat <<EOF
# Written by tools/profile-build-arm.sh on $(date -Iseconds). Inert unless
# $FLAG exists; remove with: tools/profile-build-arm.sh --disarm
[Unit]
Description=Profile the levineuwirth.org build once, before login
Documentation=file://$REPO/BUILD_PROFILE_RUNBOOK.local.md
Wants=network-online.target
After=network-online.target local-fs.target
Before=greetd.service
ConditionPathExists=$FLAG

[Service]
Type=oneshot
User=$(id -un)
Group=$(id -gn)
WorkingDirectory=$REPO
Environment=HOME=$HOME
Environment=PATH=$PATH
$(toolchain_env)
Environment=LANG=${LANG:-C.UTF-8}
Environment=PYTHONUNBUFFERED=1
ExecStartPre=/usr/bin/rm -f $FLAG
ExecStart=/usr/bin/python3 $(engine_path tools/profile-build.py) --timeout-min $SCENARIO_TIMEOUT_MIN
TimeoutStartSec=${TIMEOUT_MIN}min
StandardOutput=journal+console
StandardError=journal+console

[Install]
WantedBy=multi-user.target
EOF
}

status() {
    local enabled
    enabled=$(systemctl is-enabled "$UNIT" 2>/dev/null) || true
    echo "unit:    ${enabled:-not installed}"
    if [ -f "$FLAG" ]; then
        echo "flag:    armed ($FLAG, set $(stat -c %y "$FLAG" | cut -d. -f1))"
    else
        echo "flag:    not armed"
    fi
    echo "results: $(ls -1d "$REPO"/.build-profiles/2* 2>/dev/null | tail -1 || echo none)"
}

case "${1:-}" in
    --status)
        status
        exit 0
        ;;
    --print)
        render_unit
        exit 0
        ;;
    --disarm)
        rm -f "$FLAG"
        if [ -f "$UNIT_PATH" ]; then
            sudo systemctl disable "$UNIT"
            sudo rm -f "$UNIT_PATH"
            sudo systemctl daemon-reload
        fi
        status
        exit 0
        ;;
    "")
        ;;
    *)
        echo "usage: $0 [--print | --status | --disarm]" >&2
        exit 2
        ;;
esac

for need in python3 cabal make /usr/bin/time; do
    command -v "$need" >/dev/null || { echo "profile-build-arm: $need not found on PATH" >&2; exit 1; }
done

unit=$(mktemp)
trap 'rm -f "$unit"' EXIT
render_unit > "$unit"

echo "profile-build-arm: installing $UNIT_PATH (sudo)"
sudo install -m 0644 "$unit" "$UNIT_PATH"
sudo systemctl daemon-reload
sudo systemctl enable "$UNIT"
mkdir -p "$(dirname "$FLAG")"
date -Iseconds > "$FLAG"
echo
status
echo
echo "Armed. Plug in AC power, then reboot. The login screen will wait for the"
echo "profile (up to ${TIMEOUT_MIN} min); results land in .build-profiles/latest/summary.md."
