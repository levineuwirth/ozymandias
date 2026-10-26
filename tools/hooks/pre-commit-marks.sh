#!/usr/bin/env bash
# Pre-commit advisory: warn when newly-added essay files are missing a
# monogram (mark.svg) or the epistemic status field. Warning only —
# this hook never blocks a commit. The author is the one staging, and
# the audit table at `make audit-marks` is the canonical view; this
# hook just nudges at the moment of commit.
#
# Install (one-time):
#
#     ln -s ../../engine/tools/hooks/pre-commit-marks.sh .git/hooks/pre-commit
#
# Or chain into an existing pre-commit:
#
#     bash engine/tools/hooks/pre-commit-marks.sh
#
# Scope: newly-added (status `A`) .md files under content/essays/.
# Modified files are not warned about — the author has presumably made
# a deliberate choice about marks by then.

set -u

# Newly-added .md files under content/essays/ in this commit.
# `--name-status` output is TAB-separated (status<TAB>path); split on the
# tab so paths containing spaces survive intact.
mapfile -t added < <(
    git diff --cached --name-status --diff-filter=A -- 'content/essays/*.md' \
        | cut -f2-
)

if [[ ${#added[@]} -eq 0 ]]; then
    exit 0
fi

warnings=0

for path in "${added[@]}"; do
    # Resolve the dual-form mark.svg candidate path. Mirrors
    # build/Marks.hs and tools/audit-marks.py.
    if [[ "$(basename -- "$path")" == "index.md" ]]; then
        mark="$(dirname -- "$path")/mark.svg"
    else
        mark="${path%.md}.mark.svg"
    fi

    has_mark=0
    has_status=0
    [[ -f "$mark" ]] && has_mark=1

    # Best-effort frontmatter probe: does any line in the YAML head
    # block start with `status:`? Avoids a YAML dependency in the
    # hook, which has to run before the build environment is sourced.
    # Probe the STAGED blob (`git show :path`), not the working tree —
    # the commit contains the index content, which may differ.
    if git show ":$path" 2>/dev/null \
            | awk '/^---$/{f++; next} f==1 && /^status:[[:space:]]*[^[:space:]]/{print; exit}' \
            | grep -q .; then
        has_status=1
    fi

    if [[ $has_mark -eq 0 || $has_status -eq 0 ]]; then
        if [[ $warnings -eq 0 ]]; then
            echo "[marks] advisory: newly-added essays missing marks:" >&2
        fi
        msgs=()
        [[ $has_mark   -eq 0 ]] && msgs+=("no mark.svg at $mark")
        [[ $has_status -eq 0 ]] && msgs+=("no status: in frontmatter")
        printf '  %s — %s\n' "$path" "$(IFS=, ; echo "${msgs[*]}")" >&2
        warnings=$((warnings + 1))
    fi
done

if [[ $warnings -gt 0 ]]; then
    echo "[marks] (advisory only — commit not blocked. \`make audit-marks\` for the full report.)" >&2
fi

exit 0
