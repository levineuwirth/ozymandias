#!/usr/bin/env python3
"""make watch's second watcher (plan Q7).

Hakyll's watcher doesn't descend into a linked directory, so with the engine
a checkout of its own beside the site (engine -> ../ozymandias) it misses
the engine's edits. This runs the watch command it is given and, while that
runs, looks over the engine's files every second; when one changes, it
touches data/engine-changed.stamp, which Hakyll's watcher does see, and the
rebuild that sets off finds the engine's changed files itself. An edit to
the generator's Haskell still needs the watch restarted, the generator
being compiled once. With the engine inside the site, Hakyll sees it
already, and this only runs the command.

    watch-engine.py -- cabal run site -- watch
"""

from __future__ import annotations

import os
import subprocess
import sys
import time
from pathlib import Path

ENGINE = Path("engine")
STAMP = Path("data/engine-changed.stamp")
# Neither the site's inputs nor anything an edit there would show.
SKIP = {".git", "__pycache__", "dist-newstyle", "tests", "fixture-site"}
INTERVAL = float(os.environ.get("WATCH_ENGINE_INTERVAL") or 1)


def snapshot(root: Path) -> dict[str, tuple[int, int]]:
    """Each file under root as path -> (size, mtime_ns)."""
    seen: dict[str, tuple[int, int]] = {}
    for directory, dirs, names in os.walk(root):
        dirs[:] = [d for d in dirs if d not in SKIP]
        for name in names:
            path = os.path.join(directory, name)
            try:
                st = os.stat(path)
            except OSError:
                continue
            seen[path] = (st.st_size, st.st_mtime_ns)
    return seen


def main(argv: list[str]) -> int:
    if len(argv) < 2 or argv[0] != "--":
        print("usage: watch-engine.py -- COMMAND...", file=sys.stderr)
        return 2
    child = subprocess.Popen(argv[1:])
    try:
        if ENGINE.is_symlink():
            root = ENGINE.resolve()
            before = snapshot(root)
            while child.poll() is None:
                time.sleep(INTERVAL)
                now = snapshot(root)
                if now != before:
                    STAMP.parent.mkdir(exist_ok=True)
                    STAMP.touch()
                    before = now
        return child.wait()
    except KeyboardInterrupt:
        # The terminal's interrupt reached the command too.
        return child.wait()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
