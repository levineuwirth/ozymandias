#!/usr/bin/env python3
"""Watch the engine and site overrides under make watch's existing build lock.

A change stops the entire old command, recompiles via cabal, clears Hakyll's
cache/output, and starts a new watch. This also re-evaluates aliases when files
are added or removed. Failed compilation stays stopped until another edit;
old compiled rules never continue publishing after a compiler-input edit.
"""
from __future__ import annotations
import os
import signal
import subprocess
import sys
import time
from pathlib import Path

SKIP = {".git", "__pycache__", "dist-newstyle", "tests", "fixture-site"}
INTERVAL = float(os.environ.get("WATCH_ENGINE_INTERVAL") or 1)


def snapshot(root: Path) -> dict[str, tuple[int, int]]:
    seen = {}
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


def inputs():
    seen = {}
    for directory in (Path("engine").resolve(), Path("templates"), Path("static")):
        seen.update(snapshot(directory))
    for name in ("cabal.project", "cabal.project.local", "cabal.project.freeze", "site.yaml", "overrides.yaml"):
        path = Path(name)
        if path.exists():
            stat = path.stat()
            seen[name] = (stat.st_size, stat.st_mtime_ns)
    return seen


def stop(child):
    if child is not None and child.poll() is None:
        os.killpg(child.pid, signal.SIGTERM)
        try:
            child.wait(timeout=5)
        except subprocess.TimeoutExpired:
            os.killpg(child.pid, signal.SIGKILL)
            child.wait()


def main(argv):
    if len(argv) < 2 or argv[0] != "--" or "watch" not in argv[1:]:
        print("usage: watch-engine.py -- COMMAND... watch [WATCH_OPTIONS...]", file=sys.stderr)
        return 2
    command = argv[1:]
    clean = command[:command.index("watch")] + ["clean"]
    child = None
    before = inputs()
    try:
        child = subprocess.Popen(command, start_new_session=True)
        while True:
            time.sleep(INTERVAL)
            now = inputs()
            if now != before:
                stop(child)
                child = None
                print("watch-engine: inputs changed; rebuilding the generator and restarting watch", flush=True)
                before = now
                # The parent make/with-lock process holds the lock throughout.
                # Run compilation/clean in a process group too, for interruption.
                child = subprocess.Popen(clean, start_new_session=True)
                code = child.wait()
                child = None
                if code == 0:
                    # An edit during compilation must be incorporated by another
                    # compile before starting watch with its new rules.
                    if inputs() != before:
                        continue
                    child = subprocess.Popen(command, start_new_session=True)
                else:
                    print("watch-engine: rebuild failed; waiting for another edit", file=sys.stderr, flush=True)
            elif child is not None and child.poll() is not None:
                return child.returncode
    except KeyboardInterrupt:
        return 130
    finally:
        stop(child)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
