#!/usr/bin/env python3
"""Watch the engine and site overrides under make watch's existing build lock.

A change stops the entire old command, recompiles via cabal, clears Hakyll's
cache/output, and starts a new watch. This also re-evaluates aliases when files
are added or removed. Failed compilation stays stopped until another edit;
old compiled rules never continue publishing after a compiler-input edit.

Each watch starts after an ordinary build (the same command with `build` for
`watch`) has brought the output up to date. Hakyll's watch starts its preview
server alongside its first build, so it served the last session's pages,
stale, until that build finished: after any change to first-party CSS or JS
(the asset version every page names), the whole site. Now nothing is served
until the pages are current; with nothing to do the build takes seconds.
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
    build = command[:command.index("watch")] + ["build"]
    child = None
    before = inputs()

    def run(step):
        """One step in a process group of its own, for interruption."""
        nonlocal child
        child = subprocess.Popen(step, start_new_session=True)
        code = child.wait()
        child = None
        return code

    try:
        # A failed build still starts the watch, which reports the same error
        # and rebuilds on the next edit, as it always did.
        print("watch-engine: building before the watch starts serving", flush=True)
        run(build)
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
                # Run compilation/clean and the build in process groups too,
                # for interruption.
                code = run(clean)
                if code == 0:
                    # A failed site build still starts the watch, as above: an
                    # error in content is the watch's to report and rebuild,
                    # and content is not among the inputs watched here.
                    run(build)
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
