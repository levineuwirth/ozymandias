#!/usr/bin/env python3
"""Run the browser tests (tests/test_browser_*.py) side by side.

The modules share nothing: each starts its own serve.py on a free port and
its own browsers. One after another they took about 25 minutes. Here up to
BROWSER_JOBS of them (default 3) run at once, the longest first by the
times the last run recorded (.browser-runs/durations.json, gitignored), and
each module's output is printed whole when it ends, then a summary. Exits 1
if any module failed or was not run. `make test-browser` runs it, under the
build lock.

    tests/run_browser.py                  # every module
    tests/run_browser.py nav toc          # some ("test_browser_" optional)
    BROWSER_JOBS=1 tests/run_browser.py   # one at a time, as before

BROWSER_TIMINGS=1 adds each module's ten slowest tests (unittest
--durations; a class's setup, where the report modules do their work, is
not counted) and its harness jobs, slowest first.

Memory: a module starts only while BROWSER_MIN_FREE_GB (default 4) is
available. Six at once, on a machine already using most of its memory,
left 5 GB and was stopped. With nothing running and too little memory the
runner waits up to BROWSER_MEM_WAIT seconds (default 300), then stops,
naming the modules it did not run.

Stopping: SIGINT or SIGTERM stops the run. Each running module is sent
SIGTERM (tests/_browser.py then stops the harness scripts it started, which
run in sessions of their own), and any module still there after
BROWSER_STOP_GRACE seconds (default 30) is killed with its group. Killing a
module ends its cleanup, and the harness scripts it had yet to kill
outlive it, so the modules get a third of that grace for their harness
scripts (BROWSER_HARNESS_GRACE): the rest is margin for their own exit.
"""

from __future__ import annotations

import json
import os
import signal
import subprocess
import sys
import tempfile
import time
from pathlib import Path

# The site the modules run against ($SITE_ROOT, else the working directory,
# as tests/_helpers.py has it), and the directory holding the tests package
# they are imported from ($BROWSER_TESTS_ROOT for this runner's own tests).
SITE_ROOT = Path(os.environ.get("SITE_ROOT") or os.getcwd()).resolve()
TESTS_ROOT = Path(os.environ.get("BROWSER_TESTS_ROOT") or Path(__file__).resolve().parents[1])
DURATIONS = SITE_ROOT / ".browser-runs" / "durations.json"


class Stopped(Exception):
    def __init__(self, signum: int):
        super().__init__(signum)
        self.signum = signum


def _stop(signum, frame):
    raise Stopped(signum)


def modules(names: list[str]) -> list[str]:
    """The modules to run: all, or those named, each once (with or without
    the "test_browser_" prefix, a name is one module)."""
    found = sorted(p.stem for p in (TESTS_ROOT / "tests").glob("test_browser_*.py"))
    if not names:
        return found
    wanted = []
    for n in names:
        m = n if n.startswith("test_browser_") else f"test_browser_{n}"
        if m not in wanted:
            wanted.append(m)
    unknown = [w for w in wanted if w not in found]
    if unknown:
        raise SystemExit(f"run_browser: no such module: {', '.join(unknown)}")
    return wanted


def available_gb() -> float:
    """MemAvailable, in GB; plenty if it cannot be read."""
    try:
        for line in Path("/proc/meminfo").read_text().splitlines():
            if line.startswith("MemAvailable:"):
                return int(line.split()[1]) / 1024 / 1024
    except OSError:
        pass
    return float("inf")


def recorded() -> dict[str, float]:
    try:
        return json.loads(DURATIONS.read_text())
    except (OSError, ValueError):
        return {}


def group_alive(pgid: int) -> bool:
    """Any process left in the group (signal 0 tests, sends nothing)."""
    try:
        os.killpg(pgid, 0)
        return True
    except ProcessLookupError:
        return False
    except PermissionError:
        return True


def stop_all(running: dict, grace: float) -> None:
    """SIGTERM to each running module's group; SIGKILL to any group with a
    process left after `grace` seconds, counted once for them all. The
    group, not its leader: a module can exit on SIGTERM and leave a child
    that ignores it."""
    procs = [proc for proc, _, _ in running.values()]
    for proc in procs:
        try:
            os.killpg(proc.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
    deadline = time.monotonic() + grace
    # poll() reaps an exited module, which would otherwise count as one left.
    while time.monotonic() < deadline and any(p.poll() is None or group_alive(p.pid) for p in procs):
        time.sleep(0.1)
    for m, (proc, _, _) in running.items():
        if proc.poll() is None or group_alive(proc.pid):
            print(f"run_browser: {m} did not stop in {grace:.0f} s; killed", flush=True)
            try:
                os.killpg(proc.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
    for proc in procs:
        try:
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            pass


def main() -> int:
    jobs = max(1, int(os.environ.get("BROWSER_JOBS", "3")))
    min_free = float(os.environ.get("BROWSER_MIN_FREE_GB", "4"))
    mem_wait = float(os.environ.get("BROWSER_MEM_WAIT", "300"))
    grace = float(os.environ.get("BROWSER_STOP_GRACE", "30"))
    todo = modules(sys.argv[1:])
    took = recorded()
    # Longest first, so the long ones do not start last; unknown ones first of all.
    todo.sort(key=lambda m: -took.get(m, float("inf")))
    env = dict(os.environ, RUN_BROWSER_TESTS="1", BROWSER_HARNESS_GRACE=f"{grace / 3:g}",
               SITE_ROOT=str(SITE_ROOT))
    durations = ["--durations=10"] if os.environ.get("BROWSER_TIMINGS") == "1" else []
    running: dict[str, tuple[subprocess.Popen, float, object]] = {}
    results: dict[str, tuple[int, float]] = {}
    not_run: list[str] = []
    start = time.monotonic()
    low_since = None
    print(f"run_browser: {len(todo)} modules, {jobs} at a time", flush=True)
    signal.signal(signal.SIGINT, _stop)
    signal.signal(signal.SIGTERM, _stop)
    try:
        while todo or running:
            while todo and len(running) < jobs:
                if available_gb() < min_free:
                    if running:
                        break                      # one ending frees some
                    now = time.monotonic()
                    if low_since is None:
                        low_since = now
                        print(f"run_browser: {available_gb():.1f} GB available, under"
                              f" {min_free:g}; waiting up to {mem_wait:.0f} s", flush=True)
                    if now - low_since >= mem_wait:
                        not_run, todo = todo, []
                        print(f"run_browser: memory stayed under {min_free:g} GB;"
                              f" not run: {', '.join(not_run)}", flush=True)
                    break
                low_since = None
                m = todo.pop(0)
                log = tempfile.TemporaryFile(mode="w+")
                proc = subprocess.Popen([sys.executable, "-m", "unittest", "-v", *durations, f"tests.{m}"],
                                        cwd=TESTS_ROOT, env=env, stdout=log, stderr=subprocess.STDOUT,
                                        text=True, start_new_session=True)
                running[m] = (proc, time.monotonic(), log)
            time.sleep(0.5)
            for m, (proc, began, log) in list(running.items()):
                if proc.poll() is None:
                    continue
                del running[m]
                results[m] = (proc.returncode, time.monotonic() - began)
                log.seek(0)
                print(f"\n===== {m}: {'ok' if proc.returncode == 0 else 'FAILED'}"
                      f" in {results[m][1]:.0f} s =====", flush=True)
                sys.stdout.write(log.read())
                log.close()
                sys.stdout.flush()
    except Stopped as e:
        signal.signal(signal.SIGINT, signal.SIG_IGN)
        signal.signal(signal.SIGTERM, signal.SIG_IGN)
        print(f"\nrun_browser: stopped (signal {e.signum}); stopping"
              f" {', '.join(running) or 'nothing'}", flush=True)
        stop_all(running, grace)
        return 128 + e.signum

    wall = time.monotonic() - start
    failed = sorted(m for m, (rc, _) in results.items() if rc)
    print("\n===== summary: slowest first =====")
    for m, (rc, t) in sorted(results.items(), key=lambda kv: -kv[1][1]):
        print(f"  {t:7.1f} s  {'ok    ' if rc == 0 else 'FAILED'}  {m}")
    for m in not_run:
        print(f"  {'':7}    NOT RUN {m}")
    print(f"  {wall:7.1f} s  wall, {sum(t for _, t in results.values()):.0f} s of modules, {jobs} at a time")
    print("OK" if not failed and not not_run
          else "FAILED: " + ", ".join(failed + [f"{m} (not run)" for m in not_run]))

    # Record the times of a full, passing run for the next one's order.
    if not failed and not not_run and not sys.argv[1:]:
        DURATIONS.parent.mkdir(exist_ok=True)
        DURATIONS.write_text(json.dumps({m: round(t, 1) for m, (_, t) in results.items()},
                                        indent=1, sort_keys=True) + "\n")
    return 1 if failed or not_run else 0


if __name__ == "__main__":
    sys.exit(main())
