"""What the browser tests (tests/test_browser_*.py) share: the opt-in gate,
the built site checked for staleness, the enforcing CSP read from nginx/,
and tools/browser/serve.py started on a free port.

They run only with RUN_BROWSER_TESTS=1 (`make test-browser`), against the
_site that `make build` left. Once asked for, nothing they need may be
missing: without Playwright, without _site, or with a _site whose inputs
have changed since it was built, they fail instead of skipping.
"""

from __future__ import annotations

import ast
import atexit
import datetime
import functools
import json
import os
import re
import signal
import subprocess
import sys
import threading
import time
import unittest
import urllib.error
import urllib.request
from collections import Counter, defaultdict
from collections.abc import Iterator
from contextlib import contextmanager
from pathlib import Path

from tests._helpers import SITE_ROOT, engine_path, load_tool
from tests.target import BASELINES, CSP_CONF, built, config, route, routes

HARNESS = engine_path("tools/browser/serve.py").parent
SITE = SITE_ROOT / "_site"
BROWSERS = ("chromium", "firefox")

requires_browser = unittest.skipUnless(
    os.environ.get("RUN_BROWSER_TESTS") == "1",
    "browser tests: RUN_BROWSER_TESTS=1, or `make test-browser`")


def check_site() -> None:
    """_site is what the tree describes: `make build` finished, and no input
    has changed, been added or been removed since (tools/build-inputs.py)."""
    if not (SITE / "index.html").is_file():
        raise AssertionError("_site/ has no index.html: run `make build` first")
    differ = load_tool("build-inputs.py").stale()
    if differ:
        listed = "\n  ".join(differ[:10]) + (f"\n  ... and {len(differ) - 10} more"
                                             if len(differ) > 10 else "")
        raise AssertionError(f"_site does not match its inputs; run `make build` first:\n  {listed}")


def harness_constant(script: str, name: str):
    """A literal a tools/browser script assigns to `name` at top level (its
    widths, its modes), read without running the script: lib makes
    its output directories on import, and the others launch browsers."""
    for node in ast.parse((HARNESS / script).read_text(encoding="utf-8")).body:
        if isinstance(node, ast.Assign) and any(getattr(t, "id", None) == name
                                                for t in node.targets):
            return ast.literal_eval(node.value)
    raise AssertionError(f"tools/browser/{script} defines no {name}")


def harness_routes() -> dict[str, str]:
    """The pages the harness's sweeps load, name -> path, in order (the
    site's tests/routes.yaml, as tools/browser/lib.py reads it)."""
    return dict(routes().pages)


def assert_complete(case: unittest.TestCase, report: dict, expected, what: str) -> None:
    """A harness report holds a record for each key in `expected` and for
    nothing else. Tests check the records there are, so a page the script
    skipped, or a report it left empty, would otherwise pass unchecked."""
    expected = set(expected)
    case.assertTrue(expected, f"{what}: no records expected")
    missing, extra = sorted(expected - set(report)), sorted(set(report) - expected)
    if missing or extra:
        case.fail(f"{what}: {len(missing)} record(s) missing {missing[:12]}, "
                  f"{len(extra)} unexpected {extra[:12]}")


def page_styles(path: str | None = None) -> str:
    """The <link rel="stylesheet"> tags of a built page (by default the
    site's essay, routes.yaml), as root-relative URLs, for a fixture page to
    look as that page does."""
    from urllib.parse import urljoin
    path = path or route("essay")
    html = built(path).read_text(encoding="utf-8")
    links = re.findall(r'<link rel="stylesheet" href="([^"]+)"( media="[^"]+")?>', html)
    if not links:
        raise AssertionError(f"no stylesheets on {path}")
    return "\n".join(f'<link rel="stylesheet" href="{urljoin(path, href)}"{media}>'
                     for href, media in links)


def require_playwright() -> None:
    try:
        import playwright.sync_api  # noqa: F401
    except ImportError as e:
        raise AssertionError("Playwright is not installed: run `make venv`") from e


def enforcing_csp() -> str:
    """The policy nginx/security-headers.conf enforces (before its
    promotion on 2026-10-09, a commented candidate; either is read)."""
    text = CSP_CONF.read_text(encoding="utf-8")
    found = re.findall(r'^(?:#\s+)?add_header Content-Security-Policy "([^"]+)" always;$',
                       text, re.M)
    if len(found) != 1:
        raise AssertionError(f"expected one enforcing Content-Security-Policy line in "
                             f"nginx/security-headers.conf, found {len(found)}")
    return found[0]


@contextmanager
def site_server(workdir: Path, csp: str | None = None,
                fixtures: Path | None = None, compress: bool = False) -> Iterator[str]:
    """tools/browser/serve.py over _site on a free port, with `csp`
    enforcing (no policy at all when None), `fixtures`, if given, served
    under /__fixture/, and the .br/.gz sidecars served when `compress`, as
    nginx does; yields its base URL. CSP reports go to
    workdir/csp-reports.jsonl."""
    args = [sys.executable, str(HARNESS / "serve.py"), "--root", str(SITE), "--port", "0",
            "--csp", csp or "", "--mode", "enforce" if csp else "none",
            "--log", str(workdir / "csp-reports.jsonl")]
    if fixtures:
        args += ["--fixtures", str(fixtures)]
    if compress:
        args += ["--compress"]
    with open(workdir / "serve.stderr", "w") as stderr, \
         subprocess.Popen(args, stdout=subprocess.PIPE, stderr=stderr, text=True) as server:
        try:
            line = server.stdout.readline()
            port = re.search(r" on 127\.0\.0\.1:(\d+) ", line)
            if not port:
                raise AssertionError(f"serve.py did not start: {line!r}\n"
                                     f"{(workdir / 'serve.stderr').read_text()[-2000:]}")
            base = f"http://127.0.0.1:{port.group(1)}"
            # A server that is down leaves every page with nothing to report,
            # which reads as clean (tools/browser/README.md).
            try:
                with urllib.request.urlopen(base + "/", timeout=10) as home:
                    status = home.status
            except urllib.error.URLError as e:
                raise AssertionError(f"serve.py on {base} does not answer: {e}") from e
            if status != 200:
                raise AssertionError(f"serve.py answered / with {status}")
            yield base
        finally:
            server.terminate()
            server.wait(timeout=10)


def in_season(season: dict, day: datetime.date) -> bool:
    """Whether `day` is in `season`'s window (site.yaml's seasons), as
    theme.js reads it: month-day, inclusive, through the new year when it
    starts later than it ends."""
    start, end, md = int(season["from"].replace("-", "")), int(season["to"].replace("-", "")), \
        day.month * 100 + day.day
    return start <= md <= end if start <= end else md >= start or md <= end


@functools.cache
def off_season() -> datetime.datetime:
    """Noon on a day in no season's window. theme.js puts a season on by
    the reader's calendar, so a test that expects the plain themes fixes
    its clock here (context.clock.set_fixed_time) and passes on any date."""
    day = datetime.date(datetime.date.today().year, 1, 1)
    while any(in_season(s, day) for s in config()["seasons"]):
        day += datetime.timedelta(days=1)
    return datetime.datetime(day.year, day.month, day.day, 12)


def fake_answer(body, ctype: str = "application/json", status: int = 200) -> dict:
    """A route.fulfill() answer. It carries the CORS header the real APIs
    send, but the browser does not check CORS on a fulfilled route."""
    if not isinstance(body, (str, bytes)):
        body = json.dumps(body)
    return {"status": status, "body": body,
            "headers": {"content-type": ctype, "access-control-allow-origin": "*"}}


class FakeNetwork:
    """Requests a test answers itself, in place of the network or serve.py.

    `routes` maps a name to a pattern searched in the full URL; the first
    that matches answers from `answers[name]`: a route.fulfill() dict, a
    callable taking the route, or None to abort. A request leaving
    serve.py that no route matches is aborted and kept in `unexpected`,
    which a test should end with empty. Same-origin requests are routed
    only where `local` (a pattern for the path after the origin's "/")
    matches; the rest reach serve.py. `hits` and `requests` count and keep
    what each route answered."""

    def __init__(self, context, base: str, routes: dict, answers: dict, local: str | None = None):
        self.routes = routes
        self.answers = dict(answers)
        self.hits: Counter = Counter()
        self.requests = defaultdict(list)
        self.unexpected: list[str] = []
        origin = re.escape(base)
        context.route(re.compile(rf"^(?!{origin}/)"), self.handle)
        if local:
            context.route(re.compile(rf"^{origin}/(?:{local})"), self.handle)

    def handle(self, route) -> None:
        url = route.request.url
        for name, pattern in self.routes.items():
            if re.search(pattern, url):
                self.hits[name] += 1
                self.requests[name].append(route.request)
                reply = self.answers.get(name)
                if reply is None:
                    route.abort()
                elif callable(reply):
                    reply(route)
                else:
                    route.fulfill(**reply)
                return
        self.unexpected.append(url)
        route.abort()


BASELINE = BASELINES
UPDATE_BASELINE = os.environ.get("UPDATE_BROWSER_BASELINE") == "1"


def group_alive(pgid: int) -> bool:
    """Any process left in the group (signal 0 tests, sends nothing)."""
    try:
        os.killpg(pgid, 0)
        return True
    except ProcessLookupError:
        return False
    except PermissionError:
        return True


def stop_processes(procs, grace: float | None = None) -> None:
    """Harness scripts still running, with their Playwright drivers: SIGTERM
    to each one's process group lets the driver close the browsers it
    launched (they run in groups of their own); then SIGKILL for any group
    with a process left after `grace` seconds (BROWSER_HARNESS_GRACE,
    default 10; tests/run_browser.py sets it below its own grace, since it
    kills a module that has not stopped by then, and a module's harness
    scripts, in sessions of their own, only by way of this cleanup),
    counted once for them all. The group, not its leader: a leader can exit
    on SIGTERM and leave a child that ignores it."""
    if grace is None:
        grace = float(os.environ.get("BROWSER_HARNESS_GRACE", "10"))
    groups = [p for p in procs if p.poll() is None or group_alive(p.pid)]
    for p in groups:
        try:
            os.killpg(p.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
    deadline = time.monotonic() + grace
    # poll() reaps an exited leader, which would otherwise count as one left.
    while time.monotonic() < deadline and any(p.poll() is None or group_alive(p.pid) for p in groups):
        time.sleep(0.1)
    for p in groups:
        if p.poll() is None or group_alive(p.pid):
            try:
                os.killpg(p.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
    for p in groups:
        try:
            p.wait(timeout=5)
        except subprocess.TimeoutExpired:
            pass


def stop_process(proc: subprocess.Popen) -> None:
    stop_processes([proc])


# Every harness script this process starts, stopped when it exits however
# it exits: a class cleanup is not run when setUpClass is interrupted, and
# the scripts run in sessions of their own, where no signal to this
# process's group reaches them. SIGTERM (tests/run_browser.py stopping a
# module) is raised as KeyboardInterrupt, which unittest lets through and
# which ends the run; SystemExit, raised inside a test, would be reported
# as that test's error and the run would go on.
LAUNCHED: list[subprocess.Popen] = []
atexit.register(lambda: stop_processes(LAUNCHED))


def _terminated(signum, frame):
    raise KeyboardInterrupt(f"signal {signum}")


if threading.current_thread() is threading.main_thread() \
        and signal.getsignal(signal.SIGTERM) is signal.SIG_DFL:
    signal.signal(signal.SIGTERM, _terminated)


def run_harness(case, base: str, out: Path, jobs: list[list[str]], *,
                offline: bool = True, seconds: int = 1800) -> None:
    """Run tools/browser scripts (each an argument list: script, then its
    arguments) at once against `base`, writing to `out`, offline unless
    told otherwise; each is stopped at class cleanup if still running.
    Raises with the stderr of any that fail, or of all if time runs out."""
    # The scripts run in their own directory, so they are told the site's.
    env = dict(os.environ, BROWSER_OUT=str(out), BROWSER_PORT=base.rsplit(":", 1)[1],
               SITE_ROOT=str(SITE_ROOT))
    if offline:
        env["BROWSER_OFFLINE"] = "1"
    running = []
    for job in jobs:
        proc = subprocess.Popen([sys.executable, *job], cwd=HARNESS, env=env, text=True,
                                start_new_session=True, stdout=subprocess.DEVNULL,
                                stderr=subprocess.PIPE)
        LAUNCHED.append(proc)
        case.addClassCleanup(stop_process, proc)
        running.append((" ".join(job[:4]), proc))
    start = time.monotonic()
    deadline = start + seconds
    # Each job's own end, for BROWSER_TIMINGS (waiting in order would not say).
    took = {}
    for name, proc in running:
        threading.Thread(target=lambda n=name, p=proc: (p.wait(), took.__setitem__(n, time.monotonic() - start)),
                         daemon=True).start()
    failed = []
    for name, proc in running:
        try:
            _, err = proc.communicate(timeout=max(0.1, deadline - time.monotonic()))
        except subprocess.TimeoutExpired:
            failed.append(f"{name}: still running after {seconds} s")
            continue
        if proc.returncode:
            failed.append(f"{name} exited {proc.returncode}:\n{err[-2000:]}")
    if os.environ.get("BROWSER_TIMINGS") == "1":
        print(f"\n{case.__name__}: harness jobs, slowest first:", file=sys.stderr)
        for name, t in sorted(took.items(), key=lambda kv: -kv[1]):
            print(f"  {t:7.1f} s  {name}", file=sys.stderr)
    if failed:
        raise AssertionError("\n\n".join(failed))


def baseline(name: str, current):
    """The recorded tests/browser-baseline/<name>.json; with
    UPDATE_BROWSER_BASELINE=1, `current` is recorded there first."""
    path = BASELINE / f"{name}.json"
    if UPDATE_BASELINE:
        BASELINE.mkdir(exist_ok=True)
        path.write_text(json.dumps(current, indent=1, sort_keys=True) + "\n", encoding="utf-8")
    if not path.is_file():
        raise AssertionError(f"no {path.relative_to(SITE_ROOT)}: record one with UPDATE_BROWSER_BASELINE=1")
    return json.loads(path.read_text(encoding="utf-8"))


def require_axe() -> Path:
    """tools/browser/axe.min.js, as tools/browser/axe-version records it;
    vendored, so it is beside its record wherever A6 has put them."""
    import hashlib
    record = engine_path("tools/browser/axe-version")
    axe = record.parent / "axe.min.js"
    lines = [l.split() for l in record.read_text(encoding="utf-8").splitlines()
             if l.strip() and not l.lstrip().startswith("#")]
    want = lines[0][1]
    if not axe.is_file() or hashlib.sha256(axe.read_bytes()).hexdigest() != want:
        raise AssertionError("tools/browser/axe.min.js is missing or not the recorded axe-core "
                             f"{lines[0][0]}: run `make test-browser` (tools/browser/fetch_axe.py)")
    return axe
