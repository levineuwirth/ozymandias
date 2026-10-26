#!/usr/bin/env python3
"""profile-build.py — where a build spends its time.

Runs a set of build scenarios and records, for each one: the wall time of
every Makefile stage (from the "# ---- Stage N" lines make echoes), which
programs were on the CPU during each stage, CPU / iowait / disk throughput
over time, the longest silent stretches of output and the line that preceded
them, and /usr/bin/time -v totals (peak RSS, page faults, context switches).
A short brotli quality sweep over the largest score pages tests the leading
hypothesis for the cost of a full rebuild directly.

It is meant to run on an otherwise idle machine — ideally straight after a
reboot, before login (see tools/profile-build-arm.sh and
BUILD_PROFILE_RUNBOOK.local.md) — so that the page cache is cold for the
first build and nothing else competes for the CPU. It also runs by hand.

Every build runs with SKIP_SNAPSHOT=1, so nothing is auto-committed, and
nothing is signed, pushed or deployed. The "full" scenario runs
`cabal run site -- clean` first, so it leaves a freshly rebuilt _site.

Results go to .build-profiles/<timestamp>/ (gitignored): summary.md first,
then summary.json, and per scenario the timestamped log, the 1 s samples and
the time -v report.

Usage:
    tools/profile-build.py                       # every scenario, in order
    tools/profile-build.py --scenarios warm-noop,brotli-sweep
    tools/profile-build.py --list
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import platform
import re
import shutil
import socket
import subprocess
import sys
import threading
import time
from collections import Counter
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import sitelib  # noqa: E402

SITE_ROOT = sitelib.SITE_ROOT
OUT_ROOT = SITE_ROOT / ".build-profiles"
SAMPLE_SECONDS = 1.0
STAGE_RE = re.compile(r"^# ---- (Stage \d+: .*?)\s*-*\s*$")
CLK_TCK = os.sysconf("SC_CLK_TCK")

# name -> (description, command). Order is the default run order, and it
# matters: the first build after a boot is the only one with a cold page
# cache, and "full" must come after the no-op runs it is compared with.
SCENARIOS: dict[str, tuple[str, list[str]]] = {
    "generator": (
        "compile the site generator if any Haskell changed (kept out of the "
        "build timings that follow)",
        ["cabal", "build", "-v1", "exe:site"],
    ),
    "cold-noop": (
        "make build with nothing changed; right after a boot the page cache "
        "is cold, so this is the slowest no-op build",
        ["make", "build"],
    ),
    "warm-noop": (
        "make build again at once: the same work with a warm cache — what an "
        "ordinary deploy of a small edit costs",
        ["make", "build"],
    ),
    "full": (
        "cabal run site -- clean, then make build: the worst case, which "
        "build-freshness.sh forces after any build/*.hs change, deletion or "
        "route-metadata edit",
        ["sh", "-c", "cabal run -v0 site -- clean && make build"],
    ),
    "validate": (
        "make validate: both test suites and the site gate, as deploy runs them",
        ["make", "validate"],
    ),
    "brotli-sweep": (
        "brotli at q11 (what compress-assets uses), 9, 7 and 5, plus gzip -9, "
        "on the eight largest score pages",
        [],  # run in-process
    ),
}


# ---------------------------------------------------------------------------
# System facts
# ---------------------------------------------------------------------------


def read(path: str, default: str = "") -> str:
    try:
        return Path(path).read_text().strip()
    except OSError:
        return default


def run_text(args: list[str]) -> str:
    try:
        return subprocess.run(args, cwd=SITE_ROOT, capture_output=True, text=True,
                              timeout=60).stdout.strip()
    except (OSError, subprocess.SubprocessError):
        return ""


def dir_bytes(path: Path, pattern: str = "*") -> tuple[int, int]:
    total = count = 0
    for p in path.rglob(pattern):
        if p.is_file():
            total += p.stat().st_size
            count += 1
    return total, count


def system_facts() -> dict:
    power = {p.name: read(str(p / "online"))
             for p in Path("/sys/class/power_supply").glob("*") if (p / "online").exists()}
    svg_bytes, svg_count = dir_bytes(SITE_ROOT / "content" / "music", "*.svg")
    site_bytes, site_files = dir_bytes(SITE_ROOT / "_site") if (SITE_ROOT / "_site").exists() else (0, 0)
    return {
        "when": dt.datetime.now().astimezone().isoformat(timespec="seconds"),
        "host": socket.gethostname(),
        "kernel": platform.release(),
        "cpu": next((l.split(":", 1)[1].strip() for l in read("/proc/cpuinfo").splitlines()
                     if l.startswith("model name")), "?"),
        "threads": os.cpu_count(),
        "governor": read("/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor"),
        "epp": read("/sys/devices/system/cpu/cpu0/cpufreq/energy_performance_preference"),
        "platform_profile": read("/sys/firmware/acpi/platform_profile"),
        "power_supply_online": power,
        "uptime_s": float(read("/proc/uptime", "0").split()[0]),
        "loadavg": read("/proc/loadavg"),
        "mem_total_kb": next((int(l.split()[1]) for l in read("/proc/meminfo").splitlines()
                              if l.startswith("MemTotal")), 0),
        "sessions": run_text(["loginctl", "list-sessions", "--no-legend"]),
        "git_head": run_text(["git", "rev-parse", "--short", "HEAD"]),
        "git_dirty": len(run_text(["git", "status", "--porcelain"]).splitlines()),
        "site_bytes": site_bytes,
        "site_files": site_files,
        "score_svg_bytes": svg_bytes,
        "score_svg_files": svg_count,
        "network": network_up(),
    }


def network_up() -> bool:
    try:
        socket.getaddrinfo("github.com", 443)
        return True
    except OSError:
        return False


def wait_for_network(limit_s: int) -> bool:
    deadline = time.monotonic() + limit_s
    while time.monotonic() < deadline:
        if network_up():
            return True
        time.sleep(2)
    return network_up()


# ---------------------------------------------------------------------------
# Sampling
# ---------------------------------------------------------------------------


def cpu_totals() -> list[int]:
    return [int(x) for x in read("/proc/stat").splitlines()[0].split()[1:]]


def disk_sectors() -> tuple[int, int]:
    """Sectors read and written across whole physical disks."""
    rd = wr = 0
    for line in read("/proc/diskstats").splitlines():
        f = line.split()
        name = f[2]
        if re.fullmatch(r"(nvme\d+n\d+|sd[a-z]+|vd[a-z]+)", name):
            rd += int(f[5])
            wr += int(f[9])
    return rd, wr


def process_ticks() -> dict[int, tuple[str, int]]:
    out: dict[int, tuple[str, int]] = {}
    for d in os.listdir("/proc"):
        if not d.isdigit():
            continue
        try:
            raw = Path(f"/proc/{d}/stat").read_text()
        except OSError:
            continue
        lpar, rpar = raw.find("("), raw.rfind(")")
        comm = raw[lpar + 1:rpar]
        fields = raw[rpar + 2:].split()
        out[int(d)] = (comm, int(fields[11]) + int(fields[12]))  # utime + stime
    return out


class Sampler(threading.Thread):
    """Every SAMPLE_SECONDS: CPU split, disk throughput, memory, and the CPU
    seconds each program name used since the last sample.

    Per-program time covers processes alive at two samples in a row. A
    process that starts and exits between samples (a gzip of one small
    page) is missed there, though its CPU still shows in busy_pct, so
    read the program table as "where the long-running work went"."""

    def __init__(self, t0: float) -> None:
        super().__init__(daemon=True)
        self.t0 = t0
        self.samples: list[dict] = []
        self.stop = threading.Event()

    def run(self) -> None:
        prev_cpu, prev_disk, prev_proc = cpu_totals(), disk_sectors(), process_ticks()
        while not self.stop.wait(SAMPLE_SECONDS):
            cpu, disk, procs = cpu_totals(), disk_sectors(), process_ticks()
            delta = [a - b for a, b in zip(cpu, prev_cpu)]
            total = sum(delta) or 1
            by_comm: Counter = Counter()
            for pid, (comm, ticks) in procs.items():
                before = prev_proc.get(pid)
                used = ticks - before[1] if before and before[0] == comm else ticks if before is None else 0
                if used > 0:
                    by_comm[comm] += used / CLK_TCK
            self.samples.append({
                "t": round(time.monotonic() - self.t0, 2),
                "busy_pct": round(100 * (total - delta[3] - delta[4]) / total, 1),
                "iowait_pct": round(100 * delta[4] / total, 1),
                "read_mb_s": round((disk[0] - prev_disk[0]) * 512 / 1e6 / SAMPLE_SECONDS, 1),
                "write_mb_s": round((disk[1] - prev_disk[1]) * 512 / 1e6 / SAMPLE_SECONDS, 1),
                "mem_avail_mb": next((int(l.split()[1]) // 1024 for l in read("/proc/meminfo").splitlines()
                                      if l.startswith("MemAvailable")), 0),
                "load1": float(read("/proc/loadavg", "0").split()[0]),
                "cpu_s_by_comm": {k: round(v, 2) for k, v in by_comm.most_common(8)},
            })
            prev_cpu, prev_disk, prev_proc = cpu, disk, procs


# ---------------------------------------------------------------------------
# Running a scenario
# ---------------------------------------------------------------------------


def run_scenario(name: str, cmd: list[str], out: Path, timeout_s: int) -> dict:
    env = dict(os.environ, SKIP_SNAPSHOT="1")
    time_report = out / f"{name}.time.txt"
    full_cmd = ["/usr/bin/time", "-v", "-o", str(time_report), *cmd] \
        if Path("/usr/bin/time").exists() else cmd
    lines: list[tuple[float, str]] = []
    t0 = time.monotonic()
    sampler = Sampler(t0)
    sampler.start()
    status = None
    with open(out / f"{name}.log", "w", encoding="utf-8") as log:
        proc = subprocess.Popen(full_cmd, cwd=SITE_ROOT, env=env, stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, text=True, errors="replace",
                                bufsize=1)
        killer = threading.Timer(timeout_s, proc.kill)
        killer.start()
        assert proc.stdout
        for line in proc.stdout:
            t = time.monotonic() - t0
            line = line.rstrip("\n")
            lines.append((t, line))
            log.write(f"{t:9.2f}  {line}\n")
            print(f"[{name} {t:7.1f}s] {line}", flush=True)
        status = proc.wait()
        killer.cancel()
    wall = time.monotonic() - t0
    sampler.stop.set()
    sampler.join()
    (out / f"{name}.samples.jsonl").write_text(
        "".join(json.dumps(s) + "\n" for s in sampler.samples))
    return {
        "name": name,
        "command": " ".join(cmd),
        "exit": status,
        "timed_out": wall >= timeout_s,
        "wall_s": round(wall, 1),
        "stages": stage_table(lines, wall, sampler.samples),
        "gaps": longest_gaps(lines, wall),
        "time_v": parse_time_v(time_report),
        "cpu_s_by_comm": top_comms(sampler.samples, 0, wall, 12),
    }


def stage_table(lines: list[tuple[float, str]], wall: float, samples: list[dict]) -> list[dict]:
    marks = [(0.0, "before Stage 0")]
    for t, line in lines:
        m = STAGE_RE.match(line)
        if m:
            marks.append((t, m.group(1)))
    rows = []
    for i, (start, label) in enumerate(marks):
        end = marks[i + 1][0] if i + 1 < len(marks) else wall
        if end - start < 0.05 and label == "before Stage 0":
            continue
        window = [s for s in samples if start <= s["t"] < end] or [{}]
        rows.append({
            "stage": label,
            "seconds": round(end - start, 1),
            "share_pct": round(100 * (end - start) / wall, 1) if wall else 0,
            "busy_pct": avg(window, "busy_pct"),
            "iowait_pct": avg(window, "iowait_pct"),
            "write_mb_s": avg(window, "write_mb_s"),
            "top": top_comms(samples, start, end, 4),
        })
    return rows


def avg(window: list[dict], key: str) -> float:
    vals = [s[key] for s in window if key in s]
    return round(sum(vals) / len(vals), 1) if vals else 0.0


def top_comms(samples: list[dict], start: float, end: float, n: int) -> dict[str, float]:
    total: Counter = Counter()
    for s in samples:
        if start <= s["t"] < end:
            total.update(s["cpu_s_by_comm"])
    return {k: round(v, 1) for k, v in total.most_common(n)}


def longest_gaps(lines: list[tuple[float, str]], wall: float, n: int = 12) -> list[dict]:
    """The longest stretches with no output, and the line before each: the
    command that was running, or the last thing it said."""
    points = [(0.0, "(start)")] + lines + [(wall, "(end)")]
    gaps = [{"seconds": round(b[0] - a[0], 1), "at_s": round(a[0], 1), "after": a[1][:160]}
            for a, b in zip(points, points[1:])]
    return sorted(gaps, key=lambda g: -g["seconds"])[:n]


def parse_time_v(path: Path) -> dict:
    keep = {
        "Maximum resident set size (kbytes)": "max_rss_kb",
        "User time (seconds)": "user_s",
        "System time (seconds)": "sys_s",
        "Percent of CPU this job got": "cpu_pct",
        "Major (requiring I/O) page faults": "major_faults",
        "Voluntary context switches": "voluntary_cs",
        "Involuntary context switches": "involuntary_cs",
        "File system inputs": "fs_inputs",
        "File system outputs": "fs_outputs",
    }
    out = {}
    for line in read(str(path)).splitlines():
        key, _, val = line.strip().rpartition(": ")
        if key in keep:
            out[keep[key]] = val
    return out


# ---------------------------------------------------------------------------
# Brotli sweep
# ---------------------------------------------------------------------------


def brotli_sweep(out: Path) -> dict:
    pages = sorted((SITE_ROOT / "content" / "music").glob("*/scores/*.svg"),
                   key=lambda p: -p.stat().st_size)[:8]
    total_svg, _ = dir_bytes(SITE_ROOT / "content" / "music", "*.svg")
    sample_bytes = sum(p.stat().st_size for p in pages)
    rows = []
    for label, args in [("brotli -q 11", ["brotli", "-c", "-q", "11"]),
                        ("brotli -q 9", ["brotli", "-c", "-q", "9"]),
                        ("brotli -q 7", ["brotli", "-c", "-q", "7"]),
                        ("brotli -q 5", ["brotli", "-c", "-q", "5"]),
                        ("gzip -9", ["gzip", "-9", "-n", "-c"])]:
        if not shutil.which(args[0]) or not pages:
            continue
        t0, size = time.monotonic(), 0
        for p in pages:
            size += len(subprocess.run([*args, str(p)], capture_output=True, check=True).stdout)
        secs = time.monotonic() - t0
        rows.append({
            "codec": label,
            "seconds": round(secs, 2),
            "mb_per_s": round(sample_bytes / 1e6 / secs, 2) if secs else 0,
            "ratio_pct": round(100 * size / sample_bytes, 2) if sample_bytes else 0,
            "est_single_core_s_all_scores": round(total_svg / (sample_bytes / secs), 0) if secs else 0,
        })
    result = {"name": "brotli-sweep", "sample_files": [str(p.relative_to(SITE_ROOT)) for p in pages],
              "sample_mb": round(sample_bytes / 1e6, 1), "all_scores_mb": round(total_svg / 1e6, 1),
              "rows": rows}
    (out / "brotli-sweep.json").write_text(json.dumps(result, indent=2))
    return result


# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------


def summary_md(facts: dict, results: list[dict]) -> str:
    L = [f"# Build profile — {facts['when']}", ""]
    L += [f"- host `{facts['host']}`, {facts['cpu']}, {facts['threads']} threads, "
          f"kernel {facts['kernel']}",
          f"- governor `{facts['governor']}`, EPP `{facts['epp']}`, platform profile "
          f"`{facts['platform_profile']}`, power {facts['power_supply_online']}",
          f"- uptime at start {facts['uptime_s']:.0f} s, load {facts['loadavg']}",
          f"- sessions: {facts['sessions'] or 'none (before login)'}",
          f"- git {facts['git_head']}, {facts['git_dirty']} uncommitted path(s), "
          f"network {'up' if facts['network'] else 'DOWN'}",
          f"- _site {facts['site_bytes'] / 1e6:.0f} MB in {facts['site_files']} files; "
          f"score pages {facts['score_svg_bytes'] / 1e6:.0f} MB in {facts['score_svg_files']} files",
          ""]
    L += ["## Overview", "", "| scenario | wall s | exit | user s | sys s | peak RSS MB |",
          "|---|---:|---:|---:|---:|---:|"]
    for r in results:
        if r["name"] == "brotli-sweep":
            continue
        tv = r.get("time_v", {})
        rss = int(tv.get("max_rss_kb", 0) or 0) // 1024
        L.append(f"| {r['name']} | {r['wall_s']} | {r['exit']}{' (timeout)' if r['timed_out'] else ''} "
                 f"| {tv.get('user_s', '')} | {tv.get('sys_s', '')} | {rss} |")
    for r in results:
        L += ["", f"## {r['name']}", ""]
        if r["name"] == "brotli-sweep":
            L += [f"Eight largest score pages, {r['sample_mb']} MB; all score pages "
                  f"{r['all_scores_mb']} MB.", "",
                  "| codec | s | MB/s | size % | est. single-core s, all scores |",
                  "|---|---:|---:|---:|---:|"]
            L += [f"| {x['codec']} | {x['seconds']} | {x['mb_per_s']} | {x['ratio_pct']} "
                  f"| {x['est_single_core_s_all_scores']:.0f} |" for x in r["rows"]]
            continue
        L += [f"`{r['command']}` — {r['wall_s']} s, exit {r['exit']}", "",
              "| stage | s | % | busy % | iowait % | write MB/s | top programs (CPU s) |",
              "|---|---:|---:|---:|---:|---:|---|"]
        for s in r["stages"]:
            top = ", ".join(f"{k} {v}" for k, v in s["top"].items())
            L.append(f"| {s['stage']} | {s['seconds']} | {s['share_pct']} | {s['busy_pct']} "
                     f"| {s['iowait_pct']} | {s['write_mb_s']} | {top} |")
        L += ["", "Longest silences (the line before each is what was running):", ""]
        L += [f"- {g['seconds']} s at {g['at_s']} s, after: `{g['after']}`" for g in r["gaps"][:8]]
        L += ["", "CPU seconds by program over the whole run: "
              + ", ".join(f"{k} {v}" for k, v in r["cpu_s_by_comm"].items())]
    return "\n".join(L) + "\n"


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--scenarios", default=",".join(SCENARIOS))
    ap.add_argument("--list", action="store_true")
    ap.add_argument("--timeout-min", type=int, default=40,
                    help="per scenario (default 40)")
    ap.add_argument("--wait-network", type=int, default=120,
                    help="seconds to wait for DNS before starting (default 120)")
    args = ap.parse_args(argv)

    if args.list:
        for name, (desc, _) in SCENARIOS.items():
            print(f"{name:13} {desc}")
        return 0
    chosen = [s.strip() for s in args.scenarios.split(",") if s.strip()]
    unknown = [s for s in chosen if s not in SCENARIOS]
    if unknown:
        ap.error(f"unknown scenario(s): {', '.join(unknown)}")

    out = OUT_ROOT / dt.datetime.now().strftime("%Y%m%d-%H%M%S")
    out.mkdir(parents=True)
    print(f"profile-build: writing to {out}", flush=True)
    if args.wait_network and not wait_for_network(args.wait_network):
        print("profile-build: no network; code-refs and archive fetches will fail fast",
              flush=True)
    facts = system_facts()
    (out / "system.json").write_text(json.dumps(facts, indent=2))

    results = []
    for name in chosen:
        print(f"profile-build: — {name}: {SCENARIOS[name][0]}", flush=True)
        if name == "brotli-sweep":
            results.append(brotli_sweep(out))
        else:
            results.append(run_scenario(name, SCENARIOS[name][1], out, args.timeout_min * 60))
        # Written after every scenario, so a run cut short still reports.
        (out / "summary.json").write_text(json.dumps({"system": facts, "results": results}, indent=2))
        (out / "summary.md").write_text(summary_md(facts, results))

    latest = OUT_ROOT / "latest"
    if latest.is_symlink() or latest.exists():
        latest.unlink()
    latest.symlink_to(out.name)
    print(f"profile-build: done — {out / 'summary.md'}", flush=True)
    return 0 if all(r.get("exit", 0) == 0 for r in results) else 1


if __name__ == "__main__":
    sys.exit(main())
