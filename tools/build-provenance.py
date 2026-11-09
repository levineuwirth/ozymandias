#!/usr/bin/env python3
"""Capture revisions before compilation; never infer them from HEAD afterwards.

begin invalidates the previous successful record. stamp validates the captured
revisions and engine worktree before publishing that record. check is repeated
at the end of the build and immediately before each deployment transfer.
"""
import hashlib
import json
import os
import subprocess
import sys
from pathlib import Path

STATE = Path(os.environ.get("STATE_DIR", "data"))
PENDING = STATE / "build-provenance.pending.json"
RECORD = STATE / "build-provenance.json"


def git(root, *args):
    return subprocess.check_output(["git", "-C", str(root), *args], stderr=subprocess.PIPE)


def capture():
    result = {"site": git(".", "rev-parse", "HEAD").decode().strip(), "engine": None}
    if Path("engine").is_dir():
        head = git("engine", "rev-parse", "HEAD").decode().strip()
        dirty = git("engine", "status", "--porcelain", "--untracked-files=all", "--", ".")
        digest = hashlib.sha256(git("engine", "diff", "HEAD", "--binary", "--", "."))
        for name in git("engine", "ls-files", "--others", "--exclude-standard", "-z", "--", ".").split(b"\0"):
            if name:
                path = Path("engine") / os.fsdecode(name)
                digest.update(name + b"\0")
                digest.update(os.fsencode(os.readlink(path)) if path.is_symlink() else path.read_bytes())
        result["engine"] = {"commit": head, "dirty": bool(dirty), "worktree": digest.hexdigest()}
    return result


def write(path, value):
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(json.dumps(value, sort_keys=True) + "\n")
    tmp.replace(path)


def validate(path):
    if not path.exists():
        raise ValueError(f"no {path}: run a complete make build first")
    recorded = json.loads(path.read_text())
    if recorded != capture():
        raise ValueError("site or engine changed since the build began; rebuild before publishing")
    return recorded


def main(command):
    STATE.mkdir(parents=True, exist_ok=True)
    if command == "begin":
        for path in (RECORD,):
            path.unlink(missing_ok=True)
        write(PENDING, capture())
    elif command == "stamp":
        recorded = validate(PENDING)
        (STATE / "last-build-commit.txt").write_text(recorded["site"] + "\n")
        if engine := recorded["engine"]:
            (STATE / "last-build-engine.txt").write_text(engine["commit"] + "\n" + ("dirty\n" if engine["dirty"] else ""))
        write(RECORD, recorded)
    elif command == "check":
        validate(RECORD)
    else:
        raise ValueError("usage: build-provenance.py begin|stamp|check")


if __name__ == "__main__":
    try:
        main(sys.argv[1] if len(sys.argv) == 2 else "")
    except (ValueError, OSError, subprocess.CalledProcessError) as error:
        sys.exit(f"build provenance: {error}")
