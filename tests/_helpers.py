"""What the test modules share: a tool loaded by path, the generator built
once per run, a clean environment for the scripts under test, stub
executables, and a script stopped mid-run.

Imported as `from tests._helpers import ...`, which works both under
`make test` (unittest discovery) and for one module run on its own.
"""

from __future__ import annotations

import importlib.util
import os
import shutil
import signal
import subprocess
import sys
import time
import unittest
from collections.abc import Iterable
from functools import lru_cache
from pathlib import Path
from types import ModuleType

# The site under test: $SITE_ROOT, else the working directory, from which
# `make test` runs. The engine is the one these tests belong to: ENGINE
# holds tests/, beside the engine's other files (tools, templates, static
# assets, build/), as tools/sitelib.py has it.
SITE_ROOT = Path(os.environ.get("SITE_ROOT") or os.getcwd()).resolve()
ENGINE = Path(__file__).resolve().parents[1]


def engine_path(rel: str | Path) -> Path:
    """An engine file or directory by its path in the engine."""
    return ENGINE / rel


def copy_engine_tree(rel: str, dest: Path, **copytree) -> Path:
    """Copy the directory `rel` (templates, tools) as a build sees it to
    `dest`: the engine's, then the site's over it, as the build's aliases
    let a site file win."""
    for source in dict.fromkeys((engine_path(rel), SITE_ROOT / rel)):
        if source.is_dir():
            shutil.copytree(source, dest, dirs_exist_ok=True, **copytree)
    return dest


def load_tool(filename: str, name: str | None = None, *, register: bool = False,
              site: bool = False) -> ModuleType:
    """tools/<filename> as a module: the engine's, or with `site` the site's
    own. The tools' names have hyphens, so they cannot be imported by name.
    `register` puts the module in sys.modules before running it, which a
    dataclass defined in the tool needs."""
    name = name or Path(filename).stem.replace("-", "_")
    spec = importlib.util.spec_from_file_location(name, (SITE_ROOT if site else ENGINE) / "tools" / filename)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    if register:
        sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


requires_cabal = unittest.skipUnless(shutil.which("cabal"), "cabal not on PATH")


@lru_cache(maxsize=1)
def site_binary() -> Path:
    """The generator, rebuilt first if any source changed; once per run,
    not once per test class."""
    done = subprocess.run(["cabal", "build", "-v0", "exe:site"], cwd=ENGINE,
                          capture_output=True, text=True)
    if done.returncode:
        raise AssertionError(f"`cabal build -v0 exe:site` failed:\n{done.stderr[-2000:]}")
    found = subprocess.run(["cabal", "list-bin", "-v0", "exe:site"], cwd=ENGINE,
                           capture_output=True, text=True, check=True)
    return Path(found.stdout.strip())


# Variables that `make deploy`, a locked build or a developer's shell can
# export, and that change what the scripts and tools under test do: the
# deploy's DEPLOY_* overrides made the deploy guard's refusal tests fail
# inside the very deploy they were meant to allow (40190c8).
_LEAKY_PREFIXES = ("DEPLOY_", "BORG_")
_LEAKY_NAMES = ("SITE_LOCK_HELD", "LOCK_TIMEOUT", "REQUIRE_WEBP", "SITE_ENV",
                "SITE_BINARY", "SITE_ROOT")


def script_env(path: Path | str | None = None, **overrides: str) -> dict[str, str]:
    """This process's environment without those variables, with `path`
    (a stub directory) put first on PATH, and `overrides` on top."""
    env = {k: v for k, v in os.environ.items()
           if not k.startswith(_LEAKY_PREFIXES) and k not in _LEAKY_NAMES}
    if path is not None:
        env["PATH"] = f"{path}{os.pathsep}{env.get('PATH', '')}"
    env.update(overrides)
    return env


def stub_bin(directory: Path, names: Iterable[str], script: str) -> Path:
    """Write `script` as an executable under each of `names` in
    `directory`, for a test to put in front of the real commands."""
    directory.mkdir(parents=True, exist_ok=True)
    for name in names:
        path = directory / name
        path.write_text(script)
        path.chmod(0o755)
    return directory


def interrupt_when(args: list[str], env: dict[str, str], marker: Path,
                   timeout: float = 10) -> bool:
    """Run `args` in its own process group, wait for `marker` to appear,
    then SIGTERM the whole group, as a killed service or a reboot would
    stop it. True if the marker appeared before `timeout`."""
    with subprocess.Popen(args, env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                          text=True, start_new_session=True) as process:
        try:
            deadline = time.monotonic() + timeout
            while not marker.exists() and time.monotonic() < deadline:
                time.sleep(0.02)
            return marker.exists()
        finally:
            os.killpg(process.pid, signal.SIGTERM)
            process.communicate(timeout=5)


def write_site_config(root: Path, **overrides) -> dict:
    """Explicit neutral identity for scratch builds that evaluate Site.rules."""
    import json

    config = {
        "title": "Example Site", "author": "Example Author",
        "author-sort": "Author, Example", "email": "author@example.org",
        "contact-email": "contact@example.org", "url": "https://example.org",
        "host": "example.org", "source-repo": "https://git.example.org/author/site",
        "source-branch": "main", "forge-host": "git.example.org",
        "content-license": "CC-BY-NC-SA-4.0", "code-license": "MIT", "feeds": [],
        "signing-fingerprint": "", "font-preloads": [], "seasons": [],
        "manifest": {"name": "Example Site", "short-name": "Example",
                     "description": "An example site.", "theme-color": "#16140f",
                     "background-color": "#16140f"},
        # The sections the engine has rules for, until A8's switches.
        "sections": [{"dir": d, "kind": k, "dingbat": None} for d, k in (
            ("essays", "Essay"), ("blog", "Post"), ("poetry", "Poem"), ("fiction", "Fiction"),
            ("music", "Composition"), ("photography", "Photo"))],
        "standalone": [],
        "portals": [{"name": "Notes", "tag": "notes"}],
        "nav-primary": [{"label": "Home", "href": "/"}],
        "library": {"shelf-cap": 4, "shelf-max": 5, "description": "Every shelf.",
                    "see-also": []},
    }
    config.update(overrides)
    # JSON is also YAML; no Python YAML defaults or schema reader.
    (root / "site.yaml").write_text(json.dumps(config), encoding="utf-8")
    return config
