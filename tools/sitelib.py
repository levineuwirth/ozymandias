"""Small helpers the site's Python tools share. Standard library only.

A tool imports this, and the other shared modules beside it (front_matter,
photo_naming, shared_rules, unpublished), after putting its own directory on
the path, which works whether it runs as a script or a test loads it by path:

    sys.path.insert(0, str(Path(__file__).resolve().parent))
    import sitelib  # noqa: E402
"""

from __future__ import annotations

import json
import os
import re
import subprocess
import sys
from collections.abc import Iterator
from contextlib import contextmanager
from functools import lru_cache
from pathlib import Path

def site_root() -> Path:
    """The site a tool works on: $SITE_ROOT, else the working directory,
    which is where the Makefile runs every tool. A tool's paths in the site
    and its questions to the generator (which reads the site's site.yaml)
    both start here."""
    return Path(os.environ.get("SITE_ROOT") or os.getcwd()).resolve()


# The site as the tool started, for tools that name their paths once. The
# engine is wherever these tools are: ENGINE holds tools/ (the site's
# engine/ since A6e), and its other files (templates, static assets,
# build/) are found through 'engine_path'.
SITE_ROOT = site_root()
ENGINE = Path(__file__).resolve().parent.parent


def engine_path(rel: str | Path) -> Path:
    """An engine file or directory by its path in the engine."""
    return ENGINE / rel


@contextmanager
def atomic_path(path: Path, *, durable: bool = True, mode: int | None = None) -> Iterator[Path]:
    """Yield a temporary sibling of `path` to write; when the block ends
    without an exception, one rename puts it in place, so an interrupted
    write never leaves a truncated file at `path`.

    The temporary is dot-prefixed (Hakyll and the site's copy rules skip
    it, so a build running alongside never publishes one), unique to the
    process (two runs cannot write into the same one), ends in `.tmp`
    (gitignored, and refused by the build and check-site), and is removed
    if the block fails. `durable` fsyncs the file before the rename and
    the directory after, so the rename survives a power loss; outputs
    regenerated on every run (sidecars, thumbnails, build stamps) turn it
    off for speed. `mode` sets the permissions before the rename.

    The block must not finish early without raising: a `return` inside it
    puts whatever was written in place.
    """
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(f".{path.name}.{os.getpid()}.tmp")
    try:
        yield tmp
        if durable:
            _fsync(tmp, os.O_RDONLY)
        if mode is not None:
            os.chmod(tmp, mode)
        os.replace(tmp, path)
        if durable:
            _fsync(path.parent, os.O_RDONLY | os.O_DIRECTORY)
    finally:
        tmp.unlink(missing_ok=True)


def _fsync(path: Path, flags: int) -> None:
    fd = os.open(path, flags)
    try:
        os.fsync(fd)
    finally:
        os.close(fd)


def atomic_write_bytes(path: Path, data: bytes, *, durable: bool = True,
                       skip_if_unchanged: bool = False) -> bool:
    """Write `data` to `path` through 'atomic_path'. With
    `skip_if_unchanged`, identical bytes are left alone so the file keeps
    its mtime (Hakyll judges an input changed by it). True if written."""
    if skip_if_unchanged:
        try:
            if path.read_bytes() == data:
                return False
        except OSError:
            pass
    with atomic_path(path, durable=durable) as tmp:
        tmp.write_bytes(data)
    return True


def atomic_write_text(path: Path, text: str, **kwargs) -> bool:
    """'atomic_write_bytes' for UTF-8 text."""
    return atomic_write_bytes(path, text.encode("utf-8"), **kwargs)


def slugify(text: str) -> str:
    """Lowercase, drop what is neither a word character, space nor hyphen,
    and join the words with single hyphens: "Rilke's Elegies" →
    "rilkes-elegies"."""
    s = re.sub(r"[^\w\s-]", "", text.lower())
    s = re.sub(r"[\s_]+", "-", s)
    s = re.sub(r"-+", "-", s)
    return s.strip("-")


@lru_cache(maxsize=None)
def site_binary(build: bool = True) -> str:
    """The generator, which the tools ask whatever it alone decides (drafts,
    shared lists, routes, tags, links), rebuilt first if a source changed;
    without `build`, the one last built, built only if there is none.
    SITE_BINARY names one instead: the tests run it over scratch trees that
    have no Haskell project to build."""
    if binary := os.environ.get("SITE_BINARY"):
        return binary

    # From the site's project, which builds its engine (`packages: engine`)
    # with the engine's dependency set: an engine checked out on its own has
    # no project. Inside a site (engine/ a directory, before A10b), cabal
    # finds the same project above the engine.
    project = SITE_ROOT if (SITE_ROOT / "cabal.project").is_file() else ENGINE

    def listed() -> str:
        return subprocess.check_output(
            ["cabal", "list-bin", "-v0", "exe:site"], cwd=project, text=True,
        ).strip()

    if not build and Path(found := listed()).is_file():
        return found
    subprocess.run(["cabal", "build", "-v0", "exe:site"], cwd=project, check=True)
    return listed()


@lru_cache(maxsize=None)
def site_config(build: bool = False) -> dict:
    """The site's site.yaml as the generator reads and checks it (`site
    config`): the identity a tool states (host, url, emails, the signing
    fingerprint) comes from here, not from the tool.

    The generator reads site.yaml as it runs, so the one last built
    answers, and asking compiles nothing unless `build` is set: the
    link-rot timer and the passphrase preset run outside the build lock,
    where a compile could race a build's."""
    return json.loads(subprocess.check_output([site_binary(build), "config"], cwd=site_root(), text=True))


# The engine's switches (switches.yaml): each one's kind, default and what
# it needs. The generator reads the same file (build/Config.hs).
REGISTRY = ENGINE / "switches.yaml"
# YAML 1.1's y and n, which the generator's YAML reader takes for booleans
# and PyYAML leaves as strings; its other spellings (yes/no, on/off) both
# read alike (test_config holds them to each other).
YAML11_BOOLS = {"y": True, "Y": True, "n": False, "N": False}


def switches(settings: dict | None = None) -> dict[str, bool]:
    """The site's switches as the generator resolves them
    (Config.resolveSwitches), without it: those its site.yaml sets (or
    `settings`, a site.yaml's contents), over the registry's defaults. A
    name the registry lacks is refused; what each needs is the generator's
    to refuse, so make and the tools need no compiled generator."""
    import yaml

    if settings is None:
        settings = yaml.safe_load((site_root() / "site.yaml").read_text(encoding="utf-8")) or {}
    registry = yaml.safe_load(REGISTRY.read_text(encoding="utf-8")) or {}
    chosen = {name: YAML11_BOOLS.get(value, value) if isinstance(value, str) else value
              for name, value in (settings.get("switches") or {}).items()}
    if unknown := sorted(set(chosen) - set(registry)):
        raise ValueError(f"site.yaml: switches not in {REGISTRY.name}: {', '.join(unknown)}")
    if not all(isinstance(value, bool) for value in chosen.values()):
        raise ValueError("site.yaml: each switch is true or false")
    return {name: chosen.get(name, spec["default"]) for name, spec in registry.items()}


def switch_on(name: str, settings: dict | None = None) -> bool:
    """Whether the site (or `settings`) has switch `name` on. A name the
    registry lacks raises KeyError: asking for one is a mistake."""
    resolved = switches(settings)
    if name not in resolved:
        raise KeyError(f"no switch {name} in {REGISTRY}")
    return resolved[name]


if __name__ == "__main__":
    # `sitelib.py config KEY` prints one value, for the shell tools
    # (site-paths.sh's site_config); `sitelib.py switch NAME` exits 0 when
    # the switch is on, 1 when it is off, and 2 when it cannot say (no such
    # switch, an unreadable site.yaml, a missing module), which make must not
    # read as off (engine.mk's switch_on).
    if len(sys.argv) == 3 and sys.argv[1] == "switch":
        try:
            on = switch_on(sys.argv[2])
        except Exception as error:   # noqa: BLE001 — any failure is "cannot say"
            print(f"sitelib: switch {sys.argv[2]}: {error!r}", file=sys.stderr)
            sys.exit(2)
        sys.exit(0 if on else 1)
    if len(sys.argv) != 3 or sys.argv[1] != "config":
        sys.exit("usage: sitelib.py config KEY | sitelib.py switch NAME")
    value = site_config().get(sys.argv[2])
    if not isinstance(value, str):
        sys.exit(f"site config: no {sys.argv[2]}")
    print(value)
