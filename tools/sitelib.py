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
    """An engine file or directory by its path in the engine: under engine/
    once A6 has moved it there, beside these tools, or, while A6 has moved
    these tools and not it, beside the site."""
    for found in (ENGINE / "engine" / rel, ENGINE / rel, SITE_ROOT / rel):
        if found.exists():
            return found
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

    # From the engine, wherever it is: cabal finds the cabal.project above
    # it, which names the engine's package (from A6b, `packages: engine`).
    def listed() -> str:
        return subprocess.check_output(
            ["cabal", "list-bin", "-v0", "exe:site"], cwd=ENGINE, text=True,
        ).strip()

    if not build and Path(found := listed()).is_file():
        return found
    subprocess.run(["cabal", "build", "-v0", "exe:site"], cwd=ENGINE, check=True)
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


if __name__ == "__main__":
    # `sitelib.py config KEY` prints one value, for the shell tools
    # (site-paths.sh's site_config).
    if len(sys.argv) != 3 or sys.argv[1] != "config":
        sys.exit("usage: sitelib.py config KEY")
    value = site_config().get(sys.argv[2])
    if not isinstance(value, str):
        sys.exit(f"site config: no {sys.argv[2]}")
    print(value)
