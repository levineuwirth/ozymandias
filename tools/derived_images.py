"""The images the tools made, and so may write over: the ownership record.

Two tools write image files beside the author's own: tools/convert-images.sh
(a WebP companion, `lake.webp` beside `lake.jpg`) and
tools/generate-thumbnails.py (a photograph's variants, `view.w480.jpg` and
`view.w480.webp` beside `view.jpg`). Their outputs have names an author may
give a file too, and nothing in a file says who wrote it. So each tool
records what it writes, and writes over or deletes only what it recorded:

    data/derived-images.json
    {"version": 1,
     "outputs": {"content/blog/lake.webp":
                   {"producer": "convert-images",
                    "source": "content/blog/lake.jpg",
                    "digest": "sha256:…"}}}

An output is bound to the tool that made it, the image it was made from,
its own path, and the bytes last written there. A file with no entry, an
entry of another tool or another image, or other bytes than the entry's (an
author edited it, or put a file of their own there) is not the tool's: it is
left exactly as it is, by a write, by `--force`, and by `--prune`. The one
case in which the image may differ is an output whose recorded source is
gone: it is still the tool's own bytes, and nothing else's.

The generator reads the same record (build/DerivedImages.hs): a page shows
an image with a companion, and the blog publishes one, only when the record
says the companion was made from that image and the bytes are still those,
or when the author declared it:

    data/companions.json          (the author's, hand-written or `declare`)
    {"content/blog/moving.webp": "content/blog/moving.gif"}

A declared companion is the author's file, shown with the image named, and
no tool writes to it.

Each tree has its own record, at data/derived-images.json under its root:
the site's (content/, static/) and the engine's (its static/), whose images
every site that links the engine shares. Paths in a record are relative to
its tree's root. The records are local state, like the files they describe
(a site that keeps its companions out of Git keeps the record out too).

Files that were there before the record (every companion of a site that
upgrades) are unrecorded, and so are not used until they are adopted:
`make adopt-images` makes each one again in a scratch place and records
those that come out byte for byte the same, which only the tool's own
output does; the rest are listed and left for the author to look at.

    python3 engine/tools/derived_images.py declare WEBP [IMAGE]
    python3 engine/tools/derived_images.py status
"""

from __future__ import annotations

import hashlib
import json
import os
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import sitelib  # noqa: E402

RECORD = Path("data/derived-images.json")
DECLARED = Path("data/companions.json")
VERSION = 1


class RecordError(Exception):
    """A record that cannot be read: the tool stops, since without it
    nothing says which files are its own."""


def digest(path: Path) -> str:
    """The bytes of a file, as the record names them."""
    sha = hashlib.sha256()
    with open(path, "rb") as handle:
        for block in iter(lambda: handle.read(1 << 20), b""):
            sha.update(block)
    return "sha256:" + sha.hexdigest()


def _load(path: Path) -> dict:
    try:
        text = path.read_text(encoding="utf-8")
    except FileNotFoundError:
        return {}
    except OSError as error:
        raise RecordError(f"{path}: {error}") from error
    try:
        document = json.loads(text)
    except ValueError as error:
        raise RecordError(f"{path}: not JSON ({error})") from error
    if not isinstance(document, dict):
        raise RecordError(f"{path}: not a JSON object")
    return document


class Tree:
    """One root and the record of the outputs under it."""

    def __init__(self, root: Path) -> None:
        self.root = Path(os.path.abspath(root))
        self.path = self.root / RECORD
        document = _load(self.path)
        if document and document.get("version") != VERSION:
            raise RecordError(f"{self.path}: version {document.get('version')!r}, and this tool reads {VERSION}")
        outputs = document.get("outputs", {})
        if not isinstance(outputs, dict) or not all(
                isinstance(entry, dict) and all(isinstance(entry.get(key), str)
                                                for key in ("producer", "source", "digest"))
                for entry in outputs.values()):
            raise RecordError(f"{self.path}: outputs are not producer, source and digest each")
        self.outputs: dict[str, dict[str, str]] = outputs
        self.dirty = False

    def holds(self, path: Path) -> bool:
        return os.path.abspath(path).startswith(str(self.root) + os.sep)

    def rel(self, path: Path) -> str:
        return Path(os.path.abspath(path)).relative_to(self.root).as_posix()

    def save(self) -> None:
        if not self.dirty:
            return
        document = {"version": VERSION, "outputs": dict(sorted(self.outputs.items()))}
        with sitelib.atomic_path(self.path, durable=False) as tmp:
            tmp.write_text(json.dumps(document, indent=1, ensure_ascii=True) + "\n", encoding="utf-8")
        self.dirty = False


class Records:
    """The records of the trees a tool writes in: the engine's and the
    site's, and one more for a directory outside both (a photograph being
    prepared elsewhere), which keeps its own."""

    def __init__(self, *, site: Path | None = None, engine: Path | None = None,
                 elsewhere: Path | None = None) -> None:
        site = Path(sitelib.SITE_ROOT if site is None else site)
        engine = Path(sitelib.ENGINE if engine is None else engine)
        # The engine first: inside a site (engine/ a directory) its files
        # are under the site's root too, and they are the engine's.
        roots = [engine] + ([site] if os.path.abspath(site) != os.path.abspath(engine) else [])
        if elsewhere is not None and not any(
                os.path.abspath(elsewhere).startswith(os.path.abspath(root) + os.sep)
                or os.path.abspath(elsewhere) == os.path.abspath(root) for root in roots):
            roots.append(elsewhere)
        self.trees = [Tree(root) for root in roots]
        self.site = next(tree for tree in self.trees if tree.root == Path(os.path.abspath(site)))
        declared = _load(self.site.root / DECLARED)
        if not all(isinstance(image, str) for image in declared.values()):
            raise RecordError(f"{self.site.root / DECLARED}: each WebP names one image")
        self._declared: dict[str, str] = declared

    def _tree(self, path: Path) -> Tree:
        for tree in self.trees:
            if tree.holds(path):
                return tree
        raise RecordError(f"{path}: in no tree whose images this tool records")

    def entry(self, output: Path) -> dict[str, str] | None:
        tree = self._tree(output)
        return tree.outputs.get(tree.rel(output))

    def declared_for(self, output: Path) -> str | None:
        """The image the author declared this WebP the companion of, if
        any: the file is then the author's, and no tool writes to it."""
        return self._declared.get(self.site.rel(output)) if self.site.holds(output) else None

    def made_by(self, output: Path, producer: str) -> bool:
        """Is this file an output `producer` recorded, whatever its bytes
        now? (A tool passes over another's outputs as inputs of its own.)"""
        entry = self.entry(output)
        return entry is not None and entry["producer"] == producer

    def owns(self, output: Path, producer: str, source: Path | None = None) -> bool:
        """May `producer` write over this file, or delete it? Only where
        the record says it wrote these very bytes there, from `source`
        (when one is given), or from an image that is gone."""
        if self.declared_for(output) is not None:
            return False
        entry = self.entry(output)
        if entry is None or entry["producer"] != producer:
            return False
        if os.path.islink(output) or not os.path.isfile(output) or entry["digest"] != digest(output):
            return False
        tree = self._tree(output)
        if source is not None and tree.holds(source) and entry["source"] == tree.rel(source):
            return True
        return not os.path.lexists(tree.root / entry["source"])

    def may_write(self, output: Path, producer: str, source: Path) -> bool:
        """Is the place free, or the file there this tool's own?"""
        if self.declared_for(output) is not None:
            return False
        return not os.path.lexists(output) or self.owns(output, producer, source)

    def wrote(self, output: Path, producer: str, source: Path, *, save: bool = True) -> None:
        """Record a file just written, and save: a file on disk without
        its entry would no longer be the tool's. (An adoption, which
        writes no file, saves once at its end: `save_all`.)"""
        tree = self._tree(output)
        tree.outputs[tree.rel(output)] = {"producer": producer, "source": tree.rel(source),
                                          "digest": digest(output)}
        tree.dirty = True
        if save:
            tree.save()

    def save_all(self) -> None:
        for tree in self.trees:
            tree.save()

    def removed(self, output: Path) -> None:
        tree = self._tree(output)
        if tree.outputs.pop(tree.rel(output), None) is not None:
            tree.dirty = True
            tree.save()

    def forget_missing(self) -> None:
        """Drop the entries of outputs that are no longer there."""
        for tree in self.trees:
            gone = [rel for rel in tree.outputs if not os.path.lexists(tree.root / rel)]
            for rel in gone:
                del tree.outputs[rel]
            if gone:
                tree.dirty = True
                tree.save()


def request_full_rebuild() -> None:
    """Pages were built with what the record said then. Dropping the rules
    hash makes the next build a full one (tools/build-freshness.sh: its
    state may be deleted, and the build re-seeds it)."""
    (sitelib.SITE_ROOT / "data/rules-hash.txt").unlink(missing_ok=True)


def show(path: Path) -> str:
    """A path as the site's author knows it."""
    try:
        return Path(os.path.abspath(path)).relative_to(sitelib.SITE_ROOT).as_posix()
    except ValueError:
        return str(path)


IMAGE_SUFFIXES = (".jpg", ".jpeg", ".png", ".gif")


def declare(webp: Path, image: Path | None) -> int:
    site = Path(sitelib.SITE_ROOT)
    webp = Path(os.path.abspath(webp))
    if not webp.is_file() or webp.suffix != ".webp":
        print(f"declare: {show(webp)} is not a .webp file", file=sys.stderr)
        return 1
    if image is None:
        beside = sorted(p for p in webp.parent.iterdir()
                        if p.is_file() and p.suffix.lower() in IMAGE_SUFFIXES and p.name[:-len(p.suffix)] == webp.stem)
        if len(beside) != 1:
            print(f"declare: {show(webp)} has {len(beside)} images of its name beside it; name the one it is for",
                  file=sys.stderr)
            return 1
        image = beside[0]
    image = Path(os.path.abspath(image))
    if not image.is_file():
        print(f"declare: {show(image)} is not a file", file=sys.stderr)
        return 1
    for path in (webp, image):
        if not str(path).startswith(str(site) + os.sep):
            print(f"declare: {path} is not in the site", file=sys.stderr)
            return 1
    path = site / DECLARED
    declared = _load(path)
    declared[webp.relative_to(site).as_posix()] = image.relative_to(site).as_posix()
    with sitelib.atomic_path(path) as tmp:
        tmp.write_text(json.dumps(dict(sorted(declared.items())), indent=1, ensure_ascii=False) + "\n",
                       encoding="utf-8")
    request_full_rebuild()
    print(f"declare: {show(webp)} is the companion of {show(image)}, by hand; no tool writes to it.")
    return 0


def status() -> int:
    records = Records()
    for tree in records.trees:
        by: dict[str, int] = {}
        for entry in tree.outputs.values():
            by[entry["producer"]] = by.get(entry["producer"], 0) + 1
        made = ", ".join(f"{count} by {producer}" for producer, count in sorted(by.items())) or "none"
        print(f"{show(tree.path)}: {made}")
    print(f"{show(records.site.root / DECLARED)}: {len(records._declared)} declared by hand")
    return 0


def main(argv: list[str]) -> int:
    try:
        if argv[:1] == ["declare"] and len(argv) in (2, 3):
            return declare(Path(argv[1]), Path(argv[2]) if len(argv) == 3 else None)
        if argv == ["status"]:
            return status()
    except RecordError as error:
        print(f"derived-images: {error}", file=sys.stderr)
        return 1
    print("usage: derived_images.py declare WEBP [IMAGE] | derived_images.py status", file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
