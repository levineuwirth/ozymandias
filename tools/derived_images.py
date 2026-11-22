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
                    "digest": "sha256:…",
                    "source-digest": "sha256:…",
                    "source-stamp": [mtime in ns, size]}}}

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

A declared companion is the author's file, shown with the image named and
with no other (whatever an entry of the record says of it), and no tool
writes to it. `declare` drops the file's entry, if it had one.

Each tree has its own record and its own declarations, under its root: the
site's (content/, static/) and the engine's (its static/), whose images
every site that links the engine shares. Paths in them are relative to the
tree's root, and name files of that tree only: a site cannot declare an
engine file, which the other sites would not know of (`declare` writes an
engine file's line in the engine's own data/companions.json). A hand-written
line is read as the file it leads to (`./static/lake.webp` is
`static/lake.webp`), and one that leads to another tree's file, the engine's
inside the site included, stops the tools. The records are local state,
like the files they describe (a site that keeps its companions out of Git
keeps the record out too).

A tool holds a tree's lock (on its data/ directory) from before it reads
the record until after it has saved it, so that two sites converting the
engine's images at once do not each save a record without the other's
entries: the second waits, and reads what the first wrote.

An entry also says which image the output was made from: the image's
bytes (source-digest), and its size and time then (source-stamp), which
spare reading the image while they are unchanged. The output is the
image's only while the image is still that one. When it is not (other
bytes, whichever file is newer; or another file than the entry names) the
tool makes the output again, and until it has (a build without cwebp), the
generator shows the image alone. An image whose time alone changed (a
checkout) is read once, found the same, and its entry's time brought up.
An image of the same size and the same time to the nanosecond is taken
for the same image without being read, as make and Git take a file.
An entry that says nothing of its image pairs nothing.

What an entry says of the image is said of the bytes the tool read, not of
the file as it is when the entry is written: an image saved again while it
was being converted has an output of the image before, and an entry that
says so (`consume`, `still`). The tool then makes the output again.

Files that were there before the record (every companion of a site that
upgrades) are unrecorded, and so are not used until they are adopted:
`make adopt-images` makes each one again in a scratch place and records
those that come out byte for byte the same, which only the tool's own
output does; the rest are listed and left for the author to look at. An
entry of an earlier form of the record, which does not say which bytes
its image had, is completed the same way.

    python3 engine/tools/derived_images.py declare WEBP [IMAGE]
    python3 engine/tools/derived_images.py adopt [--dry-run]     (make adopt-images)
    python3 engine/tools/derived_images.py status
"""

from __future__ import annotations

import errno
import fcntl
import hashlib
import importlib.util
import json
import os
import shutil
import stat as stat_module
import sys
import tempfile
from contextlib import contextmanager
from dataclasses import dataclass
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


def stamp(path: Path) -> list[int]:
    """An image's size and time, as an entry notes them."""
    stat = os.stat(path)
    return [stat.st_mtime_ns, stat.st_size]


@dataclass(frozen=True)
class Consumed:
    """An image as a tool read it: the bytes it made its output from, and
    which file they were."""
    data: bytes
    digest: str
    stamp: list
    file: tuple


def consume(source: Path) -> Consumed:
    """Read an image once, for the tool to work from and for the record to
    name: what is recorded as an output's image is then the image the
    output was made of, whatever is saved at its path meanwhile. A file
    written to as it is read is read again."""
    for _ in range(5):
        with open(source, "rb") as handle:
            before = os.fstat(handle.fileno())
            data = handle.read()
            after = os.fstat(handle.fileno())
        if (before.st_mtime_ns, before.st_size) == (after.st_mtime_ns, after.st_size) and len(data) == after.st_size:
            return Consumed(data, "sha256:" + hashlib.sha256(data).hexdigest(), [after.st_mtime_ns, after.st_size],
                            (after.st_dev, after.st_ino, after.st_mtime_ns, after.st_size))
    raise OSError(f"{source}: being written as it is read")


def still(source: Path, taken: Consumed) -> bool:
    """Is the file at this path the one that was read, unchanged?"""
    try:
        stat = os.stat(source)
    except OSError:
        return False
    return (stat.st_dev, stat.st_ino, stat.st_mtime_ns, stat.st_size) == taken.file


def digest_of(data: bytes) -> str:
    return "sha256:" + hashlib.sha256(data).hexdigest()


# What link(2) says on a filesystem that has no hard links (exFAT, some
# network mounts).
NO_HARD_LINKS = {errno.EPERM, errno.ENOTSUP, errno.EOPNOTSUPP, errno.ENOSYS}


def place(held: Path, output: Path) -> None:
    """Give the file at `held` the name `output` too, if nothing has that
    name: FileExistsError otherwise, and what is there is not touched.

    A hard link, which puts the whole file there in one step. Where the
    filesystem has none, a copy into a file created for it, and created
    only if the name is free: the rule holds, but a reader may see the
    file while it is being written, and a run killed then leaves part of
    one (no entry says it is the tool's, so nothing shows it, and the tool
    lists it as a file that is not its own)."""
    try:
        os.link(held, output, follow_symlinks=False)
        return
    except FileExistsError:
        raise
    except OSError as error:
        if error.errno not in NO_HARD_LINKS:
            raise
    if os.path.islink(held):
        os.symlink(os.readlink(held), output)
        return
    fd = os.open(output, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    made = os.fstat(fd)
    try:
        with os.fdopen(fd, "wb") as target, open(held, "rb") as origin:
            shutil.copyfileobj(origin, target)
            os.fchmod(target.fileno(), stat_module.S_IMODE(os.fstat(origin.fileno()).st_mode))
    except BaseException:
        # Part of a file, under a name this call made: taken away again,
        # unless another file has the name by now.
        try:
            there = os.lstat(output)
            if (there.st_dev, there.st_ino) == (made.st_dev, made.st_ino):
                os.unlink(output)
        except OSError:
            pass
        raise


class Tree:
    """One root, the record of the outputs under it, and its author's
    declarations."""

    def __init__(self, root: Path, *, lock: bool = False) -> None:
        self.root = Path(os.path.abspath(root))
        self.real = os.path.realpath(self.root)
        self.path = self.root / RECORD
        self.outputs: dict[str, dict] = {}
        self.declared: dict[str, str] = {}
        self.dirty = False
        self._lock: int | None = None
        # Locked before it is read: what is read is then what is there
        # until this tool has saved.
        if lock:
            self.lock()
        try:
            self.load()
        except BaseException:
            self.unlock()
            raise

    def load(self) -> None:
        document = _load(self.path)
        if document and document.get("version") != VERSION:
            raise RecordError(f"{self.path}: version {document.get('version')!r}, and this tool reads {VERSION}")
        outputs = document.get("outputs", {})
        if not isinstance(outputs, dict) or not all(
                isinstance(entry, dict) and all(isinstance(entry.get(key), str)
                                                for key in ("producer", "source", "digest"))
                for entry in outputs.values()):
            raise RecordError(f"{self.path}: outputs are not producer, source and digest each")
        declared: dict[str, str] = {}
        for webp, image in _load(self.root / DECLARED).items():
            for path in (webp, image):
                # A declaration names files of its own tree. One that leads
                # out of it (a site's line for an engine file, through the
                # site's engine link) would be honored by this site alone.
                if (not isinstance(path, str) or not path or os.path.isabs(path)
                        or not self.within(self.root / path)):
                    raise RecordError(f"{self.root / DECLARED}: {path!r} is not a file of this tree "
                                      "(an engine image's companion is declared in the engine)")
            # Kept as the path the tools ask by, however the line spells
            # it: `./static/lake.webp` protects static/lake.webp.
            webp, image = self.rel(self.root / webp), self.rel(self.root / image)
            if declared.setdefault(webp, image) != image:
                raise RecordError(f"{self.root / DECLARED}: {webp} is declared the companion of two images")
        self.outputs, self.declared, self.dirty = outputs, declared, False

    def within(self, path: Path) -> bool:
        """Is the file, where its directory really is, under this root?"""
        parent = os.path.realpath(os.path.dirname(os.path.abspath(path)))
        return parent == self.real or parent.startswith(self.real + os.sep)

    def rel(self, path: Path) -> str:
        """A file's path in this tree, whichever way it was reached (the
        engine's files through a site's engine link, too)."""
        parent = os.path.realpath(os.path.dirname(os.path.abspath(path)))
        return Path(os.path.relpath(os.path.join(parent, os.path.basename(path)), self.real)).as_posix()

    def lock(self) -> None:
        """Hold the tree, waiting for another tool to finish with it. The
        lock is on data/ itself: the record is replaced by a rename, and a
        lock on the file would stay with the one that was replaced."""
        (self.root / "data").mkdir(parents=True, exist_ok=True)
        self._lock = os.open(self.root / "data", os.O_RDONLY)
        try:
            fcntl.flock(self._lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            print(f"derived-images: waiting for another tool to finish with {self.root / RECORD}", file=sys.stderr)
            fcntl.flock(self._lock, fcntl.LOCK_EX)

    def unlock(self) -> None:
        if self._lock is not None:
            os.close(self._lock)
            self._lock = None

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
    prepared elsewhere), which keeps its own.

    A tool that writes opens them with `Records.held`, which locks each
    tree, the engine's first, reads the records under the locks, and saves
    them before letting go (or, for a preview, does not)."""

    def __init__(self, *, site: Path | None = None, engine: Path | None = None,
                 elsewhere: Path | None = None, lock: bool = False) -> None:
        site = Path(sitelib.SITE_ROOT if site is None else site)
        engine = Path(sitelib.ENGINE if engine is None else engine)
        # The engine first: inside a site (engine/ a directory) its files
        # are under the site's root too, and they are the engine's.
        roots = [engine] + ([site] if os.path.realpath(site) != os.path.realpath(engine) else [])
        if elsewhere is not None and not any(
                (os.path.realpath(elsewhere) + os.sep).startswith(os.path.realpath(root) + os.sep)
                for root in roots):
            roots.append(elsewhere)
        self.trees: list[Tree] = []
        try:
            for root in roots:
                self.trees.append(Tree(root, lock=lock))
            # Each declared file is its own tree's: with the engine inside
            # the site, an engine file is under the site's root too, and
            # the site's line for it would be the site's alone.
            for tree in self.trees:
                for path in [p for pair in tree.declared.items() for p in pair]:
                    if self._tree(tree.root / path) is not tree:
                        raise RecordError(f"{tree.root / DECLARED}: {path!r} is not a file of this tree "
                                          "(an engine image's companion is declared in the engine)")
        except BaseException:
            self.close()
            raise

    @classmethod
    @contextmanager
    def held(cls, *, save: bool = True, **trees):
        records = cls(lock=True, **trees)
        try:
            yield records
            if save:
                records.save_all()
        finally:
            records.close()

    def close(self) -> None:
        for tree in self.trees:
            tree.unlock()

    def _tree(self, path: Path) -> Tree:
        for tree in self.trees:
            if tree.within(path):
                return tree
        raise RecordError(f"{path}: in no tree whose images this tool records")

    def entry(self, output: Path) -> dict | None:
        tree = self._tree(output)
        return tree.outputs.get(tree.rel(output))

    def declared_for(self, output: Path) -> str | None:
        """The image the author declared this WebP the companion of, if
        any: the file is then the author's, and no tool writes to it."""
        tree = self._tree(output)
        return tree.declared.get(tree.rel(output))

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
        if source is not None and tree.within(source) and entry["source"] == tree.rel(source):
            return True
        return not os.path.lexists(tree.root / entry["source"])

    def may_write(self, output: Path, producer: str, source: Path) -> bool:
        """Is the place free, or the file there this tool's own?"""
        if self.declared_for(output) is not None:
            return False
        return not os.path.lexists(output) or self.owns(output, producer, source)

    def publish(self, staged: Path, output: Path, producer: str, source: Path,
                taken: Consumed, written: str) -> bool:
        """Publish a staged sibling without replacing an author's concurrent save.

        Link into an empty slot atomically. For a replacement, take the old
        file aside and verify the bytes actually displaced before installing
        the new one. Another save into the slot always wins.

        While the old file is aside, the name has no file. That is on
        purpose: a reader then (another site building with the engine's
        images) finds no companion and shows the image alone, where a
        replacement in one step would either write over a save made a
        moment before or put the new file, for that moment, where the
        author's is. A run killed there leaves the name empty, and the
        next run makes the file again.

        The file taken aside has a `.tmp` name, as every temporary file of
        the tools has: ignored by a site's Git, and never published. If it
        has to be kept (it may be the author's, and its name was taken by
        another save), it is given a name without `.tmp`, which the author
        sees.

        `place` says what is done where the filesystem has no hard links.
        """
        if self.declared_for(output) is not None:
            return False
        try:
            place(staged, output)
        except FileExistsError:
            if not self.owns(output, producer, source):
                return False
            fd, backup_name = tempfile.mkstemp(dir=output.parent, prefix=".previous-image-", suffix=".tmp")
            os.close(fd)
            backup = Path(backup_name)
            try:
                os.replace(output, backup)
            except FileNotFoundError:
                backup.unlink()
                return False
            except OSError:
                backup.unlink()
                raise

            try:
                entry = self.entry(output)
                if backup.is_symlink() or not backup.is_file() or digest(backup) != entry["digest"]:
                    return False
                try:
                    place(staged, output)
                except FileExistsError:
                    return False
                backup.unlink()
            finally:
                if os.path.lexists(backup):
                    try:
                        place(backup, output)
                    except FileExistsError:
                        # Both names may now hold an author's save. Keep both,
                        # the displaced one where its author will see it.
                        kept = backup.with_suffix("")
                        try:
                            place(backup, kept)
                        except FileExistsError:
                            kept = backup
                        else:
                            backup.unlink()
                        print(f"{show(output)} changed during publication. The displaced file is at "
                              f"{show(kept)}", file=sys.stderr)
                    else:
                        backup.unlink()
        self.wrote(output, producer, source, taken, written)
        self.save_all()
        return True

    def fresh(self, output: Path, producer: str, source: Path) -> bool:
        """Is the output there what `producer` would make of `source` now?
        By its entry, when it has one: the image is the one the output was
        made from, by its size and time, or else by its bytes (a checkout
        changes the time alone; the entry is then brought up to it). The
        dates say nothing of an image replaced by an older file, or by
        another file of another name. A file with no entry (not the
        tool's to write over in any case) goes by the dates, as it always
        did."""
        if not os.path.lexists(output):
            return False
        tree, entry = self._tree(output), self.entry(output)
        if entry is not None and entry["producer"] == producer:
            if not tree.within(source) or entry["source"] != tree.rel(source):
                return False
            now = stamp(source)
            if entry.get("source-stamp") == now:
                return True
            if "source-digest" in entry and entry["source-digest"] == digest(source):
                entry["source-stamp"] = now
                tree.dirty = True
                return True
            return False
        return os.stat(output).st_mtime >= os.stat(source).st_mtime

    def wrote(self, output: Path, producer: str, source: Path, taken: Consumed | None = None,
              written: str | None = None) -> None:
        """Enter a file just written (or, in an adoption, just found to be
        what the tool would write): made of the image as it was read
        (`taken`), and with the digest of the bytes written (`written`).
        Both are given by a tool, which knows them; what is at the two
        paths by now may be something else."""
        tree = self._tree(output)
        if taken is None:
            taken = consume(source)
        tree.outputs[tree.rel(output)] = {"producer": producer, "source": tree.rel(source),
                                          "digest": digest(output) if written is None else written,
                                          "source-digest": taken.digest, "source-stamp": list(taken.stamp)}
        tree.dirty = True

    def incomplete(self, output: Path, producer: str, source: Path) -> bool:
        """Is this the tool's own output of this image, by an entry that
        does not say which bytes the image had (a record of an earlier
        form)? An adoption completes it."""
        entry = self.entry(output)
        return (entry is not None and "source-digest" not in entry
                and self.declared_for(output) is None and self.owns(output, producer, source)
                and self._tree(output).within(source) and entry["source"] == self._tree(output).rel(source))

    def removed(self, output: Path) -> None:
        tree = self._tree(output)
        if tree.outputs.pop(tree.rel(output), None) is not None:
            tree.dirty = True

    def made_under(self, root: Path, producer: str) -> list[tuple[Path, Path]]:
        """The outputs `producer` recorded at or under `root`, each with
        the image its entry names (there or not)."""
        real = os.path.realpath(root)
        found = []
        for tree in self.trees:
            for rel, entry in tree.outputs.items():
                output = os.path.join(tree.real, rel)
                if entry["producer"] == producer and (output == real or output.startswith(real + os.sep)):
                    found.append((Path(output), Path(tree.real) / entry["source"]))
        return sorted(found)

    def forget_missing(self) -> None:
        """Drop the entries of outputs that are no longer there."""
        for tree in self.trees:
            gone = [rel for rel in tree.outputs if not os.path.lexists(tree.root / rel)]
            for rel in gone:
                del tree.outputs[rel]
            if gone:
                tree.dirty = True

    def save_all(self) -> None:
        for tree in self.trees:
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
    with Records.held() as records:
        # The line goes in the declarations of the tree the file is in: the
        # site's, or the engine's, which every site that links it reads.
        try:
            tree = records._tree(webp)
        except RecordError:
            tree = None
        # One tree for both, by the tree each is in: with the engine inside
        # the site, an engine image is under the site's root as well.
        try:
            same = tree is not None and records._tree(image) is tree
        except RecordError:
            same = False
        if not same:
            print(f"declare: {show(webp)} and {show(image)} are not files of one tree (the site, or the engine)",
                  file=sys.stderr)
            return 1
        path = tree.root / DECLARED
        declared = {**tree.declared, tree.rel(webp): tree.rel(image)}
        with sitelib.atomic_path(path) as tmp:
            tmp.write_text(json.dumps(dict(sorted(declared.items())), indent=1, ensure_ascii=False) + "\n",
                           encoding="utf-8")
        # The author's now, and no longer what a tool made of another
        # image: its entry goes (saved as the records are let go).
        records.removed(webp)
    request_full_rebuild()
    print(f"declare: {show(webp)} is the companion of {show(image)}, by hand (in {show(path)}); "
          "no tool writes to it.")
    return 0


def status() -> int:
    records = Records()
    for tree in records.trees:
        by: dict[str, int] = {}
        for entry in tree.outputs.values():
            by[entry["producer"]] = by.get(entry["producer"], 0) + 1
        made = ", ".join(f"{count} by {producer}" for producer, count in sorted(by.items())) or "none"
        print(f"{show(tree.path)}: {made}")
        print(f"{show(tree.root / DECLARED)}: {len(tree.declared)} declared by hand")
    return 0


def _tool(filename: str):
    """A tool beside this one, as a module (their names have hyphens)."""
    name = Path(filename).stem.replace("-", "_")
    if name in sys.modules:
        return sys.modules[name]
    spec = importlib.util.spec_from_file_location(name, Path(__file__).resolve().parent / filename)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


def adopt(dry_run: bool) -> int:
    """Record the files from before the record that the tools would make
    the same, byte for byte: the photographs' variants first, then the
    companions, which are not made of a file the first stage found to be a
    variant. One reading of the records serves both, so a preview shows
    what the adoption would do, and writes nothing at all."""
    thumbnails, converter = _tool("generate-thumbnails.py"), _tool("convert_images.py")
    with Records.held(save=not dry_run) as records:
        records.forget_missing()
        adopted = 0
        failed = False
        count, bad = thumbnails.adopt_all(records, dry_run=dry_run)
        adopted, failed = adopted + count, failed or bad
        count, bad = converter.adopt_all(records, dry_run=dry_run)
        adopted, failed = adopted + count, failed or bad
    if adopted and not dry_run:
        request_full_rebuild()
    return 1 if failed else 0


def main(argv: list[str]) -> int:
    try:
        if argv[:1] == ["declare"] and len(argv) in (2, 3):
            return declare(Path(argv[1]), Path(argv[2]) if len(argv) == 3 else None)
        if argv[:1] == ["adopt"] and argv[1:] in ([], ["--dry-run"]):
            return adopt(dry_run=bool(argv[1:]))
        if argv == ["status"]:
            return status()
    except RecordError as error:
        print(f"derived-images: {error}", file=sys.stderr)
        return 1
    print("usage: derived_images.py declare WEBP [IMAGE] | derived_images.py adopt [--dry-run] | "
          "derived_images.py status", file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
