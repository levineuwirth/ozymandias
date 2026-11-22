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

A file is given its name by a step that fails where a file already is (a
hard link, or a rename that refuses an existing name: `place`), so that a
file the author saves while an encoder runs is never written over; where
the filesystem has no such step, the tools write no image. To replace its
own file a tool takes it aside first, into a `.tmp` directory beside it
that says which file it was, and checks that it is the one it wrote. A run
stopped there leaves it aside, and the next run of a tool puts it back
before it judges anything (`Records.restore_displaced`): it looks in the
directories where the images are now, whatever the record says of where
they were, and touches there only a directory that is exactly as a run
leaves one. `adopt` and `declare` correct the record and move no file, so
they wait: with a file aside, the record is of files that are not in
their places.

A run stopped after it put its file in place and before it saved the
record leaves a file whose entry has other bytes. No run writes over that,
as none writes over a file its author changed. `make adopt-images` tells
the two apart: an output that is, byte for byte, what the tool makes of
its image now is the tool's, and its entry is brought up to it.

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

import ctypes
import errno
import fcntl
import hashlib
import importlib.util
import json
import os
import re
import sys
import tempfile
from collections.abc import Iterable
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
# network mounts), and what renameat2(2) says where it cannot be told to
# leave an existing file alone.
NO_HARD_LINKS = {errno.EPERM, errno.ENOTSUP, errno.EOPNOTSUPP, errno.ENOSYS}
NO_SUCH_RENAME = {errno.EINVAL, errno.ENOTSUP, errno.EOPNOTSUPP, errno.ENOSYS}

# What a tool leaves beside an image while it replaces its own file there
# (Records.publish): a directory `.previous-image-KEY-XXXXXXXX.tmp`, KEY of
# the file's name (`aside_key`). It is made first, and is empty of any
# file taken aside until the file is moved into it, as DISPLACED, its name
# written beside it as NAMED before that. So one without DISPLACED took
# nothing aside, whatever else a stopped run left in it.
ASIDE = ".previous-image-"
RESERVED = re.compile(r"\.previous-image-([0-9a-f]{16})-[a-z0-9_]{8}\.tmp")
DISPLACED, NAMED, PROBES = "displaced.tmp", "name.tmp", ("probe.tmp", "probe-2.tmp")


class NoSafePlace(OSError):
    """The filesystem can give a file a name only by a step that would
    replace a file already there. No image is put in such a place."""


def _rename_unless_there(held: Path, output: Path) -> None:
    """rename(2) that fails where `output` exists (Linux renameat2 with
    RENAME_NOREPLACE): one step, as a hard link is."""
    renameat2 = None
    if sys.platform.startswith("linux"):
        try:
            renameat2 = ctypes.CDLL(None, use_errno=True).renameat2
        except (OSError, AttributeError):
            pass
    if renameat2 is not None:
        renameat2.argtypes = [ctypes.c_int, ctypes.c_char_p, ctypes.c_int, ctypes.c_char_p, ctypes.c_uint]
        renameat2.restype = ctypes.c_int
        at_cwd, noreplace = -100, 1
        if renameat2(at_cwd, os.fsencode(os.path.abspath(held)),
                     at_cwd, os.fsencode(os.path.abspath(output)), noreplace) == 0:
            return
        error = ctypes.get_errno()
        if error == errno.EEXIST:
            raise FileExistsError(error, os.strerror(error), str(output))
        if error not in NO_SUCH_RENAME:
            raise OSError(error, os.strerror(error), str(output))
    raise NoSafePlace(errno.ENOTSUP, "this filesystem has neither hard links nor a rename that leaves an existing "
                      "file alone, so a file saved here meanwhile could be written over; no image is written "
                      "in this directory", str(output.parent))


def place(held: Path, output: Path) -> None:
    """Give the file at `held` the name `output` instead, if nothing has
    that name: FileExistsError otherwise, and what is there is not
    touched, nor `held`.

    Each way puts the whole file there in one step, or fails: a hard link
    (and `held` is then unlinked), or, where the filesystem has none, a
    rename that refuses an existing name. Where it has neither,
    NoSafePlace: a copy into the name would share a file with whoever
    saves there meanwhile, and nothing the tools write is worth that."""
    try:
        os.link(held, output, follow_symlinks=False)
    except FileExistsError:
        raise
    except OSError as error:
        if error.errno not in NO_HARD_LINKS:
            raise
        _rename_unless_there(held, output)
        return
    os.unlink(held)


_PLACEABLE: set[int] = set()


def can_place(reserved: Path) -> None:
    """NoSafePlace unless `place` works on this filesystem: asked, in the
    directory reserved for it, before a file is taken aside, which could
    not be put back otherwise. (Where a name is taken, link(2) says so
    before it says the filesystem has no links.)"""
    device = os.stat(reserved).st_dev
    if device in _PLACEABLE:
        return
    first, second = (reserved / name for name in PROBES)
    first.write_bytes(b"")
    try:
        place(first, second)
    finally:
        first.unlink(missing_ok=True)
        second.unlink(missing_ok=True)
    _PLACEABLE.add(device)


def _one_file(first: Path, second: Path) -> bool:
    """Are the two names there, and names of one file?"""
    try:
        one, other = os.lstat(first), os.lstat(second)
    except OSError:
        return False
    return (one.st_dev, one.st_ino) == (other.st_dev, other.st_ino)


def aside_key(output: Path) -> str:
    """What a file taken aside from `output` has in its name: enough to
    say which file of the directory it was, at any length of name."""
    return hashlib.sha256(os.fsencode(output.name)).hexdigest()[:16]


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
        author's is.

        The file is taken aside into a directory with a `.tmp` name, as
        every temporary file of the tools has (ignored by Git, and never
        published), made before the file is moved and saying which file it
        is for. A run stopped there leaves it, and the next run puts the
        file back before it decides anything (`restore_displaced`), so a
        file the author saved a moment before it was taken is not left
        where nobody looks; a directory with no file aside in it is a run
        stopped before it took one, and is cleared. If the file has to be
        kept (it may be the author's, and its name was taken by another
        save), it is given a name without `.tmp`, which the author sees.

        `place` says how a file is given its name, and where none is.
        """
        if self.declared_for(output) is not None:
            return False
        try:
            place(staged, output)
        except FileExistsError:
            if not self.owns(output, producer, source):
                return False
            reserved = Path(tempfile.mkdtemp(dir=output.parent, prefix=f"{ASIDE}{aside_key(output)}-",
                                             suffix=".tmp"))
            backup = reserved / DISPLACED
            try:
                can_place(reserved)
                (reserved / NAMED).write_bytes(os.fsencode(output.name))
                os.replace(output, backup)
            except FileNotFoundError:
                self._clear(reserved)
                return False
            except OSError:
                self._clear(reserved)
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
                        print(f"{show(output)} changed during publication. The displaced file is at "
                              f"{show(self._keep(reserved))}", file=sys.stderr)
                self._clear(reserved)
        self.wrote(output, producer, source, taken, written)
        self.save_all()
        return True

    @staticmethod
    def _keep(reserved: Path) -> Path:
        """Give the file aside in `reserved` a name its author sees (the
        directory's, less `.tmp`), and never one another file has. A run
        stopped between giving it that name and taking the other away
        left one file under both: the other name goes."""
        aside, kept = reserved / DISPLACED, reserved.with_suffix("")
        try:
            place(aside, kept)
        except FileExistsError:
            if not _one_file(aside, kept):
                return aside
            aside.unlink()
        return kept

    @staticmethod
    def _holds(reserved: Path) -> tuple[str, str | None]:
        """What is in a reserved directory, if it is as a run leaves one:
        ("reserved", None) with no file aside (nothing, or the empty
        files a run asks the filesystem with, or the name it was about to
        take a file from); ("aside", name) with a file aside and the name
        it had. ("other", None) for anything else: something there is
        not the tools', and nothing in it is theirs to move or delete."""
        key = RESERVED.fullmatch(reserved.name).group(1)
        name = None
        with os.scandir(reserved) as found:
            entries = {entry.name: entry for entry in found}
        if not set(entries) <= {DISPLACED, NAMED, *PROBES}:
            return "other", None
        for probe in PROBES:
            if probe in entries and not (entries[probe].is_file(follow_symlinks=False)
                                         and entries[probe].stat(follow_symlinks=False).st_size == 0):
                return "other", None
        if NAMED in entries:
            if not entries[NAMED].is_file(follow_symlinks=False):
                return "other", None
            name = os.fsdecode((reserved / NAMED).read_bytes())
            if name and (name in (".", "..") or os.sep in name or "\0" in name or aside_key(Path(name)) != key):
                return "other", None
        if DISPLACED not in entries:
            return "reserved", None
        # A file is moved there only once the name is written, and after
        # the filesystem was asked.
        if not name or any(probe in entries for probe in PROBES):
            return "other", None
        return "aside", name

    @classmethod
    def _clear(cls, reserved: Path) -> None:
        """Take a reserved directory away, if no file is aside in it and
        nothing is there but what a run leaves."""
        try:
            if cls._holds(reserved)[0] != "reserved":
                return
            for name in (NAMED, *PROBES):
                (reserved / name).unlink(missing_ok=True)
            os.rmdir(reserved)
        except OSError:
            pass

    def _left_aside(self, roots: Iterable[Path], deep: bool) -> list[tuple[Path, str | None]]:
        """What stopped runs left, at or under `roots` (the directories
        themselves alone, if not `deep`): each reserved directory, and
        each file an earlier form of the tools took aside (a file of a
        reserved directory's name, with the name it was taken from).

        Looked for where the images are, and not where the record says
        outputs were: a directory may have been renamed since, or be a
        link to another place now. A directory is looked into only if it
        is, where it really is, in a tree this tool holds: another
        tree's is another tool's to put back, under that tree's lock and
        by that tree's record. Only a name `publish` gives is taken for
        one; a file of such a name only if it says, as that earlier form
        did, that it was taken from an output the record has."""
        def failed(error: OSError) -> None:
            raise error

        found: list[tuple[Path, str | None]] = []
        recorded: dict[int, dict[str, str]] = {}
        for root in roots:
            if not os.path.isdir(root):
                continue
            for directory, subdirectories, files in os.walk(root, onerror=failed):
                reserved = sorted(name for name in subdirectories if RESERVED.fullmatch(name))
                earlier = sorted(name for name in files if RESERVED.fullmatch(name))
                subdirectories[:] = [name for name in subdirectories if name not in reserved] if deep else []
                if not reserved and not earlier:
                    continue
                real = Path(os.path.realpath(directory))
                try:
                    tree = self._tree(real / "any")
                except RecordError:
                    continue
                found += [(real / name, None) for name in reserved if not os.path.islink(real / name)]
                if earlier and id(tree) not in recorded:
                    recorded[id(tree)] = {aside_key(Path(rel)): os.path.basename(rel) for rel in tree.outputs}
                for name in earlier:
                    was = recorded[id(tree)].get(RESERVED.fullmatch(name).group(1))
                    if was is not None and not os.path.islink(real / name):
                        found.append((real / name, was))
        return found

    def pending(self, roots: Iterable[Path], *, deep: bool = True) -> list[Path]:
        """The files stopped runs took aside and nothing has put back, at
        or under `roots`. While there are any, the record is of files
        that are not in their places: `restore_displaced` first."""
        waiting = []
        for path, was in self._left_aside(roots, deep):
            if was is not None:
                waiting.append(path)
            elif os.path.lexists(path / DISPLACED):
                waiting.append(path / DISPLACED)
        return waiting

    def restore_displaced(self, roots: Iterable[Path], *, deep: bool = True) -> None:
        """Put back what a stopped run left aside (`publish`), before any
        output is judged or made again. `_left_aside` says where it is
        looked for.

        A file aside whose name is free goes back to it: the tool's own
        (the bytes the entry of that path has) is then its own as before,
        and any other is left as it is, and named: the author's, saved in
        the moment before it was taken, or the tool's in a directory the
        record does not know by that name. Where the name has a file
        again, the tool's own is deleted and any other is kept beside it
        under a name the author sees. A reserved directory with no file
        aside is cleared. One that is not as a run leaves it is left
        whole, and named if a file is aside in it.

        A file an earlier form of the tools took aside (one commit of
        them, which kept it as a file of the reserved name) is not moved:
        RecordError names it, for the author to put back or delete, and
        nothing is converted until then."""
        by_directory: dict[Path, list[tuple[bool, Path, Path]]] = {}
        earlier = []
        for path, was in self._left_aside(roots, deep):
            if was is not None:
                earlier.append((path, was))
                continue
            state, name = self._holds(path)
            if state == "reserved":
                self._clear(path)       # a run stopped before it took the file aside
            elif state == "other":
                if os.path.lexists(path / DISPLACED):
                    print(f"derived-images: {show(path / DISPLACED)} looks like a file set aside by a run that "
                          "was stopped, and its directory is not as a run leaves one. It is left there for you "
                          "to look at.", file=sys.stderr)
            else:
                output, aside = path.parent / name, path / DISPLACED
                entry = self.entry(output)
                own = (entry is not None and not aside.is_symlink() and aside.is_file()
                       and digest(aside) == entry["digest"])
                by_directory.setdefault(path.parent, []).append((own, path, output))
        for found in by_directory.values():
            # An author's file before the tool's own, where both are
            # aside from one name: the name is the author's then.
            for own, reserved, output in sorted(found, key=lambda one: one[0]):
                self._restore(reserved, output, own)
        if earlier:
            raise RecordError("\n  ".join(
                ["an earlier form of this tool took a file aside when it was stopped, and nothing has put it back:",
                 *(f"{show(path)}  (from {was}, in that directory)" for path, was in earlier),
                 "It may be the only copy of what you saved there. Move it to that name if it is, or delete it"]))

    def _restore(self, reserved: Path, output: Path, own: bool) -> None:
        aside = reserved / DISPLACED
        if _one_file(aside, output):
            aside.unlink()              # put back already, and stopped before this
            self._clear(reserved)
            return
        try:
            place(aside, output)
        except FileExistsError:
            if own:
                aside.unlink()
            else:
                print(f"derived-images: {show(output)} was set aside by a run that was stopped, and another file "
                      f"has its name since. The one set aside is at {show(self._keep(reserved))}", file=sys.stderr)
        else:
            if not own:
                print(f"derived-images: {show(output)} was set aside by a run that was stopped, and is back in "
                      "its place. The record does not say this tool wrote it there (it was saved in the moment "
                      "before, or its directory has another name now), so it is left as it is.", file=sys.stderr)
        self._clear(reserved)

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

    def stale(self, output: Path, producer: str, source: Path) -> bool:
        """Is a file there, and not the bytes its entry has, where the
        entry is this tool's, of this image, and the author declared
        nothing? An author's edit of the tool's file is that, and so is
        what a run leaves when it is stopped after it put its file in
        place and before it saved the record. No run writes over either.
        An adoption tells them apart, by making the output again
        (`make adopt-images`)."""
        entry, tree = self.entry(output), self._tree(output)
        return (entry is not None and entry["producer"] == producer and self.declared_for(output) is None
                and tree.within(source) and entry["source"] == tree.rel(source)
                and not os.path.islink(output) and os.path.isfile(output) and entry["digest"] != digest(output))

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


def waiting(records: Records, roots: list[Path], tool: str, nothing: str, *, deep: bool = True) -> bool:
    """Say which files a stopped run took aside and nothing has put back,
    if any: what the record says of their names is then of files that are
    not there, and a tool that only reads and corrects the record would
    correct it wrongly."""
    aside = records.pending(roots, deep=deep)
    if aside:
        print(f"{tool}: a run that was stopped took {len(aside)} file(s) aside, and they are not back in their "
              "places:", file=sys.stderr)
        for path in aside[:5]:
            print(f"  {show(path)}", file=sys.stderr)
        if len(aside) > 5:
            print(f"  … and {len(aside) - 5} more", file=sys.stderr)
        print(f"  {nothing}. The next conversion puts them back (make convert-images, or a build); "
              "then run this again.", file=sys.stderr)
    return bool(aside)


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
        if waiting(records, [webp.parent], "declare", "Nothing was declared", deep=False):
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
        # Before an entry is dropped or corrected: one whose file a
        # stopped run took aside would be dropped for a file that is
        # coming back, or corrected for a file that is about to be
        # replaced by the one aside.
        if waiting(records, converter.image_roots()[2], "derived-images", "Nothing was adopted"):
            return 1
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
    except (RecordError, OSError) as error:
        print(f"derived-images: {error}", file=sys.stderr)
        return 1
    print("usage: derived_images.py declare WEBP [IMAGE] | derived_images.py adopt [--dry-run] | "
          "derived_images.py status", file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
