#!/usr/bin/env python3
"""
import-content.py — Import content from external sources into the site.

Produces Markdown files under content/{type}/ from plain text, structured
data files, or existing Markdown files.

Stages:
  Reader → Splitter → Schema → Writer

All four stages implemented.
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from dataclasses import dataclass, field
from datetime import date as date_type, datetime
from pathlib import Path
from typing import Any, Callable

import yaml

sys.path.insert(0, str(Path(__file__).resolve().parent))
import shared_rules  # noqa: E402
import sitelib  # noqa: E402
import front_matter  # noqa: E402
from sitelib import slugify  # noqa: E402

# ---------------------------------------------------------------------------
# Types
# ---------------------------------------------------------------------------

@dataclass
class Document:
    body: str
    meta: dict[str, Any] = field(default_factory=dict)
    source_path: Path | None = None

class ContentImportError(ValueError):
    """Invalid source content or an unsupported import request."""


# ---------------------------------------------------------------------------
# Reader registry
# ---------------------------------------------------------------------------

ReaderFn = Callable[..., list[Document]]

_readers: dict[str, ReaderFn] = {}


def reader(name: str) -> Callable[[ReaderFn], ReaderFn]:
    def decorate(fn: ReaderFn) -> ReaderFn:
        _readers[name] = fn
        return fn
    return decorate


def get_reader(name: str) -> ReaderFn:
    fn = _readers.get(name)
    if fn is None:
        available = ", ".join(sorted(_readers))
        print(f"error: unknown reader {name!r} (available: {available})",
              file=sys.stderr)
        sys.exit(2)
    return fn


# ---------------------------------------------------------------------------
# Built-in readers
# ---------------------------------------------------------------------------

@reader("plain-text")
def read_plain_text(source: Path) -> list[Document]:
    """Read a single plain-text file as one document."""
    body = source.read_text(encoding="utf-8", errors="replace")
    return [Document(body=body, source_path=source)]




@reader("file-per-document")
def read_file_per_document(patterns: list[str]) -> list[Document]:
    """Read files matching glob patterns, one document per file.
    Each file's stem becomes meta['slug'], and frontmatter is parsed
    when the file starts with delimiter-only `---` lines."""
    docs: list[Document] = []
    for pattern in patterns:
        matched = sorted(Path().glob(pattern))
        if not matched:
            print(f"warning: no files matched {pattern!r}", file=sys.stderr)
        for path in matched:
            text = path.read_text(encoding="utf-8", errors="replace")
            meta: dict[str, Any] = {"slug": path.stem}
            body = text
            parts = front_matter.split(text)
            if parts is not None:
                try:
                    frontmatter = yaml.safe_load(parts[0])
                except yaml.YAMLError as exc:
                    raise ContentImportError(
                        f"{path}: invalid YAML frontmatter: {exc}"
                    ) from exc
                if frontmatter is None:
                    frontmatter = {}
                if not isinstance(frontmatter, dict):
                    raise ContentImportError(
                        f"{path}: frontmatter must be a YAML mapping"
                    )
                meta.update(frontmatter)
                body = parts[1]
            docs.append(Document(body=body, meta=meta, source_path=path))
    return docs


def _documents_from_records(
    raw: Any, source: Path, format_name: str,
) -> list[Document]:
    records = raw if isinstance(raw, list) else [raw]
    docs: list[Document] = []
    for index, record in enumerate(records):
        label = f"{format_name} entry {index}"
        if not isinstance(record, dict):
            raise ContentImportError(f"{label} must be a mapping")
        if not all(isinstance(key, str) for key in record):
            raise ContentImportError(f"{label} contains a non-string field name")
        meta = dict(record)
        body = meta.pop("body", "")
        if not isinstance(body, str):
            raise ContentImportError(
                f"{label} field 'body' must be a string, got "
                f"{type(body).__name__}"
            )
        docs.append(Document(body=body, meta=meta, source_path=source))
    return docs


@reader("yaml")
def read_yaml(source: Path) -> list[Document]:
    """Read a YAML file containing one mapping or a list of mappings."""
    raw = yaml.safe_load(source.read_text(encoding="utf-8"))
    return _documents_from_records(raw, source, "yaml")


@reader("json")
def read_json(source: Path) -> list[Document]:
    """Read a JSON file containing one object or a list of objects."""
    raw = json.loads(source.read_text(encoding="utf-8"))
    return _documents_from_records(raw, source, "json")


# ---------------------------------------------------------------------------
# Splitter registry
# ---------------------------------------------------------------------------

SplitterFn = Callable[[Document, dict[str, Any]], list[Document]]

_splitters: dict[str, SplitterFn] = {}


def splitter(name: str) -> Callable[[SplitterFn], SplitterFn]:
    def decorate(fn: SplitterFn) -> SplitterFn:
        _splitters[name] = fn
        return fn
    return decorate


def get_splitter(name: str) -> SplitterFn:
    fn = _splitters.get(name)
    if fn is None:
        available = ", ".join(sorted(_splitters))
        print(f"error: unknown splitter {name!r} (available: {available})",
              file=sys.stderr)
        sys.exit(2)
    return fn


# ---------------------------------------------------------------------------
# Built-in splitters
# ---------------------------------------------------------------------------

@splitter("none")
def split_none(doc: Document, kwargs: dict[str, Any]) -> list[Document]:
    """Pass through — return the document unchanged."""
    return [doc]


def _heading_title(line: str) -> str | None:
    title = line.lstrip("# \t")
    title = title.split(" {#")[0].split("{:")[0].strip()
    return title or None


@splitter("heading-1")
def split_heading_1(doc: Document, kwargs: dict[str, Any]) -> list[Document]:
    """Split on Markdown h1 headings (`# Title`). Each heading line becomes
    the split document's `title`; content before the first heading is kept
    as a preamble document without a title."""
    sections: list[tuple[str | None, list[str]]] = []
    current_title: str | None = None
    current_lines: list[str] = []
    found_heading = False

    for line in doc.body.splitlines():
        if line.startswith("# ") or line.startswith("#\t"):
            found_heading = True
            if current_lines:
                sections.append((current_title, current_lines))
            current_title = _heading_title(line)
            current_lines = []
        else:
            current_lines.append(line)
    if current_lines:
        sections.append((current_title, current_lines))

    if not found_heading:
        return [doc]

    result: list[Document] = []
    for title, lines in sections:
        body = "\n".join(lines).strip()
        if not body:
            continue
        meta = dict(doc.meta)
        meta["number"] = len(result) + 1
        if title:
            meta["title"] = title
        result.append(Document(body=body, meta=meta, source_path=doc.source_path))

    return result if result else [doc]


def compile_split_regex(pattern: str) -> re.Pattern[str]:
    if not pattern:
        raise ContentImportError("regex splitter requires --regex PATTERN")
    try:
        matcher = re.compile(pattern, re.MULTILINE)
    except re.error as exc:
        raise ContentImportError(
            f"invalid regex pattern {pattern!r}: {exc}"
        ) from exc
    if matcher.groups > 0:
        raise ContentImportError(
            "regex splitter does not support capture groups "
            "(use a non-capturing pattern)"
        )
    return matcher


@splitter("regex")
def split_regex(doc: Document, kwargs: dict[str, Any]) -> list[Document]:
    """Split on lines matching a regex pattern (non-capture only). Pass
    the pattern via `--regex PATTERN`. Matched delimiter lines are
    removed from the body; each segment between matches becomes a
    document."""
    matcher = kwargs.get("matcher")
    if matcher is None:
        pattern = kwargs.get("pattern", "")
        if not isinstance(pattern, str):
            raise ContentImportError("regex splitter pattern must be a string")
        matcher = compile_split_regex(pattern)

    parts = matcher.split(doc.body)
    if len(parts) <= 1:
        return [doc]

    result: list[Document] = []
    for segment in parts:
        body = segment.strip()
        if not body:
            continue
        meta = dict(doc.meta)
        meta["number"] = len(result) + 1
        result.append(Document(body=body, meta=meta,
                               source_path=doc.source_path))

    return result if result else [doc]


_PAGE_BREAK_RE = re.compile(
    r"(?m)^[ \t]*---[ \t]*(?:\r?\n|$)|\f"
)


@splitter("page-break")
def split_page_break(doc: Document, kwargs: dict[str, Any]) -> list[Document]:
    """Split on page-break markers: a line containing only `---` (with
    optional surrounding whitespace) or a form-feed character (`\\f`).
    The delimiter line is consumed."""
    parts = _PAGE_BREAK_RE.split(doc.body)
    if len(parts) <= 1:
        return [doc]

    result: list[Document] = []
    for segment in parts:
        body = segment.strip()
        if not body:
            continue
        meta = dict(doc.meta)
        meta["number"] = len(result) + 1
        result.append(Document(body=body, meta=meta,
                               source_path=doc.source_path))

    return result if result else [doc]


# ---------------------------------------------------------------------------
# Schema stage — augment Document.meta with frontmatter fields
# ---------------------------------------------------------------------------

# Default per-type profiles. These set sensible defaults for output
# directory, template, and author field name.  Overridable via --field.
TYPE_PROFILES: dict[str, dict[str, Any]] = {
    "fiction": {
        "output_dir": "content/fiction",
        "author_field": "authors",
        "body_hard_lines": True,
    },
    "essay": {
        "output_dir": "content/essays",
        "author_field": "authors",
        "body_hard_lines": False,
    },
    "blog": {
        "output_dir": "content/blog",
        "author_field": "authors",
        "body_hard_lines": False,
    },
    "page": {
        "output_dir": "content",
        "author_field": "authors",
        "body_hard_lines": False,
    },
    "poetry": {
        "output_dir": "content/poetry",
        "author_field": "poet",
        "body_hard_lines": True,
    },
}


def auto_abstract(body: str, max_chars: int = 200) -> str:
    """Best-effort abstract from the first paragraph."""
    para: list[str] = []
    for line in body.splitlines():
        stripped = line.strip()
        if not stripped and para:
            break
        if stripped:
            para.append(stripped)
    text = " ".join(para)
    if len(text) <= max_chars:
        return text
    if max_chars < 3:
        return text[:max_chars]

    suffix = " …"
    limit = max_chars - len(suffix)
    cut = text.rfind(" ", 0, limit + 1)
    if cut <= 0:
        cut = limit
    return text[:cut].rstrip() + suffix


WRITING_TYPES = frozenset({"essay", "blog", "fiction", "poetry"})


def _document_label(doc: Document, index: int) -> str:
    source = f" from {doc.source_path}" if doc.source_path else ""
    return f"document {index + 1}{source}"


def _validate_document_fields(doc: Document, index: int) -> None:
    label = _document_label(doc, index)
    if not isinstance(doc.body, str):
        raise ContentImportError(
            f"{label}: body must be a string, got {type(doc.body).__name__}"
        )

    for key in ("title", "abstract", "slug", "collection", "poet"):
        value = doc.meta.get(key)
        if value is not None and not isinstance(value, str):
            raise ContentImportError(
                f"{label}: field {key!r} must be a string, got "
                f"{type(value).__name__}"
            )

    for key in ("authors", "tags"):
        value = doc.meta.get(key)
        if value is None:
            continue
        if (not isinstance(value, list)
                or not all(isinstance(entry, str) for entry in value)):
            raise ContentImportError(
                f"{label}: field {key!r} must be a list of strings"
            )

    number = doc.meta.get("number")
    if (number is not None
            and (not isinstance(number, int) or isinstance(number, bool)
                 or number < 1)):
        raise ContentImportError(
            f"{label}: field 'number' must be a positive integer"
        )


def _normalize_document_date(
    doc: Document, index: int, required: bool,
) -> None:
    label = _document_label(doc, index)
    value = doc.meta.get("date")
    if value is None or value == "":
        if required:
            raise ContentImportError(
                f"{label}: a date in YYYY-MM-DD format is required"
            )
        doc.meta.pop("date", None)
        return

    if isinstance(value, datetime):
        value = value.date()
    if isinstance(value, date_type):
        normalized = value.isoformat()
    elif isinstance(value, str):
        normalized = value.strip()
    else:
        raise ContentImportError(
            f"{label}: field 'date' must use YYYY-MM-DD format"
        )

    if not re.fullmatch(r"\d{4}-\d{2}-\d{2}", normalized):
        raise ContentImportError(
            f"{label}: field 'date' must use YYYY-MM-DD format"
        )
    try:
        date_type.fromisoformat(normalized)
    except ValueError as exc:
        raise ContentImportError(
            f"{label}: invalid date {normalized!r}"
        ) from exc
    doc.meta["date"] = normalized


def _infer_title(body: str) -> tuple[str | None, str]:
    lines = body.splitlines()
    first_index = next(
        (index for index, line in enumerate(lines) if line.strip()), None
    )
    if first_index is None:
        return None, body

    first_line = lines[first_index].strip()
    title: str | None = None
    if first_line.startswith("# ") or first_line.startswith("#\t"):
        title = _heading_title(first_line)
    else:
        followed_by_blank = (
            first_index + 1 < len(lines)
            and not lines[first_index + 1].strip()
        )
        if (followed_by_blank and len(first_line) < 80
                and not first_line.endswith((".", ",", ";", ":", "!", "?"))):
            title = first_line

    if title is None:
        return None, body
    lines.pop(first_index)
    return title, "\n".join(lines).strip()


def apply_schema(docs: list[Document], args: argparse.Namespace) -> list[Document]:
    """Augment each document's meta with inferred fields and CLI overrides."""
    profile = TYPE_PROFILES.get(args.type, TYPE_PROFILES["page"])

    for index, doc in enumerate(docs):
        _validate_document_fields(doc, index)
        body = doc.body.strip()

        title_prefix = getattr(args, "title_prefix", None) or ""
        has_title = bool(doc.meta.get("title"))

        if title_prefix and doc.meta.get("number") is not None:
            doc.meta["title"] = f"{title_prefix} {doc.meta['number']}"
        elif not has_title:
            inferred_title, body = _infer_title(body)
            doc.meta["title"] = inferred_title or "Untitled"

        if "abstract" not in doc.meta:
            doc.meta["abstract"] = auto_abstract(body)

        if args.date is not None:
            doc.meta["date"] = args.date

        if args.tags is not None:
            doc.meta["tags"] = [
                tag.strip() for tag in args.tags.split(",") if tag.strip()
            ]

        if args.author is not None:
            author_field = profile["author_field"]
            doc.meta[author_field] = (
                args.author if author_field == "poet" else [args.author]
            )

        if hasattr(args, "field") and args.field:
            for key_value in args.field:
                if "=" not in key_value:
                    print(
                        f"warning: --field {key_value!r} is not key=value, "
                        "skipping",
                        file=sys.stderr,
                    )
                    continue
                key, value = key_value.split("=", 1)
                doc.meta[key] = value

        lines = body.splitlines()
        content_lines = [line for line in lines if line.strip()]
        if content_lines:
            indent = min(
                len(line) - len(line.lstrip()) for line in content_lines
            )
            lines = [
                line[indent:] if len(line) >= indent else line for line in lines
            ]

        normalized: list[str] = []
        blank_run = 0
        for line in lines:
            if not line.strip():
                blank_run += 1
                if blank_run <= 2:
                    normalized.append(line)
            else:
                blank_run = 0
                normalized.append(line)
        doc.body = "\n".join(normalized).strip()

        _validate_document_fields(doc, index)
        _normalize_document_date(
            doc, index, required=args.type in WRITING_TYPES,
        )

    return docs


# ---------------------------------------------------------------------------
# Writer stage — generate Markdown files with YAML frontmatter
# ---------------------------------------------------------------------------

INTERNAL_FIELDS = frozenset({
    "source_path", "delimiter",
})


def yaml_frontmatter(meta: dict[str, Any]) -> str:
    """Render meta dict as YAML frontmatter.  Strips internal fields,
    uses block scalar (`|`) for multi-line strings."""
    cleaned = {k: v for k, v in meta.items() if k not in INTERNAL_FIELDS}
    # Use yaml.dump with block style; sort_keys=False preserves insertion
    # order so title/date come first.
    raw = yaml.dump(cleaned, default_flow_style=False,
                    allow_unicode=True, sort_keys=False)
    return raw.strip()


def slugify_path(title: str, number: int | None = None) -> str:
    """Derive a filesystem-safe slug from a title, optionally prefixed
    with a zero-padded number for ordering."""
    base = slugify(title)
    if number is not None:
        base = f"{number:04d}-{base}"
    return base or "untitled"

def document_slug(doc: Document) -> str:
    title = doc.meta.get("title", "Untitled")
    slug_source = doc.meta.get("slug") or title
    return slugify_path(slug_source, doc.meta.get("number"))


def detect_collection_slug(
    docs: list[Document], cli_collection: str | None,
) -> str | None:
    """Determine one non-empty collection slug from CLI or document metadata."""
    values = (
        [cli_collection]
        if cli_collection
        else [
            doc.meta["collection"]
            for doc in docs
            if doc.meta.get("collection")
        ]
    )
    if not values:
        return None

    slugs = {slugify(value) for value in values}
    if "" in slugs:
        raise ContentImportError("collection name must contain a letter or number")
    if len(slugs) > 1:
        raise ContentImportError(
            "all documents in one import must use the same collection"
        )
    return slugs.pop()


def generate_collection_index(
    docs: list[Document], collection_name: str,
) -> str:
    """Generate a collection index.md with links to each document."""
    entries: list[str] = []
    for doc in sorted(docs, key=lambda item: item.meta.get("number") or 0):
        title = doc.meta.get("title", "Untitled")
        link_title = (
            title.replace("\\", "\\\\")
            .replace("[", "\\[")
            .replace("]", "\\]")
        )
        abstract = " ".join(doc.meta.get("abstract", "").split())
        abstract_line = f"  ·  {abstract[:120]}" if abstract else ""
        entries.append(
            f"- [{link_title}]({document_slug(doc)}.html){abstract_line}"
        )

    first = docs[0].meta if docs else {}
    index_meta: dict[str, Any] = {
        "title": collection_name,
        "abstract": f"{len(docs)} piece{'s' if len(docs) != 1 else ''}",
    }
    if first.get("date"):
        index_meta["date"] = first["date"]
    if first.get("tags"):
        index_meta["tags"] = first["tags"]

    authors = first.get("authors")
    if isinstance(authors, list):
        author = ", ".join(authors)
    else:
        author = first.get("poet", "")
    details: list[str] = []
    if author:
        details.append(f"*{author}*")
    if first.get("date"):
        details.append(str(first["date"]))

    detail_line = " · ".join(details)
    body_parts = [part for part in (detail_line, "\n".join(entries)) if part]
    body = "\n\n".join(body_parts)
    return f"---\n{yaml_frontmatter(index_meta)}\n---\n\n{body}\n"


FileEntry = tuple[Path, str, str]


def reserved_section_dirs() -> list[str]:
    """The content/ directories a page collection may not take, as the
    generator defines them (build/Patterns.hs, through
    tools/shared_rules.py), so the importer and the build cannot disagree
    about one."""
    return shared_rules.shared_rules()["reserved-sections"]


def validate_collection_request(
    docs: list[Document], args: argparse.Namespace,
) -> None:
    cli_collection = getattr(args, "collection", None)
    collection_slug = detect_collection_slug(docs, cli_collection)
    if (collection_slug and args.type == "page"
            and collection_slug in reserved_section_dirs()):
        raise ContentImportError(
            f"page collection slug {collection_slug!r} conflicts with a "
            "reserved content directory"
        )


def collection_prefix(type_name: str, collection_slug: str) -> str:
    """The URL path a collection's pages live under, without the leading
    slash: `<slug>/` for pages, `<section>/<slug>/` otherwise."""
    profile = TYPE_PROFILES.get(type_name, TYPE_PROFILES["page"])
    section_path = profile["output_dir"].removeprefix("content").strip("/")
    return "/".join(part for part in (section_path, collection_slug) if part) + "/"


def validate_collection_routes(
    docs: list[Document], args: argparse.Namespace,
) -> None:
    """Refuse a collection whose URL space already belongs to other pages.

    The build refuses two items on one route (build/RouteCheck.hs), and a
    collection always writes <prefix>index.html, which a tag page, a
    generated page (stats, build, authors, archive, bibliography) or a
    static directory under the same name already has. Asks the generator
    which routes exist (`site list-routes`, run against the tree in the
    current directory) rather than keeping a list of names; pages from
    the collection's own directory are a re-import, not a claim."""
    collection_slug = detect_collection_slug(docs, getattr(args, "collection", None))
    if not collection_slug:
        return
    prefix = collection_prefix(args.type, collection_slug)
    own_dir = TYPE_PROFILES.get(args.type, TYPE_PROFILES["page"])["output_dir"]
    own = f"{own_dir}/{collection_slug}/"
    try:
        listing = subprocess.run(
            [sitelib.site_binary(), "list-routes"], cwd=sitelib.site_root(),
            capture_output=True, text=True, check=True,
        ).stdout
    except (OSError, subprocess.CalledProcessError) as exc:
        detail = getattr(exc, "stderr", "") or str(exc)
        raise ContentImportError(
            f"could not list the site's routes: {detail.strip()}") from exc
    claims = sorted(
        f"/{route} ({source})"
        for route, _, source in (line.partition("\t") for line in listing.splitlines())
        if route.startswith(prefix) and not source.startswith(own)
    )
    if claims:
        shown = ", ".join(claims[:5]) + (f", and {len(claims) - 5} more" if len(claims) > 5 else "")
        raise ContentImportError(
            f"collection {collection_slug!r} would share /{prefix} with existing pages: "
            f"{shown}. Choose another --collection name."
        )


def assemble_file_entries(
    docs: list[Document], args: argparse.Namespace,
) -> list[FileEntry]:
    validate_collection_request(docs, args)
    profile = TYPE_PROFILES.get(args.type, TYPE_PROFILES["page"])
    base_dir = Path(profile["output_dir"])
    cli_collection = getattr(args, "collection", None)
    collection_slug = detect_collection_slug(docs, cli_collection)

    if collection_slug:
        collection_url = "/" + collection_prefix(args.type, collection_slug)
        for doc in docs:
            if cli_collection:
                doc.meta["collection"] = collection_slug
            else:
                doc.meta.setdefault("collection", collection_slug)
            doc.meta["collection-url"] = collection_url

    entries: list[FileEntry] = []
    destinations: dict[Path, str] = {}

    def add_entry(path: Path, content: str, label: str) -> None:
        previous = destinations.get(path)
        if previous is not None:
            raise ContentImportError(
                f"duplicate output destination {path}: {previous} and {label}"
            )
        destinations[path] = label
        entries.append((path, content, label))

    output_dir = base_dir / collection_slug if collection_slug else base_dir
    for index, doc in enumerate(docs):
        title = doc.meta.get("title", "Untitled")
        path = output_dir / f"{document_slug(doc)}.md"
        frontmatter = yaml_frontmatter(doc.meta)
        content = f"---\n{frontmatter}\n---\n\n{doc.body}\n"
        add_entry(path, content, f"document {index + 1} ({title})")

    if collection_slug and docs:
        collection_name = cli_collection or next(
            (
                doc.meta["collection"]
                for doc in docs
                if doc.meta.get("collection")
            ),
            collection_slug,
        )
        index_path = base_dir / collection_slug / "index.md"
        index_content = generate_collection_index(docs, collection_name)
        add_entry(index_path, index_content, "collection index")

    return entries


def write_docs(
    docs: list[Document],
    args: argparse.Namespace,
    file_entries: list[FileEntry] | None = None,
) -> int:
    """Write documents as Markdown files with YAML frontmatter."""
    entries = file_entries if file_entries is not None else assemble_file_entries(
        docs, args
    )
    written = 0
    site = sitelib.site_root()
    for path, content, _label in sorted(entries, key=lambda entry: entry[0]):
        if (site / path).exists() and not args.overwrite:
            print(f"  skip  {path}")
            continue
        (site / path).parent.mkdir(parents=True, exist_ok=True)
        (site / path).write_text(content, encoding="utf-8")
        print(f"  write {path}")
        written += 1
    return written


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Import content from external sources into the site.")
    parser.add_argument("source", nargs="*",
                        help="Source path(s) or glob pattern(s)")
    parser.add_argument("--reader", default="plain-text",
                        help="Input format reader (default: plain-text)")
    parser.add_argument("--list-readers", action="store_true",
                        help="List available readers and exit")
    parser.add_argument("--splitter", default="none",
                        help="Content splitting strategy (default: none)")
    parser.add_argument("--regex",
                        help="Regex pattern for the 'regex' splitter")
    parser.add_argument("--list-splitters", action="store_true",
                        help="List available splitters and exit")
    parser.add_argument("--type", default="page",
                        choices=sorted(TYPE_PROFILES),
                        help="Content type (default: page)")
    parser.add_argument("--date",
                        help="Publication date (ISO format, e.g. 2026-07-17)")
    parser.add_argument("--tags",
                        help="Comma-separated tags (e.g. 'fiction,short-story')")
    parser.add_argument("--author",
                        help="Author name (maps to 'poet' for poetry, 'authors' for others)")
    parser.add_argument("--field", action="append", default=[],
                        help="Arbitrary frontmatter field (repeatable, e.g. --field key=val)")
    parser.add_argument("--title-prefix",
                        help="Prefix for numbered titles (e.g. 'Chapter', 'Sonnet')")
    parser.add_argument(
        "--collection",
        help="Collection name (groups documents under a directory with index)",
    )
    parser.add_argument("--dry-run", action="store_true",
                        help="Show what would be written; write nothing")
    parser.add_argument("--overwrite", action="store_true",
                        help="Overwrite existing files")
    parser.add_argument("--dump", action="store_true",
                        help="Print parsed documents for debugging")
    return parser


def main(argv: list[str]) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)

    if args.list_readers:
        print("Available readers:")
        for name in sorted(_readers):
            doc = (_readers[name].__doc__ or "").strip().split("\n")[0]
            print(f"  {name:22s}  {doc}")
        return 0

    if args.list_splitters:
        print("Available splitters:")
        for name in sorted(_splitters):
            doc = (_splitters[name].__doc__ or "").strip().split("\n")[0]
            print(f"  {name:22s}  {doc}")
        return 0

    if not args.source:
        print(
            "error: source argument is required "
            "(use --list-readers or --list-splitters to see available options)",
            file=sys.stderr,
        )
        return 2

    reader_fn = get_reader(args.reader)
    try:
        if args.reader == "file-per-document":
            docs = reader_fn(args.source)
        else:
            if len(args.source) > 1:
                print(
                    f"error: {args.reader} reader expects a single source path",
                    file=sys.stderr,
                )
                return 2
            source = Path(args.source[0])
            if not source.exists():
                print(f"error: source not found: {source}", file=sys.stderr)
                return 2
            docs = reader_fn(source)
    except (
        ContentImportError,
        json.JSONDecodeError,
        OSError,
        yaml.YAMLError,
    ) as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 2

    if not docs:
        print("error: reader produced no documents", file=sys.stderr)
        return 2
    print(f"[reader] {len(docs)} document(s) from {args.reader}", file=sys.stderr)

    splitter_fn = get_splitter(args.splitter)
    splitter_kwargs: dict[str, Any] = {}
    try:
        if args.splitter == "regex":
            splitter_kwargs["matcher"] = compile_split_regex(args.regex or "")

        split_docs: list[Document] = []
        for doc in docs:
            split_docs.extend(splitter_fn(doc, splitter_kwargs))
    except ContentImportError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 2

    print(
        f"[splitter] {len(split_docs)} document(s) after {args.splitter}",
        file=sys.stderr,
    )

    try:
        schema_docs = apply_schema(split_docs, args)
        file_entries = assemble_file_entries(schema_docs, args)
        validate_collection_routes(schema_docs, args)
    except ContentImportError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 2

    print(
        f"[schema] {len(schema_docs)} document(s) after schema "
        f"(type={args.type})",
        file=sys.stderr,
    )

    if args.dump:
        for index, doc in enumerate(schema_docs):
            print(f"\n{'=' * 60}")
            print(f"Document {index}")
            print(f"  source: {doc.source_path}")
            print(f"  meta:   {json.dumps(doc.meta, indent=2, default=str)}")
            preview = doc.body[:300].rstrip()
            print(f"  body ({len(doc.body)} chars, preview):")
            for line in preview.splitlines()[:10]:
                print(f"    {line}")
            if len(doc.body) > 300:
                print(f"    … ({len(doc.body) - 300} more chars)")

    if args.dry_run:
        print(
            f"\n[DRY RUN] Would process {len(schema_docs)} document(s):",
            file=sys.stderr,
        )
        for path, _content, label in sorted(
            file_entries, key=lambda entry: entry[0]
        ):
            print(f"  {path}  ←  {label}")
        return 0

    try:
        written = write_docs(schema_docs, args, file_entries)
    except OSError as exc:
        print(f"error: could not write output: {exc}", file=sys.stderr)
        return 2

    if written:
        print(f"\n{written} file(s) written to content/", file=sys.stderr)
        print(
            "Next: review the generated files, then make clean && make build",
            file=sys.stderr,
        )
    else:
        print(
            "\nNothing written (all files exist; use --overwrite to replace)",
            file=sys.stderr,
        )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
