"""A page's front matter, split as Hakyll splits it: a leading line of
three or more dashes opens the YAML, and the next line of as many dashes
or dots closes it (Hakyll.Core.Provider.Metadata). The tools and tests
read pages through this rather than each through its own pattern; one of
those split on any "---" and cut a value holding Pandoc's em dash
("title: A --- B") short, and none took a longer fence, which Hakyll does.

Slightly more lenient than Hakyll: trailing whitespace after the opening
fence is allowed, as build/Drafts.hs allows it.
"""

from __future__ import annotations

import json
import re

import yaml

_OPENING = re.compile(r"(-{3,})[ \t]*\r?\n")


def split(text: str) -> tuple[str, str] | None:
    """(YAML, body), or None for a page without front matter."""
    opening = _OPENING.match(text)
    if opening is None:
        return None
    closing = re.compile(rf"\r?\n[-.]{{{len(opening.group(1))}}}[ \t]*(?:\r?\n|\Z)")
    # From the opening line's own newline, so empty front matter closes.
    end = closing.search(text, opening.end() - 1)
    if end is None:
        return None
    return text[opening.end():end.start()], text[end.end():]


def load(text: str) -> dict:
    """The front matter as a mapping: empty when the page has none, or when
    its YAML is not a mapping. Invalid YAML raises yaml.YAMLError."""
    parts = split(text)
    if parts is None:
        return {}
    data = yaml.safe_load(parts[0])
    return data if isinstance(data, dict) else {}


# ---------------------------------------------------------------------------
# Writing: the importers and scaffolders build front matter line by line
# ---------------------------------------------------------------------------

_ISO_DATE = re.compile(r"\d{4}-\d{2}-\d{2}")

# Keys whose ISO date is written bare, as the site's front matter has them.
DATE_KEYS = ("date", "captured")


def scalar(value) -> str:
    """A YAML scalar for one value. Booleans and numbers stay bare; anything
    else becomes a JSON string, which YAML reads as a double-quoted scalar
    and as nothing else. Quoting only "when needed" missed the plain
    scalars YAML reads as something else (true, null, 2026-10-04, - x), so
    a poem titled "True" came back a boolean, and a quote or backslash in a
    title or lens name broke the file."""
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, (int, float)):
        return str(value)
    return json.dumps(str(value), ensure_ascii=False)


def field(key: str, value) -> str:
    """`key: value`, with an ISO date under a date key left bare."""
    if key in DATE_KEYS and isinstance(value, str) and _ISO_DATE.fullmatch(value):
        return f"{key}: {value}"
    return f"{key}: {scalar(value)}"


def tags_field(tags: list[str]) -> str:
    """`tags:` as a flow list of JSON strings: "travel: denmark" stays one
    tag instead of breaking the front matter."""
    return "tags: [" + ", ".join(scalar(t) for t in tags) + "]"
