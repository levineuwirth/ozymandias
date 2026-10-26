"""Use the generator's draft boundary for snapshots and build freshness.

The Python tools must not interpret YAML independently of Hakyll. Rebuild
the scanner if needed, then ask it about the actual content tree. A scan
failure aborts its caller instead of treating every page as published.
"""

import hashlib
import json
from pathlib import Path
import subprocess
import sys

sys.path.insert(0, str(Path(__file__).resolve().parent))
import sitelib  # noqa: E402


def load_boundary(content: Path) -> dict[str, list[str]]:
    summary = subprocess.check_output(
        [sitelib.site_binary(), "list-unpublished", str(content.resolve())], text=True,
    )
    boundary = {"file": [], "dir": [], "stem": []}
    for line in summary.splitlines():
        kind, path = line.split(" ", 1)
        boundary[kind].append(path)
    return boundary


def withheld(path: Path, boundary: dict[str, list[str]]) -> bool:
    name = str(path.resolve())
    return (name in boundary["file"]
            or any(name == d or name.startswith(d + "/") for d in boundary["dir"])
            or any(name.startswith(s) for s in boundary["stem"]))


if __name__ == "__main__":
    if sys.argv[1:] != ["hash"]:
        sys.exit("usage: unpublished.py hash")
    encoded = json.dumps(load_boundary(sitelib.site_root() / "content"), sort_keys=True).encode()
    print(hashlib.sha256(encoded).hexdigest())
