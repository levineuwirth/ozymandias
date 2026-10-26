"""The lists the tools share with the generator, as `site shared-rules`
prints them.

A list that a tool must agree with the build on (the epistemic
vocabularies, the content directories a page collection may not take) is
defined once, in build/, and read here: hand copies of these drifted.
Fails if the generator does not build.
"""

from __future__ import annotations

import json
import sys
import subprocess
from functools import lru_cache
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import sitelib  # noqa: E402


@lru_cache(maxsize=1)
def shared_rules() -> dict:
    return json.loads(subprocess.check_output(
        [sitelib.site_binary(), "shared-rules"], cwd=sitelib.site_root(), text=True))
