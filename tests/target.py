"""The site the engine's tests run against: where it is, how it is
configured, and which of the engine's files its build reads.

Imported as `from tests.target import ...`; not named site.py, which would
stand in for the standard library's `site` wherever unittest puts this
directory on the path.

A test of a feature a site may turn off says so with `requires_switch`: it
skips with the reason "switch off: <name>", and engine/tests/ci.py, which
fails on any other skip, accepts that one only when the site's config does
turn the feature off. Until A8 adds them, a site has no switches and every
feature is on.
"""

from __future__ import annotations

import functools
import json
import subprocess
from pathlib import Path

import yaml

from tests._helpers import SITE_ROOT, engine_path, site_binary

SITE = SITE_ROOT / "_site"
CONTENT = SITE_ROOT / "content"
FONTS = SITE_ROOT / "tools" / "fonts.yaml"
SWITCH_OFF = "switch off: "


@functools.cache
def config() -> dict:
    """The site's site.yaml as the generator reads and checks it."""
    return json.loads(subprocess.check_output([str(site_binary()), "config"], cwd=SITE_ROOT, text=True))


def switch_on(name: str, settings: dict | None = None) -> bool:
    """Whether the site (or `settings`, a config) turns feature `name` on."""
    switches = (config() if settings is None else settings).get("switches") or {}
    return switches.get(name, True) is not False


def requires_switch(name: str):
    """A test method that runs only when the site turns `name` on, and
    otherwise skips with a reason ci.py can check against the config."""
    def decorate(test):
        @functools.wraps(test)
        def run(self, *args, **kwargs):
            if not switch_on(name):
                self.skipTest(SWITCH_OFF + name)
            return test(self, *args, **kwargs)
        return run
    return decorate


def engine_file(rel: str) -> Path:
    """The file the build reads for `rel`: the site's own when it overrides
    the engine's, else the engine's (build/EngineFiles.hs)."""
    own = SITE_ROOT / rel
    return own if own.is_file() else engine_path(rel)


def fonts() -> dict | None:
    """The site's web fonts (tools/fonts.yaml, for engine/tools/subset-fonts.py),
    or None for a site without any."""
    return yaml.safe_load(FONTS.read_text(encoding="utf-8")) if FONTS.is_file() else None
