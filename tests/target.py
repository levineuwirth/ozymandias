"""The site the engine's tests run against: where it is, how it is
configured, and which of the engine's files its build reads.

Imported as `from tests.target import ...`; not named site.py, which would
stand in for the standard library's `site` wherever unittest puts this
directory on the path.

A test of a feature a site may turn off says so with `requires_switch`: it
skips with the reason "switch off: <name>". engine/tests/ci.py fails on any
skip but those the site explains (`explained`): a switch it turns off, or
web fonts it has none of.

Switches are read from site.yaml here, not from the generator: they are
plain booleans, and ci.py runs where there is no generator (the GitHub
workflow). engine/tests/test_config.py checks the reading against the
generator's. Until A8 adds them a site has none, and every feature is on;
A8's switch registry, a data file both read, supplies the engine's
defaults.
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
SITE_YAML = SITE_ROOT / "site.yaml"
FONTS = SITE_ROOT / "tools" / "fonts.yaml"
SWITCH_OFF = "switch off: "
NO_FONTS = "the site has no tools/fonts.yaml"


@functools.cache
def config() -> dict:
    """The site's site.yaml as the generator reads and checks it."""
    return json.loads(subprocess.check_output([str(site_binary()), "config"], cwd=SITE_ROOT, text=True))


def switches(settings: dict | None = None) -> dict:
    """The switches the site's site.yaml (or `settings`) sets."""
    if settings is None:
        settings = yaml.safe_load(SITE_YAML.read_text(encoding="utf-8")) or {}
    return dict(settings.get("switches") or {})


def switch_on(name: str, settings: dict | None = None) -> bool:
    """Whether the site (or `settings`) turns feature `name` on."""
    return switches(settings).get(name, True) is not False


def explained(reason: str, settings: dict | None = None) -> bool:
    """Whether the site explains a test's skip: a switch it turns off, or
    web fonts it has none of."""
    if reason.startswith(SWITCH_OFF):
        return not switch_on(reason.removeprefix(SWITCH_OFF), settings)
    return reason == NO_FONTS and not FONTS.is_file()


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
