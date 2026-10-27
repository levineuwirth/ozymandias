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

from tests._helpers import SITE_ROOT, engine_path, load_tool, site_binary

SITE = SITE_ROOT / "_site"
CONTENT = SITE_ROOT / "content"
SITE_YAML = SITE_ROOT / "site.yaml"
FONTS = SITE_ROOT / "tools" / "fonts.yaml"
# What the browser tests compare against, and the policy they serve the site
# under, as its server does.
BASELINES = SITE_ROOT / "tests" / "browser-baseline"
CSP_CONF = SITE_ROOT / "nginx" / "security-headers.conf"
SWITCH_OFF = "switch off: "
NO_FONTS = "the site has no tools/fonts.yaml"


@functools.cache
def config() -> dict:
    """The site's site.yaml as the generator reads and checks it."""
    return json.loads(subprocess.check_output([str(site_binary()), "config"], cwd=SITE_ROOT, text=True))


def site_yaml() -> dict:
    """The site's site.yaml as written, for the plain values a test needs
    where there is no generator (CI): its switches, its font preloads."""
    return yaml.safe_load(SITE_YAML.read_text(encoding="utf-8")) or {}


def switches(settings: dict | None = None) -> dict:
    """The switches the site's site.yaml (or `settings`) sets."""
    return dict((site_yaml() if settings is None else settings).get("switches") or {})


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


@functools.cache
def routes():
    """The site's pages for the browser tests and the harness (its
    tests/routes.yaml, engine/tools/browser/routes.py)."""
    return load_tool("browser/routes.py", "site_routes").Routes(SITE_ROOT)


def route(role: str) -> str:
    """The path of the site's page for `role` (routes.yaml's roles)."""
    return routes().role(role)


def built(path: str) -> Path:
    """The file the built site serves for a page's URL path: a directory's
    index.html, or the file itself."""
    rel = path.lstrip("/")
    return SITE / rel / "index.html" if path.endswith("/") else SITE / rel


def fonts() -> dict | None:
    """The site's web fonts (tools/fonts.yaml, for engine/tools/subset-fonts.py),
    or None for a site without any."""
    return yaml.safe_load(FONTS.read_text(encoding="utf-8")) if FONTS.is_file() else None
