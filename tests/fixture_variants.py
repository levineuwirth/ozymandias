"""The fixture site's builds (plan §7.4): its two profiles
(fixture-site/profiles.yaml), and the variants that build each switch the
other way where the profiles do not.

- An off variant for each switch on in minimal: minimal with that switch
  off, and with it every switch left without what it needs.
- An alternative variant for each switch with `needs-any`: the full
  profile with only one of the alternatives on, one variant per
  alternative.

Each build is a full set of switches and the inputs its copy leaves out.
The generator is the judge of a set: test_fixture_site has it read each
one first, and a set it refuses is this module's mistake.
"""

from __future__ import annotations

import json
import shutil
from dataclasses import dataclass, field
from pathlib import Path

import yaml

from tests._helpers import engine_path

FIXTURE = engine_path("fixture-site")
REGISTRY = engine_path("switches.yaml")


@dataclass(frozen=True)
class Build:
    name: str
    switches: dict[str, bool]
    without: tuple[str, ...] = field(default=())


def registry() -> dict[str, dict]:
    return yaml.safe_load(REGISTRY.read_text(encoding="utf-8")) or {}


def profiles() -> dict[str, dict]:
    return yaml.safe_load((FIXTURE / "profiles.yaml").read_text(encoding="utf-8"))


def switched(config: dict, switches: dict[str, bool]) -> dict:
    """`config` (a site.yaml's) with `switches`, less the sections, the
    standalone pages' contexts and the links to routes a switch owns
    (switches.yaml's `section`, `contexts` and `routes`) that `switches`
    has off: the generator refuses them."""
    terms = registry()
    sections = {spec["section"]: name for name, spec in terms.items() if "section" in spec}
    contexts = {c: name for name, spec in terms.items() for c in spec.get("contexts") or []}
    routes = {r: name for name, spec in terms.items() for r in spec.get("routes") or []}

    def built(link: dict) -> bool:
        owner = routes.get(link["href"].split("#")[0].split("?")[0])
        return owner is None or switches[owner]

    library = {"library": dict(config["library"], **{"see-also": [l for l in config["library"]["see-also"]
                                                                  if built(l)]})} if "library" in config else {}
    return dict(config, switches=switches, **library,
                **{"nav-primary": [l for l in config["nav-primary"] if built(l)]},
                sections=[s for s in config["sections"]
                          if s["dir"] not in sections or switches[sections[s["dir"]]]],
                standalone=[p for p in config["standalone"]
                            if p["context"] not in contexts or switches[contexts[p["context"]]]])


def copy_site(build: "Build", site: Path, engine: Path) -> None:
    """The fixture site in a copy of its own at `site`, as `build` has it: its
    `without` left out, its switches written into its site.yaml, the engine
    beside it."""
    shutil.copytree(FIXTURE, site)
    for rel in build.without:
        shutil.rmtree(site / rel) if (site / rel).is_dir() else (site / rel).unlink()
    config = switched(yaml.safe_load((site / "site.yaml").read_text(encoding="utf-8")), build.switches)
    # JSON, which YAML reads as written: a dumped string "y" would come back
    # as a boolean.
    (site / "site.yaml").write_text(json.dumps(config), encoding="utf-8")
    (site / "engine").symlink_to(engine)


def settle(switches: dict[str, bool], terms: dict[str, dict]) -> dict[str, bool]:
    """`switches` with every switch that lacks what it needs turned off, until
    none does: what turning one off takes with it."""
    settled = dict(switches)
    changed = True
    while changed:
        changed = False
        for name, spec in terms.items():
            needs, any_of = spec.get("needs") or [], spec.get("needs-any") or []
            if settled[name] and (not all(settled[n] for n in needs)
                                  or (any_of and not any(settled[n] for n in any_of))):
                settled[name] = False
                changed = True
    return settled


def builds() -> list[Build]:
    """The profiles, then the variants, each named for what it shows."""
    terms, profile = registry(), profiles()
    found = [Build(name, dict(spec["switches"]), tuple(spec.get("without") or ()))
             for name, spec in profile.items()]
    minimal = next(b for b in found if b.name == "minimal")
    full = next(b for b in found if b.name == "full")
    for name in terms:
        if minimal.switches[name]:
            found.append(Build(f"minimal-without-{name}",
                               settle(dict(minimal.switches, **{name: False}), terms), minimal.without))
    for name, spec in terms.items():
        for alternative in spec.get("needs-any") or []:
            only = {other: other == alternative for other in spec["needs-any"]}
            found.append(Build(f"{name}-by-{alternative}",
                               settle(dict(full.switches, **only), terms), full.without))
    return found
