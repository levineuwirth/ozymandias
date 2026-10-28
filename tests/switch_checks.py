"""What each switch makes of the fixture site, both ways (plan §7.3): for
every switch the engine registers, a check that its feature is there in a
build with it on, and one that nothing of it is left in a build with it
off. test_fixture_site runs them on each of the fixture's builds, as the
build's switches say, and its coverage test fails unless every switch has
both checks and both have run.

Each check takes the copy the build ran in (its _site/ inside) and raises
AssertionError saying what is wrong.
"""

from __future__ import annotations

from pathlib import Path

ARCHIVED = "example-cited-page"   # the fixture's archive entry (archive/manifest.yaml)


def pages(site: Path) -> list[Path]:
    return sorted((site / "_site").rglob("*.html"))


def text(site: Path, rel: str) -> str:
    return (site / "_site" / rel).read_text(encoding="utf-8")


def archive_on(site: Path) -> None:
    assert ARCHIVED in text(site, "archive/index.html"), "the /archive/ index lists no entry"
    assert (site / "_site/archive" / ARCHIVED / "index.html").is_file(), "no page for the archived entry"
    assert "archive-affordance" in text(site, "essays/long/index.html"), \
        "the essay's link to the archived page has no archive mark"
    assert 'id="archive"' in text(site, "build/index.html"), "the build page has no link-archive section"


def archive_off(site: Path) -> None:
    assert not (site / "_site/archive").exists(), "_site/archive/ was built"
    marked = [p.name for p in pages(site) if "archive-affordance" in p.read_text(encoding="utf-8")]
    assert not marked, f"archive marks on {marked}"
    assert 'id="archive"' not in text(site, "build/index.html"), "the build page has a link-archive section"


# Each registered switch: (its on check, its off check).
CHECKS = {
    "archive": (archive_on, archive_off),
}
