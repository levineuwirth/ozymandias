#!/usr/bin/env python3
"""Phase B's preservation harness: what a mechanical change must leave as it
was, recorded from a built site and compared with another record.

    preserve.py snapshot OUT [--root=DIR] [--pages=NAME,...] [--jobs=N]
                             [--browser=chromium] [--shots=0] [--season=ID]
                             [--volatile=SELECTOR,...] [--init=FILE]
    preserve.py compare BASE NEW [--report=DIR]

A snapshot serves the site's _site (or --root) as nginx does
(serve.py, with the site's enforcing policy when it has one) and visits each
page of its route list (tests/routes.yaml, routes.py) in each theme, with the
clock set to a day no season dresses, motion reduced, and nothing fetched
but from the site itself:

  styles   the computed style of every element under <html> but <head>, and
           of each ::before and ::after that has content, for PROPERTIES,
           at 1280px wide and the smallest, default and largest text sizes
           (utils.js's TEXT_SIZE; the default, tokens.css's or the site's),
           plus each screenshot's width and media at the default text size;
  shots    the page at 320, 768, 1280 and 1600px wide, and in print, at the
           default size, from the top down to SHOT_HEIGHT.

What moves on its own (VOLATILE: a Leaflet map's interior) is recorded as
an element without its insides, and masked in screenshots; the clock
stands still, and each record waits until the page does. --season records
a season worn (the clock inside its window, chosen as the theme), which the
default's day outside every season never shows; --volatile and --init are
for a targeted check of what VOLATILE masks (a script that holds it still).

Elements are named by their path from <html> (tag and position among its
siblings of that tag), so a record is only comparable with one of a site
whose page bodies are the same, which a mechanical Phase B change keeps
(sitediff.py --body-only says so). compare names each element, pseudo-
element and property that differs, and each screenshot whose pixels differ
by more than TOLERANCE in a channel (--exact: by anything), with an image of
where; it exits 1 when anything does.
"""

from __future__ import annotations

import argparse
import asyncio
import datetime
import gzip
import hashlib
import json
import os
import re
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path[:0] = [str(HERE), str(HERE.parent)]
from routes import Routes  # noqa: E402

THEMES = ("light", "dark", "cappuccino")
WIDTHS = (320, 768, 1280, 1600)
STYLE_WIDTH = 1280
HEIGHT = 900
SHOT_HEIGHT = 6000
# Screenshots of one build differed by up to 2 in a channel at the edge of an
# anti-aliased curve (the score reader's play button at 320px, now and then).
# A change of colour is the computed styles' to catch, which they do to the
# unit; the screenshots are for what styles don't name, never that faint.
TOLERANCE = 2
FORMAT = 2

# What a reader sees of an element, and what places it: the properties a
# move of CSS between files or layers could change without meaning to.
PROPERTIES = [
    "display", "position", "top", "right", "bottom", "left", "float", "clear", "z-index",
    "box-sizing", "width", "height", "min-width", "max-width", "min-height", "max-height",
    "margin-top", "margin-right", "margin-bottom", "margin-left",
    "padding-top", "padding-right", "padding-bottom", "padding-left",
    "border-top-width", "border-right-width", "border-bottom-width", "border-left-width",
    "border-top-style", "border-right-style", "border-bottom-style", "border-left-style",
    "border-top-color", "border-right-color", "border-bottom-color", "border-left-color",
    "border-top-left-radius", "border-top-right-radius", "border-bottom-right-radius",
    "border-bottom-left-radius", "outline-style", "outline-width", "outline-color", "outline-offset",
    "color", "background-color", "background-image", "background-position", "background-size",
    "background-repeat", "box-shadow", "text-shadow", "opacity", "visibility", "filter",
    "mix-blend-mode", "transform", "clip-path", "overflow-x", "overflow-y",
    "font-family", "font-size", "font-weight", "font-style", "font-stretch",
    "font-variant-caps", "font-variant-numeric", "font-variant-ligatures",
    "font-feature-settings", "font-variation-settings", "font-kerning", "font-optical-sizing",
    "line-height", "letter-spacing", "word-spacing", "text-align", "text-indent",
    "text-transform", "text-decoration-line", "text-decoration-style", "text-decoration-color",
    "text-decoration-thickness", "text-underline-offset", "white-space", "hyphens",
    "word-break", "overflow-wrap", "vertical-align", "list-style-type", "list-style-position",
    "flex-direction", "flex-wrap", "flex-grow", "flex-shrink", "flex-basis", "order",
    "justify-content", "align-items", "align-self", "align-content", "row-gap", "column-gap",
    "grid-template-columns", "grid-template-rows", "grid-column-start", "grid-column-end",
    "grid-row-start", "grid-row-end", "grid-auto-flow", "column-count", "column-width",
    "object-fit", "aspect-ratio", "cursor", "pointer-events", "user-select",
    "fill", "stroke", "stroke-width", "content", "counter-increment", "counter-reset",
    "scroll-margin-top", "caret-color", "accent-color",
]

# What moves on its own, timed by script, so that two records of one build
# differ: recorded as an element, but not what is inside it, and masked in
# screenshots. Leaflet's map settles its zoom and lays out its tiles on
# timers; its interior is the vendored library's.
VOLATILE = [".leaflet-container"]

# Each element's computed style, and its ::before and ::after where they have
# content, as indexes into a table of distinct styles (most share one).
COLLECT = """
([props, volatile]) => {
  const styles = [], index = new Map(), elements = [];
  const id = cs => {
    const values = props.map(p => cs.getPropertyValue(p));
    const key = JSON.stringify(values);
    let i = index.get(key);
    if (i === undefined) { i = styles.length; styles.push(values); index.set(key, i); }
    return i;
  };
  const pseudo = (el, which) => {
    const cs = getComputedStyle(el, which);
    const content = cs.getPropertyValue('content');
    return content && content !== 'none' && content !== 'normal' ? id(cs) : null;
  };
  const SKIP = new Set(['head', 'script', 'style', 'template', 'noscript']);
  const walk = (el, path) => {
    elements.push([path, id(getComputedStyle(el)), pseudo(el, '::before'), pseudo(el, '::after')]);
    if (volatile.some(v => el.matches(v))) return;
    const seen = {};
    for (const child of el.children) {
      const tag = child.tagName.toLowerCase();
      if (SKIP.has(tag)) continue;
      seen[tag] = (seen[tag] || 0) + 1;
      walk(child, path + '>' + tag + ':' + seen[tag]);
    }
  };
  walk(document.documentElement, 'html');
  return {props, styles, elements};
}
"""

THEME_INIT = "try { localStorage.setItem('theme', %s); } catch (e) {}"
SIZE_INIT = "try { localStorage.setItem('text-size', '%d'); } catch (e) {}"
# The calendar stopped at a fixed moment, so no season dresses a page and
# nothing that counts time (a countdown, a relative date) moves between two
# records. Unlike the browser tests' (lib.DATE_INIT), time does not run on:
# timers still fire, performance.now() still advances.
DATE_INIT = """
(() => {
  const R = Date, now = %d;
  function D(...a) {
    if (!new.target) return new R(now).toString();
    return a.length ? new R(...a) : new R(now);
  }
  D.prototype = R.prototype; D.parse = R.parse; D.UTC = R.UTC;
  D.now = () => now;
  window.Date = D;
})();
"""

# Until the page holds still: animations and transitions finished (an
# endless one cancelled), no change to the document for `quiet` ms, and the
# images above `limit` loaded and decoded; at most `max` ms. A map settling
# its zoom and tiles, a label fading in, an image arriving late each made two
# records of the same build differ.
STILL = """
async ([quiet, max, limit]) => {
  const start = performance.now();
  let last = start;
  const watch = new MutationObserver(() => { last = performance.now(); });
  watch.observe(document.documentElement, {subtree: true, childList: true, attributes: true, characterData: true});
  const above = i => i.getBoundingClientRect().top + window.scrollY < limit;
  while (performance.now() - start < max) {
    for (const a of document.getAnimations()) { try { a.finish(); } catch (e) { a.cancel(); } }
    const waiting = [...document.images].filter(i => above(i) && !i.complete);
    if (performance.now() - last >= quiet && waiting.length === 0) break;
    await new Promise(r => setTimeout(r, 100));
  }
  watch.disconnect();
  await Promise.all([...document.images].filter(i => above(i) && i.complete && i.naturalWidth)
                    .map(i => i.decode().catch(() => {})));
}
"""


def site_root() -> Path:
    return Path(os.environ.get("SITE_ROOT") or os.getcwd()).resolve()


DEFAULT_SIZE = re.compile(r"--text-size-default\s*:\s*(\d+)px")


def text_sizes(engine: Path, root: Path | None = None) -> tuple[int, int, int]:
    """utils.js's TEXT_SIZE, its smallest and largest, and the default
    between them: the site's own --text-size-default (theme.css, else
    site.css) where it sets one, else tokens.css's."""
    js = (engine / "static/js/utils.js").read_text(encoding="utf-8")
    m = re.search(r"TEXT_SIZE\s*=\s*\{\s*min:\s*(\d+),\s*max:\s*(\d+),", js)
    if not m:
        raise SystemExit("preserve: utils.js's TEXT_SIZE not found")
    low, high = map(int, m.groups())
    sheets = [] if root is None else [root / "static/css/theme.css", root / "static/css/site.css"]
    for sheet in [*sheets, engine / "static/css/tokens.css"]:
        found = DEFAULT_SIZE.findall(sheet.read_text(encoding="utf-8")) if sheet.is_file() else []
        if found:
            return low, int(found[-1]), high
    raise SystemExit("preserve: tokens.css's --text-size-default not found")


def off_season(root: Path) -> int:
    """Noon UTC on the first day of 2026 that none of site.yaml's seasons
    dresses, in milliseconds."""
    import yaml
    config = yaml.safe_load((root / "site.yaml").read_text(encoding="utf-8")) or {}
    windows = [(int(s["from"].replace("-", "")), int(s["to"].replace("-", "")))
               for s in config.get("seasons") or []]
    day = datetime.date(2026, 1, 1)
    while True:
        md = day.month * 100 + day.day
        if not any((a <= md <= b) if a <= b else (md >= a or md <= b) for a, b in windows):
            break
        day += datetime.timedelta(days=1)
    moment = datetime.datetime(day.year, day.month, day.day, 12, tzinfo=datetime.timezone.utc)
    return int(moment.timestamp() * 1000)


def in_season(root: Path, season: str) -> int:
    """Noon UTC on the first day of site.yaml's season `season` in 2026, in
    milliseconds: a moment that season dresses."""
    import yaml
    config = yaml.safe_load((root / "site.yaml").read_text(encoding="utf-8")) or {}
    found = [s for s in config.get("seasons") or [] if s.get("id") == season]
    if not found:
        raise SystemExit(f"preserve: site.yaml has no season {season!r}")
    month, day = map(int, found[0]["from"].split("-"))
    moment = datetime.datetime(2026, month, day, 12, tzinfo=datetime.timezone.utc)
    return int(moment.timestamp() * 1000)


def enforcing_policy(root: Path) -> str:
    """The policy the site's nginx enforces, if it keeps one there."""
    conf = root / "nginx/security-headers.conf"
    if not conf.is_file():
        return ""
    found = re.findall(r'^(?:#\s+)?add_header Content-Security-Policy "([^"]+)" always;$',
                       conf.read_text(encoding="utf-8"), re.M)
    return found[0] if len(found) == 1 else ""


def serve(root: Path, served: Path, workdir: Path) -> tuple[subprocess.Popen, str]:
    policy = enforcing_policy(root)
    server = subprocess.Popen(
        [sys.executable, str(HERE / "serve.py"), "--root", str(served), "--port", "0",
         "--csp", policy, "--mode", "enforce" if policy else "none",
         "--log", str(workdir / "serve.jsonl")],
        stdout=subprocess.PIPE, stderr=open(workdir / "serve.stderr", "w"), text=True)
    line = server.stdout.readline()
    port = re.search(r" on 127\.0\.0\.1:(\d+) ", line)
    if not port:
        server.terminate()
        raise SystemExit(f"preserve: serve.py did not start: {line!r}")
    return server, f"http://127.0.0.1:{port.group(1)}"


async def settled(page, url: str) -> None:
    await page.goto(url, wait_until="load", timeout=90000)
    try:
        await page.wait_for_load_state("networkidle", timeout=15000)
    except Exception:
        pass
    await page.evaluate("document.fonts.ready.then(() => true)")


async def still(page, limit: int) -> None:
    await page.evaluate(STILL, [500, 10000, limit])


async def context(browser, base: str, theme: str, size: int | None, moment: int, width: int,
                  init: str | None = None):
    ctx = await browser.new_context(viewport={"width": width, "height": HEIGHT},
                                    reduced_motion="reduce", device_scale_factor=1)
    await ctx.add_init_script(DATE_INIT % moment)
    if init:
        await ctx.add_init_script(init)
    await ctx.add_init_script(THEME_INIT % json.dumps(theme))
    if size is not None:
        await ctx.add_init_script(SIZE_INIT % size)
    # Nothing but the site itself: tiles, CDNs and providers are refused.
    await ctx.route(re.compile(r"^(?!" + re.escape(base) + r"[/?#]|" + re.escape(base) + r"$)"),
                    lambda route: route.abort())
    return ctx


async def record_styles(page, target: Path, volatile: list[str]) -> None:
    data = await page.evaluate(COLLECT, [PROPERTIES, volatile])
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_bytes(gzip.compress(json.dumps(data, separators=(",", ":")).encode(), mtime=0))


async def record_page(browser, base, name, path, out: Path, moment, sizes, shots, errors,
                      themes, volatile, init):
    url = base + path
    for theme in themes:
        for size in sizes:
            ctx = await context(browser, base, theme, None if size == sizes[1] else size, moment, STYLE_WIDTH, init)
            page = await ctx.new_page()
            page.on("pageerror", lambda e, n=name: errors.setdefault(n, []).append(str(e)[:200]))
            try:
                await settled(page, url)
                await still(page, HEIGHT)
                await record_styles(page, out / "styles" / name / f"{theme}-{size}.json.gz", volatile)
            finally:
                await ctx.close()
        if not shots:
            continue
        for width in WIDTHS + ("print",):
            ctx = await context(browser, base, theme, None, moment, STYLE_WIDTH if width == "print" else width, init)
            page = await ctx.new_page()
            try:
                if width == "print":
                    await page.emulate_media(media="print")
                await settled(page, url)
                # Lazy images load as they come near: brought in, then the
                # page back at its top, instantly.
                height = await page.evaluate("document.scrollingElement.scrollHeight")
                for y in range(0, min(height, SHOT_HEIGHT), HEIGHT):
                    await page.evaluate(f"window.scrollTo({{top: {y}, behavior: 'instant'}})")
                    await page.wait_for_timeout(40)
                await page.evaluate("window.scrollTo({top: 0, behavior: 'instant'})")
                try:
                    await page.wait_for_load_state("networkidle", timeout=10000)
                except Exception:
                    pass
                await still(page, SHOT_HEIGHT)
                await record_styles(page, out / "styles" / name / f"{theme}-view-{width}.json.gz", volatile)
                height = await page.evaluate("document.scrollingElement.scrollHeight")
                full = await page.evaluate("document.scrollingElement.scrollWidth")
                target = out / "shots" / name / f"{theme}-{width}.png"
                target.parent.mkdir(parents=True, exist_ok=True)
                await page.screenshot(path=str(target), full_page=True, animations="disabled", caret="hide",
                                      clip={"x": 0, "y": 0, "width": full, "height": min(height, SHOT_HEIGHT)},
                                      mask=[page.locator(v) for v in volatile], mask_color="#ff00ff")
            finally:
                await ctx.close()


async def snapshot(out: Path, served: Path, pages: dict[str, str], jobs: int, browser_name: str, shots: bool,
                   season: str | None = None, volatile: list[str] = VOLATILE, init: str | None = None):
    """With `season`, the clock stands inside that season's window and each
    page wears it (a reader who chose it), in place of the three themes."""
    from playwright.async_api import async_playwright
    root = site_root()
    engine = HERE.parents[1]
    sizes = text_sizes(engine, root)
    moment = in_season(root, season) if season else off_season(root)
    themes = [season] if season else list(THEMES)
    if not pages or jobs < 1:
        raise SystemExit("preserve: snapshot needs at least one page and --jobs >= 1")
    # Never mix a rerun with old records or an old completion marker. A failed
    # run leaves diagnostics, but cannot be compared as a complete snapshot.
    try:
        out.mkdir(parents=True, exist_ok=False)
    except FileExistsError:
        raise SystemExit(f"preserve: snapshot output already exists; choose a new directory: {out}")
    server, base = serve(root, served, out)
    errors: dict[str, list[str]] = {}
    try:
        async with async_playwright() as p:
            browser = await getattr(p, browser_name).launch()
            gate = asyncio.Semaphore(jobs)

            async def one(name, path):
                async with gate:
                    await record_page(browser, base, name, path, out, moment, sizes, shots, errors,
                                      themes, volatile, init)

            await asyncio.gather(*(one(n, path) for n, path in pages.items()))
            version = browser.version
            await browser.close()
    finally:
        server.terminate()
        server.wait(timeout=10)
    (out / "meta.json").write_text(json.dumps({
        "format": FORMAT, "complete": True,
        "site": str(root), "served": str(served), "browser": f"{browser_name} {version}",
        "moment": moment, "volatile": volatile, "themes": themes, "sizes": sizes, "widths": WIDTHS,
        "season": season, "init": hashlib.sha256(init.encode()).hexdigest() if init else None,
        "style_width": STYLE_WIDTH, "height": HEIGHT, "shot_height": SHOT_HEIGHT,
        "properties": PROPERTIES, "shots": shots,
        "pages": pages, "errors": errors}, indent=1) + "\n")
    print(f"preserve: {len(pages)} pages, {browser_name} {version}, into {out}")


def resolved(path: Path, properties: list[str]) -> dict[str, tuple]:
    data = json.loads(gzip.decompress(path.read_bytes()))
    props, styles = data["props"], data["styles"]
    elements = data["elements"]
    if props != properties or not styles or not elements:
        raise ValueError("empty style record or properties differ from the manifest")
    if any(len(values) != len(props) or any(not isinstance(v, str) for v in values) for values in styles):
        raise ValueError("invalid style table")
    paths = set()
    for element in elements:
        if len(element) != 4 or not isinstance(element[0], str) or element[0] in paths:
            raise ValueError("invalid or duplicate element")
        paths.add(element[0])
        for i, value in enumerate(element[1:]):
            if value is None and i > 0:
                continue
            if type(value) is not int or not 0 <= value < len(styles):
                raise ValueError("invalid style index")
    style = lambda i: None if i is None else dict(zip(props, styles[i]))
    return {e[0]: (style(e[1]), style(e[2]), style(e[3])) for e in elements}


SETTINGS = ("format", "browser", "moment", "volatile", "themes", "sizes", "widths",
            "style_width", "height", "shot_height", "properties", "shots", "pages", "season", "init")


def inventory(meta: dict) -> dict[str, set[Path]]:
    """The full recording matrix, derived from settings rather than files found."""
    styles, shots = set(), set()
    for name in meta["pages"]:
        for theme in meta["themes"]:
            styles.update(Path(name) / f"{theme}-{size}.json.gz" for size in meta["sizes"])
            if meta["shots"]:
                for width in [*meta["widths"], "print"]:
                    styles.add(Path(name) / f"{theme}-view-{width}.json.gz")
                    shots.add(Path(name) / f"{theme}-{width}.png")
    return {"styles": styles, "shots": shots}


def manifest(root: Path) -> dict:
    """A completion marker is required even when both directories are empty."""
    meta = json.loads((root / "meta.json").read_text())
    if not isinstance(meta, dict) or any(key not in meta for key in SETTINGS):
        raise ValueError("missing recording settings; take a fresh snapshot")
    if meta["format"] != FORMAT or meta.get("complete") is not True:
        raise ValueError("unsupported or incomplete snapshot; take a fresh snapshot")
    if not isinstance(meta["browser"], str) or not meta["browser"] or type(meta["shots"]) is not bool:
        raise ValueError("invalid browser or screenshot setting")
    for key in ("moment", "style_width", "height", "shot_height"):
        if type(meta[key]) is not int or meta[key] < 1:
            raise ValueError(f"invalid {key}")
    for key in ("sizes", "widths"):
        values = meta[key]
        if (not isinstance(values, list) or not values or
                any(type(v) is not int or v < 1 for v in values) or len(set(values)) != len(values)):
            raise ValueError(f"invalid {key}")
    if len(meta["sizes"]) != 3:
        raise ValueError("expected smallest, default and largest text sizes")
    for key in ("themes", "properties", "volatile"):
        values = meta[key]
        if (not isinstance(values, list) or any(not isinstance(v, str) or not v for v in values) or
                len(set(values)) != len(values) or (key != "volatile" and not values)):
            raise ValueError(f"invalid {key}")
    if meta["season"] is not None and (not isinstance(meta["season"], str)
                                       or not re.fullmatch(r"[A-Za-z0-9_-]+", meta["season"])):
        raise ValueError("invalid season")
    if meta["init"] is not None and (not isinstance(meta["init"], str)
                                     or not re.fullmatch(r"[0-9a-f]{64}", meta["init"])):
        raise ValueError("invalid init")
    pages = meta["pages"]
    if not isinstance(pages, dict) or not pages:
        raise ValueError("snapshot has no routes")
    for name in [*pages, *meta["themes"]]:
        if not isinstance(name, str) or not re.fullmatch(r"[A-Za-z0-9_-]+", name):
            raise ValueError(f"invalid record name: {name!r}")
    if any(not isinstance(path, str) or not path.startswith("/") for path in pages.values()):
        raise ValueError("invalid route paths")
    return meta


def compare(base: Path, new: Path, report: Path | None, tolerance: int = TOLERANCE) -> int:
    lines: list[str] = []
    manifests = []
    for label, root in (("base", base), ("new", new)):
        try:
            meta = manifest(root)
            manifests.append(meta)
            for kind, expected in inventory(meta).items():
                found = {p.relative_to(root / kind) for p in (root / kind).rglob("*") if p.is_file()}
                lines += [f"{label}: missing {kind}/{p}" for p in sorted(expected - found)]
                lines += [f"{label}: unexpected {kind}/{p}" for p in sorted(found - expected)]
        except (OSError, ValueError, TypeError) as e:
            lines.append(f"{label}: invalid snapshot: {e}")
    if len(manifests) == 2:
        lines += [f"incompatible recording setting: {key}" for key in SETTINGS
                  if manifests[0][key] != manifests[1][key]]
    if not lines:
        for kind, expected in inventory(manifests[0]).items():
            for rel in sorted(expected):
                try:
                    if kind == "styles":
                        props = manifests[0]["properties"]
                        lines += style_differences(rel, resolved(base / kind / rel, props),
                                                   resolved(new / kind / rel, props))
                    else:
                        lines += shot_difference(rel, base / kind / rel, new / kind / rel, report, tolerance)
                except (OSError, ValueError, KeyError, TypeError, IndexError, EOFError) as e:
                    lines.append(f"{kind}/{rel}: invalid record: {e}")
    for line in lines[:400]:
        print(line)
    if len(lines) > 400:
        print(f"... and {len(lines) - 400} more")
    print(f"preserve: {len(lines)} difference(s)")
    if report:
        report.mkdir(parents=True, exist_ok=True)
        (report / "differences.txt").write_text("\n".join(lines) + "\n")
    return 1 if lines else 0


def style_differences(rel: Path, a: dict, b: dict) -> list[str]:
    out = []
    if a.keys() != b.keys():
        gone, added = sorted(a.keys() - b.keys()), sorted(b.keys() - a.keys())
        out.append(f"{rel}: the elements differ ({len(gone)} gone, {len(added)} added; "
                   f"first {(gone or added)[0]}): the bodies changed, compare those first")
    for path in sorted(a.keys() & b.keys()):
        for part, x, y in zip(("", "::before", "::after"), a[path], b[path]):
            if x == y:
                continue
            if x is None or y is None:
                out.append(f"{rel}: {path}{part} {'appeared' if x is None else 'went'}")
                continue
            for prop in x:
                if x[prop] != y.get(prop):
                    out.append(f"{rel}: {path}{part} {prop}: {x[prop]!r} -> {y.get(prop)!r}")
    return out


def shot_difference(rel: Path, a: Path, b: Path, report: Path | None, tolerance: int) -> list[str]:
    if a.read_bytes() == b.read_bytes():
        return []
    from PIL import Image, ImageChops
    x, y = Image.open(a).convert("RGB"), Image.open(b).convert("RGB")
    if x.size != y.size:
        return [f"shots/{rel}: size {x.size} -> {y.size}"]
    beyond = ImageChops.difference(x, y).point(lambda v: 255 if v > tolerance else 0)
    box = beyond.getbbox()
    if box is None:
        return []
    if report:
        target = report / "shots" / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        beyond.save(target)
    return [f"shots/{rel}: pixels differ in {box}"]


def main(argv: list[str]) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    sub = ap.add_subparsers(dest="command", required=True)
    s = sub.add_parser("snapshot")
    s.add_argument("out", type=Path)
    s.add_argument("--root", type=Path, help="the built site to serve (the site's _site)")
    s.add_argument("--pages", help="NAME,... of the route list, or NAME=PATH,...")
    s.add_argument("--jobs", type=int, default=int(os.environ.get("BROWSER_JOBS") or 3))
    s.add_argument("--browser", default="chromium", choices=["chromium", "firefox", "webkit"])
    s.add_argument("--shots", type=int, default=1)
    s.add_argument("--season", help="record inside this site.yaml season, worn, in place of the themes")
    s.add_argument("--volatile", help=f"selectors recorded without their insides and masked "
                                      f"(default {','.join(VOLATILE)}; empty for none)")
    s.add_argument("--init", type=Path, help="a script run before each page's own, as for a targeted check")
    c = sub.add_parser("compare")
    c.add_argument("base", type=Path)
    c.add_argument("new", type=Path)
    c.add_argument("--report", type=Path)
    c.add_argument("--exact", action="store_true", help=f"count a pixel differing by {TOLERANCE} or less too")
    args = ap.parse_args(argv)
    if args.command == "compare":
        return compare(args.base, args.new, args.report, 0 if args.exact else TOLERANCE)
    root = site_root()
    pages = dict(Routes(root).pages)
    if args.pages:
        chosen = {}
        for item in args.pages.split(","):
            name, _, path = item.partition("=")
            chosen[name] = path or pages[name]
        pages = chosen
    volatile = VOLATILE if args.volatile is None else [v for v in args.volatile.split(",") if v]
    init = args.init.read_text(encoding="utf-8") if args.init else None
    asyncio.run(snapshot(args.out.resolve(), (args.root or root / "_site").resolve(), pages,
                         args.jobs, args.browser, bool(args.shots), args.season, volatile, init))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
