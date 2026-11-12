# Browser harness

Playwright scripts that drive Chromium and Firefox over the built site, from
the 2026-10-01 audit and later checks. They print a line per route and dump
JSON, and assert nothing themselves. The browser tests
(`tests/test_browser_*.py`, `make test-browser`) run `csp_run.py` and assert
on what it records (`tests/test_browser_csp.py`), and drive fixture pages of
their own through `serve.py --fixtures`: popups with every provider's answer
faked, dates, the slideshow, the Random link, the footer and three
robustness fixes; the search page, keyword search over the built
Pagefind index and semantic search with its model and index faked; and
highlights, the selection toolbar, collapsible sections (and printing
them) and sidenotes; photography (view modes, masonry, the map with its data
and tiles faked, the lightbox and slideshow on real pages), the equation
gallery, and the score reader with its following (real scores and
recordings); and, offline, `axe_run.py`, `kbd_run.py`, `overflow_run.py`,
`motion_run.py` and `perf_run.py` (`tests/test_browser_a11y.py`,
`tests/test_browser_layout.py`), against the known issues and budgets in
`tests/browser-baseline/`, each report required to hold every page and
variant asked of it (`tests/test_browser_harness.py` checks the keyboard
probe's own answer on cases with a known one). The
other scripts here are still reports; `popup_run.py` meets the live
providers, and `csp_run.py` the live model, which the tests do not.

## Running

```sh
make venv                                    # Playwright is in pyproject's dev group
.venv/bin/playwright install chromium firefox
make build                                   # they read _site/
.venv/bin/python tools/browser/serve.py --root _site --port 48731 --csp "<policy>" \
    --mode enforce --log .browser-runs/csp-reports.jsonl &
curl -s -o /dev/null -w '%{http_code}\n' http://127.0.0.1:48731/   # 200, or nothing below is real
.venv/bin/python tools/browser/csp_run.py chromium 48731 csp-chromium.json [route ...]
```

`--port 0` takes any free port, which the server names on its first line.

**Check that the server answers first.** If it is down, every page fails to
load and the scripts still print `csp=0 … bad=0`; only the `failed` field in
the JSON shows the refused connections.

Results and screenshots go to `$BROWSER_OUT`, or `.browser-runs/` at the
repository root (gitignored). The pages are the site's, in its
`tests/routes.yaml` (read by `routes.py`, through `lib.py`): every page the
sweeps load, by a name of the site's own; the pages each narrower run
visits; the pages each CSP-sweep probe runs on; and a page per role the
browser tests need. `playwright install` fetches the browsers that match the
installed Playwright; there is no WebKit here.
`axe_run.py` needs axe-core as `axe.min.js` in this directory (gitignored):
`fetch_axe.py` fetches the version and checksum `axe-version` records, and
`make test-browser` runs it. `BROWSER_PORT` points a script at another
server, and `BROWSER_OFFLINE=1` refuses every request that would leave it.

## Scripts

| script | checks |
|---|---|
| `serve.py` | serves `_site/` as the production vhost does: `try_files`, the internal 404 page, `.gz`/`.br` sidecars and Range, nginx's Cache-Control with `--cache`, the security and framing headers, a given CSP (enforcing or report-only), `/proxy/*` → 404, `/csp-report` logged; with `--fixtures DIR` (e.g. `fixture/`), also `/__fixture/` from that directory |
| `csp_run.py` | per route: CSP violations, console and page errors, element load errors, failed requests, feature probes (PDF.js pages, thumbnails and print among them); with `--base=URL`, a live site, sending no CSP report (`lib.suppress_reports`) |
| `interact_run.py` | keyboard interaction: settings, lightbox, slideshow, math gallery, popups, portals, filters |
| `kbd_run.py` | tab order, focus visibility, skip link, traps, Escape, on the `keyboard` pages |
| `axe_run.py` | axe-core per route (all but the `axe-skip` pages: PDF.js's viewer), viewport and theme |
| `nojs_run.py` | pages with JavaScript off |
| `overflow_run.py` | horizontal overflow on every route at several widths |
| `motion_run.py` | reduced motion, from the OS and from the site's setting, on the `motion` pages |
| `perf_run.py` | requests, bytes, LCP and CLS on the local server, and the other origins each page asks for, on the `perf` pages |
| `popup_run.py` | every link-popup provider |
| `printcheck.py` | PDF.js thumbnails and print images under the CSP |
| `prod_smoke.py` | a few checks against production; `lib.suppress_reports`, so test traffic never reaches the report log (aborting `/csp-report` alone let Firefox's through) |
| `cls_check.py`, `footer_check.py` | layout shift, and the footer build time from `/build/time.txt` |
| `preserve.py` | Phase B's preservation record: `snapshot OUT` keeps every element's computed style (and its `::before`, `::after`) on each route, in each theme and with none chosen under the system's light and dark preference, at the smallest, default and largest text sizes, and screenshots at 320, 768, 1280 and 1600px and in print; `compare BASE NEW` names each element, property and screenshot that differs, and exits 1 if any does. The clock stands still and each record waits until the page does; a Leaflet map's interior is masked |

A mechanical change (Phase B) keeps both: record the build before it and
after it, compare, and compare the page bodies with `sitediff.py
--body-only`. A record is only comparable with one of the same browser
version, which `meta.json` names.

Use a new output directory for each `snapshot`: existing directories are
refused, including those left by interrupted runs. `meta.json` is written
only when recording finishes. `compare` requires that completion marker,
compatible browser versions and recording settings, and every expected
record; missing or extra files fail even when both snapshots have the same
gap. Older snapshots without this manifest must be recorded again.

Format 3 names the CSS-default style capture `THEME-default.json.gz`.
It leaves the reader's text-size choice unset, so the served stylesheets,
their cascade, theme and media queries determine the size. The endpoint
captures still select the smallest and largest reader settings. All three
remain separate even if the default equals an endpoint. Formats 1 and 2
must be recorded again; their numeric default labels are not accepted as
format 3 records.

Format 4 adds a reader who chose no theme, with the system preferring
light and then dark: `system-light-*` and `system-dark-*` beside each
theme's records (no theme may be named `system-*`), so the CSS's
`prefers-color-scheme` rules are recorded too. It also records each
element's `color-scheme`. A season record has neither. Format 3 snapshots
must be recorded again.

Every screenshot also has a computed-style record at its width and media
(`*-view-320.json.gz`, through `*-view-print.json.gz`). This catches even
one-unit responsive and print colour changes despite the screenshot noise
tolerance. `--shots=0` explicitly omits screenshots and these accompanying
style records; it can only be compared with another styles-only snapshot.
