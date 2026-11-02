import json, os, re, sys, time
from pathlib import Path

HERE = os.path.dirname(os.path.abspath(__file__))
# sitelib, in the engine's tools/ above this harness, and routes beside
# it, whatever SITE_ROOT and the working directory are.
sys.path[:0] = [HERE, str(Path(__file__).resolve().parents[1])]
import sitelib  # noqa: E402
from routes import Routes  # noqa: E402
# The site: $SITE_ROOT (tests/_browser.py sets it, running these scripts in
# their own directory), else the working directory.
REPO = os.environ.get('SITE_ROOT') or os.getcwd()
# Results and screenshots go to $BROWSER_OUT or .browser-runs/ (gitignored),
# not beside the scripts.
OUT = os.environ.get('BROWSER_OUT') or os.path.join(REPO, '.browser-runs')
SHOTS = os.path.join(OUT, 'shots')
# axe-core is vendored: beside the version it is checked against
# (axe-version).
AXE = str(sitelib.engine_path('tools/browser/axe-version').parent / 'axe.min.js')
os.makedirs(SHOTS, exist_ok=True)

PORT_ENFORCE = 48731
PORT_THUMB = 48732
# A test runs these scripts against a server of its own and names its port.
PORT_NONE = int(os.environ.get('BROWSER_PORT', '48733'))

# The site's pages, by name (its tests/routes.yaml, routes.py), and in the
# order the sweeps visit them.
SITE_ROUTES = Routes(REPO)
ROUTES = list(SITE_ROUTES.pages.items())

CSP_INIT = r"""
(() => {
  if (window.__cspInstalled) return; window.__cspInstalled = true;
  window.__cspv = [];
  const h = e => { try { window.__cspv.push({
      directive: e.violatedDirective, effective: e.effectiveDirective,
      blocked: e.blockedURI, source: e.sourceFile, line: e.lineNumber,
      col: e.columnNumber, sample: e.sample, disposition: e.disposition,
      doc: e.documentURI}); } catch (_) {} };
  document.addEventListener('securitypolicyviolation', h, true);
  window.__errs = [];
  // Captured, an error event also reports elements that failed to load
  // (img, script, link); those go to __reserrs, not among script errors.
  window.__reserrs = [];
  window.addEventListener('error', e => {
    const t = e.target;
    if (t && t !== window && t.tagName) {
      window.__reserrs.push(t.tagName.toLowerCase() + ' ' + (t.getAttribute('src') ?? t.getAttribute('href') ?? '(no src)'));
      return;
    }
    window.__errs.push(String(e.message) + ' @' + (e.filename||'') + ':' + (e.lineno||''));
  }, true);
  window.addEventListener('unhandledrejection', e => { window.__errs.push('unhandledrejection: ' + String(e.reason && (e.reason.stack || e.reason.message) || e.reason)); });
})();
"""

def options(args):
    """--name=value options, and the other arguments in order."""
    opts = dict(a[2:].split('=', 1) for a in args if a.startswith('--') and '=' in a)
    return opts, [a for a in args if not a.startswith('--')]

# A text size chosen in the settings panel, in px, stored as settings.js
# stores it (--text-size=N to a script that takes it).
TEXT_SIZE_INIT = "try { localStorage.setItem('text-size', '%d'); } catch (e) {}"

THEME_INIT = """
try { localStorage.setItem('theme', '%s'); } catch (e) {}
"""

# The pages' calendar moved to a date (milliseconds since the epoch), as
# theme.js reads it for seasons; time still runs from there. Playwright's
# clock would fake performance too, whose timings perf_run.py reads.
DATE_INIT = """
(() => {
  const R = Date, shift = %d - R.now();
  function D(...a) {
    if (!new.target) return new R(R.now() + shift).toString();
    return a.length ? new R(...a) : new R(R.now() + shift);
  }
  D.prototype = R.prototype; D.parse = R.parse; D.UTC = R.UTC;
  D.now = () => R.now() + shift;
  window.Date = D;
})();
"""

def offline(ctx):
    """With BROWSER_OFFLINE=1, nothing leaves the local server: map tiles,
    CDN scripts and the like are refused. The tests set it; the CSP sweep,
    which is meant to meet the live origins, does not."""
    if os.environ.get('BROWSER_OFFLINE') == '1':
        ctx.route(re.compile(r'^(?!http://127\.0\.0\.1[:/])'), lambda route: route.abort())


POLICY_HEADERS = ('content-security-policy', 'content-security-policy-report-only')


def suppress_reports(ctx):
    """No CSP report leaves the browser, for a run against a site whose
    report log is someone's evidence (production). Aborting /csp-report
    is not enough: Chromium's reports pass through routing, but Firefox
    sends its own where routing never sees them. So each document's
    policies lose their report-uri and report-to before the browser reads
    them: still enforced as served, and a violation still fires the
    page's securitypolicyviolation event (CSP_INIT records it), with
    nowhere to send a report. Several policies of one name go back as one
    header, comma-separated, as CSP reads them. /csp-report is aborted
    too, in case a report finds another way."""
    def strip(route):
        if route.request.resource_type != 'document':
            route.continue_()
            return
        resp = route.fetch()
        headers = {}
        for h in resp.headers_array:
            name, value = h['name'].lower(), h['value']
            if name in POLICY_HEADERS:
                value = re.sub(r'\s*;?\s*report-(?:uri|to)\s+[^;,]*', '', value).strip().rstrip(';')
                headers[name] = headers[name] + ', ' + value if name in headers else value
            elif name in headers:
                headers[name] += ', ' + value
            else:
                headers[name] = value
        route.fulfill(response=resp, headers=headers)
    ctx.route('**/*', strip)
    ctx.route('**/csp-report', lambda route: route.abort())


def dump(name, obj):
    p = os.path.join(OUT, name)
    with open(p, 'w') as f:
        json.dump(obj, f, indent=1, default=str)
    return p


def settle(page, ms=600):
    try:
        page.wait_for_load_state('load', timeout=20000)
    except Exception:
        pass
    try:
        page.wait_for_load_state('networkidle', timeout=8000)
    except Exception:
        pass
    page.wait_for_timeout(ms)


def scroll_through(page, step=900, max_steps=60):
    try:
        h = page.evaluate('document.scrollingElement.scrollHeight')
        y = 0; n = 0
        while y < h and n < max_steps:
            y += step; n += 1
            page.evaluate(f'window.scrollTo(0, {y})')
            page.wait_for_timeout(60)
            h = page.evaluate('document.scrollingElement.scrollHeight')
        page.evaluate('window.scrollTo(0, 0)')
    except Exception:
        pass
