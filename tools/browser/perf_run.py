"""Quick perf: requests, encoded bytes, LCP, CLS (local server, br/gz sidecars served, no network throttling).
Also the origins other than the local server each page asks for; offline
(BROWSER_OFFLINE=1) those requests are refused, so the bytes are the site's own.
usage: python perf_run.py <outname> <width> [route ...]
"""
import sys, collections
from urllib.parse import urlsplit
from playwright.sync_api import sync_playwright
from lib import *

outname, width = sys.argv[1], int(sys.argv[2])
only = set(sys.argv[3:])
BASE = f'http://127.0.0.1:{PORT_NONE}'
PROUTES = ['home', 'essay-r6', 'essay-grd', 'essay-simd', 'photo-index', 'photo-series', 'photo-single', 'photo-contact',
           'photo-year', 'photo-map', 'music-index', 'composition', 'score-reader', 'search', 'library', 'new', 'current', 'colophon', 'cv-about', 'stats', 'me']

OBS = r"""
window.__lcp = null; window.__cls = 0; window.__lcpEl = null;
try {
new PerformanceObserver(l => { for (const e of l.getEntries()) { window.__lcp = e.startTime; window.__lcpEl = e.element ? (e.element.tagName + '.' + e.element.className).slice(0, 80) : (e.url || '').slice(-60); } }).observe({type: 'largest-contentful-paint', buffered: true});
new PerformanceObserver(l => { for (const e of l.getEntries()) { if (!e.hadRecentInput) window.__cls += e.value; } }).observe({type: 'layout-shift', buffered: true});
} catch (e) {}
"""

def run():
    res = {}
    with sync_playwright() as p:
        browser = p.chromium.launch()
        for name, path in ROUTES:
            if name not in PROUTES or (only and name not in only):
                continue
            ctx = browser.new_context(viewport={'width': width, 'height': 900 if width > 500 else 812})
            ctx.add_init_script(OBS)
            offline(ctx)
            page = ctx.new_page()
            outside = set()
            def asked(request):
                u = urlsplit(request.url)
                if u.scheme in ('http', 'https') and not request.url.startswith(BASE + '/'):
                    outside.add(f'{u.scheme}://{u.netloc}')
            page.on('request', asked)
            cdp = ctx.new_cdp_session(page)
            cdp.send('Network.enable')
            cdp.send('Network.setCacheDisabled', {'cacheDisabled': True})
            sizes = {}
            types = {}
            urls = {}
            cdp.on('Network.responseReceived', lambda e: (types.__setitem__(e['requestId'], e['type']), urls.__setitem__(e['requestId'], e['response']['url'])))
            cdp.on('Network.loadingFinished', lambda e: sizes.__setitem__(e['requestId'], e['encodedDataLength']))
            page.goto(BASE + path, wait_until='load')
            try:
                page.wait_for_load_state('networkidle', timeout=10000)
            except Exception:
                pass
            page.wait_for_timeout(1500)
            nav = page.evaluate("(() => { const n = performance.getEntriesByType('navigation')[0]; return {dcl: Math.round(n.domContentLoadedEventEnd), load: Math.round(n.loadEventEnd)}; })()")
            lcp = page.evaluate('window.__lcp'); cls = page.evaluate('window.__cls'); lcpEl = page.evaluate('window.__lcpEl')
            by = collections.Counter(); byn = collections.Counter()
            for rid, sz in sizes.items():
                t = types.get(rid, '?'); by[t] += sz; byn[t] += 1
            biggest = sorted(((sz, urls.get(rid, '')[-70:]) for rid, sz in sizes.items()), reverse=True)[:4]
            rec = {'requests': len(sizes), 'kB': round(sum(sizes.values()) / 1024), 'outside': sorted(outside),
                   'by_type_kB': {k: round(v / 1024) for k, v in by.items()}, 'by_type_n': dict(byn),
                   'lcp_ms': round(lcp) if lcp else None, 'lcp_el': lcpEl, 'cls': round(cls, 4), **nav, 'biggest': biggest}
            res[name] = rec
            print(name, rec['requests'], 'req', rec['kB'], 'kB, outside', rec['outside'], 'LCP', rec['lcp_ms'], lcpEl, 'CLS', rec['cls'], 'load', nav['load'], biggest[:2], flush=True)
            ctx.close()
        browser.close()
    dump(outname, res)

run()
