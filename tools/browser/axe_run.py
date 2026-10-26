"""axe-core per route x viewport x theme.
usage: python axe_run.py <browser> <outname> <themes comma> <viewports comma e.g. 1440x1000,375x812> [--text-size=29] [route ...]
"""
import sys, json, time
from playwright.sync_api import sync_playwright
from lib import *

browser_name, outname, themes, vps = sys.argv[1], sys.argv[2], sys.argv[3].split(','), sys.argv[4].split(',')
opts, routes = options(sys.argv[5:])
only = set(routes)
TEXT_SIZE = int(opts['text-size']) if 'text-size' in opts else None
BASE = f'http://127.0.0.1:{PORT_NONE}'

AXE_OPTS = {
    'runOnly': {'type': 'tag', 'values': ['wcag2a', 'wcag2aa', 'wcag21a', 'wcag21aa', 'wcag22aa', 'best-practice']},
    'resultTypes': ['violations'],
}

def summarize(viol):
    out = []
    for v in viol:
        nodes = []
        for n in v['nodes'][:4]:
            d = {'target': n['target'], 'html': n['html'][:160]}
            if v['id'] == 'color-contrast':
                for a in n.get('any', []):
                    if a.get('data'):
                        dd = a['data']
                        d['contrast'] = {k: dd.get(k) for k in ('fgColor', 'bgColor', 'contrastRatio', 'fontSize', 'fontWeight', 'expectedContrastRatio')}
            nodes.append(d)
        out.append({'id': v['id'], 'impact': v['impact'], 'count': len(v['nodes']), 'help': v['help'], 'nodes': nodes})
    return out

# PDF.js's viewer is a vendored application, not the site's markup; the
# CSP sweep loads it (tests/test_browser_csp.py).
SKIP = ['pdfjs']

def run():
    res = {}
    with sync_playwright() as p:
        browser = getattr(p, browser_name).launch()
        for theme in themes:
            for vp in vps:
                w, h = map(int, vp.split('x'))
                ctx = browser.new_context(viewport={'width': w, 'height': h}, bypass_csp=True,
                                          reduced_motion='reduce')
                ctx.add_init_script(THEME_INIT % theme)
                if TEXT_SIZE:
                    ctx.add_init_script(TEXT_SIZE_INIT % TEXT_SIZE)
                offline(ctx)
                page = ctx.new_page()
                for name, path in ROUTES:
                    if only and name not in only:
                        continue
                    if name in SKIP:
                        continue
                    key = f'{name}|{vp}|{theme}'
                    try:
                        page.goto(BASE + path, wait_until='load', timeout=30000)
                        settle(page, 1200)
                        page.add_script_tag(path=AXE)
                        r = page.evaluate('opts => axe.run(document, opts)', AXE_OPTS)
                        res[key] = summarize(r['violations'])
                        print(key, len(res[key]), [ (v['id'], v['count']) for v in res[key]], flush=True)
                    except Exception as e:
                        res[key] = {'error': str(e)[:300]}
                        print(key, 'ERR', str(e)[:200], flush=True)
                ctx.close()
        browser.close()
    dump(outname, res)

run()
