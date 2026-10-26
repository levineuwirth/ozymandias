"""Popup providers under a given server/policy.
usage: python popup_run.py <browser> <port> <outname>
"""
import sys, re
from playwright.sync_api import sync_playwright
from lib import *

browser_name, port, outname = sys.argv[1], int(sys.argv[2]), sys.argv[3]
BASE = f'http://127.0.0.1:{port}'
PAGES = ['/essays/growing-radius-domination.html', '/memento-mori.html', '/essays/where-does-simd-help-post-quantum-cryptography/',
         '/essays/near-critical-growing-radius-domination.html', '/essays/proof-broker/', '/links.html', '/work.html',
         '/essays/scaling_outage.html', '/essays/verified-inference/', '/bibliography/']
PROV = [
    ('wikipedia', r'wikipedia\.org/wiki/'), ('doi', r'doi\.org/'), ('arxiv', r'arxiv\.org/(abs|pdf)/'),
    ('github-code', r'github\.com/[^/]+/[^/]+/(blob|tree|commit)/'), ('github-repo', r'^https://github\.com/[^/]+/[^/]+/?$'),
    ('forge', r'git\.levineuwirth\.org/'), ('openlibrary', r'openlibrary\.org'), ('youtube', r'youtube\.com|youtu\.be'),
    ('biorxiv', r'biorxiv|medrxiv'), ('ia', r'archive\.org/details'), ('pubmed', r'pubmed'),
    ('citation', r'^#ref-'), ('pdf', r'pdfjs/web/viewer'), ('internal', r'^(\.\.?/|/)(?!pdfjs)'),
]

def run():
    res = {}
    with sync_playwright() as p:
        browser = getattr(p, browser_name).launch()
        ctx = browser.new_context(viewport={'width': 1440, 'height': 1000})
        ctx.add_init_script(CSP_INIT)
        done = set()
        for path in PAGES:
            page = ctx.new_page()
            cons = []
            page.on('console', lambda m: cons.append(m.text[:300]) if m.type == 'error' else None)
            page.goto(BASE + path, wait_until='load'); settle(page, 800)
            links = page.evaluate("""() => [...document.querySelectorAll('#markdownBody a[href], main a[href]')].map((a, i) => { a.dataset.lnIdx = i; return {i, href: a.getAttribute('href'), bound: a.dataset.popupBound || null, cls: a.className}; })""")
            for kind, rx in PROV:
                if kind in done and kind not in ('wikipedia',):
                    continue
                cands = [l for l in links if re.search(rx, l['href'] or '')]
                if not cands:
                    continue
                for l in cands[:1]:
                    sel = f'[data-ln-idx="{l["i"]}"]'
                    rec = {'page': path, 'kind': kind, 'href': l['href'], 'bound': l['bound']}
                    try:
                        el = page.query_selector(sel)
                        el.scroll_into_view_if_needed(timeout=2000)
                        page.mouse.move(5, 5); page.wait_for_timeout(400)
                        nbefore = len(cons)
                        el.hover(timeout=2000)
                        vis = False
                        for _ in range(25):
                            page.wait_for_timeout(200)
                            vis = page.evaluate("(() => { const p = document.querySelector('.link-popup'); return !!p && p.classList.contains('is-visible') && p.innerText.trim().length > 0; })()")
                            if vis: break
                        page.wait_for_timeout(1200)
                        info = page.evaluate("""(() => { const p = document.querySelector('.link-popup'); if (!p) return null;
                            return {cls: p.className, text: p.innerText.slice(0, 140), imgs: [...p.querySelectorAll('img')].map(i => ({src: (i.currentSrc||i.src).slice(0, 90), ok: i.complete && i.naturalWidth > 0}))}; })()""")
                        rec.update({'visible': vis, 'popup': info, 'console': cons[nbefore:]})
                        if vis: done.add(kind)
                    except Exception as e:
                        rec['err'] = str(e)[:200]
                    res.setdefault(kind, []).append(rec)
                    print(kind, path, l['href'][:70], 'bound=', l['bound'], 'vis=', rec.get('visible'), (rec.get('popup') or {}).get('text', '')[:60].replace('\n', ' | '), [(i['src'][:50], i['ok']) for i in (rec.get('popup') or {}).get('imgs', [])], rec.get('console'), flush=True)
            csp = page.evaluate('window.__cspv || []')
            if csp:
                res.setdefault('_csp', []).append({'page': path, 'csp': csp})
                print('   CSP', path, [(c['effective'], c['blocked'][:80]) for c in csp])
            page.close()
        browser.close()
    dump(outname, res)

run()
