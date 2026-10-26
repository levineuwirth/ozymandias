from playwright.sync_api import sync_playwright
from lib import SHOTS
INIT = """window.__cls=[];new PerformanceObserver(l=>{for(const e of l.getEntries()){window.__cls.push({v:e.value, src:(e.sources||[]).map(s=>s.node&&s.node.className||s.node&&s.node.nodeName)})}}).observe({type:'layout-shift',buffered:true});"""
with sync_playwright() as p:
    b = p.chromium.launch()
    for w, h in ((1280, 800), (390, 844)):
        ctx = b.new_context(viewport={'width': w, 'height': h}); pg = ctx.new_page(); pg.add_init_script(INIT)
        for path in ('/404.html', '/me.html', '/links.html'):
            pg.goto('http://127.0.0.1:48792' + path, wait_until='networkidle'); pg.wait_for_timeout(500)
            shifts = pg.evaluate('window.__cls')
            foot = [s for s in shifts if any('footer' in str(x) for x in s['src'])]
            vis = pg.evaluate("(()=>{const r=document.querySelector('[data-build-time]').getBoundingClientRect();return r.top<innerHeight})()")
            print(f"{w}px {path}: footer in view {vis}, total CLS {sum(s['v'] for s in shifts):.4f}, footer shifts {[round(s['v'],4) for s in foot]}")
        if w == 390: pg.goto('http://127.0.0.1:48792/me.html', wait_until='networkidle'); pg.locator('footer').screenshot(path=f'{SHOTS}/footer-phone.png')
        ctx.close()
    b.close()
