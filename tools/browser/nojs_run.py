"""No-JS rendering checks.
usage: python nojs_run.py <browser> <outname>
"""
import sys
from playwright.sync_api import sync_playwright
from lib import *

browser_name, outname = sys.argv[1], sys.argv[2]
BASE = f'http://127.0.0.1:{PORT_NONE}'

PROBE = r"""
() => {
  const main = document.querySelector('main') || document.getElementById('markdownBody') || document.body;
  const vis = el => { const r = el.getBoundingClientRect(); const c = getComputedStyle(el); return r.width > 0 && r.height > 0 && c.visibility !== 'hidden' && c.display !== 'none'; };
  const imgs = [...document.images];
  const broken = imgs.filter(i => i.complete && i.naturalWidth === 0 && vis(i)).map(i => i.getAttribute('src') || i.getAttribute('data-src')).slice(0, 6);
  const lazyNoSrc = imgs.filter(i => !i.getAttribute('src') && (i.dataset.src || i.dataset.lazy)).length;
  const buttons = [...document.querySelectorAll('button')].filter(vis).map(b => (b.getAttribute('aria-label') || b.textContent || '').trim().slice(0, 40));
  const empties = [...main.querySelectorAll('div[id], section[id], ul[id], ol[id], div[class*=results], div[class*=grid], div[class*=stage], div[class*=map]')]
       .filter(el => vis(el) && el.textContent.trim().length === 0 && el.querySelectorAll('img,svg,canvas,iframe').length === 0)
       .map(el => el.tagName.toLowerCase() + (el.id ? '#' + el.id : '') + '.' + [...el.classList].slice(0,2).join('.')).slice(0, 8);
  const rawTex = [...document.querySelectorAll('span.math')].length;
  const hiddenMain = !vis(main);
  const textLen = (main.innerText || '').trim().length;
  // elements hidden that need JS to reveal (opacity 0, e.g. fade-in)
  const invisible = [...main.querySelectorAll('*')].filter(el => { const c = getComputedStyle(el); return c.opacity === '0' && el.textContent.trim().length > 20; }).map(el => el.tagName + '.' + el.className).slice(0, 5);
  const portalsReachable = !!document.querySelector('.nav-portals') && vis(document.querySelector('.nav-portals'));
  return {textLen, hiddenMain, broken, lazyNoSrc, buttons: buttons.slice(0, 15), nButtons: buttons.length, empties, rawTex, invisible, portalsReachable, noscript: document.querySelectorAll('noscript').length};
}
"""

def run():
    res = {}
    with sync_playwright() as p:
        browser = getattr(p, browser_name).launch()
        ctx = browser.new_context(viewport={'width': 1440, 'height': 1000}, java_script_enabled=False)
        page = ctx.new_page()
        for name, path in ROUTES:
            try:
                page.goto(BASE + path, wait_until='load', timeout=30000)
                page.wait_for_timeout(500)
                r = page.evaluate(PROBE)
                res[name] = r
                page.screenshot(path=f'{SHOTS}/nojs-{name}.png')
                print(name, json.dumps(r)[:600], flush=True)
            except Exception as e:
                res[name] = {'error': str(e)[:200]}
        ctx.close(); browser.close()
    dump(outname, res)

run()
