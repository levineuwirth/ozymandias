"""Reduced-motion checks.
usage: python motion_run.py <browser> <outname>
Modes: os = emulate prefers-reduced-motion: reduce; site = localStorage reduce-motion=1 only.
"""
import sys
from playwright.sync_api import sync_playwright
from lib import *

browser_name, outname = sys.argv[1], sys.argv[2]
BASE = f'http://127.0.0.1:{PORT_NONE}'
MROUTES = ['home', 'essay-r6', 'essay-proofbroker', 'photo-index', 'photo-series', 'photo-map', 'music-index',
           'composition', 'score-reader', 'search', 'colophon', 'library', 'memento', 'cv-about']
MODES = ('os', 'site', 'none')

PROBE = r"""
() => {
  const ms = s => s.split(',').map(x => x.trim()).map(x => x.endsWith('ms') ? parseFloat(x) : parseFloat(x) * 1000);
  const out = {anim: [], trans: [], scrollBehavior: getComputedStyle(document.documentElement).scrollBehavior};
  const els = [document.documentElement, ...document.querySelectorAll('*')];
  for (const el of els) {
    for (const pseudo of [null, '::before', '::after']) {
      const cs = getComputedStyle(el, pseudo);
      if (cs.animationName !== 'none' && Math.max(...ms(cs.animationDuration)) > 1) {
        out.anim.push((el.tagName + '.' + (el.className && el.className.baseVal === undefined ? el.className : '')).slice(0, 60) + (pseudo || '') + ' ' + cs.animationName + ' ' + cs.animationDuration);
      }
      if (cs.transitionProperty !== 'none' && Math.max(...ms(cs.transitionDuration)) > 1) {
        out.trans.push((el.tagName + '.' + (typeof el.className === 'string' ? el.className : '')).slice(0, 60) + (pseudo || '') + ' ' + cs.transitionProperty.slice(0, 40) + ' ' + cs.transitionDuration);
      }
      if (cs.scrollBehavior === 'smooth' && el !== document.documentElement) out.smooth = (out.smooth || []).concat([el.tagName + '.' + el.className]);
    }
  }
  out.anim = out.anim.slice(0, 10); out.nTrans = out.trans.length; out.trans = out.trans.slice(0, 10);
  // running Web Animations (JS-driven, e.g. element.animate)
  try { out.webAnims = document.getAnimations().filter(a => a.playState === 'running').map(a => (a.animationName || a.constructor.name) + ' ' + (a.effect && a.effect.getTiming && a.effect.getTiming().duration)).slice(0, 10); } catch (e) {}
  return out;
}
"""

def run():
    res = {}
    with sync_playwright() as p:
        browser = getattr(p, browser_name).launch()
        for mode in MODES:
            ctx = browser.new_context(viewport={'width': 1440, 'height': 1000},
                                      reduced_motion='reduce' if mode == 'os' else 'no-preference')
            if mode == 'site':
                ctx.add_init_script("try { localStorage.setItem('reduce-motion', '1'); } catch (e) {}")
            offline(ctx)
            page = ctx.new_page()
            for name, path in ROUTES:
                if name not in MROUTES:
                    continue
                try:
                    page.goto(BASE + path, wait_until='load')
                    settle(page, 600)
                    r = page.evaluate(PROBE)
                    if name == 'photo-series':
                        btn = page.query_selector('[data-mode="slideshow"]')
                        r['slideshow'] = None   # no button: the test says so
                        if btn:
                            btn.click(); page.wait_for_timeout(1200)
                            r['slideshow'] = page.evaluate("""(() => { const bs = [...document.querySelectorAll('button')].filter(b => /pause|play/i.test(b.getAttribute('aria-label') || b.textContent));
                                return bs.map(b => (b.getAttribute('aria-label') || b.textContent).trim()); })()""")
                            page.keyboard.press('Escape'); page.wait_for_timeout(300)
                    res[f'{name}|{mode}'] = r
                    print(name, mode, 'anim', r['anim'][:3], 'nTrans', r['nTrans'], r['trans'][:2], 'sb', r['scrollBehavior'], 'web', r.get('webAnims'), 'smooth', r.get('smooth'), 'ss', r.get('slideshow'), flush=True)
                except Exception as e:
                    res[f'{name}|{mode}'] = {'error': str(e)[:200]}
                    print(name, mode, 'ERR', str(e)[:200])
            ctx.close()
        browser.close()
    dump(outname, res)

run()
