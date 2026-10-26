"""Horizontal overflow at several widths.
usage: python overflow_run.py <browser> <outname> [--widths=320,1440] [--text-size=29] [route ...]
"""
import sys
from playwright.sync_api import sync_playwright
from lib import *

browser_name, outname = sys.argv[1], sys.argv[2]
opts, routes = options(sys.argv[3:])
only = set(routes)
BASE = f'http://127.0.0.1:{PORT_NONE}'
WIDTHS = [320, 375, 768, 1440]
RUN_WIDTHS = [int(w) for w in opts['widths'].split(',')] if 'widths' in opts else WIDTHS
TEXT_SIZE = int(opts['text-size']) if 'text-size' in opts else None

PROBE = r"""
() => {
  const se = document.scrollingElement;
  const cw = se.clientWidth, sw = se.scrollWidth;
  const res = {cw, sw, over: sw > cw, offenders: []};
  if (sw <= cw) return res;
  function clipped(el) {
    // true if some ancestor clips/scrolls horizontally (then el can't widen the page)
    for (let a = el.parentElement; a && a !== document.body && a !== document.documentElement; a = a.parentElement) {
      const ox = getComputedStyle(a).overflowX;
      if (ox !== 'visible') return true;
    }
    return false;
  }
  const all = [...document.body.querySelectorAll('*')];
  const offs = [];
  for (const el of all) {
    const r = el.getBoundingClientRect();
    if (r.width === 0 && r.height === 0) continue;
    if (r.right > cw + 1 || r.left < -1) {
      const cs = getComputedStyle(el);
      if (cs.position === 'fixed') continue;
      if (clipped(el)) continue;
      offs.push(el);
    }
  }
  // keep outermost offenders only
  const outer = offs.filter(el => !offs.some(o => o !== el && o.contains(el)));
  function desc(el) {
    let s = el.tagName.toLowerCase();
    if (el.id) s += '#' + el.id;
    if (el.classList.length) s += '.' + [...el.classList].slice(0, 3).join('.');
    const p = el.parentElement;
    let ps = p ? p.tagName.toLowerCase() + (p.id ? '#' + p.id : '') + (p.classList.length ? '.' + [...p.classList].slice(0,2).join('.') : '') : '';
    return ps + ' > ' + s;
  }
  res.offenders = outer.slice(0, 8).map(el => {
    const r = el.getBoundingClientRect();
    return {el: desc(el), left: Math.round(r.left), right: Math.round(r.right), w: Math.round(r.width), text: (el.textContent || '').trim().slice(0, 60)};
  });
  // deepest offenders (leaf-ish) for diagnosis
  const inner = offs.filter(el => !offs.some(o => o !== el && el.contains(o)));
  res.leaves = inner.slice(0, 6).map(el => { const r = el.getBoundingClientRect(); return {el: desc(el), right: Math.round(r.right), w: Math.round(r.width), text: (el.textContent || '').trim().slice(0, 60)}; });
  return res;
}
"""

def run():
    res = {}
    with sync_playwright() as p:
        browser = getattr(p, browser_name).launch()
        for w in RUN_WIDTHS:
            ctx = browser.new_context(viewport={'width': w, 'height': 900}, reduced_motion='reduce')
            if TEXT_SIZE:
                ctx.add_init_script(TEXT_SIZE_INIT % TEXT_SIZE)
            offline(ctx)
            page = ctx.new_page()
            for name, path in ROUTES:
                if only and name not in only:
                    continue
                try:
                    page.goto(BASE + path, wait_until='load', timeout=30000)
                    settle(page, 800)
                    r = page.evaluate(PROBE)
                    res[f'{name}|{w}'] = r
                    if r['over']:
                        print(name, w, r['sw'], '>', r['cw'], [o['el'] for o in r['offenders']][:4], flush=True)
                        page.screenshot(path=f'{SHOTS}/overflow-{browser_name}-{name}-{w}.png', full_page=False)
                except Exception as e:
                    res[f'{name}|{w}'] = {'error': str(e)[:200]}
            ctx.close()
        browser.close()
    dump(outname, res)

run()
