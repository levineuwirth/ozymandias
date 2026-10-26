"""Keyboard: tab order, focus visibility, hidden-focus, skip link, traps, Escape.
usage: python kbd_run.py <browser> <outname>
"""
import sys
from playwright.sync_api import sync_playwright
from lib import *

browser_name, outname = sys.argv[1], sys.argv[2]
BASE = f'http://127.0.0.1:{PORT_NONE}'
STOPS = 40

KROUTES = ['home', 'essay-proofbroker', 'essay-r6', 'essay-grd', 'photo-index', 'photo-series', 'photo-single',
           'search', 'composition', 'score-reader', 'links', 'library', 'commonplace', 'music-index', 'me', 'current']
VIEWPORTS = [(1440, 1000), (375, 812)]

FOCUS_INFO = r"""
() => {
  const el = document.activeElement;
  if (!el || el === document.body) return {body: true};
  const r = el.getBoundingClientRect();
  const cs = getComputedStyle(el);
  let ah = null, hid = null, inert = null, clipped = null;
  for (let a = el; a; a = a.parentElement) {
    if (a.getAttribute && a.getAttribute('aria-hidden') === 'true' && !ah) ah = a.tagName + '.' + a.className;
    if (a.hidden && !hid) hid = a.tagName + '.' + a.className;
    if (a.inert && !inert) inert = a.tagName + '.' + a.className;
  }
  // Clipped: a box above it that clips its overflow leaves none of it
  // showing (a collapsed container). Only boxes that do clip it count. A
  // fixed element escapes those below its containing block, which is the
  // viewport unless an ancestor is transformed, filtered or contained; an
  // absolutely positioned one, those below its containing block, the
  // nearest positioned ancestor or one of those. (Opacity and visibility
  // are `visible`'s.)
  const holdsFixed = c => c.transform !== 'none' || c.perspective !== 'none' || c.filter !== 'none'
    || (c.backdropFilter || 'none') !== 'none' || /paint|layout|strict|content/.test(c.contain)
    || /transform|perspective|filter/.test(c.willChange);
  const holdsAbsolute = c => c.position !== 'static' || holdsFixed(c);
  if (r.width > 0 && r.height > 0) {
    let waiting = cs.position === 'fixed' ? holdsFixed : cs.position === 'absolute' ? holdsAbsolute : null;
    for (let a = el.parentElement; a && !clipped; a = a.parentElement) {
      const c = getComputedStyle(a);
      if (waiting && !waiting(c)) continue;   // a box it escapes
      waiting = null;
      if (c.display !== 'contents' && (c.overflowX !== 'visible' || c.overflowY !== 'visible')) {
        const b = a.getBoundingClientRect();
        const w = Math.min(r.right, b.right) - Math.max(r.left, b.left);
        const h = Math.min(r.bottom, b.bottom) - Math.max(r.top, b.top);
        if (w < 1 || h < 1) clipped = a.tagName + '.' + a.className;
      }
      if (c.position === 'fixed') waiting = holdsFixed;
      else if (c.position === 'absolute') waiting = holdsAbsolute;
    }
  }
  const desc = el.tagName.toLowerCase() + (el.id ? '#' + el.id : '') + (el.classList.length ? '.' + [...el.classList].slice(0,3).join('.') : '');
  const name = (el.getAttribute('aria-label') || el.textContent || el.getAttribute('title') || el.getAttribute('alt') || '').trim().slice(0, 50);
  const ind = {outline: cs.outlineStyle + ' ' + cs.outlineWidth + ' ' + cs.outlineColor, shadow: cs.boxShadow, bg: cs.backgroundColor, td: cs.textDecorationLine, border: cs.borderBottomStyle + ' ' + cs.borderBottomWidth + ' ' + cs.borderBottomColor};
  const fv = el.matches(':focus-visible');
  const inView = r.width > 0 && r.height > 0 && r.bottom > 0 && r.top < innerHeight && r.right > 0 && r.left < innerWidth;
  // The browser's own answer to "can this be seen": opacity, visibility, content-visibility, display.
  const visible = el.checkVisibility ? el.checkVisibility({checkOpacity: true, checkVisibilityCSS: true}) : null;
  return {desc, name, href: el.getAttribute('href'), w: Math.round(r.width), h: Math.round(r.height), inView, ariaHidden: ah, hidden: hid, inert, clipped, visible, ind, fv};
}
"""

UNFOCUSED_STYLE = r"""
(el) => { const cs = getComputedStyle(el); return {outline: cs.outlineStyle + ' ' + cs.outlineWidth + ' ' + cs.outlineColor, shadow: cs.boxShadow, bg: cs.backgroundColor, td: cs.textDecorationLine, border: cs.borderBottomStyle + ' ' + cs.borderBottomWidth + ' ' + cs.borderBottomColor}; }
"""

# Until the focus has finished moving the page and changing how it looks:
# two frames in a row with the same scroll position and the focused element
# in the same place, and no CSS transition still running on it or an
# ancestor (section toggles fade in on focus; measured mid-fade, a loaded
# machine reads them at opacity 0), at most 1 s. Firefox finishes scrolling
# focus into view about 60 ms after the key, and a fixed 150 ms per Tab was
# most of a walk's time.
FOCUS_SETTLED = r"""
() => new Promise(done => {
  const end = performance.now() + 1000;
  let last = null, still = 0;
  const fading = a => document.getAnimations().some(an =>
    an instanceof CSSTransition && (an.pending || an.playState === 'running')
    && an.effect && an.effect.target && an.effect.target.contains(a));
  (function tick() {
    const a = document.activeElement, r = a && a.getBoundingClientRect();
    const key = scrollX + ',' + scrollY + ',' + (r ? r.top + ',' + r.left : '');
    if (key === last) still++; else { still = 0; last = key; }
    if ((still >= 2 && !(a && fading(a))) || performance.now() > end) return done();
    requestAnimationFrame(tick);
  })();
})
"""


def tab_walk(page, n=STOPS):
    stops = []
    for i in range(n):
        page.keyboard.press('Tab')
        page.evaluate(FOCUS_SETTLED)
        info = page.evaluate(FOCUS_INFO)
        if info.get('body'):
            stops.append({'i': i, 'body': True}); continue
        # compare against unfocused style
        try:
            h = page.evaluate_handle('document.activeElement')
            page.evaluate('el => { el.__lnBlur = true; }', h)
            # clone-free approach: blur, read, refocus
            page.evaluate('el => el.blur()', h)
            un = page.evaluate(UNFOCUSED_STYLE, h)
            page.evaluate('el => el.focus({preventScroll: true})', h)
            # refocus via keyboard semantics isn't possible; :focus-visible may differ after programmatic focus
            info['unfocused'] = un
            info['indicator_diff'] = un != info['ind']
        except Exception as e:
            info['cmp_err'] = str(e)[:100]
        info['i'] = i
        stops.append(info)
    return stops


def run():
    res = {}
    with sync_playwright() as p:
        browser = getattr(p, browser_name).launch()
        for vp in VIEWPORTS:
            ctx = browser.new_context(viewport={'width': vp[0], 'height': vp[1]}, reduced_motion='reduce')
            offline(ctx)
            page = ctx.new_page()
            for name, path in ROUTES:
                if name not in KROUTES:
                    continue
                key = f'{name}|{vp[0]}'
                rec = {}
                try:
                    page.goto(BASE + path, wait_until='load')
                    settle(page, 800)
                    rec['stops'] = tab_walk(page)
                    first = rec['stops'][0] if rec['stops'] else {}
                    rec['skip'] = {'first': first.get('name'), 'href': first.get('href')}
                    if first.get('href', '') and first.get('href').startswith('#'):
                        tgt = first['href'][1:]
                        rec['skip']['target_exists'] = page.evaluate('id => !!document.getElementById(id)', tgt)
                        # activate skip link and check next focus lands in main
                        page.goto(BASE + path, wait_until='load'); settle(page, 300)
                        page.keyboard.press('Tab'); page.keyboard.press('Enter'); page.wait_for_timeout(300)
                        page.keyboard.press('Tab'); page.wait_for_timeout(100)
                        rec['skip']['after'] = page.evaluate("(() => { const el = document.activeElement; const m = document.getElementById('" + tgt + "'); return {desc: el.tagName + '.' + el.className, inTarget: !!(m && m.contains(el))}; })()")
                except Exception as e:
                    rec['err'] = str(e)[:300]
                res[key] = rec
                bad = [s for s in rec.get('stops', []) if not s.get('body') and (s.get('ariaHidden') or s.get('hidden') or s.get('inert') or s.get('clipped') or not s.get('inView') or not s.get('indicator_diff'))]
                print(key, 'stops', len(rec.get('stops', [])), 'suspicious', len(bad), 'skip', rec.get('skip'), flush=True)
                for s in bad[:12]:
                    print('   ', s.get('i'), s.get('desc'), repr(s.get('name')), 'aH=%s hid=%s inert=%s clip=%s view=%s diff=%s' % (s.get('ariaHidden'), s.get('hidden'), s.get('inert'), s.get('clipped'), s.get('inView'), s.get('indicator_diff')), flush=True)
            ctx.close()
        browser.close()
    dump(outname, res)

run()
