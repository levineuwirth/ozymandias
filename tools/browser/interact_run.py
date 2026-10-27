"""Keyboard interaction checks: settings, lightbox, slideshow, math gallery, popups, portals, filters.
Runs against the ENFORCING server so CSP violations in these paths are also captured.
usage: python interact_run.py <browser> <outname>
"""
import sys
from playwright.sync_api import sync_playwright
from lib import *

browser_name, outname = sys.argv[1], sys.argv[2]
BASE = f'http://127.0.0.1:{PORT_ENFORCE}'
AE = "(() => { const e = document.activeElement; return e ? (e.tagName.toLowerCase() + (e.id ? '#' + e.id : '') + '.' + (typeof e.className === 'string' ? e.className : '').split(' ').slice(0, 2).join('.') + ' ' + JSON.stringify((e.getAttribute('aria-label') || e.textContent || '').trim().slice(0, 30))) : null; })()"

def focus_by_tab(page, selector, maxn=120):
    for i in range(maxn):
        page.keyboard.press('Tab')
        if page.evaluate(f"document.activeElement && document.activeElement.matches({selector!r})"):
            return i + 1
    return None

def tab_cycle(page, n=8):
    seq = []
    for _ in range(n):
        page.keyboard.press('Tab'); page.wait_for_timeout(50)
        seq.append(page.evaluate(AE))
    return seq

def run():
    res = {}
    with sync_playwright() as p:
        browser = getattr(p, browser_name).launch()
        ctx = browser.new_context(viewport={'width': 1440, 'height': 1000}, reduced_motion='reduce')
        ctx.add_init_script(CSP_INIT)
        page = ctx.new_page()
        errs = []
        page.on('pageerror', lambda e: errs.append(str(e)[:200]))

        # 1. settings panel
        page.goto(BASE + SITE_ROUTES.role('essay')); settle(page, 500)
        r = {}
        r['tabs_to_toggle'] = focus_by_tab(page, '.settings-toggle')
        page.keyboard.press('Enter'); page.wait_for_timeout(300)
        r['open'] = page.evaluate("document.querySelector('.settings-panel').classList.contains('is-open')")
        r['focus_after_open'] = page.evaluate(AE)
        r['cycle'] = tab_cycle(page, 12)
        r['panel_open_after_tabbing'] = page.evaluate("document.querySelector('.settings-panel').classList.contains('is-open')")
        page.keyboard.press('Escape'); page.wait_for_timeout(300)
        r['open_after_escape'] = page.evaluate("document.querySelector('.settings-panel').classList.contains('is-open')")
        r['focus_after_escape'] = page.evaluate(AE)
        # theme switching via keyboard
        page.focus('.settings-toggle'); page.keyboard.press('Enter'); page.wait_for_timeout(200)
        page.focus('[data-action="theme-dark"]'); page.keyboard.press('Enter'); page.wait_for_timeout(300)
        r['theme_after'] = page.evaluate("document.documentElement.getAttribute('data-theme')")
        r['theme_btn_pressed'] = page.evaluate("[...document.querySelectorAll('[data-action^=theme-]')].map(b => [b.dataset.action, b.getAttribute('aria-pressed'), b.classList.contains('is-active')])")
        page.keyboard.press('Escape')
        res['settings'] = r; print('settings', r, flush=True)

        # 2. portals toggle
        page.goto(BASE + SITE_ROUTES.role('essay')); settle(page, 300)
        r = {}
        page.focus('.nav-portal-toggle'); page.keyboard.press('Enter'); page.wait_for_timeout(200)
        r['expanded'] = page.evaluate("document.querySelector('.nav-portal-toggle').getAttribute('aria-expanded')")
        r['next'] = tab_cycle(page, 2)
        page.focus('.nav-portal-toggle'); page.keyboard.press('Enter')
        res['portals'] = r; print('portals', r, flush=True)

        # 3. lightbox (photo single)
        page.goto(BASE + SITE_ROUTES.role('photo')); settle(page, 500)
        r = {}
        r['tabs_to_img'] = focus_by_tab(page, 'img[data-lightbox], [data-lightbox]')
        r['focused'] = page.evaluate(AE)
        page.keyboard.press('Enter'); page.wait_for_timeout(500)
        r['open'] = page.evaluate("!!document.querySelector('.lightbox-overlay.is-open')")
        r['focus_in_overlay'] = page.evaluate("!!(document.querySelector('.lightbox-overlay.is-open') || {contains: () => false}).contains(document.activeElement)")
        r['cycle'] = tab_cycle(page, 6)
        r['cycle_contained'] = page.evaluate("!!(document.querySelector('.lightbox-overlay.is-open') || {contains: () => false}).contains(document.activeElement)")
        page.keyboard.press('Escape'); page.wait_for_timeout(400)
        r['open_after_escape'] = page.evaluate("!!document.querySelector('.lightbox-overlay.is-open')")
        r['focus_after'] = page.evaluate(AE)
        res['lightbox'] = r; print('lightbox', r, flush=True)

        # 4. slideshow (series)
        page.goto(BASE + SITE_ROUTES.role('photo-series')); settle(page, 500)
        r = {}
        r['tabs_to_btn'] = focus_by_tab(page, '[data-mode="slideshow"]')
        page.keyboard.press('Enter'); page.wait_for_timeout(800)
        r['open'] = page.evaluate("!!document.querySelector('.slideshow-overlay.is-open')")
        r['img_ok'] = page.evaluate("(() => { const i = document.querySelector('.slideshow-img'); return i ? [i.currentSrc.slice(-50), i.naturalWidth] : null; })()")
        r['focus'] = page.evaluate(AE)
        r['cycle'] = tab_cycle(page, 7)
        r['contained'] = page.evaluate("!!document.querySelector('.slideshow-overlay.is-open') && document.querySelector('.slideshow-overlay').contains(document.activeElement)")
        r['autoplay_label'] = page.evaluate("(document.querySelector('.slideshow-play')||{}).getAttribute && document.querySelector('.slideshow-play').getAttribute('aria-label')")
        page.keyboard.press('ArrowRight'); page.wait_for_timeout(500)
        r['after_arrow'] = page.evaluate("(document.querySelector('.slideshow-img')||{}).currentSrc || null")
        page.screenshot(path=f'{SHOTS}/{browser_name}-slideshow-enforce.png')
        page.keyboard.press('Escape'); page.wait_for_timeout(400)
        r['open_after_escape'] = page.evaluate("!!document.querySelector('.slideshow-overlay.is-open')")
        r['focus_after'] = page.evaluate(AE)
        res['slideshow'] = r; print('slideshow', r, flush=True)

        # 5. math gallery overlay
        page.goto(BASE + SITE_ROUTES.role('essay-gallery')); settle(page, 1000)
        r = {}
        r['tabs_to_math'] = focus_by_tab(page, '.math-focusable', 300)
        page.keyboard.press('Enter'); page.wait_for_timeout(500)
        r['overlay'] = page.evaluate("(() => { const o = [...document.querySelectorAll('[role=dialog], .gallery-overlay, .exhibit-overlay')].find(x => !x.hidden && getComputedStyle(x).display !== 'none'); return o ? o.className + ' ' + (o.getAttribute('aria-label') || '') : null; })()")
        r['cycle'] = tab_cycle(page, 6)
        page.keyboard.press('Escape'); page.wait_for_timeout(400)
        r['focus_after'] = page.evaluate(AE)
        res['math_gallery'] = r; print('math', r, flush=True)

        # 6. link popups on keyboard focus
        page.goto(BASE + SITE_ROUTES.role('essay')); settle(page, 800)
        r = {}
        r['tabs_to_internal'] = focus_by_tab(page, '#markdownBody a[data-popup-bound]', 200)
        r['focused'] = page.evaluate(AE)
        page.wait_for_timeout(2500)
        r['popup_on_focus'] = page.evaluate("(() => { const p = document.querySelector('.link-popup'); return p ? p.classList.contains('is-visible') : null; })()")
        page.keyboard.press('Escape'); page.wait_for_timeout(300)
        r['popup_after_escape'] = page.evaluate("(() => { const p = document.querySelector('.link-popup'); return p ? p.classList.contains('is-visible') : null; })()")
        res['popup_keyboard'] = r; print('popup', r, flush=True)

        # 7. hover popup Escape (WCAG 1.4.13 dismissible)
        el = page.query_selector('#markdownBody a[data-popup-bound]')
        el.hover(); page.wait_for_timeout(2500)
        r2 = {'visible_on_hover': page.evaluate("document.querySelector('.link-popup').classList.contains('is-visible')")}
        page.keyboard.press('Escape'); page.wait_for_timeout(300)
        r2['after_escape'] = page.evaluate("document.querySelector('.link-popup').classList.contains('is-visible')")
        res['popup_hover_escape'] = r2; print('popup hover esc', r2, flush=True)

        # 8. search filters toggle
        page.goto(BASE + '/search.html'); settle(page, 500)
        r = {}
        page.focus('.library-filter-toggle'); page.keyboard.press('Enter'); page.wait_for_timeout(300)
        r['expanded'] = page.evaluate("document.querySelector('.library-filter-toggle').getAttribute('aria-expanded')")
        r['hidden'] = page.evaluate("document.getElementById('search-filters').hidden")
        # tabs arrow-key behaviour
        page.focus('#search-tab-keyword'); page.keyboard.press('ArrowRight'); page.wait_for_timeout(300)
        r['arrow_focus'] = page.evaluate(AE)
        r['semantic_selected'] = page.evaluate("document.getElementById('search-tab-semantic').getAttribute('aria-selected')")
        r['panel_ids'] = page.evaluate("[...document.querySelectorAll('[role=tabpanel]')].map(p => [p.id, p.getAttribute('aria-labelledby'), p.hidden])")
        res['search'] = r; print('search', r, flush=True)

        # 9. sidenote / footnote keyboard at 1440 (sidenotes visible in margin)
        page.goto(BASE + SITE_ROUTES.role('essay-sidenotes')); settle(page, 500)
        res['sidenotes'] = page.evaluate("({n: document.querySelectorAll('.footnote-item').length, refs: document.querySelectorAll('.sidenote-ref, a.footnote-ref, sup a').length})")

        res['pageerrors'] = errs
        res['csp'] = page.evaluate('window.__cspv || []')
        print('errors', errs, flush=True)
        ctx.close(); browser.close()
    dump(outname, res)

run()
