"""CSP enforcement + console + failed requests + feature probes per route.

usage: python csp_run.py <chromium|firefox> <port> <outname> [--base=URL] [route-name ...]

With --base (e.g. https://levineuwirth.org) it sweeps that site instead of
serve.py on <port>, the site's own CSP in force, and sends no CSP report
(lib.suppress_reports): test traffic must never reach the production
report log. Violations are still recorded, from the page's
securitypolicyviolation events.
"""
import sys, json, time, re, os
from playwright.sync_api import sync_playwright
from lib import *

browser_name, port, outname = sys.argv[1], int(sys.argv[2]), sys.argv[3]
opts, routes = options(sys.argv[4:])
only = set(routes)
BASE = opts.get('base', f'http://127.0.0.1:{port}').rstrip('/')
LIVE = 'base' in opts      # any site named outright gets no reports from us


def features(page, name):
    """Route-specific interactions that exercise CSP-relevant features."""
    out = {}
    ev = page.evaluate
    # generic signals
    out['katex'] = ev("document.querySelectorAll('.katex').length")
    out['katex_err'] = ev("document.querySelectorAll('.katex-error').length")
    out['math_raw'] = ev("document.querySelectorAll('span.math, .math.inline, .math.display').length")
    out['katex_css'] = ev("[...document.styleSheets].some(s => (s.href||'').includes('katex'))")
    out['imgs_broken'] = ev("""[...document.images].filter(i => i.complete && i.naturalWidth === 0 && i.getAttribute('src') && i.loading !== 'lazy').map(i => i.currentSrc || i.src).slice(0, 10)""")
    out['transclude_unfilled'] = ev("document.querySelectorAll('div.transclude:empty').length")
    out['transclude_total'] = ev("document.querySelectorAll('div.transclude').length")

    if name in ('links', 'essay-grd', 'essay-scaling', 'essay-proofbroker', 'essay-simd', 'essay-r6', 'memento', 'work', 'essay-verified'):
        out['popups'] = hover_popups(page)
    if name == 'search':
        out['search'] = search_probe(page)
    if name == 'photo-map':
        page.wait_for_timeout(3000)
        out['map'] = ev("""({tiles: document.querySelectorAll('.leaflet-tile').length,
            tilesLoaded: document.querySelectorAll('.leaflet-tile-loaded').length,
            markers: document.querySelectorAll('.leaflet-marker-icon').length,
            err: (document.querySelector('.photography-map-error, [class*=error]')||{}).textContent || null})""")
    if name == 'photo-series':
        btn = page.query_selector('[data-mode="slideshow"]')
        out['slideshow_btn'] = bool(btn)
        if btn:
            try:
                btn.click(timeout=3000); page.wait_for_timeout(1500)
                out['slideshow'] = ev("""(() => { const d = document.querySelector('.slideshow-overlay.is-open');
                    const img = d && d.querySelector('img');
                    return {open: !!d, cls: d && d.className, img: img && img.currentSrc, ok: img && img.naturalWidth > 0}; })()""")
                page.screenshot(path=f'{SHOTS}/{browser_name}-{port}-slideshow.png')
                page.keyboard.press('Escape'); page.wait_for_timeout(500)
            except Exception as e:
                out['slideshow_err'] = str(e)[:200]
    if name in ('photo-single', 'essay-simd'):
        img = page.query_selector('img[data-lightbox]')
        out['lightbox_img'] = bool(img)
        if img:
            try:
                img.scroll_into_view_if_needed(); img.click(timeout=3000); page.wait_for_timeout(800)
                out['lightbox'] = ev("""(() => { const o = document.querySelector('.lightbox-overlay, .lightbox, [class*=lightbox][class*=open]');
                    return o ? {cls: o.className, visible: getComputedStyle(o).display !== 'none' && getComputedStyle(o).visibility !== 'hidden'} : null; })()""")
                page.keyboard.press('Escape'); page.wait_for_timeout(400)
            except Exception as e:
                out['lightbox_err'] = str(e)[:200]
    if name == 'score-reader':
        out['score'] = score_probe(page)
    if name == 'pdfjs':
        try:
            page.wait_for_selector('.page canvas, .canvasWrapper canvas', timeout=15000)
        except Exception:
            pass
        out['pdf'] = ev("""({pages: document.querySelectorAll('.page').length,
            canvases: document.querySelectorAll('.page canvas').length,
            text: document.querySelectorAll('.textLayer span').length,
            err: (document.querySelector('#errorWrapper:not([hidden]), .dialog[open]')||{}).textContent || null})""")
        out['pdf'].update(pdf_blob_probe(page, out['pdf']['pages']))
    if name in ('archive-snapshot', 'archive-pdf'):
        page.wait_for_timeout(1500)
        out['iframe'] = [{'url': f.url, 'title': (f.title() if f != page.main_frame else None)} for f in page.frames if f != page.main_frame]
    if name in ('cv-about', 'work', 'cv-projects'):
        # PDF embed / pdf popup link
        out['pdf_links'] = ev("[...document.querySelectorAll('a[href*=\"pdfjs\"], iframe[src*=\"pdfjs\"]')].map(a => a.outerHTML.slice(0, 160)).slice(0, 4)")
    if name == 'composition':
        # The score's thumbnail, by name: counting images also counts the
        # lightbox's, which has no source until it opens.
        out['composition'] = ev("""({thumb: (i => i && {src: i.currentSrc, loaded: i.complete && i.naturalWidth > 0})
                (document.querySelector('.comp-frontispiece img')),
            broken: [...document.images].filter(i => i.getAttribute('src') && i.complete && i.naturalWidth === 0).map(i => i.src).slice(0,5),
            audio: document.querySelectorAll('audio').length})""")
    if name == 'colophon':
        page.wait_for_timeout(1500)
        out['transclude_after'] = ev("[...document.querySelectorAll('div.transclude')].map(d => ({cls: d.className, len: d.textContent.length, html: d.outerHTML.slice(0,200)}))")
    if name == 'commonplace':
        # commonplace.js was an inline script: the toggle is what the CSP could stop.
        try:
            page.click('.cp-toggle-btn[data-target="cp-chrono"]', timeout=3000)
            out['cp'] = ev("""({chrono: !document.getElementById('cp-chrono').hidden,
                themed: !document.getElementById('cp-themed').hidden})""")
        except Exception as e:
            out['cp_err'] = str(e)[:200]
    return out


# Until the popup is shown with its images loaded, or `ms` have passed: its
# requests are what meet the policy. A fixed 2.6 s per kind was most of an
# essay's time. Polled in the page: wait_for_function's string predicate is
# an eval, which the enforcing policy refuses.
POPUP_READY = r"""
ms => new Promise(done => {
  const t0 = performance.now();
  (function tick() {
    const p = document.querySelector('.link-popup');
    const ready = !!p && p.classList.contains('is-visible')
      && [...p.querySelectorAll('img')].every(i => i.complete);
    if (ready || performance.now() - t0 > ms) return done(ready);
    setTimeout(tick, 50);
  })();
})
"""


def hover_popups(page):
    res = []
    pats = [
        ('wikipedia', 'a[href*="wikipedia.org/wiki/"]'),
        ('doi', 'a[href*="doi.org/"]'),
        ('arxiv', 'a[href*="arxiv.org/abs/"]'),
        ('github-code', 'a[href^="https://github.com/"][href*="/blob/"]'),
        ('github-repo', 'a[href^="https://github.com/"]:not([href*="/blob/"]):not([href*="/tree/"])'),
        ('forge', 'a[href*="git.levineuwirth.org"]'),
        ('citation', 'a.cite-link, a[href^="#ref-"]'),
        ('internal', '#markdownBody a[href^="/"]:not([href*="#"]), #markdownBody a[href^="./"], #markdownBody a[href^="../"]'),
        ('pdf', 'a.pdf-link[data-pdf-src]'),
        ('openlibrary', 'a[href*="openlibrary.org"]'),
    ]
    for kind, sel in pats:
        els = page.query_selector_all(sel)
        if not els:
            continue
        el = None
        for e in els[:8]:
            try:
                if e.is_visible():
                    el = e; break
            except Exception:
                pass
        if el is None:
            continue
        href = el.get_attribute('href')
        try:
            el.scroll_into_view_if_needed(timeout=2000)
            page.mouse.move(0, 0); page.wait_for_timeout(300)
            el.hover(timeout=2000)
            if page.evaluate(POPUP_READY, 2600):
                page.wait_for_timeout(200)     # a request the popup makes once shown
            info = page.evaluate("""(() => { const p = document.querySelector('.link-popup');
                if (!p) return null;
                const imgs = [...p.querySelectorAll('img')].map(i => ({src: i.currentSrc || i.src, ok: i.complete && i.naturalWidth > 0}));
                return {visible: p.classList.contains('is-visible') || getComputedStyle(p).opacity !== '0' && getComputedStyle(p).visibility !== 'hidden',
                        cls: p.className, text: p.innerText.slice(0, 160), imgs}; })()""")
            res.append({'kind': kind, 'href': href, 'popup': info})
        except Exception as e:
            res.append({'kind': kind, 'href': href, 'err': str(e)[:160]})
    page.mouse.move(0, 0)
    return res


def poll(page, expr, secs):
    """Wait until `expr` is true in the page. Not wait_for_function: it
    evaluates its predicate from a string, which the enforcing policy
    blocks (and reports) as eval."""
    deadline = time.time() + secs
    while not page.evaluate(expr):
        if time.time() > deadline:
            raise TimeoutError(f'{expr} still false after {secs} s')
        page.wait_for_timeout(200)


def pdf_blob_probe(page, pages):
    """PDF.js's sidebar thumbnails and its print output: both are
    <img src="blob:..."> it draws from a canvas (img-src blob:). Print
    renders every page into #printContainer, calls the browser's print and
    empties the container afterwards, so its loads are counted as they
    happen."""
    out = {}
    loaded_thumbs = ("[...document.querySelectorAll('#thumbnailsView img')].filter(i =>"
                     " i.src.startsWith('blob:') && i.complete && i.naturalWidth > 0).length")
    try:
        # Open on a wide viewport already; opened here if not.
        if page.get_attribute('#viewsManagerToggleButton', 'aria-expanded') != 'true':
            page.click('#viewsManagerToggleButton', timeout=3000)
        poll(page, f'{loaded_thumbs} > 0', 10)
    except Exception as e:
        out['thumbs_err'] = str(e)[:200]
    out['thumbs_loaded'] = page.evaluate(loaded_thumbs)
    page.evaluate("""() => {
        window.__printLoads = 0;
        document.addEventListener('load', e => {
            const t = e.target;
            if (t instanceof HTMLImageElement && t.src.startsWith('blob:')
                && t.closest('#printContainer')) window.__printLoads++;
        }, true);
        setTimeout(() => window.print(), 0);
    }""")
    try:
        poll(page, f'window.__printLoads >= {pages}', 20)
    except Exception as e:
        out['print_err'] = str(e)[:200]
    out['print_loaded'] = page.evaluate('window.__printLoads')
    return out


def search_probe(page):
    out = {}
    try:
        inp = page.wait_for_selector('#search input, .pagefind-ui__search-input', timeout=8000)
        inp.fill('domination')
        page.wait_for_timeout(2500)
        out['keyword_results'] = page.evaluate("document.querySelectorAll('.pagefind-ui__result').length")
        out['keyword_msg'] = page.evaluate("(document.querySelector('.pagefind-ui__message')||{}).textContent || null")
    except Exception as e:
        out['keyword_err'] = str(e)[:200]
    try:
        page.click('#search-tab-semantic', timeout=3000)
        page.fill('#semantic-query', 'graph domination on trees')
        page.keyboard.press('Enter')
        t0 = time.time()
        while time.time() - t0 < 90:
            n = page.evaluate("document.querySelectorAll('#semantic-results li, .semantic-result, .semantic-results > *').length")
            st = page.evaluate("(document.getElementById('semantic-status')||{}).textContent || ''")
            if n > 0 or re.search(r'(?i)fail|error|could not|unavailable', st or ''):
                break
            page.wait_for_timeout(1000)
        out['semantic_results'] = n
        out['semantic_status'] = st
        out['semantic_secs'] = round(time.time() - t0, 1)
    except Exception as e:
        out['semantic_err'] = str(e)[:200]
    return out


def score_probe(page):
    out = {}
    try:
        page.wait_for_selector('#score-reader-stage svg, #score-reader-stage img', timeout=10000)
    except Exception:
        pass
    out['stage'] = page.evaluate("""(() => { const s = document.getElementById('score-reader-stage');
        return s ? {svgs: s.querySelectorAll('svg').length, imgs: s.querySelectorAll('img').length, timing: s.dataset.timing} : null; })()""")
    try:
        btn = page.wait_for_selector('.score-follow-play', timeout=8000)
        btn.click()
        page.wait_for_timeout(3500)
        out['audio'] = page.evaluate("""(() => { const p = document.querySelector('.score-follow-play');
            return {pressed: p && p.getAttribute('aria-pressed'), label: p && p.getAttribute('aria-label'),
              time: (document.querySelector('.score-follow-time')||{}).textContent || null}; })()""")
        btn.click()
    except Exception as e:
        out['audio_err'] = str(e)[:200]
    folio = "(document.getElementById('score-folio') || {}).textContent || null"
    try:
        # Play follows the music, which need not start on page 1.
        out['before_next'] = page.evaluate(folio)
        page.click('#score-next', timeout=3000); page.wait_for_timeout(1200)
        out['after_next'] = page.evaluate(folio)
    except Exception as e:
        out['next_err'] = str(e)[:200]
    return out


def run():
    results = {}
    with sync_playwright() as p:
        bt = getattr(p, browser_name)
        launch_args = {}
        if browser_name == 'chromium':
            launch_args['args'] = ['--autoplay-policy=no-user-gesture-required']
        else:
            launch_args['firefox_user_prefs'] = {'media.autoplay.default': 0, 'media.autoplay.blocking_policy': 0}
        browser = bt.launch(**launch_args)
        for name, path in ROUTES:
            if only and name not in only:
                continue
            ctx = browser.new_context(viewport={'width': 1440, 'height': 1000})
            ctx.add_init_script(CSP_INIT)
            if LIVE:
                suppress_reports(ctx)
            page = ctx.new_page()
            rec = {'path': path, 'console': [], 'pageerrors': [], 'failed': [], 'bad_status': [], 'requests': 0}
            page.on('console', lambda m, rec=rec: rec['console'].append({'type': m.type, 'text': m.text[:400], 'loc': (m.location or {}).get('url', '')}) if m.type in ('error', 'warning') else None)
            page.on('pageerror', lambda e, rec=rec: rec['pageerrors'].append(str(e)[:400]))
            page.on('requestfailed', lambda r, rec=rec: rec['failed'].append({'url': r.url[:200], 'err': r.failure, 'type': r.resource_type}))
            def onresp(r, rec=rec):
                rec['requests'] += 1
                if r.status >= 400:
                    rec['bad_status'].append({'url': r.url[:200], 'status': r.status})
            page.on('response', onresp)
            t0 = time.time()
            rec['status'] = None
            try:
                resp = page.goto(BASE + path, wait_until='load', timeout=30000)
                rec['status'] = resp and resp.status
            except Exception as e:
                rec['goto_err'] = str(e)[:200]
            rec['load_secs'] = round(time.time() - t0, 1)
            settle(page)
            scroll_through(page)
            settle(page, 400)
            try:
                rec['features'] = features(page, name)
            except Exception as e:
                rec['features_err'] = str(e)[:300]
            page.wait_for_timeout(500)
            rec['csp'] = []
            rec['jserrs'] = []
            rec['reserrs'] = []
            for f in page.frames:
                try:
                    rec['csp'] += f.evaluate('window.__cspv || []')
                    rec['jserrs'] += f.evaluate('window.__errs || []')
                    rec['reserrs'] += f.evaluate('window.__reserrs || []')
                except Exception:
                    pass
            rec['secs'] = round(time.time() - t0, 1)
            if rec['csp'] or rec['pageerrors']:
                try:
                    page.screenshot(path=f'{SHOTS}/{browser_name}-{port}-{name}.png')
                except Exception:
                    pass
            results[name] = rec
            print(name, 'csp=%d console=%d pageerr=%d failed=%d bad=%d' % (len(rec['csp']), len(rec['console']), len(rec['pageerrors']), len(rec['failed']), len(rec['bad_status'])), flush=True)
            ctx.close()
        browser.close()
    dump(outname, results)

run()
