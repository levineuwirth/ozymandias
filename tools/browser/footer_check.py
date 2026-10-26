import sys
from playwright.sync_api import sync_playwright
import os
from lib import REPO, SHOTS
want = open(os.path.join(REPO, '_site', 'build', 'time.txt')).read().strip()
with sync_playwright() as p:
    for name in ('chromium', 'firefox'):
        b = getattr(p, name).launch(); pg = b.new_page()
        errs = []
        pg.on('console', lambda m: errs.append(m.text) if m.type == 'error' else None)
        for path in ('/', '/essays/proof-broker/', '/photography/denmark/'):
            pg.goto('http://127.0.0.1:48791' + path, wait_until='networkidle')
            got = pg.locator('[data-build-time]').inner_text().strip()
            print(name, path, 'OK' if got == want else 'MISMATCH', repr(got))
        pg.locator('footer').screenshot(path=f'{SHOTS}/footer-{name}.png')
        print(name, 'console errors:', errs)
        b.close()
