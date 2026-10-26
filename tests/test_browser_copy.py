"""Copy buttons on code blocks, in Chromium and Firefox.

static/js/copy.js gives each <pre> a copy button beside it, not inside it
(outside its scroll box, out of the copied text), except in chrome such as
popups, and again for code that arrives later (ln:content-added). The
clipboard is stubbed here, so nothing reaches the machine's own.

Checked: one button per block, a sibling of the <pre>, none in excluded
places, and still one after content is announced twice; the text copied is
the block's exactly, without the newline Pandoc ends it with; "Copied" and
"Copy failed" are shown and announced, then reset; and the button, unseen
until wanted, is seen when the keyboard reaches it.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_copy -v
"""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, check_site, enforcing_csp, page_styles, require_playwright,
                            requires_browser, site_server)

PAGE = """<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Copy fixture</title>
{styles}
<script src="/js/copy.js" defer></script>
</head><body><main id="markdownBody">
<p>A block with a fence's trailing newline:</p>
<pre id="python"><code class="language-python">def increment(x):
    return x + 1
</code></pre>
<pre id="bare">plain preformatted text</pre>
<div class="link-popup"><pre id="in-popup">not copyable</pre></div>
<div data-no-copy><pre id="opted-out">not copyable either</pre></div>
<div id="later"></div>
</main></body></html>
"""

# The clipboard, recorded or refused as window.__refuse says.
STUBS = """
window.__copied = []; window.__refuse = false;
Object.defineProperty(navigator, 'clipboard', {configurable: true, value: {
    writeText: text => window.__refuse ? Promise.reject(new Error('denied'))
                                       : (window.__copied.push(text), Promise.resolve())}});
"""


@requires_browser
class CopyButtons(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-copy-")))
        (tmp / "fixtures").mkdir()
        (tmp / "fixtures" / "copy.html").write_text(PAGE.format(styles=page_styles()), encoding="utf-8")
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp(), tmp / "fixtures"))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def open(self, browser: str):
        context = self.browsers[browser].new_context()
        self.addCleanup(context.close)
        context.add_init_script(STUBS)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        self.addCleanup(lambda: self.assertEqual(errors, [], "page errors"))
        page.goto(self.base + "/__fixture/copy.html", wait_until="load")
        return page

    def buttons(self, page) -> dict:
        """pre id -> the copy buttons beside it, in any wrapper around it (a
        block wrapped twice has two), and inside it."""
        return page.evaluate("""() => Object.fromEntries([...document.querySelectorAll('pre')].map(p =>
            [p.id, {beside: [...document.querySelectorAll('.copy-btn')]
                        .filter(b => b.parentElement.contains(p) && !p.contains(b)).length,
                    inside: p.querySelectorAll('.copy-btn').length}]))""")

    def test_where_buttons_go(self) -> None:
        one, none = {"beside": 1, "inside": 0}, {"beside": 0, "inside": 0}
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                self.assertEqual(self.buttons(page), {"python": one, "bare": one,
                                                      "in-popup": none, "opted-out": none})
                # Code that arrives later, announced twice: one button.
                page.evaluate("""() => { const later = document.getElementById('later');
                    later.innerHTML = '<pre id="added"><code>echo added</code></pre>';
                    for (let i = 0; i < 2; i++) later.dispatchEvent(new CustomEvent('ln:content-added',
                        {bubbles: true, detail: {container: later}})); }""")
                self.assertEqual(self.buttons(page)["added"], one)

    def press(self, page, pre: str) -> None:
        """Point at the block, as a reader does: the idle button takes no
        pointer events until then (components.css), then click it."""
        page.hover(f"#{pre}")
        page.locator(f"#{pre} + .copy-btn").click()

    def test_copying(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                btn = page.locator("#python + .copy-btn")
                status = page.locator("#python ~ [role=status]")
                self.expect(btn).to_have_accessible_name("Copy code to clipboard")
                self.press(page, "python")
                self.assertEqual(page.evaluate("window.__copied"),
                                 ["def increment(x):\n    return x + 1"])
                self.expect(btn).to_have_attribute("data-state", "copied")
                self.expect(status).to_have_text("Copied")
                # Back to idle after a moment.
                self.expect(btn).not_to_have_attribute("data-state", "copied", timeout=4000)
                self.expect(status).to_have_text("")
                page.evaluate("window.__refuse = true")
                self.press(page, "python")
                self.expect(btn).to_have_attribute("data-state", "error")
                self.expect(status).to_have_text("Copy failed")
                self.press(page, "bare")
                self.assertEqual(page.evaluate("window.__copied.length"), 1)

    def test_seen_when_the_keyboard_reaches_it(self) -> None:
        opacity = "b => parseFloat(getComputedStyle(b).opacity)"
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                btn = page.locator("#python + .copy-btn")
                page.mouse.move(1, 1)
                self.assertLess(btn.evaluate(opacity), 0.5, "the idle button should keep out of sight")
                page.keyboard.press("Tab")
                self.expect(btn).to_be_focused()
                btn.evaluate("b => Promise.all(b.getAnimations().map(a => a.finished.catch(() => null)))")
                self.assertEqual(btn.evaluate(opacity), 1)


if __name__ == "__main__":
    unittest.main()
