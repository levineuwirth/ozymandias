"""Inline-code email actions, using current source files without a site build.

Native mail links are intercepted before navigation; no mail app is launched.
RUN_BROWSER_TESTS=1 .venv/bin/python -m unittest tests.test_browser_email -v
"""
from __future__ import annotations

import mimetypes
import unittest
from urllib.parse import quote, urlsplit

from tests._browser import BROWSERS, require_playwright, requires_browser
from tests._helpers import engine_path
from tests.test_browser_annotations import SELECT, STUBS

ORIGIN = "http://email.test"
PAGE = """<!doctype html><html lang="en"><head><meta charset="utf-8">
<link rel="stylesheet" href="/css/base.css">
<link rel="stylesheet" href="/css/selection-popup.css">
<script src="/js/utils.js"></script>
<script src="/js/selection-popup.js" defer></script>
</head><body><main id="markdownBody">
<p><code id="address">abuse@egihosting.com</code></p>
<p id="prose">abuse@egihosting.com</p>
<pre><code id="block" class="language-python">abuse@egihosting.com</code></pre>
</main></body></html>"""


@requires_browser
class EmailSelection(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def fixture(self, browser, width=1280):
        context = self.browsers[browser].new_context(viewport={"width": width, "height": 800})
        self.addCleanup(context.close)
        context.add_init_script(STUBS)

        def answer(route):
            url = urlsplit(route.request.url)
            if url.netloc != "email.test":
                route.abort()
            elif url.path == "/":
                route.fulfill(content_type="text/html", body=PAGE)
            else:
                path = engine_path("static/" + url.path.lstrip("/"))
                if path.is_file():
                    route.fulfill(content_type=mimetypes.guess_type(path.name)[0]
                                  or "application/octet-stream", body=path.read_bytes())
                else:
                    route.abort()

        context.route("**/*", answer)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda error: errors.append(str(error)))
        self.addCleanup(lambda: self.assertEqual(errors, []))
        page.goto(ORIGIN, wait_until="load")
        return page

    def select(self, page, text, selector="#address"):
        page.mouse.click(1, 1)
        page.evaluate(SELECT, [selector, text, text])
        self.expect(page.locator(".selection-popup.is-visible")).to_be_visible()
        return page.locator(".selection-popup.is-visible .selection-popup-btn").all_inner_texts()

    def test_copy_and_native_email_link(self):
        for browser in BROWSERS:
            for width in (1280, 320):
                with self.subTest(browser=browser, width=width):
                    page = self.fixture(browser, width)
                    address = "abuse@egihosting.com"
                    self.assertEqual(self.select(page, address), ["Copy", "Email"])
                    link = page.locator(".selection-popup a[data-action=email]")
                    self.expect(link).to_have_attribute("href", "mailto:" + address)
                    self.assertIsNone(link.get_attribute("target"))
                    bounds = link.bounding_box()
                    self.assertGreaterEqual(bounds["x"], 0)
                    self.assertLessEqual(bounds["x"] + bounds["width"], width)
                    # Capture the native destination without opening a mail app.
                    page.evaluate("""() => {
                        window.__mail = [];
                        document.addEventListener('click', event => {
                            const link = event.target.closest('a[href^="mailto:"]');
                            if (link) { event.preventDefault(); window.__mail.push(link.href); }
                        }, true);
                    }""")
                    link.focus()
                    page.keyboard.press("Enter")
                    self.assertEqual(page.evaluate("window.__mail"), ["mailto:" + address])
                    self.expect(page.locator(".selection-popup.is-visible")).to_have_count(0)
                    self.assertEqual(page.evaluate("window.__opened"), [])
                    self.select(page, address)
                    page.click(".selection-popup [data-action=copy]")
                    self.assertEqual(page.evaluate("window.__copied"), [address])

    def test_mailbox_characters_are_encoded_as_an_address(self):
        addresses = ["reader+letters@sub.example.org", "a&b@example.org",
                     "reader?subject=literal@example.org", "percent%2B@example.org",
                     "hash#tag@example.org", "o'brien@example.org"]
        for browser in BROWSERS:
            page = self.fixture(browser)
            for address in addresses:
                with self.subTest(browser=browser, address=address):
                    page.locator("#address").evaluate("(el, text) => el.textContent = text", address)
                    self.assertEqual(self.select(page, address), ["Copy", "Email"])
                    local, domain = address.split("@")
                    expected = "mailto:" + quote(local, safe="~()*!.'") + "@" + domain
                    href = page.locator("[data-action=email]").get_attribute("href")
                    self.assertEqual(href, expected)
                    self.assertEqual(urlsplit(href).query, "")
                    self.assertEqual(urlsplit(href).fragment, "")

    def test_other_selections_keep_their_existing_actions(self):
        invalid = ["abuse", "example.org", "a@@example.org", ".a@example.org",
                   "a..b@example.org", "a@-example.org", "a@example..org",
                   "a@example.org?subject=unexpected", "a@example.org b@example.org"]
        for browser in BROWSERS:
            page = self.fixture(browser)
            for text in invalid:
                with self.subTest(browser=browser, text=text):
                    page.locator("#address").evaluate("(el, text) => el.textContent = text", text)
                    self.assertEqual(self.select(page, text), ["Copy"])
            with self.subTest(browser=browser, context="partial address"):
                page.locator("#address").evaluate("el => el.textContent = 'abuse@egihosting.com'")
                self.assertEqual(self.select(page, "abuse"), ["Copy"])
                self.assertEqual(self.select(page, "buse@egihosting.com"), ["Copy"])
            with self.subTest(browser=browser, context="code block"):
                self.assertEqual(self.select(page, "abuse@egihosting.com", "#block"), ["Copy", "Docs"])
            with self.subTest(browser=browser, context="prose"):
                self.assertNotIn("Email", self.select(page, "abuse@egihosting.com", "#prose"))


if __name__ == "__main__":
    unittest.main()
