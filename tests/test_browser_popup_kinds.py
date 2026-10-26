"""The popups that need no outside answer, on built pages, in Chromium and
Firefox; and what every popup must do: let the pointer in, and stay in
the window.

tests/test_browser_popups.py covers the link providers on a fixture page.
These are the rest of static/js/popups.js, on the pages that carry them:
the definition of each epistemic term (data-ep-term) in an essay's
metadata strip, the epistemic figure's preview of the block it links to,
the footer's PGP signature, and an item card's revision note. Requests
leaving serve.py are refused and fail the test.

Checked: every term a page tags has its definition (peer-status had none,
and showed nothing); the figure previews the strip and the expanded block;
the signature popup shows the page's .sig (answered here: signing is a
deploy step, so a built _site has none), by pointer and by keyboard,
and stays while pointed at (a hidden popup after the footer made the page
taller, and placing it pulled a reader at the foot off the link); the
revision note gives the dates and the note the card carries; a pointer
moving from a link into its popup keeps it open, and leaving lets it go
(WCAG 1.4.13); a hidden popup is inert, and Tab from the signature link
never lands inside one (the popup is last in <body>, and Firefox made its
scrollable <pre> a stop); a popup the keyboard is on, at its link or
inside it, stays while the pointer passes, and Escape from inside returns
to the link without reopening it; and each popup lies inside the window,
the footer's at its corner included.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_popup_kinds -v
"""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, FakeNetwork, check_site, enforcing_csp, require_playwright,
                            requires_browser, site_server)
from tests._browser import fake_answer as answer

ESSAY = "/essays/proof-broker/"
PEER_REVIEWED = "/essays/beyond-comorbidity-indices/"   # carries a peer-status
LIST = "/new.html"

POPUP = ".link-popup.is-visible"

# The pages' .sig files come from signing at deploy (tools/sign-site.sh), not
# from `make build`, so a freshly built _site has none: the signature popup
# is answered here, as a same-origin request.
SIGNATURE = ("-----BEGIN PGP SIGNATURE-----\n\niHUEABYKAB0WIQTJpCpvrURPvlZv1zhTG9wcwnBwZgUC"
             "\n-----END PGP SIGNATURE-----\n")
INSIDE = """() => { const r = document.querySelector('.link-popup').getBoundingClientRect();
    return r.left >= 0 && r.top >= 0 && r.right <= innerWidth && r.bottom <= innerHeight; }"""


@requires_browser
class PopupKinds(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-popup-kinds-")))
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp()))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def open(self, browser: str, path: str, width: int = 1440):
        context = self.browsers[browser].new_context(viewport={"width": width, "height": 900},
                                                     reduced_motion="reduce")
        self.addCleanup(context.close)
        net = FakeNetwork(context, self.base, {"sig": r"\.html\.sig$"},
                          {"sig": answer(SIGNATURE, "application/pgp-signature")}, local=r".+\.html\.sig$")
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))

        def clean():
            self.assertEqual(errors, [], "page errors")
            self.assertEqual(net.unexpected, [], "requests leaving serve.py")
        self.addCleanup(clean)
        page.goto(self.base + path, wait_until="load")
        return page

    def show(self, page, locator):
        """Point at `locator` and return the popup once it shows, checked
        to lie inside the window."""
        page.mouse.move(1, 1)
        self.expect(page.locator(POPUP)).to_have_count(0)
        locator.scroll_into_view_if_needed()
        locator.hover()
        popup = page.locator(POPUP)
        self.expect(popup).to_be_visible()
        self.assertTrue(page.evaluate(INSIDE), "the popup runs past the window")
        return popup

    def test_every_term_has_its_definition(self) -> None:
        for browser in BROWSERS:
            for path in (ESSAY, PEER_REVIEWED):
                page = self.open(browser, path)
                terms = page.eval_on_selector_all(
                    ".meta-epistemic-strip [data-ep-term]", "es => es.map(e => e.dataset.epTerm)")
                self.assertTrue(terms, path)
                for term in terms:
                    with self.subTest(browser=browser, page=path, term=term):
                        popup = self.show(page, page.locator(f'.meta-epistemic-strip [data-ep-term="{term}"]'))
                        self.expect(popup.locator(".popup-ep-term .popup-title")).to_have_text(
                            term.replace("-", " ").capitalize())
                        # textContent: innerText is empty while the popup still fades in.
                        self.assertGreater(len(popup.locator(".popup-abstract").text_content()), 40)
                        self.expect(popup.locator('a[href="/colophon.html#living-documents"]')).to_have_count(1)
            with self.subTest(browser=browser, part="peer status is among them"):
                self.assertIn("peer-status", terms)

    def test_the_figure_previews_the_epistemic_block(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser, ESSAY)
                popup = self.show(page, page.locator('.frontmatter-mark-slot a[href="#epistemic"]'))
                self.expect(popup.locator(".popup-epistemic .meta-epistemic-strip")).to_have_count(1)
                self.expect(popup.locator(".popup-epistemic .ep-expanded")).to_have_count(1)

    def test_the_signature(self) -> None:
        for browser in BROWSERS:
            for how in ("pointer", "keyboard"):
                with self.subTest(browser=browser, how=how):
                    page = self.open(browser, LIST)
                    link = page.locator("a.footer-sig-link")
                    if how == "pointer":
                        link.scroll_into_view_if_needed()
                        height = page.evaluate("document.documentElement.scrollHeight")
                        popup = self.show(page, link)
                        # Still there past the hide delay, the page unmoved: a
                        # hidden popup after the footer made the page taller, and
                        # its placing pulled a reader at the foot off the link.
                        page.wait_for_timeout(600)
                        self.expect(popup).to_be_visible()
                        self.assertEqual(page.evaluate("document.documentElement.scrollHeight"), height)
                    else:
                        link.scroll_into_view_if_needed()
                        link.focus()
                        popup = page.locator(POPUP)
                        self.expect(popup).to_be_visible()
                        self.assertTrue(page.evaluate(INSIDE), "the popup runs past the window")
                    self.expect(popup.locator(".popup-sig pre")).to_contain_text(
                        "-----BEGIN PGP SIGNATURE-----")

    STATE = """() => { const p = document.querySelector('.link-popup'), a = document.activeElement;
        return {inert: p.inert, shown: p.classList.contains('is-visible'),
                inside: p.contains(a), onLink: a.classList.contains('footer-sig-link')}; }"""

    def keyboard_to_signature(self, page):
        """The footer's signature link by keyboard: it is the page's last
        stop (the hidden popup after it is inert), so Shift+Tab from the
        top reaches it, focus-visible as a reader's would be."""
        # popups.js binds its links once its annotations have loaded.
        self.expect(page.locator("a.footer-sig-link")).to_have_attribute("data-popup-bound", "1")
        page.keyboard.press("Shift+Tab")        # from a fresh page, nothing focused
        self.assertTrue(page.evaluate(self.STATE)["onLink"], "Shift+Tab did not reach the signature")
        popup = page.locator(POPUP)
        self.expect(popup).to_be_visible()
        return popup

    def pass_pointer(self, page, box) -> None:
        """The pointer over `box` and away to the empty margin beside it,
        past the hide delay. (Not across the page: pointing at another
        link on the way is a new request, and shows that link's popup.)"""
        y = box["y"] + box["height"] / 2
        page.mouse.move(box["x"] + box["width"] / 2, y, steps=4)
        page.mouse.move(min(page.viewport_size["width"] - 5, box["x"] + box["width"] + 80), y, steps=4)
        page.wait_for_timeout(600)

    def test_the_keyboard_and_popups(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser, part="hidden, inert; Tab never lands in one"):
                page = self.open(browser, LIST)
                self.assertTrue(page.evaluate(self.STATE)["inert"])
                self.keyboard_to_signature(page)
                page.keyboard.press("Tab")
                page.wait_for_timeout(600)
                now = page.evaluate(self.STATE)
                self.assertEqual(now["shown"], now["inside"], f"focus and popup disagree: {now}")
            with self.subTest(browser=browser, part="the keyboard on the link, the pointer passing"):
                page = self.open(browser, LIST)
                self.keyboard_to_signature(page)
                self.pass_pointer(page, page.locator("a.footer-sig-link").bounding_box())
                now = page.evaluate(self.STATE)
                self.assertEqual((now["shown"], now["onLink"]), (True, True), "hidden while focused")
            with self.subTest(browser=browser, part="focus inside, the pointer passing, Escape"):
                page = self.open(browser, LIST)
                popup = self.keyboard_to_signature(page)
                # Into the popup, as Tab takes Firefox (its scrollable <pre> is a stop).
                page.evaluate("() => { const pre = document.querySelector('.link-popup pre');"
                              " pre.tabIndex = -1; pre.focus(); }")
                self.assertTrue(page.evaluate(self.STATE)["inside"])
                self.pass_pointer(page, popup.bounding_box())
                now = page.evaluate(self.STATE)
                self.assertEqual((now["shown"], now["inside"], now["inert"]), (True, True, False),
                                 "the pointer leaving hid the popup the keyboard was in")
                page.keyboard.press("Escape")
                now = page.evaluate(self.STATE)
                self.assertEqual((now["shown"], now["onLink"], now["inert"]), (False, True, True))
                page.wait_for_timeout(800)      # well past the show delay
                self.assertFalse(page.evaluate(self.STATE)["shown"], "returning focus reopened it")

    def test_the_revision_note(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser, LIST)
                # A revision's note is optional: the first card that has one.
                marker = page.locator("li.item-card:has(.item-card-revision-note) .item-card-revised").first
                card = marker.locator("xpath=ancestor::li[contains(@class, 'item-card')]")
                date = card.locator(".item-card-date").inner_text().strip()
                note = card.locator(".item-card-revision-note").text_content().strip()
                popup = self.show(page, marker)
                self.expect(popup.locator(".popup-revision .popup-meta")).to_contain_text(f"Revised {date} · from ")
                self.assertEqual(" ".join(popup.locator(".popup-revision .popup-abstract").text_content().split()),
                                 " ".join(note.split()))

    def test_the_pointer_can_enter_the_popup(self) -> None:
        # WCAG 1.4.13: content shown on hover can itself be hovered.
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser, ESSAY)
                trigger = page.locator('.meta-epistemic-strip [data-ep-term="confidence"]')
                popup = self.show(page, trigger)
                t, p = trigger.bounding_box(), popup.bounding_box()
                page.mouse.move(t["x"] + t["width"] / 2, t["y"] + t["height"] / 2)
                page.mouse.move(p["x"] + p["width"] / 2, p["y"] + min(20, p["height"] / 2), steps=8)
                page.wait_for_timeout(600)      # well past the hide delay
                self.expect(popup).to_be_visible()
                page.mouse.move(5, 890, steps=4)
                self.expect(page.locator(POPUP)).to_have_count(0)


if __name__ == "__main__":
    unittest.main()
