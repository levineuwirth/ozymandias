"""The photography slideshow's keyboard, in Chromium and Firefox (audit J10).

Space presses whichever control has focus, as on any button, and toggles
playback only when focus is elsewhere. It used to toggle playback
everywhere: Space on a focused Next started the slideshow and left the
frame where it was, and Space on Close never closed it. Opening focuses
Play/Pause, so Space straight after opening still pauses.

A fixture grid of three photographs (data: images) drives the real
photography-slideshow.js under the enforcing CSP.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_slideshow -v
"""

from __future__ import annotations

import re
import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, check_site, enforcing_csp, require_playwright,
                            requires_browser, site_server)

CARD = """<figure class="photo-card"><a class="photo-card-link" href="/photography/">
<img src="data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='40' height='30'%3E%3Crect width='40' height='30' fill='%23{fill}'/%3E%3C/svg%3E" alt="{title}"></a>
<span class="photo-card-title">{title}</span><span class="photo-card-date">2026</span></figure>"""

PAGE = """<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Slideshow fixture</title>
<script src="/js/utils.js"></script>
<script src="/js/photography-slideshow.js" defer></script>
</head><body><main>
<button type="button" data-mode="slideshow">Slideshow</button>
<div class="photography-grid">{cards}</div>
</main></body></html>
""".format(cards="\n".join(CARD.format(fill=f, title=t)
                           for f, t in (("a33", "One"), ("3a3", "Two"), ("33a", "Three"))))

PLAY = ".slideshow-play"
OPEN = re.compile(r"\bis-open\b")


@requires_browser
class SlideshowKeyboard(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-slideshow-")))
        (tmp / "fixtures").mkdir()
        (tmp / "fixtures" / "slideshow.html").write_text(PAGE, encoding="utf-8")
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp(), tmp / "fixtures"))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def opened(self, browser: str, reduced_motion: str = "no-preference"):
        """The fixture with its slideshow just opened (and playing, unless
        reduced motion is asked for). window.__focusedAs records each
        slideshow control's accessible name at the moment it took focus."""
        context = self.browsers[browser].new_context(reduced_motion=reduced_motion)
        self.addCleanup(context.close)
        context.add_init_script("""window.__focusedAs = [];
            document.addEventListener('focusin', e => {
                if (e.target.closest('.slideshow-overlay'))
                    window.__focusedAs.push(e.target.getAttribute('aria-label'));
            });""")
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        self.addCleanup(lambda: self.assertEqual(errors, []))
        page.goto(self.base + "/__fixture/slideshow.html", wait_until="networkidle")
        page.click("[data-mode=slideshow]")
        self.expect(page.locator(".slideshow-counter")).to_have_text("1 of 3")
        return page

    def label(self, page) -> str:
        return page.get_attribute(PLAY, "aria-label")

    def test_opening_focuses_play_and_space_pauses(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.opened(browser)
                self.assertEqual(page.evaluate("document.activeElement.className"),
                                 "slideshow-btn slideshow-play")
                self.assertEqual(self.label(page), "Pause slideshow")
                page.keyboard.press("Space")
                self.assertEqual(self.label(page), "Play slideshow")
                page.keyboard.press("Space")
                self.assertEqual(self.label(page), "Pause slideshow")

    def test_focus_lands_on_the_name_the_control_keeps(self) -> None:
        # A screen reader announces the name a control has when it takes
        # focus; it must already say what the control will do.
        for browser in BROWSERS:
            for motion, name in (("no-preference", "Pause slideshow"),
                                 ("reduce", "Play slideshow")):
                with self.subTest(browser=browser, reduced_motion=motion):
                    page = self.opened(browser, motion)
                    self.assertEqual(page.evaluate("window.__focusedAs"), [name])
                    self.assertEqual(self.label(page), name)

    def test_space_presses_the_focused_control(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser, control="next"):
                page = self.opened(browser)
                page.focus(".slideshow-next")
                page.keyboard.press("Space")
                self.expect(page.locator(".slideshow-counter")).to_have_text("2 of 3")
                # Steering pauses; it does not start the clock.
                self.assertEqual(self.label(page), "Play slideshow")
            with self.subTest(browser=browser, control="previous"):
                page.focus(".slideshow-prev")
                page.keyboard.press("Space")
                self.expect(page.locator(".slideshow-counter")).to_have_text("1 of 3")
            with self.subTest(browser=browser, control="close"):
                page.focus(".slideshow-close")
                page.keyboard.press("Space")
                self.expect(page.locator(".slideshow-overlay")).not_to_have_class(OPEN)
                self.assertEqual(page.evaluate("document.activeElement.dataset.mode"), "slideshow")

    def test_space_elsewhere_toggles_playback(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.opened(browser)
                # Tall enough to scroll, so a Space the slideshow failed to
                # take would show (the fixture alone is shorter than the window).
                page.evaluate("document.body.style.minHeight = '5000px'")
                page.evaluate("document.activeElement.blur()")
                page.keyboard.press("Space")
                self.assertEqual(self.label(page), "Play slideshow")
                # Browsers animate a keyboard scroll: give one time to move.
                page.wait_for_timeout(500)
                self.assertEqual(page.evaluate("window.scrollY"), 0)


if __name__ == "__main__":
    unittest.main()
