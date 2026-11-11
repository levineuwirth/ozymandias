"""Transclusion, in Chromium and Firefox.

static/js/transclude.js fills each div.transclude with a page fetched by
its data-src (its #markdownBody, or one section), the fetched page's own
transclusions included, and shares one request between the
transclusions of one URL. A fixture page transcludes two fixture pages,
A and B, and B transcludes A again; A's answers are scripted here.

Checked: a passage that loads is shown, and a URL transcluded twice is
asked once; one that fails says so and links to its source; a failed
request is not kept: it was, for the life of the page, so a later
transclusion of the same URL (B's) failed too without asking again; and a
transcluded section brings its own notes, and only those, from the list at
the end of its page, renamed with it, so its references still open them
(the script paired them by a hardcoded id prefix the renaming defeated); and
what arrives is announced to the page's other scripts, under the name they
listen for: a code block in a passage gets copy.js's button, which nothing
else gives it; and math arriving on a page that has none loads KaTeX's
stylesheet with the KaTeX sheet it imports preloaded beside it, so that
request does not wait for the first (both were asked for one after the
other).

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_transclude -v
"""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, FakeNetwork, check_site, enforcing_csp, imported_beside, page_styles,
                            require_playwright, requires_browser, site_server)
from tests._browser import fake_answer as answer
from tests.target import requires_switch

HOST = """<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Transclusion fixture</title>
<script src="/js/utils.js"></script>
<script src="/js/transclude.js" defer></script>
</head><body><main id="markdownBody">
<div class="transclude" id="t-a" data-src="/__fixture/a.html"></div>
<div class="transclude" id="t-b" data-src="/__fixture/b.html"></div>
</main></body></html>
"""
A = """<!doctype html><html lang="en"><head><title>A</title></head><body>
<main id="markdownBody"><p class="from-a">Passage A.</p></main></body></html>
"""
B = """<!doctype html><html lang="en"><head><title>B</title></head><body>
<main id="markdownBody"><p class="from-b">Passage B, which quotes A:</p>
<div class="transclude" data-src="/__fixture/a.html"></div></main></body></html>
"""

# A page with notes, and a host that transcludes its second section. The
# notes live in the list at the end of the source page, as
# Filters/Sidenotes.hs writes them.
NOTES_SOURCE = """<!doctype html><html lang="en"><head><title>Notes</title></head><body>
<main id="markdownBody">
<h2 id="one">One</h2>
<p>The first section, with a note<sup class="sidenote-ref" id="snref-a"><a href="#fn-a" role="doc-noteref">a</a></sup>.</p>
<h2 id="two">Two</h2>
<p class="from-two">The second section, with a note<sup class="sidenote-ref" id="snref-b"><a href="#fn-b" role="doc-noteref">b</a></sup>.</p>
<section class="footnotes" role="doc-endnotes"><ol class="footnotes-list">
<li id="fn-a" class="footnote-item"><span class="footnote-label" aria-hidden="true">a</span><p>Note a, of the first section.<a href="#snref-a" class="footnote-back" role="doc-backlink" aria-label="Back to reference a">\u21a9\ufe0e</a></p></li>
<li id="fn-b" class="footnote-item"><span class="footnote-label" aria-hidden="true">b</span><p>Note b, of the second section.<a href="#snref-b" class="footnote-back" role="doc-backlink" aria-label="Back to reference b">\u21a9\ufe0e</a></p></li>
</ol></section>
</main></body></html>
"""
NOTES_HOST = """<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Transcluded notes</title>
{styles}
<script src="/js/utils.js"></script>
<script src="/js/transclude.js" defer></script>
<script src="/js/sidenotes.js" defer></script>
</head><body><div class="page-shell"><main id="markdownBody">
<p>The host's own text comes first.</p>
<div class="transclude" id="t-two" data-src="/__fixture/notes-source.html" data-section="two"></div>
</main></div></body></html>
"""

# A passage with a code block, and a host that also runs copy.js.
CODE_SOURCE = """<!doctype html><html lang="en"><head><title>Code</title></head><body>
<main id="markdownBody"><pre><code class="language-python">print("arrived")</code></pre>
</main></body></html>
"""
CODE_HOST = """<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Transcluded code</title>
<script src="/js/utils.js"></script>
<script src="/js/transclude.js" defer></script>
<script src="/js/copy.js" defer></script>
</head><body><main id="markdownBody">
<div class="transclude" id="t-code" data-src="/__fixture/code-source.html"></div>
</main></body></html>
"""

# A passage with math, and a host page with none: nothing of KaTeX's is
# loaded until the passage arrives.
MATH_SOURCE = """<!doctype html><html lang="en"><head><title>Math</title></head><body>
<main id="markdownBody"><p class="from-math">Euler: <span class="math inline">e^{i\\pi} + 1 = 0</span></p>
</main></body></html>
"""
MATH_HOST = """<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Transcluded math</title>
<script src="/js/utils.js"></script>
<script src="/js/transclude.js" defer></script>
</head><body><main id="markdownBody">
<div class="transclude" id="t-math" data-src="/__fixture/math-source.html"></div>
</main></body></html>
"""


@requires_switch("transclusion")
@requires_browser
class Transclusion(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-transclude-")))
        (tmp / "fixtures").mkdir()
        (tmp / "fixtures" / "host.html").write_text(HOST, encoding="utf-8")
        (tmp / "fixtures" / "notes-source.html").write_text(NOTES_SOURCE, encoding="utf-8")
        (tmp / "fixtures" / "notes-host.html").write_text(
            NOTES_HOST.format(styles=page_styles()), encoding="utf-8")
        (tmp / "fixtures" / "code-source.html").write_text(CODE_SOURCE, encoding="utf-8")
        (tmp / "fixtures" / "code-host.html").write_text(CODE_HOST, encoding="utf-8")
        (tmp / "fixtures" / "math-source.html").write_text(MATH_SOURCE, encoding="utf-8")
        (tmp / "fixtures" / "math-host.html").write_text(MATH_HOST, encoding="utf-8")
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp(), tmp / "fixtures"))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def host(self, browser: str, a, b):
        """The host page with A and B answered by `a` and `b` (callables
        taking the route); the page's errors and any request leaving
        serve.py are checked at cleanup."""
        context = self.browsers[browser].new_context()
        self.addCleanup(context.close)
        net = FakeNetwork(context, self.base, {"a": r"/__fixture/a\.html$", "b": r"/__fixture/b\.html$"},
                          {"a": a, "b": b}, local=r"__fixture/[ab]\.html$")
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))

        def clean():
            self.assertEqual(errors, [], "page errors")
            self.assertEqual(net.unexpected, [], "requests leaving serve.py")
        self.addCleanup(clean)
        page.goto(self.base + "/__fixture/host.html", wait_until="load")
        return page, net

    def test_a_passage_loads_and_one_request_serves_a_url(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, net = self.host(browser, lambda r: r.fulfill(**answer(A, "text/html")),
                                      lambda r: r.fulfill(**answer(B, "text/html")))
                self.expect(page.locator("#t-a .from-a")).to_have_text("Passage A.")
                self.expect(page.locator("#t-b .from-b")).to_be_visible()
                self.expect(page.locator("#t-b .from-a")).to_have_text("Passage A.")
                self.expect(page.locator("#t-a")).to_have_class("transclude transclude--loaded")
                self.assertEqual(net.hits["a"], 1)

    def test_a_failed_request_is_not_kept(self) -> None:
        # A's first request is refused; B is answered only after that, so
        # B's own transclusion of A comes later and must ask again.
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                state = {"a": 0, "b": None}

                def a(route):
                    state["a"] += 1
                    if state["a"] > 1:
                        route.fulfill(**answer(A, "text/html"))
                        return
                    route.abort()
                    if state["b"]:
                        state["b"].fulfill(**answer(B, "text/html"))

                def b(route):
                    if state["a"]:
                        route.fulfill(**answer(B, "text/html"))
                    else:
                        state["b"] = route

                page, net = self.host(browser, a, b)
                error = page.locator("#t-a .transclude-error-note")
                self.expect(error).to_contain_text("This passage could not be loaded.")
                self.expect(error.locator("a")).to_have_attribute("href", "/__fixture/a.html")
                self.expect(page.locator("#t-b .from-a")).to_have_text("Passage A.")
                self.assertEqual(net.hits["a"], 2)

    @requires_switch("code")
    def test_what_arrives_is_announced_to_the_page_s_other_scripts(self) -> None:
        # copy.js has run before the passage arrives; only the announcement,
        # under the name copy.js listens for, gives the block its button.
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                context = self.browsers[browser].new_context()
                self.addCleanup(context.close)
                page = context.new_page()
                errors = []
                page.on("pageerror", lambda e: errors.append(str(e)))
                page.goto(self.base + "/__fixture/code-host.html", wait_until="load")
                passage = page.locator("#t-code")
                self.expect(passage).to_have_class("transclude transclude--loaded")
                self.expect(passage.locator("pre code.language-python")).to_be_visible()
                self.expect(passage.locator(".copy-btn")).to_have_count(1)
                self.assertEqual(errors, [])

    @requires_switch("math")
    def test_math_on_a_cold_page_does_not_wait_for_katex_s_stylesheet(self) -> None:
        # modules/katex.css @imports katex.min.css into the engine's layer.
        # Held, it must not hold that sheet back: transclude.js preloads it
        # beside the stylesheet; answered, the sheet is imported, asked for
        # once, and the passage's math is typeset.
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                context = self.browsers[browser].new_context()
                self.addCleanup(context.close)
                page = imported_beside(self, context, self.base + "/__fixture/math-host.html",
                                       "/css/modules/katex.css", ["/katex/katex.min.css"])
                errors = []
                page.on("pageerror", lambda e: errors.append(str(e)))
                self.expect(page.locator("#t-math .from-math .katex")).to_have_count(1)
                self.assertEqual(errors, [])

    def test_a_transcluded_section_brings_its_notes(self) -> None:
        for browser in BROWSERS:
            for width in (1600, 700):
                with self.subTest(browser=browser, width=width):
                    context = self.browsers[browser].new_context(viewport={"width": width, "height": 900})
                    self.addCleanup(context.close)
                    page = context.new_page()
                    errors = []
                    page.on("pageerror", lambda e: errors.append(str(e)))
                    page.goto(self.base + "/__fixture/notes-host.html", wait_until="load")
                    passage = page.locator("#t-two")
                    self.expect(passage.locator(".from-two")).to_be_visible()
                    # Its own note, renamed with it, and not the other section's.
                    notes = passage.locator(".footnote-item")
                    self.expect(notes).to_have_count(1)
                    self.expect(notes).to_contain_text("Note b, of the second section.")
                    note_id = notes.get_attribute("id")
                    self.assertNotEqual(note_id, "fn-b")
                    link = passage.locator(".sidenote-ref a")
                    self.expect(link).to_have_attribute("href", "#" + note_id)
                    link.click()
                    if width == 1600:
                        # Beside its reference, in the margin: its first
                        # line on the reference's line (baselines, read
                        # from zero-size boxes set on each; the reference's
                        # after it, where it cannot move a line break).
                        self.expect(notes).to_be_focused()
                        shift = passage.evaluate("""t => {
                            const base = (insert) => {
                                const probe = document.createElement('span');
                                probe.style.cssText = 'display:inline-block;width:0;height:0;vertical-align:baseline';
                                insert(probe);
                                const y = probe.getBoundingClientRect().top;
                                probe.remove();
                                return y;
                            };
                            const note = t.querySelector('.footnote-item');
                            return base(p => note.querySelector('p').prepend(p))
                                 - base(p => t.querySelector('.sidenote-ref').after(p));
                        }""")
                        self.assertLessEqual(abs(shift), 2)
                        box = notes.bounding_box()
                        body_right = page.locator("#markdownBody").evaluate(
                            "b => b.getBoundingClientRect().right")
                        self.assertGreater(box["x"], body_right)
                    else:
                        sheet = page.locator("dialog.sidenote-popup")
                        self.expect(sheet).to_be_visible()
                        self.expect(sheet).to_contain_text("Note b, of the second section.")
                        page.keyboard.press("Escape")
                        self.expect(link).to_be_focused()
                    self.assertEqual(errors, [], "page errors")


if __name__ == "__main__":
    unittest.main()
