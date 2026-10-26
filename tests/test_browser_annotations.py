"""Highlights and the selection toolbar, in Chromium and Firefox.

annotations.js keeps a reader's highlights in localStorage, keyed by
page, and finds their text again on every load; selection-popup.js
offers a toolbar for a selection, one of whose actions (Annotate) makes
a highlight. A fixture page, styled as an essay, carries a paragraph
with a source line break inside its text, as Pandoc leaves them, an
inline element, code and math.

Checked: a highlight across a line break is shown and found again after
a reload (audit J02: it used to be stored but never shown), as is one
across an inline element, and one across a paragraph break, which leaves
the paragraphs as they were (it used to make four of two); a phrase that
occurs twice is highlighted, and found again, where it was selected, not
at its first occurrence; malformed stored highlights are ignored, not
fatal; the Annotate picker opens inside the window when the selection
fills it, takes focus without scrolling, and stays open as the page
scrolls (it opened out of sight, and closed when scrolled to); a passage
that cannot be highlighted says so and stores nothing;
colours; the tooltip, its escaped note, Delete, and its keyboard path;
the toolbar's buttons for prose, a single word, code, math and text
outside the page's body (no Annotate); on a phone the toolbar and every
button in it inside the window (one row ran past the screen's edge, half
its actions out of reach); and what each action opens or
copies (window.open and the clipboard are stubbed, so nothing leaves the
machine).

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_annotations -v
"""

from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path
from urllib.parse import quote

from tests._browser import (BROWSERS, check_site, enforcing_csp, page_styles, require_playwright,
                            requires_browser, site_server)

PAGE = """<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Reading fixture — Levi Neuwirth</title>
{styles}
<script src="/js/utils.js"></script>
<script src="/js/annotations.js" defer></script>
<script src="/js/selection-popup.js" defer></script>
</head><body><div class="page-shell"><main id="markdownBody">
<h1 class="page-title">Reading Fixture</h1>
<p id="wrapped">As a result, three
accounts are given of the same event, each of them partial.</p>
<p id="inline">A phrase with <em>emphasis inside</em> it, and then more.</p>
<p id="plain">Serendipity favours the prepared mind.</p>
<p id="echo">The same event, told again: each of them partial.</p>
<pre><code class="language-python">def increment(x):
    return x + 1</code></pre>
<p id="math">Consider <span class="math inline">x^2 + y^2</span> here.</p>
</main>
<footer id="outside"><p>Serendipity, outside the text.</p></footer>
</div></body></html>
"""

KEY = "site-annotations"

# Until no transition on the element is running. A transition replaced
# before it ends rejects its `finished` (AbortError): that is settled too.
SETTLED = "el => Promise.all(el.getAnimations().map(a => a.finished.catch(() => null)))"

# window.open and the clipboard, recorded instead of used.
STUBS = """
window.__opened = [];
window.open = (url, target, features) => { window.__opened.push(url); return null; };
window.__copied = [];
Object.defineProperty(navigator, 'clipboard', {configurable: true, value: {
    writeText: text => { window.__copied.push(text); return Promise.resolve(); }}});
"""

# Select `start`…`end` (first occurrences, in textContent, newlines and all)
# under `selector`, then release the mouse as a reader would.
SELECT = """([selector, start, end]) => {
    const el = document.querySelector(selector);
    const nodes = [];
    const walker = document.createTreeWalker(el, NodeFilter.SHOW_TEXT);
    let n, pos = 0;
    while ((n = walker.nextNode())) { nodes.push([n, pos]); pos += n.nodeValue.length; }
    const text = el.textContent;
    const a = text.indexOf(start);
    const b = text.indexOf(end, a) + end.length;
    const at = (i, isEnd) => {
        for (const [node, from] of nodes) {
            const to = from + node.nodeValue.length;
            if (isEnd ? i <= to : i < to) return [node, i - from];
        }
    };
    const r = document.createRange();
    r.setStart(...at(a, false));
    r.setEnd(...at(b, true));
    const s = getSelection();
    s.removeAllRanges();
    s.addRange(r);
    document.dispatchEvent(new MouseEvent('mouseup', {bubbles: true}));
    return s.toString();
}"""


@requires_browser
class Annotations(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-annotations-")))
        (tmp / "fixtures").mkdir()
        (tmp / "fixtures" / "reading.html").write_text(PAGE.format(styles=page_styles()),
                                                       encoding="utf-8")
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp(), tmp / "fixtures"))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def fixture(self, browser: str, stored: str | None = None, width: int = 1280,
                height: int = 900):
        """The reading fixture, with `stored` as the saved highlights if
        given; the page's errors are checked at cleanup."""
        context = self.browsers[browser].new_context(viewport={"width": width, "height": height})
        self.addCleanup(context.close)
        context.add_init_script(STUBS)
        if stored is not None:
            context.add_init_script(f"if (!sessionStorage.getItem('seeded')) {{"
                                    f" localStorage.setItem('{KEY}', {json.dumps(stored)});"
                                    f" sessionStorage.setItem('seeded', '1'); }}")
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        self.addCleanup(lambda: self.assertEqual(errors, [], "page errors"))
        page.goto(self.base + "/__fixture/reading.html", wait_until="load")
        return page

    def select(self, page, selector: str, start: str, end: str) -> str:
        text = page.evaluate(SELECT, [selector, start, end])
        self.expect(page.locator(".selection-popup.is-visible")).to_be_visible()
        return text

    def toolbar(self, page) -> list[str]:
        return page.locator(".selection-popup.is-visible .selection-popup-btn").all_inner_texts()

    def highlight(self, page, selector: str, start: str, end: str, note: str = "",
                  color: str | None = None) -> str:
        text = self.select(page, selector, start, end)
        page.click(".selection-popup [data-action=annotate]")
        self.expect(page.locator(".ann-picker.is-visible")).to_be_visible()
        if color:
            page.click(f".ann-picker-swatch[data-color={color}]")
        page.fill(".ann-picker-note", note)
        page.press(".ann-picker-note", "Enter")
        return text

    def stored(self, page) -> list[dict]:
        return json.loads(page.evaluate(f"localStorage.getItem('{KEY}') || '[]'"))

    @staticmethod
    def collapsed(text: str) -> str:
        return " ".join(text.split())

    # -- highlights --------------------------------------------------------

    def test_a_highlight_across_a_line_break(self) -> None:
        # J02: Pandoc leaves the source's line break in the text; the
        # selection reads it as a space.
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.fixture(browser)
                text = self.highlight(page, "#wrapped", "three", "accounts", note="two lines")
                self.assertEqual(self.collapsed(text), "three accounts")
                mark = page.locator("#wrapped mark.user-annotation")
                self.expect(mark).to_have_count(1)
                self.assertEqual(mark.evaluate("m => m.textContent"), "three\naccounts")
                self.expect(page.locator(".ann-picker.is-visible")).to_have_count(0)
                [ann] = self.stored(page)
                self.assertEqual((ann["url"], ann["note"], ann["color"]),
                                 ("/__fixture/reading.html", "two lines", "amber"))
                page.reload(wait_until="load")
                self.expect(page.locator("#wrapped mark.user-annotation")).to_have_text("three\naccounts")
                self.assertEqual(len(self.stored(page)), 1)

    def marks(self, page, selector: str = "main") -> list[dict]:
        return page.eval_on_selector_all(
            f"{selector} mark.user-annotation",
            "ms => ms.map(m => ({text: m.textContent, cls: m.className, id: m.dataset.annId,"
            " tab: m.getAttribute('tabindex'), in: m.parentElement.closest('p').id}))")

    def test_a_highlight_across_an_inline_element(self) -> None:
        # A mark for each text node, the <em> left where it was.
        shape = "Array.from(document.querySelector('#inline').childNodes, n => n.nodeName + ':' + n.textContent)"
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.fixture(browser)
                before = page.evaluate(shape)
                self.highlight(page, "#inline", "phrase", "emphasis", color="sage")
                for when in ("made", "reloaded"):
                    marks = self.marks(page, "#inline")
                    self.assertEqual([(m["text"], m["cls"], m["tab"]) for m in marks], [
                        ("phrase with ", "user-annotation user-annotation--sage ann-joins-next", "0"),
                        ("emphasis", "user-annotation user-annotation--sage ann-joins-prev", None)],
                        when)
                    self.assertEqual(len({m["id"] for m in marks}), 1)
                    self.assertEqual(page.evaluate("document.querySelectorAll('#inline em').length"), 1)
                    self.assertEqual(self.collapsed(page.inner_text("#inline")),
                                     "A phrase with emphasis inside it, and then more.")
                    if when == "made":
                        page.reload(wait_until="load")
                        self.expect(page.locator("#inline mark")).to_have_count(2)
                page.hover("#inline em mark")
                page.click(".ann-tooltip.is-visible .ann-tooltip-delete")
                self.assertEqual(page.evaluate(shape), before)

    def test_a_highlight_across_a_paragraph_break(self) -> None:
        # It used to be extracted into one inline <mark> between the two
        # paragraphs, holding a clone of each half: two paragraphs became
        # four, and stayed four after the highlight was deleted.
        shape = ("Array.from(document.querySelectorAll('main > *'), e => e.tagName + '#' + e.id"
                 " + ':' + e.childNodes.length)")
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.fixture(browser)
                before = page.evaluate(shape)
                text = self.highlight(page, "main", "prepared", "The same")
                self.assertEqual(self.collapsed(text), "prepared mind. The same")
                for when in ("made", "reloaded"):
                    self.assertEqual([(m["text"], m["in"]) for m in self.marks(page)],
                                     [("prepared mind.", "plain"), ("The same", "echo")], when)
                    self.assertEqual(page.evaluate("document.querySelectorAll('main p').length"), 5)
                    if when == "made":
                        page.reload(wait_until="load")
                        self.expect(page.locator("main mark")).to_have_count(2)
                page.hover("#echo mark")
                page.click(".ann-tooltip.is-visible .ann-tooltip-delete")
                self.expect(page.locator("main mark")).to_have_count(0)
                self.assertEqual(page.evaluate(shape), before)

    def test_the_occurrence_selected(self) -> None:
        # "each of them partial" ends #wrapped and #echo; selected in #echo,
        # it used to be highlighted in #wrapped, the first.
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.fixture(browser)
                self.highlight(page, "#echo", "each", "partial")
                self.assertEqual([(m["text"], m["in"]) for m in self.marks(page)],
                                 [("each of them partial", "echo")])
                [ann] = self.stored(page)
                self.assertTrue(ann["prefix"].endswith("The same event, told again: "), ann)
                self.assertTrue(ann["suffix"].startswith(". def increment(x):"), ann)
                page.reload(wait_until="load")
                self.expect(page.locator("main mark")).to_have_count(1)
                self.assertEqual([m["in"] for m in self.marks(page)], ["echo"])
                # One made before the context was kept has none: the first.
                page.evaluate(f"""() => {{
                    const [a] = JSON.parse(localStorage.getItem('{KEY}'));
                    delete a.prefix; delete a.suffix;
                    localStorage.setItem('{KEY}', JSON.stringify([a])); }}""")
                page.reload(wait_until="load")
                self.expect(page.locator("main mark")).to_have_count(1)
                self.assertEqual([m["in"] for m in self.marks(page)], ["wrapped"])

    def test_malformed_highlights_are_ignored(self) -> None:
        # Any of these used to throw at load and on every ln:content-added,
        # and made Annotate fail without its message.
        good = {"id": 'a"b]', "url": "/__fixture/reading.html", "text": "Serendipity",
                "color": "rose", "note": 7, "created": None}
        cases = {
            "an object": ("{}", 0),
            "a string": ('"x"', 0),
            "a null entry": ("[null]", 0),
            "entries without text or id": (json.dumps([{"url": "/__fixture/reading.html"},
                                                       {"id": 3, "text": "x", "url": "/"}]), 0),
            "a quote in an id, odd fields": (json.dumps([good]), 1),
            "not JSON": ("[{", 0),
        }
        for browser in BROWSERS:
            for case, (stored, shown) in cases.items():
                with self.subTest(browser=browser, case=case):
                    page = self.fixture(browser, stored)
                    self.expect(page.locator("main mark")).to_have_count(shown)
                    if shown:
                        self.expect(page.locator("#plain mark")).to_have_class(
                            "user-annotation user-annotation--rose")
                    self.highlight(page, "#echo", "told", "again")
                    self.expect(page.locator("#echo mark")).to_have_count(1)
                    self.expect(page.locator(".ann-picker.is-visible")).to_have_count(0)
                    self.assertEqual(len(self.stored(page)), shown + 1)
                    if shown:
                        page.hover("#plain mark")
                        page.click(".ann-tooltip.is-visible .ann-tooltip-delete")
                        self.expect(page.locator("#plain mark")).to_have_count(0)
                        self.assertEqual(len(self.stored(page)), 1)

    def test_the_picker_opens_in_view(self) -> None:
        # A selection from near the top of the window to near its foot
        # leaves no room above it or below: the picker went below, out of
        # sight, and scrolling to it (or its note field taking focus)
        # closed it.
        span = """() => { const r = document.createRange();
            r.setStartBefore(document.getElementById('wrapped'));
            r.setEndAfter(document.getElementById('echo'));
            return r.getBoundingClientRect().toJSON(); }"""
        box = "s => { const r = document.querySelector(s).getBoundingClientRect(); return [r.top, r.bottom]; }"
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                # A short window, opened at that size (resizing one fires a
                # scroll, which hides the toolbar), and a passage taller
                # than it, from just below its top.
                page = self.fixture(browser, height=280)
                # Room below to scroll the passage to the top: the fixture
                # alone is too short. Instantly: the site scrolls smoothly,
                # and a scroll still moving hides the toolbar.
                page.evaluate("document.body.style.paddingBottom = '3000px'")
                page.evaluate("window.scrollTo({top: document.getElementById('wrapped')"
                              ".getBoundingClientRect().top + scrollY - 8, behavior: 'instant'})")
                # The scroll event of that jump comes a frame later, and a
                # scroll hides the toolbar: let it pass before selecting.
                page.evaluate("new Promise(r => requestAnimationFrame(() => requestAnimationFrame(r)))")
                self.select(page, "main", "As a result", "told again")
                y = page.evaluate("scrollY")
                # Not with the pointer: a clicked button takes focus, and the
                # page scrolls it clear of scroll-padding-bottom. What is
                # tested is the picker's own placement and focus.
                page.locator(".selection-popup [data-action=annotate]").evaluate("b => b.click()")
                picker = page.locator(".ann-picker.is-visible")
                self.expect(picker).to_be_visible()
                self.expect(page.locator(".ann-picker-note")).to_be_focused()
                top, bottom = page.evaluate(box, ".ann-picker")
                height, sel = bottom - top, page.evaluate(span)
                vh = page.evaluate("innerHeight")
                # The premise: no room either side of the selection.
                self.assertLess(sel["top"], height + 16)
                self.assertGreater(sel["bottom"] + 8 + height, vh)
                self.assertGreaterEqual(top, 0)
                self.assertLessEqual(bottom, vh)
                self.assertEqual(page.evaluate("scrollY"), y, "taking focus scrolled the page")
                page.mouse.wheel(0, 60)
                page.wait_for_timeout(300)
                self.assertGreater(page.evaluate("scrollY"), y)
                self.expect(picker).to_be_visible()
                page.fill(".ann-picker-note", "in view")
                page.press(".ann-picker-note", "Enter")
                self.expect(page.locator("#echo mark.user-annotation")).to_have_count(1)

    def test_what_cannot_be_highlighted_says_so(self) -> None:
        # Text already inside a highlight cannot be anchored again.
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.fixture(browser)
                self.highlight(page, "#plain", "Serendipity", "prepared")
                self.select(page, "#plain mark", "favours", "the")
                page.click(".selection-popup [data-action=annotate]")
                page.click(".ann-picker-submit")
                error = page.locator(".ann-picker.is-visible .ann-picker-error")
                self.expect(error).to_have_text("This passage could not be highlighted.")
                self.expect(error).to_have_attribute("role", "alert")
                self.assertEqual(len(self.stored(page)), 1)
                self.expect(page.locator("mark.user-annotation")).to_have_count(1)

    def test_tooltip_and_delete(self) -> None:
        note = '<img src=x onerror="window.__pwned=1">a note'
        for browser in BROWSERS:
            with self.subTest(browser=browser, how="pointer"):
                page = self.fixture(browser)
                self.highlight(page, "#plain", "Serendipity", "favours", note=note)
                page.mouse.click(5, 5)
                page.hover("#plain mark")
                tooltip = page.locator(".ann-tooltip.is-visible")
                self.expect(tooltip.locator(".ann-tooltip-note")).to_have_text(note)
                self.expect(tooltip.locator("img")).to_have_count(0)
                tooltip.locator(".ann-tooltip-delete").click()
                self.expect(page.locator("mark.user-annotation")).to_have_count(0)
                self.assertEqual(self.stored(page), [])
                # The paragraph is whole again: one text node, as before.
                self.assertEqual(page.evaluate("document.querySelector('#plain').childNodes.length"), 1)
                self.assertIsNone(page.evaluate("window.__pwned"))
            with self.subTest(browser=browser, how="keyboard"):
                self.highlight(page, "#wrapped", "three", "accounts", note="keys")
                page.mouse.click(5, 5)
                mark = page.locator("#wrapped mark")
                mark.focus()
                self.expect(page.locator(".ann-tooltip.is-visible .ann-tooltip-note")).to_have_text("keys")
                # At once: Enter re-shows the tooltip, and while it faded in
                # through visibility the browser refused focus to Delete.
                page.keyboard.press("Enter")
                self.expect(page.locator(".ann-tooltip-delete")).to_be_focused()
                page.keyboard.press("Escape")
                self.expect(page.locator(".ann-tooltip.is-visible")).to_have_count(0)
                self.expect(mark).to_be_focused()

    # -- the toolbar -------------------------------------------------------

    def test_toolbar_by_context(self) -> None:
        cases = {
            "prose": ("#plain", "favours", "prepared",
                      ["Annotate", "BibTeX", "Copy", "DuckDuckGo", "Here", "Wikipedia"]),
            "one word": ("#plain", "Serendipity", "Serendipity",
                         ["Annotate", "BibTeX", "Copy", "Define", "DuckDuckGo", "Here", "Wikipedia"]),
            "code": ("pre code", "return", "1", ["Copy", "Docs"]),
            "math": ("#math .math", "x^2", "y^2", ["Copy", "nLab", "OEIS", "Wolfram"]),
            # Outside #markdownBody there is no highlighting the text.
            "outside": ("#outside p", "Serendipity", "outside",
                        ["BibTeX", "Copy", "DuckDuckGo", "Here", "Wikipedia"]),
        }
        for browser in BROWSERS:
            page = self.fixture(browser)
            for case, (selector, start, end, buttons) in cases.items():
                with self.subTest(browser=browser, context=case):
                    page.mouse.click(5, 5)
                    self.select(page, selector, start, end)
                    self.assertEqual(self.toolbar(page), buttons)
            with self.subTest(browser=browser, part="Escape hides"):
                page.keyboard.press("Escape")
                self.expect(page.locator(".selection-popup.is-visible")).to_have_count(0)

    def test_the_toolbar_fits_a_phone(self) -> None:
        # At 375px the one-word toolbar ran from x=10 to x=559.
        boxes = """els => els.map(e => { const r = e.getBoundingClientRect();
            return [e.textContent.trim() || 'toolbar', r.left, r.top, r.right, r.bottom]; })"""
        cases = {"one word, at a line's start": ("#plain", "Serendipity", "Serendipity", "Define"),
                 "one word, further on": ("#plain", "prepared", "prepared", "Define"),
                 "a phrase": ("#inline", "phrase", "then", "Wikipedia")}
        for browser in BROWSERS:
            for width in (320, 375):
                page = self.fixture(browser, width=width, height=700)
                vw, vh = page.evaluate("[document.documentElement.clientWidth, innerHeight]")
                for case, (selector, start, end, button) in cases.items():
                    with self.subTest(browser=browser, width=width, case=case):
                        page.mouse.click(1, 1)
                        self.select(page, selector, start, end)
                        toolbar = page.locator(".selection-popup.is-visible")
                        toolbar.evaluate(SETTLED)
                        self.assertIn(button, self.toolbar(page))
                        found = page.eval_on_selector_all(
                            ".selection-popup.is-visible, .selection-popup.is-visible button", boxes)
                        self.assertGreater(len(found), 4)
                        outside = [b for b in found
                                   if b[1] < 0 or b[2] < 0 or b[3] > vw or b[4] > vh]
                        self.assertEqual(outside, [], f"window {vw}x{vh}")

    def test_toolbar_actions(self) -> None:
        phrase = "favours the prepared"
        q = quote(phrase, safe="~()*!.'")    # encodeURIComponent
        opens = {
            "search": f"https://duckduckgo.com/?q={q}",
            "here": f"/search.html?q={q}",
            "wikipedia": f"https://en.wikipedia.org/wiki/Special:Search?search={q}",
        }
        for browser in BROWSERS:
            page = self.fixture(browser)
            for action, url in opens.items():
                with self.subTest(browser=browser, action=action):
                    page.mouse.click(5, 5)
                    self.select(page, "#plain", "favours", "prepared")
                    page.click(f".selection-popup [data-action={action}]")
                    self.assertEqual(page.evaluate("window.__opened.pop()"), url)
                    self.expect(page.locator(".selection-popup.is-visible")).to_have_count(0)
            with self.subTest(browser=browser, action="define"):
                page.mouse.click(5, 5)
                self.select(page, "#plain", "Serendipity", "Serendipity")
                page.click(".selection-popup [data-action=define]")
                self.assertEqual(page.evaluate("window.__opened.pop()"),
                                 "https://en.wiktionary.org/wiki/Serendipity")
            with self.subTest(browser=browser, action="docs"):
                page.mouse.click(5, 5)
                self.select(page, "pre code", "increment", "increment")
                page.click(".selection-popup [data-action=docs]")
                self.assertEqual(page.evaluate("window.__opened.pop()"),
                                 "https://docs.python.org/3/search.html?q=increment")
            with self.subTest(browser=browser, action="copy"):
                page.mouse.click(5, 5)
                self.select(page, "#plain", "favours", "prepared")
                page.click(".selection-popup [data-action=copy]")
                self.assertEqual(page.evaluate("window.__copied.pop()"), phrase)
            with self.subTest(browser=browser, action="BibTeX"):
                page.mouse.click(5, 5)
                self.select(page, "#plain", "favours", "prepared")
                page.click(".selection-popup [data-action=cite]")
                bib = page.evaluate("window.__copied.pop()").splitlines()
                self.assertRegex(bib[0], r"^@online\{neuwirth\d{4}reading,$")
                self.assertIn("  title   = {Reading Fixture},", bib)
                self.assertIn("  author  = {Neuwirth, Levi},", bib)
                self.assertIn("  url     = {" + self.base + "/__fixture/reading.html},", bib)
                self.assertIn("  note    = {\\enquote{" + phrase + "}},", bib)


if __name__ == "__main__":
    unittest.main()
