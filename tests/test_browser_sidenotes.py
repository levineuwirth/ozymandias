"""Sidenotes, in Chromium and Firefox.

Filters/Sidenotes.hs writes each note once, as an item in a list at the end
of the body; the in-text reference links to it. sidenotes.js sets the items
in the right margin on wide screens, level with their references, and on
narrow screens lends an item to a modal reading sheet. A fixture page,
styled as an essay, has sections, crowded notes and one long note with a
list, code and a link; the font-loading test uses a built essay.

Checked, as a reader meets it: in the margin each note is beside its
reference, its first line on the reference's line at every text size,
or just below the note before it, never overlapping another,
and follows the text when a section above closes, when a section holding
its reference closes (it goes too), when web fonts arrive (J03), and when
a note above it grows; what follows the article clears the last note (a
long final note ran over the footer), on screen, while paper gets no
extra space; the margin is used only when it holds a readable measure at
the reader's text size (at 1500 px and the largest size the column was
263 px and a note 700 px tall), and the choice follows the setting as it
changes, without flickering; a note pushed far below its reference rises
beside it while active, and goes back after; the note's letter hangs in
the gutter on its first line (it used to take a line of its own, boxed);
the active note, hovered or reached from the keyboard, is marked by its
color and a hairline in the gutter, keyboard focus included; closing the last section
keeps every other note; without JavaScript the notes are the list; hover, click, Enter and Space on a reference pin the
note and take focus to it, Escape and a click elsewhere let go; a link to
a note (#fn-…) shows it beside its reference, opening a closed section
first; the narrow sheet is modal (focus stays in it, the page does not
scroll behind it), is named for its note, keeps a reading measure where
the screen is wider than a phone, holds the list item itself rather than a copy, opens
each note at its top with the close button in view, and puts the item
back and focus on its reference however it closes; printed, each note
appears once, in the list.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_sidenotes -v
"""

from __future__ import annotations

import re
import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, check_site, enforcing_csp, page_styles, require_playwright,
                            requires_browser, site_server)
from tests.target import route


def ref(label: str) -> str:
    return (f'<sup class="sidenote-ref" id="snref-{label}">'
            f'<a href="#fn-{label}" role="doc-noteref">{label}</a></sup>')


def back(label: str) -> str:
    return (f'<a href="#snref-{label}" class="footnote-back" role="doc-backlink" '
            f'aria-label="Back to reference {label}">↩︎</a>')


def item(label: str, blocks: str) -> str:
    return (f'<li id="fn-{label}" class="footnote-item">'
            f'<span class="footnote-label" aria-hidden="true">{label}</span>{blocks}</li>')


LONG = " ".join(["A long note runs on beside the text, and on, and further on still."] * 14)

PAGE = f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Sidenotes fixture</title>
{{styles}}
<script src="/js/utils.js"></script>
<script src="/js/collapse.js" defer></script>
<script src="/js/sidenotes.js" defer></script>
</head><body><div class="page-shell"><main id="markdownBody">
<h1 class="page-title">Sidenotes Fixture</h1>
<p id="p0">An opening paragraph with a note{ref("a")} and text after it.</p>
<h2 id="first">First section</h2>
{{filler}}
<p id="p1">A paragraph with a long note{ref("b")} and, close after it, a short one{ref("c")}.</p>
<h2 id="second">Second section</h2>
<p id="p2">The second section's text, with a note{ref("d")} and more after it.</p>
{{filler}}
<p>A closing paragraph, with a long last note{ref("e")}.</p>
<section class="footnotes" role="doc-endnotes"><ol class="footnotes-list">
{item("a", f"<p>The first note, beside its reference.{back('a')}</p>")}
{item("b", f'<p id="b-start">{LONG}</p><ul><li>An item in a note</li></ul>'
           f'<pre><code>code in a note</code></pre>'
           f'<p><a id="note-link" href="#p0">A link in a note</a>.{back("b")}</p>')}
{item("c", f"<p>A short note.{back('c')}</p>")}
{item("d", f"<p>The note itself, beside its reference.{back('d')}</p>")}
{item("e", f"<p>{LONG}{back('e')}</p>")}
</ol></section>
</main>
<div id="after">What follows the article.</div>
</div></body></html>
"""
FILLER = "\n".join(f"<p>Filler paragraph {i}, long enough to take a line or two of the measure "
                   f"so that closing this section moves what follows it.</p>" for i in range(12))

def classes(*names: str) -> re.Pattern:
    """A class attribute holding every one of `names`, in any order."""
    return re.compile("".join(rf"(?=.*(^|\s){n}(\s|$))" for n in names))


ACTIVE, RAISED = classes("is-active"), classes("is-raised")

FIXTURE = "/__fixture/notes.html"
KEY = "section-collapsed:/__fixture/notes.html:"
WIDE, NARROW = 1600, 700
SHEET = "dialog.sidenote-popup"

# The baseline of a line, read from a zero-size inline box set on it for
# the moment of the reading. A reference's box goes after it: before it,
# the box would add a place to break the line, and a word that wrapped
# together with its reference could part from it. The note text's first
# baseline skips the hanging letter.
BASELINES = """
    const baseline = insert => {
        const probe = document.createElement('span');
        probe.style.cssText = 'display:inline-block;width:0;height:0;vertical-align:baseline';
        insert(probe);
        const y = probe.getBoundingClientRect().top;
        probe.remove();
        return y;
    };
    const firstText = note => document.createTreeWalker(note, NodeFilter.SHOW_TEXT, {
        acceptNode: n => n.parentElement.closest('.footnote-label') || !n.data.trim()
            ? NodeFilter.FILTER_SKIP : NodeFilter.FILTER_ACCEPT,
    }).nextNode();
"""

# Where each note is, as the reader sees it: its box, the baseline of its
# reference's line and of its own first line, and the body column's right
# edge. A reference with no boxes (in a closed section) reports whether
# its note is shown.
GEOMETRY = """() => {
    """ + BASELINES + """
    const body = document.getElementById('markdownBody').getBoundingClientRect();
    return [...document.querySelectorAll('#markdownBody .sidenote-ref')].map(ref => {
        const note = document.getElementById(ref.querySelector('a').getAttribute('href').slice(1));
        const r = note.getBoundingClientRect();
        const shown = r.height > 0, refShown = ref.getClientRects().length > 0;
        return {
            id: note.id,
            raised: note.classList.contains('is-raised'),
            refShown: refShown,
            refTop: ref.getBoundingClientRect().top,
            refBase: refShown ? baseline(p => ref.after(p)) : null,
            noteBase: shown ? baseline(p => firstText(note).before(p)) : null,
            shown: shown,
            top: r.top, bottom: r.bottom, left: r.left,
            bodyRight: body.right,
        };
    });
}"""


@requires_browser
class Sidenotes(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-sidenotes-")))
        (tmp / "fixtures").mkdir()
        (tmp / "fixtures" / "notes.html").write_text(
            PAGE.format(styles=page_styles(), filler=FILLER), encoding="utf-8")
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp(), tmp / "fixtures"))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def open(self, browser: str, path: str = FIXTURE, width: int = WIDE,
             collapsed: tuple[str, ...] = (), wait: str = "load", context_setup=None,
             javascript: bool = True):
        context = self.browsers[browser].new_context(viewport={"width": width, "height": 900},
                                                     java_script_enabled=javascript)
        self.addCleanup(context.close)
        if collapsed:
            context.add_init_script("".join(
                f"try {{ localStorage.setItem('{KEY}{h}', '1'); }} catch (e) {{}}" for h in collapsed))
        if context_setup:
            context_setup(context)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        self.addCleanup(lambda: self.assertEqual(errors, [], "page errors"))
        page.goto(self.base + path, wait_until=wait)
        return page

    def poll(self, page, script: str, arg=None, want=True, timeout: float = 5.0) -> None:
        """Wait until `script` (a function, given `arg`) returns `want`.
        Not wait_for_function: under the site's CSP that is an eval."""
        import time
        deadline = time.monotonic() + timeout
        while (got := page.evaluate(script, arg)) != want:
            if time.monotonic() > deadline:
                self.fail(f"{script} gave {got!r}, not {want!r}")
            page.wait_for_timeout(50)

    def settled(self, page) -> None:
        """Two frames: sidenotes.js repositions at most once per frame."""
        page.evaluate("new Promise(r => requestAnimationFrame(() => requestAnimationFrame(r)))")

    def still(self, page) -> float:
        """scrollY once the page has stopped moving: scrolling is smooth
        site-wide, so a reading taken mid-scroll means nothing."""
        return page.evaluate("""() => new Promise(resolve => {
            let last = scrollY, same = 0;
            (function tick() {
                requestAnimationFrame(() => {
                    same = scrollY === last ? same + 1 : 0;
                    last = scrollY;
                    if (same >= 5) resolve(scrollY); else tick();
                });
            })();
        })""")

    def assert_in_margin(self, page) -> list[dict]:
        """Every shown note is right of the text, at or below its reference,
        its first line on its reference's line (baselines within 2px)
        unless the note above pushed it down, and clear of the note above.
        A raised note (the active one, come loose from its reference) is
        on its reference's line, over the others; the column runs on
        beneath it as if it were in its place, which the reader cannot
        see, so the note after it is only checked for overlap. Returns
        the geometry."""
        self.settled(page)
        notes = page.evaluate(GEOMETRY)
        prev_bottom, unseen = None, False
        for n in notes:
            if not n["refShown"]:
                self.assertFalse(n["shown"], f"{n['id']}: its reference is hidden, the note is not")
                continue
            self.assertTrue(n["shown"], n)
            self.assertGreater(n["left"], n["bodyRight"], f"{n['id']} is not in the margin: {n}")
            shift = n["noteBase"] - n["refBase"]   # how far below its line the note's first line is
            self.assertGreaterEqual(shift, -2, f"{n['id']} is above its reference")
            if n["raised"]:
                self.assertLessEqual(abs(shift), 2, f"{n['id']} rose, but not to its line: {n}")
                unseen = True
                continue
            level_top = n["top"] - shift
            if unseen:
                if prev_bottom is not None:
                    self.assertGreaterEqual(n["top"], prev_bottom, f"{n['id']} overlaps the note above")
            elif prev_bottom is None or level_top >= prev_bottom + 12:
                self.assertLessEqual(abs(shift), 2, f"{n['id']} is not on its reference's line: {n}")
            else:
                self.assertGreaterEqual(n["top"], prev_bottom, f"{n['id']} overlaps the note above")
                self.assertLessEqual(n["top"], prev_bottom + 13, f"{n['id']} was pushed too far")
            prev_bottom, unseen = n["bottom"], False
        return notes

    # -- the margin ----------------------------------------------------------

    def test_notes_sit_beside_their_references(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                notes = {n["id"]: n for n in self.assert_in_margin(page)}
                # The crowded pair: c follows the long b, pushed below it.
                self.assertGreater(notes["fn-c"]["top"] - notes["fn-c"]["refTop"], 100)
                # The list itself shows nothing of its own.
                self.assertEqual(page.locator("section.footnotes").evaluate(
                    "s => getComputedStyle(s).borderTopStyle"), "none")
                # Rich content stays in its note.
                self.expect(page.locator("#fn-b ul li")).to_have_text("An item in a note")
                self.expect(page.locator("#fn-b pre")).to_have_text("code in a note")

    def test_what_follows_clears_the_last_note(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                notes = self.assert_in_margin(page)
                after = page.locator("#after").evaluate("a => a.getBoundingClientRect().top")
                last = max(n["bottom"] for n in notes if n["shown"])
                self.assertGreater(last, page.locator("#p2").evaluate(
                    "p => p.getBoundingClientRect().bottom") + 100, "no note runs past the text")
                self.assertLessEqual(last, after, "the last note runs into what follows")
                # Paper gets the list, and no room kept for margin notes.
                page.emulate_media(media="print")
                self.assertIn(page.locator("#markdownBody").evaluate(
                    "b => getComputedStyle(b).minHeight"), ("auto", "0px"))

    def test_a_growing_note_moves_the_ones_below(self) -> None:
        # Notes were placed when the text moved, not when a note itself
        # grew: math typeset or an image loading in one overlapped the next.
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                self.assert_in_margin(page)
                before = {n["id"]: n for n in page.evaluate(GEOMETRY)}
                # Note b has note c right under it.
                page.evaluate("""() => {
                    const p = document.createElement('p');
                    p.textContent = 'Arriving late, and long. '.repeat(40);
                    document.getElementById('fn-b').appendChild(p);
                }""")
                notes = {n["id"]: n for n in self.assert_in_margin(page)}
                self.assertGreater(notes["fn-c"]["top"] - before["fn-c"]["top"], 300)
                after = page.locator("#after").evaluate("a => a.getBoundingClientRect().top")
                self.assertLessEqual(max(n["bottom"] for n in notes.values()), after)

    def test_the_margin_needs_room_for_a_measure(self) -> None:
        cases = ((1500, None, "margin"), (1600, "29px", "list"), (1920, "29px", "margin"))
        for browser in BROWSERS:
            for width, size, layout in cases:
                with self.subTest(browser=browser, width=width, text=size or "default"):
                    setup = None
                    if size:
                        # As settings.js applies a stored size: before the
                        # notes are laid out. (Chromium runs init scripts
                        # before <html> exists.)
                        setup = lambda c, size=size: c.add_init_script(
                            "document.addEventListener('DOMContentLoaded', () => document.documentElement"
                            f".style.setProperty('--text-size', '{size}'))")
                    page = self.open(browser, width=width, context_setup=setup)
                    body = page.locator("#markdownBody")
                    self.expect(body).to_have_attribute("data-sidenotes", layout)
                    if layout == "margin":
                        self.assert_in_margin(page)
                        measure = page.locator("#fn-a").evaluate(
                            "n => n.getBoundingClientRect().width / parseFloat(getComputedStyle(n).fontSize)")
                        self.assertGreaterEqual(measure, 13.9)
                    else:
                        # The list, and the sheet for a reference.
                        self.expect(page.locator("section.footnotes")).to_have_css("border-top-style", "solid")
                        page.locator("#snref-a a").click()
                        self.expect(page.locator(SHEET)).to_be_visible()
            with self.subTest(browser=browser, change="the text size, as the settings panel sets it"):
                page = self.open(browser, width=1600)
                body = page.locator("#markdownBody")
                self.expect(body).to_have_attribute("data-sidenotes", "margin")
                page.evaluate("document.documentElement.style.setProperty('--text-size', '29px')")
                self.expect(body).to_have_attribute("data-sidenotes", "list")
                page.evaluate("document.documentElement.style.removeProperty('--text-size')")
                self.expect(body).to_have_attribute("data-sidenotes", "margin")
                # And it stays: no flipping frame to frame.
                seen = page.evaluate("""() => new Promise(resolve => {
                    const body = document.getElementById('markdownBody'), out = [];
                    (function tick() {
                        out.push(body.dataset.sidenotes);
                        if (out.length < 20) requestAnimationFrame(tick); else resolve(out);
                    })();
                })""")
                self.assertEqual(set(seen), {"margin"})
                self.assert_in_margin(page)

    def test_a_detached_note_rises_beside_its_reference(self) -> None:
        # Note d is pushed far below its reference by the long note b. While
        # it is the active note it sits on its reference's line.
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                before = {n["id"]: n for n in self.assert_in_margin(page)}
                self.assertGreater(before["fn-d"]["noteBase"] - before["fn-d"]["refBase"], 300,
                                   "note d is not detached: nothing was tested")
                note = page.locator("#fn-d")
                page.locator("#snref-d a").click()
                self.expect(note).to_have_class(classes("is-active", "is-raised"))
                self.expect(note).to_be_in_viewport()
                risen = {n["id"]: n for n in self.assert_in_margin(page)}["fn-d"]
                self.assertTrue(risen["raised"])
                # Drawn over the notes around it, which stay where they were.
                box = note.bounding_box()
                over = page.evaluate(f"document.elementFromPoint({box['x'] + 5}, {box['y'] + 5})"
                                     ".closest('.footnote-item').id")
                self.assertEqual(over, "fn-d")
                page.mouse.move(5, 5)   # the click left the pointer on the reference
                page.keyboard.press("Escape")
                self.expect(note).not_to_have_class(ACTIVE)
                self.expect(note).not_to_have_class(RAISED)
                after = {n["id"]: n for n in self.assert_in_margin(page)}
                for k in ("fn-b", "fn-c", "fn-d"):
                    self.assertLessEqual(abs((after[k]["top"] - after[k]["refTop"])
                                             - (before[k]["top"] - before[k]["refTop"])), 1, k)
                # A note beside its reference does not rise.
                page.locator("#snref-a a").click()
                self.expect(page.locator("#fn-a")).to_have_class(ACTIVE)
                self.expect(page.locator("#fn-a")).not_to_have_class(RAISED)
            with self.subTest(browser=browser, via="a link to the note"):
                page = self.open(browser, path=FIXTURE + "#fn-d")
                self.expect(page.locator("#fn-d")).to_have_class(classes("is-active", "is-raised"))
                self.expect(page.locator("#snref-d")).to_be_in_viewport()

    def test_first_lines_sit_on_their_references_lines(self) -> None:
        # The note's top used to sit at the reference's raised superscript;
        # its first line now shares the reference line's baseline, at any
        # text size. (assert_in_margin checks the baselines.)
        for browser in BROWSERS:
            for size in ("17px", "23px", "29px"):
                with self.subTest(browser=browser, text=size):
                    setup = lambda c, size=size: c.add_init_script(
                        "document.addEventListener('DOMContentLoaded', () => document.documentElement"
                        f".style.setProperty('--text-size', '{size}'))")
                    page = self.open(browser, width=1920, context_setup=setup)
                    self.expect(page.locator("#markdownBody")).to_have_attribute("data-sidenotes", "margin")
                    notes = {n["id"]: n for n in self.assert_in_margin(page)}
                    self.assertLessEqual(abs(notes["fn-a"]["noteBase"] - notes["fn-a"]["refBase"]), 2)

    def test_the_letter_hangs_in_the_gutter(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                got = page.evaluate("""() => {
                    """ + BASELINES + """
                    const note = document.getElementById('fn-a');
                    const label = note.querySelector('.footnote-label');
                    const n = note.getBoundingClientRect(), l = label.getBoundingClientRect();
                    const text = firstText(note);
                    const range = document.createRange();
                    range.setStart(text, 0); range.setEnd(text, 1);
                    return {
                        noteLeft: n.left, noteTop: n.top,
                        line: parseFloat(getComputedStyle(note).lineHeight),
                        bodyRight: document.getElementById('markdownBody').getBoundingClientRect().right,
                        labelLeft: l.left, labelRight: l.right,
                        labelBase: baseline(p => label.prepend(p)),
                        textBase: baseline(p => text.before(p)),
                        textLeft: range.getBoundingClientRect().left,
                    };
                }""")
                # In the gutter: clear of the text column, left of the note.
                self.assertGreater(got["labelLeft"], got["bodyRight"], got)
                self.assertLessEqual(got["labelRight"], got["noteLeft"] + 0.5, got)
                # On the first line, which it shares: the text starts flush
                # on the note's top line, not a line down.
                self.assertLessEqual(abs(got["labelBase"] - got["textBase"]), 1, got)
                self.assertLess(got["textBase"] - got["noteTop"], got["line"], got)
                self.assertLessEqual(abs(got["textLeft"] - got["noteLeft"]), 1, got)

    def test_the_active_note_is_marked(self) -> None:
        rule = "id => getComputedStyle(document.getElementById(id), '::before').opacity"
        # The note's text and its letter take the body text's color.
        same_color = """id => { const n = document.getElementById(id), c = getComputedStyle(n).color;
            return c === getComputedStyle(n.querySelector('.footnote-label')).color
                && c === getComputedStyle(document.getElementById('p0')).color; }"""
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                note, link = page.locator("#fn-a"), page.locator("#snref-a a")
                self.assertEqual(page.evaluate(rule, "fn-a"), "0")
                self.assertFalse(page.evaluate(same_color, "fn-a"))
                page.hover("#snref-a")
                self.poll(page, rule, "fn-a", "1")
                self.poll(page, same_color, "fn-a")
                page.mouse.move(5, 5)
                self.poll(page, rule, "fn-a", "0")
                # From the keyboard: the same marks, and no focus ring
                # boxing the note.
                link.focus()
                page.keyboard.press("Enter")
                self.expect(note).to_be_focused()
                self.assertTrue(note.evaluate("n => n.matches(':focus-visible')"))
                self.assertEqual(note.evaluate("n => getComputedStyle(n).outlineStyle"), "none")
                self.poll(page, rule, "fn-a", "1")
                self.poll(page, same_color, "fn-a")

    def test_without_javascript_the_notes_are_the_list(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser, javascript=False)
                body_right = page.locator("#markdownBody").evaluate("b => b.getBoundingClientRect().right")
                last_text = page.locator("#p2").evaluate("p => p.getBoundingClientRect().bottom")
                for label in "abcde":
                    box = page.locator(f"#fn-{label}").bounding_box()
                    self.assertLessEqual(box["x"] + box["width"], body_right + 1, label)
                    self.assertGreater(box["y"], last_text, label)

    def test_notes_follow_a_section_closing(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                before = {n["id"]: n for n in self.assert_in_margin(page)}
                page.locator("#first .section-toggle").click()
                self.expect(page.locator("#section-body-first")).to_have_attribute("hidden", "")
                after = {n["id"]: n for n in self.assert_in_margin(page)}
                # Its section's notes went with it; the one below moved up.
                self.assertFalse(after["fn-b"]["shown"])
                self.assertFalse(after["fn-c"]["shown"])
                self.assertGreater(before["fn-d"]["top"] - after["fn-d"]["top"], 100)

    def test_closing_the_last_section_keeps_the_other_notes(self) -> None:
        # The list follows the last section in the body; collapse.js used to
        # wrap it into that section, so closing it took every note.
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser, collapsed=("second",))
                self.expect(page.locator("#p2")).to_be_hidden()
                notes = {n["id"]: n for n in self.assert_in_margin(page)}
                self.assertEqual([k for k, n in notes.items() if n["shown"]], ["fn-a", "fn-b", "fn-c"])
                self.assertFalse(notes["fn-e"]["shown"])
                self.assertEqual(page.locator("section.footnotes").evaluate(
                    "s => !!s.closest('.section-body')"), False)

    def test_notes_follow_late_fonts(self) -> None:
        # J03: the notes were set once, at DOMContentLoaded, often before the
        # web fonts; when they arrived the text moved and the notes did not.
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                held = []
                page = self.open(browser, path=route("essay-footnotes"), wait="domcontentloaded",
                                 context_setup=lambda c: c.route("**/*.woff2", lambda route: held.append(route)))
                self.expect(page.locator("#markdownBody .footnote-item").first).to_be_visible()
                self.settled(page)
                refs = page.evaluate("[...document.querySelectorAll('.sidenote-ref')].map(r => r.offsetTop)")
                self.assertGreater(len(held), 0, "no web font was requested")
                self.assertGreaterEqual(len(refs), 3)
                for route in held:
                    route.continue_()
                page.evaluate("document.fonts.ready.then(() => 1)")
                page.wait_for_load_state("load")
                self.settled(page)
                moved = page.evaluate("[...document.querySelectorAll('.sidenote-ref')].map(r => r.offsetTop)")
                self.assertGreater(max(abs(a - b) for a, b in zip(refs, moved)), 10,
                                   "the fonts moved no reference: nothing was tested")
                self.assert_in_margin(page)

    # -- linking ---------------------------------------------------------------

    def test_hover_and_activation(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                # Note a sits beside its reference (d, pushed down by b,
                # rises when active: test_a_detached_note_rises_...).
                ref, note, link = page.locator("#snref-a"), page.locator("#fn-a"), page.locator("#snref-a a")
                page.hover("#snref-a")
                self.expect(ref).to_have_class(ACTIVE)
                self.expect(note).to_have_class(ACTIVE)
                page.mouse.move(5, 5)
                self.expect(note).not_to_have_class(ACTIVE)
                # A click pins the note and takes focus to it; a click
                # elsewhere lets go.
                link.click()
                page.mouse.move(5, 5)
                self.expect(note).to_have_class(ACTIVE)
                self.expect(note).to_be_focused()
                page.mouse.click(5, 5)
                self.expect(note).not_to_have_class(ACTIVE)
                # The keyboard: Enter and Space both pin; Escape lets go and
                # returns to the reference.
                for key in ("Enter", "Space"):
                    link.focus()
                    page.keyboard.press(key)
                    self.expect(note).to_have_class(ACTIVE)
                    self.expect(note).to_be_focused()
                    page.keyboard.press("Escape")
                    self.expect(note).not_to_have_class(ACTIVE)
                    self.expect(link).to_be_focused()
                # From a note beside its reference, Tab reaches the
                # back-link, which shows and returns to the reference
                # without moving the page.
                link.click()
                page.keyboard.press("Tab")
                backlink = page.locator("#fn-a .footnote-back")
                self.expect(backlink).to_be_focused()
                self.expect(backlink).to_have_css("opacity", "1")
                y = self.still(page)
                page.keyboard.press("Enter")
                self.expect(link).to_be_focused()
                self.assertEqual(self.still(page), y)
                page.mouse.move(5, 5)   # the click left the pointer on the reference
                self.expect(page.locator("#fn-a")).not_to_have_class(ACTIVE)

    def test_a_link_to_a_note(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser, layout="margin"):
                page = self.open(browser, path=FIXTURE + "#fn-d")
                note = page.locator("#fn-d")
                self.expect(note).to_have_class(ACTIVE)
                self.expect(note).to_be_in_viewport()
                self.assert_in_margin(page)
            with self.subTest(browser=browser, layout="margin, its section closed"):
                page = self.open(browser, path=FIXTURE + "#fn-d", collapsed=("second",))
                self.expect(page.locator("#p2")).to_be_visible()
                self.expect(page.locator("#fn-d")).to_be_in_viewport()
                self.assert_in_margin(page)
            with self.subTest(browser=browser, layout="list, its section closed"):
                # The note is in the list, in view; the reader's closed
                # section stays closed.
                page = self.open(browser, path=FIXTURE + "#fn-d", width=NARROW, collapsed=("second",))
                self.expect(page.locator("#fn-d")).to_be_in_viewport()
                self.expect(page.locator("#p2")).to_be_hidden()

    # -- the sheet -------------------------------------------------------------

    def test_the_sheet_is_modal_and_lends_the_note_itself(self) -> None:
        for browser in BROWSERS:
            page = self.open(browser, width=NARROW)
            link = page.locator("#snref-b a")
            for open_with, close in (("Enter", "Escape"), ("Space", "button"), ("click", "outside")):
                with self.subTest(browser=browser, open=open_with, close=close):
                    page.evaluate("window.__note = document.getElementById('fn-b')")
                    page.evaluate("document.getElementById('snref-b')"
                                  ".scrollIntoView({block: 'center', behavior: 'instant'})")
                    y = page.evaluate("scrollY")
                    if open_with == "click":
                        link.click()
                    else:
                        link.focus()
                        page.keyboard.press(open_with)
                    self.expect(page.locator(SHEET)).to_be_visible()
                    self.assertTrue(page.evaluate(f"document.querySelector('{SHEET}').matches(':modal')"))
                    # The list item itself, not a copy: one element per id,
                    # listeners and all.
                    self.assertTrue(page.evaluate(
                        f"document.querySelector('{SHEET} .footnote-item') === window.__note"))
                    self.assertEqual(page.evaluate("document.querySelectorAll('#note-link, #b-start').length"), 2)
                    self.expect(page.locator("#fn-b")).to_be_focused()
                    # Focus stays in the sheet, both ways round.
                    for key in ("Tab",) * 4 + ("Shift+Tab",) * 4:
                        page.keyboard.press(key)
                        self.assertFalse(page.evaluate(
                            "document.querySelector('main').contains(document.activeElement)"), key)
                    # The page behind does not scroll.
                    page.mouse.move(NARROW / 2, 20)
                    page.mouse.wheel(0, 600)
                    page.wait_for_timeout(300)
                    self.assertEqual(page.evaluate("scrollY"), y)
                    if close == "Escape":
                        page.keyboard.press("Escape")
                    elif close == "button":
                        page.click(".sidenote-popup-close")
                    else:
                        page.mouse.click(5, 5)
                    self.expect(page.locator(SHEET)).to_be_hidden()
                    self.expect(link).to_be_focused()
                    # The note is back in its place in the list.
                    self.assertEqual(page.evaluate(
                        "[...document.querySelectorAll('.footnotes-list > li')].map(li => li.id)"),
                        ["fn-a", "fn-b", "fn-c", "fn-d", "fn-e"])
                    self.assertEqual(page.evaluate("document.documentElement.style.overflow"), "")
                    self.assertEqual(page.evaluate("scrollY"), y)

    def test_the_sheet_is_named_and_keeps_a_measure(self) -> None:
        for browser in BROWSERS:
            for width in (390, 1280):
                with self.subTest(browser=browser, width=width):
                    page = self.open(browser, width=width)
                    page.locator("#snref-b a").click()
                    sheet = page.get_by_role("dialog", name="Note b")
                    self.expect(sheet).to_be_visible()
                    self.expect(page.locator(".sidenote-popup-title")).to_have_text("Note b")
                    self.expect(page.locator(f"{SHEET} .footnote-label")).to_be_hidden()
                    box = sheet.bounding_box()
                    em = sheet.evaluate("s => parseFloat(getComputedStyle(s).fontSize)")
                    if width == 390:
                        self.assertAlmostEqual(box["width"], width, delta=1)
                    else:
                        # About 65 characters, centered.
                        self.assertLessEqual(box["width"], 36 * em + 1)
                        self.assertAlmostEqual(box["x"], width - box["x"] - box["width"], delta=1)

    def test_each_note_opens_at_its_top(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser, width=NARROW)
                page.locator("#snref-b a").click()
                sheet = page.locator(SHEET)
                self.expect(sheet).to_be_visible()
                self.assertGreater(sheet.evaluate("s => s.scrollHeight - s.clientHeight"), 200,
                                   "the long note fits: nothing was tested")
                sheet.evaluate("s => { s.scrollTop = s.scrollHeight; }")
                # The close button stays in view however far the note runs.
                close = page.locator(".sidenote-popup-close")
                top = sheet.evaluate("s => s.getBoundingClientRect().top")
                self.assertGreaterEqual(close.bounding_box()["y"], top - 1)
                self.expect(close).to_be_in_viewport()
                close.click()
                # Long enough to keep a scroll offset if one were carried
                # over (a short note in between would clamp it to 0).
                page.locator("#snref-b a").click()
                self.expect(sheet).to_be_visible()
                self.assertEqual(sheet.evaluate("s => s.scrollTop"), 0)

    # -- print -----------------------------------------------------------------

    def test_each_note_prints_once(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page = self.open(browser)
                page.emulate_media(media="print")
                self.expect(page.locator("section.footnotes")).to_be_visible()
                texts = page.evaluate("""() => [...document.querySelectorAll('body *')]
                    .filter(el => el.checkVisibility() && [...el.childNodes].some(
                        n => n.nodeType === 3 && n.textContent.includes('The first note')))
                    .length""")
                self.assertEqual(texts, 1)
                # The list, not the margin: the notes follow the text.
                body_right = page.locator("#markdownBody").evaluate("b => b.getBoundingClientRect().right")
                box = page.locator("#fn-a").bounding_box()
                self.assertLessEqual(box["x"] + box["width"], body_right + 1)


if __name__ == "__main__":
    unittest.main()
