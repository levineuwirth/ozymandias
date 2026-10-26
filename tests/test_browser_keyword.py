"""Keyword search on /search.html with the filters, in Chromium and Firefox.

Pagefind runs as built, over the built site's index: a query term is
chosen from the index itself, one with more results than two pages show,
so nothing here names a page. The epistemic metadata the filters read
(/data/epistemic-meta.json) is faked, keyed to that term's results, so
which results pass is known.

Pagefind's index carries no epistemic fields, so search-filters.js hides
results after Pagefind renders them, rewrites the count to describe what
is shown, and loads further pages until enough pass or the results run
out (F04). Checked: ?q= and the timing; a filter that passes only the
last result loads every page and says "1 of N"; clearing restores
Pagefind's own count; nothing passing says so; pages without metadata are
never filtered; a missing Pagefind bundle breaks nothing else.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_keyword -v
"""

from __future__ import annotations

import re
import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, FakeNetwork, check_site, enforcing_csp, require_playwright,
                            requires_browser, site_server)
from tests._browser import fake_answer as answer

CANDIDATES = ("proof", "model", "verification", "graph", "language", "theory")
PAGE_SIZE = 5          # Pagefind UI's default, and search-filters.js's target
MAX_AUTOLOAD = 10      # search-filters.js's PF_MAX_AUTOLOAD

ROUTES = {"epistemic": r"/data/epistemic-meta\.json$", "archive": r"/data/archive-meta\.json$"}

RESULT = "#search .pagefind-ui__result"
SHOWN = "#search .pagefind-ui__result:not(.search-filtered)"
MESSAGE = "#search .pagefind-ui__message"
MORE = "#search .pagefind-ui__button"


def key(url: str) -> str:
    """search-filters.js's lookup form of a result URL."""
    return url + "index.html" if url.endswith("/") else url


@requires_browser
class KeywordSearch(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-keyword-")))
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp()))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)
        cls.term, cls.urls = cls.choose_term()

    @classmethod
    def choose_term(cls) -> tuple[str, list[str]]:
        """A term whose results fill more than two pages and that the
        filters' autoload can exhaust, and its result URLs in Pagefind's
        order, from Pagefind's own JavaScript API."""
        page = cls.browsers["chromium"].new_page()
        try:
            page.goto(cls.base + "/search.html", wait_until="load")
            found = page.evaluate("""async terms => {
                const pf = await import('/pagefind/pagefind.js');
                for (const t of terms) {
                    const s = await pf.search(t);
                    if (s.results.length > 2 * 5 && s.results.length <= 5 + 10 * 5)
                        return [t, await Promise.all(s.results.map(r => r.data().then(d => d.url)))];
                }
                return null;
            }""", list(CANDIDATES))
        finally:
            page.close()
        if not found:
            raise AssertionError(f"none of {CANDIDATES} has between 11 and 55 results")
        return found[0], found[1]

    def search_page(self, browser: str, epistemic: dict | None = None, query: bool = True, **answers):
        """/search.html?q=<term> on the keyword tab, `epistemic` as the
        metadata. At cleanup: no page errors, no request that no route
        answers."""
        context = self.browsers[browser].new_context(viewport={"width": 1280, "height": 1000})
        self.addCleanup(context.close)
        net = FakeNetwork(context, self.base, ROUTES,
                          {"epistemic": answer(epistemic or {}), "archive": answer({})},
                          local=r"data/(?:epistemic|archive)-meta\.json$")
        net.answers.update(answers)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        page.goto(self.base + "/search.html" + (f"?q={self.term}" if query else ""), wait_until="load")

        def clean():
            self.assertEqual(errors, [], "page errors")
            self.assertEqual(net.unexpected, [], "requests no route answers")
        self.addCleanup(clean)
        return page, net

    def first_page(self, page) -> None:
        self.expect(page.locator(MESSAGE)).to_have_text(f"{len(self.urls)} results for {self.term}")
        self.expect(page.locator(RESULT)).to_have_count(PAGE_SIZE)

    def filter_status(self, page, value: str) -> None:
        if page.get_attribute("#search-filters", "hidden") is not None:
            page.click(".library-filter-toggle")
        page.click(f".filter-status-btn[data-value='{value}']")

    def shown(self, page) -> list[str]:
        """The links of the results not filtered out, once every result
        Pagefind has added is filled in (it adds placeholders first)."""
        self.expect(page.locator(f"{RESULT} .pagefind-ui__result-link")).to_have_count(
            page.locator(RESULT).count())
        return [a.get_attribute("href") for a in
                page.locator(f"{SHOWN} .pagefind-ui__result-link").all()]

    def test_query_parameter_and_timing(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, _ = self.search_page(browser)
                self.first_page(page)
                self.expect(page.locator("#search .pagefind-ui__search-input")).to_have_value(self.term)
                self.expect(page.locator("#search-timing")).to_have_text(re.compile(r"^\d+ ms$"))
                self.assertEqual(self.shown(page), self.urls[:PAGE_SIZE])

    def test_a_filter_loads_pages_until_it_has_its_results(self) -> None:
        # Only the last result is a draft: every page has to be loaded.
        meta = {key(u): {"status": "Draft" if u == self.urls[-1] else "Durable"} for u in self.urls}
        n = len(self.urls)
        for browser in BROWSERS:
            page, _ = self.search_page(browser, meta)
            self.first_page(page)
            with self.subTest(browser=browser, filter="draft"):
                self.filter_status(page, "draft")
                self.expect(page.locator(MESSAGE)).to_have_text(
                    f"1 of {n} results for “{self.term}” match the active filters.")
                self.expect(page.locator(RESULT)).to_have_count(n)
                self.expect(page.locator(MORE)).to_have_count(0)
                self.assertEqual(self.shown(page), [self.urls[-1]])
                self.expect(page.locator(f"{RESULT}.search-filtered").first).to_be_hidden()
            with self.subTest(browser=browser, filter="cleared"):
                self.filter_status(page, "draft")
                self.expect(page.locator(MESSAGE)).to_have_text(f"{n} results for {self.term}")
                self.expect(page.locator(f"{RESULT}.search-filtered")).to_have_count(0)
                self.assertEqual(self.shown(page), self.urls)

    def test_nothing_passing_says_so(self) -> None:
        meta = {key(u): {"status": "Durable"} for u in self.urls}
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, _ = self.search_page(browser, meta)
                self.first_page(page)
                self.filter_status(page, "draft")
                self.expect(page.locator(MESSAGE)).to_have_text(
                    f"No results for “{self.term}” match the active filters "
                    f"({len(self.urls)} before filtering).")
                self.assertEqual(self.shown(page), [])

    def test_unclassified_pages_stay(self) -> None:
        # Only the first result has metadata; the rest are never filtered.
        meta = {key(self.urls[0]): {"status": "Durable"}}
        n = len(self.urls)
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, _ = self.search_page(browser, meta)
                self.first_page(page)
                self.filter_status(page, "draft")
                # Four of the first five pass, so one more page is loaded,
                # and only one: placeholders still filling in are not
                # taken for results that failed.
                loaded = min(2 * PAGE_SIZE, n)
                self.expect(page.locator(RESULT)).to_have_count(loaded)
                page.wait_for_timeout(1000)
                self.expect(page.locator(RESULT)).to_have_count(loaded)
                visible = loaded - 1
                more = n > loaded
                self.expect(page.locator(MESSAGE)).to_have_text(
                    f"{visible} matching results for “{self.term}” so far — {n} found before filtering."
                    if more else f"{visible} of {n} results for “{self.term}” match the active filters.")
                self.assertEqual(self.shown(page), self.urls[1:loaded])

    def test_without_pagefind(self) -> None:
        # search.js skips its setup when the bundle is missing; the page,
        # the filters and the other tab still work.
        # The semantic tab, with ?q= set, starts the model import: refused.
        missing = {"pagefind-ui": answer("", "text/plain", 404), "cdn": None}
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                context = self.browsers[browser].new_context()
                self.addCleanup(context.close)
                net = FakeNetwork(context, self.base, {"pagefind-ui": r"/pagefind/pagefind-ui\.js$",
                                                       "cdn": r"^https://cdn\.jsdelivr\.net/"},
                                  missing, local=r"pagefind/pagefind-ui\.js$")
                page = context.new_page()
                errors = []
                page.on("pageerror", lambda e: errors.append(str(e)))
                page.goto(self.base + f"/search.html?q={self.term}", wait_until="load")
                self.assertGreaterEqual(net.hits["pagefind-ui"], 1)   # Firefox asks twice
                page.click(".library-filter-toggle")
                self.expect(page.locator("#search-filters")).to_be_visible()
                page.click("#search-tab-semantic")
                self.expect(page.locator("#search-panel-semantic")).to_be_visible()
                self.assertEqual(errors, [])
                self.assertEqual(net.unexpected, [])


if __name__ == "__main__":
    unittest.main()
