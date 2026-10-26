"""Semantic search on /search.html, in Chromium and Firefox, with its model
and index faked.

static/js/semantic-search.js imports transformers.js from jsDelivr, embeds
the query with a ~23 MB model, and ranks the paragraphs of
/data/semantic-index.bin by cosine similarity. Here the import is answered
with a small module whose pipeline() maps a query such as "e0" or "e0+e1"
to that unit vector, and the index holds twelve passages whose scores
against e0 fall, and against e1 rise, in index order: rankings are known
exactly, nothing is downloaded, and a test can slow, fail or report
progress on any step. The real model and index are the CSP sweep's to
run. Everything else is the built page: the tabs, the filter panel,
search-filters.js, with its metadata faked too.

Checked: the first-use messages and progress; ranking and the top-8 cut;
filters applied before the cut, re-ranked without re-embedding (F04),
status and archive filters, unclassified pages kept; a slower earlier
query never overwrites a later one, nor a cleared box (F02); failures of
the import, the pipeline, the index and a mismatched index, each with its
message, a retry that recovers and a way to keyword search; ?q= runs no
model until the tab shows (F03); the tabs' keyboard pattern (A06); result
text escaped.

    RUN_BROWSER_TESTS=1 python -m unittest tests.test_browser_semantic -v
"""

from __future__ import annotations

import json
import math
import re
import struct
import tempfile
import unittest
from pathlib import Path

from tests._browser import (BROWSERS, FakeNetwork, check_site, enforcing_csp, require_playwright,
                            requires_browser, site_server)
from tests._browser import fake_answer as answer

DIM = 384
CDN = "https://cdn.jsdelivr.net/npm/@xenova/transformers@2.17.2"

# pipeline() as transformers.js 2.x shapes it, configured by window.__fakeModel:
#   progress: progress events to report before loading, progressMs apart
#   failLoad: pipeline() rejects;  delays: {query: ms} before an embedding
FAKE_TRANSFORMERS = """
const sleep = ms => new Promise(r => setTimeout(r, ms));
const cfg = () => window.__fakeModel || {};
export const env = {};
export async function pipeline(task, model, opts) {
  window.__fakeLoads = (window.__fakeLoads || []).concat([{task, model, quantized: opts.quantized,
      localModelPath: env.localModelPath, allowRemoteModels: env.allowRemoteModels}]);
  for (const p of (cfg().progress || [])) { await sleep(cfg().progressMs || 150); opts.progress_callback(p); }
  if (cfg().failLoad) throw new Error('fake: the model would not load');
  window.__fakeEmbeds = [];
  return async function (text, options) {
    window.__fakeEmbeds.push(text);
    const ms = (cfg().delays || {})[text];
    if (ms) await sleep(ms);
    const v = new Float32Array(384);
    for (const part of text.split('+')) {
      const m = /^e(\\d+)$/.exec(part.trim());
      if (m) v[+m[1]] += 1;
    }
    let n = Math.hypot(...v);
    if (!n) { v[383] = 1; n = 1; }
    return { data: v.map(x => x / n) };
  };
}
"""

N = 12
# Passage k: angle 5k degrees from e0 towards e1. Against e0 the scores
# fall with k; against e1 they rise.
VECTORS = []
for k in range(N):
    v = [0.0] * DIM
    v[0], v[1] = math.cos(math.radians(5 * k)), math.sin(math.radians(5 * k))
    VECTORS.append(v)
INDEX = struct.pack(f"<{N * DIM}f", *[x for v in VECTORS for x in v])


def url(k: int) -> str:
    return f"/archive/item-{k}/" if k >= 10 else f"/notes/passage-{k}/"


META = [{"url": url(k), "title": f"Passage {k}", "heading": f"Passage {k}" if k % 2 else f"Section {k}",
         "excerpt": f"The text of passage {k}."} for k in range(N)]


def key(k: int) -> str:
    """search-filters.js's lookup form of url(k)."""
    return url(k) + "index.html"


# Epistemic metadata: passages 0-2 drafts, 3-7 durable, 8 without any.
EPISTEMIC = {key(k): {"status": "Draft" if k < 3 else "Durable", "importance": str(1 + k % 5),
                      "confidence": "proved" if k == 7 else str(10 * k), "scope": "broad"}
             for k in range(8)}
ARCHIVE = {key(10): {"status": "live"}, key(11): {"status": "rotted"}}

ROUTES = {
    # A retry after a failed import asks for CDN?retry=n (see loadModel).
    "cdn":       "^" + re.escape(CDN) + r"(?:\?retry=\d+)?$",
    "index":     r"/data/semantic-index\.bin$",
    "meta":      r"/data/semantic-meta\.json$",
    "epistemic": r"/data/epistemic-meta\.json$",
    "archive":   r"/data/archive-meta\.json$",
}
ANSWERS = {
    "cdn": answer(FAKE_TRANSFORMERS, "text/javascript"),
    "index": answer(INDEX, "application/octet-stream"),
    "meta": answer(META),
    "epistemic": answer(EPISTEMIC),
    "archive": answer(ARCHIVE),
}

FIRST_USE = "Preparing semantic search — fetching the language model (about 23 MB, once per browser)."
MODEL_FAILED = ("The semantic search model could not be loaded. It is a large one-time "
                "download and needs a working connection.")
INDEX_FAILED = "Semantic search is unavailable right now — its index could not be loaded."


@requires_browser
class SemanticSearch(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        check_site()
        require_playwright()
        from playwright.sync_api import expect, sync_playwright
        cls.expect = staticmethod(expect)
        tmp = Path(cls.enterClassContext(tempfile.TemporaryDirectory(prefix="browser-semantic-")))
        cls.base = cls.enterClassContext(site_server(tmp, enforcing_csp()))
        playwright = cls.enterClassContext(sync_playwright())
        cls.browsers = {}
        for name in BROWSERS:
            cls.browsers[name] = getattr(playwright, name).launch()
            cls.addClassCleanup(cls.browsers[name].close)

    def search_page(self, browser: str, model: dict | None = None, path: str = "/search.html",
                    tab: str | None = "semantic", **answers):
        """/search.html with the fakes, the fake model configured by
        `model`, and `tab` saved as the last one used. Every #semantic-status
        text is kept in window.__statuses. At cleanup: no page errors, no
        request that no route answers."""
        context = self.browsers[browser].new_context(viewport={"width": 1280, "height": 1000})
        self.addCleanup(context.close)
        script = f"window.__fakeModel = {json.dumps(model or {})};"
        if tab:
            script += f"try {{ localStorage.setItem('search-tab', '{tab}'); }} catch (e) {{}}"
        script += """
            window.__statuses = [];
            document.addEventListener('DOMContentLoaded', () => {
                const s = document.getElementById('semantic-status');
                if (!s) return;   // another frame's document
                new MutationObserver(() => window.__statuses.push(s.textContent))
                    .observe(s, {childList: true, characterData: true, subtree: true});
            });"""
        context.add_init_script(script)
        net = FakeNetwork(context, self.base, ROUTES, ANSWERS,
                          local=r"data/(?:semantic-index\.bin|semantic-meta\.json"
                                r"|epistemic-meta\.json|archive-meta\.json)$")
        net.answers.update(answers)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        page.goto(self.base + path, wait_until="load")

        def clean():
            self.assertEqual(errors, [], "page errors")
            self.assertEqual(net.unexpected, [], "requests no route answers")
        self.addCleanup(clean)
        return page, net

    def search(self, page, query: str) -> None:
        page.fill("#semantic-query", query)

    def titles(self, page) -> list[str]:
        return page.locator(".semantic-result-title").all_inner_texts()

    def expect_titles(self, page, ks: list[int]) -> None:
        self.expect(page.locator(".semantic-result-title")).to_have_text([f"Passage {k}" for k in ks])

    def status(self, page):
        return page.locator("#semantic-status")

    def open_filters(self, page) -> None:
        if page.get_attribute("#search-filters", "hidden") is not None:
            page.click(".library-filter-toggle")

    # -- tests -------------------------------------------------------------

    def test_first_use_and_ranking(self) -> None:
        progress = [{"status": "progress", "file": "model.onnx", "loaded": 5, "total": 20},
                    {"status": "progress", "file": "tokenizer.json", "loaded": 0, "total": 20},
                    {"status": "progress", "file": "model.onnx", "loaded": 20, "total": 20}]
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, net = self.search_page(browser, {"progress": progress})
                self.search(page, "e0")
                self.expect_titles(page, list(range(8)))
                self.expect(self.status(page)).to_have_text("")
                said = page.evaluate("window.__statuses")
                pct = "Preparing semantic search — {}% of a one-time ~23 MB download."
                # One aggregate percentage over every file: 5/20, then 5/40, then 25/40.
                self.assertEqual([m for m in said if m == FIRST_USE or "%" in m],
                                 [FIRST_USE, pct.format(25), pct.format(12), pct.format(50)])
                # The model is the self-hosted one, never fetched from a hub.
                self.assertEqual(page.evaluate("window.__fakeLoads"), [{
                    "task": "feature-extraction", "model": "all-MiniLM-L6-v2", "quantized": True,
                    "localModelPath": "/models/", "allowRemoteModels": False}])
                # A heading unlike the title is shown after it; results link to their pages.
                first = page.locator(".semantic-result").first
                self.expect(first.locator(".semantic-result-heading")).to_have_text("§ Section 0")
                self.expect(first.locator("a")).to_have_attribute("href", url(0))
                self.expect(page.locator(".semantic-result").nth(1).locator(
                    ".semantic-result-heading")).to_have_count(0)
                # The other way round, and loaded once.
                self.search(page, "e1")
                self.expect_titles(page, list(range(11, 3, -1)))
                self.assertEqual(len(page.evaluate("window.__fakeLoads")), 1)
                self.assertEqual((net.hits["cdn"], net.hits["index"], net.hits["meta"]), (1, 1, 1))

    def test_filters_apply_before_the_cut(self) -> None:
        for browser in BROWSERS:
            page, _ = self.search_page(browser)
            self.search(page, "e0")
            self.expect_titles(page, list(range(8)))
            embeds = len(page.evaluate("window.__fakeEmbeds"))
            self.open_filters(page)
            with self.subTest(browser=browser, filter="durable"):
                # Drafts 0-2 go; 8 and 9, with no metadata, and the archive
                # copies stay; the eight slots fill from further down (F04).
                page.click(".filter-status-btn[data-value=durable]")
                self.expect_titles(page, [3, 4, 5, 6, 7, 8, 9, 10])
            with self.subTest(browser=browser, filter="durable, importance 3+"):
                page.click(".filter-threshold-btn[data-field=importance][data-value='3']")
                # importance 1 + k % 5 >= 3: k = 3, 4, 7 among the durable ones.
                self.expect_titles(page, [3, 4, 7, 8, 9, 10, 11])
            with self.subTest(browser=browser, filter="cleared"):
                page.click(".filter-threshold-btn[data-field=importance][data-value='3']")
                page.click(".filter-status-btn[data-value=durable]")
                self.expect_titles(page, list(range(8)))
            with self.subTest(browser=browser, filter="confidence 75"):
                page.fill("#filter-confidence", "75")
                # 10k >= 75: k = 8+ have none; 7 is "proved", which passes any threshold.
                self.expect_titles(page, [7, 8, 9, 10, 11])
                page.fill("#filter-confidence", "")
            with self.subTest(browser=browser, part="no re-embedding"):
                self.assertEqual(len(page.evaluate("window.__fakeEmbeds")), embeds)

    def test_archive_filters(self) -> None:
        for browser in BROWSERS:
            page, _ = self.search_page(browser)
            self.search(page, "e1")
            self.expect_titles(page, list(range(11, 3, -1)))
            self.open_filters(page)
            with self.subTest(browser=browser, filter="exclude archive"):
                page.click(".filter-archive-mode-btn[data-value=exclude]")
                self.expect_titles(page, list(range(9, 1, -1)))
                page.click(".filter-archive-mode-btn[data-value=exclude]")
            with self.subTest(browser=browser, filter="archive only"):
                page.click(".filter-archive-mode-btn[data-value=only]")
                self.expect_titles(page, [11, 10])
            with self.subTest(browser=browser, filter="archive only, rotted"):
                page.click(".filter-archive-status-btn[data-value=rotted]")
                self.expect_titles(page, [11])
            with self.subTest(browser=browser, filter="nothing left"):
                page.click(".filter-archive-status-btn[data-value=rotted]")
                page.click(".filter-archive-status-btn[data-value=moved]")
                self.expect(page.locator(".semantic-result")).to_have_count(0)
                self.expect(self.status(page)).to_have_text(
                    "No results match the active filters. Clear a filter, or try the keyword tab.")

    def test_later_queries_win(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser, case="slower earlier query"):
                page, _ = self.search_page(browser, {"delays": {"e0": 1500}})
                self.search(page, "e0")
                page.wait_for_timeout(600)   # past the 400 ms debounce: e0 is embedding
                self.search(page, "e1")
                self.expect_titles(page, list(range(11, 3, -1)))
                page.wait_for_timeout(1500)  # e0 finishes, and must not land
                self.expect_titles(page, list(range(11, 3, -1)))
                self.expect(self.status(page)).to_have_text("")
            with self.subTest(browser=browser, case="box cleared"):
                page, _ = self.search_page(browser, {"delays": {"e0": 1500}})
                self.search(page, "e0")
                page.wait_for_timeout(600)
                self.search(page, "")
                page.wait_for_timeout(1500)
                self.expect(page.locator(".semantic-result")).to_have_count(0)
                self.expect(self.status(page)).to_have_text("")

    def test_failures_say_so_and_recover(self) -> None:
        cases = {
            "import fails": ({}, {"cdn": None}, MODEL_FAILED),
            "pipeline fails": ({"failLoad": True}, {}, MODEL_FAILED),
            "index missing": ({}, {"index": answer("", "text/plain", 404)}, INDEX_FAILED),
            "index and meta disagree": ({}, {"index": answer(INDEX[:DIM * 4 * 5],
                                                             "application/octet-stream")}, INDEX_FAILED),
        }
        for browser in BROWSERS:
            for case, (model, broken, message) in cases.items():
                with self.subTest(browser=browser, case=case):
                    page, net = self.search_page(browser, model, **broken)
                    self.search(page, "e0")
                    self.expect(self.status(page)).to_have_text(message)
                    self.expect(page.locator(".semantic-result")).to_have_count(0)
                    retry = page.locator(".semantic-retry-btn")
                    self.expect(retry).to_have_text("Try again")
                    # Mended, a retry finds it.
                    net.answers.update(ANSWERS)
                    page.evaluate("window.__fakeModel = {}")
                    retry.click()
                    self.expect_titles(page, list(range(8)))
                    self.expect(self.status(page)).to_have_text("")
                    if case == "import fails":
                        # Chromium remembers a failed module fetch for the
                        # page's life: the retry has to ask for another URL.
                        self.assertEqual([r.url for r in net.requests["cdn"]],
                                         [CDN, CDN + "?retry=1"])

    def test_keyword_instead(self) -> None:
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, _ = self.search_page(browser, cdn=None)
                self.search(page, "graph")
                button = page.locator(".semantic-keyword-btn")
                self.expect(button).to_have_text("Search by keyword instead")
                button.click()
                self.expect(page.locator("#search-tab-keyword")).to_have_attribute("aria-selected", "true")
                self.expect(page.locator("#search .pagefind-ui__search-input")).to_have_value("graph")
                self.expect(page.locator("#search .pagefind-ui__result").first).to_be_visible()

    def test_query_parameter_waits_for_the_tab(self) -> None:
        # F03: ?q= prefills both panels but downloads no model until the
        # semantic tab shows.
        for browser in BROWSERS:
            with self.subTest(browser=browser, opened="keyword"):
                page, net = self.search_page(browser, path="/search.html?q=e0", tab=None)
                self.expect(page.locator("#semantic-query")).to_have_value("e0")
                page.wait_for_timeout(1000)
                self.assertEqual(net.hits["cdn"], 0)
                page.click("#search-tab-semantic")
                self.expect_titles(page, list(range(8)))
            with self.subTest(browser=browser, opened="semantic"):
                page, net = self.search_page(browser, path="/search.html?q=e1", tab="semantic")
                self.expect_titles(page, list(range(11, 3, -1)))

    def test_tabs(self) -> None:
        # A06: a tablist with roving tabindex and manual activation.
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, net = self.search_page(browser, tab=None)
                keyword, semantic = page.locator("#search-tab-keyword"), page.locator("#search-tab-semantic")
                self.expect(keyword).to_have_attribute("aria-selected", "true")
                self.expect(keyword).to_have_attribute("tabindex", "0")
                self.expect(semantic).to_have_attribute("tabindex", "-1")
                self.expect(page.locator("#search-panel-semantic")).to_be_hidden()
                keyword.focus()
                page.keyboard.press("ArrowRight")
                self.expect(semantic).to_be_focused()
                # Focus alone does not switch: that could start a 23 MB download.
                self.expect(semantic).to_have_attribute("aria-selected", "false")
                page.keyboard.press("Enter")
                self.expect(semantic).to_have_attribute("aria-selected", "true")
                self.expect(page.locator("#search-panel-semantic")).to_be_visible()
                self.expect(page.locator("#search")).to_be_hidden()
                page.keyboard.press("Home")
                self.expect(keyword).to_be_focused()
                page.keyboard.press("End")
                self.expect(semantic).to_be_focused()
                self.assertEqual(net.hits["cdn"], 0)
                # Remembered.
                page.reload(wait_until="load")
                self.expect(page.locator("#search-tab-semantic")).to_have_attribute("aria-selected", "true")

    def test_result_text_is_escaped(self) -> None:
        hostile = [dict(m) for m in META]
        hostile[0].update(title='<img src=x onerror="window.__pwned=1">Evil',
                          heading="<b>bold</b>", excerpt="<script>window.__pwned=2</script>",
                          url='/x" onmouseover="window.__pwned=3')
        for browser in BROWSERS:
            with self.subTest(browser=browser):
                page, _ = self.search_page(browser, meta=answer(hostile))
                self.search(page, "e0")
                first = page.locator(".semantic-result").first
                self.expect(first.locator(".semantic-result-title")).to_have_text(hostile[0]["title"])
                self.expect(first.locator(".semantic-result-excerpt")).to_have_text(hostile[0]["excerpt"])
                self.expect(page.locator("#semantic-results img, #semantic-results script, "
                                         "#semantic-results b")).to_have_count(0)
                first.locator("a").hover()
                self.assertIsNone(page.evaluate("window.__pwned"))


if __name__ == "__main__":
    unittest.main()
