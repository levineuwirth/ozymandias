"""Which popup provider static/js/popups.js picks for a link.

Each provider names an anchored `host` pattern for the link's hostname and a
`match` pattern for its href; getProvider needs both. Before 2026-10-04 it
tried `match` alone, unanchored, and took gist.github.com/user/id for a
GitHub repository and a path containing doi.org/10.… for a DOI (the link
icons in build/Filters/Links.hs already matched on the host). popups.js
itself runs in node, and its own getProvider is asked; it is not copied
here, so a change to how it chooses is tested too."""

import json
import re
import shutil
import subprocess
import unittest

from tests._helpers import engine_path

POPUPS_JS = engine_path("static/js/popups.js")
PROVIDER_RE = re.compile(r"name: '([a-z]+)'.*?\n\s*host:\s+(/.+?/),\n\s*match: (/.+?/),\n", re.S)

CASES = {
    "https://en.wikipedia.org/wiki/Graph_theory": "wikipedia",
    "https://de.m.wikipedia.org/wiki/Graph": "wikipedia",
    "https://evil.example/wikipedia.org/wiki/X": None,
    "https://arxiv.org/abs/2401.01234v2": "arxiv",
    "https://arxiv.org/pdf/hep-th/9901001": "arxiv",
    "https://doi.org/10.1000/xyz": "doi",
    "https://dx.doi.org/10.1000/xyz": "doi",
    "https://example.org/notes/doi.org/10.1000/x": None,
    "https://github.com/owner/repo": "github",
    "https://gist.github.com/user/abc123": None,
    "https://notgithub.com/a/b": None,
    "https://openlibrary.org/works/OL1W": "openlibrary",
    "https://www.biorxiv.org/content/10.1101/2020.01.01.000001v1": "biorxiv",
    "https://www.medrxiv.org/content/10.1101/2020.01.01.000001v1": "medrxiv",
    "https://www.youtube.com/watch?v=abc": "youtube",
    "https://youtu.be/abc": "youtube",
    "https://archive.org/details/someitem": "archive",
    "https://web.archive.org/web/2020/https://archive.org/details/x": None,
    "https://pubmed.ncbi.nlm.nih.gov/12345678/": "pubmed",
    "https://example.com/": None,
}


@unittest.skipUnless(shutil.which("node"), "node not on PATH")
class PopupProviderTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.providers = PROVIDER_RE.findall(POPUPS_JS.read_text(encoding="utf-8"))

    def test_every_provider_is_read_and_anchored(self) -> None:
        names = [name for name, _, _ in self.providers]
        self.assertEqual(len(names), POPUPS_JS.read_text(encoding="utf-8").count("match: /"))
        for name, host, _ in self.providers:
            with self.subTest(provider=name):
                self.assertRegex(host, r"^/\^.*\$/$", "host must be anchored at both ends")

    def test_provider_for_each_link(self) -> None:
        # popups.js runs whole in a bare sandbox (it waits for
        # DOMContentLoaded, which never comes). Two lines are put in
        # before bind(): one hands out getProvider, the other makes the
        # provider it returns name its entry instead of fetching.
        script = r"""
const fs = require('fs'), vm = require('vm');
const [file, cases] = [process.argv[1], JSON.parse(process.argv[2])];
let src = fs.readFileSync(file, 'utf8');
const at = src.indexOf('    function bind(el, provider)');
if (at < 0) throw new Error('popups.js has no bind()');
src = src.slice(0, at)
    + '    providerContent = function (target, entry) { return entry.name; };\n'
    + '    window.__getProvider = getProvider;\n' + src.slice(at);
const noop = () => {};
const window = {addEventListener: noop, matchMedia: () => ({matches: false, addEventListener: noop})};
window.window = window;
const ctx = vm.createContext({window, URL, console, navigator: {},
    document: {addEventListener: noop, readyState: 'loading', documentElement: {}},
    location: {href: 'https://levineuwirth.org/essays/x/', origin: 'https://levineuwirth.org'}});
vm.runInContext(src, ctx);
const out = {};
for (const href of cases) {
    const p = window.__getProvider(href);
    out[href] = p ? p(null) : null;
}
console.log(JSON.stringify(out));
"""
        done = subprocess.run(["node", "-e", script, str(POPUPS_JS), json.dumps(list(CASES))],
                              capture_output=True, text=True, check=True)
        self.assertEqual(json.loads(done.stdout), CASES)

if __name__ == "__main__":
    unittest.main()
