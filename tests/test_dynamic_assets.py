"""Late content keeps its enhancements when the initial page needs none,
and loads a switch's files only with that switch on (utils.js's
ozyEnhance, which a page has whatever its switches)."""
import json
import shutil
import subprocess
import unittest

from tests._helpers import engine_path

UTILS = engine_path("static/js/utils.js")


@unittest.skipUnless(shutil.which("node"), "node is required")
class DynamicAssetTests(unittest.TestCase):
    def run_case(self, case, switches="code math"):
        harness = r"""
const vm = require('vm'), fs = require('fs');
const added = [], links = [];
global.window = global;
global.CustomEvent = class {};
global.Event = class {};
global.dispatchEvent = () => {};
global.document = {
  baseURI: 'https://example.org/',
  addEventListener() {},
  querySelector: sel => {
    const metas = {assets: '/assets/abc123', switches: """ + json.dumps(switches) + r"""};
    const m = /^meta\[name="ozy:([a-z-]+)"\]$/.exec(sel);
    return m && m[1] in metas ? {getAttribute: () => metas[m[1]]} : null;
  },
  querySelectorAll: () => links,
  createElement: tag => ({tagName: tag, remove() { this.removed = true; }}),
  head: {appendChild(el) { added.push(el); if (el.tagName === 'link') links.push(el); }}
};
function root(code = true, math = false) {
  const node = {className: 'language-python'};
  return {nodeType: 1, dataset: {}, classList: {contains: () => false},
    querySelector: sel => math && sel === '.math' ? {} : null,
    querySelectorAll: sel => code && sel.includes('code') ? [node] : [],
    matches: () => false, dispatchEvent() {}, node};
}
vm.runInThisContext(fs.readFileSync(""" + json.dumps(str(UTILS)) + r""", 'utf8'));
const tick = () => new Promise(resolve => setImmediate(resolve));
(async () => {
""" + case + r"""
})().catch(e => { console.error(e); process.exitCode = 1; });
"""
        result = subprocess.run(["node"], input=harness, text=True,
                                capture_output=True, timeout=10)
        self.assertEqual(result.returncode, 0, result.stderr)
        return json.loads(result.stdout)

    def test_two_code_fragments_share_the_download_and_both_get_highlighted(self):
        result = self.run_case(r"""
const a = root(), b = root();
const first = ozyHighlightCode(a), second = ozyHighlightCode(b);
global.Prism = {languages: {python: {}}, highlightElement(c) { c.highlighted = true; }};
added.find(e => e.src === '/assets/abc123/js/prism.min.js').onload();
await Promise.all([first, second]);
console.log(JSON.stringify({scripts: added.filter(e => e.src).length,
  styles: links.length, a: a.node.highlighted, b: b.node.highlighted}));
""")
        self.assertEqual(result, {"scripts": 1, "styles": 1, "a": True, "b": True})

    def test_a_failed_math_download_can_be_retried(self):
        result = self.run_case(r"""
ozyEnhance(root(false, true));
added.find(e => e.src === '/katex/katex.min.js').onerror();
await tick();
ozyEnhance(root(false, true));
await tick();
console.log(JSON.stringify(added.filter(e => e.src === '/katex/katex.min.js').length));
""")
        self.assertEqual(result, 2)

    def test_a_switch_that_is_off_loads_nothing(self):
        # Its files are not published: asking for them would only fail.
        for switches in ("", "math", "code"):
            with self.subTest(switches=switches):
                result = self.run_case(r"""
ozyEnhance(root(true, true)); await tick();
console.log(JSON.stringify(added.map(e => e.src || e.href)));
""", switches)
                code = ["/assets/abc123/css/modules/syntax.css", "/assets/abc123/js/prism.min.js"]
                math = ["/katex/katex.min.css", "/assets/abc123/css/modules/katex.css", "/katex/katex.min.js"]
                self.assertEqual(sorted(result),
                                 sorted((code if "code" in switches else []) + (math if "math" in switches else [])))

    def test_plain_content_does_not_load_feature_assets(self):
        result = self.run_case(r"""
ozyEnhance(root(false)); await tick(); console.log(JSON.stringify(added.length));
""")
        self.assertEqual(result, 0)
