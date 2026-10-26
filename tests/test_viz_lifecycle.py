"""Vega views must not leak or render out of order during a theme change."""
import json
import shutil
import subprocess
import unittest

from tests._helpers import engine_path

VIZ = engine_path("static/js/viz.js")


@unittest.skipUnless(shutil.which('node'), 'node is required')
class VizLifecycleTests(unittest.TestCase):
    def probe(self, case):
        script = r'''
const fs = require('fs'), vm = require('vm');
const container = {}, pending = [], calls = [], released = [];
let ready, changed, systemChanged, dark = false;
global.window = global;
global.document = {
  documentElement: {dataset: {}},
  addEventListener(name, cb) { ready = cb; },
  querySelectorAll(selector) {
    return selector === 'script.vega-spec'
      ? [{textContent: '{}', closest: () => container}] : [container];
  }
};
global.MutationObserver = class { constructor(cb) { changed = cb; } observe() {} };
global.matchMedia = () => ({matches: dark, addEventListener(name, cb) {systemChanged = cb;}});
global.vegaEmbed = (el, spec) => {
  const id = calls.length; calls.push(spec.config);
  return new Promise((resolve, reject) => pending.push({reject,
    resolve: () => resolve({id, finalize() { released.push(id); }})}));
};
vm.runInThisContext(fs.readFileSync(''' + json.dumps(str(VIZ)) + r''', 'utf8'));
const tick = () => new Promise(r => setImmediate(r));
const theme = value => { document.documentElement.dataset.theme = value;
  changed([{attributeName: 'data-theme'}]); };
(async () => {
''' + case + r'''
})().catch(e => { console.error(e); process.exitCode = 1; });
'''
        result = subprocess.run(['node'], input=script, text=True, capture_output=True,
                                timeout=10)
        self.assertEqual(result.returncode, 0, result.stderr)
        return json.loads(result.stdout)

    def test_theme_changes_wait_for_the_pending_render_and_keep_only_the_latest(self):
        data = self.probe(r'''
ready(); await tick(); theme('light'); theme('dark'); await tick();
const before = calls.length;
pending[0].resolve(); await tick();
const after = calls.length;
pending[pending.length-1].resolve(); await tick();
console.log(JSON.stringify({before, after, released, current: container._vegaResult.id,
  mark: calls[calls.length-1].mark.color}));
''')
        self.assertEqual(data['before'], 1, 'overlapping renders race on the same DOM')
        self.assertEqual(data['after'], 2)
        self.assertEqual(data['released'], [0])
        self.assertEqual(data['current'], 1)
        self.assertEqual(data['mark'], '#d4d0c8')

    def test_a_failed_render_does_not_block_a_queued_theme_change(self):
        data = self.probe(r'''
ready(); await tick(); theme('dark'); await tick();
pending[0].reject(new Error('fixture failure')); await tick();
pending[pending.length-1].resolve(); await tick(); theme('light'); await tick();
console.log(JSON.stringify({calls: calls.length, released}));
''')
        self.assertEqual(data, {'calls': 3, 'released': [1]})

    def test_system_theme_is_used_only_without_an_explicit_choice(self):
        data = self.probe(r'''
ready(); await tick(); pending[0].resolve(); await tick();
dark = true; systemChanged(); await tick(); pending[1].resolve(); await tick();
theme('light'); await tick(); pending[2].resolve(); await tick();
dark = false; systemChanged(); await tick();
console.log(JSON.stringify({calls: calls.length, released}));
''')
        self.assertEqual(data, {'calls': 3, 'released': [0, 1]})
