"""MTG API responses through the real popup fetch, parse, escape and cache path."""
import json
import shutil
import subprocess
import unittest

from bs4 import BeautifulSoup
from tests._helpers import engine_path

SCRIPT = r"""
const fs = require('fs'), vm = require('vm');
const [root, input] = [process.argv[1], JSON.parse(process.argv[2])];
const noop = () => {};
const document = {addEventListener: noop, readyState: 'loading', documentElement: {},
    querySelector: () => ({getAttribute: () => 'mtgwiki scryfall'})};
const window = {document, addEventListener: noop,
    matchMedia: () => ({matches: false, addEventListener: noop})};
window.window = window;
const requests = [];
const ctx = vm.createContext({window, document, URL, console, navigator: {},
    location: {href: 'https://example.org/essay/', origin: 'https://example.org'},
    fetch: async (url, init) => {
        requests.push({url, init});
        return {ok: input.ok !== false, headers: {get: () => input.type || 'application/json'},
                json: async () => input.data};
    }});
vm.runInContext(fs.readFileSync(root + '/utils.js', 'utf8'), ctx);
let src = fs.readFileSync(root + '/popups.js', 'utf8');
src = src.replace('    function bind(el, provider)',
    '    window.__getProvider = getProvider;\n    function bind(el, provider)');
vm.runInContext(src, ctx);
(async () => {
    const provider = window.__getProvider(input.href);
    const target = {getAttribute: () => input.href};
    const first = provider ? await provider(target) : null;
    const second = provider ? await provider(target) : null;
    console.log(JSON.stringify({first, second, requests}));
})().catch(e => { console.error(e); process.exitCode = 1; });
"""

CARD_URL = 'https://scryfall.com/card/znr/203/scute-swarm'
CARD = {
    'object': 'card', 'name': 'Scute Swarm', 'mana_cost': '{2}{G}',
    'type_line': 'Creature — Insect', 'power': '1', 'toughness': '1',
    'oracle_text': 'Landfall — Whenever a land enters, create a token.',
    'set_name': 'Zendikar Rising', 'collector_number': '203',
    'image_uris': {'normal': 'https://cards.scryfall.io/normal/front/test.jpg'},
}


@unittest.skipUnless(shutil.which('node'), 'node not on PATH')
class MagicPreviewTests(unittest.TestCase):
    def preview(self, href=CARD_URL, data=None, **kwargs):
        result = subprocess.run(
            ['node', '-e', SCRIPT, str(engine_path('static/js')),
             json.dumps(dict(href=href, data=CARD if data is None else data, **kwargs))],
            capture_output=True, text=True, check=True,
        )
        return json.loads(result.stdout)

    def test_exact_printing_image_rules_and_success_cache(self):
        result = self.preview()
        self.assertEqual(len(result['requests']), 1)
        self.assertEqual(result['first'], result['second'])
        request = result['requests'][0]
        self.assertEqual(request['url'], 'https://api.scryfall.com/cards/znr/203')
        self.assertEqual(request['init']['headers']['Accept'], 'application/json')
        soup = BeautifulSoup(result['first'], 'html.parser')
        self.assertEqual(soup.select_one('.popup-title').text, 'Scute Swarm')
        self.assertIn('1/1', soup.text)
        self.assertIn(CARD['oracle_text'], soup.text)
        self.assertEqual(soup.select_one('img.is-card')['src'], CARD['image_uris']['normal'])

    def test_double_faced_card_uses_front_image_and_both_rules(self):
        card = {'object': 'card', 'name': 'Front // Back', 'card_faces': [
            {'name': 'Front', 'mana_cost': '{1}{B}', 'oracle_text': 'Front rules.',
             'image_uris': CARD['image_uris']},
            {'name': 'Back', 'oracle_text': 'Back rules.'}]}
        soup = BeautifulSoup(self.preview(data=card)['first'], 'html.parser')
        self.assertIn('Front: Front rules. // Back: Back rules.', soup.text)
        self.assertEqual(soup.select_one('img')['src'], CARD['image_uris']['normal'])

    def test_language_and_collector_encoding(self):
        for path, endpoint in [('znr/203/ja/scute-swarm', 'znr/203/ja'),
                               ('m14/173/fog', 'm14/173'),
                               ('unh/42%2B/a-card', 'unh/42%2B')]:
            with self.subTest(path=path):
                result = self.preview('https://scryfall.com/card/' + path)
                self.assertEqual(result['requests'][0]['url'], 'https://api.scryfall.com/cards/' + endpoint)

    def test_wiki_extract_title_encoding_and_redirects(self):
        result = self.preview('https://mtg.wiki/page/Urza%27s_Saga#Story',
                              {'query': {'pages': {'42': {'title': "Urza's Saga", 'extract': 'A set.'}}}})
        from urllib.parse import parse_qs, urlsplit
        params = parse_qs(urlsplit(result['requests'][0]['url']).query)
        self.assertEqual(params['titles'], ["Urza's_Saga"])
        self.assertEqual(params['redirects'], ['1'])
        self.assertEqual(params['prop'], ['extracts'])
        self.assertEqual(params['explaintext'], ['1'])
        self.assertIn('A set.', result['first'])

    def test_missing_errors_and_wrong_content_type_are_not_cached(self):
        for href, data, kwargs in [
            ('https://mtg.wiki/page/Missing', {'query': {'pages': {'-1': {'missing': ''}}}}, {}),
            ('https://mtg.wiki/page/Empty', {'query': {'pages': {'1': {'title': 'Empty', 'extract': ''}}}}, {}),
            (CARD_URL, {'object': 'error', 'details': 'No card found'}, {}),
            (CARD_URL, {}, {}), (CARD_URL, CARD, {'ok': False}),
            (CARD_URL, CARD, {'type': 'text/html'}),
        ]:
            with self.subTest(href=href, data=data, kwargs=kwargs):
                result = self.preview(href, data, **kwargs)
                self.assertIsNone(result['first'])
                self.assertIsNone(result['second'])
                self.assertEqual(len(result['requests']), 2)

    def test_malformed_encoding_makes_no_request(self):
        for href in ['https://mtg.wiki/page/%ZZ', 'https://scryfall.com/card/znr/%ZZ']:
            result = self.preview(href)
            self.assertIsNone(result['first'])
            self.assertEqual(result['requests'], [])

    def test_api_text_is_escaped_and_unsafe_images_are_dropped(self):
        hostile = '<img src=x onerror=alert(1)>'
        result = self.preview(data=dict(CARD, name=hostile, oracle_text=hostile,
                                       image_uris={'normal': 'javascript:alert(1)'}))
        soup = BeautifulSoup(result['first'], 'html.parser')
        self.assertEqual(soup.select_one('.popup-title').text, hostile)
        self.assertEqual(soup.select_one('.popup-extract').text, hostile)
        self.assertFalse(soup.select('img, script, [onerror]'))
