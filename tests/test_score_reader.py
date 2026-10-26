"""Gutter selection must preserve brackets even at the initial phone scale.

Exercise the reader's actual selection function with browser-style rectangles;
Chromium gives an unstroked vertical polyline zero width but nonzero height.
The visible result is also checked in Chromium and Firefox during review.
"""

import json
import re
import shutil
import subprocess
import unittest

from tests._helpers import engine_path




@unittest.skipUnless(shutil.which("node"), "node is needed to exercise the reader")
class GutterTests(unittest.TestCase):
    def test_brackets_survive_phone_scale_and_subsequent_zoom(self):
        source = (engine_path("static/js/score-reader.js")).read_text()
        function = re.search(
            r"    function gutterCopy\(.*?(?=\n    function )", source, re.S)
        self.assertIsNotNone(function)
        result = subprocess.run(
            ["node"], input=function.group() + r"""
const results = [];
for (const width of [280, 320, 768, 1440, 4600]) {
    for (const left of [0, 37, -1200]) {
        const edge = left + width * 0.1;
        function element(name, tag, x, w, h) {
            return {localName: tag,
                getBoundingClientRect: () => ({left: x, width: w, height: h}),
                cloneNode: () => name};
        }
        const node = {
            getBoundingClientRect: () => ({left, width}),
            cloneNode: () => ({children: [], appendChild(el) { this.children.push(el); }}),
            children: [
                element('paper', 'path', left, width, width * 1.3),
                element('label', 'use', left + width * 0.04, width * 0.04, 10),
                // A bracket only 0.28 CSS px left of the staff on a phone.
                element('bracket', 'polyline', edge - width * 0.001, 0, width * 0.4),
                element('staff', 'polyline', edge, width * 0.8, 0),
                element('note', 'use', edge + width * 0.1, 8, 8),
                element('defs', 'defs', left, width, width),
                element('empty', 'desc', left, 0, 0)
            ]
        };
        results.push({width, left, selected: gutterCopy(node, 0.1).children});
    }
}
console.log(JSON.stringify(results));
""", text=True, capture_output=True, check=True)
        for row in json.loads(result.stdout):
            with self.subTest(width=row["width"], left=row["left"]):
                self.assertEqual(row["selected"], ["paper", "label", "bracket"])
