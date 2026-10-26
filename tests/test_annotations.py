"""Exercise the annotation anchor's real JavaScript: the text stream, its
offset mapping, and which occurrence of a highlight's text it takes."""

import json
import shutil
import subprocess
import unittest

from tests._helpers import engine_path


@unittest.skipUnless(shutil.which("node"), "node is needed to exercise annotation anchoring")
class AnnotationAnchorTests(unittest.TestCase):
    def run_anchor(self, cases):
        source = (engine_path("static/js/annotations.js")).read_text()
        start = source.index("    function collapse(")
        end = source.index("    /* ------------------------------------------------------------------\n"
                           "       Apply a single annotation")
        script = r"""
let nodes;
const NodeFilter = {SHOW_TEXT: 4};
const document = {
    createTreeWalker() {
        let i = 0;
        return {nextNode: () => nodes[i++] || null};
    },
    createRange() {
        return {
            setStart(node, offset) { this.start = [nodes.indexOf(node), offset]; },
            setEnd(node, offset) { this.end = [nodes.indexOf(node), offset]; }
        };
    }
};
""" + source[start:end] + "\nconst cases = " + json.dumps(cases) + r""";
const results = cases.map(c => {
    nodes = c.nodes.map(n => ({nodeValue: n.text.repeat(n.repeat || 1),
        parentElement: {closest: () => n.marked || null}}));
    const stream = textStream({});
    const needle = collapse(c.needle);
    if (!needle) return null;
    const idx = locate(stream, needle, c.near ?? null, c.prefix || '', c.suffix || '');
    return idx === -1 ? null : rangeAt(stream, idx, needle.length);
});
console.log(JSON.stringify(results));
"""
        # The previous per-character charAt flattened the growing string:
        # 600k characters took ~20 s; the linear scan takes under 0.2 s.
        # A generous timeout catches the freeze without timing tiny inputs.
        result = subprocess.run(["node"], input=script, text=True,
                                capture_output=True, check=True, timeout=10)
        return json.loads(result.stdout)

    def test_whitespace_across_nodes_maps_to_original_offsets(self):
        result = self.run_anchor([
            {"nodes": [{"text": "  alpha\n "}, {"text": "\tbeta\u00a0gamma  "}],
             "needle": "alpha beta gamma"},
            {"nodes": [{"text": "hidden", "marked": True}, {"text": "visible"}],
             "needle": "visible"},
            {"nodes": [{"text": "hidden", "marked": True}, {"text": "visible"}],
             "needle": "hidden"},
            {"nodes": [{"text": "text"}], "needle": " \n\t"},
            {"nodes": [{"text": "text"}], "needle": "absent"},
        ])
        self.assertEqual(result, [
            {"start": [0, 2], "end": [1, 11]},
            {"start": [1, 0], "end": [1, 7]}, None, None, None,
        ])

    def test_the_occurrence_meant(self):
        # Highlights used to take the first occurrence of their text,
        # wherever the reader had selected it.
        page = [{"text": "The cat sat. "}, {"text": "The  cat ran.\n"}, {"text": "The cat hid."}]
        result = self.run_anchor([
            # Where the selection began (a stream index), or one past a space.
            {"nodes": page, "needle": "The cat", "near": 13},
            {"nodes": page, "needle": "The cat", "near": 12},
            {"nodes": page, "needle": "The cat", "near": 5},
            # On a later load, by the text stored around it.
            {"nodes": page, "needle": "The cat", "prefix": "ran. ", "suffix": " hid."},
            {"nodes": page, "needle": "The cat", "prefix": "sat. ", "suffix": " ran"},
            # Context that matches nowhere well, or none at all: the first.
            {"nodes": page, "needle": "The cat", "prefix": "zzz", "suffix": "zzz"},
            {"nodes": page, "needle": "The cat"},
            # Already highlighted where the selection is: refused, not moved.
            {"nodes": [{"text": "The cat", "marked": True}, {"text": " and The cat"}],
             "needle": "The cat", "near": 0},
            {"nodes": [{"text": "The cat", "marked": True}, {"text": " and The cat"}],
             "needle": "The cat"},
        ])
        self.assertEqual(result, [
            {"start": [1, 0], "end": [1, 8]},
            {"start": [1, 0], "end": [1, 8]},
            None,
            {"start": [2, 0], "end": [2, 7]},
            {"start": [1, 0], "end": [1, 8]},
            {"start": [0, 0], "end": [0, 7]},
            {"start": [0, 0], "end": [0, 7]},
            None,
            {"start": [1, 5], "end": [1, 12]},
        ])

    def test_long_page_does_not_freeze_while_locating_a_quote(self):
        result = self.run_anchor([{
            "nodes": [{"text": "hello world ", "repeat": 50000}, {"text": "the end"}],
            "needle": "world the end",
        }])
        self.assertEqual(result, [{"start": [0, 599994], "end": [1, 7]}])
