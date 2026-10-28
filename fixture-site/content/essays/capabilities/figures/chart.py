"""The fixture's interactive figure (.visualization): a Vega-Lite spec on
stdout, from the standard library alone."""

import json

print(json.dumps({
    "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
    "data": {"values": [{"x": 1, "y": 2}, {"x": 2, "y": 3}, {"x": 3, "y": 1}]},
    "mark": "line",
    "encoding": {"x": {"field": "x", "type": "quantitative"},
                 "y": {"field": "y", "type": "quantitative"}},
}))
