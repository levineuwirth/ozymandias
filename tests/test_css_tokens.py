"""The engine's style values are tokens (B2).

tokens.css declares the type scale, font weights, corner radii, the
shadows used more than once, two dials (--tracking-scale, --motion-scale)
and the default text size, and lists the breakpoints; the rest of the
engine's CSS asks for them. Here: every rem font size a step of the scale;
no literal that a token holds; every letter spacing and every time scaled
by its dial; every token asked for declared; every width query
in the breakpoint table and every row of the table in use; the default text
size set once, in px, which utils.js reads rather than repeats. A site
checks its own sheets with text_size_overrides(root)."""

import re
import unittest
from pathlib import Path

from tests._helpers import engine_path

COMMENT = re.compile(r"/\*.*?\*/", re.S)
DECL = re.compile(
    r"(?<![\w-])(letter-spacing|font-weight|border(?:-(?:top|bottom)-(?:left|right))?-radius"
    r"|box-shadow|transition(?:-duration|-delay)?|animation(?:-duration|-delay)?)"
    r"\s*:\s*([^;{}]*?)\s*(?=;|\})", re.S)
TOKEN_DECL = re.compile(r"(--[\w-]+)\s*:\s*([^;{}]*?)\s*;", re.S)
TOKEN_USE = re.compile(r"var\((--(?:weight|radius|shadow|transition|text-size|step|accent)[\w-]*"
                       r"|--tracking-scale|--motion-scale|--type-ratio)\b")
FONT_SIZE = re.compile(r"(?<![\w-])font-size\s*:\s*([^;{}]*?)\s*(?=;|\})", re.S)
REM = re.compile(r"(?<![\w.-])\d*\.?\d+rem\b")
TIME = re.compile(r"(?<![\w.-])(\d*\.?\d+)(ms|s)\b")
SCALED_TIME = re.compile(r"calc\((?:var\(--[\w-]+,\s*)?\d*\.?\d+m?s\)? \* var\(--motion-scale\)\)")
SCALED_TRACKING = re.compile(r"calc\(-?\d*\.?\d+em \* var\(--tracking-scale\)\)")
# Letter spacing that is not tracking: the dinkus's gap, which its own
# padding-left balances; scaled alone it would sit off center.
TRACKING_EXCEPT = {("static/css/typography.css", "0.5em")}

TOKENS = engine_path("static/css/tokens.css")


def sheets(root: Path) -> dict[str, str]:
    """Each stylesheet under static/css, comments blanked."""
    return {str(p.relative_to(root)): COMMENT.sub(" ", p.read_text(encoding="utf-8"))
            for p in sorted((root / "static/css").rglob("*.css"))}


def tokens() -> dict[str, str]:
    """tokens.css's first :root block, name to value."""
    text = COMMENT.sub(" ", TOKENS.read_text(encoding="utf-8"))
    block = re.search(r":root\s*\{(.*?)\}", text, re.S).group(1)
    return {n: " ".join(v.split()) for n, v in TOKEN_DECL.findall(block)}


def declarations():
    for rel, text in sheets(engine_path("")).items():
        if rel != "static/css/tokens.css":
            for m in DECL.finditer(text):
                yield rel, m.group(1), " ".join(m.group(2).split())


def unscaled_times(value: str) -> list[str]:
    rest = SCALED_TIME.sub("", value)
    return [a + b for a, b in TIME.findall(rest) if float(a) != 0]


def breakpoint_table() -> set[tuple[str, str]]:
    """tokens.css's BREAKPOINTS rows: (kind, condition), e.g.
    ('media', 'max-width: 540px'), ('container', 'width < 47.5rem')."""
    text = TOKENS.read_text(encoding="utf-8")
    table = re.search(r"BREAKPOINTS(.*?)\*/", text, re.S).group(1)
    rows = set()
    for kind, cond, size in re.findall(
            r"^\s*(media|container)\s+(max-width|min-width|width\s*<=?|width\s*>=?)\s+(\d*\.?\d+(?:px|rem))",
            table, re.M):
        cond = " ".join(cond.split())
        rows.add((kind, f"{cond}: {size}" if kind == "media" else f"{cond} {size}"))
    return rows


def width_queries() -> dict[tuple[str, str], list[str]]:
    """Every width query in the engine's CSS and JS, with where."""
    found: dict[tuple[str, str], list[str]] = {}
    texts = dict(sheets(engine_path("")))
    for p in sorted(engine_path("static/js").glob("*.js")):
        texts[str(p.relative_to(engine_path("")))] = p.read_text(encoding="utf-8")
    for rel, text in texts.items():
        for q in re.findall(r"@media[^{]*|matchMedia\([^)]*\)", text):
            for cond, size in re.findall(r"\((max-width|min-width)\s*:\s*(\d*\.?\d+(?:px|rem|em))\)", q):
                found.setdefault(("media", f"{cond}: {size}"), []).append(rel)
        for q in re.findall(r"@container[^{]*", text):
            for cond, size in re.findall(r"\((width\s*[<>]=?)\s*(\d*\.?\d+(?:px|rem|em))\)", q):
                found.setdefault(("container", f"{' '.join(cond.split())} {size}"), []).append(rel)
    return found


def text_size_overrides(root: Path) -> list[str]:
    """A site's sheets that set --text-size, which is the reader's (set on
    <html>, or else the default): a site sets --text-size-default."""
    return [rel for rel, text in sheets(root).items()
            if rel != "static/css/tokens.css" and re.search(r"--text-size\s*:", text)]


class Tokens(unittest.TestCase):
    def test_rem_font_sizes_are_steps_of_the_scale(self):
        """A rem size in a font-size is a step (var(--step-N)); em sizes
        (relative to their surroundings), print's pt and the fluid
        clamp() display titles are left as they are."""
        bad = [(rel, " ".join(v.split())) for rel, text in sheets(engine_path("")).items()
               if rel != "static/css/tokens.css"
               for v in FONT_SIZE.findall(text)
               if REM.search(v) and not v.strip().startswith("clamp(")]
        self.assertEqual(bad, [])

    def test_the_scale_is_one_ratio_from_one_rem(self):
        values = tokens()
        self.assertRegex(values.get("--type-ratio", ""), r"^\d*\.?\d+$")
        self.assertEqual(values.get("--step-0"), "1rem")
        steps = sorted(int(n[len("--step-"):]) for n in values if n.startswith("--step-"))
        self.assertEqual(steps, list(range(steps[0], steps[-1] + 1)), "a gap in the scale")
        for n in steps:
            if n:
                near = f"--step-{n - 1 if n > 0 else n + 1}"
                op = "*" if n > 0 else "/"
                self.assertEqual(values[f"--step-{n}"], f"calc(var({near}) {op} var(--type-ratio))")

    def test_weights_are_tokens(self):
        bad = [(f, v) for f, p, v in declarations()
               if p == "font-weight" and not re.fullmatch(r"var\(--weight-[a-z]+\)( !important)?|inherit|normal", v)]
        self.assertEqual(bad, [])

    def test_no_radius_or_shadow_is_a_token_s_value_written_out(self):
        values = tokens()
        radii = {v for n, v in values.items() if n.startswith("--radius-")}
        shadows = {re.sub(r"\s*,\s*", ", ", v) for n, v in values.items() if n.startswith("--shadow-")}
        self.assertTrue(radii and shadows)
        bad = [(f, p, v) for f, p, v in declarations()
               if ("radius" in p and set(v.split()) & radii)
               or (p == "box-shadow" and re.sub(r"\s*,\s*", ", ", v) in shadows)]
        self.assertEqual(bad, [])

    def test_letter_spacing_turns_with_the_tracking_dial(self):
        bad = [(f, v) for f, p, v in declarations()
               if p == "letter-spacing" and v not in ("0", "normal")
               and not SCALED_TRACKING.fullmatch(v) and (f, v) not in TRACKING_EXCEPT]
        self.assertEqual(bad, [])

    def test_times_turn_with_the_motion_dial(self):
        """Reduced motion's !important 0.01ms is the one fixed time."""
        bad = [(f, v) for f, p, v in declarations()
               if p.startswith(("transition", "animation")) and "!important" not in v
               and unscaled_times(v)]
        bad += [("tokens.css", f"{n}: {v}") for n, v in tokens().items() if unscaled_times(v)]
        self.assertEqual(bad, [])

    def test_every_token_asked_for_is_declared(self):
        """Declared anywhere in tokens.css: the palette's (the accent) are
        declared per theme, past the first block."""
        declared = set(TOKEN_DECL.findall(COMMENT.sub(" ", TOKENS.read_text(encoding="utf-8"))))
        declared = {name for name, _ in declared}
        asked = {(rel, name) for rel, text in sheets(engine_path("")).items()
                 for name in TOKEN_USE.findall(text)}
        self.assertEqual(sorted((r, n) for r, n in asked if n not in declared), [])

    def test_every_width_query_is_in_the_breakpoint_table(self):
        table, found = breakpoint_table(), width_queries()
        self.assertTrue(table)
        self.assertEqual({q: w for q, w in found.items() if q not in table}, {})
        self.assertEqual(sorted(table - set(found)), [], "rows no query uses")

    def test_the_default_text_size_is_set_once_in_px(self):
        values = tokens()
        self.assertRegex(values.get("--text-size-default", ""), r"^\d+px$")
        self.assertEqual(values.get("--text-size"), "var(--text-size-default)")
        others = [rel for rel, text in sheets(engine_path("")).items()
                  if rel != "static/css/tokens.css" and re.search(r"--text-size(-default)?\s*:", text)]
        self.assertEqual(others, [])
        self.assertEqual(text_size_overrides(engine_path("fixture-site")), [])

    def test_utils_js_reads_the_default_rather_than_repeating_it(self):
        js = engine_path("static/js/utils.js").read_text(encoding="utf-8")
        self.assertNotRegex(js, r"base:\s*\d")
        self.assertIn("'--text-size-default'", js)
        settings = engine_path("static/js/settings.js").read_text(encoding="utf-8")
        self.assertNotRegex(settings, r"(min|max|base):\s*\d")


if __name__ == "__main__":
    unittest.main()
