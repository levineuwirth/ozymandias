"""The figure inventory of a LaTeX paper, read from its source: the
independent reference the extractor matches the build's marks against.

    inventory.py <main.tex>   (writes JSON to stdout)

A prototype of the source stage's figure inventory: it follows \\input and
\\include from main.tex, drops comments, and lists every figure and figure*
environment in document order, with its label and how many \\caption
commands it has (\\subcaption and \\captionof are not captions of the float).
It reads text, not TeX: a figure hidden in a conditional or built by a macro
is not seen, and the engine tool's source stage replaces this reader."""
import json
import re
import sys
from pathlib import Path

INPUT = re.compile(r"\\(?:input|include)\s*\{([^}]+)\}")
FIGURE = re.compile(r"\\begin\s*\{(figure\*?)\}(.*?)\\end\s*\{\1\}", re.S)
CAPTION = re.compile(r"\\caption\s*[\[{]")
LABEL = re.compile(r"\\label\s*\{([^}]+)\}")


def uncomment(text):
    # A % starts a comment unless escaped (an odd run of backslashes before it).
    return re.sub(r"(?<!\\)((?:\\\\)*)%.*", r"\1", text)


def expand(path, root, seen=()):
    if path in seen:
        raise ValueError(f"{path} inputs itself")
    text = uncomment(path.read_text(encoding="utf-8"))

    def sub(m):
        name = m.group(1).strip()
        if not Path(name).suffix:
            name += ".tex"
        # TeX resolves \input against the directory it runs in (the main
        # file's), not the including file's.
        target = root / name
        if not target.is_file():
            raise ValueError(f"{path.name} inputs {name}, which is not in {root}")
        return expand(target, root, seen + (path,))
    return INPUT.sub(sub, text)


def inventory(main):
    body = expand(main, main.parent)
    if "\\begin{document}" in body:
        body = body.split("\\begin{document}", 1)[1]
    figures = []
    for n, m in enumerate(FIGURE.finditer(body), 1):
        inner = m.group(2)
        labels = LABEL.findall(inner)
        figures.append({"n": n, "env": m.group(1), "label": labels[-1] if labels else None,
                        "captions": len(CAPTION.findall(inner))})
    return {"source": main.name, "figures": figures}


if __name__ == "__main__":
    json.dump(inventory(Path(sys.argv[1]).resolve()), sys.stdout, indent=1)
    print()
