"""Cut each marked figure out of the marked build, as SVG, by its recorded
region; publish the figures only if every check passes.

    extract.py <paper dir> <out dir>

The paper directory holds A/main.pdf (the plain build), B/main.pdf and
B/main.ozyfig (the build with ozyport.sty's marks) and inventory.json (the
figures the source declares, from inventory.py).

<out dir> must not exist. Everything is made in a fresh work directory beside
it (<out>.work-*), and the figures move to <out dir> only when nothing was
refused. On a refusal the exit status is 1 and <out dir> is not created; the
work directory stays, as diagnostics.

Refused:
- the marked build draws any page differently from the plain build, or has
  a different number of pages;
- the marks do not match the inventory: a figure the source declares has no
  marks, or marks name a figure it does not declare; a figure lacks exactly
  one begin and one end mark; a captioned figure lacks exactly one capbegin
  and one capend, or an uncaptioned one has any; a figure has more than one
  caption (not supported); a figure's marks span pages;
- a figure's marks are on a page the build does not have (page 0 would
  otherwise select the last page), or its region is empty or inverted
  before padding, or reaches outside its page after padding;
- two regions on a page overlap;
- a word straddles a region's edge;
- the converter fails, except for the one reproduced cairo teardown abort,
  and then only with a fresh, well-formed SVG of the region's size.

Region: the float's content, from where it begins to where its caption
begins (caption below) or from where the caption ends to where the float
ends (caption above), across the float's width, on the recorded page.
pdfTeX's \\pdfsavepos counts from the page's lower left in sp; PDF user
space is in bp from the same corner."""
import json
import re
import signal
import subprocess
import sys
import tempfile
import xml.etree.ElementTree as ET
from collections import defaultdict
from html import unescape
from pathlib import Path

from PIL import Image, ImageChops, ImageFilter
from pypdf import PdfReader, PdfWriter
from pypdf.generic import RectangleObject

SP_PER_BP = 65536 * 72.27 / 72
PAD = 3  # bp: a stroke centered on the region's edge is not cut
DPI = 150
KINDS = ("begin", "capbegin", "capend", "end")
# cairo 1.18.6 (poppler 26.10) aborts here, after the SVG surface is
# finished, on figures with included images or plots. Nothing else is excused.
KNOWN_ABORT = "pdftocairo: ../cairo/src/cairo-hash.c:217: _cairo_hash_table_destroy: Assertion `hash_table->live_entries == 0' failed."


def run(*cmd):
    return subprocess.run([str(c) for c in cmd], check=True, capture_output=True, text=True).stdout


def fresh(work, name):
    """A path in a new directory of its own: nothing can be there already."""
    return Path(tempfile.mkdtemp(dir=work)) / name


def to_svg(pdf, work, box):
    """The region's PDF as SVG, or (None, why). The SVG is written to a path
    that did not exist before this call, so a file there is this call's."""
    svg = fresh(work, "figure.svg")
    r = subprocess.run(["pdftocairo", "-svg", str(pdf), str(svg)], capture_output=True, text=True)
    note = ""
    if r.returncode != 0:
        if r.returncode == -signal.SIGABRT and r.stderr.strip() == KNOWN_ABORT:
            note = " (converter hit the known cairo teardown abort)"
        else:
            return None, f"pdftocairo exited {r.returncode}: {r.stderr.strip()[-200:] or 'no message'}"
    if not svg.is_file():
        return None, "pdftocairo wrote no SVG"
    try:
        root = ET.parse(svg).getroot()
    except ET.ParseError as e:
        return None, f"the SVG is not well-formed ({e})"
    if root.tag != "{http://www.w3.org/2000/svg}svg":
        return None, f"the output is not an SVG document ({root.tag})"
    w, h = (float(root.get(k, "0").removesuffix("pt")) for k in ("width", "height"))
    if abs(w - (box[2] - box[0])) > 0.01 or abs(h - (box[3] - box[1])) > 0.01:
        return None, f"the SVG is {w:.2f}x{h:.2f} pt, the region {box[2] - box[0]:.2f}x{box[3] - box[1]:.2f}"
    return svg, note


def crop(reader, page, box, path):
    w = PdfWriter()
    w.add_page(reader.pages[page - 1])
    w.pages[0].mediabox = RectangleObject(box)
    w.pages[0].cropbox = RectangleObject(box)
    with open(path, "wb") as f:
        w.write(f)


def read_marks(path):
    """{figure: {kind: [(page, x, y, width_pt), ...]}}, every mark kept, so
    a duplicate is seen rather than overwritten."""
    marks = defaultdict(lambda: defaultdict(list))
    for k, line in enumerate(path.read_text().split("\n"), 1):
        if not line.strip():
            continue
        f = line.split()
        if len(f) != 7 or f[1] not in KINDS or not all(s.lstrip("-").isdigit() for s in (f[0], f[2], f[3], f[4])):
            raise ValueError(f"{path.name} line {k} is not a mark: {line!r}")
        marks[int(f[0])][f[1]].append((int(f[2]), int(f[3]), int(f[4]), float(f[5].rstrip("pt"))))
    return marks


def match(inventory, marks, page_count):
    """Refusals for any disagreement between the source's figures and the
    build's marks, or marks on a page the build does not have."""
    refusals = []
    declared = {f["n"]: f for f in inventory["figures"]}
    if not declared:
        refusals.append("the source declares no figures; there is nothing to extract")
    for n in sorted(set(marks) - set(declared)):
        refusals.append(f"marks name figure {n}, which the source does not declare")
    for n, fig in sorted(declared.items()):
        name = f"figure {n} ({fig['label'] or 'unlabeled'})"
        m = marks.get(n)
        if not m:
            refusals.append(f"{name}: declared in the source, but the build recorded no marks for it")
            continue
        if fig["captions"] > 1:
            refusals.append(f"{name}: {fig['captions']} captions; one caption per figure is supported")
            continue
        want = {"begin": 1, "end": 1, "capbegin": fig["captions"], "capend": fig["captions"]}
        wrong = [f"{k} {len(m.get(k, []))} (expected {c})" for k, c in want.items() if len(m.get(k, [])) != c]
        if wrong:
            refusals.append(f"{name}: marks {', '.join(wrong)}")
            continue
        pages = sorted({v[0] for vs in m.values() for v in vs})
        if len(pages) != 1:
            refusals.append(f"{name}: its marks span pages {pages}")
        elif not 1 <= pages[0] <= page_count:
            # pypdf would take page 0 as pages[-1], the last page.
            refusals.append(f"{name}: its marks are on page {pages[0]}, outside the build's pages 1 to {page_count}")
    return refusals


def main(paper, out):
    if out.exists():
        print(f"REFUSE: {out} already exists; the output must be fresh")
        return 1
    out.parent.mkdir(parents=True, exist_ok=True)
    work = Path(tempfile.mkdtemp(prefix=f"{out.name}.work-", dir=out.parent))
    A, B = paper / "A" / "main.pdf", paper / "B" / "main.pdf"
    refusals = []

    # 1. The marks move nothing: every page of the two builds draws the same.
    plain, marked = PdfReader(A), PdfReader(B)
    pages = len(plain.pages)
    if pages != len(marked.pages):
        refusals.append(f"the marked build has {len(marked.pages)} pages, the plain build {pages}")
    elif A.read_bytes() != B.read_bytes():
        for v, pdf in (("A", A), ("B", B)):
            run("pdftocairo", "-png", "-r", 72, pdf, work / f"page{v}")
        moved = []
        for p in range(1, pages + 1):
            name = f"-{p:0{len(str(pages))}d}.png"
            a = Image.open(work / f"pageA{name}").convert("RGB")
            b = Image.open(work / f"pageB{name}").convert("RGB")
            if a.size != b.size or ImageChops.difference(a, b).getbbox():
                moved.append(p)
        if moved:
            refusals.append(f"the marks moved something: pages {moved} draw differently with them")
    print(f"{paper.name}: {pages} pages")

    # 2. The marks against the source's inventory, then regions from them.
    try:
        inventory = json.loads((paper / "inventory.json").read_text())
        marks = read_marks(paper / "B" / "main.ozyfig")
    except (OSError, ValueError) as e:
        return refuse(refusals + [f"cannot read the inventory or the marks: {e}"], work)
    bad = match(inventory, marks, min(pages, len(marked.pages)))
    refusals += bad
    regions = {}
    if not bad:
        for n, m in sorted(marks.items()):
            m = {k: v[0] for k, v in m.items()}
            page, x, top, _ = m["begin"]
            # The float's width: \hsize at the caption is the float's own (for
            # figure*, at the float box's start it is still the column's).
            width_pt = max(v[3] for v in m.values())
            if "capbegin" in m and top - m["capbegin"][2] < 2 * 65536:  # caption first
                hi, lo = m["capend"][2], m["end"][2]
            else:
                hi, lo = top, m["capbegin"][2] if "capbegin" in m else m["end"][2]
            # Padding protects edge strokes only after the body has positive area.
            if not (width_pt > 0 and hi > lo):
                refusals.append(f"figure {n}: its region on page {page} is empty or inverted "
                                f"before padding (width {width_pt:g} pt, height {(hi - lo) / 65536:g} pt)")
                continue
            x0, x1 = x / SP_PER_BP, (x + width_pt * 65536) / SP_PER_BP
            box = [x0 - PAD, lo / SP_PER_BP, x1 + PAD, hi / SP_PER_BP + PAD]
            mb = marked.pages[page - 1].mediabox
            if box[0] < mb.left or box[1] < mb.bottom or box[2] > mb.right or box[3] > mb.top:
                refusals.append(f"figure {n}: its region {[round(v, 1) for v in box]} leaves page {page} "
                                f"({[round(float(v), 1) for v in (mb.left, mb.bottom, mb.right, mb.top)]})")
            else:
                regions[n] = (page, box)

    # 3. Two figures on one page must not share any of the page.
    for i in regions:
        for j in regions:
            if i < j and regions[i][0] == regions[j][0]:
                a, b = regions[i][1], regions[j][1]
                if a[0] < b[2] and b[0] < a[2] and a[1] < b[3] and b[1] < a[3]:
                    refusals.append(f"figures {i} and {j}: their regions on page {regions[i][0]} overlap")

    # 4. No word straddles a region's edge (pdftotext's y counts from the top).
    words = defaultdict(list)
    page_h, p = {}, 0
    for line in run("pdftotext", "-bbox", B, "-").split("\n"):
        if mp := re.match(r'\s*<page width="([\d.]+)" height="([\d.]+)"', line):
            p += 1
            page_h[p] = float(mp.group(2))
        elif mw := re.match(r'\s*<word xMin="([\d.]+)" yMin="([\d.]+)" xMax="([\d.]+)" yMax="([\d.]+)">(.*)</word>', line):
            wx0, wy0, wx1, wy1 = map(float, mw.groups()[:4])
            words[p].append((wx0, page_h[p] - wy1, wx1, page_h[p] - wy0, unescape(mw.group(5))))
    inside = {}
    for n, (page, box) in regions.items():
        inside[n] = [w for w in words[page] if w[0] >= box[0] and w[2] <= box[2] and w[1] >= box[1] and w[3] <= box[3]]
        cut = [w[4] for w in words[page] if w not in inside[n]
               and w[0] < box[2] and box[0] < w[2] and w[1] < box[3] and box[1] < w[3]]
        if cut:
            refusals.append(f"figure {n}: words cut by its region's edge: {cut[:8]}")

    if refusals:
        return refuse(refusals, work)

    # 5. Extract: the marked build's page cropped to the region, as SVG.
    assets = work / "assets"
    assets.mkdir()
    for n, (page, box) in regions.items():
        pdf = fresh(work, "region.pdf")
        crop(marked, page, box, pdf)
        svg, note = to_svg(pdf, work, box)
        if svg is None:
            refusals.append(f"figure {n}: {note}")
            continue
        svg.rename(assets / f"figure-{n}.svg")
        svg = assets / f"figure-{n}.svg"
        t = svg.read_text()

        # 6. Diagnostic only: the SVG drawn by librsvg against the plain
        # build's region drawn by poppler. It cannot certify every element.
        ppdf = fresh(work, "plain.pdf")
        crop(plain, page, box, ppdf)
        run("pdftocairo", "-png", "-singlefile", "-r", DPI, ppdf, ppdf.with_suffix(""))
        region = Image.open(ppdf.with_suffix(".png")).convert("RGB")
        png = fresh(work, "drawn.png")
        run("rsvg-convert", "-w", region.width, "-h", region.height, "-b", "white", "-o", png, svg)
        fig = Image.open(png).convert("RGB")

        def coarse(im):
            g = im.convert("L").filter(ImageFilter.GaussianBlur(2))
            return g.resize((g.width // 3, g.height // 3), Image.BOX).tobytes()
        cells = list(zip(coarse(fig), coarse(region)))
        differ = sum(1 for a, b in cells if abs(a - b) > 48)
        ink = sum(1 for _, b in cells if b < 200)
        print(f"  figure {n}: page {page}, {box[2] - box[0]:.0f}x{box[3] - box[1]:.0f} bp, "
              f"svg {svg.stat().st_size // 1024} KB{note}, {t.count('<path')} paths, {t.count('<image')} images; "
              f"{len(inside[n])} words inside; drawn: {differ} of {ink} inked coarse cells differ")

    if refusals:
        return refuse(refusals, work)
    assets.rename(out)
    print(f"published {len(regions)} figures to {out}; diagnostics in {work}")
    return 0


def refuse(refusals, work):
    for r in refusals:
        print(f"REFUSE: {r}")
    print(f"nothing published; diagnostics in {work}")
    return 1


if __name__ == "__main__":
    sys.exit(main(Path(sys.argv[1]), Path(sys.argv[2])))
