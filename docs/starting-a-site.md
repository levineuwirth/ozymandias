# Starting a site

What a new site on Ozymandias needs, in the order it is best built, and what
earlier sites had to find out on the way. The reference for each mechanism
is the README; this is the path through it.

## The order

1. **The look, as one page.** Settle it before the site exists: one page
   drawn by hand, with a switch for every open choice, shown until the
   look is right. Draw an inner page beside the home page: a look that
   works once can be too much on every page. Keep the study's source, and
   any tool that made its data, in the site's repository from its first
   commit; a scratch directory is lost to a reboot.
2. **The skin, in the site alone.** Palette, faces, measure, menu: the
   site's three style hooks. This step also answers whether the look needs
   anything of the engine, cheaply, before the work that depends on it.
3. **The drawings.** Whatever the page draws by script, in the banner slot
   and in the site's own scripts.
4. **The pages.** Content, one-off pages, sections.
5. **The host.** Its nginx configuration, which the browser tests also read.

What the engine must gain for a look is a place to put it (a slot, a hook
for a script, a way to declare themes), not code that knows what the look
is. If the look needs a template the contract does not admit, raise that as
engine work before the skin, not during it.

## The repository

Start from `fixture-site/` for the shape and from the README's "A site" for
the files. Beyond those:

- **`.gitignore`.** The engine's tests require that every name the build
  refuses to publish is also ignored (`tests/test_gitignore.py`), and the
  tools write files a site should not commit: `static/**/*.webp`,
  `content/**/*.webp`, `**/*.dims.yaml`, `data/derived-images.json`,
  `data/pairings/`, `**/*.tmp`, `/IGNORE.txt`, and the `data/` build state
  the README lists. Copy an existing site's and remove what the site does
  not have.
- **`site.yaml`.** A site whose source is not public leaves out
  `source-repo`, `forge-host` and `code-license` together (README,
  "`site.yaml`"). List only the sections the site has, with `address` and
  `title` where the directory's name is not the address. Turn off every
  default switch the site does not want yet: features are easier to add
  than to take back.
- **`overrides.yaml`.** One entry for each engine file the site replaces,
  with the blob id it was reviewed against
  (`git -C engine rev-parse HEAD:<path>`), the hooks' stubs and the empty
  banner partial included.
- **`tests/`.** Three files the engine's own tests read of every site (see
  "Tests" below). Without them `make test` fails on a clean checkout.

## The skin

A site's look is `static/css/theme.css`, `fonts.css` and `site.css`, each
inside `@layer site { … }`. The `site` layer beats every engine rule,
whatever its specificity. That is why a skin needs no template, and also why
a broad site rule replaces an engine component's own without warning.

- **Set values, not whole rules, where the engine specializes.** A site's
  `:focus-visible { outline: … }` replaced the engine's focus ring
  everywhere, including on margin notes, which mark focus with a hairline
  and drew both. `:focus-visible { outline-color: … }` keeps every
  component's own marking.
- **Tokens first.** `--text-size-default` (in px; it may differ by width in
  media queries, and the settings panel calls it 100%), `--body-max-width`
  (in rem, so it follows the reader's text size), `--line-height`,
  `--page-padding`, the palette, `--font-serif`, `--font-sans` (point it at
  the text face when the site has no sans), `--font-mono`. A site may add
  tokens of its own, such as a display face.
- **The menu.** `body > header:first-of-type` may take `position: static`
  or `relative` to scroll away; `relative` keeps it over anything drawn
  behind it. Its container queries keep the phone layout's tap padding:
  change the desktop spacing inside `@container site-nav (width > 32rem)`.
  `.nav-logo` hides when the site has no mark.
- **Engine flourishes a look may not want.** The first paragraph's drop cap
  and small-caps lead-in (`#markdownBody > p:first-of-type::first-letter`
  and `::first-line`), the indent of a paragraph after a paragraph
  (`#markdownBody p + p`), hyphenation in titles (`hyphens: manual`), and
  the centered head of a dated entry (`.essay-frontmatter`). A dated
  entry's head is on a grid of its own, wider than the text: to stand it
  over the text, give it the text's width, padding and centering, including
  the full width the text takes at 680px and less.
- **Links within the site.** The build marks them for an icon
  (`data-link-icon="internal"`) whose drawing is the site's to give. A site
  that draws none must say so (`a[data-link-icon="internal"]::after {
  content: none; }`), or every internal link carries a gray square.
- **Narrow windows.** At 900px and less the engine pads the body's foot for
  its contents bar, even on a page without one. A site that draws to the
  window's foot removes it there:
  `body:not(:has(#toc-mobile-bar)) { padding-bottom: 0; }`.

### One look only

The engine always declares two themes, `light` and `dark`, and the panel
offers what the stylesheets declare. For a site with one look, set the
palette on `:root` in `theme.css`, with its `color-scheme`. That rule wins
over both engine themes, whichever is worn. Then take the panel's Theme row
away:

```css
.settings-section:has(> .settings-row > [data-action^="theme-"]) { display: none; }
```

The engine's print palette is in its own layer, so a site palette set this
way also prints. Restate the engine's print colors inside
`@media print { :root { … } }`, including any token the site's rules read
(a background gradient's stops, the link colors). Check print under both a
light and a dark system preference.

### Fonts

`tools/fonts.yaml` names each upstream at a pinned revision, and
`tools/font-sources.json` pins each download's SHA-256.
`tools/subset-fonts.py` refuses a download whose digest is not pinned, so
compute the digests before the first run, from the same URL the tool
builds: `urllib.request.quote` of the path. Run it as its header says, with
`uv run --with fonttools --with brotli`. Declare the `@font-face` rules in
`fonts.css` with the fallback markers the tool fills. List the first
screen's faces under `font-preloads` in `site.yaml`; the engine's tests
require at least one.

A family's `weight` instances a variable font at one weight before
subsetting. For a font with other axes left variable (an optical size, say)
fontTools fails there with a `KeyError` on a composite glyph. Subsetting
first and instancing after works. Until the tool does that, keep such a
font whole, one file per style.

## The drawings

- **The slot.** `templates/partials/banner.html` renders between the menu
  and the page on every page but the score reader's. Give its outer element
  `data-pagefind-ignore="all"`, and `role="presentation"` if it is a
  `header`. Its context is the site's identity and the page's `url`,
  `title` and `license` (`contract.yaml`). Front matter does not reach it,
  and `override-review.py --check` refuses a field it is not given.
- **A page's own choice.** A page that should draw differently names a page
  stylesheet in its front matter (`stylesheets: [quiet]`). That stylesheet
  sets a custom property on `:root`, and the script reads it with
  `getComputedStyle`. A page's stylesheets load before its deferred
  scripts run.
- **The scripts.** `site.yaml`'s `scripts:` loads the site's scripts on
  every page, deferred, after the engine's. A script finds its elements
  through the slot (`body > .banner-slot …`) and does nothing where there
  are none.
- **Placing drawings.** Nothing above the slot is positioned. A drawing set
  `position: absolute; top: 0` therefore stands at the top of the page,
  behind the menu, without measuring it. A canvas behind everything is
  `position: fixed` with `z-index: 0`, and the page shell and the menu take
  `position: relative` with a higher `z-index`. Only the page and the menu
  paint a background, so the shell is clear over a fixed canvas.
- **Sizes.** Work out a drawing's size from the window and from what the
  script itself set. Do not read a size back after changing it: under
  Reduce Motion the engine gives every property a 0.01ms transition, and a
  value read just after a change is the one it is leaving.
- **What moves the page.** The window's size, a late font, and the menu's
  height all move the page. The menu changes height without the window
  with the reader's text size and Focus Mode, so observe it with a
  `ResizeObserver` and lay the drawings out again when it changes.
- **Less motion.** A script follows both `prefers-reduced-motion` and
  `[data-reduce-motion]` on `<html>`, which the settings panel sets. Watch
  the attribute with a `MutationObserver`. Give CSS animations the same two
  conditions; the engine's global rule shortens them, and a site may want
  something still instead.
- **The Content-Security-Policy.** Markup a script builds must carry no
  `style="…"` attributes, which a strict policy refuses: use classes. A
  gradient stop takes its color from a class (`stop-color: var(--x)`).
  Setting `element.style` from the script is allowed.
- **Print.** Hide the drawings in `@media print`.

## The pages

A one-off page is a `standalone:` entry. A Markdown file in a standalone
page's directory without an entry stops the build, so each sub-page needs
its own entry. A section of writing served at another address takes
`address`. A fenced div (`::: works`) gives a block a class for the site's
stylesheet without HTML in the source. The home page's `<h1>` is
visually hidden (`templates/home.html`), so a visible opening line there is
the site's to style.

## Tests

`make test` runs the engine's suite against the site. Run it from a clean
checkout in the site's first week, and read every failure as a finding
about the site or the engine. A failure that keeps appearing in a scratch
run is not "known" until someone has read it.

- **`tests/routes.yaml`** (`tools/browser/routes.py`): `pages` by name,
  `kinds` with one page of each kind the site has, and every role in
  `ROLES`, as a page or `null` where the site has none. The not-found page
  is never null. Probes for the always-on features (`search`, `lightbox`,
  `pdf-links`) are listed, `[]` where the site has no page for them; a site
  with a search page must name it. A test that needs a page the site lacks
  is skipped, with the role named as the reason.
- **`tests/identity-policy.yaml`**: `site-files` names every file Git sees
  outside `engine/`, by directory where it can, and each file that
  overrides an engine file by its own path as well. `fixed-strings` and
  `fixed-patterns` may be empty.
- **`tests/identity-allowances.yaml`**: for a site with nothing to allow,
  `initial-count: 0`, `entries: []`, and `bootstrap-parent` set to the
  commit the file is first committed on top of. The check freezes it there.
- **A site's own tests** go in `tests/test_*.py`. Without any, `make test`
  says there is nothing to run there.
- **Browser tests** (`RUN_BROWSER_TESTS=1`, `make test-browser`) serve
  `_site` under the policy in the site's `nginx/security-headers.conf`, so
  they cannot run until the host step writes it. Until then, run them in a
  scratch checkout with a copy of another site's.
- **Before a review.** Visit print (light and dark system), a phone's width,
  the largest and smallest text size, Focus Mode and Reduce Motion. Earlier
  sites' reviews kept finding faults there that one view of a page missed.

## Gaps worth closing in the engine

Each of these was worked around in a site, and each is a candidate for the
engine:

- a site with one look has no way to declare it, and hides the Theme row
  by CSS;
- the internal-link icon has no default drawing;
- the narrow-window body padding is applied without a contents bar;
- `subset-fonts.py` instances before subsetting, which fails for fonts with
  other variable axes;
- the banner's context has no page field a site can set, so a page's
  choice goes through a page stylesheet.
