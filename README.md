# Ozymandias

A static site engine for long-form writing, on
[Hakyll](https://jaspervdj.be/hakyll/) and [Pandoc](https://pandoc.org/):
sidenotes, citations and a bibliography, wikilinks and backlinks,
transclusion, link previews, figures drawn at build time, scores,
photography, and search. Each site is its own repository, which checks the
engine out beside it and chooses its features in `site.yaml`; the engine
holds no site's identity.

## A site

```
ozymandias/                  this repository
my-site/
  site.yaml                  identity, feeds, sections, navigation, switches
  content/                   the writing
  templates/ static/         the site's own files, and overrides of the engine's
  overrides.yaml             which engine files the site overrides, as reviewed
  Makefile                   include engine/engine.mk, then the site's targets
  cabal.project              packages: engine
  cabal.project.freeze       a link to engine/cabal.project.freeze
  engine -> ../ozymandias    made by the Makefile, and ignored by Git
```

A site uses the engine's latest commit: there are no pins, and the tests
are what keep an engine change from breaking a site. `fixture-site/` is a
small site of this shape that the engine's own tests build.

## Building

Run the commands below from the site directory (`my-site/` above). Paths in
“this repository” refer to the engine; executable commands use the site’s
`engine/` link to reach it.

```sh
make venv          # once: the Python tools' environment, in the site's .venv
make dev           # drafts visible, served on http://localhost:8000
make watch         # Hakyll's preview server, rebuilding on save
make build         # the production build, into _site/
make test          # the engine's tests and the site's
make test-clean    # the same, on HEAD, in fresh clones of both
```

`make build` automatically commits uncommitted changes under `content/`
before building, so stability labels use a recorded revision. That commit
remains even if the build fails. Use `SKIP_SNAPSHOT=1 make build` to build
without creating it.

`make build` is incremental. `tools/build-freshness.sh` forces a full
rebuild when correctness needs one: the Haskell rules, the cabal metadata or
`site.yaml` changed, content was deleted or renamed, route-defining front
matter such as `tags:` changed, or the last full rebuild is over 7 days old,
since compile-time values like stability labels drift. Run it after adding
images or PDFs: `make dev` and `make watch` skip their WebP companions and
thumbnails, which then survive later dev builds.

A build runs only with the engine it was compiled from: the generator
embeds a digest of its Haskell sources, dependency freeze, project settings
and effective compiler invocation (`site version` prints it), and
`site build` refuses an `engine/` that holds others; `make build` compiles
it first. A production build also wants the engine committed
(`ENGINE_DEV=1` builds one being edited), and records its commit in the
site's `data/last-build-engine.txt`, for the deploy. Both repository revisions
are captured before compilation and checked again before recording a
successful build and before transfer; a commit made during the build
requires another build. `make watch` runs a
second watcher for an engine linked beside the site, whose edits Hakyll's
own watcher doesn't see. Engine inputs and site overrides trigger a clean
rebuild and restart under the same lock, including Haskell changes. A failed
compile waits for another edit rather than serving with the old generator.

`build`, `watch`, `dev` and the targets that write `_site/`, `_cache/` or
`static/` hold an exclusive lock on `data/.site-build.lock`
(`tools/with-lock.sh`, through `flock`): a second one fails at once rather
than interleave writes, and `LOCK_TIMEOUT=<seconds>` makes it wait.

**Requirements:** GHC and `cabal`; `pagefind` on `PATH`; `python3` with
PyYAML, `git`, `curl`, `unzip` and `sha256sum` (the build vendors PDF.js,
Leaflet, the search model, transformers.js and Vega through
`tools/download-*.sh`, against pinned SHA-256 sums); `flock`. Optional: `uv` (for `make venv`), `cwebp` (WebP
companions), `pdftoppm` (PDF thumbnails). Signing needs `gpg`.

## `site.yaml`

`cabal run -v0 site -- config` prints the validated configuration as JSON,
and `cabal run -v0 site -- config --make` as a GNU make fragment of
`SITE_*` variables. Compilers load `site.yaml` as a Hakyll dependency, and
the freshness check hashes it.

- **Identity:** `title`, `author`, `author-sort`, `email` (the feeds'),
  `contact-email`, `url` and `host` (`url`'s hostname, with or without
  `www.`; links to either are the site's own), `source-repo`,
  `source-branch` and `forge-host`, `content-license`, `code-license`,
  `signing-fingerprint`, and `manifest` for `/site.webmanifest`. Three of
  these may be left out, each with an effect of its own, and a site whose
  source is not public leaves out all three:
  - without `source-repo`, the engine's footer links no repository (a
    `code-license` is then named unlinked) and the source-refs switch is
    refused, so nothing is published under `/source/`;
  - without `forge-host`, no link is marked as the forge's. It says
    nothing else: a site that keeps `source-repo` still links it;
  - without `code-license`, the engine's footer names no license, linked
    or not, and the site's `LICENSE` is not published under `/licenses/`.
    With `source-repo` and the source-refs switch, a `source-preview` that
    lists `LICENSE` still publishes it under `/source/`, as it does any
    file it lists.

  `$site-source-repo$` and `$site-code-license$` are absent from templates
  where site.yaml does not state them, whatever a page's front matter
  names, so a site's own footer asks `$if(…)$` first. This is about what
  the engine's templates and rules publish; a site's own templates and
  pages say what they say. Template
  identity is HTML-escaped; manifest values are JSON-encoded.
  `content-license` is what a reader may take for granted: every page's
  `<head>` links it (`rel="license"`) where the engine knows its page. A
  page whose front matter names another `license:` is the exception, and
  says so wherever it is met: a mark among the links at its head, a Rights
  entry in the record at its foot (with `license-url:` its link, with
  `license-note:` a sentence more), a mark on its card, `dcterms.rights`
  and its own `rel="license"` in `<head>`, and `<rights>` on its feed
  entry. The copyright line is the page's year and credit; a dedication to
  the public (`Public domain`, `CC0`) has none. A page that restates the
  site's license is not an exception. A name the engine does not know,
  with no `license-url:`, is shown unlinked and reported by the build.
- **Feeds:** each a `path`, `title`, `description`, `sections`, `advertise`
  and `limit` (`null` for all entries). Only advertised feeds appear in the
  head; photography has its own image-bearing feed context. An empty feed is
  valid, its `<updated>` the build time.
- **Sections:** the sections the site has, each one of the engine's by its
  content directory (`dir`), with the label its pages carry on the New
  page (`kind`) and an ornament (`dingbat`: one the site's stylesheets
  draw, a rule for `body[data-dingbat="NAME"]`, as its `site.css` declares
  them; `site dingbats` lists them). The sections of writing are `essays`,
  `blog`, `poetry` and `fiction`, and a site lists the ones it has, any or
  none. One it does not list is not routed: it has no landing, a feed may
  not carry it, and a file under its `content/` directory stops the build.
  A section of writing may be served somewhere else than at its
  directory's name, with `address`, and its landing may be titled with
  `title`: `{dir: blog, address: notebook, title: Notebook, kind: Note}`
  is written in `content/blog/` and served at `/notebook/`: its pages,
  its collections, a page's own script (a `.js` beside it) and its
  landing, which is there before the first entry and is in the sitemap.
  `essays` publishes every other file beside a page (figures, data,
  PDFs, at the essay's address). `blog` publishes the images its pages
  show (`.avif`, `.gif`, `.jpeg`, `.jpg`, `.png`, `.svg`, `.webp`): an
  entry or a collection's landing names an image beside or below it by
  a relative path (`![The lake](first.lake.jpg)` in
  `content/blog/first.md`; a link or an `<img>` counts too), and the
  image is published at the section's address with the width, height
  and WebP companion the build's tools make for it. An image under
  `content/blog/` that no published page names is not published: one
  only a `draft: true` entry shows is withheld with it, one a published
  entry shows too stays, and `site list-unpublished` lists what is
  withheld. A feed's entries name their images, and every other
  relative link, by the whole address. No other file beside a blog
  entry is published, and none beside a page of `poetry` or `fiction`:
  a relative link to one is a missing file (`check-site` reports it),
  and those go under `static/`. Wikilinks and tags follow the address (`[[Notebook]]` is
  the landing; a tag `notebook` has no page of its own), and a page
  collection may not take it. An address may not be a directory the
  engine or the build writes (`assets`, `data`, `drafts`, `pagefind`,
  `source` and the like; the build names it), nor a page a switch that
  is on generates (`stats`, `build`, `archive`, `bibliography`). The
  importers (`import-content.py`, `import-poetry.py`) write only into a
  section the site lists. `music` and `photography`
  are listed with their switches on, and are always at `/music/` and
  `/photography/`. Labels, ornaments and order are the site's. `portals` are the
  home page's tags in order; `nav-primary` the first navigation row;
  `library` the library page's shelves and see-also links.
- **`standalone`:** the one-off pages, each a `source`, `route`, `template`
  and the `context` it compiles with (`essay`, `page`, `now`, `vita`,
  `projects`), optionally with `scores`, a `dingbat` and `flags`. A Markdown
  file in a standalone page's directory without an entry stops the build.
- **`seasons`:** dressings over a theme, each with a window (`from`, `to`,
  `MM-DD`), a `stylesheet` and optionally a `script`, which registers what it
  draws with `static/js/stage.js`.
- **`scripts`:** the site's own scripts for every page, paths under `/js/`
  (its `static/js/`), published under the asset version and loaded after
  the engine's (but `math.js`, which typesets and comes last), deferred,
  before a page's own `js:`. A registry exists only where its switch
  loads it (`window.ozyPhotography` on photography's pages), so a script
  for every page asks for one before using it. See "Overriding engine
  files" for what they may extend.
- **`reader-settings`:** choices the site gives its readers in Settings,
  each an `id`, a `label` for its row and two or more `options`, buttons
  named from their ids as themes are. The first option is the default and
  is worn as nothing; another is worn as `data-reader-ID="OPTION"` on the
  root element, kept in the reader's browser and restored before the first
  paint. What an option looks like is the site's stylesheets' to say.
- **`source-preview`:** what `/source/` publishes of the site's own files,
  for the source-refs switch's popups; `engine-repo`, where prose naming one
  of the engine's files links instead.
- **`popup-providers`:** which other sites link previews may ask, none by
  default (the names are under `popups` in `switches.yaml`).
- **`epistemic-figure`:** what stands at the head of a page with an
  epistemic profile (the epistemic and marks switches): `radar`, the
  default, the page's mark at the left and the radar at the right; `seal`,
  the mark alone at the right with the profile lettered around it; or
  `dial`, the mark inside a graduated ring. A seal or a dial is the one link
  to the record at the page's foot, where the radar and every value then
  are; the strip of values under the abstract stays with a dial and, with a
  seal, is for narrow windows.

## Switches

Features beyond the core are switches, registered in `switches.yaml` with
their kind, default and what each needs. A site sets the ones it wants under
`switches:`; the generator resolves them over the defaults and refuses a
name the registry lacks, or a switch on without what it needs. A switch may
own a content section, which `site.yaml` then lists only with it on, and
assets published only with it on. The tools read the same resolution
through `tools/sitelib.py` (`python3 engine/tools/sitelib.py switch NAME`), without the
generator.

## Overriding engine files

A site file at an engine file's path replaces it once the site's
`overrides.yaml` declares it, with the Git blob id of the engine file it was
last reviewed against. When the engine file changes, the site's `make test`
fails until the review is done: `make override-review` shows each change,
and `python3 engine/tools/override-review.py --accept <path>` records it. A template
override is also held to `contract.yaml`, which no review clears: the
templates a site may override, the fields the engine renders each with, and
the hooks its scripts and styles need in the result.

A script is better extended than copied. A site's own script, loaded after
the engine's (`site.yaml`'s `scripts` on every page, or a page's `js:` front
matter), registers into what `contract.yaml`'s `scripts` lists:
- `window.ozyPopups`: a preview for its own elements;
- `window.ozySelection`: a selection toolbar context or action;
- `window.ozyPhotography`: a layout of the photography grid, the map's
  markers, tooltips and targets, and the photographs' data (`photos()`);
- `ozy:content-added` and `window.ozyEnhance`: content added after load.

Photography is redesigned through that and its templates: every
photography template and partial is an override point, with the fields
and hooks `contract.yaml` names, and `contract.yaml`'s `data` describes
`/photography/photos.json` (every photograph's fields, as the templates
are given them), `map.json` and the photographs' sidecars, so a site's
own presentation can be built from data rather than the engine's markup.

The content event announces insertion synchronously; highlighting and
typesetting may finish later. A photography mode owns its listeners and
queued layout work: install them on entry and remove or cancel them on
leaving. Entry must tolerate repeated selection without stacking listeners.

What a switch adds to `popups.js` and `selection-popup.js` is a module of
its own (`static/js/popups/`, `static/js/selection-popup/`), served inside
that one file with the switch on (`site script-modules`), and overridable
by path like any other file. An element that draws how far the reader is
through the page carries `data-read-progress`: `reading.js` keeps
`--read-progress`, from 0 to 1, on it.

## Styling a site

The stylesheets stay separate files in the source, and a page loads them in
a few bundles, built in order (`build/EngineFiles.hs`'s `cssBundles`; `site
css-bundles` prints a site's): `core.css` (`tokens.css`, `theme.css`,
`fonts.css`, the structure sheets, `site.css`), `interact.css` and
`modules.css` (the switches' sheets), with what a page needs for itself
linked between them (sidenotes, a collection's styles, syntax colors where
it has code). The engine ships empty `theme.css`, `fonts.css` and
`site.css` hooks; override these in the site's `static/css/` and record them
in `overrides.yaml`. A bundled sheet cannot `@import`: in a bundle the rule
no longer comes first, and browsers ignore it with everything it would
bring, so the build refuses one. A page's own stylesheet or a season's,
linked alone, may.

- `theme.css`: palettes, component color tokens and font stacks.
- `fonts.css`: `@font-face` declarations, including generated fallback faces.
  Point `tools/fonts.yaml`'s `fallbacks.css` at this file. The fonts
  `tools/subset-fonts.py` subsets come from `fonts.yaml`'s upstreams, each an
  `https` address (a project's pinned revision, a foundry's download) or a
  directory in the site, every file pinned by its SHA-256 in
  `tools/font-sources.json`. Local paths are resolved before reading;
  symlinks must also point to files within the site.
- `site.css`: ornaments, brand icons, what every page of the site needs.
- A page's own styles: `static/css/NAME.css`, named in its front matter
  (`stylesheets: [NAME]`), loaded by that page alone.
- A season's stylesheet (`site.yaml`'s `seasons`): screen only, scoped to
  `[data-season]`, loaded after the site's own.

Put every site stylesheet's rules inside `@layer site { ... }`, seasons
and pages' own included: an unlayered sheet beats every layered rule,
whatever its specificity (`tests/test_css_layers.py` checks a site's).
`tokens.css` declares `@layer engine, site;` and supplies layout tokens,
generic font stacks and a default light/dark palette. Structure and feature
rules use `@layer engine`. A normal site declaration therefore wins over an
engine declaration even when the engine uses an ID selector. CSS reverses
the layer order for `!important`: the engine's (`[hidden]`, print
visibility, about 70 in all) win over a site's `!important` too, so a site
overriding one of those cannot, and should not try. Vendor styles enter the
engine layer through the Leaflet and Pagefind wrappers in
`modules/`, which `@import` them unchanged (their relative font and image
URLs stay theirs, and Pagefind writes its sheet after the build); the head
preloads each imported file, so it is fetched beside its wrapper, not after
it.

The tokens a site may set, on `:root` in its `theme.css`, besides the
palette and font stacks: `--accent` and `--accent-muted`, per theme, the
ink of what should read as set apart (the library's shelves, the links
page's sections); `--text-size-default`, in px, the size the settings
panel calls 100% (`utils.js` reads it; `--text-size` is the size in force,
the reader's choice or this, so a site sets the default, not it);
`--type-ratio`, the ratio of the type scale, whose steps `--step--5` to
`--step-8` (1rem times the ratio that many times) are every rem font size
the engine sets; `--weight-regular`, `-medium`, `-semibold` and `-bold`, the four weights the
engine asks of a face; `--radius-sm`, `-md`, `-lg`, `-xl` and `-pill`;
`--shadow-bubble` and `--shadow-plate`; `--transition-fast` and
`--transition-medium`; and two dials, `--tracking-scale`, which multiplies
every letter spacing the engine sets, and `--motion-scale`, every duration
and delay. A media query cannot read a custom property, so the breakpoints
are literals, listed in `tokens.css`. `tests/test_css_tokens.py` keeps the
engine's stylesheets to these tokens and that list.

A site's themes are its stylesheets': each rule for `[data-theme="ID"]`
alone, at the top level of `tokens.css` or `theme.css` or in an `@layer`
there, is a theme, and the settings panel offers them in the order they
first appear, labeled from their ids (`sepia-night` is "Sepia night").
Whitespace and comments around the attribute's name, `=` and value are
accepted. IDs use lowercase letters, digits and hyphens; the `system-`
prefix is reserved for the preservation tool's system-preference records.
The engine's are `light` and `dark`; a site adds its own in `theme.css`
and needs no change to a script or template. The one beside `:root`
(`tokens.css`'s `:root, [data-theme="light"]`) is worn with none chosen.
A reader who chose none while the system prefers dark wears the first
theme whose `color-scheme` is dark: the build writes each of that theme's
rules in `tokens.css` and `theme.css` again for them, as
`:root:not([data-theme])` in `@media (prefers-color-scheme: dark)`, so a
stylesheet has no such copies of its own (`site system-dark` shows what
it writes; the tests refuse a hand copy). `site themes` lists the themes,
and the build fails on a theme with no `color-scheme`, on two beside
`:root`, and on a season (`site.yaml`) whose base is not a theme or whose
id is one.

Conditional overrides of an existing theme are copied inside their
original media, supports, container and scope conditions, in the same
layer. They do not declare additional selectable themes. Print overrides
are copied too unless the rule already includes the system selector.

Each theme declares its `color-scheme`, `light` or `dark`, in its rule, as
`tokens.css`'s light and dark do (a site redefining those inherits theirs):
the browser draws scrollbars, form fields and an embedded frame's backdrop
to match, and charts (`viz.js`) take their light or dark palette from it.
A site whose print rules recolor its themes sets it back to `light` there.

The header is a sticky top bar unless a site says otherwise, and a site may
give it (`body > header:first-of-type`, the menu's) another `position`: `static`, to scroll away with the
page, or `absolute`, to stand beside it as a masthead. `nav.js` measures the
header and publishes three lengths on `:root`. `--nav-height` is what stays
over the page while it scrolls, the header's height when it is `sticky` or
`fixed` and 0 otherwise; the contents column, a jump's landing place and the
scroll-spy keep clear of it. `--nav-flow-height` is the room the header
takes in the page's flow, 0 when it is `absolute` or `fixed`; the page shell
fills the window less that. `--nav-box-height` is the header's height
wherever it stands, for the site that sets it beside the page and must keep
what shares its column (the contents) below it. The navigation is a size container, so in a
narrow masthead it takes its phone layout unasked.

The gear and its panel are the header's, and a site may pin them to the
window instead (`.settings-wrap`, `position: fixed`). For that site
`settings.js` marks the wrap, and the engine styles nothing by the marks:
`data-away` from when the reader scrolls down into the page until they
scroll back up 40px or come within two fifths of a window of the top, and
`data-near` while a mouse or pen is within 140px of the gear. A page that
opens part-way down is not away until the reader scrolls. The panel's
first button, Top of page, is the way back for a page whose header has
scrolled off.

Between the menu and the page a site may put a banner of its own:
`templates/partials/banner.html`, an override point (`contract.yaml`) that
`default.html` renders on every page with the site's identity and the page's
title and address. The score reader, which has its own shell and no menu,
has none; a site's scripts load there all the same, and should not count on
a banner. The engine's is an empty file, so a site without one has nothing
there. When the partial renders content, the engine encloses it in a
`.banner-slot` between the menu and `.page-shell`. The site controls its
content, appearance and print styles. A site using a decorative `header`
can give it `role="presentation"` so the menu remains the page's sole
banner landmark and the heading inside keeps its heading role.
`nav.js` measures the slot's in-flow height, including its content's
margins, and publishes it as `--banner-height` whenever it takes up room.
The shell subtracts that height beside `--nav-flow-height` to keep a short
page's footer at the window's foot. A site that draws its banner by script
can reserve its height in CSS before the script runs. A heading in the
banner would stand as each page's title in search results. `data-pagefind-ignore="all"`
on its element keeps both indexes from taking it.

`frontmatter.css` and `callouts.css` are always present: the title grid and
editorial callouts do not require the marks or math switches. The fixture
site demonstrates a complete skin. A mechanical restyling records before
and after builds with `tools/browser/preserve.py`; see its README for the
comparison and snapshot completeness checks.

## The identity check

`make test` checks every file the engine's repository holds, but its
vendored code, against the identity `cabal run -v0 site -- config` returns, and against the
site's `tests/identity-policy.yaml`: its own files and directories, the
engine's vendored files, and patterns for names that must not return. Every
file Git sees in the site needs exactly one owner, so a new file is
classified before it is committed. A site file at an engine file's path must
be declared in `overrides.yaml`, but for the files each repository has of
its own (`LICENSE`, `README.md`, Git's) and a link to the engine's file at
that path (`cabal.project.freeze`). This exception only concerns override
collisions; it does not exempt the engine README from identity scanning.

## What reaches `_site/`

Everything under an essay's directory that is not a page source is
published as it is, so figures, data and scripts can sit beside the prose
that uses them; a stray file there becomes a public URL, whether or not Git
ignores it, and a stray `.md` is compiled as an essay. `neverPublish` in
`build/Site.hs` keeps private-looking names out (`*.local.md`, `*.draft.md`,
keys and credentials, `.env`, editor backups, logs, `__pycache__/`), and the
artifact gate, `tools/check-site.py`, fails the build when a name on its own
list reaches `_site/` anyway. Neither can tell what an ordinary file is for:
give a private note a `.local.md` name.

First-party scripts and stylesheets are published under `/assets/<hash>/`,
a hash of all of them, so a page only ever loads the version it was built
with. `tools/asset-versions.py` keeps a site's earlier versions on its server
for 90 days after each was replaced, for pages left open, and a deploy sends
the new version before any page that names it (`tools/deploy-guard.sh`
guards the transfer). A deploy is the site's own (its Makefile), but it
should refuse an engine with uncommitted changes, or a site built from
another engine commit than the one checked out, as `data/last-build-engine.txt`
records it; `make engine-published` refuses an engine whose HEAD its origin
lacks, for a site that doesn't push the engine itself.

## Optional features

- **Related links:** `tools/embed.py` computes page embeddings into the
  site's `data/similar-links.json`; it needs `make venv`, and without it the
  build skips them with a notice. The Python project is this repository's
  (`pyproject.toml`, `uv.lock`); the environment is the site's `.venv`.
  After editing `engine/pyproject.toml`, re-lock with `uv lock --project engine`.
- **Semantic search:** `make download-model` fetches the quantized model
  `static/js/semantic-search.js` uses, and `make download-libraries` the
  transformers.js library and ONNX runtime it runs the model in (and Vega,
  for `viz`), so neither search nor a figure asks another host for code.
- **Images:** `tools/convert-images.sh` writes a `.webp` beside every JPEG
  and PNG, with `cwebp` (without it the build warns and images fall
  back), and `tools/generate-thumbnails.py` a photograph's variants
  (`view.w480.jpg`, `view.w480.webp`). Those are names an author may give
  a file too, so the tools keep a record of what they wrote
  (`data/derived-images.json`: the tool, the image it was made from and
  that image's bytes, the path, and the bytes last written) and go by
  it. The record is local state: a site keeps it out of Git with the
  files it describes, and `data/pairings/` with it, where each build
  notes what the record pairs so that a page is built again when that
  changes. Pages using images also follow changes to directory aliases
  under `content/` and `static/`. A watch checks within two seconds of a
  tool's run or a declaration's edit, and within ten of a file changed by
  hand. `site --verbose build` follows the same pairings. Under
  `data/pairings/` the build writes over and deletes only a state file
  its own manifest lists and that is still the bytes it wrote, and writes
  nothing through a link. Where it cannot keep the states (another file
  where a state file goes, a state file edited since, `data/` or
  `data/pairings/` a link, a manifest that is not its own) the build
  stops and names the file, and a watch ends: built on, a page would
  keep a pairing that no longer holds. Deleting `data/pairings/` is
  always safe; the next build writes it again. The engine's images
  have a record of their own in the engine, which its sites share; a tool
  holds a record's lock while it works, so two sites building at once
  wait for each other there.
  - a tool writes where there is no file, and writes over or prunes only
    a file the record says it made and that is unchanged since. A file saved
    by the author while an encoder runs keeps its place: a tool's own file
    is taken aside and checked before the new one goes in, so for that
    moment the name has no file (a site building just then shows the
    image alone). A run stopped in that moment leaves the file aside
    (in a directory `.previous-image-….tmp` beside it), and the next run
    puts it back before it converts anything, wherever its directory is
    by then, saying so if the record does not say it is the tool's own.
    A file gets
    its name by a hard link or, on a filesystem without them, by a rename
    that refuses an existing name; where there is neither, the tools stop
    and write no image there. Each finished
    thumbnail is recorded even if encoding its sibling fails. Its own
    file is made again when its image is not the one it was made from
    (other bytes, whichever file is older, or another file altogether;
    an image whose time alone changed, as a checkout changes it, is the
    same image). What an entry says of the image is said of the bytes the
    tool read: an image saved again while it was being converted is
    converted again, and a photograph's variants are made again by the
    next run. `--prune` finds a variant's WebP by the record when the
    variant itself is already gone. Any other
    file of that name (made by hand, shown by a page as an image of its
    own, edited after the tool wrote it) is left exactly as it is, by a
    plain run, `--force` and `--prune` alike, and is counted in the
    tool's summary;
  - a page shows an image with a WebP of its name, a photograph's srcset
    offers a variant, and the blog publishes an image's companion, only
    when the record pairs them, the bytes are still the recorded ones,
    and the image is still the one they were made from: an image replaced
    on a machine without `cwebp` is shown alone, not with the picture
    that was there before. A `lake.jpg` beside a `lake.webp` the record
    does not know is shown alone, and a page built while the two were
    paired is built again, on the next build, once they are not (the
    WebP changed by hand, say);
  - a WebP made by hand is paired by declaring it, in
    `data/companions.json` (`{"content/blog/moving.webp":
    "content/blog/moving.gif"}`; `python3 engine/tools/derived_images.py
    declare FILE.webp` writes the line, and takes the file out of the
    record if a tool made it). No tool writes to a declared file, and it
    is shown with the image declared and no other. A line is read as the
    file it leads to (`./static/lake.webp` is `static/lake.webp`, and so
    is a path through a link to that directory), by the tools and the
    build alike. A
    site declares its own files; one for an engine image goes in the
    engine's `data/companions.json`, where `declare` puts it and every
    site that links the engine reads it, and a site's own line for an
    engine file stops the tools;
  - `make adopt-images` is for the files that were there before the
    record, which is every companion of a site that upgrades: it makes
    each again in a scratch place and records those that come out byte
    for byte the same, lists the rest, and writes nothing else
    (`ADOPT_FLAGS=--dry-run` says what it would do, and changes nothing).
    An entry from an earlier form of the record, which does not say which
    bytes its image had, is completed the same way. So is an entry that
    has other bytes than the file (a run stopped after it put its file in
    place and before it saved the record): no build writes over such a
    file, which may as well be one its author changed, and the adoption
    brings the entry up to it only if it is what the tool makes of its
    image now. While a file that a stopped run took aside is not back in
    its place, an adoption (and `declare`, in that directory) changes
    nothing, names the file, and says what puts it back: the next
    conversion (`make convert-images`, or a build), the thumbnail script
    for a photograph prepared outside `content/`, or the author, where
    what was left is not as a run leaves it.
    Until then the site is correct and heavier: the images are shown
    without them.

  One name is still one image, and the converter stops the build, naming
  the files and deleting nothing, where two images would be shown with
  one companion: two beside each other that differ only in the extension
  (`map.png`, `map.JPG`, `map.gif`), or the same under `static/` and the
  engine's `static/`, which are served as one tree (the site's
  `images/color.png` and the engine's `images/color.JPG` at
  `/images/color.webp`). A site file of the engine file's exact name is
  an override, not a second image: the site's is served, and it is shown
  with the site's own companion or alone, never with the engine's.
- **PDF thumbnails:** `make pdf-thumbs`, with `pdftoppm`.
- **Scholarly papers:** an essay with `scholarly: true` carries Google
  Scholar's `citation_*` tags in its head (`build/Scholar.hs`): its title,
  each author in order (each with an institution, if given), the
  publication date, an optional DOI, and the absolute URL of its PDF.
  The fields are `citation-title`, `citation-authors`, `citation-date`
  (`YYYY-MM-DD`, `YYYY-MM` or `YYYY`) and `citation-pdf`, which default to
  `title`, the names in `authors`, `date` and `preprint`. The optional fields
  are `citation-doi` (a bare DOI), `citation-arxiv` (the page then links
  the paper's arXiv abstract page; quote it, `"2611.01230"`, or YAML reads
  a number and drops its trailing zeros) and `citation-institution`. That is one
  institution per author, as a list in author order, and it defaults to
  `affiliation` for a single author. A date is given as Scholar asks:
  the full date, or the year (a month alone gives its year). Scholar takes
  a PDF only from the page's own directory, so the build publishes a PDF
  that lives elsewhere (`/papers/x.pdf`) again beside its page, named for
  a flat page (`essays/x.md`, `essays/x.pdf`) or as it is in a page's own
  directory. When a file, or another page's copy, already has that name, the
  copy takes the next free one (`x-paper.pdf`, `index-paper.pdf`, then
  `x-2.pdf`, `x-3.pdf`, …); a flat page's own name goes to that page first.
  The copies are placed together on every build, and a page and its copy
  are rebuilt whenever their placement changes. The tags and the Preprint
  link name that copy. Titles are read as a browser shows them: inline
  markup off, character references (`&eacute;`) decoded. A flagged page missing a field, or naming a
  PDF that is not there, that the site does not publish (a refused or
  withheld file) or that is over Scholar's 5 MB limit, stops the build, as
  does the flag on any published page but an essay; a draft is not
  checked. `noindex:` is read as a boolean, the same way for the robots
  tag, the sitemap (which leaves the page out) and this check (a flagged
  page cannot be one). Flag papers only: Scholar distrusts a site that
  marks up pages that are not papers.

## This repository

- `build/`: the generator, Haskell: `Site.hs` is the rules' entry point,
  `Compilers.hs` the Pandoc pipeline, `Filters/` the AST transforms
  (sidenotes, wikilinks, transclusion, scores, figures…), `Patterns.hs` the
  content patterns the rules share. `ozymandias.cabal` beside it;
  `cabal.project.freeze`, the dependency set every site builds with.
- `templates/`, `static/`: the default templates, the CSS and JS,
  link icons and neutral defaults for a site's identity assets; the PDF.js,
  Leaflet, model, transformers.js and Vega downloads land in `static/` too.
- `tools/`: the build's Python and shell tools, and `tools/browser/`, the
  browser test harness.
- `tests/`: the engine's tests, golden pages and fixtures; `fixture-site/`,
  built under two profiles (`fixture-site/profiles.yaml`) and the variants
  that turn each remaining switch the other way, so that every switch is
  checked both ways.
- `engine.mk`: the build, test and report targets a site's Makefile
  includes; `switches.yaml`, `contract.yaml`.

## License

GPL-2.0-or-later, with an additional permission: a website the engine
generates may be conveyed under terms of its author's choice, including the
parts of the engine copied into it (see [LICENSE](LICENSE)). Earlier
releases were under the MIT License, which still applies to copies of them.
