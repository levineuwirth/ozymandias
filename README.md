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
Leaflet and the search model through `tools/download-*.sh`, against pinned
SHA-256 sums); `flock`. Optional: `uv` (for `make venv`), `cwebp` (WebP
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
  `signing-fingerprint`, and `manifest` for `/site.webmanifest`. Template
  identity is HTML-escaped; manifest values are JSON-encoded.
- **Feeds:** each a `path`, `title`, `description`, `sections`, `advertise`
  and `limit` (`null` for all entries). Only advertised feeds appear in the
  head; photography has its own image-bearing feed context. An empty feed is
  valid, its `<updated>` the build time.
- **Sections:** the content directories (`dir`), the label their pages carry
  on the New page (`kind`) and an ornament (`dingbat`). The engine routes
  essays, blog, poetry and fiction, and music and photography with their
  switches; labels, ornaments and order are the site's. `portals` are the
  home page's tags in order; `nav-primary` the first navigation row;
  `library` the library page's shelves and see-also links.
- **`standalone`:** the one-off pages, each a `source`, `route`, `template`
  and the `context` it compiles with (`essay`, `page`, `now`, `vita`,
  `projects`), optionally with `scores`, a `dingbat` and `flags`. A Markdown
  file in a standalone page's directory without an entry stops the build.
- **`seasons`:** dressings over a theme, each with a window (`from`, `to`,
  `MM-DD`), a `stylesheet` and optionally a `script`, which registers what it
  draws with `static/js/stage.js`.
- **`source-preview`:** what `/source/` publishes of the site's own files,
  for the source-refs switch's popups; `engine-repo`, where prose naming one
  of the engine's files links instead.
- **`popup-providers`:** which other sites link previews may ask, none by
  default (the names are under `popups` in `switches.yaml`).

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
  Point `tools/fonts.yaml`'s `fallbacks.css` at this file.
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
engine layer through the KaTeX, Leaflet and Pagefind wrappers in
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

Each theme declares its `color-scheme`, `light` or `dark`, in its rule, as
`tokens.css`'s light and dark do (a site redefining those inherits theirs):
the browser draws scrollbars, form fields and an embedded frame's backdrop
to match, and charts (`viz.js`) take their light or dark palette from it.
A site whose print rules recolor its themes sets it back to `light` there.

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
  `static/js/semantic-search.js` uses.
- **Images:** `tools/convert-images.sh` writes a `.webp` beside every JPEG
  and PNG, with `cwebp`; without it the build warns and images fall back.
- **PDF thumbnails:** `make pdf-thumbs`, with `pdftoppm`.

## This repository

- `build/`: the generator, Haskell: `Site.hs` is the rules' entry point,
  `Compilers.hs` the Pandoc pipeline, `Filters/` the AST transforms
  (sidenotes, wikilinks, transclusion, scores, figures…), `Patterns.hs` the
  content patterns the rules share. `ozymandias.cabal` beside it;
  `cabal.project.freeze`, the dependency set every site builds with.
- `templates/`, `static/`: the default templates, the CSS and JS, KaTeX,
  link icons and neutral defaults for a site's identity assets; the PDF.js,
  Leaflet and model downloads land in `static/` too.
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
