# engine.mk — the engine's make targets: the build and its steps, the tests
# and the reports. A site's Makefile includes it from the site's root,
# having set what only the site knows:
#
#   SITE_INPUTS      what else its build compiles or publishes (BUILD_INPUTS)
#   SITE_TEST_DIRS   directories whose tests/ `make test` also runs
#
# and adds its own targets, its deploy among them.

.PHONY: build build-locked sign download-model download-pdfjs download-leaflet compress-assets convert-images thumbnails thumbnails-locked pdf-thumbs pdf-thumbs-locked watch watch-locked clean test test-clean override-review test-browser test-browser-locked validate validate-locked viz-provenance viz-provenance-check audit-viz audit-marks archive-gc archive-wayback archive-suggest archive-check dev dev-locked venv
.DEFAULT_GOAL := build

# Prerequisite orders (deploy: build -> sign; deploy-clean: clean ->
# deploy) are only correct serially; under `make -j` they could
# interleave. This build has no intra-target parallelism worth
# preserving, so disable it outright.
.NOTPARALLEL:


# How many items the generator compiles at once (build/SiteThreads.hs: a whole
# number from 1 to 256, default 4). `make build SITE_THREADS=8` or the environment.
ifdef SITE_THREADS
export SITE_THREADS
endif

# The project's Python: the venv's interpreter when `make venv` has made one,
# else the system's. Called directly, not through `uv run`, which syncs the
# environment first and so could download and install packages in the
# middle of a build or a deploy. Tools that need the venv's packages check
# for VENV_PY and skip, with a note, without it.
VENV_PY := .venv/bin/python3
PY      := $(if $(wildcard $(VENV_PY)),$(VENV_PY),python3)

# The environment, from the engine's Python project (engine/pyproject.toml
# and uv.lock) into the site's own .venv: two sites sharing an engine
# cannot change packages under each other's build. UV_PROJECT_ENVIRONMENT
# is absolute because uv resolves a relative one against the project.
# --locked, not --frozen: a lock that no longer matches pyproject.toml is
# refused instead of installed as it stands, which would leave a new
# dependency out without a word. Re-lock with `uv lock --project engine`.
venv:
	UV_PROJECT_ENVIRONMENT="$(CURDIR)/.venv" uv sync --locked --project engine

# ---------------------------------------------------------------------------
# Inter-process lock  (B06)
# ---------------------------------------------------------------------------
# _site/ and _cache/ are shared mutable state and nothing about them is
# concurrency-safe. `.NOTPARALLEL:` above only orders prerequisites within
# ONE make invocation; it does nothing about `make watch` in one terminal
# and `make deploy` in another, which is how a half-written mixture of two
# builds gets rsync --delete'd to the VPS.
#
# So each of build / deploy / watch / dev is a two-line wrapper that
# re-enters make under flock, and the real recipe lives in <name>-locked.
# The smaller targets that write _site/, _cache/ or static/ (clean, sign,
# compress-assets, convert-images, thumbnails, pdf-thumbs) and validate,
# which reads _site/, take the same lock. with-lock.sh is re-entrant, so
# when the locked build or deploy runs one of them it passes through.
# GNU make executes any recipe line containing $(MAKE) even under `-n`, and
# passes `-n` down, so `make -n build` still prints the whole real recipe in
# order rather than just the wrapper.
#
# LOCK_TIMEOUT=<seconds> waits for a busy lock instead of failing at once.
LOCK_FILE := data/.site-build.lock
WITH_LOCK  = $(call tool,with-lock.sh) $(LOCK_FILE)

# An engine tool by name: engine/tools/<name>, as tools/sitelib.py's
# engine_path has it. The site's own tools (cv-pdfs.py) are named
# directly, from tools/.
tool = engine/tools/$(1)

# Flags handed to the post-build artifact gate (tools/check-site.py).
#
# Empty by default: every check is an error, including a missing /404.html
# (build/Site.hs generates it; see F12 in the audit). Pass
# --allow-missing-404 only while bisecting a broken 404 rule.
#
# --require-webp makes any missing JPEG/PNG WebP companion fatal. `make build`
# only warns, so a machine without cwebp can still build; a site's deploy
# requires it by setting REQUIRE_WEBP, which `validate` (below) passes on.
#
# `make build CHECK_SITE_FLAGS='--warn-only'` reports
# every finding and still exits 0. That is for seeing the whole picture
# while fixes are landing — a site the gate rejects is not fit to deploy,
# and `make deploy` runs the gate again through `validate` regardless.
CHECK_SITE_FLAGS ?=

# What a build reads besides content/: the engine, the site's sources its
# rules read, and what else the site compiles or publishes (SITE_INPUTS).
# The build records them for the browser tests (tools/build-inputs.py), and
# a site's deploy can refuse a tree that differs from HEAD in any of them.
# Expanded where it is used, so SITE_INPUTS counts wherever the site sets it.
BUILD_INPUTS = engine templates static data tools site.yaml code-refs archive \
	cabal.project cabal.project.freeze Makefile $(SITE_INPUTS)

# ---------------------------------------------------------------------------
# build
# ---------------------------------------------------------------------------
# SITE_ENV is forced to production here (B08). build/Site.hs branches on
# `SITE_ENV == "dev"` to include content/drafts/ in listings, feeds and
# manifests; deleting _site/drafts afterwards does not undo a draft that
# leaked into an index page. A target-specific variable outranks the
# environment, so an inherited SITE_ENV=dev from the parent shell — the
# actual failure mode — cannot reach the compiler. `dev` and `watch` set it
# back to dev for themselves.
build: export SITE_ENV = production
build:
	@$(WITH_LOCK) $(MAKE) --no-print-directory build-locked

build-locked: export SITE_ENV = production
build-locked:
	# ---- Stage 0: snapshot, freshness decision, asset prerequisites -------
	#
	# Auto-snapshot any uncommitted content/ changes BEFORE the build
	# so the stability heuristic in build/Stability.hs sees a stable
	# git history. If a subsequent step fails, the snapshot remains in
	# the history — that's intentional. The next successful build
	# either reuses it (no new content/ changes) or appends another
	# snapshot on top, so failures don't disappear from the log.
	#
	# `git add content/` respects .gitignore, which excludes credential-
	# shaped patterns (.env, *.key, *.pem, id_rsa*, credentials*, etc.)
	# so a stray secret dropped under content/ is NOT auto-staged. To
	# intentionally commit a normally-ignored file, use `git add -f`
	# manually before running `make build`.
	#
	# The commit and its guard are pathspec-limited to content/ so that
	# anything the user had previously staged for other reasons is left
	# staged, not silently swept into the auto-commit.
	#
	# SKIP_SNAPSHOT=1 skips the auto-commit entirely, for building
	# someone else's checkout or verifying a build without touching the
	# history. The build is otherwise identical; only the stability
	# labels of just-edited pieces can differ.
	@if [ "$(SKIP_SNAPSHOT)" = "1" ]; then \
	  echo "build: SKIP_SNAPSHOT=1 — not auto-committing content/"; \
	else \
	  git add content/; \
	  git diff --cached --quiet -- content/ || git commit -m "auto: $$(date -u +%Y-%m-%dT%H:%M:%SZ)" -- content/; \
	fi
	@mkdir -p data
	@date +%s > data/build-start.txt
	# Recorded again as the last step: until then _site is not what its
	# inputs describe (tools/build-inputs.py, read by the browser tests).
	@rm -f data/build-inputs.json
	# Full-rebuild-or-incremental decision. Four triggers force a clean
	# here (Hakyll rules changed, content deleted/renamed, route-defining
	# frontmatter changed, stale full-rebuild stamp); otherwise the build
	# is incremental. This is what lets `make deploy` skip its old
	# unconditional `clean`.
	@$(call tool,build-freshness.sh) check
	# Responsive photo variants (P01). Must come BEFORE convert-images so
	# each .w480/.w960/.w1440 file gets a .webp companion of its own, and
	# before the dimension sidecars below (which skip variants outright).
	@$(MAKE) -s thumbnails
	@$(call tool,convert-images.sh)
	@$(MAKE) -s pdf-thumbs
	@$(call tool,download-pdfjs.sh)
	@$(call tool,download-leaflet.sh)
	# The semantic-search model (engine/static/models/, gitignored), like the two
	# above: without it the deploy's rsync --delete would remove the
	# published model (audit D07). Skips present files after re-checking
	# their pinned SHA-256.
	@$(call tool,download-model.sh) >/dev/null
	# Photography pipeline (Phase 3): generate per-photo EXIF + palette
	# sidecars under content/photography/**/*.{exif,palette}.yaml so the
	# Hakyll context can merge them with frontmatter at compile time.
	# Plus per-image dimension sidecars across static/images/ and
	# content/** so build/Filters/Images.hs can attach width / height
	# attrs to body images for CLS prevention.
	# Gated on .venv presence, same as embed.py — failures are non-fatal.
	@if [ -x $(VENV_PY) ]; then \
	  $(VENV_PY) $(call tool,extract-exif.py)       || echo "Warning: EXIF extraction failed (build continues with frontmatter only)"; \
	  $(VENV_PY) $(call tool,extract-palette.py)    || echo "Warning: palette extraction failed (build continues with frontmatter only)"; \
	  $(VENV_PY) $(call tool,extract-dimensions.py) || echo "Warning: dimension extraction failed (build continues without width/height attrs)"; \
	else \
	  echo "Photography sidecars skipped: run 'make venv' to enable EXIF + palette + dimension extraction (build continues with frontmatter only)"; \
	fi
	# Archive pipeline (Phase 1): fetch any manifest URL without a local
	# artifact, extract text, write archive/<slug>/PROVENANCE.json and
	# data/archive-index.json. Gated on .venv, same as embed.py. A SHA or
	# slug-URL integrity error exits non-zero and halts the build; a
	# transient network failure is non-fatal (the entry retries next build).
	@if [ -x $(VENV_PY) ]; then \
	  $(VENV_PY) $(call tool,archive.py) fetch; \
	else \
	  echo "Archive fetch skipped: run 'make venv' to enable link archiving (build continues)"; \
	fi
	# Code-reference snapshots: fetch any GitHub blob/tree/commit linked from
	# content/ that has no snapshot in code-refs/ yet, for the hover popup.
	# Python stdlib plus the site draft scanner; not gated on .venv.
	# Network failures are warnings;
	# the link keeps its previous snapshot or gets no code popup.
	@python3 $(call tool,code-refs.py) fetch
	# ---- Stage 1: compile (produces the HTML that embed.py reads) --------
	cabal run site -- build
	# Purge dev/watch leftovers BEFORE embedding or indexing: drafts under
	# _site/drafts/ (written only by SITE_ENV=dev builds into this same
	# _site) must never reach the embedding outputs, pagefind, or the VPS.
	# This runs before embed.py specifically so a draft paragraph cannot
	# enter data/semantic-index.bin, which ships to every visitor.
	@if [ -d _site/drafts ]; then \
	  echo "build: removing _site/drafts (dev leftovers)"; \
	  rm -rf _site/drafts; \
	fi
	# ---- Stage 2: embed --------------------------------------------------
	#
	# embed.py reads _site/**/*.html and writes three files back into
	# data/: similar-links.json and the semantic-index.bin /
	# semantic-meta.json pair (copied into _site/data/ by a Hakyll match
	# rule). Then `site footer-data` splits similar-links.json and stage 1's
	# _site/data/backlinks.json into one file per page under data/footer/,
	# rewriting only the files whose entries changed (build/FooterData.hs,
	# audit H01). It runs whether or not embed.py did: backlinks need it too.
	#
	# It used to run AFTER the only compile pass, which meant every one of
	# those three outputs reached the VPS exactly one build late: the
	# published semantic metadata described the previous edit of the site,
	# and a new essay's Related section did not appear until the build
	# after the one that created it. (B01)
	@touch data/.embed-start
	@if [ -x $(VENV_PY) ]; then \
	  HF_HUB_DISABLE_IMPLICIT_TOKEN=1 $(VENV_PY) $(call tool,embed.py) || echo "Warning: embedding failed — data/similar-links.json not updated (build continues)"; \
	else \
	  echo "Embedding skipped: run 'make venv' to enable similar-links (build continues)"; \
	fi
	cabal run site -- footer-data
	# ---- Stage 3: recompile the consumers of what stage 2 produced -------
	#
	# The footer files and the semantic pair are matched Hakyll resources:
	# a page whose Backlinks or Related changed recompiles, and the semantic
	# pair is copied into _site/data/ — including on a first-ever build,
	# where none of these existed as identifiers during stage 1. Stage 2
	# leaves identical bytes alone, so when nothing it writes is newer than
	# its start there is nothing for this pass to do, and it is skipped
	# rather than paying a second Hakyll start-up and dependency check
	# (audit D16).
	@if [ -n "$$(find data/footer -newer data/.embed-start -print -quit)" ] \
	   || [ data/semantic-index.bin -nt data/.embed-start ] \
	   || [ data/semantic-meta.json -nt data/.embed-start ]; then \
	  echo "cabal run site -- build"; cabal run site -- build; \
	else \
	  echo "build: stage 2 changed nothing — the second compile pass is skipped"; \
	fi
	@$(call tool,build-freshness.sh) stamp
	# Defense in depth: stage 3 runs with SITE_ENV=production and cannot
	# create drafts, but an aborted earlier run could have left some.
	@if [ -d _site/drafts ]; then \
	  echo "build: removing _site/drafts (post-compile)"; \
	  rm -rf _site/drafts; \
	fi
	# Hakyll never deletes an output whose source is gone, and the score
	# pages are gitignored, so build-freshness never sees them go: remove
	# published score files that no longer have a source (audit M02).
	@python3 $(call tool,prune-site.py) _site
	# ---- Stage 4: index --------------------------------------------------
	# Runs after stage 3 so the keyword index covers the Related sections.
	# Indexed into a scratch directory and copied over by content, so
	# unchanged index files keep their mtimes (deploys and readers' caches
	# skip them) while stale fragments are still removed; see the script.
	$(call tool,pagefind-index.sh) _site
	# ---- Stage 5: stamp --------------------------------------------------
	# The site-wide build time every footer shows, written once to
	# _site/build/time.txt; static/js/nav.js puts it in each page's empty
	# <span data-build-time>. Until 2026-10-06 it was stamped into every
	# page (P05), so every build rewrote ~500 unchanged pages and every
	# deploy re-signed and re-sent them; pages now stay byte-identical
	# until their content changes. See tools/stamp-build-time.py.
	@python3 $(call tool,stamp-build-time.py) _site
	@$(call tool,compress-assets.sh) _site
	# ---- Stage 6: gate ---------------------------------------------------
	# Reject the finished artifact before anything can sign or ship it:
	# published private/ignored files (S01), figure-render error blocks and
	# unresolved "Figure ?" references that Hakyll happily exits 0 on
	# (B03), draft links (B08), missing images, broken feeds. See
	# tools/check-site.py. `make validate` runs the same gate by hand.
	@python3 $(call tool,check-site.py) _site $(CHECK_SITE_FLAGS)
	# IGNORE.txt pins pages against stability changes for one build
	# (build/Stability.hs). Clear it only once the build has passed the
	# gate: a rejected build has not used the pins.
	@: > IGNORE.txt
	@BUILD_END=$$(date +%s); \
	 BUILD_START=$$(cat data/build-start.txt); \
	 echo $$((BUILD_END - BUILD_START)) > data/last-build-seconds.txt.tmp && \
	 mv data/last-build-seconds.txt.tmp data/last-build-seconds.txt
	# What this build was built from, for tests that need _site to match
	# the tree (tests/_browser.py). Last, so nothing the build writes
	# afterwards reads as a change.
	@python3 $(call tool,build-inputs.py) record $(BUILD_INPUTS) content

sign:
	@$(WITH_LOCK) $(call tool,sign-site.sh)

# Download the quantized ONNX model for client-side semantic search.
# Run once; files are gitignored. Safe to re-run (skips existing files).
download-model:
	@$(call tool,download-model.sh)

# Vendor Mozilla's prebuilt PDF.js viewer into engine/static/pdfjs/.
# Runs automatically as part of `build` (skips when already present).
# Files are gitignored; sha256-verified against engine/tools/pdfjs-checksums.sha256.
download-pdfjs:
	@$(call tool,download-pdfjs.sh)

# Vendor Leaflet + leaflet.markercluster into engine/static/leaflet/.
# Used only by /photography/map/. Runs automatically as part of `build`
# (skips when already present). Files are gitignored; sha256-verified
# against engine/tools/leaflet-checksums.sha256.
download-leaflet:
	@$(call tool,download-leaflet.sh)

# Generate .gz and .br sidecars for compressible text assets in _site/.
# Runs automatically as part of `build`. Pairs with `gzip_static` /
# `brotli_static` in the nginx vhost (see nginx/static-assets.conf).
compress-assets:
	@$(WITH_LOCK) $(call tool,compress-assets.sh) _site

# Convert JPEG/PNG images to WebP companions (also runs automatically in build).
#
# Requires the cwebp binary. On Arch that is libwebp-utils, NOT libwebp —
# libwebp is the library only and installing it leaves cwebp absent, which
# is exactly how this site came to ship 375 JPEGs and zero WebP files while
# every build printed "OK". (C08, P02)
#   Arch    pacman -S libwebp-utils
#   Debian  apt install webp
# The script still exits 0 when cwebp is missing (the <picture> sources are
# only emitted for .webp files that exist, so the site stays correct), but
# it now says so loudly. `make validate REQUIRE_WEBP=1` makes it fatal, and
# `make deploy` sets that itself.
convert-images:
	@$(WITH_LOCK) $(call tool,convert-images.sh)

# Generate responsive delivery variants for photography (also runs in build).
#
# Every photography surface asked for the full 2400px JPEG: a contact-sheet
# frame is ~220 CSS pixels wide and a map tooltip is 224, so an index page
# cost 32-40 MB to paint thumbnails. This writes a ladder of siblings the
# templates offer through `srcset` and lets the browser choose (P01):
#
#   content/photography/<series>/<name>.jpg
#     -> <name>.w480.jpg  <name>.w960.jpg  <name>.w1440.jpg
#
# Each width only when the source is strictly wider; same directory, same
# extension, same colour profile; JPEG q82 progressive; no EXIF. Reruns
# rewrite nothing (a variant newer than its source is skipped), so this is
# cheap on every build after the first.
#
# The files are gitignored (content/photography/**/*.jpg already covers
# them) and reach the VPS through `make deploy`'s rsync of _site/, exactly
# like the delivery JPEGs they came from.
#
#   make thumbnails                              generate what is missing
#   make thumbnails THUMBNAIL_FLAGS=--dry-run    report, write nothing
#   make thumbnails THUMBNAIL_FLAGS=--force      rewrite every variant
#   make thumbnails THUMBNAIL_FLAGS=--prune      drop orphaned variants
#
# Without Pillow the script prints an install hint and exits 0, the same
# contract convert-images.sh keeps for a missing cwebp: the site stays
# correct, just heavier. tools/check-site.py is what makes a genuinely
# missing srcset target fatal.
thumbnails:
	@$(WITH_LOCK) $(MAKE) --no-print-directory thumbnails-locked

thumbnails-locked:
	@$(PY) $(call tool,generate-thumbnails.py) $(THUMBNAIL_FLAGS)

# Generate first-page thumbnails for the PDFs under static/ (also runs in
# build). Requires pdftoppm: pacman -S poppler  /  apt install poppler-utils
# Each is written as foo.thumb.png beside its PDF. Without pdftoppm the
# target says so and exits 0.
pdf-thumbs:
	@$(WITH_LOCK) $(MAKE) --no-print-directory pdf-thumbs-locked

pdf-thumbs-locked:
	# A failing pdftoppm must at least warn: the `find | while` pipeline's
	# exit status is the last iteration's, so without the `||` a corrupt
	# PDF would silently ship without a thumbnail.
	# Walk ALL of static/ (not just papers/): /cv.pdf and /resume.pdf are
	# the most-linked PDFs on the site and need hover thumbnails too.
	# An old static/pdfjs/ (download-pdfjs.sh refuses it) is pruned.
	@if command -v pdftoppm >/dev/null 2>&1; then \
	  find static -path static/pdfjs -prune -o -name '*.pdf' -print 2>/dev/null | while read pdf; do \
	    thumb="$${pdf%.pdf}.thumb"; \
	    if [ ! -f "$${thumb}.png" ] || [ "$$pdf" -nt "$${thumb}.png" ]; then \
	      echo "  pdf-thumb $$pdf"; \
	      pdftoppm -r 100 -f 1 -l 1 -png -singlefile "$$pdf" "$$thumb" \
	        || echo "Warning: pdf-thumb failed for $$pdf (page ships without a thumbnail)" >&2; \
	    fi; \
	  done; \
	else \
	  echo "pdf-thumbs: pdftoppm not found — install poppler (skipping)"; \
	fi

watch:
	@$(WITH_LOCK) $(MAKE) --no-print-directory watch-locked

watch-locked: export SITE_ENV = dev
watch-locked:
	cabal run site -- watch

clean:
	@$(WITH_LOCK) cabal run site -- clean

# ---------------------------------------------------------------------------
# test / validate
# ---------------------------------------------------------------------------
# `make test` runs every stdlib-unittest suite (B10):
#
#   engine/tests/         the engine's — figures, imports, the artifact gate
#   tests/                the site's own, if it has them
#   <dir>/tests/          for each of the site's SITE_TEST_DIRS
#
# The last are named because `discover -s tests` only looks at the
# top-level directory: a suite elsewhere once went unrun (B10, 20 tests).
# engine/ is on PYTHONPATH for the first two: tests/ has no __init__.py, so
# `tests` is one namespace package over the site's and the engine's, and
# `from tests._helpers import ...` finds the engine's helpers from either.
#
# -v is deliberate: several tests self-skip — engine/tests/test_viz.py needs
# matplotlib and skips without .venv, and its built-page checks skip again
# when _site/ is absent. A quiet run of 35 tests where 21 skipped printed
# "OK" and looked like coverage. With -v every skip names itself.
#
# REQUIRE_VENV=1 turns a missing .venv into a failure instead of a
# degraded run. `validate` sets it, because a deploy gated on a suite that
# silently skipped most of itself is not a gate.
test:
	@if [ -d .venv ]; then \
	  PY="$(CURDIR)/.venv/bin/python3"; \
	elif [ "$(REQUIRE_VENV)" = "1" ]; then \
	  echo "test: REQUIRE_VENV=1 but .venv/ is absent — run 'make venv'." >&2; \
	  echo "      Without it engine/tests/test_viz.py skips every figure check." >&2; \
	  exit 1; \
	else \
	  PY=python3; \
	  echo "test: .venv/ absent — running with system python3; expect skips."; \
	fi; \
	export PYTHONPATH="$(CURDIR)/engine$${PYTHONPATH:+:$$PYTHONPATH}"; \
	echo "== engine/tests/ =="; \
	"$$PY" -m unittest discover -s engine/tests -v || exit 1; \
	if [ -d tests ]; then \
	  echo "== tests/ =="; \
	  "$$PY" -m unittest discover -s tests -v || exit 1; \
	else \
	  echo "tests/ not present — skipping"; \
	fi; \
	for dir in $(SITE_TEST_DIRS); do \
	  if [ -d "$$dir/tests" ]; then \
	    echo "== $$dir/tests/ =="; \
	    ( cd "$$dir" && "$$PY" -m unittest discover -s tests -v ) || exit 1; \
	  else \
	    echo "$$dir/tests/ not present — skipping"; \
	  fi; \
	done

# The suite on HEAD as committed, in a fresh clone under a minimal
# environment (tools/test-clean.sh): catches what only this working tree
# supplies, such as an untracked fixture or an exported variable. Slower
# than `make test`: the generator compiles from scratch. KEEP=1 keeps the clone.
test-clean:
	@$(call tool,test-clean.sh)

# The site's overrides of engine files whose engine file changed since their
# review (overrides.yaml), each with the engine's diff. Bring the override
# along, then record the review: `engine/tools/override-review.py --accept
# <path>`. That clears only the review; the contract's hook check
# (`override-review.py --check`, also in `make test`) and the browser tests
# still apply (engine/templates/contract.yaml).
override-review:
	@$(PY) $(call tool,override-review.py)

# The browser tests (engine/tests/test_browser_*.py): Chromium and Firefox through
# Playwright over the built site, served as the production vhost serves it
# (tools/browser/serve.py), under the enforcing CSP. Opt-in: `make test`
# skips them without RUN_BROWSER_TESTS=1. Run `make build` first; inputs
# changed since it finished (tools/build-inputs.py) fail the run. Locked
# like `validate`, so no build or deploy rewrites _site underneath them.
# Needs the network (popups, map tiles, transformers.js). `playwright
# install` matches the browsers to the installed Playwright and is quick
# when they already are. The modules share nothing, so engine/tests/run_browser.py
# runs BROWSER_JOBS of them at once (default 3; BROWSER_JOBS=1 for one at a
# time), longest first, each one's output whole; BROWSER_TIMINGS=1 adds the
# report scripts' times.
test-browser:
	@$(WITH_LOCK) $(MAKE) --no-print-directory test-browser-locked

test-browser-locked:
	@test -x $(VENV_PY) || { echo "test-browser: .venv/ is absent — run 'make venv'." >&2; exit 1; }
	@.venv/bin/playwright install chromium firefox
	@$(VENV_PY) $(call tool,browser/fetch_axe.py)
	@$(VENV_PY) engine/tests/run_browser.py

# The deployment contract, checked in one command: both test suites plus
# the finished-artifact gate over _site/. `deploy` depends on this, so a
# published private file, a figure that failed to render, a broken feed or
# a link into /drafts/ stops the deploy instead of reaching the VPS.
#
#   make validate                      gate an existing _site
#   make validate REQUIRE_WEBP=1       also fail on missing WebP companions
#   make validate CHECK_SITE_FLAGS='--warn-only'  report everything, exit 0
validate:
	@$(WITH_LOCK) $(MAKE) --no-print-directory validate-locked

validate-locked: REQUIRE_VENV = 1
validate-locked: test
	@test -d _site || { echo "validate: _site/ does not exist — run 'make build' first" >&2; exit 1; }
	@python3 $(call tool,check-site.py) _site $(CHECK_SITE_FLAGS) $(if $(REQUIRE_WEBP),--require-webp,)
	@$(MAKE) --no-print-directory viz-provenance-check

# Checksum the CSVs behind each figure into figures/data/PROVENANCE.json, so
# a reader who downloads the data can tell it is what the chart was drawn
# from. `write` seeds a TODO source note per file and preserves whatever the
# author writes there; `check` (viz-provenance-check) verifies the recorded
# checksums still match and exits non-zero if not.
viz-provenance:
	@$(PY) $(call tool,viz-provenance.py) write

viz-provenance-check:
	@$(PY) $(call tool,viz-provenance.py) check

# Render every figure and report what a reader would notice: a mark with no
# variation in it, text that cannot be read against what sits behind it,
# labels too small at the body column, missing alt / desc. Exits 0 — a
# report, like audit-marks. Pass STRICT=1 to fail instead.
#
# This is the complement to `make test`, which checks the pipeline contract
# (colour, determinism, ids, captions). Every visualization bug found in this
# repo so far was invisible in the source and obvious in the render: a
# heatmap drawn from constant data, white labels repainted near-black, 4px
# tick text, a y-axis reading "(imes)".
audit-viz:
	@$(PY) $(call tool,audit-viz.py) $(if $(STRICT),--strict,)

# Report which content pieces are missing a monogram (mark.svg) and / or
# the epistemic figure (status: frontmatter). Exits 0 unconditionally;
# this is a coverage report, not a build gate. The optional pre-commit
# hook tools/hooks/pre-commit-marks.sh (not installed by default; MARKS.md
# § 9.3) makes its own, narrower check of newly added essays.
audit-marks:
	@$(PY) $(call tool,audit-marks.py)

# Evict archived works: delete archive/<slug>/ directories whose slug is
# recorded in archive/removed.yaml. Opt-in — NEVER run by `make build`.
# Orphan directories (not in manifest.yaml, not in removed.yaml) are
# reported, never deleted. See ARCHIVE.md - Eviction & removal.
archive-gc:
	@$(PY) $(call tool,archive.py) gc

# Submit archived URLs to the Wayback Machine and backfill the capture URL
# into each PROVENANCE.json. A slow network job — opt-in, never run by
# `make build`. Always exits 0; an entry without a capture retries next run.
archive-wayback:
	@$(PY) $(call tool,archive.py) wayback

# Print works cited in data/*.bib but not yet archived, as manifest-ready
# lines the author copies by hand. Read-only — it never edits the manifest
# (bibliography auto-seeding is rejected by design; see ARCHIVE.md).
# Offline: scans local files only, no network.
archive-suggest:
	@$(PY) $(call tool,archive.py) suggest

# Probe every archived URL for link rot, updating data/archive-state.json.
# A slow network job — opt-in, never run by `make build`. Asymmetric
# hysteresis: `rotted` needs 3 consecutive failures over >=14 days; a
# single success recovers immediately. The next build consumes the state.
archive-check:
	@$(PY) $(call tool,archive.py) check

# Dev build includes any in-progress drafts under content/drafts/essays/.
# SITE_ENV=dev is read by build/Site.hs; `make build` now forces
# SITE_ENV=production for itself, so a dev value set here (or exported in
# your shell) can no longer leak into a production compile.
#
# A dev build writes to _site-dev/ with its own store, _cache-dev/
# (build/Utils.hs outputDirFor), never to the _site/ that deploy publishes:
# a draft collection is routed outside /drafts/ in dev, and before the split
# a dev session could leave such a page for the next deploy to sign and
# publish (audit D06). The clean below only clears the dev pair.
dev:
	@$(WITH_LOCK) $(MAKE) --no-print-directory dev-locked

dev-locked: export SITE_ENV = dev
dev-locked:
	cabal run site -- clean
	cabal run site -- build
	python3 -m http.server 8000 --bind 127.0.0.1 --directory _site-dev
