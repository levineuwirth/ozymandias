.PHONY: build deploy sign download-model download-leaflet convert-images pdf-thumbs watch clean dev

# Source .env for GITHUB_TOKEN and GITHUB_REPO if it exists.
# .env format: KEY=value (one per line, no `export` prefix, no quotes needed).
-include .env
export

build:
	@date +%s > data/build-start.txt
	@./tools/convert-images.sh
	@$(MAKE) -s pdf-thumbs
	@if [ -d content/photography ]; then ./tools/download-leaflet.sh; fi
	# Photography pipeline: when content/photography/ exists, generate
	# per-photo EXIF + palette sidecars and per-image dimension sidecars
	# (the latter site-wide for CLS prevention). Gated on .venv presence,
	# matching the embed.py pattern — failures are non-fatal.
	@if [ -d .venv ]; then \
	  uv run python tools/extract-exif.py       || echo "Warning: EXIF extraction failed (build continues with frontmatter only)"; \
	  uv run python tools/extract-palette.py    || echo "Warning: palette extraction failed (build continues with frontmatter only)"; \
	  uv run python tools/extract-dimensions.py || echo "Warning: dimension extraction failed (build continues without width/height attrs)"; \
	else \
	  echo "Photography sidecars skipped: run 'uv sync' to enable EXIF + palette + dimension extraction (build continues with frontmatter only)"; \
	fi
	cabal run site -- build
	pagefind --site _site
	@if [ -d .venv ]; then \
	  uv run python tools/embed.py || echo "Warning: embedding failed — data/similar-links.json not updated (build continues)"; \
	else \
	  echo "Embedding skipped: run 'uv sync' to enable similar-links (build continues)"; \
	fi
	> IGNORE.txt
	@BUILD_END=$$(date +%s); \
	 BUILD_START=$$(cat data/build-start.txt); \
	 echo $$((BUILD_END - BUILD_START)) > data/last-build-seconds.txt

sign:
	@./tools/sign-site.sh

# Download the quantized ONNX model for client-side semantic search.
# Run once; files are gitignored. Safe to re-run (skips existing files).
download-model:
	@./tools/download-model.sh

# Vendor Leaflet + leaflet.markercluster into static/leaflet/.
# Used only by /photography/map/. Runs automatically as part of `build`
# when content/photography/ exists (skips when files already present).
# Files are gitignored; sha256-verified against tools/leaflet-checksums.sha256.
download-leaflet:
	@./tools/download-leaflet.sh

# Convert JPEG/PNG images to WebP companions (also runs automatically in build).
# Requires cwebp: pacman -S libwebp  /  apt install webp
convert-images:
	@./tools/convert-images.sh

# Generate first-page thumbnails for PDFs in static/papers/ (also runs in build).
# Requires pdftoppm: pacman -S poppler  /  apt install poppler-utils
# Thumbnails are written as static/papers/foo.thumb.png alongside each PDF.
# Skipped silently when pdftoppm is not installed or static/papers/ is empty.
pdf-thumbs:
	@if command -v pdftoppm >/dev/null 2>&1; then \
	  find static/papers -name '*.pdf' 2>/dev/null | while read pdf; do \
	    thumb="$${pdf%.pdf}.thumb"; \
	    if [ ! -f "$${thumb}.png" ] || [ "$$pdf" -nt "$${thumb}.png" ]; then \
	      echo "  pdf-thumb $$pdf"; \
	      pdftoppm -r 100 -f 1 -l 1 -png -singlefile "$$pdf" "$$thumb"; \
	    fi; \
	  done; \
	else \
	  echo "pdf-thumbs: pdftoppm not found — install poppler (skipping)"; \
	fi

deploy: clean build sign
	@test -n "$(VPS_USER)" || (echo "deploy: VPS_USER not set in .env" >&2; exit 1)
	@test -n "$(VPS_HOST)" || (echo "deploy: VPS_HOST not set in .env" >&2; exit 1)
	@test -n "$(VPS_PATH)" || (echo "deploy: VPS_PATH not set in .env" >&2; exit 1)
	@command -v notify-send >/dev/null 2>&1 && notify-send "make deploy" "Ready to rsync — waiting for SSH auth" || true
	rsync -avz --delete _site/ $(VPS_USER)@$(VPS_HOST):$(VPS_PATH)/
	git push -u origin main

watch: export SITE_ENV = dev
watch:
	cabal run site -- watch

clean:
	cabal run site -- clean

# Dev build includes any in-progress drafts under content/drafts/essays/.
# SITE_ENV=dev is read by build/Site.hs; drafts are otherwise invisible to
# every build (make build / make deploy / cabal run site -- build directly).
dev: export SITE_ENV = dev
dev:
	cabal run site -- clean
	cabal run site -- build
	python3 -m http.server 8000 --directory _site
