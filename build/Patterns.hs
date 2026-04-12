{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Canonical content-pattern definitions, shared across modules.
--
-- Several modules need to enumerate "all author-written content" or
-- "all essays". Historically each module hard-coded its own slightly
-- different list, which produced silent omissions (e.g. directory-form
-- essays not appearing on author pages). This module is the single source
-- of truth — every place that needs a content pattern should import from
-- here, not write its own.
module Patterns
    ( -- * Per-section patterns
      essayPattern
    , draftEssayPattern
    , blogPattern
    , poetryPattern
    , fictionPattern
    , musicPattern
    , standalonePagesPattern
      -- * Aggregated patterns
    , allWritings        -- essays + blog + poetry + fiction
    , allContent         -- everything that backlinks should index
    , authorIndexable    -- everything that should appear on /authors/{slug}/
    , tagIndexable       -- everything that should appear on /<tag>/
    ) where

import Hakyll

-- ---------------------------------------------------------------------------
-- Per-section
-- ---------------------------------------------------------------------------

-- | All published essays — flat files and directory-based (with co-located
-- assets). Drafts under @content/drafts/essays/**@ are intentionally NOT
-- included; 'Site.rules' unions them in conditionally when @SITE_ENV=dev@.
essayPattern :: Pattern
essayPattern = "content/essays/*.md" .||. "content/essays/*/index.md"

-- | In-progress essay drafts. Matches the flat and directory forms under
-- @content/drafts/essays/@. Only 'Site.rules' consumes this, gated on
-- @SITE_ENV=dev@ — every other module that enumerates content (Authors,
-- Tags, Backlinks, Stats, feeds) sees only 'essayPattern', so drafts are
-- automatically invisible to listings, tags, authors, backlinks, and stats.
draftEssayPattern :: Pattern
draftEssayPattern =
       "content/drafts/essays/*.md"
  .||. "content/drafts/essays/*/index.md"

-- | All blog posts.  Currently flat-only; co-located blog assets would
-- require a directory variant analogous to 'essayPattern'.
blogPattern :: Pattern
blogPattern = "content/blog/*.md"

-- | All poetry: flat poems plus collection poems, excluding collection
-- index pages (which are landing pages, not poems).
poetryPattern :: Pattern
poetryPattern =
       "content/poetry/*.md"
  .||. ("content/poetry/*/*.md" .&&. complement "content/poetry/*/index.md")

-- | All fiction. Currently flat-only.
fictionPattern :: Pattern
fictionPattern = "content/fiction/*.md"

-- | Music compositions (landing pages live at @content/music/<slug>/index.md@).
musicPattern :: Pattern
musicPattern = "content/music/*/index.md"

-- | Top-level standalone pages (about, colophon, current, gpg, …).
standalonePagesPattern :: Pattern
standalonePagesPattern = "content/*.md"

-- ---------------------------------------------------------------------------
-- Aggregations
-- ---------------------------------------------------------------------------

-- | All long-form authored writings.
allWritings :: Pattern
allWritings = essayPattern .||. blogPattern .||. poetryPattern .||. fictionPattern

-- | Every content file the backlinks pass should index. Includes music
-- landing pages and top-level standalone pages, in addition to writings.
allContent :: Pattern
allContent =
       essayPattern
  .||. blogPattern
  .||. poetryPattern
  .||. fictionPattern
  .||. musicPattern
  .||. standalonePagesPattern

-- | Content shown on author index pages — essays + blog posts.
-- (Poetry and fiction have their own dedicated indexes and are not
-- aggregated by author.)
authorIndexable :: Pattern
authorIndexable = (essayPattern .||. blogPattern) .&&. hasNoVersion

-- | Content shown on tag index pages — essays + blog posts.
tagIndexable :: Pattern
tagIndexable = (essayPattern .||. blogPattern) .&&. hasNoVersion
