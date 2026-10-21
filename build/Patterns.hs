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
    , photographyPattern
    , allPhotoEntries
    , standalonePagesPattern
    , standaloneSources
    , pageCollectionPattern
      -- * Withheld (@draft: true@)
    , unpublished
      -- * Aggregated patterns
    , allContent         -- everything that backlinks should index
    , authorIndexable    -- everything that should appear on /authors/{slug}/
    , tagIndexable       -- everything that should appear on /<tag>/
    ) where

import Hakyll
import Config (SiteConfig (..), Standalone (..))
import Drafts (currentUnpublished, unpublishedPattern)
import Sections (reservedSectionDirs)

-- | Pages a production build withholds for @draft: true@, and the files
-- that go with them (build/Drafts.hs). Every per-section pattern below
-- excludes it, so listings, tags, authors, feeds, backlinks and stats
-- never see them; empty under @SITE_ENV=dev@.
unpublished :: Pattern
unpublished = unpublishedPattern currentUnpublished

published :: Pattern -> Pattern
published p = p .&&. complement unpublished

-- ---------------------------------------------------------------------------
-- Per-section
-- ---------------------------------------------------------------------------

-- | All published essays — flat files, directory-based essays, and entries
-- inside one-level collection directories.
essayPattern :: Pattern
essayPattern = published $
       "content/essays/*.md"
  .||. "content/essays/*/*.md"

-- | In-progress essay drafts. Matches the flat and directory forms under
-- @content/drafts/essays/@. Only 'Site.rules' consumes this, gated on
-- @SITE_ENV=dev@ — every other module that enumerates content (Authors,
-- Tags, Backlinks, Stats, feeds) sees only 'essayPattern', so drafts are
-- automatically invisible to listings, tags, authors, backlinks, and stats.
draftEssayPattern :: Pattern
draftEssayPattern =
       "content/drafts/essays/*.md"
  .||. "content/drafts/essays/*/index.md"

-- | All blog posts: flat posts plus entries inside collection directories.
-- Collection index pages are landing pages and compile separately.
blogPattern :: Pattern
blogPattern = published $
       "content/blog/*.md"
  .||. ("content/blog/*/*.md" .&&. complement "content/blog/*/index.md")

-- | All poetry: flat poems plus collection poems, excluding collection
-- index pages (which are landing pages, not poems).
poetryPattern :: Pattern
poetryPattern = published $
       "content/poetry/*.md"
  .||. ("content/poetry/*/*.md" .&&. complement "content/poetry/*/index.md")

-- | All fiction: flat stories plus entries inside collection directories.
-- Collection index pages are landing pages and compile separately.
fictionPattern :: Pattern
fictionPattern = published $
       "content/fiction/*.md"
  .||. ("content/fiction/*/*.md" .&&. complement "content/fiction/*/index.md")

-- | Music compositions (landing pages live at @content/music/<slug>/index.md@).
musicPattern :: Pattern
musicPattern = published "content/music/*/index.md"

-- | All photo entries — flat singles plus directory-form entries.
--
--   Phase 1 supports two shapes:
--     * flat:      @content/photography/<slug>.md@
--     * directory: @content/photography/<slug>/index.md@
--
--   The section landing page at @content/photography/index.md@ is
--   excluded; it routes via 'Site.rules' as the catalog landing
--   (analogous to @content/music/index.md@), not as a photo entry.
--
--   Phase 5 will extend this pattern with collection-photo files
--   (@content/photography/<series>/<photo>.md@) when series support
--   lands; until then directory-form @index.md@ files are treated as
--   single-photo entries (a series is just a directory with siblings).
photographyPattern :: Pattern
photographyPattern = published $
       ("content/photography/*.md" .&&. complement "content/photography/index.md")
  .||. "content/photography/*/index.md"

-- | Every photographic entry, including children of series. Distinct
--   from 'photographyPattern' (which enumerates only top-level entries
--   and series landings) for surfaces that should enumerate every
--   photograph individually:
--
--     * @/photography/by-year/<year>/@        — one frame per file
--     * @/photography/contact-sheet/@         — every frame in the roll
--     * @/photography/map.json@               — one pin per geotagged photo
--     * @/photography/feed.xml@               — one entry per shot
--     * Tag indexes                           — siblings have their own tags
--
--   The main @/photography/@ landing and the library shelf use
--   'photographyPattern' instead, so a series shows up as a single
--   aggregate card rather than once for the landing plus once per child.
allPhotoEntries :: Pattern
allPhotoEntries = published $
       photographyPattern
  .||. ("content/photography/*/*.md" .&&. complement "content/photography/*/index.md")

-- | Page collection entries live one directory below @content/@. The
-- reserved directories ("Sections") are excluded so their files retain
-- specialized compilers and routes.
pageCollectionPattern :: SiteConfig -> Pattern
pageCollectionPattern cfg = published $
    "content/*/*.md" .&&. complement reservedSectionPages
  where
    reservedSectionPages =
        foldr1 (.||.) [fromGlob ("content/" ++ d ++ "/*.md") | d <- reservedSectionDirs cfg]

-- | Top-level pages, the standalone pages in site.yaml, and generic page
-- collections.
standalonePagesPattern :: SiteConfig -> Pattern
standalonePagesPattern cfg = published $
       "content/*.md"
  .||. standaloneSources cfg
  .||. pageCollectionPattern cfg

-- | The sources of site.yaml's standalone pages, as globs: a 'fromList'
-- pattern matches only the unversioned identifier, and the backlinks pass
-- compiles each page as @version "links"@.
standaloneSources :: SiteConfig -> Pattern
standaloneSources = foldr ((.||.) . fromGlob . standaloneSource) (fromList []) . siteStandalone

-- ---------------------------------------------------------------------------
-- Aggregations
-- ---------------------------------------------------------------------------

-- | Every content file the backlinks pass should index. Includes music
-- landing pages and the standalone pages, in addition to writings: the
-- directory-form ones (@content/me/index.md@) are full essays rendered
-- with backlinks, whose outgoing links must be visible to the link graph.
--
-- Photography is deliberately excluded, but note what this pattern
-- governs: link *sources*. Photo pages DO render the backlinks block
-- now (see 'Contexts.photographyCtx'), and they receive backlinks
-- normally — a poem that references a frame surfaces on that frame,
-- because inversion keys on the target URL and does not require the
-- target to appear here. What they do not do is contribute outbound
-- links: a caption-scale entry with an empty body has no prose to
-- give the graph, so indexing 240-odd of them would cost a compile
-- pass each and return nothing. Revisit if photo bodies start
-- carrying real prose.
allContent :: SiteConfig -> Pattern
allContent cfg =
       essayPattern
  .||. blogPattern
  .||. poetryPattern
  .||. fictionPattern
  .||. musicPattern
  .||. standalonePagesPattern cfg

-- | Content shown on author index pages — essays + blog posts.
-- (Poetry and fiction have their own dedicated indexes and are not
-- aggregated by author.)
authorIndexable :: Pattern
authorIndexable = (essayPattern .||. blogPattern) .&&. hasNoVersion

-- | Content shown on tag index pages — essays + every photographic entry
--   (including sibling photos in series). Blog posts are deliberately
--   excluded: tags are a topical index for substantial writing, and no
--   blog post is ever "about" a topic the way an essay is — including
--   them would dilute tag pages. (Blog posts still get related-content
--   links via the separate keyword/embedding-based similarity pass.)
--   Photography sub-tags (@photography/landscape@, @photography/film@,
--   …) generate proper @/<sub-tag>/@ pages from this pattern; the
--   bare @photography@ top-level tag is filtered out in
--   'Tags.getExpandedTags' to avoid colliding with the section
--   landing's route at @/photography/@.
tagIndexable :: Pattern
tagIndexable =
    (essayPattern .||. allPhotoEntries)
    .&&. hasNoVersion
