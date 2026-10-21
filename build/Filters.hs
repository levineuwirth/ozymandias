{-# LANGUAGE GHC2021 #-}
-- | Re-exports all Pandoc AST filter modules and provides a single
--   @applyAll@ combinator that chains them in the correct order.
module Filters
    ( applyAll
    , preprocessSource
    ) where

import           Text.Pandoc.Definition (Pandoc)
import           Config (SiteConfig)
import           Sections (directoryRouteSlugs)

import qualified Filters.Sidenotes  as Sidenotes
import qualified Filters.Typography as Typography
import qualified Filters.Links      as Links
import qualified Filters.SourceRefs as SourceRefs
import qualified Filters.Smallcaps  as Smallcaps
import qualified Filters.Archive    as Archive
import qualified Filters.CodeRefs   as CodeRefs
import qualified Filters.Dropcaps   as Dropcaps
import qualified Filters.Math       as Math
import qualified Filters.Wikilinks     as Wikilinks
import qualified Filters.Transclusion  as Transclusion
import qualified Filters.EmbedPdf      as EmbedPdf
import qualified Filters.Code       as Code
import qualified Filters.Images     as Images
import qualified Filters.Aftermatter as Aftermatter
import qualified Filters.FigureRefs as FigureRefs

-- | Apply all AST-level filters in pipeline order.
--   Run on the Pandoc document after reading, before writing.
--
--   'Filters.Images.apply' is the only IO-performing filter (it probes the
--   filesystem for @.webp@ companions before deciding whether to emit
--   @<picture>@). It runs first — i.e. innermost in the composition — and
--   every downstream filter stays pure. @srcDir@ is the directory of the
--   source Markdown file, passed through to Images for relative-path
--   resolution of co-located assets.
--   @numberFigures@ opts the page in to 'Filters.FigureRefs' (frontmatter
--   @figure-numbering: true@, read by "Compilers"). It runs last, and
--   outermost: it rewrites the @\<figure\>@ HTML that Images and Viz emit,
--   so both must have produced their markup before it looks for any.
applyAll :: SiteConfig -> Bool -> FilePath -> Pandoc -> IO Pandoc
applyAll cfg numberFigures srcDir doc = do
    imagesDone     <- Images.apply     srcDir doc
    sourceRefsDone <- SourceRefs.apply cfg imagesDone
    pure
        . (if numberFigures then FigureRefs.apply else id)
        . Aftermatter.apply
        . Sidenotes.apply
        . Typography.apply
        . Links.apply cfg
        . CodeRefs.apply
        . Archive.apply
        . Smallcaps.apply
        . Dropcaps.apply
        . Math.apply
        . Code.apply
        $ sourceRefsDone

-- | Apply source-level preprocessors to the raw Markdown string.
--   Order matters: EmbedPdf must run before Transclusion, because the
--   transclusion parser would otherwise treat {{pdf:...}} as a broken slug.
preprocessSource :: SiteConfig -> String -> String
preprocessSource cfg = Transclusion.preprocess slugs . EmbedPdf.preprocess . Wikilinks.preprocess slugs
  where slugs = directoryRouteSlugs cfg
