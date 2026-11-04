{-# LANGUAGE GHC2021 #-}
-- | Re-exports all Pandoc AST filter modules and provides a single
--   @applyAll@ combinator that chains them in the correct order.
module Filters
    ( applyAll
    , preprocessSource
    ) where

import           Text.Pandoc.Definition (Pandoc)
import           Config (SiteConfig, switchOn)
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
--   @initials@ are the page's sets of drawn initials, in order
--   (frontmatter @initials:@, resolved by 'Filters.Dropcaps.initialsDir'),
--   and the stage its status draws them at.
--   'Filters.Dropcaps' reads it, so it is IO too; it runs after the rest,
--   on the paragraphs as they will be written.
--   A switch's filter runs only with the switch on.
applyAll :: SiteConfig -> Bool -> FilePath -> Dropcaps.Initials -> Pandoc -> IO Pandoc
applyAll cfg numberFigures srcDir initials doc = do
    imagesDone     <- Images.apply     srcDir doc
    sourceRefsDone <- if switchOn cfg "source-refs" then SourceRefs.apply cfg imagesDone
                                                     else pure (SourceRefs.plain imagesDone)
    Dropcaps.apply initials
        . (if numberFigures then FigureRefs.apply else id)
        . Aftermatter.apply (switchOn cfg "new")
        . Sidenotes.apply
        . Typography.apply
        . Links.apply cfg
        . on "code-refs" CodeRefs.apply
        . Archive.apply
        . Smallcaps.apply
        . on "math" Math.apply
        . on "code" Code.apply
        $ sourceRefsDone
  where
    on name filter' = if switchOn cfg name then filter' else id

-- | Apply source-level preprocessors to the raw Markdown string.
--   Order matters: EmbedPdf must run before Transclusion, because the
--   transclusion parser would otherwise treat {{pdf:...}} as a broken slug.
preprocessSource :: SiteConfig -> String -> String
preprocessSource cfg = Transclusion.preprocess (switchOn cfg "transclusion") slugs . EmbedPdf.preprocess (switchOn cfg "pdf-viewer")
                      . Wikilinks.preprocess slugs
  where slugs = directoryRouteSlugs cfg
