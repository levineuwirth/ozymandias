{-# LANGUAGE GHC2021 #-}
-- | Re-exports all Pandoc AST filter modules and provides a single
--   @applyAll@ combinator that chains them in the correct order.
module Filters
    ( applyAll
    , preprocessSource
    ) where

import           Text.Pandoc.Definition (Pandoc)

import qualified Filters.Sidenotes  as Sidenotes
import qualified Filters.Typography as Typography
import qualified Filters.Links      as Links
import qualified Filters.Smallcaps  as Smallcaps
import qualified Filters.Dropcaps   as Dropcaps
import qualified Filters.Math       as Math
import qualified Filters.Wikilinks     as Wikilinks
import qualified Filters.Transclusion  as Transclusion
import qualified Filters.EmbedPdf      as EmbedPdf
import qualified Filters.Code       as Code
import qualified Filters.Images     as Images

-- | Apply all AST-level filters in pipeline order.
--   Run on the Pandoc document after reading, before writing.
--
--   'Filters.Images.apply' is the only IO-performing filter (it probes the
--   filesystem for @.webp@ companions before deciding whether to emit
--   @<picture>@). It runs first — i.e. innermost in the composition — and
--   every downstream filter stays pure. @srcDir@ is the directory of the
--   source Markdown file, passed through to Images for relative-path
--   resolution of co-located assets.
applyAll :: FilePath -> Pandoc -> IO Pandoc
applyAll srcDir doc = do
    imagesDone <- Images.apply srcDir doc
    pure
        . Sidenotes.apply
        . Typography.apply
        . Links.apply
        . Smallcaps.apply
        . Dropcaps.apply
        . Math.apply
        . Code.apply
        $ imagesDone

-- | Apply source-level preprocessors to the raw Markdown string.
--   Order matters: EmbedPdf must run before Transclusion, because the
--   transclusion parser would otherwise treat {{pdf:...}} as a broken slug.
preprocessSource :: String -> String
preprocessSource = Transclusion.preprocess . EmbedPdf.preprocess . Wikilinks.preprocess
