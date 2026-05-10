{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Shared Pandoc inline utilities.
--
-- 'stringify' collapses a list of 'Inline' nodes to plain 'Text', for use
-- as alt text, plain-text excerpts for word-counting, comparison keys, etc.
-- Keeping a single implementation prevents 'Compilers', 'Filters.Images',
-- and any future caller from drifting on which inline node types they
-- handle.
--
-- Mirrors the shape of @Text.Pandoc.Shared.stringify@ but is local so we
-- don't need a @Text.Pandoc.Shared@ import from every Pandoc-handling
-- module. It is a lossy conversion: structural elements (notes, raw HTML,
-- images-without-alt) yield empty strings rather than placeholder text.
module Inlines (stringify) where

import           Data.Text              (Text)
import qualified Data.Text              as T
import           Text.Pandoc.Definition

-- | Render a list of 'Inline' nodes to a plain 'Text' value.
--
-- The fallthrough cases are deliberately silent (@""@):
--
--   * 'Note' contents would balloon the result with footnote bodies.
--   * 'RawInline' is format-specific (e.g. raw HTML) and would leak markup
--     into contexts that expect plain text.
--   * 'Image' alt-only fallback would otherwise render the alt text twice
--     when the image is itself nested inside a link.
--
-- All recursing inline-container nodes (Emph, Strong, Link, Span, …) walk
-- their children, so formatting and inline links contribute their
-- visible text exactly once.
stringify :: [Inline] -> Text
stringify = T.concat . map go
  where
    go (Str t)            = t
    go Space              = " "
    go SoftBreak          = " "
    go LineBreak          = " "
    go (Emph ils)         = stringify ils
    go (Strong ils)       = stringify ils
    go (Strikeout ils)    = stringify ils
    go (Superscript ils)  = stringify ils
    go (Subscript ils)    = stringify ils
    go (SmallCaps ils)    = stringify ils
    go (Underline ils)    = stringify ils
    go (Quoted _ ils)     = stringify ils
    go (Cite _ ils)       = stringify ils
    go (Code _ t)         = t
    go (Math _ t)         = t
    go (Link _ ils _)     = stringify ils
    go (Image _ ils _)    = stringify ils
    go (Span _ ils)       = stringify ils
    go (RawInline _ _)    = ""
    go (Note _)           = ""
