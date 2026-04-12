{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Typographic refinements applied to the Pandoc AST.
--
--   Currently: expands common Latin abbreviations to @<abbr>@ elements
--   (e.g. → exempli gratia, i.e. → id est, etc.).  Pandoc's @smart@
--   reader extension already handles em-dashes, en-dashes, ellipses,
--   and curly quotes, so those are not repeated here.
module Filters.Typography (apply) where

import           Data.Text    (Text)
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk (walk)
import           Utils            (escapeHtmlText)

-- | Apply all typographic transformations to the document.
apply :: Pandoc -> Pandoc
apply = walk expandAbbrev

-- ---------------------------------------------------------------------------
-- Abbreviation expansion
-- ---------------------------------------------------------------------------

-- | Abbreviations that should be wrapped in @<abbr title="…">@.
--   Each entry is (verbatim text as it appears in the Pandoc Str token,
--   long-form title for the tooltip).
abbrevMap :: [(Text, Text)]
abbrevMap =
    [ ("e.g.",    "exempli gratia")
    , ("i.e.",    "id est")
    , ("cf.",     "confer")
    , ("viz.",    "videlicet")
    , ("ibid.",   "ibidem")
    , ("op.",     "opere")      -- usually followed by "cit." in a separate token
    , ("NB",      "nota bene")
    , ("NB:",     "nota bene")
    ]

-- | If the Str token exactly matches a known abbreviation, replace it with
--   a @RawInline "html"@ @<abbr>@ element; otherwise leave it unchanged.
--
--   Both the @title@ attribute and the visible body pass through
--   'escapeHtmlText' for consistency with every other raw-HTML emitter
--   in the filter pipeline. The abbreviations themselves are ASCII-safe
--   so this is defense-in-depth rather than a live hazard.
expandAbbrev :: Inline -> Inline
expandAbbrev (Str t) =
    case lookup t abbrevMap of
        Just title ->
            RawInline "html" $
                "<abbr title=\"" <> escapeHtmlText title <> "\">"
                    <> escapeHtmlText t <> "</abbr>"
        Nothing -> Str t
expandAbbrev x = x
