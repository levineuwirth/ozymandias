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
import qualified Data.Text    as T
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk (walk)
import           Utils            (escapeHtmlText)

-- | Apply all typographic transformations to the document.
apply :: Pandoc -> Pandoc
apply = walk (concatMap expandAbbrev)

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

-- | Wrap a known abbreviation in a @RawInline "html"@ @<abbr>@ element.
--
--   Pandoc's @smart@ extension binds an abbreviation it recognises to the
--   following word with a no-break space, so "e.g. this" arrives as the
--   single token @Str "e.g.\\160this"@. Matching whole tokens therefore
--   caught only an abbreviation that ended a source line, and most of the
--   site's went unmarked. A token is now split: any opening bracket, the
--   abbreviation, then the rest, no-break space included.
--
--   Both the @title@ attribute and the visible body pass through
--   'escapeHtmlText' for consistency with every other raw-HTML emitter
--   in the filter pipeline. The abbreviations themselves are ASCII-safe
--   so this is defense-in-depth rather than a live hazard.
expandAbbrev :: Inline -> [Inline]
expandAbbrev (Str t)
    | (opening, rest) <- T.span (`elem` ("([" :: String)) t
    , ((abbrev, title, after) : _) <-
        [ (a, ti, r) | (a, ti) <- abbrevMap, Just r <- [splitAbbrev a rest] ]
    = [ Str opening | not (T.null opening) ]
      ++ [ RawInline "html" $
             "<abbr title=\"" <> escapeHtmlText title <> "\">"
                 <> escapeHtmlText abbrev <> "</abbr>" ]
      ++ [ Str after | not (T.null after) ]
  where
    -- The whole token, or the abbreviation followed by a no-break space or
    -- closing punctuation ("e.g.," and "(i.e.)" are both common).
    splitAbbrev a s
        | s == a    = Just ""
        | otherwise = case T.stripPrefix a s of
            Just r | Just (c, _) <- T.uncons r
                   , c == '\x00A0' || c `elem` (",;:)]" :: String) -> Just r
            _ -> Nothing
expandAbbrev x = [x]
