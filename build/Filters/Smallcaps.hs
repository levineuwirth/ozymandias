{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Automatic small-caps wrapping for abbreviations in body text.
--
--   Any @Str@ token that consists entirely of uppercase letters (and
--   hyphens) and is at least three characters long is wrapped in
--   @<abbr class="smallcaps">@.  This catches CSS, HTML, API, NASA, etc.
--   while avoiding single-character tokens (\"I\", \"A\") and mixed-case
--   words.
--
--   Authors can also use Pandoc span syntax for explicit control:
--   @[TEXT]{.smallcaps}@ — Pandoc already emits the @smallcaps@ class on
--   those spans, and typography.css styles @.smallcaps@ directly, so no
--   extra filter logic is needed for that case.
--
--   The filter is /not/ applied inside headings (where Fira Sans uppercase
--   text looks intentional) or inside @Code@/@RawInline@ inlines.
module Filters.Smallcaps (apply) where

import           Data.Char             (isUpper, isAlpha)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk      (walk)
import qualified Utils                 as U

-- | Apply smallcaps detection to paragraph-level content.
--   Skips heading blocks to avoid false positives.
apply :: Pandoc -> Pandoc
apply (Pandoc meta blocks) = Pandoc meta (map applyBlock blocks)

applyBlock :: Block -> Block
applyBlock b@(Header {}) = b          -- leave headings untouched
applyBlock b              = walk wrapCaps b

-- | Wrap an all-caps Str token in an abbr element, preserving any trailing
--   punctuation (comma, period, colon, semicolon, closing paren/bracket)
--   outside the abbr element.
wrapCaps :: Inline -> Inline
wrapCaps (Str t) =
    let (core, trail) = stripTrailingPunct t
    in if isAbbreviation core
       then RawInline "html" $
                "<abbr class=\"smallcaps\">" <> escHtml core <> "</abbr>"
                <> trail
       else Str t
wrapCaps x = x

-- | Split trailing punctuation from the token body.
stripTrailingPunct :: Text -> (Text, Text)
stripTrailingPunct t =
    let isPunct c = c `elem` (",.:;!?)]\'" :: String)
        trail = T.takeWhileEnd isPunct t
        core  = T.dropEnd (T.length trail) t
    in (core, trail)

-- | True if the token looks like an abbreviation: all uppercase (plus
--   hyphens), at least 3 characters, contains at least one alpha character.
isAbbreviation :: Text -> Bool
isAbbreviation t =
    T.length t >= 3
    && T.all (\c -> isUpper c || c == '-') t
    && T.any isAlpha t

escHtml :: Text -> Text
escHtml = U.escapeHtmlText
