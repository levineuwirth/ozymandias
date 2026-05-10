{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Shared utilities used across the build system.
--
-- The HTML escapers (one for 'String', one for 'Text') live here so that
-- every filter, context, and renderer goes through the same definition.
-- The expansion order matters: @&@ MUST be replaced first, otherwise the
-- @&amp;@ injected by other rules gets re-escaped to @&amp;amp;@. The
-- pure-character-by-character implementation used here avoids that hazard
-- entirely (each character is mapped exactly once).
module Utils
    ( wordCount
    , readingTime
    , escapeHtml
    , escapeHtmlText
    , trim
    , authorSlugify
    , authorNameOf
    , normaliseUrl
    , percentDecode
    ) where

import           Data.Char                  (isAlphaNum, isSpace, toLower)
import           Data.Maybe                 (fromMaybe)
import qualified Data.ByteString            as BS
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Encoding.Error   as TE

-- | Count the number of words in a string (split on whitespace).
wordCount :: String -> Int
wordCount = length . words

-- | Estimate reading time in minutes (assumes 200 words per minute).
-- Minimum is 1 minute.
readingTime :: String -> Int
readingTime s = max 1 (wordCount s `div` 200)

-- | Escape HTML special characters: @&@, @<@, @>@, @\"@, @\'@.
--
-- Safe for use in attribute values and text content. The order of the
-- @case@ branches is irrelevant — each input character maps to exactly
-- one output sequence.
escapeHtml :: String -> String
escapeHtml = concatMap escChar
  where
    escChar '&'  = "&amp;"
    escChar '<'  = "&lt;"
    escChar '>'  = "&gt;"
    escChar '"'  = "&quot;"
    escChar '\'' = "&#39;"
    escChar c    = [c]

-- | 'Text' counterpart of 'escapeHtml'.
escapeHtmlText :: T.Text -> T.Text
escapeHtmlText = T.concatMap escChar
  where
    escChar '&'  = "&amp;"
    escChar '<'  = "&lt;"
    escChar '>'  = "&gt;"
    escChar '"'  = "&quot;"
    escChar '\'' = "&#39;"
    escChar c    = T.singleton c

-- | Strip leading and trailing whitespace.
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Lowercase a string, drop everything that isn't alphanumeric or
-- space, then replace runs of spaces with single hyphens.
--
-- Used for author URL slugs (e.g. @"Jane Doe" → "jane-doe"@).
-- Centralised here so 'Authors' and 'Contexts' cannot drift on Unicode
-- edge cases.
authorSlugify :: String -> String
authorSlugify = map (\c -> if c == ' ' then '-' else c)
              . filter (\c -> isAlphaNum c || c == ' ')
              . map toLower

-- | Extract the author name from a "Name | url" frontmatter entry.
-- The URL portion is dropped (it's no longer used by the author system,
-- which routes everything through @/authors/{slug}/@).
authorNameOf :: String -> String
authorNameOf s = trim (takeWhile (/= '|') s)

-- ---------------------------------------------------------------------------
-- URL normalisation
-- ---------------------------------------------------------------------------

-- | Normalise an internal URL as a stable map key:
--
--     * strip everything from a @?@ or @#@ onward,
--     * ensure a leading @/@,
--     * strip a trailing @.html@ extension,
--     * percent-decode the path so @/essays/caf%C3%A9@ and
--       @/essays/café@ collide on the same key.
--
-- This is the canonical normaliser used by 'Backlinks' (writing keys into
-- @data/backlinks.json@) and 'Stats' (looking up those keys for orphan
-- counting and most-linked detection). Keeping a single implementation here
-- prevents the two surfaces from drifting on percent-encoding or fragment
-- handling.
--
-- 'SimilarLinks' uses a slightly different normalisation that preserves
-- trailing slashes on directory-style URLs (because @embed.py@ produces
-- keys like @/blog/@ rather than @/blog/index@); see its own
-- @normaliseUrl@ for that variant.
normaliseUrl :: String -> String
normaliseUrl url =
    let t  = T.pack url
        t1 = fst (T.breakOn "?" (fst (T.breakOn "#" t)))
        t2 = if T.isPrefixOf "/" t1 then t1 else "/" `T.append` t1
        t3 = fromMaybe t2 (T.stripSuffix ".html" t2)
    in  percentDecode (T.unpack t3)

-- | Decode percent-escapes (@%XX@) into raw bytes, then re-interpret the
-- resulting bytestring as UTF-8. Invalid escapes are passed through
-- verbatim so this is safe to call on already-decoded input.
percentDecode :: String -> String
percentDecode = T.unpack . TE.decodeUtf8With TE.lenientDecode . BS.pack . go
  where
    go []                 = []
    go ('%':a:b:rest)
        | Just hi <- hexDigit a
        , Just lo <- hexDigit b
        = fromIntegral (hi * 16 + lo) : go rest
    go (c:rest)           = fromIntegral (fromEnum c) : go rest

    hexDigit c
        | c >= '0' && c <= '9' = Just (fromEnum c - fromEnum '0')
        | c >= 'a' && c <= 'f' = Just (fromEnum c - fromEnum 'a' + 10)
        | c >= 'A' && c <= 'F' = Just (fromEnum c - fromEnum 'A' + 10)
        | otherwise            = Nothing
