{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Wikilink syntax preprocessor.
--
--   Applied to the raw Markdown source string /before/ Pandoc parsing.
--   Transforms:
--
--   * @[[Page Title]]@           → @[Page Title](/page-title)@
--   * @[[Page Title|Display]]@   → @[Display](/page-title)@
--
--   The URL slug is derived from the page title: lowercased, spaces
--   replaced with hyphens, non-alphanumeric characters stripped, and
--   a @.html@ suffix appended so the link resolves identically under
--   the dev server, file:// previews, and nginx in production.
module Filters.Wikilinks (preprocess) where

import           Data.Char    (isAlphaNum, toLower, isSpace)
import           Data.List    (intercalate)
import qualified Utils        as U

-- | Scan the raw Markdown source for @[[…]]@ wikilinks and replace them
--   with standard Markdown link syntax.
preprocess :: String -> String
preprocess [] = []
preprocess ('[':'[':rest) =
    case break (== ']') rest of
        (inner, ']':']':after)
            | not (null inner) ->
                toMarkdownLink inner ++ preprocess after
        _ -> '[' : '[' : preprocess rest
preprocess (c:rest) = c : preprocess rest

-- | Convert the inner content of @[[…]]@ to a Markdown link.
--
--   Display text is escaped via 'escMdLinkText' so that a literal @]@, @[@,
--   or backslash in the display does not break the surrounding Markdown
--   link syntax. The URL itself is produced by 'slugify' and therefore only
--   ever contains @[a-z0-9-]@, so no URL-side encoding is needed — adding
--   one would be defense against a character set we can't produce.
toMarkdownLink :: String -> String
toMarkdownLink inner =
    let (title, display) = splitOnPipe inner
        url              = "/" ++ slugify title ++ ".html"
    in "[" ++ escMdLinkText display ++ "](" ++ url ++ ")"

-- | Escape the minimum set of characters that would prematurely terminate
--   a Markdown link's display-text segment: backslash (escape char), @[@,
--   and @]@. Backslash MUST be escaped first so the escapes we introduce
--   for @[@ and @]@ are not themselves re-escaped.
--
--   Deliberately NOT escaped: @_@, @*@, @\`@, @<@. Those are inline
--   formatting markers in Markdown and escaping them would strip the
--   author's ability to put emphasis, code, or inline HTML in a wikilink's
--   display text.
escMdLinkText :: String -> String
escMdLinkText = concatMap esc
  where
    esc '\\' = "\\\\"
    esc '['  = "\\["
    esc ']'  = "\\]"
    esc c    = [c]

-- | Split on the first @|@; if none, display = title.
splitOnPipe :: String -> (String, String)
splitOnPipe s =
    case break (== '|') s of
        (title, '|':display) -> (U.trim title, U.trim display)
        _                    -> (U.trim s,     U.trim s)

-- | Produce a URL slug: lowercase, words joined by hyphens,
--   non-alphanumeric characters removed.
--
--   Trailing punctuation is dropped rather than preserved as a dangling
--   hyphen — @slugify "end." == "end"@, not @"end-"@. This is intentional:
--   author-authored wikilinks tend to end sentences with a period and the
--   desired URL is almost always the terminal-punctuation-free form.
slugify :: String -> String
slugify = intercalate "-" . words . map toLowerAlnum
  where
    toLowerAlnum c
        | isAlphaNum c = toLower c
        | isSpace c    = ' '
        | c == '-'     = '-'
        | otherwise    = ' '   -- replace punctuation with a space so words
                                -- split correctly and double-hyphens are
                                -- collapsed by 'words'

