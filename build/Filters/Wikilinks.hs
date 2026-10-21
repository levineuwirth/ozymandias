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
--
--   Code protection (honest scope): lines inside /fenced/ code blocks
--   are passed through untouched (see 'mapOutsideFences'), and within a
--   line, inline code spans (backtick runs, CommonMark equal-length
--   matching) are skipped — so both fenced and @`inline`@ examples can
--   show @[[…]]@ literally. Indented code blocks and code spans that
--   cross a line break are NOT recognised; a wikilink inside those is
--   still rewritten.
module Filters.Wikilinks
    ( preprocess
    , mapOutsideFences
      -- * Route-shape awareness (shared with 'Filters.Transclusion')
    , slugUrlPath
    ) where

import           Data.Char    (isAlphaNum, toLower, isSpace)
import           Data.List    (intercalate, isSuffixOf)
import qualified Utils        as U

-- | Scan the raw Markdown source for @[[…]]@ wikilinks and replace them
--   with standard Markdown link syntax. Processing is line-by-line and
--   skips fenced code blocks; a wikilink therefore cannot span a line
--   break (which was never a sensible authoring form).
--   @slugs@ are the bare slugs whose page is a directory index
--   ('Sections.directoryRouteSlugs').
preprocess :: [String] -> String -> String
preprocess slugs = mapOutsideFences (replaceWikilinks slugs)

replaceWikilinks :: [String] -> String -> String
replaceWikilinks slugs = go
  where
    go [] = []
    -- Inline code span: a backtick run opens a span closed by a run of
    -- exactly the same length (CommonMark). Its body passes through
    -- verbatim so documentation can quote @`[[…]]`@ literally. An
    -- unclosed run is literal text — and then a following @[[…]]@ is
    -- genuinely a wikilink, matching how Pandoc will read the line.
    go s@('`':_) =
        let (run, afterRun) = span (== '`') s
        in  case codeSpan (length run) afterRun of
                Just (body, after) -> run ++ body ++ run ++ go after
                Nothing            -> run ++ go afterRun
    go ('[':'[':rest) =
        case break (== ']') rest of
            (inner, ']':']':after)
                | not (null inner) ->
                    toMarkdownLink slugs inner ++ go after
            _ -> '[' : '[' : go rest
    go (c:rest) = c : go rest

    -- @codeSpan n s@: the span body and the remainder after a closing
    -- run of exactly @n@ backticks; 'Nothing' when no closer exists on
    -- this line.
    codeSpan :: Int -> String -> Maybe (String, String)
    codeSpan n = loop
      where
        loop [] = Nothing
        loop s@('`':_) =
            let (run, rest) = span (== '`') s
            in  if length run == n
                    then Just ("", rest)
                    else prepend run <$> loop rest
        loop (c:cs) = prepend [c] <$> loop cs
        prepend pre (body, after) = (pre ++ body, after)

-- ---------------------------------------------------------------------------
-- Fence-aware line mapping (shared by all source-level preprocessors)
-- ---------------------------------------------------------------------------

-- | Apply a line transformation to every line that is not part of a
--   fenced code block. Shared by the three source-level preprocessors
--   (wikilinks here, 'Filters.Transclusion', 'Filters.EmbedPdf') so
--   their directive syntax can be quoted literally inside fenced code.
--
--   Fence tracking follows CommonMark: an opener is at most three
--   spaces of indentation followed by a run of at least three backticks
--   or tildes (longer runs allowed); for backtick fences the info
--   string may not contain a backtick. The closer uses the same fence
--   character, a run at least as long as the opener, and nothing but
--   whitespace after it. An unclosed fence extends to the end of the
--   document. Fence delimiter lines themselves pass through untouched.
--
--   Honest scope: only /fenced/ code blocks are protected. Indented
--   code blocks and inline code spans are not recognised here — a
--   directive inside either is still rewritten.
mapOutsideFences :: (String -> String) -> String -> String
mapOutsideFences f = unlines . go Nothing . lines
  where
    go _ [] = []
    go Nothing (l:ls) =
        case openingFence l of
            Just fence -> l : go (Just fence) ls
            Nothing    -> f l : go Nothing ls
    go st@(Just fence) (l:ls)
        | closesFence fence l = l : go Nothing ls
        | otherwise           = l : go st ls

-- | The fence character and run length of a CommonMark fence opener,
--   or 'Nothing' when the line does not open a fence.
openingFence :: String -> Maybe (Char, Int)
openingFence l = do
    rest <- stripFenceIndent l
    case rest of
        (c:_) | c == '`' || c == '~' ->
            let run  = takeWhile (== c) rest
                n    = length run
                info = drop n rest
            in  if n >= 3 && (c == '~' || '`' `notElem` info)
                    then Just (c, n)
                    else Nothing
        _ -> Nothing

-- | True when the line closes the fence opened by @(c, n)@: the same
--   fence character, a run at least as long as the opener, and only
--   whitespace after it.
closesFence :: (Char, Int) -> String -> Bool
closesFence (c, n) l =
    case stripFenceIndent l of
        Nothing   -> False
        Just rest ->
            let run = takeWhile (== c) rest
            in  length run >= n && all isSpace (drop (length run) rest)

-- | Strip up to three leading spaces (the indentation CommonMark allows
--   on a fence line); 'Nothing' for four or more, which would be an
--   indented code block rather than a fence.
stripFenceIndent :: String -> Maybe String
stripFenceIndent l =
    let (indent, rest) = span (== ' ') l
    in  if length indent <= 3 then Just rest else Nothing

-- | Convert the inner content of @[[…]]@ to a Markdown link.
--
--   Display text is escaped via 'escMdLinkText' so that a literal @]@, @[@,
--   or backslash in the display does not break the surrounding Markdown
--   link syntax. The URL itself is produced by 'slugify' and therefore only
--   ever contains @[a-z0-9-]@, so no URL-side encoding is needed — adding
--   one would be defense against a character set we can't produce.
toMarkdownLink :: [String] -> String -> String
toMarkdownLink slugs inner =
    let (title, display) = splitOnPipe inner
        url              = "/" ++ slugUrlPath slugs (slugify title)
    in "[" ++ escMdLinkText display ++ "](" ++ url ++ ")"

-- ---------------------------------------------------------------------------
-- Route shape
-- ---------------------------------------------------------------------------

-- | Root-relative path (no leading slash) for a bare slug: the directory
--   form for one of @slugs@, @\<slug\>.html@ otherwise.
--
--   Both source-level preprocessors resolve a bare slug to a URL without
--   access to Hakyll's route table — the substitution happens on the raw
--   Markdown string, long before any route exists — so the slugs whose
--   page is a directory index come from the taxonomy
--   ('Sections.directoryRouteSlugs'). Authors can also bypass the table
--   by writing the directory form explicitly — @{{build\/}}@ — which
--   'Filters.Transclusion' maps to itself.
--
--   A slug that already spells out a directory index —
--   @essays\/deep-dive\/index.html@, which is the shape of the Hakyll
--   route for every @content\/essays\/\<slug\>\/index.md@ — collapses to
--   the directory form (audit C03). Serving the same page under two
--   spellings splits every URL-keyed thing the site keeps: the canonical
--   link, the sitemap, the backlink join, and the per-page localStorage
--   keys in @annotations.js@ and @collapse.js@. Only one of them can be
--   the address, and everywhere else on the site it is @\/essays\/x\/@.
slugUrlPath :: [String] -> String -> String
slugUrlPath slugs slug
    | Just dir <- stripDirIndex slug  = dir ++ "/"
    | slug `elem` slugs               = slug ++ "/"
    | otherwise                       = slug ++ ".html"
  where
    -- "a/b/index.html" → Just "a/b"; "index.html" → Just ""; else Nothing.
    stripDirIndex s
        | suffix `isSuffixOf` s = Just (take (length s - length suffix) s)
        | s == "index.html"     = Just ""
        | otherwise             = Nothing
      where suffix = "/index.html" :: String

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

