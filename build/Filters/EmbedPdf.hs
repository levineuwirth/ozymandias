{-# LANGUAGE GHC2021 #-}
-- | Source-level preprocessor for inline PDF embeds.
--
--   Rewrites block-level @{{pdf:...}}@ directives to raw HTML that renders the
--   named file inside a vendored PDF.js viewer iframe.
--
--   Syntax (must be the sole content of a line after trimming):
--
-- > {{pdf:/papers/foo.pdf}}          — embed from page 1
-- > {{pdf:/papers/foo.pdf#5}}        — start at page 5  (bare integer)
-- > {{pdf:/papers/foo.pdf#page=5}}   — start at page 5  (explicit form)
--
--   The file path must be root-relative (begins with @/@).
--   PDF.js is expected to be vendored at @/pdfjs/web/viewer.html@.
module Filters.EmbedPdf (preprocess) where

import Data.Char (isDigit)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Utils as U

-- | Apply PDF-embed substitution to the raw Markdown source string.
preprocess :: String -> String
preprocess = unlines . map processLine . lines

processLine :: String -> String
processLine line =
    case parseDirective (U.trim line) of
        Nothing                    -> line
        Just (filePath, pageHash)  -> renderEmbed filePath pageHash

-- | Parse a @{{pdf:/path/to/file.pdf}}@ or @{{pdf:/path.pdf#N}}@ directive.
--   Returns @(filePath, pageHash)@ where @pageHash@ is either @""@ or @"#page=N"@.
parseDirective :: String -> Maybe (String, String)
parseDirective s
    | not ("{{pdf:" `isPrefixOf` s) = Nothing
    | not ("}}"     `isSuffixOf` s) = Nothing
    | otherwise =
        let inner        = take (length s - 2) (drop 6 s)  -- strip "{{pdf:" and "}}"
            (path, frag) = break (== '#') inner
        in  if null path
                then Nothing
                else Just (path, parsePageHash frag)

-- | Convert the fragment part of the directive (e.g. @#5@ or @#page=5@) to a
--   PDF.js-compatible @#page=N@ hash, or @""@ if absent/invalid.
parsePageHash :: String -> String
parsePageHash ('#' : rest)
    | "page=" `isPrefixOf` rest =
        let n = takeWhile isDigit (drop 5 rest)
        in  if null n then "" else "#page=" ++ n
    | all isDigit rest && not (null rest) = "#page=" ++ rest
parsePageHash _ = ""

-- | Render the HTML for a PDF embed.
renderEmbed :: String -> String -> String
renderEmbed filePath pageHash =
    let viewerUrl = "/pdfjs/web/viewer.html?file=" ++ encodeQueryValue filePath ++ pageHash
    in  "<div class=\"pdf-embed-wrapper\">"
     ++ "<iframe class=\"pdf-embed\""
     ++ " src=\"" ++ viewerUrl ++ "\""
     ++ " title=\"PDF document\""
     ++ " loading=\"lazy\""
     ++ " allowfullscreen></iframe>"
     ++ "</div>"

-- | Percent-encode characters that would break a query-string value.
--   Slashes are left unencoded so root-relative paths remain readable and
--   work correctly with PDF.js's internal fetch.  @#@ is encoded for
--   defense-in-depth even though the directive parser already splits on it
--   before this function is called.
encodeQueryValue :: String -> String
encodeQueryValue = concatMap enc
  where
    enc ' ' = "%20"
    enc '&' = "%26"
    enc '?' = "%3F"
    enc '+' = "%2B"
    enc '"' = "%22"
    enc '#' = "%23"
    enc c   = [c]

