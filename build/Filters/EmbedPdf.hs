{-# LANGUAGE GHC2021 #-}
-- | Source-level preprocessor for inline PDF embeds.
--
--   Rewrites block-level @{{pdf:...}}@ directives to raw HTML that renders the
--   named file inside a vendored PDF.js viewer iframe, with the pdf-viewer
--   switch on; without it, to a link to the file, which "Filters.Links"
--   then marks as a PDF's.
--
--   Syntax (must be the sole content of a line after trimming):
--
-- > {{pdf:/papers/foo.pdf}}          — embed from page 1
-- > {{pdf:/papers/foo.pdf#5}}        — start at page 5  (bare integer)
-- > {{pdf:/papers/foo.pdf#page=5}}   — start at page 5  (explicit form)
--
--   The file path must be root-relative (begins with @/@).
--   PDF.js is expected to be vendored at @/pdfjs/web/viewer.html@.
--
--   Code protection (honest scope): lines inside /fenced/ code blocks
--   are passed through untouched ('Filters.Wikilinks.mapOutsideFences'),
--   so fenced examples can show @{{pdf:…}}@ literally. Indented code
--   blocks and inline code spans are NOT recognised — a full-line
--   directive inside either is still rewritten.
module Filters.EmbedPdf (preprocess) where

import Data.Char (isDigit)
import Data.List (isPrefixOf, isSuffixOf)
import System.FilePath (takeFileName)
import Filters.Wikilinks (mapOutsideFences)
import qualified Utils as U

-- | Apply PDF-embed substitution to the raw Markdown source string,
--   skipping lines inside fenced code blocks. @viewer@: the pdf-viewer
--   switch.
preprocess :: Bool -> String -> String
preprocess viewer = mapOutsideFences processLine
  where
    processLine line =
        case parseDirective (U.trim line) of
            Nothing                    -> line
            Just (filePath, pageHash)
                | viewer               -> renderEmbed filePath pageHash
                | otherwise            -> renderLink filePath pageHash

-- | Parse a @{{pdf:/path/to/file.pdf}}@ or @{{pdf:/path.pdf#N}}@ directive.
--   Returns @(filePath, pageHash)@ where @pageHash@ is either @""@ or @"#page=N"@.
parseDirective :: String -> Maybe (String, String)
parseDirective s
    | not ("{{pdf:" `isPrefixOf` s) = Nothing
    | not ("}}"     `isSuffixOf` s) = Nothing
    | otherwise =
        -- Strip "{{pdf:", then "}}". The second count is of what the first
        -- left: taking (length s - 2) of it kept the closing braces, so
        -- every embed's path or page number ended in "}}" and broke.
        let body         = drop 6 s
            inner        = take (length body - 2) body
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

-- | A Markdown link to the file, named by its file name, for a site without
--   the viewer: the page a reader is sent to is the PDF's own.
renderLink :: String -> String -> String
renderLink filePath pageHash =
    "[" ++ concatMap escape (takeFileName filePath) ++ "](<" ++ filePath ++ pageHash ++ ">)"
  where
    escape c | c `elem` ("\\[]*_`<>" :: String) = ['\\', c]
             | otherwise                         = [c]

-- | Render the HTML for a PDF embed.
renderEmbed :: String -> String -> String
renderEmbed filePath pageHash =
    let viewerUrl = U.pdfViewerUrl filePath ++ pageHash
    in  "<div class=\"pdf-embed-wrapper\">"
     ++ "<iframe class=\"pdf-embed\""
     ++ " src=\"" ++ viewerUrl ++ "\""
     ++ " title=\"PDF document\""
     ++ " loading=\"lazy\""
     ++ " allowfullscreen></iframe>"
     ++ "</div>"

