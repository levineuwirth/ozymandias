{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Inline SVG score fragments into the Pandoc AST.
--
-- Fenced-div syntax in Markdown:
--
-- > :::score-fragment{score-name="Main Theme, mm. 1–8" score-caption="The opening gesture."}
-- > ![](scores/main-theme.svg)
-- > :::
--
-- The filter reads the referenced SVG from disk (path resolved relative to
-- the source file's directory), replaces hardcoded black fills/strokes with
-- @currentColor@ for dark-mode compatibility, and emits a @\<figure\>@ with
-- the appropriate exhibit attributes for gallery.js TOC integration.
module Filters.Score (inlineScores) where

import           Control.Exception      (IOException, try)
import           Data.Maybe             (listToMaybe)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           System.Directory       (doesFileExist)
import           System.FilePath        ((</>))
import           System.IO              (hPutStrLn, stderr)
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk       (walkM)
import qualified Utils                  as U

-- | Walk the Pandoc AST and inline all score-fragment divs.
--   @baseDir@ is the directory of the source file; image paths in the
--   fenced-div are resolved relative to it.
inlineScores :: FilePath -> Pandoc -> IO Pandoc
inlineScores baseDir = walkM (inlineScore baseDir)

inlineScore :: FilePath -> Block -> IO Block
inlineScore baseDir (Div (_, cls, attrs) blocks)
    | "score-fragment" `elem` cls = do
        let mName    = lookup "score-name"    attrs
            mCaption = lookup "score-caption" attrs
            mPath    = findImagePath blocks
        case mPath of
            Nothing   -> return $ Div ("", cls, attrs) blocks
            Just path -> do
                let fullPath = baseDir </> T.unpack path
                exists <- doesFileExist fullPath
                if not exists
                    then do
                        hPutStrLn stderr $
                            "[Score] missing SVG: " ++ fullPath
                            ++ " (referenced from a score-fragment in " ++ baseDir ++ ")"
                        return (errorBlock mName ("Missing score: " <> path))
                    else do
                        result <- try (TIO.readFile fullPath) :: IO (Either IOException T.Text)
                        case result of
                            Left e -> do
                                hPutStrLn stderr $
                                    "[Score] read error on " ++ fullPath ++ ": " ++ show e
                                return (errorBlock mName ("Could not read score: " <> path))
                            Right svgRaw -> do
                                let html = buildHtml mName mCaption (processColors svgRaw)
                                return $ RawBlock (Format "html") html
inlineScore _ block = return block

-- | Render an inline error block in place of a missing or unreadable score.
--   Mirrors the convention in 'Filters.Viz.errorBlock' so build failures are
--   visible to the author without aborting the entire site build.
errorBlock :: Maybe T.Text -> T.Text -> Block
errorBlock mName message =
    RawBlock (Format "html") $ T.concat
        [ "<figure class=\"score-fragment score-fragment--error\""
        , maybe "" (\n -> " data-exhibit-name=\"" <> escHtml n <> "\"") mName
        , ">"
        , "<div class=\"score-fragment-error\">"
        , escHtml message
        , "</div>"
        , "</figure>"
        ]

-- | Extract the image src from the first Para that contains an Image inline.
findImagePath :: [Block] -> Maybe T.Text
findImagePath blocks = listToMaybe
    [ src
    | Para inlines       <- blocks
    , Image _ _ (src, _) <- inlines
    ]

-- | Replace hardcoded black fill/stroke values with @currentColor@ so the
--   SVG inherits the CSS @color@ property in both light and dark modes.
--
--   6-digit hex patterns are at the bottom of the composition chain
--   (applied first) so they are replaced before the 3-digit shorthand,
--   preventing partial matches (e.g. @#000@ matching the prefix of @#000000@).
processColors :: T.Text -> T.Text
processColors
    -- 3-digit hex and keyword patterns (applied after 6-digit replacements)
    = T.replace "fill=\"#000\""       "fill=\"currentColor\""
    . T.replace "fill=\"black\""      "fill=\"currentColor\""
    . T.replace "stroke=\"#000\""    "stroke=\"currentColor\""
    . T.replace "stroke=\"black\""   "stroke=\"currentColor\""
    . T.replace "fill:#000"          "fill:currentColor"
    . T.replace "fill:black"         "fill:currentColor"
    . T.replace "stroke:#000"       "stroke:currentColor"
    . T.replace "stroke:black"      "stroke:currentColor"
    -- 6-digit hex patterns (applied first — bottom of the chain)
    . T.replace "fill=\"#000000\""    "fill=\"currentColor\""
    . T.replace "stroke=\"#000000\"" "stroke=\"currentColor\""
    . T.replace "fill:#000000"       "fill:currentColor"
    . T.replace "stroke:#000000"    "stroke:currentColor"

buildHtml :: Maybe T.Text -> Maybe T.Text -> T.Text -> T.Text
buildHtml mName mCaption svgContent = T.concat
    [ "<figure class=\"score-fragment exhibit\""
    , maybe "" (\n -> " data-exhibit-name=\"" <> escHtml n <> "\"") mName
    , " data-exhibit-type=\"score\">"
    , "<div class=\"score-fragment-inner\">"
    , svgContent
    , "</div>"
    , maybe "" (\c -> "<figcaption class=\"score-caption\">" <> escHtml c <> "</figcaption>") mCaption
    , "</figure>"
    ]

escHtml :: T.Text -> T.Text
escHtml = U.escapeHtmlText
