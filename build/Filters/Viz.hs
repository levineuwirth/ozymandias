{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Inline data visualizations into the Pandoc AST.
--
-- Two fenced-div classes are recognized in Markdown:
--
-- __Static figure__ (Matplotlib → SVG, no client-side JS required):
--
-- > ::: {.figure script="figures/myplot.py" caption="Caption text"}
-- > :::
--
-- Runs the Python script; stdout must be an SVG document with a
-- transparent background.  Black fills and strokes are replaced with
-- @currentColor@ so figures adapt to dark mode automatically.
-- See @tools/viz_theme.py@ for the recommended matplotlib setup.
--
-- __Interactive figure__ (Altair/Vega-Lite → JSON spec):
--
-- > ::: {.visualization script="figures/myplot.py" caption="Caption text"}
-- > :::
--
-- Runs the Python script; stdout must be a Vega-Lite JSON spec.  The spec
-- is embedded verbatim inside a @\<script type=\"application\/json\"\>@ tag;
-- @viz.js@ picks it up and renders it via Vega-Embed, applying a
-- monochrome theme that responds to the site\'s light/dark toggle.
--
-- __Authoring conventions:__
--
-- * Scripts are run from the project root; paths are relative to it.
-- * @script=@ paths are resolved relative to the source file\'s directory.
-- * For @.figure@ scripts: use pure black (@#000000@) for all drawn
--   elements and transparent backgrounds so @processColors@ and CSS
--   @currentColor@ handle dark mode.
-- * For @.visualization@ scripts: set encoding colours to @\"black\"@;
--   @viz.js@ applies the site palette via Vega-Lite @config@.
-- * Set @viz: true@ in the page\'s YAML frontmatter to load Vega JS.
module Filters.Viz (inlineViz) where

import           Control.Exception      (IOException, catch)
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import           System.Directory       (doesFileExist)
import           System.Exit            (ExitCode (..))
import           System.FilePath        ((</>))
import           System.IO              (hPutStrLn, stderr)
import           System.Process         (readProcessWithExitCode)
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk       (walkM)
import qualified Utils                  as U

-- ---------------------------------------------------------------------------
-- Public entry point
-- ---------------------------------------------------------------------------

-- | Walk the Pandoc AST and inline all @.figure@ and @.visualization@ divs.
--   @baseDir@ is the directory of the source file; @script=@ paths are
--   resolved relative to it.
inlineViz :: FilePath -> Pandoc -> IO Pandoc
inlineViz baseDir = walkM (transformBlock baseDir)

-- ---------------------------------------------------------------------------
-- Block transformation
-- ---------------------------------------------------------------------------

transformBlock :: FilePath -> Block -> IO Block
transformBlock baseDir blk@(Div (_, cls, attrs) _)
    | "figure" `elem` cls = do
        result <- runScript baseDir attrs
        case result of
            Left err ->
                warn "figure" err >> return (errorBlock err)
            Right out ->
                let caption = attr "caption" attrs
                in  return $ RawBlock (Format "html")
                        (staticFigureHtml (processColors out) caption)
    | "visualization" `elem` cls = do
        result <- runScript baseDir attrs
        case result of
            Left err ->
                warn "visualization" err >> return (errorBlock err)
            Right out ->
                let caption = attr "caption" attrs
                in  return $ RawBlock (Format "html")
                        (interactiveFigureHtml (escScriptTag out) caption)
    | otherwise = return blk
transformBlock _ b = return b

-- ---------------------------------------------------------------------------
-- Script execution
-- ---------------------------------------------------------------------------

-- | Run @python3 <script>@.  Returns the script\'s stdout on success, or an
--   error message on failure (non-zero exit, missing @script=@ attribute, or
--   missing script file).
runScript :: FilePath -> [(T.Text, T.Text)] -> IO (Either String T.Text)
runScript baseDir attrs =
    case lookup "script" attrs of
        Nothing -> return (Left "missing script= attribute")
        Just p  -> do
            let fullPath = baseDir </> T.unpack p
            exists <- doesFileExist fullPath
            if not exists
                then return (Left ("script not found: " ++ fullPath))
                else do
                    (ec, out, err) <-
                        readProcessWithExitCode "python3" [fullPath] ""
                        `catch` (\e -> return (ExitFailure 1, "", show (e :: IOException)))
                    return $ case ec of
                        ExitSuccess   -> Right (T.pack out)
                        ExitFailure _ -> Left $
                            "in " ++ fullPath ++ ": "
                            ++ (if null err then "non-zero exit" else err)

-- ---------------------------------------------------------------------------
-- SVG colour post-processing (mirrors Filters.Score.processColors)
-- ---------------------------------------------------------------------------

-- | Replace hardcoded black fill/stroke values with @currentColor@ so the
--   embedded SVG inherits the CSS text colour in both light and dark modes.
processColors :: T.Text -> T.Text
processColors
    = T.replace "fill=\"#000\""       "fill=\"currentColor\""
    . T.replace "fill=\"black\""      "fill=\"currentColor\""
    . T.replace "stroke=\"#000\""     "stroke=\"currentColor\""
    . T.replace "stroke=\"black\""    "stroke=\"currentColor\""
    . T.replace "fill:#000"           "fill:currentColor"
    . T.replace "fill:black"          "fill:currentColor"
    . T.replace "stroke:#000"         "stroke:currentColor"
    . T.replace "stroke:black"        "stroke:currentColor"
    . T.replace "fill=\"#000000\""    "fill=\"currentColor\""
    . T.replace "stroke=\"#000000\""  "stroke=\"currentColor\""
    . T.replace "fill:#000000"        "fill:currentColor"
    . T.replace "stroke:#000000"      "stroke:currentColor"

-- ---------------------------------------------------------------------------
-- JSON safety for <script> embedding
-- ---------------------------------------------------------------------------

-- | Replace @<\/@ with the JSON Unicode escape @\u003c\/@ so that Vega-Lite
--   JSON embedded inside a @\<script\>@ tag cannot accidentally close it.
--   JSON.parse decodes the escape back to @<\/@ transparently.
escScriptTag :: T.Text -> T.Text
escScriptTag = T.replace "</" "\\u003c/"

-- ---------------------------------------------------------------------------
-- HTML output
-- ---------------------------------------------------------------------------

staticFigureHtml :: T.Text -> T.Text -> T.Text
staticFigureHtml svgContent caption = T.concat
    [ "<figure class=\"viz-figure\">"
    , svgContent
    , if T.null caption then ""
      else "<figcaption class=\"viz-caption\">" <> escHtml caption <> "</figcaption>"
    , "</figure>"
    ]

interactiveFigureHtml :: T.Text -> T.Text -> T.Text
interactiveFigureHtml jsonSpec caption = T.concat
    [ "<figure class=\"viz-interactive\">"
    , "<div class=\"vega-container\">"
    , "<script type=\"application/json\" class=\"vega-spec\">"
    , jsonSpec
    , "</script>"
    , "</div>"
    , if T.null caption then ""
      else "<figcaption class=\"viz-caption\">" <> escHtml caption <> "</figcaption>"
    , "</figure>"
    ]

errorBlock :: String -> Block
errorBlock msg = RawBlock (Format "html") $ T.concat
    [ "<div class=\"viz-error\"><strong>Visualization error:</strong> "
    , escHtml (T.pack msg)
    , "</div>"
    ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

attr :: T.Text -> [(T.Text, T.Text)] -> T.Text
attr key kvs = fromMaybe "" (lookup key kvs)

warn :: String -> String -> IO ()
warn kind msg = hPutStrLn stderr $ "[Viz] " ++ kind ++ " error: " ++ msg

escHtml :: T.Text -> T.Text
escHtml = U.escapeHtmlText
