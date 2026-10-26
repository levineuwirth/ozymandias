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
-- * Scripts run under @.venv\/bin\/python3@ when that virtualenv exists
--   (@make venv@ creates it), otherwise under @python3@ from @PATH@.
-- * @script=@ paths are resolved relative to the source file\'s directory.
-- * For @.figure@ scripts: use pure black (@#000000@) for all drawn
--   elements and transparent backgrounds so @blackToCurrentColor@ and CSS
--   @currentColor@ handle dark mode. A colour that is deliberately not
--   black (a white label on a dark cell, a mid-grey fill) is left alone by
--   both, so it survives into the page as written.
-- * @caption=@ is parsed as Markdown: code spans, emphasis, links and
--   @$…$@ math all work.
-- * For @.visualization@ scripts: set encoding colours to @\"black\"@;
--   @viz.js@ applies the site palette via Vega-Lite @config@.
-- * Set @viz: true@ in the page\'s YAML frontmatter to load Vega JS.
module Filters.Viz (inlineViz) where

import           Control.Exception      (IOException, catch)
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import           System.Directory       (doesFileExist)
import           SvgColor               (blackToCurrentColor)
import           System.Environment     (getEnvironment, lookupEnv)
import           System.Exit            (ExitCode (..))
import           System.FilePath        (searchPathSeparator, takeDirectory, (</>))
import           System.IO              (hPutStrLn, stderr)
import           System.Process         (CreateProcess (..), proc, readCreateProcessWithExitCode)
import           System.Timeout         (timeout)
import           Text.Read              (readMaybe)
import qualified Text.Pandoc            as Pandoc
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk       (walkM)
import           EngineFiles            (resolveFile)
import           PandocOptions          (readerOpts, writerOpts)
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
transformBlock baseDir blk@(Div (divId, cls, attrs) _)
    | "figure" `elem` cls = do
        result <- runScript baseDir attrs
        case result of
            Left err ->
                warn "figure" err >> return (errorBlock err)
            Right out ->
                let caption = attr "caption" attrs
                    token   = idToken (attr "script" attrs)
                in  return $ RawBlock (Format "html")
                        (staticFigureHtml divId
                             (namespaceIds token (blackToCurrentColor out)) caption)
    | "visualization" `elem` cls = do
        result <- runScript baseDir attrs
        case result of
            Left err ->
                warn "visualization" err >> return (errorBlock err)
            Right out ->
                let caption = attr "caption" attrs
                in  return $ RawBlock (Format "html")
                        (interactiveFigureHtml divId (escScriptTag out) caption)
    | otherwise = return blk
transformBlock _ b = return b

-- ---------------------------------------------------------------------------
-- Script execution
-- ---------------------------------------------------------------------------

-- | Path to the Python interpreter used for figure scripts.
--
--   Prefers the project virtualenv over whatever @python3@ happens to be
--   first on @PATH@.  Scripts run from the project root, so the relative
--   path matches the @[ -d .venv ]@ gates the Makefile uses for its other
--   Python steps.
--
--   Without this preference a bare @make build@ breaks every figure: it
--   runs @cabal run site@ outside @uv run@, so the scripts inherit a
--   system interpreter that has no numpy or matplotlib and each one exits
--   with @ModuleNotFoundError@.  Falling back to @PATH@ keeps a
--   system-wide install (or an already-activated venv) working.
pythonExe :: IO FilePath
pythonExe = do
    let venvPython = ".venv" </> "bin" </> "python3"
    inVenv <- doesFileExist venvPython
    return (if inVenv then venvPython else "python3")

-- | The scripts' environment: this process's, with the directory that
--   holds @tools/viz_theme.py@ first on @PYTHONPATH@. The scripts put
--   @tools@ on their own path, which finds the theme only while it is the
--   site's; the engine's is under @engine/tools@ ("EngineFiles").
scriptEnvironment :: IO [(String, String)]
scriptEnvironment = do
    theme     <- takeDirectory <$> resolveFile ("tools" </> "viz_theme.py")
    inherited <- getEnvironment
    let rest = [p | Just p <- [lookup "PYTHONPATH" inherited], not (null p)]
        path = foldr1 (\a b -> a ++ [searchPathSeparator] ++ b) (theme : rest)
    pure (("PYTHONPATH", path) : filter ((/= "PYTHONPATH") . fst) inherited)

-- | Run @\<python\> <script>@.  Returns the script\'s stdout on success, or an
--   error message on failure (non-zero exit, missing @script=@ attribute, or
--   missing script file).  See 'pythonExe' for interpreter selection.
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
                    py     <- pythonExe
                    envir  <- scriptEnvironment
                    limit  <- scriptTimeoutSeconds
                    result <- withDeadline limit
                        (readCreateProcessWithExitCode
                            ((proc py [fullPath]) { env = Just envir }) "")
                        `catch` (\e -> return (Just (ExitFailure 1, "", show (e :: IOException))))
                    return $ case result of
                        Nothing -> Left $
                            "in " ++ fullPath ++ ": timed out after "
                            ++ show limit ++ "s (set VIZ_TIMEOUT_SECONDS "
                            ++ "to change the deadline; 0 disables it)"
                        Just (ExitSuccess, out, _) -> Right (T.pack out)
                        Just (ExitFailure _, _, err) -> Left $
                            "in " ++ fullPath ++ ": "
                            ++ (if null err then "non-zero exit" else err)

-- | Wall-clock deadline for one figure script, in seconds.
--
--   Audit smaller-finding 8 / B03: 'readProcessWithExitCode' had no
--   deadline, so an authored script that blocks — an infinite loop, a
--   solver that does not converge, a stray @input()@ reading from a
--   closed stdin — hung the whole build with no output and no diagnosis.
--   The failure mode of a build system should be a bad page, not no
--   answer.
--
--   @VIZ_TIMEOUT_SECONDS@ overrides the 120-second default; a value of
--   @0@ (or a negative one) disables the deadline for a deliberately
--   long-running figure. An unparseable value falls back to the default
--   rather than failing the build.
scriptTimeoutSeconds :: IO Int
scriptTimeoutSeconds = do
    raw <- lookupEnv "VIZ_TIMEOUT_SECONDS"
    return (fromMaybe defaultSeconds (raw >>= readMaybe))
  where
    defaultSeconds = 120

-- | Run an action under a deadline in seconds; 'Nothing' on expiry.
--
--   'timeout' delivers an asynchronous exception, which unwinds
--   'readCreateProcessWithExitCode'\'s @withCreateProcess@ bracket; that
--   bracket's cleanup terminates the child and closes its pipes, so the
--   stuck interpreter does not outlive the build. A non-positive limit
--   means "no deadline".
withDeadline :: Int -> IO a -> IO (Maybe a)
withDeadline seconds act
    | seconds <= 0 = Just <$> act
    | otherwise    = timeout (seconds * 1000000) act

-- ---------------------------------------------------------------------------
-- SVG id namespacing
-- ---------------------------------------------------------------------------

-- | Suffix every id in one figure's SVG, and every reference to one, so that
--   several inlined figures can share a page.
--
--   Matplotlib restarts its counters per figure, so five inlined SVGs gave
--   one page 491 duplicate ids — @figure_1@, @axes_1@, @text_1@ five times
--   each — of which 47 were referenced by 737 @\<use\>@ and @url(#…)@
--   pointers. Every one resolved to whichever copy came first in the
--   document. That was invalid HTML that happened to render correctly only
--   because all the figures used the same font, so the colliding glyph
--   definitions were identical; a figure with a different font or
--   @svg.fonttype@ would have silently drawn the wrong glyphs.
--
--   The token is a __suffix__ rather than a prefix on purpose:
--   @static\/css\/viz.css@ selects on @g[id^=\"text_\"]@, which is what
--   themes default-coloured label text, and a prefix would stop that
--   matching.
--
--   Content-hashed ids (@p1a2b…@, @m3c4d…@) are already unique per figure
--   because 'tools\/viz_theme.py' salts them with the script name; suffixing
--   them again is harmless and keeps the pass uniform.
namespaceIds :: T.Text -> T.Text -> T.Text
namespaceIds token
    | T.null token = id
    | otherwise    =
          suffixDelimited " id=\""    '"' suffix
        . suffixDelimited "href=\"#"  '"' suffix
        . suffixDelimited "url(#"     ')' suffix
  where
    suffix = "-" <> token

-- | Append @suffix@ to every value introduced by @prefix@ and closed by
--   @terminator@. All three call sites are self-delimiting, so this needs no
--   knowledge of which ids exist and cannot confuse @text_1@ with @text_10@.
suffixDelimited :: T.Text -> Char -> T.Text -> T.Text -> T.Text
suffixDelimited prefix terminator suffix = go
  where
    go t =
        let (pre, rest) = T.breakOn prefix t
        in  if T.null rest
                then pre
                else
                    let body         = T.drop (T.length prefix) rest
                        (val, after) = T.break (== terminator) body
                    in  pre <> prefix <> val <> suffix <> go after

-- | A short, stable, per-figure token derived from the script path:
--   @figures\/fig_kem_level.py@ becomes @fig-kem-level@. Deterministic, so it
--   does not disturb the reproducible-output contract, and unique within a
--   page because two figures in one directory cannot share a filename.
idToken :: T.Text -> T.Text
idToken script =
    T.map dash
        . T.dropWhileEnd (== '.')
        . fst . T.breakOn ".py"
        . T.takeWhileEnd (/= '/')
        $ script
  where
    dash c = if c == '_' then '-' else c

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

-- | The @#id@ on the fenced div is carried onto the @\<figure\>@ so
--   "Filters.FigureRefs" can anchor a cross-reference to it. Without an
--   explicit id the figure still gets numbered; it just gets a generated
--   anchor instead of the author's.
--
--   The SVG is wrapped in a scroll container carrying the figure's natural
--   width. Matplotlib bakes label text at a fixed size in user units, so
--   scaling a wide figure down to a narrow column scales its text with it:
--   @fig_decomp@ is 855 units across and its 10-unit tick labels came out at
--   roughly 4px on a 375px phone, against a legible floor near 10px. The
--   wrapper lets a figure keep a readable size and pan sideways instead.
staticFigureHtml :: T.Text -> T.Text -> T.Text -> T.Text
staticFigureHtml figId svgContent caption = T.concat
    [ "<figure", idAttr figId, " class=\"viz-figure\">"
    , "<div class=\"viz-scroll\"" <> naturalWidthStyle svgContent
      <> " tabindex=\"0\">"
    , svgContent
    , "</div>"
    , captionHtml caption
    , "</figure>"
    ]

-- | Expose the figure's natural width to CSS as @--viz-natural-width@.
--
--   The value is the viewBox width taken as pixels, which renders one user
--   unit per pixel — so matplotlib's default 10-unit label lands at 10px,
--   the smallest size that still reads. (Its @width@ attribute is the same
--   number in @pt@; using that instead would render at 4\/3 scale, more
--   comfortable but forcing a scrollbar on figures that read acceptably at
--   the 800px body column today.)
naturalWidthStyle :: T.Text -> T.Text
naturalWidthStyle svg = case viewBoxWidth svg of
    Nothing -> ""
    Just w  -> " style=\"--viz-natural-width:" <> T.pack (show w) <> "px\""

-- | Width component of the SVG root's @viewBox@, rounded up.
viewBoxWidth :: T.Text -> Maybe Int
viewBoxWidth svg =
    let (_, rest) = T.breakOn needle svg
    in  if T.null rest
            then Nothing
            else case T.words (T.takeWhile (/= '"')
                                   (T.drop (T.length needle) rest)) of
                     (_ : _ : w : _) ->
                         ceiling <$> (readMaybe (T.unpack w) :: Maybe Double)
                     _ -> Nothing
  where
    needle = "viewBox=\""

interactiveFigureHtml :: T.Text -> T.Text -> T.Text -> T.Text
interactiveFigureHtml figId jsonSpec caption = T.concat
    [ "<figure", idAttr figId, " class=\"viz-interactive\">"
    , "<div class=\"vega-container\">"
    , "<script type=\"application/json\" class=\"vega-spec\">"
    , jsonSpec
    , "</script>"
    , "</div>"
    , captionHtml caption
    , "</figure>"
    ]

-- | Wrap a rendered caption in its @\<figcaption\>@, or emit nothing when
--   the @caption=@ attribute is absent or empty.
captionHtml :: T.Text -> T.Text
captionHtml caption
    | T.null caption = ""
    | otherwise      =
        "<figcaption class=\"viz-caption\">" <> renderCaption caption <> "</figcaption>"

-- | Render a @caption=@ attribute value as Markdown.
--
--   Fenced-div attributes reach this filter as flat 'T.Text' — Pandoc does
--   not parse them — so unlike 'Filters.Images', which already holds
--   @[Inline]@ taken from an image's alt text, the caption has to be read
--   before it can be written. Escaping it instead, as this did originally,
--   shipped the source verbatim: every caption in the corpus uses code
--   spans and @$…$@ math, so readers saw literal backticks and
--   @$\\delta$@ under each figure.
--
--   The caption is read and written with the body's own options
--   ("PandocOptions"). Pandoc's 'def' would not do: its reader enables far
--   less than the site's, so a caption kept its straight quotes and its
--   @$…$@ stayed literal while body prose one line above got both; and
--   its math method wraps TeX in @\\(…\\)@ delimiters, while
--   @static\/js\/katex-bootstrap.js@ feeds each @\<span class="math"\>@'s
--   @textContent@ straight to @katex.render@, which wants bare TeX.
--
--   A caption is inline content, so the reader's block wrapper is unwrapped
--   to keep a @\<p\>@ out of the @\<figcaption\>@. On the (essentially
--   impossible) round-trip failure, fall back to the old escaped text.
renderCaption :: T.Text -> T.Text
renderCaption caption =
    case Pandoc.runPure convert of
        Right html -> T.strip html
        Left  _    -> escHtml caption
  where
    convert = do
        Pandoc meta blocks <- Pandoc.readMarkdown readerOpts caption
        Pandoc.writeHtml5String writerOpts (Pandoc meta (map unBlock blocks))

    unBlock (Para ils) = Plain ils
    unBlock b          = b


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

idAttr :: T.Text -> T.Text
idAttr t = if T.null t then "" else " id=\"" <> escHtml t <> "\""

warn :: String -> String -> IO ()
warn kind msg = hPutStrLn stderr $ "[Viz] " ++ kind ++ " error: " ++ msg

escHtml :: T.Text -> T.Text
escHtml = U.escapeHtmlText
