{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
module Compilers
    ( essayCompiler
    , postCompiler
    , pageCompiler
    , poetryCompiler
    , fictionCompiler
    , compositionCompiler
    , readerOpts
    , writerOpts
    ) where

import           Hakyll
import           Text.Pandoc.Definition     (Pandoc (..), Block (..),
                                             Inline (..))
import           Text.Pandoc.Options        (ReaderOptions (..), WriterOptions (..),
                                             HTMLMathMethod (..))
import           Text.Pandoc.Extensions     (enableExtension, Extension (..))
import qualified Data.Text                  as T
import           Data.Maybe                 (fromMaybe)
import           System.FilePath            (takeDirectory)
import           Utils                      (wordCount, readingTime, escapeHtml)
import           Filters                    (applyAll, preprocessSource)
import qualified Citations
import qualified Filters.Score              as Score
import qualified Filters.Viz               as Viz

-- ---------------------------------------------------------------------------
-- Reader / writer options
-- ---------------------------------------------------------------------------

readerOpts :: ReaderOptions
readerOpts = defaultHakyllReaderOptions

-- | Reader options with hard_line_breaks enabled — every source newline within
--   a paragraph becomes a <br>. Used for poetry so stanza lines render as-is.
poetryReaderOpts :: ReaderOptions
poetryReaderOpts = readerOpts
    { readerExtensions = enableExtension Ext_hard_line_breaks
                            (readerExtensions readerOpts) }

writerOpts :: WriterOptions
writerOpts = defaultHakyllWriterOptions
    { writerHTMLMathMethod  = KaTeX ""
    , writerHighlightStyle  = Nothing
    , writerNumberSections  = False
    , writerTableOfContents = False
    }

-- ---------------------------------------------------------------------------
-- Inline stringification (local, avoids depending on Text.Pandoc.Shared)
-- ---------------------------------------------------------------------------

stringify :: [Inline] -> T.Text
stringify = T.concat . map inlineToText
  where
    inlineToText (Str t)           = t
    inlineToText Space             = " "
    inlineToText SoftBreak         = " "
    inlineToText LineBreak         = " "
    inlineToText (Emph ils)        = stringify ils
    inlineToText (Strong ils)      = stringify ils
    inlineToText (Strikeout ils)   = stringify ils
    inlineToText (Superscript ils) = stringify ils
    inlineToText (Subscript ils)   = stringify ils
    inlineToText (SmallCaps ils)   = stringify ils
    inlineToText (Quoted _ ils)    = stringify ils
    inlineToText (Cite _ ils)      = stringify ils
    inlineToText (Code _ t)        = t
    inlineToText (RawInline _ t)   = t
    inlineToText (Link _ ils _)    = stringify ils
    inlineToText (Image _ ils _)   = stringify ils
    inlineToText (Note _)          = ""
    inlineToText (Span _ ils)      = stringify ils
    inlineToText _                 = ""

-- ---------------------------------------------------------------------------
-- TOC extraction
-- ---------------------------------------------------------------------------

-- | Collect (level, identifier, title-text) for h2/h3 headings.
collectHeadings :: Pandoc -> [(Int, T.Text, String)]
collectHeadings (Pandoc _ blocks) = concatMap go blocks
  where
    go (Header lvl (ident, _, _) inlines)
        | lvl == 2 || lvl == 3
        = [(lvl, ident, T.unpack (stringify inlines))]
    go _ = []

-- ---------------------------------------------------------------------------
-- TOC tree
-- ---------------------------------------------------------------------------

data TOCNode = TOCNode T.Text String [TOCNode]

buildTree :: [(Int, T.Text, String)] -> [TOCNode]
buildTree = go 2
  where
    go _ [] = []
    go lvl ((l, i, t) : rest)
        | l == lvl  =
            let (childItems, remaining) = span (\(l', _, _) -> l' > lvl) rest
                children                = go (lvl + 1) childItems
            in  TOCNode i t children : go lvl remaining
        | l < lvl   = []
        | otherwise = go lvl rest   -- skip unexpected deeper items at this level

renderTOC :: [TOCNode] -> String
renderTOC [] = ""
renderTOC nodes = "<ol>\n" ++ concatMap renderNode nodes ++ "</ol>\n"
  where
    renderNode (TOCNode i t children) =
        "<li><a href=\"#" ++ T.unpack i ++ "\" data-target=\"" ++ T.unpack i ++ "\">"
        ++ Utils.escapeHtml t ++ "</a>" ++ renderTOC children ++ "</li>\n"

-- | Build a TOC HTML string from a Pandoc document.
buildTOC :: Pandoc -> String
buildTOC doc = renderTOC (buildTree (collectHeadings doc))

-- ---------------------------------------------------------------------------
-- Compilers
-- ---------------------------------------------------------------------------

-- | Shared compiler pipeline parameterised on reader options.
--   Saves toc/word-count/reading-time/bibliography snapshots.
essayCompilerWith :: ReaderOptions -> Compiler (Item String)
essayCompilerWith rOpts = do
    -- Raw Markdown source (used for word count / reading time).
    body <- getResourceBody
    let src = itemBody body

    -- Apply source-level preprocessors (wikilinks, etc.) before parsing.
    let body' = itemSetBody (preprocessSource src) body

    -- Parse to Pandoc AST.
    pandocItem <- readPandocWith rOpts body'

    -- Get further-reading keys from Hakyll metadata (YAML frontmatter is stripped
    -- before being passed to readPandocWith, so we read it from Hakyll instead).
    ident <- getUnderlying
    meta  <- getMetadata ident
    let frKeys = map T.pack $ fromMaybe [] (lookupStringList "further-reading" meta)
    let bibPath = T.pack $ fromMaybe "data/bibliography.bib" (lookupString "bibliography" meta)

    -- Run citeproc, transform citation spans → superscripts, extract bibliography.
    (pandocWithCites, bibHtml, furtherHtml) <- unsafeCompiler $
        Citations.applyCitations frKeys bibPath (itemBody pandocItem)

    -- Inline SVG score fragments and data visualizations (both read files
    -- relative to the source file's directory).
    filePath <- getResourceFilePath
    let srcDir = takeDirectory filePath
    pandocWithScores <- unsafeCompiler $
        Score.inlineScores srcDir pandocWithCites
    pandocWithViz <- unsafeCompiler $
        Viz.inlineViz srcDir pandocWithScores

    -- Apply remaining AST-level filters (sidenotes, smallcaps, links, etc.).
    -- applyAll touches the filesystem via Images.apply (webp existence
    -- check), so it runs through unsafeCompiler.
    pandocFiltered <- unsafeCompiler $ applyAll srcDir pandocWithViz
    let pandocItem'    = itemSetBody pandocFiltered pandocItem

    -- Build TOC from the filtered AST.
    let toc = buildTOC pandocFiltered

    -- Write HTML.
    let htmlItem = writePandocWith writerOpts pandocItem'

    -- Save snapshots keyed to this item's identifier.
    _ <- saveSnapshot "toc"                  (itemSetBody toc                            htmlItem)
    _ <- saveSnapshot "word-count"           (itemSetBody (show (wordCount src))         htmlItem)
    _ <- saveSnapshot "reading-time"         (itemSetBody (show (readingTime src))       htmlItem)
    _ <- saveSnapshot "bibliography"         (itemSetBody (T.unpack bibHtml)             htmlItem)
    _ <- saveSnapshot "further-reading-refs" (itemSetBody (T.unpack furtherHtml)         htmlItem)

    return htmlItem

-- | Compiler for essays.
essayCompiler :: Compiler (Item String)
essayCompiler = essayCompilerWith readerOpts

-- | Compiler for blog posts: same pipeline as essays.
postCompiler :: Compiler (Item String)
postCompiler = essayCompiler

-- | Compiler for poetry: enables hard_line_breaks so each source line becomes
--   a <br>, preserving verse line endings without manual trailing-space markup.
poetryCompiler :: Compiler (Item String)
poetryCompiler = essayCompilerWith poetryReaderOpts

-- | Compiler for fiction: same pipeline as essays; visual differences are
--   handled entirely by the reading template and reading.css.
fictionCompiler :: Compiler (Item String)
fictionCompiler = essayCompiler

-- | Compiler for music composition landing pages: full essay pipeline
--   (TOC, sidenotes, score fragments, citations, smallcaps, etc.).
compositionCompiler :: Compiler (Item String)
compositionCompiler = essayCompiler

-- | Compiler for simple pages: filters applied, no TOC snapshot.
pageCompiler :: Compiler (Item String)
pageCompiler = do
    body <- getResourceBody
    let src   = itemBody body
        body' = itemSetBody (preprocessSource src) body
    filePath   <- getResourceFilePath
    let srcDir  = takeDirectory filePath
    pandocItem <- readPandocWith readerOpts body'
    pandocFiltered <- unsafeCompiler $ applyAll srcDir (itemBody pandocItem)
    let pandocItem' = itemSetBody pandocFiltered pandocItem
    let htmlItem    = writePandocWith writerOpts pandocItem'
    _ <- saveSnapshot "word-count"   (itemSetBody (show (wordCount src))   htmlItem)
    _ <- saveSnapshot "reading-time" (itemSetBody (show (readingTime src)) htmlItem)
    return htmlItem
