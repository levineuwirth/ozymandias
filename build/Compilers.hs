{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
module Compilers
    ( essayCompiler
    , postCompiler
    , pageCompiler
    , poetryCompiler
    , fictionCompiler
    , compositionCompiler
    , photographyCompiler
    , sidecarCompiler
    , readerOpts
    , writerOpts
    , transformDocument
    , pageInitials
    , buildTOC
    ) where

import           Config (SiteConfig, loadConfig)
import           Hakyll
import           Text.Pandoc.Definition     (Pandoc (..), Block (..),
                                             Inline (..), Citation (..),
                                             nullAttr, nullMeta)
import           Text.Pandoc.Class          (runPure)
import           Text.Pandoc.Walk           (walk, query)
import           Text.Pandoc.Writers        (writeHtml5String)
import           Text.Pandoc.Options        (ReaderOptions)
import qualified Data.Text                  as T
import           Control.Monad              (forM, forM_, void, when)
import           Data.List                  (isInfixOf)
import           Data.Maybe                 (fromMaybe, mapMaybe)
import           System.Directory           (doesDirectoryExist, listDirectory)
import           System.FilePath            (takeDirectory, takeExtension, (</>))
import           Hakyll.Core.Compiler.Internal (compilerTellDependencies)
import           Utils                      (wordCount, readingTime, escapeHtml, inlinesText, parseBool)
import           PandocOptions              (poetryReaderOpts, readerOpts, writerOpts)
import           Filters                    (applyAll, preprocessSource)
import           Filters.Dropcaps           (Initials (..), initialsDir)
import qualified Citations
import qualified Filters.Headings           as Headings
import qualified Filters.Score              as Score
import qualified Filters.Viz               as Viz
import           EngineFiles                (staticImageDimensions)

-- ---------------------------------------------------------------------------
-- Reader / writer options
-- ---------------------------------------------------------------------------

-- 'readerOpts', 'poetryReaderOpts' and 'writerOpts' live in "PandocOptions"
-- (re-exported here), where the filters can reach them too.

-- ---------------------------------------------------------------------------
-- Table of contents
-- ---------------------------------------------------------------------------

-- | A heading's inlines as the body renders them, for its TOC entry: the
--   site's writer, so quotation marks, math (typeset by KaTeX like the
--   body's) and abbreviations come out as they do in the heading itself.
--   Notes go; a link becomes its text, an anchor cannot hold another; and
--   a span loses its id, which the heading already has.
tocHtml :: [Inline] -> String
tocHtml ils = either (const (T.unpack (escapeText (inlinesText clean)))) (T.unpack . T.strip)
    (runPure (writeHtml5String writerOpts (Pandoc nullMeta [Plain clean])))
  where
    clean = walk unlink (walk (filter (not . isNote)) ils)
    isNote (Note _) = True
    isNote _        = False
    unlink (Link _ xs _)          = Span nullAttr xs
    unlink (Image _ xs _)         = Span nullAttr xs
    unlink (Span (_, cls, kv) xs) = Span ("", cls, kv) xs
    unlink x                      = x
    escapeText = T.pack . Utils.escapeHtml . T.unpack

-- ---------------------------------------------------------------------------
-- TOC extraction
-- ---------------------------------------------------------------------------

-- | Collect (level, identifier, entry HTML, plain label) for h2/h3 headings.
collectHeadings :: Pandoc -> [(Int, T.Text, String, String)]
collectHeadings (Pandoc _ blocks) = concatMap go blocks
  where
    go (Header lvl (ident, _, _) inlines)
        | lvl == 2 || lvl == 3
        = [(lvl, ident, tocHtml inlines, T.unpack (inlinesText inlines))]
    go _ = []

-- ---------------------------------------------------------------------------
-- TOC tree
-- ---------------------------------------------------------------------------

data TOCNode = TOCNode T.Text String String [TOCNode]

buildTree :: [(Int, T.Text, String, String)] -> [TOCNode]
buildTree = go 2
  where
    go _ [] = []
    go lvl ((l, i, h, t) : rest)
        | l == lvl  =
            let (childItems, remaining) = span (\(l', _, _, _) -> l' > lvl) rest
                children                = go (lvl + 1) childItems
            in  TOCNode i h t children : go lvl remaining
        | l < lvl   = []
        | otherwise = go lvl rest   -- skip unexpected deeper items at this level

renderTOC :: [TOCNode] -> String
renderTOC [] = ""
renderTOC nodes = "<ol>\n" ++ concatMap renderNode nodes ++ "</ol>\n"
  where
    renderNode (TOCNode i h t children) =
        "<li><a href=\"#" ++ T.unpack i ++ "\" data-target=\"" ++ T.unpack i
        ++ "\" data-label=\"" ++ Utils.escapeHtml t ++ "\">"
        ++ h ++ "</a>" ++ renderTOC children ++ "</li>\n"

-- | Build a TOC HTML string from a Pandoc document.
buildTOC :: Pandoc -> String
buildTOC doc = renderTOC (buildTree (collectHeadings doc))

-- ---------------------------------------------------------------------------
-- Compilers
-- ---------------------------------------------------------------------------

-- | Register the bibliography inputs a citeproc run reads as tracked
--   Hakyll dependencies: the page's own @.bib@ file and every CSL style
--   ('Citations.cslStyles').
--
--   Both are read through 'unsafeCompiler' (and, for the CSL, from a path
--   resolved in "Citations"), so nothing else puts them in the
--   dependency graph. Only files some rule actually claims can be
--   'load'ed — an identifier outside Hakyll's universe is a hard error,
--   and can never be out of date either — hence the 'getMatches' guard
--   and the matching no-route rules in "Site".
--
--   Scope note: the dependency is registered whether or not the page ends
--   up citing anything, because that is only known after citeproc has
--   run. The cost is recompiling non-citing pages when a @.bib@ changes;
--   the alternative is a page whose bibliography silently rots.
trackBibliographyInputs :: Identifier -> Compiler ()
trackBibliographyInputs bibIdent = do
    track (fromList [bibIdent])
    track (Citations.cslStyles .&&. hasNoVersion)
  where
    track pat = do
        ids <- getMatches pat
        forM_ ids $ \i -> void (load i :: Compiler (Item String))

-- | Shared compiler pipeline parameterised on reader options.
--   Saves toc/word-count/reading-time/bibliography snapshots.
essayCompilerWith :: ReaderOptions -> Compiler (Item String)
essayCompilerWith rOpts = do
    cfg <- loadConfig
    -- Raw Markdown source (used for word count / reading time).
    body <- getResourceBody
    let src = itemBody body

    -- Apply source-level preprocessors (wikilinks, etc.) before parsing.
    let body' = itemSetBody (preprocessSource cfg src) body

    -- Parse to Pandoc AST.
    --
    -- Heading levels are normalised immediately after parsing, before
    -- anything reads or emits a heading: the imported research essays use
    -- h1 for their body sections (the page title is an h1 from the
    -- template), which left the document with a dozen top-level headings
    -- and their major sections out of the TOC entirely, since
    -- 'collectHeadings' collects h2/h3. 'Filters.Headings.normalizeLevels'
    -- is the identity for every document that already starts at h2.
    rawPandoc  <- readPandocWith rOpts body'
    let pandocItem = fmap Headings.normalizeLevels rawPandoc

    -- Get further-reading keys from Hakyll metadata (YAML frontmatter is stripped
    -- before being passed to readPandocWith, so we read it from Hakyll instead).
    ident <- getUnderlying
    meta  <- getMetadata ident
    let frKeys = map T.pack $ fromMaybe [] (lookupStringList "further-reading" meta)
    let bibPath = T.pack $ fromMaybe "data/bibliography.bib" (lookupString "bibliography" meta)

    -- Bibliography inputs are read by citeproc inside 'unsafeCompiler',
    -- which is invisible to Hakyll's dependency graph: without an explicit
    -- 'load' an edit to a .bib entry (or to the CSL style) leaves every
    -- already-compiled page serving its cached bibliography. Loading the
    -- entry's own .bib and the CSL registers both as tracked inputs.
    -- Guarded by 'getMatches' because 'load' on an identifier no rule
    -- claims is a hard error, and 'bibliography:' is author-supplied.
    -- The synthetic /bibliography/ pages do the same over every .bib file
    -- (see build/Site.hs).
    trackBibliographyInputs (fromFilePath (T.unpack bibPath))

    filePath <- getResourceFilePath
    let srcDir = takeDirectory filePath
    -- Opt-in figure numbering. Off unless the page asks for it: three
    -- essays already number by hand in three different conventions, and
    -- numbering them automatically would double up.
    let numberFigures =
            fromMaybe False (lookupString "figure-numbering" meta >>= parseBool)

    let initials = pageInitials srcDir meta
    trackInitials initials

    (pandocFiltered, bibHtml, furtherHtml) <- unsafeCompiler $
        transformDocument cfg frKeys bibPath numberFigures srcDir initials (itemBody pandocItem)
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
    -- The keys this page cites or lists as further reading, for
    -- /bibliography/, which claims "every work cited across this site" and
    -- so lists only these (audit C07).
    let citeKeys = query (\i -> case i of
                              Cite cs _ -> map citationId cs
                              _         -> []) (itemBody pandocItem)
    _ <- saveSnapshot "cite-keys"            (itemSetBody (unwords (map T.unpack (citeKeys ++ frKeys))) htmlItem)

    return htmlItem

-- | The page's sets of drawn initials, in order (frontmatter
--   @initials:@, one path or a list, each resolved by 'initialsDir'), and
--   the stage its @status:@ draws them at, as a class: @Working model@ is
--   @working-model@.
pageInitials :: FilePath -> Metadata -> Initials
pageInitials srcDir meta = Initials
    { initialSets  = mapMaybe (initialsDir srcDir) $
          fromMaybe (maybe [] pure (lookupString "initials" meta)) (lookupStringList "initials" meta)
    , initialStage = stage <$> lookupString "status" meta
    }
  where
    stage = T.intercalate "-" . T.words . T.toLower . T.pack

-- | Record that a page depends on its sets of drawn initials, whose letters
--   'Filters.Dropcaps' reads in IO, where Hakyll cannot see: without it,
--   editing a letter left every page drawn from it as it was. The sets'
--   files are claimed by the static rules, so the dependency fires; a letter
--   added to a set later reaches its pages at their next edit or a clean
--   build.
trackInitials :: Initials -> Compiler ()
trackInitials (Initials dirs _) = do
    letters <- unsafeCompiler $ fmap concat $ forM dirs $ \dir -> do
        exists <- doesDirectoryExist dir
        if exists then map (dir </>) . filter ((== ".svg") . takeExtension) <$> listDirectory dir
                  else pure []
    compilerTellDependencies [IdentifierDependency (fromFilePath f) | f <- letters]

-- | Everything 'essayCompilerWith' does between the parsed document and the
--   filtered one, outside Hakyll, so that @site render-fixture@ (build/Golden.hs,
--   tests/test_golden.py) runs the same code a page does.
--
--   Citeproc first: it turns citations into numbered markers and pulls the
--   bibliography out. Then score fragments and visualizations, which read
--   files relative to @srcDir@. Then the remaining AST filters (sidenotes,
--   smallcaps, links, …); 'applyAll' probes the filesystem for @.webp@
--   companions, so it is IO too.
--
--   Returns the filtered document, the bibliography HTML and the
--   further-reading HTML.
transformDocument :: SiteConfig -> [T.Text] -> T.Text -> Bool -> FilePath -> Initials -> Pandoc
                  -> IO (Pandoc, T.Text, T.Text)
transformDocument cfg frKeys bibPath numberFigures srcDir initials doc = do
    (withCites, bibHtml, furtherHtml) <- Citations.applyCitations frKeys bibPath doc
    withScores <- Score.inlineScores srcDir withCites
    withViz    <- Viz.inlineViz srcDir withScores
    filtered   <- applyAll cfg numberFigures srcDir initials withViz
    return (filtered, bibHtml, furtherHtml)

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

-- | Compiler for photography pages: body prose runs through the same
--   source preprocessors and AST filters as other content (so wikilinks,
--   smallcaps, sidenotes, image @<picture>@ wrapping, etc. all work in
--   caption / process-note prose), but skips TOC, word-count,
--   reading-time, citations, and further-reading. Visual content has no
--   meaningful word count, and the epistemic / bibliography surfaces in
--   'essayCtx' don't apply here.
photographyCompiler :: Compiler (Item String)
photographyCompiler = do
    (src, htmlItem) <- filteredPage
    -- Filters.Images reads a *.dims.yaml sidecar for each Markdown image,
    -- inside unsafeCompiler where Hakyll cannot see it. Only a page whose
    -- own Markdown has an image needs the dependency, and today no
    -- photograph's does: putting it on all 378 pages cost ~11 s of every
    -- build's out-of-date check (audit H06). A glob, so a new sidecar counts.
    when ("![" `isInfixOf` src) $
        void $ getMatches (   "content/photography/*.dims.yaml"
                         .||. "content/photography/*/*.dims.yaml"
                         .||. staticImageDimensions)
    return htmlItem

-- | Reduced pipeline for tag-meta sidecar markdown files. Applies
--   source-level preprocessors and AST filters (wikilinks, sidenotes,
--   smallcaps, links, etc.) so sidecar prose can use the same rich
--   markdown features as essays, then saves the rendered HTML under
--   the @"body"@ snapshot. Skips TOC, word count, reading time, and
--   citations — none of those belong in a portal intro. The item
--   itself is not routed; the body is consumed only via snapshot
--   loads by the tag-index rule and the home-page grid.
sidecarCompiler :: Compiler (Item String)
sidecarCompiler = do
    (_, htmlItem) <- filteredPage
    saveSnapshot "body" htmlItem

-- | Compiler for simple pages: filters applied, no TOC snapshot.
pageCompiler :: Compiler (Item String)
pageCompiler = do
    (src, htmlItem) <- filteredPage
    _ <- saveSnapshot "word-count"   (itemSetBody (show (wordCount src))   htmlItem)
    _ <- saveSnapshot "reading-time" (itemSetBody (show (readingTime src)) htmlItem)
    return htmlItem

-- | The lighter pipeline the photography, sidecar and page compilers
--   share: the source preprocessors, the reader, the AST filters (no
--   citations, scores, visualizations or figure numbering) and the
--   writer. Returns the raw source too, for the word count.
filteredPage :: Compiler (String, Item String)
filteredPage = do
    cfg <- loadConfig
    body     <- getResourceBody
    filePath <- getResourceFilePath
    let src = itemBody body
    pandocItem <- readPandocWith readerOpts (itemSetBody (preprocessSource cfg src) body)
    meta       <- getMetadata =<< getUnderlying
    let srcDir   = takeDirectory filePath
        initials = pageInitials srcDir meta
    trackInitials initials
    filtered   <- unsafeCompiler $
        applyAll cfg False srcDir initials (itemBody pandocItem)
    return (src, writePandocWith writerOpts (itemSetBody filtered pandocItem))
