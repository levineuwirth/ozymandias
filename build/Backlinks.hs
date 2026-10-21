{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Backlinks with context: build-time computation of which pages link to
-- each page, including the sentence that contains each link.
--
-- Architecture (dependency-correct, no circular deps):
--
--   1. Each content file is compiled under @version "links"@: a lightweight
--      pass that parses the source, walks the AST block-by-block, splits
--      each paragraph into sentences, and for every internal link records
--      the URL *and* the HTML of the sentence that contains it. The result
--      is serialised as a JSON array of @{url, context}@ objects.
--
--   2. A @create ["data/backlinks.json"]@ rule loads all "links" items,
--      inverts the map, and serialises
--      @target → [{url, title, abstract, context}]@ as JSON.
--
--   3. Between the two compile passes, @site footer-data@ splits that JSON
--      into one file per page (build/FooterData.hs). @backlinksField@ reads
--      the page's file and injects an HTML list showing each source's title
--      and a quoted sentence of context. A page depends on its own file
--      only, so it recompiles when its backlinks change and not whenever
--      any page's links do; @data/backlinks.json@ itself changes on almost
--      every edit, and Hakyll has no early cutoff (audit H01).
--
-- Dependency order (no cycles):
--   content "links" versions → data/backlinks.json
--   data/footer/<page>.json (written between passes) → content default versions
module Backlinks
    ( backlinkRules
    , backlinksField
    , referencedByField
    , backlinkMathField
    ) where

import           Data.List                  (isInfixOf, nub, nubBy, partition,
                                             sortBy, stripPrefix)
import           Data.Ord                   (comparing)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import qualified Data.Text.Encoding         as TE
import qualified Data.Aeson                 as Aeson
import           Data.Aeson                 ((.=))
import           Text.Pandoc.Class          (runPure)
import           Text.Pandoc.Writers        (writeHtml5String)
import           Text.Pandoc.Definition     (Block (..), Inline (..), Pandoc (..),
                                             nullMeta)
import           Text.Pandoc.Options        (WriterOptions (..), HTMLMathMethod (..))
import           Text.Pandoc.Walk           (query, walk)
import           Hakyll
import           Config                     (SiteConfig, loadConfig)
import           Compilers                  (readerOpts, writerOpts)
import           Filters                    (preprocessSource)
import qualified Patterns                   as P
import           ArchiveIndex               (archiveSlugFor)
import           Utils                      (canonicalUrlPath)
import           FooterData                 (footerEntries, normaliseUrl)

-- ---------------------------------------------------------------------------
-- Link-with-context entry (intermediate, saved by the "links" pass)
-- ---------------------------------------------------------------------------

data LinkEntry = LinkEntry
    { leUrl       :: T.Text   -- internal URL (as found in the AST)
    , leSentence  :: String   -- HTML of the sentence containing the link
    , leParagraph :: String   -- HTML of the full surrounding paragraph
    } deriving (Show, Eq)

instance Aeson.ToJSON LinkEntry where
    toJSON e = Aeson.object
        [ "url"       .= leUrl e
        , "sentence"  .= leSentence e
        , "paragraph" .= leParagraph e
        ]

instance Aeson.FromJSON LinkEntry where
    parseJSON = Aeson.withObject "LinkEntry" $ \o ->
        LinkEntry
            <$> o Aeson..: "url"
            <*> o Aeson..: "sentence"
            <*> o Aeson..: "paragraph"

-- ---------------------------------------------------------------------------
-- Backlink source record (stored in data/backlinks.json)
-- ---------------------------------------------------------------------------

data BacklinkSource = BacklinkSource
    { blUrl       :: String
    , blTitle     :: String
    , blAbstract  :: String
    , blSentence  :: String   -- raw HTML of the sentence containing the link
    , blParagraph :: String   -- raw HTML of the full paragraph (hover popup)
    , blFragment  :: String   -- archived-target fragment (no '#'), else ""
    } deriving (Show, Eq, Ord)

instance Aeson.ToJSON BacklinkSource where
    toJSON bl = Aeson.object
        [ "url"       .= blUrl bl
        , "title"     .= blTitle bl
        , "abstract"  .= blAbstract bl
        , "sentence"  .= blSentence bl
        , "paragraph" .= blParagraph bl
        , "fragment"  .= blFragment bl
        ]

instance Aeson.FromJSON BacklinkSource where
    parseJSON = Aeson.withObject "BacklinkSource" $ \o ->
        BacklinkSource
            <$> o Aeson..:  "url"
            <*> o Aeson..:  "title"
            <*> o Aeson..:  "abstract"
            <*> o Aeson..:  "sentence"
            <*> o Aeson..:  "paragraph"
            <*> o Aeson..:? "fragment" Aeson..!= ""

-- ---------------------------------------------------------------------------
-- Writer options for context rendering
-- ---------------------------------------------------------------------------

-- | Minimal writer options for rendering paragraph context: no template
-- (fragment only) and the site's own math method, so a quoted sentence
-- typesets exactly like the same sentence on its source page.
--
-- This used to be 'PlainMath', on the reasoning that an excerpt is a
-- preview and KaTeX might not be loaded on the target page. Both halves
-- were wrong. 'PlainMath' converts what it can to Unicode and re-emits
-- everything else as its literal TeX *with the @$@ delimiters kept*,
-- still wearing @class="math"@ — and @static\/js\/katex-bootstrap.js@
-- renders every @.math@ element from its @textContent@. So a preview
-- quoting @$c(H)\\le C\\sqrt N$@ handed KaTeX a string beginning with a
-- dollar sign, which it rejected and (@throwOnError: false@) painted red
-- in the footer of an otherwise finished page. The KaTeX method emits
-- bare LaTeX with no delimiters, which is exactly what that bootstrap
-- expects, so the quote and the @¶@ full-paragraph popup both typeset.
--
-- Loading: every template that renders @$backlinks$@ sets @math@ through
-- 'Contexts.essayCtx' (essay, reading, composition; the commonplace page
-- has no backlinks field at all). The one surface outside that set is the
-- archive page's @$referenced-by$@, which 'backlinkMathField' covers.
contextWriterOpts :: WriterOptions
contextWriterOpts = writerOpts
    { writerTemplate       = Nothing
    , writerHTMLMathMethod = KaTeX ""
    }

-- ---------------------------------------------------------------------------
-- Context extraction
-- ---------------------------------------------------------------------------

-- | URL filter: skip external links, pseudo-schemes, anchor-only fragments,
-- and static-asset paths.
isPageLink :: T.Text -> Bool
isPageLink u
    -- An archived external URL is kept regardless of scheme or extension:
    -- pass 2 inverts it to its /archive/<slug>/ page.
    | isArchived = True
    | otherwise  =
        not (T.isPrefixOf "http://"  u) &&
        not (T.isPrefixOf "https://" u) &&
        -- protocol-relative //host/path is external, not a page path
        not (T.isPrefixOf "//"       u) &&
        not (T.isPrefixOf "#"        u) &&
        not (T.isPrefixOf "mailto:"  u) &&
        not (T.isPrefixOf "tel:"     u) &&
        not (T.null u) &&
        not (hasStaticExt u)
  where
    isArchived = case archiveSlugFor u of
                     Just _  -> True
                     Nothing -> False
    staticExts = [".pdf",".svg",".png",".jpg",".jpeg",".webp",
                  ".mp3",".mp4",".woff2",".woff",".ttf",".ico",
                  ".json",".asc",".xml",".gz",".zip"]
    hasStaticExt x = any (`T.isSuffixOf` T.toLower x) staticExts

-- | Render a list of inlines to an HTML fragment string.
-- Uses Plain (not Para) to avoid a wrapping <p> — callers add their own.
--
-- This pass runs on the raw AST, before citeproc and the sidenote filter.
-- A citation would render as its source syntax (@[\@PralatWormald,
-- Theorem 3.1]@) and a footnote as a whole footnotes section inside the
-- excerpt (audit H04). A citation becomes "[…]", the mark for matter left
-- out of a quotation: the cited work's number or label belongs to the
-- other page's bibliography and would mean nothing here. Notes go.
renderInlines :: [Inline] -> String
renderInlines inlines =
    case runPure (writeHtml5String contextWriterOpts doc) of
        Left  _   -> ""
        Right txt -> T.unpack txt
  where
    doc = Pandoc nullMeta [Plain (walk quoteable inlines)]
    quoteable :: [Inline] -> [Inline]
    quoteable = concatMap one
    one (Cite _ _) = [Str "[\8230]"]
    one (Note _)   = []
    one x          = [x]

-- | Split a list of inlines into sentences by terminator punctuation.
--
-- A @Str@ whose last non-closing-punctuation character is @.@, @!@, or @?@
-- ends a sentence when followed by @Space@, @SoftBreak@, @LineBreak@, or
-- end-of-list. Closing quote/bracket characters after the terminator
-- (e.g. @right-double-quote@, @)@, @]@) are tolerated.
--
-- The splitter is deliberately simple: abbreviations like "e.g." or "Dr."
-- will cause occasional over-splitting. That is acceptable for backlink
-- previews, where a slightly short context is preferable to the complexity
-- of abbreviation detection.
splitSentences :: [Inline] -> [[Inline]]
splitSentences = go []
  where
    go acc [] = if null acc then [] else [reverse acc]
    go acc (tok : rest)
        | isTerminator tok && leadingBreak rest =
            reverse (tok : acc) : go [] (dropLeadingBreak rest)
        | otherwise =
            go (tok : acc) rest

    isTerminator :: Inline -> Bool
    isTerminator (Str s) = endsWithTerminator s
    isTerminator _       = False

    endsWithTerminator :: T.Text -> Bool
    endsWithTerminator t =
        case T.unsnoc (T.dropWhileEnd isClosingPunct t) of
            Just (_, c) -> c == '.' || c == '!' || c == '?'
            Nothing     -> False

    isClosingPunct :: Char -> Bool
    isClosingPunct c = c `elem` (")]\"'\x201D\x2019" :: String)

    leadingBreak :: [Inline] -> Bool
    leadingBreak []              = True
    leadingBreak (Space     : _) = True
    leadingBreak (SoftBreak : _) = True
    leadingBreak (LineBreak : _) = True
    leadingBreak _               = False

    dropLeadingBreak :: [Inline] -> [Inline]
    dropLeadingBreak (Space     : xs) = xs
    dropLeadingBreak (SoftBreak : xs) = xs
    dropLeadingBreak (LineBreak : xs) = xs
    dropLeadingBreak xs               = xs

-- | Extract @LinkEntry@ records from a Pandoc document.
-- For every internal link in a paragraph, emit an entry carrying the HTML
-- of the sentence containing the link (default display) and the HTML of
-- the full paragraph (hover/popup context).
-- Recurses into Div, BlockQuote, BulletList, OrderedList, and
-- DefinitionList. @Plain@ matters as much as @Para@: Pandoc renders
-- tight list items (the default @- item@ Markdown form) as @Plain@
-- blocks, so without it every link written in a tight list would be
-- invisible to the backlinks system.
extractLinksWithContext :: Pandoc -> [LinkEntry]
extractLinksWithContext (Pandoc _ blocks) = concatMap go blocks
  where
    go :: Block -> [LinkEntry]
    go (Para inlines)         = paraEntries inlines
    go (Plain inlines)        = paraEntries inlines
    go (BlockQuote bs)        = concatMap go bs
    go (Div _ bs)             = concatMap go bs
    go (BulletList items)     = concatMap (concatMap go) items
    go (OrderedList _ items)  = concatMap (concatMap go) items
    go (DefinitionList defs)  = concatMap defEntries defs
    go _                      = []

    defEntries :: ([Inline], [[Block]]) -> [LinkEntry]
    defEntries (term, bodies) =
        paraEntries term ++ concatMap (concatMap go) bodies

    paraEntries :: [Inline] -> [LinkEntry]
    paraEntries inlines =
        let paraHtml  = renderInlines inlines
            sentences = splitSentences inlines
        in  concatMap (sentenceEntries paraHtml) sentences

    sentenceEntries :: String -> [Inline] -> [LinkEntry]
    sentenceEntries paraHtml sentence =
        let urls = filter isPageLink (query getUrl sentence)
        in  if null urls then []
            else
                let sentHtml = renderInlines sentence
                in  map (\u -> LinkEntry u sentHtml paraHtml) urls

    getUrl :: Inline -> [T.Text]
    getUrl (Link _ _ (url, _)) = [url]
    getUrl _                   = []

-- ---------------------------------------------------------------------------
-- Lightweight links compiler
-- ---------------------------------------------------------------------------

-- | Compile a source file lightly: parse the Markdown (wikilinks preprocessed),
-- extract internal links with their paragraph context, and serialise as JSON.
linksCompiler :: Compiler (Item String)
linksCompiler = do
    cfg  <- loadConfig
    body <- getResourceBody
    let src   = itemBody body
    let body' = itemSetBody (preprocessSource cfg src) body
    pandocItem <- readPandocWith readerOpts body'
    let entries = nubBy sameEntry
                        (extractLinksWithContext (itemBody pandocItem))
    makeItem . TL.unpack . TLE.decodeUtf8 . Aeson.encode $ entries
  where
    sameEntry a b =
        leUrl       a == leUrl       b &&
        leSentence  a == leSentence  b &&
        leParagraph a == leParagraph b

-- ---------------------------------------------------------------------------
-- Archive-aware target keying
-- ---------------------------------------------------------------------------

-- | The @data/backlinks.json@ key an outbound URL inverts to. An archived
-- external URL canonicalises to its @/archive/<slug>/@ page key — computed
-- exactly as 'FooterData.footerKey' computes the archive page's own key (the
-- same string fed through 'normaliseUrl'), so the two always agree. Every
-- other URL is normalised as before.
targetKey :: T.Text -> T.Text
targetKey u = case archiveSlugFor u of
    Just slug -> T.pack (normaliseUrl ("/archive/" ++ slug ++ "/index.html"))
    Nothing   -> T.pack (normaliseUrl (T.unpack u))

-- | The fragment (without @#@) of an archived URL, for granular grouping
-- of "Referenced by". Empty for a non-archived URL or one with no fragment
-- — so granular grouping stays an archive-only behaviour.
archiveFragment :: T.Text -> String
archiveFragment u = case archiveSlugFor u of
    Just _  -> T.unpack (T.drop 1 (T.dropWhile (/= '#') u))
    Nothing -> ""

-- ---------------------------------------------------------------------------
-- Content patterns (must match the rules in Site.hs — sourced from
-- Patterns.allContent so additions to the canonical list automatically
-- propagate to backlinks).
-- ---------------------------------------------------------------------------

allContent :: SiteConfig -> Pattern
allContent = P.allContent

-- ---------------------------------------------------------------------------
-- Hakyll rules
-- ---------------------------------------------------------------------------

-- | Register the @version "links"@ rules for all content and the
-- @create ["data/backlinks.json"]@ rule.  Call this from 'Site.rules'.
backlinkRules :: SiteConfig -> Rules ()
backlinkRules cfg = do
    -- Pass 1: extract links + context from each content file.
    match (allContent cfg) $ version "links" $
        compile linksCompiler

    -- Pass 2: invert the map and write the backlinks JSON.
    create ["data/backlinks.json"] $ do
        route   idRoute
        compile $ do
            items <- loadAll (allContent cfg .&&. hasVersion "links")
                        :: Compiler [Item String]
            pairs <- concat <$> mapM toSourcePairs items
            makeItem . TL.unpack . TLE.decodeUtf8 . Aeson.encode
                $ Map.fromListWith (++) [(k, [v]) | (k, v) <- pairs]

-- | For one "links" item, produce @(normalised-target-url, BacklinkSource)@
-- pairs — one per internal link found in the source file.
toSourcePairs :: Item String -> Compiler [(T.Text, BacklinkSource)]
toSourcePairs item = do
    let ident0   = setVersion Nothing (itemIdentifier item)
    mRoute       <- getRoute ident0
    meta         <- getMetadata ident0
    -- The href a reader clicks, so it is the canonical directory form —
    -- not the raw route, which for a directory-routed essay ends in
    -- index.html and would be the site's only surviving link to the
    -- second spelling of a page (audit C03). This is the display URL
    -- only; the join key is 'normaliseUrl', which strips both spellings.
    let srcUrl   = maybe "" canonicalUrlPath mRoute
    let title    = fromMaybe "(untitled)" (lookupString "title" meta)
    let abstract = fromMaybe "" (lookupString "abstract" meta)
    case mRoute of
        Nothing -> return []
        Just _  ->
            case Aeson.decodeStrict (TE.encodeUtf8 (T.pack (itemBody item)))
                    :: Maybe [LinkEntry] of
                Nothing      -> return []
                Just entries ->
                    return [ ( targetKey (leUrl e)
                             , BacklinkSource srcUrl title abstract
                                              (rebaseFragmentLinks srcUrl (leSentence  e))
                                              (rebaseFragmentLinks srcUrl (leParagraph e))
                                              (archiveFragment (leUrl e))
                             )
                           | e <- entries ]

-- | Point an excerpt's same-page links at the page it was quoted from.
--
-- The excerpt is rendered in its source file's terms, so a cross-reference
-- written there as @[(H1)](#cloud-h1)@ arrives as @href="#cloud-h1"@ — which,
-- quoted in another page's Backlinks footer, names an anchor that page does
-- not have. 'isPageLink' already keeps such links out of the backlinks
-- graph; this keeps them working inside the quote. tools/check-site.py
-- (link-fragments) fails the build on any that slip through.
--
-- A fragment the excerpt defines itself stays put: a quoted footnote is
-- rendered into the excerpt with its own @#fn1@ and @#fnref1@, and the
-- source page has neither, because its notes become sidenotes.
rebaseFragmentLinks :: String -> String -> String
rebaseFragmentLinks srcUrl html
    | null srcUrl = html
    | otherwise   = case T.splitOn marker (T.pack html) of
        []             -> html
        (lead : hrefs) -> T.unpack (T.concat (lead : concatMap rebase hrefs))
  where
    marker   = "href=\"#"
    rebased  = T.pack ("href=\"" ++ escapeHtml srcUrl ++ "#")
    localIds = map (T.takeWhile (/= '"')) (drop 1 (T.splitOn " id=\"" (T.pack html)))
    rebase chunk
        | T.takeWhile (/= '"') chunk `elem` localIds = [marker, chunk]
        | otherwise                                   = [rebased, chunk]

-- ---------------------------------------------------------------------------
-- Context field
-- ---------------------------------------------------------------------------

-- | Context field @$backlinks$@ that injects an HTML list of pages that link
-- to the current page, each with its paragraph context.
-- Returns @noResult@ (so @$if(backlinks)$@ is false) when there are none.
backlinksField :: Context String
backlinksField = backlinksFieldWith renderBacklinks "backlinks"

-- | "Referenced by" for archive pages. Same lookup as 'backlinksField',
-- but the sources are grouped by the fragment each citation targets, so an
-- archived work's page can show which section/page each citing essay points
-- at (granular backlinks).
referencedByField :: Context String
referencedByField = backlinksFieldWith renderReferencedBy "referenced-by"

-- | @$math$@ for a page whose only math arrives through a backlink context.
--
-- 'contextWriterOpts' emits quoted math the way the rest of the site does:
-- bare LaTeX in a @class="math"@ span, typeset at runtime by
-- @static\/js\/katex-bootstrap.js@. That script and the KaTeX stylesheet are
-- both behind @$if(math)$@, and every template carrying @$backlinks$@ gets
-- @math@ from 'Contexts.essayCtx'. The archive page does not: it renders
-- @$referenced-by$@ off a bare 'siteCtx', so a citing sentence containing
-- math would otherwise show its LaTeX source. This field sets the flag for
-- exactly those pages, mirroring the @needsKatex@ guards the bibliography
-- routes already use in 'Site.rules', and returns 'noResult' otherwise so a
-- math-free archive page keeps loading no typesetter.
backlinkMathField :: Context String
backlinkMathField = field "math" $ \item -> do
    sources <- footerEntries "backlinks" item
    if any hasMath sources
        then return "true"
        else noResult "no math in any backlink context"
  where
    hasMath s = any ("class=\"math" `isInfixOf`) [blSentence s, blParagraph s]

-- | Shared machinery for 'backlinksField' and 'referencedByField': the
-- page's sources from its footer file (build/FooterData.hs), sorted, handed
-- to the given renderer. Fails when there are none, so that the template's
-- @$if(…)$@ hides the section.
backlinksFieldWith :: ([BacklinkSource] -> String) -> String -> Context String
backlinksFieldWith renderSources name = field name $ \item -> do
    sources <- footerEntries "backlinks" item
    if null sources
        then fail "no backlinks"
        else return (renderSources (sortBy (comparing blTitle) sources))

-- ---------------------------------------------------------------------------
-- HTML rendering
-- ---------------------------------------------------------------------------

-- | Render backlink sources as an HTML list. Each item shows:
--   * the source title as a link (serif body face),
--   * a <blockquote> of the sentence containing the link (default context),
--   * a small hoverable "¶" affordance that reveals the full paragraph in
--     a CSS-driven popup when hovered or keyboard-focused.
--
-- 'blSentence' and 'blParagraph' are already HTML fragments produced by
-- the Pandoc writer, so they are emitted unescaped.
renderBacklinks :: [BacklinkSource] -> String
renderBacklinks sources =
    "<ul class=\"backlinks-list\">\n"
    ++ concatMap renderBacklinkItem (groupBySource sources)
    ++ "</ul>"

-- | Collect the sources that share a page into one list each, in order of
-- first appearance.
--
-- @data/backlinks.json@ holds one record per *link*, so a companion paper
-- citing this one from four different paragraphs arrived as four sibling
-- entries and printed its title four times over. Grouping is by URL, not
-- title: two distinct pages may legitimately share a title, and merging
-- those would attribute one page's sentence to the other.
--
-- The incoming list is already sorted by title, so this preserves that
-- order and is deterministic — the signing pipeline needs byte-identical
-- output from identical inputs. Quadratic in the number of sources, which
-- is a handful per page.
groupBySource :: [BacklinkSource] -> [[BacklinkSource]]
groupBySource sources =
    [ [ s | s <- sources, blUrl s == u ] | u <- nub (map blUrl sources) ]

-- | "Referenced by", grouped by the fragment each citation targets.
-- Sources citing the work with no fragment render first as a plain list;
-- each distinct fragment then gets its own subheading. With no fragments
-- anywhere (the common case) this collapses to exactly the flat list.
renderReferencedBy :: [BacklinkSource] -> String
renderReferencedBy sources =
    let (general, fragmented) = partition (null . blFragment) sources
        groups = Map.toList $ Map.fromListWith (flip (++))
                     [ (blFragment s, [s]) | s <- fragmented ]
    in  renderList general ++ concatMap renderGroup groups
  where
    renderList [] = ""
    renderList ss = "<ul class=\"backlinks-list\">\n"
                    ++ concatMap renderBacklinkItem (groupBySource ss)
                    ++ "</ul>\n"
    renderGroup (frag, ss) =
        "<div class=\"referenced-by-group\">"
        ++ "<h3 class=\"referenced-by-fragment\">"
        ++ escapeHtml (fragmentLabel frag) ++ "</h3>"
        ++ renderList ss
        ++ "</div>\n"

-- | Human label for a cited fragment: a PDF @#page=N@ becomes "Page N";
-- any other @#anchor@ is shown verbatim behind a section mark.
fragmentLabel :: String -> String
fragmentLabel frag =
    case stripPrefix "page=" frag of
        Just n  -> "Page " ++ n
        Nothing -> "\x00A7 " ++ frag

-- | One backlink @<li>@ per *source page*: the title as a link, then the
-- sentence around its first mention as a blockquote. When that page links
-- here more than once, the remaining mentions go inside a closed
-- @<details>@ so a page cited four times costs four lines instead of four
-- repetitions of its title.
--
-- The disclosure is plain @<details>/<summary>@, the same construct the
-- version-history "More" list uses: it needs no JavaScript, it is
-- keyboard-operable and announced by screen readers for free, and its
-- contents stay in the DOM while closed — which is what lets
-- @katex-bootstrap.js@ typeset the math in a hidden mention on load, and
-- lets in-page search find it.
--
-- 'blSentence' / 'blParagraph' are already HTML fragments from the Pandoc
-- writer, so they are emitted unescaped.
renderBacklinkItem :: [BacklinkSource] -> String
renderBacklinkItem [] = ""
renderBacklinkItem (bl : rest) =
    "<li class=\"backlink-item\">"
    ++ "<a class=\"backlink-source\" href=\""
    ++ escapeHtml (blUrl bl) ++ "\">"
    ++ escapeHtml (blTitle bl) ++ "</a>"
    ++ renderQuote bl
    ++ moreMentions
    ++ "</li>\n"
  where
    moreMentions
        | null rest = ""
        | otherwise =
            "<details class=\"backlink-more\">"
            ++ "<summary>" ++ escapeHtml summaryLabel ++ "</summary>"
            ++ concatMap renderQuote rest
            ++ "</details>"
    summaryLabel = case length rest of
        1 -> "1 more mention"
        n -> show n ++ " more mentions"

-- | The quoted sentence for one mention, with the @¶@ affordance that
-- reveals its full paragraph on hover or keyboard focus. Emits nothing
-- when the extractor found no sentence.
renderQuote :: BacklinkSource -> String
renderQuote bl
    | null (blSentence bl) = ""
    | otherwise =
        "<blockquote class=\"backlink-quote\">"
        ++ blSentence bl
        ++ paragraphAffordance
        ++ "</blockquote>"
  where
    paragraphAffordance
        | null (blParagraph bl)            = ""
        | blParagraph bl == blSentence bl  = ""
        | otherwise                        =
            "<span class=\"backlink-full\">"
            ++ "<button type=\"button\" class=\"backlink-full-trigger\""
            ++ " aria-label=\"Show full paragraph\" tabindex=\"0\">\x00B6</button>"
            ++ "<span class=\"backlink-full-popup\" role=\"tooltip\">"
            ++ blParagraph bl
            ++ "</span>"
            ++ "</span>"
