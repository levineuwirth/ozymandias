{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Backlinks with context: build-time computation of which pages link to
-- each page, including the paragraph that contains each link.
--
-- Architecture (dependency-correct, no circular deps):
--
--   1. Each content file is compiled under @version "links"@: a lightweight
--      pass that parses the source, walks the AST block-by-block, and for
--      every internal link records the URL *and* the HTML of its surrounding
--      paragraph.  The result is serialised as a JSON array of
--      @{url, context}@ objects.
--
--   2. A @create ["data/backlinks.json"]@ rule loads all "links" items,
--      inverts the map, and serialises
--      @target → [{url, title, abstract, context}]@ as JSON.
--
--   3. @backlinksField@ loads that JSON at page render time and injects
--      an HTML list showing each source's title and context paragraph.
--      The @load@ call establishes a proper Hakyll dependency so pages
--      recompile when backlinks change.
--
-- Dependency order (no cycles):
--   content "links" versions → data/backlinks.json → content default versions
module Backlinks
    ( backlinkRules
    , backlinksField
    ) where

import           Data.List                  (nubBy, sortBy)
import           Data.Ord                   (comparing)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Map.Strict            as Map
import           Data.Map.Strict            (Map)
import qualified Data.ByteString            as BS
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Encoding.Error   as TE
import qualified Data.Aeson                 as Aeson
import           Data.Aeson                 ((.=))
import           Text.Pandoc.Class          (runPure)
import           Text.Pandoc.Writers        (writeHtml5String)
import           Text.Pandoc.Definition     (Block (..), Inline (..), Pandoc (..),
                                             nullMeta)
import           Text.Pandoc.Options        (WriterOptions (..), HTMLMathMethod (..))
import           Text.Pandoc.Walk           (query)
import           Hakyll
import           Compilers                  (readerOpts, writerOpts)
import           Filters                    (preprocessSource)
import qualified Patterns                   as P

-- ---------------------------------------------------------------------------
-- Link-with-context entry (intermediate, saved by the "links" pass)
-- ---------------------------------------------------------------------------

data LinkEntry = LinkEntry
    { leUrl     :: T.Text   -- internal URL (as found in the AST)
    , leContext :: String   -- HTML of the surrounding paragraph
    } deriving (Show, Eq)

instance Aeson.ToJSON LinkEntry where
    toJSON e = Aeson.object ["url" .= leUrl e, "context" .= leContext e]

instance Aeson.FromJSON LinkEntry where
    parseJSON = Aeson.withObject "LinkEntry" $ \o ->
        LinkEntry <$> o Aeson..: "url" <*> o Aeson..: "context"

-- ---------------------------------------------------------------------------
-- Backlink source record (stored in data/backlinks.json)
-- ---------------------------------------------------------------------------

data BacklinkSource = BacklinkSource
    { blUrl      :: String
    , blTitle    :: String
    , blAbstract :: String
    , blContext  :: String   -- raw HTML of the paragraph containing the link
    } deriving (Show, Eq, Ord)

instance Aeson.ToJSON BacklinkSource where
    toJSON bl = Aeson.object
        [ "url"      .= blUrl bl
        , "title"    .= blTitle bl
        , "abstract" .= blAbstract bl
        , "context"  .= blContext bl
        ]

instance Aeson.FromJSON BacklinkSource where
    parseJSON = Aeson.withObject "BacklinkSource" $ \o ->
        BacklinkSource
            <$> o Aeson..: "url"
            <*> o Aeson..: "title"
            <*> o Aeson..: "abstract"
            <*> o Aeson..: "context"

-- ---------------------------------------------------------------------------
-- Writer options for context rendering
-- ---------------------------------------------------------------------------

-- | Minimal writer options for rendering paragraph context: no template
-- (fragment only), plain math fallback (context excerpts are previews, not
-- full renders, and KaTeX CSS may not be loaded on all target pages).
contextWriterOpts :: WriterOptions
contextWriterOpts = writerOpts
    { writerTemplate       = Nothing
    , writerHTMLMathMethod = PlainMath
    }

-- ---------------------------------------------------------------------------
-- Context extraction
-- ---------------------------------------------------------------------------

-- | URL filter: skip external links, pseudo-schemes, anchor-only fragments,
-- and static-asset paths.
isPageLink :: T.Text -> Bool
isPageLink u =
    not (T.isPrefixOf "http://"  u) &&
    not (T.isPrefixOf "https://" u) &&
    not (T.isPrefixOf "#"        u) &&
    not (T.isPrefixOf "mailto:"  u) &&
    not (T.isPrefixOf "tel:"     u) &&
    not (T.null u) &&
    not (hasStaticExt u)
  where
    staticExts = [".pdf",".svg",".png",".jpg",".jpeg",".webp",
                  ".mp3",".mp4",".woff2",".woff",".ttf",".ico",
                  ".json",".asc",".xml",".gz",".zip"]
    hasStaticExt x = any (`T.isSuffixOf` T.toLower x) staticExts

-- | Render a list of inlines to an HTML fragment string.
-- Uses Plain (not Para) to avoid a wrapping <p> — callers add their own.
renderInlines :: [Inline] -> String
renderInlines inlines =
    case runPure (writeHtml5String contextWriterOpts doc) of
        Left  _   -> ""
        Right txt -> T.unpack txt
  where
    doc = Pandoc nullMeta [Plain inlines]

-- | Extract @(internal-url, context-html)@ pairs from a Pandoc document.
-- Context is the HTML of the immediate surrounding paragraph.
-- Recurses into Div, BlockQuote, BulletList, and OrderedList.
extractLinksWithContext :: Pandoc -> [LinkEntry]
extractLinksWithContext (Pandoc _ blocks) = concatMap go blocks
  where
    go :: Block -> [LinkEntry]
    go (Para inlines)         = paraEntries inlines
    go (BlockQuote bs)        = concatMap go bs
    go (Div _ bs)             = concatMap go bs
    go (BulletList items)     = concatMap (concatMap go) items
    go (OrderedList _ items)  = concatMap (concatMap go) items
    go _                      = []

    -- For a Para block: find all internal links it contains, and for each
    -- return a LinkEntry with the paragraph's HTML as context.
    paraEntries :: [Inline] -> [LinkEntry]
    paraEntries inlines =
        let urls = filter isPageLink (query getUrl inlines)
        in  if null urls then []
            else
                let ctx = renderInlines inlines
                in  map (\u -> LinkEntry u ctx) urls

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
    body <- getResourceBody
    let src   = itemBody body
    let body' = itemSetBody (preprocessSource src) body
    pandocItem <- readPandocWith readerOpts body'
    let entries = nubBy (\a b -> leUrl a == leUrl b && leContext a == leContext b)
                        (extractLinksWithContext (itemBody pandocItem))
    makeItem . TL.unpack . TLE.decodeUtf8 . Aeson.encode $ entries

-- ---------------------------------------------------------------------------
-- URL normalisation
-- ---------------------------------------------------------------------------

-- | Normalise an internal URL as a map key: strip query string, fragment,
-- and trailing @.html@; ensure a leading slash; percent-decode the path
-- so that @\/essays\/caf%C3%A9@ and @\/essays\/café@ collide on the same
-- key.
normaliseUrl :: String -> String
normaliseUrl url =
    let t  = T.pack url
        t1 = fst (T.breakOn "?" (fst (T.breakOn "#" t)))
        t2 = if T.isPrefixOf "/" t1 then t1 else "/" `T.append` t1
        t3 = fromMaybe t2 (T.stripSuffix ".html" t2)
    in  percentDecode (T.unpack t3)

-- | Decode percent-escapes (@%XX@) into raw bytes, then re-interpret the
-- resulting bytestring as UTF-8. Invalid escapes are passed through
-- verbatim so this is safe to call on already-decoded input.
percentDecode :: String -> String
percentDecode = T.unpack . TE.decodeUtf8With lenientDecode . pack . go
  where
    go []                 = []
    go ('%':a:b:rest)
        | Just hi <- hexDigit a
        , Just lo <- hexDigit b
        = fromIntegral (hi * 16 + lo) : go rest
    go (c:rest)           = fromIntegral (fromEnum c) : go rest

    hexDigit c
        | c >= '0' && c <= '9' = Just (fromEnum c - fromEnum '0')
        | c >= 'a' && c <= 'f' = Just (fromEnum c - fromEnum 'a' + 10)
        | c >= 'A' && c <= 'F' = Just (fromEnum c - fromEnum 'A' + 10)
        | otherwise            = Nothing

    pack = BS.pack
    lenientDecode = TE.lenientDecode

-- ---------------------------------------------------------------------------
-- Content patterns (must match the rules in Site.hs — sourced from
-- Patterns.allContent so additions to the canonical list automatically
-- propagate to backlinks).
-- ---------------------------------------------------------------------------

allContent :: Pattern
allContent = P.allContent

-- ---------------------------------------------------------------------------
-- Hakyll rules
-- ---------------------------------------------------------------------------

-- | Register the @version "links"@ rules for all content and the
-- @create ["data/backlinks.json"]@ rule.  Call this from 'Site.rules'.
backlinkRules :: Rules ()
backlinkRules = do
    -- Pass 1: extract links + context from each content file.
    match allContent $ version "links" $
        compile linksCompiler

    -- Pass 2: invert the map and write the backlinks JSON.
    create ["data/backlinks.json"] $ do
        route   idRoute
        compile $ do
            items <- loadAll (allContent .&&. hasVersion "links")
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
    let srcUrl   = maybe "" (\r -> "/" ++ r) mRoute
    let title    = fromMaybe "(untitled)" (lookupString "title" meta)
    let abstract = fromMaybe "" (lookupString "abstract" meta)
    case mRoute of
        Nothing -> return []
        Just _  ->
            case Aeson.decodeStrict (TE.encodeUtf8 (T.pack (itemBody item)))
                    :: Maybe [LinkEntry] of
                Nothing      -> return []
                Just entries ->
                    return [ ( T.pack (normaliseUrl (T.unpack (leUrl e)))
                             , BacklinkSource srcUrl title abstract (leContext e)
                             )
                           | e <- entries ]

-- ---------------------------------------------------------------------------
-- Context field
-- ---------------------------------------------------------------------------

-- | Context field @$backlinks$@ that injects an HTML list of pages that link
-- to the current page, each with its paragraph context.
-- Returns @noResult@ (so @$if(backlinks)$@ is false) when there are none.
backlinksField :: Context String
backlinksField = field "backlinks" $ \item -> do
    blItem <- load (fromFilePath "data/backlinks.json") :: Compiler (Item String)
    case Aeson.decodeStrict (TE.encodeUtf8 (T.pack (itemBody blItem)))
            :: Maybe (Map T.Text [BacklinkSource]) of
        Nothing    -> fail "backlinks: could not parse data/backlinks.json"
        Just blMap -> do
            mRoute <- getRoute (itemIdentifier item)
            case mRoute of
                Nothing -> fail "backlinks: item has no route"
                Just r  ->
                    let key     = T.pack (normaliseUrl ("/" ++ r))
                        sources = fromMaybe [] (Map.lookup key blMap)
                        sorted  = sortBy (comparing blTitle) sources
                    in  if null sorted
                        then fail "no backlinks"
                        else return (renderBacklinks sorted)

-- ---------------------------------------------------------------------------
-- HTML rendering
-- ---------------------------------------------------------------------------

-- | Render backlink sources as an HTML list.
-- Each item shows the source title as a link (always visible) and a
-- <details> element containing the context paragraph (collapsed by default).
-- @blContext@ is already HTML produced by the Pandoc writer — not escaped.
renderBacklinks :: [BacklinkSource] -> String
renderBacklinks sources =
    "<ul class=\"backlinks-list\">\n"
    ++ concatMap renderOne sources
    ++ "</ul>"
  where
    renderOne bl =
        "<li class=\"backlink-item\">"
        ++ "<a class=\"backlink-source\" href=\"" ++ escapeHtml (blUrl bl) ++ "\">"
        ++ escapeHtml (blTitle bl) ++ "</a>"
        ++ ( if null (blContext bl) then ""
             else "<details class=\"backlink-details\">"
                  ++ "<summary class=\"backlink-summary\">context</summary>"
                  ++ "<div class=\"backlink-context\">" ++ blContext bl ++ "</div>"
                  ++ "</details>" )
        ++ "</li>\n"
