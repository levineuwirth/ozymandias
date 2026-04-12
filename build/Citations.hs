{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Citation processing pipeline.
--
--   Steps:
--   1. Skip if the document contains no Cite nodes and frKeys is empty.
--   2. Inject default bibliography / CSL metadata if absent.
--   3. Inject nocite entries for further-reading keys.
--   4. Run Pandoc's citeproc to resolve references and generate bibliography.
--   5. Walk the AST and replace Cite nodes with numbered superscripts.
--   6. Extract the citeproc bibliography div from the body, reorder by
--      first-appearance, split into cited / further-reading sections,
--      and render to an HTML string for the template's $bibliography$ field.
--
--   Returns (Pandoc without refs div, bibliography HTML).
--   The bibliography HTML is empty when there are no citations.
--
--   NOTE: processCitations with in-text CSL leaves Cite nodes as Cite nodes
--   in the AST — it only populates their inline content and creates the refs
--   div. The HTML writer later wraps them in <span class="citation">. We must
--   therefore match Cite nodes (not Span nodes) in our transform pass.
--
--   NOTE: Hakyll strips YAML frontmatter before passing to readPandocWith, so
--   the Pandoc Meta is empty. further-reading keys are passed explicitly by the
--   caller (read from Hakyll's own metadata via lookupStringList).
--
--   NOTE: Does not import Contexts to avoid cycles.
module Citations (applyCitations) where

import           Data.List           (intercalate, nub, partition, sortBy)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (fromMaybe, mapMaybe)
import           Data.Ord            (comparing)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Pandoc
import           Text.Pandoc.Citeproc (processCitations)
import           Text.Pandoc.Walk


-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Process citations in a Pandoc document.
--   @frKeys@: further-reading citation keys (read from Hakyll metadata by
--   the caller, since Hakyll strips YAML frontmatter before parsing).
--   Returns @(body, citedHtml, furtherHtml)@ where @body@ has Cite nodes
--   replaced with numbered superscripts and no bibliography div,
--   @citedHtml@ is the inline-cited references HTML, and @furtherHtml@ is
--   the further-reading-only references HTML (each empty when absent).
applyCitations :: [Text] -> Text -> Pandoc -> IO (Pandoc, Text, Text)
applyCitations frKeys bibPath doc
    | not (hasCitations frKeys doc) = return (doc, "", "")
    | otherwise = do
        let doc1      = injectMeta frKeys bibPath doc
        processed    <- runIOorExplode $ processCitations doc1
        let (body, citedHtml, furtherHtml) = transformAndExtract frKeys processed
        return (body, citedHtml, furtherHtml)


-- ---------------------------------------------------------------------------
-- Detection
-- ---------------------------------------------------------------------------

-- | True if the document has inline [@key] cites or a further-reading list.
hasCitations :: [Text] -> Pandoc -> Bool
hasCitations frKeys doc =
    not (null (query collectCites doc))
    || not (null frKeys)
  where
    collectCites (Cite {}) = [()]
    collectCites _         = []


-- ---------------------------------------------------------------------------
-- Metadata injection
-- ---------------------------------------------------------------------------

-- | Inject default bibliography / CSL paths and nocite for further-reading.
injectMeta :: [Text] -> Text -> Pandoc -> Pandoc
injectMeta frKeys bibPath (Pandoc meta blocks) =
    let meta1  = if null frKeys then meta
                 else insertMeta "nocite" (nociteVal frKeys) meta
        meta2  = case lookupMeta "bibliography" meta1 of
                   Nothing -> insertMeta "bibliography"
                                (MetaString bibPath) meta1
                   Just _  -> meta1
        meta3  = case lookupMeta "csl" meta2 of
                   Nothing -> insertMeta "csl"
                                (MetaString "data/chicago-notes.csl") meta2
                   Just _  -> meta2
    in Pandoc meta3 blocks
  where
    -- Each key becomes its own Cite node (matching what pandoc parses from
    -- nocite: "@key1 @key2" in YAML frontmatter).
    nociteVal keys = MetaInlines (intercalate [Space] (map mkCiteNode keys))
    mkCiteNode k   = [Cite [Citation k [] [] AuthorInText 1 0] [Str ("@" <> k)]]

-- | Insert a key/value pair into Pandoc Meta.
insertMeta :: Text -> MetaValue -> Meta -> Meta
insertMeta k v (Meta m) = Meta (Map.insert k v m)


-- ---------------------------------------------------------------------------
-- Transform pass
-- ---------------------------------------------------------------------------

-- | Number citation Cite nodes and extract the bibliography div.
transformAndExtract :: [Text] -> Pandoc -> (Pandoc, Text, Text)
transformAndExtract frKeys doc@(Pandoc meta _) =
    let citeOrder = collectCiteOrder doc     -- keys, first-appearance order
        keyNums   = Map.fromList (zip citeOrder [1 :: Int ..])
        -- Replace Cite nodes with numbered superscript markers
        doc'      = walk (transformInline keyNums) doc
        -- Pull bibliography div out of body and render to HTML
        (bodyBlocks, citedHtml, furtherHtml) = extractBibliography citeOrder frKeys
                                                 (pandocBlocks doc')
    in (Pandoc meta bodyBlocks, citedHtml, furtherHtml)
  where
    pandocBlocks (Pandoc _ bs) = bs

-- | Collect citation keys in order of first appearance (body only).
--   NOTE: after processCitations, Cite nodes remain as Cite in the AST;
--   they are not converted to Span nodes with in-text CSL.
--   We query only blocks (not metadata) so that nocite Cite nodes injected
--   into the 'nocite' meta field are not mistakenly treated as inline citations.
collectCiteOrder :: Pandoc -> [Text]
collectCiteOrder (Pandoc _ blocks) = nub (query extractKeys blocks)
  where
    extractKeys (Cite citations _) = map citationId citations
    extractKeys _                  = []

-- | Replace a Cite node with a numbered superscript marker.
transformInline :: Map Text Int -> Inline -> Inline
transformInline keyNums (Cite citations _) =
    let keys = map citationId citations
        nums = mapMaybe (`Map.lookup` keyNums) keys
    in case (keys, nums) of
        -- Both lists are guaranteed non-empty by the @null nums@ check
        -- below, but pattern-match to keep this total instead of
        -- relying on @head@.
        (firstKey : _, firstNum : _) ->
            RawInline "html" (markerHtml keys firstKey firstNum nums)
        _ ->
            Str ""
transformInline _ x = x

markerHtml :: [Text] -> Text -> Int -> [Int] -> Text
markerHtml keys firstKey firstNum nums =
    let label   = "[" <> T.intercalate "," (map tshow nums) <> "]"
        allIds  = T.intercalate " " (map ("ref-" <>) keys)
    in "<sup class=\"cite-marker\" id=\"cite-back-" <> tshow firstNum <> "\">"
    <> "<a href=\"#ref-" <> firstKey <> "\" class=\"cite-link\""
    <> " data-cite-keys=\"" <> allIds <> "\">"
    <> label <> "</a></sup>"
  where tshow = T.pack . show


-- ---------------------------------------------------------------------------
-- Bibliography extraction + rendering
-- ---------------------------------------------------------------------------

-- | Separate the @refs@ div from body blocks and render it to HTML.
--   Returns @(bodyBlocks, citedHtml, furtherHtml)@.
extractBibliography :: [Text] -> [Text] -> [Block] -> ([Block], Text, Text)
extractBibliography citeOrder frKeys blocks =
    let (bodyBlocks, refDivs) = partition (not . isRefsDiv) blocks
        (citedHtml, furtherHtml) = case refDivs of
            []    -> ("", "")
            (d:_) -> renderBibDiv citeOrder frKeys d
    in (bodyBlocks, citedHtml, furtherHtml)
  where
    isRefsDiv (Div ("refs", _, _) _) = True
    isRefsDiv _                       = False

-- | Render the citeproc @refs@ Div into two HTML strings:
--   @(citedHtml, furtherHtml)@ — each is empty when there are no entries
--   in that section. Headings are rendered in the template, not here.
renderBibDiv :: [Text] -> [Text] -> Block -> (Text, Text)
renderBibDiv citeOrder _frKeys (Div _ children) =
    let keyIndex = Map.fromList (zip citeOrder [0 :: Int ..])
        (citedEntries, furtherEntries) =
            partition (isCited keyIndex) children
        sorted   = sortBy (comparing (entryOrder keyIndex)) citedEntries
        numbered = zipWith addNumber [1..] sorted
        citedHtml  = renderEntries "csl-bib-body cite-refs" numbered
        furtherHtml
            | null furtherEntries = ""
            | otherwise           = renderEntries "csl-bib-body further-reading-refs" furtherEntries
    in (citedHtml, furtherHtml)
renderBibDiv _ _ _ = ("", "")

isCited :: Map Text Int -> Block -> Bool
isCited keyIndex (Div (rid, _, _) _) = Map.member (stripRefPrefix rid) keyIndex
isCited _        _                    = False

entryOrder :: Map Text Int -> Block -> Int
entryOrder keyIndex (Div (rid, _, _) _) =
    fromMaybe maxBound $ Map.lookup (stripRefPrefix rid) keyIndex
entryOrder _ _ = maxBound

-- | Prepend [N] marker to a bibliography entry block.
addNumber :: Int -> Block -> Block
addNumber n (Div attrs@(divId, _, _) content) =
    Div attrs
        ( Plain [ RawInline "html"
                    ("<a class=\"ref-num\" href=\"#" <> divId <> "\">[" <> T.pack (show n) <> "]</a>") ]
          : content )
addNumber _ b = b

-- | Strip the @ref-@ prefix that citeproc adds to div IDs.
stripRefPrefix :: Text -> Text
stripRefPrefix t = fromMaybe t (T.stripPrefix "ref-" t)

-- | Render a list of blocks as an HTML string (used for bibliography sections).
renderEntries :: Text -> [Block] -> Text
renderEntries cls entries =
    case runPure (writeHtml5String wOpts (Pandoc nullMeta entries)) of
        Left  _    -> ""
        Right html -> "<div class=\"" <> cls <> "\">\n" <> html <> "</div>\n"
  where
    wOpts = def { writerWrapText = WrapNone }
