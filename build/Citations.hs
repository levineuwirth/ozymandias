{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
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
module Citations
    ( applyCitations
      -- * For synthetic bibliography pages (Phase 6b)
    , renderBibliographyHtml
      -- * The style, for dependency tracking
    , cslStyles
    ) where

import           Control.Monad.State.Strict (State, gets, modify', runState)
import           Data.List           (intercalate, intersperse, nub, partition, sortBy)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (fromMaybe, mapMaybe)
import           Data.Ord            (comparing)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Pandoc
import           Text.Pandoc.Citeproc (processCitations)
import           Text.Pandoc.Walk

import           Hakyll              (Pattern, (.||.))

import           BibExtras           (BibExtra (..), emptyBibExtra, parseBibExtras)
import           EngineFiles         (resolveFile)
import qualified Filters.Archive     (annotateBlock)
import qualified Filters.Links       (pdfViewerTarget)


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
        -- Read custom fields (@file:@, @keywords:@) from the .bib file
        -- in parallel with citeproc. These don't affect citation
        -- resolution — they enhance the rendered bibliography entries.
        extras       <- parseBibExtras (T.unpack bibPath)
        csl          <- cslStyle
        let doc1      = injectMeta csl frKeys bibPath doc
        processed    <- runIOorExplode $ processCitations doc1
        let (body, citedHtml, furtherHtml) = transformAndExtract extras frKeys processed
        return (body, citedHtml, furtherHtml)

-- | Render a standalone bibliography section from a list of citekeys and
--   a set of @.bib@ file paths. Used by the synthetic @\/bibliography\/@
--   pages (Phase 6b) to produce CSL-formatted entries outside of any
--   essay's citation context.
--
--   Given citekeys are passed to citeproc via a synthesized @nocite@
--   metadata entry on an otherwise empty document; citeproc emits a
--   @refs@ Div whose children are the rendered entries. We then reorder
--   the children to match the caller-supplied @keys@ list (citeproc's
--   own ordering is overridden so callers control sort), enhance each
--   entry with the Phase 6a PDF-link and keyword-strip hooks, and
--   render to HTML wrapped in @\<div class="csl-bib-body"\>@.
--
--   @extras@ is the combined 'BibExtra' map for the same @.bib@ files;
--   passed in so that 'enhanceEntry' can consult @file:@ and
--   @keywords:@ without each entry re-parsing the files.
renderBibliographyHtml :: [FilePath]            -- ^ .bib paths
                      -> Map String BibExtra    -- ^ enhancement map
                      -> [String]               -- ^ citekeys, in desired order
                      -> IO Text
renderBibliographyHtml _ _ [] = return ""
renderBibliographyHtml bibPaths extras keys = do
    csl <- cslStyle
    let doc = synthesizeNociteDoc csl bibPaths keys
    processed <- runIOorExplode $ processCitations doc
    let refsDivs = concatMap unwrapRefs (pandocBlocks processed)
        ordered  = reorderByKeys keys refsDivs
        enhanced = map (annotateArchive . enhanceEntry extras) ordered
    return (renderEntries "csl-bib-body" enhanced)
  where
    pandocBlocks (Pandoc _ bs) = bs
    unwrapRefs (Div ("refs", _, _) children) = children
    unwrapRefs _                              = []

-- | Build a Pandoc doc whose only citation-relevant content is a
--   @nocite@ metadata entry listing every supplied citekey. Runs
--   through 'processCitations' to emit a fully-formatted @refs@ Div
--   containing every entry.
synthesizeNociteDoc :: Text -> [FilePath] -> [String] -> Pandoc
synthesizeNociteDoc csl bibPaths keys =
    let meta = Meta $ Map.fromList
            [ ("bibliography", bibPathMeta bibPaths)
            , ("csl",          MetaString csl)
            , ("nocite",       nociteVal (map T.pack keys))
            ]
    in Pandoc meta []
  where
    bibPathMeta [p] = MetaString (T.pack p)
    bibPathMeta ps  = MetaList (map (MetaString . T.pack) ps)

    nociteVal ks = MetaInlines (intercalate [Space] (map mkCite ks))
    mkCite k     = [Cite [Citation k [] [] AuthorInText 1 0] [Str ("@" <> k)]]

-- | Reorder a list of @csl-entry@ Divs to match a requested key order.
--   Divs not in the key list (shouldn't happen in practice, but safe
--   by construction) drop to the end in their original order.
reorderByKeys :: [String] -> [Block] -> [Block]
reorderByKeys keys divs =
    let divMap = Map.fromList [ (T.unpack (stripRefPrefix d), blk)
                              | blk@(Div (d, _, _) _) <- divs ]
        found = mapMaybe (`Map.lookup` divMap) keys
        leftovers = filter (\blk -> case blk of
                               Div (d, _, _) _ ->
                                   T.unpack (stripRefPrefix d) `notElem` keys
                               _ -> True) divs
    in found ++ leftovers


-- ---------------------------------------------------------------------------
-- The style
-- ---------------------------------------------------------------------------

-- | The CSL style a page gets unless it names one: the site's, else the
--   engine's ("EngineFiles").
cslStyle :: IO Text
cslStyle = T.pack <$> resolveFile "data/chicago-notes.csl"

-- | Every CSL style the site and the engine have. Citeproc reads them
--   outside Hakyll, so the compilers load these to depend on them.
cslStyles :: Pattern
cslStyles = "data/*.csl" .||. "engine/data/*.csl"

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
injectMeta :: Text -> [Text] -> Text -> Pandoc -> Pandoc
injectMeta csl frKeys bibPath (Pandoc meta blocks) =
    let meta1  = if null frKeys then meta
                 else insertMeta "nocite" (nociteVal frKeys) meta
        meta2  = case lookupMeta "bibliography" meta1 of
                   Nothing -> insertMeta "bibliography"
                                (MetaString bibPath) meta1
                   Just _  -> meta1
        meta3  = case lookupMeta "csl" meta2 of
                   Nothing -> insertMeta "csl"
                                (MetaString csl) meta2
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
transformAndExtract :: Map String BibExtra -> [Text] -> Pandoc -> (Pandoc, Text, Text)
transformAndExtract extras frKeys doc@(Pandoc meta _) =
    let citeOrder = collectCiteOrder doc     -- keys, first-appearance order
        keyNums   = Map.fromList (zip citeOrder [1 :: Int ..])
        -- Replace Cite nodes with numbered superscript markers. The walk
        -- is stateful because each *occurrence* needs its own id (see
        -- 'MarkerState'); it also records where each key was first cited
        -- so the bibliography can link back to that occurrence.
        --
        -- Blocks only, like 'collectCiteOrder'. The further-reading keys
        -- sit in the metadata as nocite Cite nodes, and a walk over the
        -- whole document visits them first: a key both cited and listed
        -- spent its cite-back-<n> id on a node that is never rendered, so
        -- the bibliography's back-link led nowhere.
        (blocks', st) = runState (walkM (transformInline keyNums) (pandocBlocks doc))
                                 emptyMarkerState
        backMap    = msFirstOccurrence st
        -- Pull bibliography div out of body and render to HTML
        (bodyBlocks, citedHtml, furtherHtml) = extractBibliography extras backMap citeOrder frKeys
                                                 blocks'
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

-- | State threaded through the marker-rewriting walk.
--
--   @msOccurrences@ counts how many markers have already been emitted
--   for a given leading citation number, so the /n/th repeat of a
--   reference can be given its own id. @msFirstOccurrence@ records, for
--   every key in every cluster, the id of the marker where that key was
--   first cited — the target the bibliography's return link points at.
data MarkerState = MarkerState
    { msOccurrences     :: !(Map Int Int)
    , msFirstOccurrence :: !(Map Text Text)
    }

emptyMarkerState :: MarkerState
emptyMarkerState = MarkerState Map.empty Map.empty

-- | Replace a Cite node with a numbered superscript marker.
--
--   Every occurrence gets a unique @id@: the first citation of
--   reference /n/ keeps the stable @cite-back-\<n\>@ anchor, and each
--   repeat gets @cite-back-\<n\>-\<k\>@ (k from 2). Reference ids
--   (@ref-\<key\>@, emitted by citeproc on the bibliography entries) are
--   untouched — the two id spaces are disjoint because a first-occurrence
--   id never contains a hyphen after the number.
transformInline :: Map Text Int -> Inline -> State MarkerState Inline
transformInline keyNums (Cite citations _) = do
    let keys = map citationId citations
        nums = mapMaybe (`Map.lookup` keyNums) keys
    case (keys, nums) of
        -- Both lists are guaranteed non-empty by the @null nums@ check
        -- below, but pattern-match to keep this total instead of
        -- relying on @head@.
        (firstKey : _, firstNum : _) -> do
            seen <- gets (fromMaybe 0 . Map.lookup firstNum . msOccurrences)
            let occ      = seen + 1
                markerId = backAnchorId firstNum occ
            modify' $ \st -> st
                { msOccurrences = Map.insert firstNum occ (msOccurrences st)
                  -- insertWith keeps the value already there, so the
                  -- earliest occurrence wins for every key in the cluster.
                , msFirstOccurrence =
                    foldr (\k m -> Map.insertWith (\_ old -> old) k markerId m)
                          (msFirstOccurrence st) keys
                }
            return $ RawInline "html" (markerHtml keys markerId firstKey nums)
        _ ->
            return (Str "")
transformInline _ x = return x

-- | Id for the /occ/th marker of reference number /n/.
backAnchorId :: Int -> Int -> Text
backAnchorId n occ
    | occ <= 1  = base
    | otherwise = base <> "-" <> T.pack (show occ)
  where base = "cite-back-" <> T.pack (show n)

markerHtml :: [Text] -> Text -> Text -> [Int] -> Text
markerHtml keys markerId firstKey nums =
    let label   = "[" <> T.intercalate "," (map tshow nums) <> "]"
        allIds  = T.intercalate " " (map ("ref-" <>) keys)
    in "<sup class=\"cite-marker\" id=\"" <> markerId <> "\">"
    <> "<a href=\"#ref-" <> firstKey <> "\" class=\"cite-link\""
    <> " data-cite-keys=\"" <> allIds <> "\">"
    <> label <> "</a></sup>"
  where tshow = T.pack . show


-- ---------------------------------------------------------------------------
-- Bibliography extraction + rendering
-- ---------------------------------------------------------------------------

-- | Separate the @refs@ div from body blocks and render it to HTML.
--   Returns @(bodyBlocks, citedHtml, furtherHtml)@.
extractBibliography :: Map String BibExtra -> Map Text Text -> [Text] -> [Text] -> [Block]
                    -> ([Block], Text, Text)
extractBibliography extras backMap citeOrder frKeys blocks =
    let (bodyBlocks, refDivs) = partition (not . isRefsDiv) blocks
        (citedHtml, furtherHtml) = case refDivs of
            []    -> ("", "")
            (d:_) -> renderBibDiv extras backMap citeOrder frKeys d
    in (bodyBlocks, citedHtml, furtherHtml)
  where
    isRefsDiv (Div ("refs", _, _) _) = True
    isRefsDiv _                       = False

-- | Render the citeproc @refs@ Div into two HTML strings:
--   @(citedHtml, furtherHtml)@ — each is empty when there are no entries
--   in that section. Headings are rendered in the template, not here.
--
--   Entry bodies are enhanced before numbering: title-wrapped as a
--   @.pdf-link[data-pdf-src]@ when the .bib @file:@ field is set (so
--   popups.js's PDF hover preview fires), and a trailing
--   @\<div class="bib-keywords"\>@ appended when @keywords:@ is set.
renderBibDiv :: Map String BibExtra -> Map Text Text -> [Text] -> [Text] -> Block -> (Text, Text)
renderBibDiv extras backMap citeOrder _frKeys (Div _ children) =
    let enhanced = map (annotateArchive . enhanceEntry extras) children
        keyIndex = Map.fromList (zip citeOrder [0 :: Int ..])
        (citedEntries, furtherEntries) =
            partition (isCited keyIndex) enhanced
        sorted   = sortBy (comparing (entryOrder keyIndex)) citedEntries
        numbered = zipWith (addNumber backMap) [1..] sorted
        citedHtml  = renderEntries "csl-bib-body cite-refs" numbered
        furtherHtml
            | null furtherEntries = ""
            | otherwise           = renderEntries "csl-bib-body further-reading-refs" furtherEntries
    in (citedHtml, furtherHtml)
renderBibDiv _ _ _ _ _ = ("", "")


-- ---------------------------------------------------------------------------
-- Bib entry enhancement (Phase 6a)
-- ---------------------------------------------------------------------------

-- | Bibliography-side archive annotation: the same affordance (and
--   rotted-link flip) 'Filters.Archive' gives body links, applied to a
--   rendered CSL entry. The bibliography never travels the body filter
--   chain — it is extracted and rendered to an HTML string for the
--   template's @$bibliography$@ field — so the entry Divs are annotated
--   here, after 'enhanceEntry' (whose PDF-link title wrap must see the
--   entry's original inline shape).
annotateArchive :: Block -> Block
annotateArchive = Filters.Archive.annotateBlock

-- | Augment a single @csl-entry@ Div with the custom fields we parsed
--   from the .bib file. Other Blocks pass through unchanged.
enhanceEntry :: Map String BibExtra -> Block -> Block
enhanceEntry extras b@(Div attrs@(divId, _, _) blocks) =
    let key    = T.unpack (stripRefPrefix divId)
        extra  = fromMaybe emptyBibExtra (Map.lookup key extras)
        withLink = case bibFile extra of
            Nothing -> blocks
            Just fp -> map (wrapFirstTitleBlock (T.pack fp)) blocks
        withKw   = withLink ++ keywordsBlocks (bibKeywords extra)
    in case (bibFile extra, bibKeywords extra) of
        (Nothing, []) -> b
        _             -> Div attrs withKw
enhanceEntry _ b = b

-- | In one block of an entry, wrap the first title-bearing inline
--   with a @.pdf-link@ anchor. Pandoc's CSL-formatted references
--   render the title as either a @Quoted@ (article titles in
--   Chicago-notes: "Paper Title") or an @Emph@ (book titles:
--   /Book Title/), and those are the first such inline in each
--   entry. We wrap at the block level and fall back to passing the
--   block through if no matching inline appears.
wrapFirstTitleBlock :: Text -> Block -> Block
wrapFirstTitleBlock href = \case
    Para  ils -> Para  (wrapFirstTitle href ils)
    Plain ils -> Plain (wrapFirstTitle href ils)
    other     -> other

-- | Left-to-right scan: wrap the first title-bearing inline in a link
--   pointing at the PDF. Pandoc's CSL renderer emits article titles as
--   @Span@ nodes (whose rendered HTML wraps quotation marks around the
--   title text) and book titles as @Emph@; @Quoted@ appears in some
--   other CSL styles. First match of any of these is treated as the
--   title; subsequent ones pass through — journal names are also
--   @Emph@ on @\@article@ entries but come after the @Span@ title, so
--   the article case picks the right target.
wrapFirstTitle :: Text -> [Inline] -> [Inline]
wrapFirstTitle href inls = reverse . fst $ foldl step ([], False) inls
  where
    step (acc, True)  inl = (inl:acc, True)
    step (acc, False) inl = case inl of
        Span _ _   -> (asPdfLink href [inl] : acc, True)
        Quoted _ _ -> (asPdfLink href [inl] : acc, True)
        Emph   _   -> (asPdfLink href [inl] : acc, True)
        _          -> (inl:acc, False)

-- | Build the @.pdf-link[data-pdf-src]@ anchor that popups.js binds to.
--   A hosted PDF opens in the PDF.js viewer, as a body link to one does
--   ('Filters.Links.pdfViewerTarget'); any other @file:@ is linked as is.
asPdfLink :: Text -> [Inline] -> Inline
asPdfLink href content =
    let (url, src) = fromMaybe (href, href) (Filters.Links.pdfViewerTarget href)
    in  Link ("", ["pdf-link"], [("data-pdf-src", src)]) content (url, "")

-- | Trailing keyword strip, linking each keyword to the future
--   @/bibliography/\<keyword\>/@ page. Returns @[]@ when the keyword
--   list is empty so the entry gets no extra block at all.
keywordsBlocks :: [String] -> [Block]
keywordsBlocks [] = []
keywordsBlocks ks =
    [ Div ("", ["bib-keywords"], [])
          [Plain (intersperse (Str ", ") (map keywordLink ks))]
    ]
  where
    keywordLink k =
        Link ("", ["bib-keyword"], [])
             [Str (T.pack k)]
             (T.pack ("/bibliography/" ++ k ++ "/"), "")

isCited :: Map Text Int -> Block -> Bool
isCited keyIndex (Div (rid, _, _) _) = Map.member (stripRefPrefix rid) keyIndex
isCited _        _                    = False

entryOrder :: Map Text Int -> Block -> Int
entryOrder keyIndex (Div (rid, _, _) _) =
    fromMaybe maxBound $ Map.lookup (stripRefPrefix rid) keyIndex
entryOrder _ _ = maxBound

-- | Prepend the @[N]@ marker to a bibliography entry block.
--
--   The marker is the entry's /return link/: it points at the first
--   occurrence of that reference in the body (recorded during the marker
--   walk), so a reader who has jumped down to the bibliography can get
--   back to where the citation was made. Entries with no recorded
--   occurrence — further-reading-only keys, or an entry citeproc emitted
--   that the body never cites — keep the previous self-link, which is
--   inert but valid.
addNumber :: Map Text Text -> Int -> Block -> Block
addNumber backMap n (Div attrs@(divId, _, _) content) =
    Div attrs
        ( Plain [ RawInline "html"
                    ("<a class=\"ref-num\" href=\"#" <> target <> "\">[" <> T.pack (show n) <> "]</a>") ]
          : content )
  where
    target = fromMaybe divId (Map.lookup (stripRefPrefix divId) backMap)
addNumber _ _ b = b

-- | Strip the @ref-@ prefix that citeproc adds to div IDs.
stripRefPrefix :: Text -> Text
stripRefPrefix t = fromMaybe t (T.stripPrefix "ref-" t)

-- | Render a list of blocks as an HTML string (used for bibliography sections).
--
--   The container carries @role="list"@. Pandoc's HTML writer stamps
--   @role="listitem"@ on every @.csl-entry@ Div, and an element with that
--   role must have a list-role parent (WAI-ARIA @aria-required-parent@;
--   axe flags the orphaned form). Pandoc emits the pairing itself when it
--   writes a whole @refs@ Div, but we extract the entries and re-wrap
--   them here, so the container role has to be re-established — for the
--   cited-references body, the further-reading body, and the synthetic
--   @\/bibliography\/@ pages alike, all three of which come through this
--   one function.
--   Math in an entry title (@$k$@-dominating set, @$d$@-regular graph)
--   is rendered here, at build time, as MathML (audit C07).
--
--   The default writer method emits @\<span class="math inline"\>@ holding
--   the LaTeX source, which is a promise that a client-side renderer will
--   arrive. On an essay it does: @essayCtx@ sets @math: true@ and KaTeX
--   loads. @\/bibliography\/@ is generated from @siteCtx@ and sets no such
--   flag, so its entries carried unrendered notation with nothing coming
--   to render it. MathML needs no runtime at all, which is the right
--   answer for a reference list: correct without JavaScript, and it does
--   not pull a 300 KB typesetter onto a page whose only mathematics is one
--   italic letter.
--
--   'unclaimMathSpans' is a guard rather than a fix: Pandoc's MathML
--   writer emits a bare @\<math\>@ element with no wrapper, so on the
--   current corpus it matches nothing. It exists because
--   @katex-bootstrap.js@ renders every @.math@ element's /text content/,
--   and on an essay page (which does load KaTeX) a wrapper left behind by
--   some future writer change would be typeset a second time, from the
--   MathML's own text.
renderEntries :: Text -> [Block] -> Text
renderEntries cls entries =
    case runPure (writeHtml5String wOpts (Pandoc nullMeta entries)) of
        Left  _    -> ""
        Right html -> "<div class=\"" <> cls <> "\" role=\"list\">\n"
                        <> unclaimMathSpans html <> "</div>\n"
  where
    wOpts = def { writerWrapText       = WrapNone
                , writerHTMLMathMethod = MathML
                }

-- | Rename Pandoc's @math@ wrapper class on already-rendered MathML so the
--   client-side KaTeX bootstrap does not treat it as unrendered source.
--   The @inline@ / @display@ half is kept for anyone styling it.
unclaimMathSpans :: Text -> Text
unclaimMathSpans =
      T.replace "class=\"math inline\""  "class=\"math-rendered inline\""
    . T.replace "class=\"math display\"" "class=\"math-rendered display\""
