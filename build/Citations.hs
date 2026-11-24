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
    , plainCitations
      -- * For synthetic bibliography pages (Phase 6b)
    , renderBibliographyHtml
      -- * The style, for dependency tracking
    , cslStyles
    ) where

import           Control.Monad.State.Strict (State, gets, modify', runState)
import           Data.Data           (Data, cast, gmapM)
import           Data.List           (intercalate, intersperse, nub, partition, sortBy)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import           Data.Maybe          (fromMaybe, mapMaybe)
import           Data.Ord            (comparing)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Pandoc
import           Text.Pandoc.Citeproc (processCitations)
import           Text.Pandoc.Walk
import qualified Text.HTML.TagSoup   as TS

import           Hakyll              (Pattern, (.||.))

import           BibExtras           (BibExtra (..), emptyBibExtra, parseBibExtras)
import           Config              (SiteConfig, switchOn)
import           EngineFiles         (resolveFile)
import qualified Filters.Archive     (annotateBlock)
import qualified Filters.Links       (pdfViewerTarget)


-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | A page's citations without the bibliography switch: each as its author
--   wrote it, plain text, with no marker and no list. Left as they are they
--   would render with the class the artifact gate refuses (a citation
--   showing its source syntax).
plainCitations :: Pandoc -> Pandoc
plainCitations = walk (concatMap plain)
  where
    plain :: Inline -> [Inline]
    plain (Cite _ written) = written
    plain inline           = [inline]

-- | Process citations in a Pandoc document.
--   @frKeys@: further-reading citation keys (read from Hakyll metadata by
--   the caller, since Hakyll strips YAML frontmatter before parsing).
--   Returns @(body, citedHtml, furtherHtml, visibleKeys)@ where @body@ has Cite nodes
--   replaced with numbered superscripts and no bibliography div,
--   @citedHtml@ is the inline-cited references HTML, and @furtherHtml@ is
--   the further-reading-only references HTML (each empty when absent).
--   @cfg@: the site's switches, for the entries' links ('enhanceEntry').
applyCitations :: SiteConfig -> [Text] -> Text -> Pandoc -> IO (Pandoc, Text, Text, [Text])
applyCitations cfg frKeys bibPath doc
    | not (hasCitations frKeys doc) = return (doc, "", "", frKeys)
    | otherwise = do
        extras <- parseBibExtras (T.unpack bibPath)
        csl <- cslStyle
        -- Citeproc selects entries before formatting them. Resolve aliases
        -- for both citations and nocite at that boundary. A primary key
        -- always denotes its own entry, even if another entry claims it.
        let aliases = Map.fromList
                [ (T.pack a, T.pack k) | (k, e) <- Map.toList extras, a <- bibIds e
                                       , Map.notMember a extras ]
            canonical "*" = "*"
            canonical k   = Map.findWithDefault k k aliases
            rename (Cite cs ils) = Cite [c { citationId = canonical (citationId c) } | c <- cs] ils
            rename i = i
            further = map canonical frKeys
            -- Move the author's fragment target with their bibliography.
            -- If citeproc creates its own Div, another element in the
            -- source may already own the name (e.g. a References heading).
            refsAnchor = if null (query (\b -> [() | isRefsDiv b]) doc) then "" else "refs"
        processed <- runIOorExplode $ processCitations (injectMeta csl further bibPath (walk rename doc))
        let enhance = annotateArchive . enhanceEntry cfg extras
            (body, citedHtml, furtherHtml, shown) = transformAndExtract enhance refsAnchor further processed
        return (body, citedHtml, furtherHtml, shown ++ filter (/= "*") further)


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
renderBibliographyHtml :: SiteConfig            -- ^ the switches ('enhanceEntry')
                      -> [FilePath]             -- ^ .bib paths
                      -> Map String BibExtra    -- ^ enhancement map
                      -> [String]               -- ^ citekeys, in desired order
                      -> IO Text
renderBibliographyHtml _ _ _ [] = return ""
renderBibliographyHtml cfg bibPaths extras keys = do
    csl <- cslStyle
    let doc = synthesizeNociteDoc csl bibPaths keys
    processed <- runIOorExplode $ processCitations doc
    let refsDivs = concatMap unwrapRefs (pandocBlocks processed)
        ordered  = reorderByKeys keys refsDivs
        enhanced = map (annotateArchive . enhanceEntry cfg extras) ordered
    return (renderEntries "" "csl-bib-body" enhanced)
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
transformAndExtract :: (Block -> Block) -> Text -> [Text] -> Pandoc -> (Pandoc, Text, Text, [Text])
transformAndExtract enhance refsAnchor frKeys (Pandoc meta blocks) =
    let found = Set.fromList (query refIds blocks)
        -- Work only on the body. The metadata's nocite nodes never appear
        -- on the page. Nor does citeproc's formatted copy of a locator.
        settled = walk (concatMap (settle found)) (walk unrendered blocks)
        citeOrder = filter (`Set.member` found) (nub (query citationKeys settled))
        keyNums = Map.fromList (zip citeOrder [1 :: Int ..])
        (marked, st) = runState (markCitations keyNums settled) emptyMarkerState
        (body, citedHtml, furtherHtml) =
            extractBibliography enhance (msFirstOccurrence st) citeOrder refsAnchor frKeys marked
    in (Pandoc meta body, citedHtml, furtherHtml, citeOrder)
  where
    unrendered (Cite cs _) = Cite cs []
    unrendered i           = i
    refIds (Div ("refs", _, _) entries) = [ stripRefPrefix i | Div (i, _, _) _ <- entries ]
    refIds _                            = []

citationKeys :: Inline -> [Text]
citationKeys (Cite cs _) = map citationId cs
citationKeys _           = []

-- | Keep the citations that have entries, or the keys of an entirely
--   missing cluster. Splice every locator note into the surrounding inline
--   list, where it is numbered in source order alongside ordinary notes.
settle :: Set.Set Text -> Inline -> [Inline]
settle found (Cite cs _) = Cite kept [] : notes
  where
    notes = concatMap (\c -> outerNotes (citationPrefix c <> citationSuffix c)) cs
    resolved = filter ((`Set.member` found) . citationId) cs
    kept | null resolved = [c { citationPrefix = [], citationSuffix = [] } | c <- cs]
         | otherwise = [c { citationPrefix = withoutNotes (citationPrefix c)
                          , citationSuffix = withoutNotes (citationSuffix c) } | c <- resolved]
    withoutNotes = walk (filter (\case Note _ -> False; _ -> True))
settle _ i = [i]

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
--   Stop descending at the outer Cite. Citations in its prefix or suffix
--   are part of that same label and share its backlink. Notes have already
--   moved outside it. Text is a leaf in the document.
markCitations :: Data a => Map Text Int -> a -> State MarkerState a
markCitations keyNums node
    | Just (Cite cs _) <- cast node = do
        marker <- citationMarker keyNums cs
        return (fromMaybe node (cast marker))
    | Just (_ :: Text) <- cast node = return node
    | otherwise = gmapM (markCitations keyNums) node

citationMarker :: Map Text Int -> [Citation] -> State MarkerState Inline
citationMarker keyNums citations = do
    let numbered = [ (c, n) | c <- citations, Just n <- [Map.lookup (citationId c) keyNums] ]
        keys = map (citationId . fst) numbered
        inLabel = query citationKeys (concatMap (\c -> citationPrefix c <> citationSuffix c) citations)
    case numbered of
        ((firstCite, firstNum) : _) -> do
            let firstKey = citationId firstCite
            seen <- gets (fromMaybe 0 . Map.lookup firstNum . msOccurrences)
            let occ      = seen + 1
                markerId = backAnchorId firstNum occ
            modify' $ \st -> st
                { msOccurrences = Map.insert firstNum occ (msOccurrences st)
                  -- insertWith keeps the value already there, so the
                  -- earliest occurrence wins for every key in the cluster.
                , msFirstOccurrence =
                    foldr (\k m -> Map.insertWith (\_ old -> old) k markerId m)
                          (msFirstOccurrence st) (keys ++ inLabel)
                }
            return $ RawInline "html" (markerHtml keys markerId firstKey (markerLabel keyNums citations))
        -- No key of the cluster has an entry: the citation is shown where
        -- it is made, as @[?key]@, unlinked (no entry to link to), so the
        -- missing source is seen at the claim, with the key to fix.
        -- Citeproc has already warned in the build's output.
        [] -> return $ RawInline "html" (missingHtml (map citationId citations))

-- | The marker of a citation none of whose keys has an entry: its keys,
--   each after a @?@, as text, with what is missing in its title.
missingHtml :: [Text] -> Text
missingHtml keys =
    "<sup class=\"cite-marker cite-missing\" title=\"No bibliography entry for "
    <> esc (T.intercalate ", " keys) <> "\">"
    <> "[" <> T.intercalate "; " (map (("?" <>) . esc) keys) <> "]</sup>"
  where
    esc = T.concatMap $ \c -> case c of
        '&' -> "&amp;"
        '<' -> "&lt;"
        '>' -> "&gt;"
        '"' -> "&quot;"
        _   -> T.singleton c

-- | Id for the /occ/th marker of reference number /n/.
backAnchorId :: Int -> Int -> Text
backAnchorId n occ
    | occ <= 1  = base
    | otherwise = base <> "-" <> T.pack (show occ)
  where base = "cite-back-" <> T.pack (show n)

markerHtml :: [Text] -> Text -> Text -> Text -> Text
markerHtml keys markerId firstKey label =
    let allIds  = T.intercalate " " (map ("ref-" <>) keys)
    in "<sup class=\"cite-marker\" id=\"" <> markerId <> "\">"
    <> "<a href=\"#ref-" <> firstKey <> "\" class=\"cite-link\""
    <> " data-cite-keys=\"" <> allIds <> "\">"
    <> label <> "</a></sup>"

-- | A marker's label, as HTML: the numbers of a cluster, @[3,4]@, and,
--   where its author wrote any, each citation's prefix and locator as
--   written: @[\@PralatWormald, Theorem 4.1]@ is @[8, Theorem 4.1]@, @[see
--   \@a, p. 3; \@b]@ is @[see 1, p. 3; 2]@. Pandoc keeps the locator in the
--   citation's suffix, which the label once left out, and with it which
--   theorem or page was meant. A cluster with neither is labelled as it
--   always was.
--
--   A bracketed citation's suffix is as written, its own separator with it
--   (@[\@a, p. 3]@, @[\@a 23]@ as @[1 23]@, @[\@a(23)]@ as @[1(23)]@). A
--   textual citation's has none (@\@a [23]@ is the key and @23@), and is
--   given a comma: @[1, 23]@. The label is inside the marker's link, so
--   links in it are taken off, Markdown or HTML, and 'settle' has taken its
--   footnotes out; a citation in a locator is shown as its numbers, as
--   text (@[2]@, or @[?key]@ without an entry).
markerLabel :: Map Text Int -> [Citation] -> Text
markerLabel keyNums cs = inlineHtml (walk unlink (walk (concatMap label) [Cite cs []]))
  where
    -- A bottom-up inline walk has already labelled any citations in these
    -- prefixes and suffixes. Keep everything as inlines until the one HTML
    -- write, so brackets, markup and significant spaces travel together.
    label (Cite cluster _) = [Str "["] ++ intercalate sep (map one cluster) ++ [Str "]"]
      where
        sep | all bare cluster && any ((`Map.member` keyNums) . citationId) cluster = [Str ","]
            | otherwise = [Str ";", Space]
    label i = [i]
    bare c = null (citationPrefix c) && null (citationSuffix c)
    one c =
        let prefix = citationPrefix c
            key = citationId c
            number = maybe ("?" <> key) (T.pack . show) (Map.lookup key keyNums)
        in prefix ++ [Space | not (null prefix)] ++ [Str number] ++ suffix c
    suffix c = case (citationMode c, citationSuffix c) of
        (_, []) -> []
        (AuthorInText, xs@(Str t : _))
            | Just (ch, _) <- T.uncons t, ch `elem` (",;" :: String) -> xs
        (AuthorInText, xs) -> Str "," : Space : xs
        (_, xs) -> xs

-- | The footnotes in some inlines that are in no other footnote there, in
--   source order: a note written inside a note goes out with the one it is
--   in, and the descent stops at a note. (Subtracting the notes found inside
--   others, by value, also took out a separate note that read the same.)
outerNotes :: [Inline] -> [Inline]
outerNotes = concatMap outer
  where
    outer i = case i of
        Note _          -> [i]
        Emph xs         -> outerNotes xs
        Underline xs    -> outerNotes xs
        Strong xs       -> outerNotes xs
        Strikeout xs    -> outerNotes xs
        Superscript xs  -> outerNotes xs
        Subscript xs    -> outerNotes xs
        SmallCaps xs    -> outerNotes xs
        Quoted _ xs     -> outerNotes xs
        Cite cs xs      -> concatMap (\c -> outerNotes (citationPrefix c <> citationSuffix c)) cs
                           <> outerNotes xs
        Link _ xs _     -> outerNotes xs
        Image _ xs _    -> outerNotes xs
        Span _ xs       -> outerNotes xs
        _               -> []

-- | Links off, for inside the marker's link (a link in a link is not
--   HTML): a Markdown link becomes its text, and an HTML anchor's tags go,
--   inside any raw HTML, leaving what was between them.
unlink :: [Inline] -> [Inline]
unlink = concatMap $ \i -> case i of
    Link _ inner _ -> inner
    RawInline fmt@(Format f) t
        | f `elem` ["html", "html5"] ->
            let t' = stripAnchorTags t in [RawInline fmt t' | not (T.null t')]
    _ -> [i]

-- | Raw HTML with its @\<a …>@ and @\</a>@ tags taken out, what they held
--   kept: @\<em>\<a href="#c">ch\</a>\</em>@ is @\<em>ch\</em>@. Read by
--   an HTML tokenizer: a @>@ inside a quoted attribute (@title="n > 1"@) is
--   part of the tag, not its end.
stripAnchorTags :: Text -> Text
stripAnchorTags t
    | not (any isAnchor tags) = t
    | otherwise               = TS.renderTagsOptions opts (filter (not . isAnchor) tags)
  where
    tags = TS.parseTags t
    isAnchor tag = case tag of
        TS.TagOpen name _ -> T.toLower name == "a"
        TS.TagClose name  -> T.toLower name == "a"
        _                 -> False
    -- Written back as it came: text as text, no tag made self-closing.
    opts = TS.renderOptions { TS.optMinimize = const False }

-- | Write the marker label with the bibliography's math settings.
inlineHtml :: [Inline] -> Text
inlineHtml ils =
    case runPure (writeHtml5String wOpts (Pandoc nullMeta [Plain ils])) of
        Left  _    -> T.concat [ t | Str t <- ils ]
        -- Only the writer's line ends come off: a no-break space that opens
        -- a locator (@[\@a\\ 23]@) is its separator, and T.strip took it,
        -- joining reference 1 and page 23 into [123].
        Right html -> unclaimMathSpans (T.dropAround (`elem` ("\r\n" :: String)) html)
  where
    wOpts = def { writerWrapText = WrapNone, writerHTMLMathMethod = MathML }


-- ---------------------------------------------------------------------------
-- Bibliography extraction + rendering
-- ---------------------------------------------------------------------------

-- | Separate the @refs@ div from body blocks and render it to HTML.
--   Returns @(bodyBlocks, citedHtml, furtherHtml)@.
extractBibliography :: (Block -> Block) -> Map Text Text -> [Text] -> Text -> [Text] -> [Block]
                    -> ([Block], Text, Text)
extractBibliography enhance backMap citeOrder refsAnchor frKeys blocks =
    -- Wherever it sits: a @#refs@ in a wrapper Div was left in the body,
    -- entries the page does not cite and all.
    let refDivs   = query (\b -> [ b | isRefsDiv b ]) blocks
        bodyBlocks = walk (filter (not . isRefsDiv)) blocks
        (citedHtml, furtherHtml) = case refDivs of
            []    -> ("", "")
            (d:_) -> renderBibDiv enhance backMap citeOrder refsAnchor frKeys d
    in (bodyBlocks, citedHtml, furtherHtml)

isRefsDiv :: Block -> Bool
isRefsDiv (Div ("refs", _, _) _) = True
isRefsDiv _                     = False

-- | Render the citeproc @refs@ Div into two HTML strings:
--   @(citedHtml, furtherHtml)@ — each is empty when there are no entries
--   in that section. Headings are rendered in the template, not here.
--
--   Entry bodies are enhanced before numbering: title-wrapped as a
--   @.pdf-link[data-pdf-src]@ when the .bib @file:@ field is set (so
--   popups.js's PDF hover preview fires), and a trailing
--   @\<div class="bib-keywords"\>@ appended when @keywords:@ is set.
renderBibDiv :: (Block -> Block) -> Map Text Text -> [Text] -> Text -> [Text] -> Block -> (Text, Text)
renderBibDiv enhance backMap citeOrder refsAnchor frKeys (Div _ children) =
    let enhanced = map enhance children
        keyIndex = Map.fromList (zip citeOrder [0 :: Int ..])
        -- Further reading is what the page lists: an entry citeproc made
        -- for a key cited only where the page does not show it (a missing
        -- key's locator) is neither cited nor listed.
        (citedEntries, rest) = partition (isCited keyIndex) enhanced
        furtherEntries = filter listed rest
        -- @further-reading: ["*"]@ lists every entry the page does not cite.
        listed (Div (i, _, _) _) = "*" `elem` frKeys || stripRefPrefix i `elem` frKeys
        listed _                 = False
        sorted   = sortBy (comparing (entryOrder keyIndex)) citedEntries
        numbered = zipWith (addNumber backMap) [1..] sorted
        onlyFurther = null numbered && not (null furtherEntries)
        citedHtml
            | onlyFurther = ""
            | otherwise   = renderEntries refsAnchor "csl-bib-body cite-refs" numbered
        furtherHtml
            | null furtherEntries = ""
            | otherwise           = renderEntries (if onlyFurther then refsAnchor else "") "csl-bib-body further-reading-refs" furtherEntries
    in (citedHtml, furtherHtml)
renderBibDiv _ _ _ _ _ _ = ("", "")


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
--   from the .bib file: its PDF (through the viewer with the pdf-viewer
--   switch on) and its keywords (linked to their pages with the
--   bibliography-index switch on). Other Blocks pass through unchanged.
enhanceEntry :: SiteConfig -> Map String BibExtra -> Block -> Block
enhanceEntry cfg extras b@(Div attrs@(divId, _, _) blocks) =
    let key    = T.unpack (stripRefPrefix divId)
        extra  = fromMaybe emptyBibExtra (Map.lookup key extras)
        withLink = case bibFile extra of
            Nothing -> blocks
            Just fp -> map (wrapFirstTitleBlock (asPdfLink (switchOn cfg "pdf-viewer") (T.pack fp))) blocks
        withKw   = withLink ++ keywordsBlocks (switchOn cfg "bibliography-index") (bibKeywords extra)
    in case (bibFile extra, bibKeywords extra) of
        (Nothing, []) -> b
        _             -> Div attrs withKw
enhanceEntry _ _ b = b

-- | In one block of an entry, wrap the first title-bearing inline
--   with a @.pdf-link@ anchor. Pandoc's CSL-formatted references
--   render the title as either a @Quoted@ (article titles in
--   Chicago-notes: "Paper Title") or an @Emph@ (book titles:
--   /Book Title/), and those are the first such inline in each
--   entry. We wrap at the block level and fall back to passing the
--   block through if no matching inline appears.
wrapFirstTitleBlock :: ([Inline] -> Inline) -> Block -> Block
wrapFirstTitleBlock link = \case
    Para  ils -> Para  (wrapFirstTitle link ils)
    Plain ils -> Plain (wrapFirstTitle link ils)
    other     -> other

-- | Left-to-right scan: wrap the first title-bearing inline in a link
--   pointing at the PDF. Pandoc's CSL renderer emits article titles as
--   @Span@ nodes (whose rendered HTML wraps quotation marks around the
--   title text) and book titles as @Emph@; @Quoted@ appears in some
--   other CSL styles. First match of any of these is treated as the
--   title; subsequent ones pass through — journal names are also
--   @Emph@ on @\@article@ entries but come after the @Span@ title, so
--   the article case picks the right target.
wrapFirstTitle :: ([Inline] -> Inline) -> [Inline] -> [Inline]
wrapFirstTitle link inls = reverse . fst $ foldl step ([], False) inls
  where
    step (acc, True)  inl = (inl:acc, True)
    step (acc, False) inl = case inl of
        Span _ _   -> (link [inl] : acc, True)
        Quoted _ _ -> (link [inl] : acc, True)
        Emph   _   -> (link [inl] : acc, True)
        _          -> (inl:acc, False)

-- | Build the @.pdf-link[data-pdf-src]@ anchor that popups.js binds to.
--   A hosted PDF opens in the PDF.js viewer, as a body link to one does
--   ('Filters.Links.pdfViewerTarget', with the pdf-viewer switch on); any
--   other @file:@ is linked as is.
asPdfLink :: Bool -> Text -> [Inline] -> Inline
asPdfLink viewer href content =
    let (url, src) = fromMaybe (href, href) (Filters.Links.pdfViewerTarget viewer href)
    in  Link ("", ["pdf-link"], [("data-pdf-src", src)]) content (url, "")

-- | Trailing keyword strip, linking each keyword to its
--   @/bibliography/\<keyword\>/@ page when there are such pages
--   (@paged@), and naming it otherwise. Returns @[]@ when the keyword
--   list is empty so the entry gets no extra block at all.
keywordsBlocks :: Bool -> [String] -> [Block]
keywordsBlocks _ [] = []
keywordsBlocks paged ks =
    [ Div ("", ["bib-keywords"], [])
          [Plain (intersperse (Str ", ") (map keyword ks))]
    ]
  where
    keyword k
        | paged     = Link ("", ["bib-keyword"], []) [Str (T.pack k)]
                           (T.pack ("/bibliography/" ++ k ++ "/"), "")
        | otherwise = Span ("", ["bib-keyword"], []) [Str (T.pack k)]

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
--   MathML needs no runtime at all, which is the right answer for a
--   reference list (and, since, for the whole site: 'Filters.Math').
--
--   'unclaimMathSpans' renames a wrapper Pandoc's writer leaves only where
--   it falls back to TeX, so the page's @math@ flag (which loads
--   @math.js@, for display formulas) is not set by a bibliography entry.
renderEntries :: Text -> Text -> [Block] -> Text
renderEntries ident cls entries =
    case runPure (writeHtml5String wOpts (Pandoc nullMeta entries)) of
        Left  _    -> ""
        Right html -> "<div" <> idAttr <> " class=\"" <> cls <> "\" role=\"list\">\n"
                        <> unclaimMathSpans html <> "</div>\n"
  where
    idAttr = if T.null ident then "" else " id=\"" <> ident <> "\""
    wOpts = def { writerWrapText       = WrapNone
                , writerHTMLMathMethod = MathML
                }

-- | Rename Pandoc's @math@ wrapper class on a bibliography entry's math.
--   The @inline@ / @display@ half is kept for anyone styling it.
unclaimMathSpans :: Text -> Text
unclaimMathSpans =
      T.replace "class=\"math inline\""  "class=\"math-rendered inline\""
    . T.replace "class=\"math display\"" "class=\"math-rendered display\""
