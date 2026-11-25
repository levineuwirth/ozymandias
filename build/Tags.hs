{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Hierarchical tag system.
--
--   Tags are slash-separated strings in YAML frontmatter:
--     tags: [research/mathematics, nonfiction/essays, typography]
--
--   "research/mathematics" expands to ["research", "research/mathematics"]
--   so /research/ aggregates everything tagged with any research/* sub-tag.
--
--   Pages live at /<tag>/index.html — no /tags/ namespace:
--     research              → /research/
--     research/mathematics  → /research/mathematics/
--     typography            → /typography/
--
--   Optional sidecar files at @content/tag-meta/<tag-path>.md@ supply
--   a per-tag @tooltip:@ (frontmatter) and prose intro (body). When
--   a sidecar exists, the tag page exposes @$portal-tooltip$@ and
--   @$portal-intro$@; when it is absent, both fields are noResult
--   and the corresponding @$if$@ blocks render nothing.
module Tags
    ( buildAllTags
    , pagedTags
    , applyTagRules
    , anchoredTagsRules
    , tagPaginationThreshold
    , tagPageSize
    , sortAndGroupByDisplayAt
    , sidecarIdentifier
    , portalIntroField
    , portalTooltipField
    , seeAlsoContext
    , portalShelfContext
    ) where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, when)
import Data.Char  (isSpace, toLower, toUpper)
import qualified Data.Function as F
import Data.List  (groupBy, intercalate, isPrefixOf, isSuffixOf, nub, sort, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Ord   (Down (..), comparing)
import Data.Set   (Set)
import qualified Data.Set as Set
import Data.Time  (UTCTime (..), toGregorian)
import qualified Data.Text as T
import Hakyll hiding (escapeHtml)
import Utils (escapeHtml, inDefault, writerlyDate)
import Config     (SiteConfig (..), Portal (..), Section (..), loadConfig, switchOn, tagDisplayName)
import System.Directory (doesFileExist)
import System.FilePath (dropFileName, takeFileName)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Scientific as Sci
import qualified Data.Yaml as Y
import TagMap     (MapPiece (..), MapTag (..), layoutMap, renderMap)
import Patterns   (blogPattern, fictionPattern, musicPattern, poetryPattern, tagIndexable)
import Sections   (sectionOfRoute, sectionOwnedTopLevelTags, tagPageUrl)
import Contexts   (abstractField, contentKindField, displaySortKey, identifierDisplayUTCMaybe, svgAspect,
                   photographyCtx, recentFirstByDisplay, revisionDateFields, siteCtx,
                   tagLinksFieldExcludingScope)


-- ---------------------------------------------------------------------------
-- Pagination policy
-- ---------------------------------------------------------------------------

-- | Maximum number of items for which a tag index ships its full list and
--   relies purely on the client-side 25/50/100/All toggle (matching
--   @\/new.html@). Tags above this threshold fall back to server-side
--   pagination at 'tagPageSize' per page; the count toggle then operates
--   within the current page only.
tagPaginationThreshold :: Int
tagPaginationThreshold = 150

-- | Page size used for server-side pagination on tag pages that exceed
--   'tagPaginationThreshold'.
tagPageSize :: Int
tagPageSize = 100


-- ---------------------------------------------------------------------------
-- Hierarchy expansion
-- ---------------------------------------------------------------------------

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s = case dropWhile p s of
    ""  -> []
    s'  -> w : wordsBy p rest
      where (w, rest) = break p s'

-- | "research/mathematics" → ["research", "research/mathematics"]
--   "a/b/c"                → ["a", "a/b", "a/b/c"]
--   "typography"           → ["typography"]
expandTag :: String -> [String]
expandTag t =
    let segs = wordsBy (== '/') t
    in  [ intercalate "/" (take n segs) | n <- [1 .. length segs] ]

-- | All expanded tags for an item (reads the "tags" metadata field).
getExpandedTags :: MonadMetadata m => SiteConfig -> Identifier -> m [String]
getExpandedTags cfg ident = pagedTags cfg <$> getTags ident

-- | The tags that get a page for a list of tags as written: each with its
--   ancestors, empty path segments dropped ("photography//a" pages
--   @photography/a@), less the 'Sections.sectionOwnedTopLevelTags', which
--   would collide with section landings. @site expand-tags@ answers this for
--   tools/scaffold-photos.py, whose own copy kept the empty segments.
--   The tag index is built from them whatever the tag-pages switch, since
--   /stats/ and /build/ count it; the pages, and the commands that list
--   them, ask the switch.
pagedTags :: SiteConfig -> [String] -> [String]
pagedTags cfg = filter (`notElem` sectionOwnedTopLevelTags cfg) . nub . concatMap expandTag


-- ---------------------------------------------------------------------------
-- Identifiers and URLs
-- ---------------------------------------------------------------------------

tagFilePath :: String -> FilePath
tagFilePath tag = tag ++ "/index.html"

tagIdentifier :: String -> Identifier
tagIdentifier = fromFilePath . tagFilePath

-- | Identifier of the optional sidecar for a given tag.
--   "nonfiction"           → content/tag-meta/nonfiction.md
--   "nonfiction/philosophy" → content/tag-meta/nonfiction/philosophy.md
sidecarIdentifier :: String -> Identifier
sidecarIdentifier tag = fromFilePath ("content/tag-meta/" ++ tag ++ ".md")


-- ---------------------------------------------------------------------------
-- Building the Tags index
-- ---------------------------------------------------------------------------

-- | Scan all essays and blog posts and build the Tags index.
buildAllTags :: SiteConfig -> Rules Tags
buildAllTags cfg =
    buildTagsWith (getExpandedTags cfg) (tagIndexable cfg) tagIdentifier


-- ---------------------------------------------------------------------------
-- Sidecar fields
-- ---------------------------------------------------------------------------

-- | Field exposing a sidecar's rendered HTML body as @$portal-intro$@.
--   Fails with 'noResult' when the sidecar body is empty or whitespace-only,
--   so @$if(portal-intro)$@ is false and the render site emits nothing.
--
--   Takes a function that yields the sidecar identifier from the current
--   item — this lets tag pages bind the sidecar statically at rule time
--   (@const sidecarId@) while the home-page portal listField derives it
--   per-item from the item body.
portalIntroField :: (Item a -> Identifier) -> Context a
portalIntroField getSidecarId = field "portal-intro" $ \item -> do
    let sidecarId = getSidecarId item
    html <- itemBody <$> loadSnapshot sidecarId "body"
    if all isSpace html
        then noResult "sidecar body is empty"
        else return html

-- | Field exposing a sidecar's @tooltip:@ frontmatter value as
--   @$portal-tooltip$@. Fails with 'noResult' when the key is absent
--   or the value is empty / whitespace-only. Accepts a per-item
--   identifier resolver for the same reason as 'portalIntroField'.
portalTooltipField :: (Item a -> Identifier) -> Context a
portalTooltipField getSidecarId = field "portal-tooltip" $ \item -> do
    let sidecarId = getSidecarId item
    meta <- getMetadata sidecarId
    case fmap trim (lookupString "tooltip" meta) of
        Just t | not (null t) -> return t
        _                     -> noResult "no tooltip"

-- ---------------------------------------------------------------------------
-- A tag page's surroundings
-- ---------------------------------------------------------------------------

-- | Direct parent of a tag path. @Nothing@ for top-level tags.
--   "epistemology/social" → Just "epistemology"
--   "a/b/c"               → Just "a/b"
parentOf :: String -> Maybe String
parentOf t = case wordsBy (== '/') t of
    []   -> Nothing
    [_]  -> Nothing
    segs -> Just (intercalate "/" (init segs))   -- 'init' safe: 2+ segments

-- | Number of @/@ characters in a tag path (i.e., depth - 1).
slashCount :: String -> Int
slashCount = length . filter (== '/')

-- | The tags with items directly beneath a scope, alphabetically.
childrenOf :: Tags -> String -> [String]
childrenOf tags scope = sort
    [ c | (c, _) <- tagsMap tags
        , (scope ++ "/") `isPrefixOf` c
        , slashCount c == slashCount scope + 1 ]

-- | What a tag page offers to go to next. A subtag: its parent, then its
--   siblings, alphabetically. A portal: the other portals of its facet
--   (site.yaml's order). Any other top-level tag: every portal. Only tags
--   with a page are offered: a tag with items, or a portal whose page is a
--   section's landing.
seeAlsoTags :: SiteConfig -> Tags -> String -> [String]
seeAlsoTags cfg tags scope = case parentOf scope of
    Just parent -> parent : sort [ s | s <- tKeys, parentOf s == Just parent, s /= scope ]
    Nothing -> case [ p | p <- portals, portalTag p == scope ] of
        (p : _) -> [ portalTag q | q <- portals, portalTag q /= scope
                                 , portalFacet q == portalFacet p, hasPage (portalTag q) ]
        []      -> [ portalTag q | q <- portals, hasPage (portalTag q) ]
  where
    portals   = sitePortals cfg
    tKeys     = map fst (tagsMap tags)
    hasPage t = t `elem` tKeys || t `elem` sectionOwnedTopLevelTags cfg

-- | @$for(see-also)$@ on a tag page ('seeAlsoTags'): @$see-also-name$@,
--   @$see-also-url$@, @$see-also-up$@ on the parent, and the tag's gloss as
--   @$portal-tooltip$@. 'noResult' when there is nothing to offer.
seeAlsoContext :: Set Identifier -> Tags -> String -> Context String
seeAlsoContext sidecarSet tags scope = listFieldWith "see-also" itemCtx $ \_ -> do
    cfg <- loadConfig
    case seeAlsoTags cfg tags scope of
        [] -> noResult "nothing to see also"
        ts -> return [ Item (fromFilePath ("_tag/" ++ t)) t | t <- ts ]
  where
    itemCtx =
           field "see-also-name" (\i -> do cfg <- loadConfig
                                           return (escapeHtml (tagDisplayName cfg (itemBody i))))
        <> field "see-also-url"  (\i -> do cfg <- loadConfig
                                           maybe (noResult "no page") return (tagPageUrl cfg (itemBody i)))
        <> field "see-also-up"   (\i -> if Just (itemBody i) == parentOf scope
                                            then return "true" else noResult "not the parent")
        <> field "portal-tooltip" (glossOf sidecarSet . itemBody)

-- | What a tag page needs to know of each piece under it.
data Piece = Piece
    { pieceKind    :: String          -- ^ its section's kind, or "Page"
    , pieceSection :: Maybe String    -- ^ its section's directory
    , pieceWhen    :: Maybe UTCTime   -- ^ its display date, if it has one
    }

pieceOf :: SiteConfig -> Identifier -> Compiler Piece
pieceOf cfg ident = do
    r <- getRoute ident
    let sec = sectionOfRoute cfg =<< r
    t <- identifierDisplayUTCMaybe ident
    return (Piece (maybe "Page" sectionKind sec) (sectionDir <$> sec) t)

-- | "one essay", "seven essays", "97 photos", "eight pieces": the count in
--   words up to ten, and the pieces' kind when they share one.
countPhrase :: [String] -> String
countPhrase kinds = numberWord n ++ " " ++ plural noun
  where
    n    = length kinds
    noun = case nub kinds of
        [k] -> map toLower k
        _   -> "piece"
    plural w = if n == 1 then w else w ++ "s"
    numberWord k
        | k >= 1 && k <= 10 = words "one two three four five six seven eight nine ten" !! (k - 1)
        | otherwise         = show k

-- | When the pieces date from: "November 2026", "May to November 2026",
--   "2024 to 2026". Nothing for no pieces.
spanPhrase :: [UTCTime] -> Maybe String
spanPhrase [] = Nothing
spanPhrase ts
    | (y0, m0) == (y1, m1) = Just (month m0 ++ " " ++ show y0)
    | y0 == y1             = Just (month m0 ++ " to " ++ month m1 ++ " " ++ show y1)
    | otherwise            = Just (show y0 ++ " to " ++ show y1)
  where
    ym t = let (y, m, _) = toGregorian (utctDay t) in (y, m)
    (y0, m0) = ym (minimum ts)
    (y1, m1) = ym (maximum ts)
    month m = words "January February March April May June July August September October November December" !! (m - 1)

capitalize :: String -> String
capitalize (c : cs) = toUpper c : cs
capitalize []       = []

-- | A sidecar's tooltip, the tag's one-line gloss, or 'noResult'.
glossOf :: Set Identifier -> String -> Compiler String
glossOf sidecarSet tag
    | sidecarId `Set.member` sidecarSet = do
        meta <- getMetadata sidecarId
        case fmap trim (lookupString "tooltip" meta) of
            Just t | not (null t) -> return t
            _                     -> noResult "no tooltip"
    | otherwise = noResult "no sidecar"
  where
    sidecarId = sidecarIdentifier tag

-- | Context for the current tag's sidecar, if one exists, and eager
--   registration of the snapshot dependency.
--
--   When a sidecar exists, the body snapshot is loaded unconditionally
--   (and discarded) so Hakyll's dependency tracker sees the edge
--   /tag page → sidecar body/ on every compile — even when the
--   rendered @$if(portal-intro)$@ gate is false because the body is
--   empty. Without this, the first build after populating a previously
--   empty sidecar would not re-render the tag page (the lazy field
--   load inside 'portalIntroField' never fires while the gate is
--   false, so the dep is never established).
--
--   Tags with no sidecar take the @mempty@ branch and register no
--   dependency, which is correct — there is nothing to depend on.
sidecarContext :: Set Identifier -> String -> Compiler (Context String)
sidecarContext sidecarSet tag
    | sidecarId `Set.member` sidecarSet = do
        _ <- loadSnapshot sidecarId "body" :: Compiler (Item String)
        return ( portalIntroField   (const sidecarId)
              <> portalTooltipField (const sidecarId))
    | otherwise = return mempty
  where
    sidecarId = sidecarIdentifier tag

-- | The sidecar's @featured:@ paths (@content/essays/foo.md@), in order.
featuredPaths :: Set Identifier -> String -> Compiler [FilePath]
featuredPaths sidecarSet tag
    | sidecarId `Set.member` sidecarSet =
        fromMaybe [] . lookupStringList "featured" <$> getMetadata sidecarId
    | otherwise = return []
  where
    sidecarId = sidecarIdentifier tag


-- ---------------------------------------------------------------------------
-- Tag index page rules
-- ---------------------------------------------------------------------------

-- | Item-level context for a card on a tag page. Provides the fields
--   consumed by @templates/partials/item-card.html@ (@$item-kind$@,
--   @$date-iso$@, @$date-created$@, @$abstract$@, @$item-tags$@) with
--   tag-ribbon suppression scoped to the current tag.
--
--   Composes 'siteCtx' (not bare 'defaultContext') so per-item fields
--   the card partial gates on — notably @$has-monogram$@ — fire here
--   the same way they do on /new.html and the library.
tagItemCtx :: String -> Context String
tagItemCtx scope =
    contentKindField
    <> dateField "date-created"  writerlyDate
    <> dateField "date"          writerlyDate
    <> revisionDateFields
    <> tagLinksFieldExcludingScope "item-tags" scope
    <> abstractField
    <> siteCtx

-- | Everything a tag page shows besides its items' own fields:
--
--   * @$title$@, the tag's display name, and @$for(tag-crumbs)$@ (its
--     ancestors: @$crumb-name$@, @$crumb-url$@);
--   * @$tag-facts$@ ("Seven essays · May to November 2026"), over the
--     whole tag, not only this page of it;
--   * @$tag-facet$@, the portal's facet (site.yaml), which the title
--     page's ornament follows;
--   * @$for(tag-within)$@, the tags directly beneath it (@$within-name$@,
--     @$within-url$@, @$within-count$@ in words, @$within-n$@ in figures,
--     @$within-gloss$@);
--   * the items: @$photos$@ when every piece is a photograph (the
--     sidecar's @featured:@ first), else @$for(contents)$@, the page's
--     pieces by year (@$contents-year$@, "Undated" for the last run, and
--     @$for(contents-entries)$@ with each piece's fields, @$entry-month$@,
--     and @$entry-mixed$@ where the tag mixes kinds), and
--     @$for(tag-frontispiece)$@, the sidecar's first @featured:@ piece on
--     this page;
--   * @$for(tag-gallery)$@, the sidecar's @gallery:@ ('galleryOf':
--     @$gallery-src$@, @$gallery-url$@, @$gallery-title$@, @$gallery-ar$@);
--   * @$for(tag-alongside)$@, the other tags its pieces carry, most shared
--     first (@$alongside-parent$@, @$alongside-name$@, @$alongside-url$@,
--     @$alongside-count$@);
--   * @$for(see-also)$@ ('seeAlsoTags': @$see-also-name$@,
--     @$see-also-url$@, @$see-also-up$@ on a parent, @$portal-tooltip$@).
tagPageContext :: Tags -> Set Identifier -> String -> [Item String]
               -> Compiler (Context String)
tagPageContext tags sidecarSet scope items = do
    cfg <- loadConfig
    let name    = tagDisplayName cfg
        urlOf t = tagPageUrl cfg t
        allIds  = fromMaybe [] (lookup scope (tagsMap tags))
        idsOf t = fromMaybe [] (lookup t (tagsMap tags))
    scCtx  <- sidecarContext sidecarSet scope
    pieces <- mapM (pieceOf cfg) allIds
    paths  <- featuredPaths sidecarSet scope
    within <- mapM (\c -> do ks <- mapM (fmap pieceKind . pieceOf cfg) (idsOf c)
                             return (c, ks))
                   (childrenOf tags scope)
    -- The page's pieces by the year of their display date, newest first as
    -- they come, an undated run last ('recentFirstByDisplay').
    dated <- mapM (\i -> (,) i <$> identifierDisplayUTCMaybe (itemIdentifier i)) items
    gallery <- galleryOf sidecarSet scope
    written <- mapM getTags allIds
    let facts = capitalize (countPhrase (map pieceKind pieces))
                ++ maybe "" (" \183 " ++) (spanPhrase (mapMaybe pieceWhen pieces))
        related t = t == scope || (t ++ "/") `isPrefixOf` scope || (scope ++ "/") `isPrefixOf` t
        shared = Map.fromListWith (+)
            [ (t, 1 :: Int) | ts <- written, t <- nub ts, not (related t), isJust (urlOf t) ]
        alongside = sortBy (comparing (\(t, n) -> (Down n, map toLower (name t))))
                           (Map.toList shared)
        crumbs = init (expandTag scope)

        photoGrid = not (null pieces) && all ((== Just "photography") . pieceSection) pieces
        featured  = [ i | p <- paths, i <- items, itemIdentifier i == fromFilePath p ]
        rest      = [ i | i <- items, itemIdentifier i `notElem` map itemIdentifier featured ]
        -- The portal's facet (site.yaml), which the title page's ornament
        -- follows; a subtag takes its portal's.
        facet = listToMaybe [ portalFacet q | q <- sitePortals cfg
                                            , portalTag q == takeWhile (/= '/') scope
                                            , not (null (portalFacet q)) ]
        mixed = length (nub (map pieceKind pieces)) > 1
        years = groupBy (\a b -> yearOf a == yearOf b) dated
        yearOf = fmap (\t -> let (y, _, _) = toGregorian (utctDay t) in y) . snd
        yearItem run = Item (fromFilePath ("_tag-year/" ++ maybe "undated" show (yearOf (head run)))) run
        yearCtx = field "contents-year" (return . maybe "Undated" show . yearOf . head . itemBody)
               <> listFieldWith "contents-entries" entryCtx (return . map toItem . itemBody)
        toItem (i, t) = itemSetBody (itemBody i, t) i
        entryCtx = field "entry-month" (\i -> maybe (noResult "undated") (return . monthName) (snd (itemBody i)))
                <> (if mixed then constField "entry-mixed" "true" else mempty)
                <> contramapItem fst (tagItemCtx scope)
        itemsField key ctx = listField key ctx . return

        tagItem t = Item (fromFilePath ("_tag/" ++ t)) t
        crumbCtx = field "crumb-name" (return . escapeHtml . name . itemBody)
                <> field "crumb-url"  (maybe (noResult "no page") return . urlOf . itemBody)
        withinCtx = field "within-name"  (return . escapeHtml . name . fst . itemBody)
                 <> field "within-url"   (maybe (noResult "no page") return . urlOf . fst . itemBody)
                 <> field "within-count" (return . countPhrase . snd . itemBody)
                 <> field "within-n"     (return . show . length . snd . itemBody)
                 <> field "within-gloss" (glossOf sidecarSet . fst . itemBody)
        alongsideCtx =
               field "alongside-parent" (\i -> maybe (noResult "top-level") (return . escapeHtml . name)
                                                     (parentOf (fst (itemBody i))))
            <> field "alongside-name"  (return . escapeHtml . name . fst . itemBody)
            <> field "alongside-url"   (maybe (noResult "no page") return . urlOf . fst . itemBody)
            <> field "alongside-count" (return . show . snd . itemBody)
        nonEmpty key ctx xs = if null xs then mempty else listField key ctx (return xs)
        galleryCtx = field "gallery-src"   (return . escapeHtml . gSrc . itemBody)
                  <> field "gallery-url"   (return . escapeHtml . gUrl . itemBody)
                  <> field "gallery-title" (return . escapeHtml . gTitle . itemBody)
                  <> field "gallery-ar"    (return . gAspect . itemBody)
        flag key on = if on then constField key "true" else mempty
    return $
           constField "title" (name scope)
        <> constField "tag"   scope
        <> constField "tag-facts" facts
        <> nonEmpty "tag-crumbs" crumbCtx (map tagItem crumbs)
        <> nonEmpty "tag-within" withinCtx
                    [ Item (fromFilePath ("_tag-within/" ++ c)) w | w@(c, _) <- within ]
        <> nonEmpty "tag-alongside" alongsideCtx
                    [ Item (fromFilePath ("_tag-alongside/" ++ t)) a | a@(t, _) <- alongside ]
        <> seeAlsoContext sidecarSet tags scope
        <> (if photoGrid
                then itemsField "photos" (photographyCtx <> tagItemCtx scope) (featured ++ rest)
                else nonEmpty "contents" yearCtx (map yearItem years)
                  <> nonEmpty "tag-frontispiece" (tagItemCtx scope) (take 1 featured))
        <> maybe mempty (constField "tag-facet" . escapeHtml) facet
        <> nonEmpty "tag-gallery" galleryCtx
                    [ Item (fromFilePath ("_tag-gallery/" ++ show n)) g | (n, g) <- zip [(0 :: Int) ..] gallery ]
        <> constField "list-page" "true"
        <> flag "photography" photoGrid
        <> tagDescriptionField cfg sidecarSet scope
        <> scCtx

-- | Page identifier for a tag index page.
--   Page 1 → <tag>/index.html
--   Page N → <tag>/page/N/index.html
tagPageId :: String -> PageNumber -> Identifier
tagPageId tag 1 = fromFilePath $ tag ++ "/index.html"
tagPageId tag n = fromFilePath $ tag ++ "/page/" ++ show n ++ "/index.html"

-- | Generate index pages for every tag. Tags with at most
--   'tagPaginationThreshold' items render a single page with the full
--   list; larger tags fall back to server-side pagination at
--   'tagPageSize' per page, partitioned and sorted by the same
--   display-date key ('sortAndGroupByDisplayAt') so ordering across pages
--   is monotone. 'tagPageContext' says what each page shows.
--
--   @baseCtx@ should be @siteCtx@ (passed in to avoid a circular import).
applyTagRules :: SiteConfig -> Tags -> Context String -> Rules ()
applyTagRules cfg tags baseCtx = do
    -- Hakyll's @**/*@ glob needs a subdirectory level, so the flat and
    -- nested sidecar paths each need their own pattern. Keep this list
    -- in sync with the matching rule in Site.rules.
    sidecarIds <- getMatches ("content/tag-meta/*.md" .||. "content/tag-meta/**/*.md")
    let sidecarSet = Set.fromList sidecarIds
        mapOn      = switchOn cfg "tag-map"
        page tag pat extra = do
            route idRoute
            compile $ do
                items  <- recentFirstByDisplay =<< loadAll (pat .&&. hasNoVersion)
                ctx    <- tagPageContext tags sidecarSet tag items
                mapCtx <- if mapOn
                    then maybe mempty (constField "tag-map" . scrollable) . lookup tag
                             <$> loadTagMaps
                    else return mempty
                makeItem ""
                    >>= inDefault "templates/tag-index.html" (ctx <> mapCtx <> extra <> baseCtx)
    when mapOn $ tagMapRules tags baseCtx
    anchoredTagsRules "_dependencies/tags" tags $ \tag pat -> do
        let itemCount = length (fromMaybe [] (lookup tag (tagsMap tags)))
        if itemCount <= tagPaginationThreshold
            then page tag pat mempty
            else do
                -- The sidecar's featured pieces lead the first page, so
                -- they go first before the tag is cut into pages.
                featuredIds <- if sidecarIdentifier tag `Set.member` sidecarSet
                    then map fromFilePath . fromMaybe [] . lookupStringList "featured"
                             <$> getMetadata (sidecarIdentifier tag)
                    else return []
                paginate <- buildPaginateWith (sortAndGroupFeaturedAt featuredIds tagPageSize)
                                              pat (tagPageId tag)
                paginateRules paginate $ \pageNum pat' ->
                    page tag pat' (paginateContext paginate pageNum)

-- | 'tagsRules', with the dependency every tag page shares held once.
--
--   'tagsRules' gives each tag page the same pattern dependency: the
--   pattern the tags were built from, with every identifier it matched.
--   Hakyll re-matches each held pattern against every identifier on every
--   build, once per holder: 54 tag pages × 76 ms, 4 s of every pass's
--   out-of-date check (audit H06). Here an anchor item holds the pattern,
--   and each tag page depends on the anchor by identifier, which costs a
--   set lookup. The effect is the same: the anchor is out of date when a
--   page joins or leaves the pattern or any matched page changes, and so,
--   through it, is every tag page.
anchoredTagsRules :: Identifier -> Tags -> (String -> Pattern -> Rules ()) -> Rules ()
anchoredTagsRules anchor tags rules = do
    rulesExtraDependencies [tagsDependency tags] $
        create [anchor] $ compile (makeItem ("" :: String))
    forM_ (tagsMap tags) $ \(tag, identifiers) ->
        rulesExtraDependencies [IdentifierDependency anchor] $
            create [tagsMakeId tags tag] $
                rules tag (fromList identifiers)



-- | Partition identifiers into pages of @n@, most recent first by
--   /display/ date — the same key ('displaySortKey') that
--   'recentFirstByDisplay' sorts by within each rendered page, new pieces
--   before revisions on a shared day — so cross-page ordering is monotone. With creation-date partitioning
--   (plain @sortRecentFirst@), a recently revised old item stayed on a
--   late page but jumped to its top; now it migrates to the early page
--   where its displayed date says it belongs.
--
--   An undated page sorts after every dated one, as on the page itself
--   ('recentFirstByDisplay').
sortAndGroupByDisplayAt :: (MonadMetadata m, MonadFail m)
                        => Int -> [Identifier] -> m [[Identifier]]
sortAndGroupByDisplayAt = sortAndGroupFeaturedAt []

-- | 'sortAndGroupByDisplayAt' with some identifiers first, in their order:
--   a tag's sidecar's @featured:@, which lead its first page however old
--   they are.
sortAndGroupFeaturedAt :: (MonadMetadata m, MonadFail m)
                       => [Identifier] -> Int -> [Identifier] -> m [[Identifier]]
sortAndGroupFeaturedAt featured n ids = do
    keyed <- mapM (\i -> (,) <$> displaySortKey i <*> pure i) ids
    let sorted = map snd (sortBy (comparing fst) keyed)
        lead   = [ f | f <- featured, f `elem` ids ]
    return $ paginateEvery n $ lead ++ filter (`notElem` lead) sorted


-- | @$description$@ for a tag index: what is filed under it, with the
--   tag's own sidecar tooltip when it has one.
--
--   Composed /before/ @baseCtx@ so it wins over 'Contexts.descriptionField'.
--   That fallback excerpts the page's first body paragraph, which on a
--   list page is the first /item's/ abstract — a description of one essay,
--   presented as a description of the index. Audit C01 asks for a clear
--   fallback policy for list pages; this is it, and the @tooltip:@ in
--   @content\/tag-meta\/\<tag\>.md@ (the same string the home-page portal
--   grid shows on hover) is the author's own one-line gloss of the tag.
tagDescriptionField :: SiteConfig -> Set Identifier -> String -> Context String
tagDescriptionField cfg sidecarSet tag = field "description" $ \_ -> do
    mTip <- (Just <$> glossOf sidecarSet tag) <|> return Nothing
    return $ case mTip of
        Just tip -> filed ++ " \8212 " ++ tip ++ "."
        Nothing  -> filed ++ "."
  where
    filed = "Everything on this site filed under \8220" ++ tagDisplayName cfg tag ++ "\8221"



-- ---------------------------------------------------------------------------
-- The map
-- ---------------------------------------------------------------------------

-- | The item holding every drawing of the map: the whole map under @""@
--   and each tag's lit version under the tag, as a 'show'n association
--   list. One layout per build, shared by the tag pages and /map/.
tagMapId :: Identifier
tagMapId = "_tag-map/drawings"

loadTagMaps :: Compiler [(String, String)]
loadTagMaps = read . itemBody <$> load tagMapId

-- | The SVG in the container that lets it pan sideways at phone widths,
--   where the labels would otherwise be too small to read (tag-map.css).
scrollable :: String -> String
scrollable svg = "<div class=\"tag-map-scroll\">" ++ svg ++ "</div>"

-- | The tags and pieces the map draws: every tag outside the sections'
--   own trees (the photographs' places have the photography pages), and
--   the pieces filed under them, each with the tags it is written with.
tagMapInput :: Tags -> Compiler ([MapTag], [MapPiece])
tagMapInput tags = do
    cfg <- loadConfig
    let owned    = sectionOwnedTopLevelTags cfg
        top      = takeWhile (/= '/')
        entries  = [ e | e@(t, _) <- tagsMap tags, top t `notElem` owned ]
        onMap    = Set.fromList (map fst entries)
        facetOf t = fromMaybe "" (listToMaybe [ portalFacet p | p <- sitePortals cfg
                                                              , portalTag p == top t ])
        mapTags  = [ MapTag t (tagDisplayName cfg t) (fromMaybe ("/" ++ t ++ "/") (tagPageUrl cfg t))
                            (length ids) (parentOf t) (facetOf t)
                   | (t, ids) <- entries ]
    pieces <- forM (nub (sort (concatMap snd entries))) $ \i -> do
        title <- fromMaybe (toFilePath i) <$> getMetadataField i "title"
        ts    <- filter (`Set.member` onMap) <$> getTags i
        r     <- getRoute i
        let url = maybe "" (\p -> "/" ++ (if takeFileName p == "index.html" then dropFileName p else p)) r
        return (MapPiece (toFilePath i) (stripTags title) url ts)
    return (mapTags, pieces)

-- | The drawings, and /map/: the whole map, and beneath it every tag on
--   it as a list, which is what a screen reader and a search engine read.
tagMapRules :: Tags -> Context String -> Rules ()
tagMapRules tags baseCtx = do
    rulesExtraDependencies [IdentifierDependency "_dependencies/tags"] $
        create [tagMapId] $ compile $ do
            (mapTags, pieces) <- tagMapInput tags
            let layout = layoutMap mapTags pieces
            makeItem $ show $ ("", renderMap layout Nothing)
                : [ (mtTag t, renderMap layout (Just (mtTag t))) | t <- mapTags ]
    create ["map/index.html"] $ do
        route idRoute
        compile $ do
            cfg      <- loadConfig
            drawings <- loadTagMaps
            (mapTags, pieces) <- tagMapInput tags
            let portalOrder = map portalTag (sitePortals cfg)
                rank t = (fromMaybe (length portalOrder) (lookup (mtTag t) (zip portalOrder [0 ..])),
                          map toLower (mtName t))
                tops   = sortBy (comparing rank) (filter (isNothing . mtParent) mapTags)
                kids t = sortBy (comparing rank) [ c | c <- mapTags, mtParent c == Just (mtTag t) ]
                filedUnder t = sortBy (comparing (map toLower . mpTitle))
                                      [ q | q <- pieces, mtTag t `elem` mpTags q ]
                link url label = "<a href=\"" ++ escapeHtml url ++ "\">" ++ escapeHtml label ++ "</a>"
                -- The whole tree, to any depth, and under each tag the
                -- pieces written with it: every destination the drawing
                -- has, in the tab order, as the drawing's are not.
                node t =
                    let inner = map node (kids t)
                             ++ [ "<li class=\"tag-map-index-piece\">" ++ link (mpUrl q) (mpTitle q) ++ "</li>"
                                | q <- filedUnder t ]
                    in  "<li>" ++ link (mtUrl t) (mtName t)
                        ++ "<span class=\"tag-within-count\">" ++ show (mtCount t) ++ "</span>"
                        ++ (if null inner then "" else "<ul>" ++ concat inner ++ "</ul>")
                        ++ "</li>"
                index = "<ul class=\"tag-map-index-list\">" ++ concatMap node tops ++ "</ul>"
                facts = mapFacts mapTags pieces
                ctx = constField "title" "Map"
                   <> constField "description"
                        ("Every tag on this site and the pieces filed under it, drawn as one map. "
                         ++ "A section's own tags, such as the photographs' places, are on its pages instead.")
                   <> constField "map-facts" facts
                   <> constField "map-index" index
                   <> constField "list-page" "true"
                   <> constField "tag-map" (scrollable (fromMaybe "" (lookup "" drawings)))
            makeItem "" >>= inDefault "templates/tag-map.html" (ctx <> baseCtx)


-- | "17 tags and 31 pieces": what the whole map draws.
mapFacts :: [MapTag] -> [MapPiece] -> String
mapFacts mapTags pieces = show (length mapTags) ++ " tags and " ++ show (length pieces) ++ " pieces"


-- ---------------------------------------------------------------------------
-- The home page's shelf of portals
-- ---------------------------------------------------------------------------

-- | A portal as a spine on the home page's shelf.
data Spine = Spine
    { spinePortal  :: Portal
    , spineCount   :: Int            -- ^ the pieces under it
    , spineCaption :: String         -- ^ its gloss, else its facts
    , spineCover   :: Maybe String   -- ^ its sidecar's @cover:@, an image
    }

-- | The home page's shelf (switch @portal-shelf@, templates/partials/
--   portal-shelf.html): site.yaml's portals as spines, in their order, a
--   run of portals with one facet standing together, each spine as thick
--   as the pieces under it. A portal's pieces are those its tag page
--   lists (the tag index, its subtags' with it) and those published at its
--   address by a section, which is how a form (Poetry, Music) that is a
--   section and no tag has any. With tag-map on, the whole map stands open under the
--   shelf (@$shelf-map$@), and portal-shelf.js lights a portal's region on
--   it when its spine is pointed at; pointing at the map itself turns the
--   caption to the tag or piece pointed at (@$for(shelf-map-tags)$@ gives
--   each tag's name and caption).
--
--   The caption a spine gives is its gloss (the sidecar's @tooltip:@),
--   else its facts in a phrase ("seven essays, May to November 2026");
--   a portal the map does not draw (a form) may name a @cover:@ image in
--   its sidecar to show instead.
portalShelfContext :: Tags -> Compiler (Context String)
portalShelfContext tags = do
    cfg        <- loadConfig
    sidecarSet <- Set.fromList <$> getMatches ("content/tag-meta/*.md" .||. "content/tag-meta/**/*.md")
    ids        <- getMatches (tagIndexable cfg
                              .||. ((blogPattern .||. poetryPattern .||. fictionPattern .||. musicPattern)
                                    .&&. hasNoVersion))
    -- Where each piece is published: its section's address, which is the
    -- portal's when the portal is a section (a site may serve a section at
    -- an address of its own, as a blog at /notebook/).
    placed <- forM ids $ \i -> do
        r <- getRoute i
        return (i, sectionAddress <$> (sectionOfRoute cfg =<< r))
    -- The tags each piece the tag pages file is written with: a section's
    -- own tag ("poetry") has no key in the tag index, the section having
    -- its address, so an essay tagged with it is found here.
    indexable <- getMatches (tagIndexable cfg)
    written   <- forM indexable $ \i -> (,) i <$> getTags i
    spines <- forM (sitePortals cfg) $ \p -> do
        -- A portal's pieces: everything the tag pages file under its tag
        -- or any tag beneath it (the tag index, and the tags as written,
        -- for a section's own tag, which has no key in the index), and
        -- what its section publishes; each once.
        let t      = portalTag p
            under' k = k == t || (t ++ "/") `isPrefixOf` k
            tagged = Set.fromList ([ i | (k, is) <- tagsMap tags, under' k, i <- is ]
                                ++ [ i | (i, ts) <- written, any under' ts ])
            inSec  = Set.fromList [ i | (i, Just a) <- placed, a == t ]
            under  = Set.toList (tagged `Set.union` inSec)
        pieces <- mapM (pieceOf cfg) under
        gloss  <- (Just <$> glossOf sidecarSet t) <|> return Nothing
        cover  <- if sidecarIdentifier t `Set.member` sidecarSet
                      then fmap trim . lookupString "cover" <$> getMetadata (sidecarIdentifier t)
                      else return Nothing
        let facts = countPhrase (map pieceKind pieces)
                    ++ maybe "" (", " ++) (spanPhrase (mapMaybe pieceWhen pieces))
        return (Spine p (length under) (fromMaybe facts gloss) (cover >>= \c -> if null c then Nothing else Just c))
    mapCtx <- if not (switchOn cfg "tag-map") then return mempty else do
        drawings          <- loadTagMaps
        (mapTags, pieces) <- tagMapInput tags
        -- Each tag on the map with the caption the shelf gives it when it
        -- is pointed at on the map: its gloss, else its facts.
        captions <- forM mapTags $ \t -> do
            gloss <- (Just <$> glossOf sidecarSet (mtTag t)) <|> return Nothing
            ps    <- mapM (pieceOf cfg) (fromMaybe [] (lookup (mtTag t) (tagsMap tags)))
            let facts = countPhrase (map pieceKind ps)
                        ++ maybe "" (", " ++) (spanPhrase (mapMaybe pieceWhen ps))
            return (t, fromMaybe facts gloss)
        let mapTagCtx = field "map-tag"         (return . escapeHtml . mtTag . fst . itemBody)
                     <> field "map-tag-name"    (return . escapeHtml . mtName . fst . itemBody)
                     <> field "map-tag-url"     (return . escapeHtml . mtUrl . fst . itemBody)
                     <> field "map-tag-caption" (return . escapeHtml . snd . itemBody)
        return $ constField "shelf-map" (fromMaybe "" (lookup "" drawings))
              <> constField "shelf-map-facts" (mapFacts mapTags pieces)
              <> listField "shelf-map-tags" mapTagCtx
                     (mapM (\(n, c) -> return (Item (fromFilePath ("_shelf-map-tag/" ++ show n)) c))
                           (zip [(0 :: Int) ..] captions))
              -- head.html and default.html load the map's stylesheet and
              -- script on a page with a map.
              <> constField "tag-map" "shelf"
    let portal   = spinePortal . itemBody
        spineCtx =
               field "shelf-name"    (return . escapeHtml . portalName . portal)
            <> field "shelf-tag"     (return . escapeHtml . portalTag . portal)
            <> field "shelf-url"     (\i -> return (escapeHtml ("/" ++ portalTag (portal i) ++ "/")))
            <> field "shelf-facet"   (\i -> case portalFacet (portal i) of
                                                "" -> noResult "no facet"
                                                f  -> return (escapeHtml f))
            <> field "shelf-size"    (return . show . spineCount . itemBody)
            <> field "shelf-count"   (\i -> case spineCount (itemBody i) of
                                                0 -> noResult "nothing under it yet"
                                                n -> return (show n))
            <> field "shelf-caption" (return . escapeHtml . spineCaption . itemBody)
            <> field "shelf-cover"   (maybe (noResult "no cover") (return . escapeHtml) . spineCover . itemBody)
        facetCtx = listFieldWith "shelf-portals" spineCtx (mapM makeItem . itemBody)
        facets   = groupBy ((==) `F.on` (portalFacet . spinePortal)) spines
    return $ listField "shelf-facets" facetCtx (mapM makeItem facets) <> mapCtx


-- ---------------------------------------------------------------------------
-- A tag page's optional gallery, and its contents' months
-- ---------------------------------------------------------------------------

monthName :: UTCTime -> String
monthName t = let (_, m, _) = toGregorian (utctDay t)
              in  words "January February March April May June July August September October November December" !! (m - 1)

-- | Hakyll has no contramap for contexts; a context over the first of a
--   pair, for the contents' entries (an item with its display date).
contramapItem :: (a -> b) -> Context b -> Context a
contramapItem f (Context c) = Context $ \key args item -> c key args (fmap f item)

-- | A figure in a tag's gallery: its image, and the piece it is from.
data Plate = Plate
    { gSrc    :: String   -- ^ the image, a path on the site
    , gUrl    :: String   -- ^ the piece's page
    , gTitle  :: String   -- ^ the piece's title, for the image's alt and title
    , gAspect :: String   -- ^ width over height, for the row's proportions
    }

-- | The sidecar's @gallery:@, set by hand: images from the writings filed
--   here, each with the piece it is from, shown in one justified row
--   (each as wide as its shape needs, at one height) without frames or
--   captions. A gallery that names a piece the build does not publish
--   fails, naming it.
--
--   > gallery:
--   >   - image: /essays/example/figures/plot.svg
--   >     piece: content/essays/example/index.md
galleryOf :: Set Identifier -> String -> Compiler [Plate]
galleryOf sidecarSet tag
    | sidecarId `Set.notMember` sidecarSet = return []
    | otherwise = do
        meta <- getMetadata sidecarId
        case KM.lookup "gallery" meta of
            Nothing -> return []
            Just (Y.Array v) -> mapM plate (foldr (:) [] v)
            Just _ -> fail (toFilePath sidecarId ++ ": gallery must be a list of image and piece")
  where
    sidecarId = sidecarIdentifier tag
    plate (Y.Object o)
        | Just (Y.String img) <- KM.lookup "image" o
        , Just (Y.String pc)  <- KM.lookup "piece" o = do
            let src   = T.unpack img
                ident = fromFilePath (T.unpack pc)
            r <- getRoute ident
            url <- maybe (fail (toFilePath sidecarId ++ ": gallery piece is not published: " ++ T.unpack pc))
                         (return . pageAddress) r
            title <- fromMaybe url <$> getMetadataField ident "title"
            ar <- unsafeCompiler (imageAspect src)
            return (Plate src url title ar)
    plate _ = fail (toFilePath sidecarId ++ ": each gallery entry has an image and a piece")
    pageAddress r = "/" ++ (if "index.html" `isSuffixOf` r then take (length r - length ("index.html" :: String)) r else r)

-- | Width over height of an image at a path on the site, from its source
--   in content/ or static/: a @.dims.yaml@ sidecar beside it, or an SVG's
--   own viewBox; 1.5 when neither says.
imageAspect :: String -> IO String
imageAspect url = do
    found <- filterIO doesFileExist [ root ++ url | root <- ["content", "static"] ]
    case found of
        []       -> return fallback
        path : _ -> do
            dims <- readDims (path ++ ".dims.yaml")
            case dims of
                Just (w, h) | w > 0 && h > 0 -> return (show (fromIntegral (round (fromIntegral w / fromIntegral h * 10000 :: Double) :: Integer) / 10000 :: Double))
                _ | ".svg" `isSuffixOf` path -> fromMaybe fallback <$> svgAspect path
                  | otherwise -> return fallback
  where
    fallback = "1.5"
    filterIO f = fmap concat . mapM (\x -> (\b -> [x | b]) <$> f x)
    readDims file = do
        exists <- doesFileExist file
        if not exists then return Nothing else do
            decoded <- Y.decodeFileEither file
            return $ case decoded of
                Right (Y.Object o) -> (,) <$> (int =<< KM.lookup "width" o) <*> (int =<< KM.lookup "height" o)
                _ -> Nothing
    int :: Y.Value -> Maybe Int
    int (Y.Number n) = Sci.toBoundedInteger n
    int _            = Nothing

