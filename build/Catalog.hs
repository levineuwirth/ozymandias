{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Music catalog: featured works + grouped-by-category listing.
-- Renders HTML directly (same pattern as Backlinks.hs) to avoid the
-- complexity of nested listFieldWith.
module Catalog
    ( musicCatalogCtx
    ) where

import Data.Char     (isSpace, toLower)
import Data.List     (groupBy, isPrefixOf, sortBy)
import Data.Maybe    (fromMaybe)
import Data.Ord      (comparing)
import Data.Aeson    (Value (..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector       as V
import qualified Data.Text         as T
import Hakyll
import Contexts (siteCtx)

-- ---------------------------------------------------------------------------
-- Entry type
-- ---------------------------------------------------------------------------

data CatalogEntry = CatalogEntry
    { ceTitle           :: String
    , ceUrl             :: String
    , ceYear            :: Maybe String
    , ceDuration        :: Maybe String
    , ceInstrumentation :: Maybe String
    , ceCategory        :: String      -- defaults to "other"
    , ceFeatured        :: Bool
    , ceHasScore        :: Bool
    , ceHasRecording    :: Bool
    }

-- ---------------------------------------------------------------------------
-- Category helpers
-- ---------------------------------------------------------------------------

categoryOrder :: [String]
categoryOrder = ["orchestral","chamber","solo","vocal","choral","electronic","other"]

categoryLabel :: String -> String
categoryLabel "orchestral" = "Orchestral"
categoryLabel "chamber"    = "Chamber"
categoryLabel "solo"       = "Solo"
categoryLabel "vocal"      = "Vocal"
categoryLabel "choral"     = "Choral"
categoryLabel "electronic" = "Electronic"
categoryLabel _            = "Other"

categoryRank :: String -> Int
categoryRank c = fromMaybe (length categoryOrder)
                            (lookup c (zip categoryOrder [0..]))

-- ---------------------------------------------------------------------------
-- Parsing helpers
-- ---------------------------------------------------------------------------

-- | @featured: true@ in YAML becomes Bool True in Aeson; also accept the
-- string "true" in case the author quotes it.
isFeatured :: Metadata -> Bool
isFeatured meta =
    case KM.lookup "featured" meta of
        Just (Bool True)     -> True
        Just (String "true") -> True
        _                    -> False

-- | True if a @recording@ key is present, or any movement has an @audio@ key.
hasRecordingMeta :: Metadata -> Bool
hasRecordingMeta meta =
    KM.member "recording" meta || anyMovHasAudio meta
  where
    anyMovHasAudio m =
        case KM.lookup "movements" m of
            Just (Array v) -> any movHasAudio (V.toList v)
            _              -> False
    movHasAudio (Object o) = KM.member "audio" o
    movHasAudio _          = False

-- | Parse a year: accepts Number (e.g. @year: 2019@) or String.
parseYear :: Metadata -> Maybe String
parseYear meta =
    case KM.lookup "year" meta of
        Just (Number n) -> Just $ show (floor (fromRational (toRational n) :: Double) :: Int)
        Just (String t) -> Just (T.unpack t)
        _               -> Nothing

parseCatalogEntry :: Item String -> Compiler (Maybe CatalogEntry)
parseCatalogEntry item = do
    meta   <- getMetadata (itemIdentifier item)
    mRoute <- getRoute (itemIdentifier item)
    case mRoute of
        Nothing -> return Nothing
        Just r  -> do
            let title  = fromMaybe "(untitled)" (lookupString "title" meta)
                url    = "/" ++ r
                year   = parseYear meta
                dur    = lookupString "duration" meta
                instr  = lookupString "instrumentation" meta
                cat    = fromMaybe "other" (lookupString "category" meta)
            return $ Just CatalogEntry
                { ceTitle           = title
                , ceUrl             = url
                , ceYear            = year
                , ceDuration        = dur
                , ceInstrumentation = instr
                , ceCategory        = cat
                , ceFeatured        = isFeatured meta
                , ceHasScore        = not (null (fromMaybe [] (lookupStringList "score-pages" meta)))
                , ceHasRecording    = hasRecordingMeta meta
                }

-- ---------------------------------------------------------------------------
-- HTML rendering
-- ---------------------------------------------------------------------------
--
-- Trust model: per the site convention (see also Stats.hs:pageLink),
-- frontmatter @title@ values are author-controlled trusted HTML and may
-- contain inline markup such as @<em>...</em>@. They are emitted
-- pre-escaped — but we still escape every other interpolated frontmatter
-- value (year, duration, instrumentation) and sanitize hrefs through
-- 'safeHref', so a stray @<@ in those fields cannot break the markup.

-- | Defense-in-depth href sanitiser. Mirrors 'Stats.isSafeUrl'.
safeHref :: String -> String
safeHref u =
    let norm = map toLower (dropWhile isSpace u)
    in  if not ("//" `isPrefixOf` norm)
           && any (`isPrefixOf` norm) ["/", "https://", "mailto:", "#"]
        then escAttr u
        else "#"

escAttr :: String -> String
escAttr = concatMap esc
  where
    esc '&'  = "&amp;"
    esc '<'  = "&lt;"
    esc '>'  = "&gt;"
    esc '"'  = "&quot;"
    esc '\'' = "&#39;"
    esc c    = [c]

escText :: String -> String
escText = concatMap esc
  where
    esc '&' = "&amp;"
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc c   = [c]

renderIndicators :: CatalogEntry -> String
renderIndicators e = concatMap render
    [ (ceHasScore e,     "<span class=\"catalog-ind\" title=\"Score available\">&#9724;</span>")
    , (ceHasRecording e, "<span class=\"catalog-ind\" title=\"Recording available\">&#9834;</span>")
    ]
  where
    render (True,  s) = s
    render (False, _) = ""

renderEntry :: CatalogEntry -> String
renderEntry e = concat
    [ "<li class=\"catalog-entry\">"
    ,   "<div class=\"catalog-entry-main\">"
    ,     "<a class=\"catalog-title\" href=\"", safeHref (ceUrl e), "\">"
    ,       ceTitle e
    ,     "</a>"
    ,     renderIndicators e
    ,     maybe "" (\y -> "<span class=\"catalog-year\">" ++ escText y ++ "</span>") (ceYear e)
    ,     maybe "" (\d -> "<span class=\"catalog-duration\">" ++ escText d ++ "</span>") (ceDuration e)
    ,   "</div>"
    ,   maybe "" (\i -> "<div class=\"catalog-instrumentation\">" ++ escText i ++ "</div>") (ceInstrumentation e)
    , "</li>"
    ]

renderCategorySection :: String -> [CatalogEntry] -> String
renderCategorySection cat entries = concat
    [ "<section class=\"catalog-section\">"
    ,   "<h2 class=\"catalog-section-title\">", escText (categoryLabel cat), "</h2>"
    ,   "<ul class=\"catalog-list\">"
    ,   concatMap renderEntry entries
    ,   "</ul>"
    , "</section>"
    ]

-- ---------------------------------------------------------------------------
-- Load all compositions (excluding the catalog index itself)
-- ---------------------------------------------------------------------------

loadEntries :: Compiler [CatalogEntry]
loadEntries = do
    items   <- loadAll ("content/music/*/index.md" .&&. hasNoVersion)
    mItems  <- mapM parseCatalogEntry items
    return [e | Just e <- mItems]

-- ---------------------------------------------------------------------------
-- Context fields
-- ---------------------------------------------------------------------------

-- | @$featured-works$@: HTML list of featured entries; noResult when none.
featuredWorksField :: Context String
featuredWorksField = field "featured-works" $ \_ -> do
    entries <- loadEntries
    let featured = filter ceFeatured entries
    if null featured
        then fail "no featured works"
        else return $
               "<ul class=\"catalog-list catalog-featured-list\">"
            ++ concatMap renderEntry featured
            ++ "</ul>"

-- | @$has-featured$@: present when at least one composition is featured.
hasFeaturedField :: Context String
hasFeaturedField = field "has-featured" $ \_ -> do
    entries <- loadEntries
    if any ceFeatured entries then return "true" else fail "no featured works"

-- | @$catalog-by-category$@: HTML for all category sections.
-- Sorted by canonical category order; if no compositions exist yet,
-- returns a placeholder paragraph.
catalogByCategoryField :: Context String
catalogByCategoryField = field "catalog-by-category" $ \_ -> do
    entries <- loadEntries
    if null entries
        then return "<p class=\"catalog-empty\">Works forthcoming.</p>"
        else do
            let sorted  = sortBy (comparing (categoryRank . ceCategory)) entries
                grouped = groupBy (\a b -> ceCategory a == ceCategory b) sorted
            return $ concatMap renderGroup grouped
  where
    -- groupBy on a non-empty list yields non-empty sublists, but pattern
    -- matching is total whereas 'head' is not.
    renderGroup []        = ""
    renderGroup g@(e : _) = renderCategorySection (ceCategory e) g

musicCatalogCtx :: Context String
musicCatalogCtx =
    constField "catalog" "true"
    <> hasFeaturedField
    <> featuredWorksField
    <> catalogByCategoryField
    <> siteCtx
