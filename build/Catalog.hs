{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Music catalog: a shelf of the works, then the catalogue proper.
--
-- The shelf holds every work as the spine of a bound score, oldest to
-- newest, each as thick as its score is long; one work stands face-out,
-- showing its first page. Below it, the catalogue lists the works by genre,
-- newest first, as a programme would set them.
--
-- Renders HTML directly (same pattern as Backlinks.hs) to avoid the
-- complexity of nested listFieldWith.
module Catalog
    ( musicCatalogCtx
    ) where

import Data.List       (groupBy, intercalate, sortBy, stripPrefix)
import Data.Maybe      (fromMaybe, listToMaybe)
import Data.Ord        (Down (..), comparing)
import Data.Aeson      (Value (..))
import qualified Data.Aeson.Key    as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector       as V
import qualified Data.Text         as T
import System.FilePath (takeDirectory, (</>))
import Data.Time.Calendar (Day)
import Data.Time.Format   (defaultTimeLocale, parseTimeM)
import Hakyll hiding (escapeHtml)
import Utils    (escapeHtml, formatIso, isoDate, isSafeUrl, writerlyDate)
import Contexts (durationPrimes, scorePageList, scoreThumb, siteCtx, svgAspect)

-- ---------------------------------------------------------------------------
-- Entry type
-- ---------------------------------------------------------------------------

data CatalogEntry = CatalogEntry
    { ceTitle           :: String
    , ceUrl             :: String
    , ceYear            :: Maybe String
    , ceOpus            :: Maybe String
    , ceDuration        :: Maybe String
    , ceInstrumentation :: Maybe String
    , ceCategory        :: String      -- defaults to "other"
    , ceFeatured        :: Bool
    , ceHasRecording    :: Bool
    , ceWithheld        :: [String]       -- movements listed with a status
    , cePages           :: Int
    , ceFirstPage       :: Maybe String   -- absolute URL of page 1's image: its thumbnail, or the page
    , ceAspect          :: Maybe String   -- page 1's width / height
    , ceSortKey         :: String         -- year, completion, then date: chronological
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

-- | The movements a partial work lists but does not publish — those with
--   a @status@ — as the names the catalogue will print ("III", not "III.").
withheldMovements :: Metadata -> [String]
withheldMovements meta =
    case KM.lookup "movements" meta of
        Just (Array v) ->
            [ reverse (dropWhile (== '.') (reverse (T.unpack n)))
            | Object o <- V.toList v
            , KM.member "status" o
            , Just (String n) <- [KM.lookup "name" o] ]
        _ -> []

-- | A @completed:@ date as sortable text, when it parses: "30 December
--   2024", "March 2026" and "2026-03-15" all order correctly; anything
--   else (or nothing) sorts before the dated works of its year.
completedKey :: Metadata -> String
completedKey meta = case lookupString "completed" meta of
    Nothing -> ""
    Just raw ->
        let formats = [writerlyDate, "%B %Y", isoDate]
            parsed  = listToMaybe [ d | f <- formats
                                      , Just d <- [parseTimeM True defaultTimeLocale f raw :: Maybe Day] ]
        in  maybe "" (formatIso) parsed

-- | A scalar that YAML may hand over as a number or a string:
--   @year: 2019@ and @opus: '17'@ alike.
parseScalar :: String -> Metadata -> Maybe String
parseScalar key meta =
    case KM.lookup (AK.fromString key) meta of
        Just (Number n) -> Just $ show (floor (fromRational (toRational n) :: Double) :: Int)
        Just (String t) -> Just (T.unpack t)
        _               -> Nothing

parseCatalogEntry :: Item String -> Compiler (Maybe CatalogEntry)
parseCatalogEntry item = do
    meta   <- getMetadata (itemIdentifier item)
    mRoute <- getRoute (itemIdentifier item)
    -- Through 'scorePageList' rather than reading @score-pages@ here, so the
    -- shelf cannot disagree with what the reader shows: a composition that
    -- declares its pages with @score-dir@ has no @score-pages@ key to find.
    pages  <- scorePageList item
    thumb  <- scoreThumb item
    let srcDir = takeDirectory (toFilePath (itemIdentifier item))
    aspect <- case pages of
        (p : _) -> unsafeCompiler (svgAspect (srcDir </> p))
        []      -> return Nothing
    case mRoute of
        Nothing -> return Nothing
        Just r  -> do
            let url    = "/" ++ r
                slugDir = takeDirectory url
                year   = parseScalar "year" meta
                -- Fold unknown categories into the canonical "other"
                -- bucket here: two distinct unknown values share a rank
                -- but would groupBy into separate groups, rendering as
                -- adjacent duplicate "Other" sections.
                rawCat = fromMaybe "other" (lookupString "category" meta)
                cat    = if rawCat `elem` categoryOrder then rawCat else "other"
            return $ Just CatalogEntry
                { ceTitle           = fromMaybe "(untitled)" (lookupString "title" meta)
                , ceUrl             = url
                , ceYear            = year
                , ceOpus            = parseScalar "opus" meta
                , ceDuration        = lookupString "duration" meta
                , ceInstrumentation = lookupString "instrumentation" meta
                , ceCategory        = cat
                , ceFeatured        = isFeatured meta
                , ceHasRecording    = hasRecordingMeta meta
                , ceWithheld        = withheldMovements meta
                , cePages           = length pages
                , ceFirstPage       = (\p -> slugDir ++ "/" ++ p) <$> maybe (listToMaybe pages) Just thumb
                , ceAspect          = aspect
                , ceSortKey         = fromMaybe "0000" year ++ "|"
                                      ++ completedKey meta ++ "|"
                                      ++ fromMaybe "" (lookupString "date" meta)
                }

-- ---------------------------------------------------------------------------
-- HTML rendering
-- ---------------------------------------------------------------------------
--
-- Trust model: per the site convention (see also Stats.hs:pageLink),
-- frontmatter @title@ values are author-controlled trusted HTML and may
-- contain inline markup such as @<em>...</em>@. They are emitted
-- pre-escaped — but we still escape every other interpolated frontmatter
-- value (year, opus, duration, instrumentation) and sanitize hrefs through
-- 'safeHref', so a stray @<@ in those fields cannot break the markup.

-- | Defense-in-depth href sanitiser ('Utils.isSafeUrl').
safeHref :: String -> String
safeHref u =
    if isSafeUrl u then escapeHtml u else "#"

escText :: String -> String
escText = concatMap esc
  where
    esc '&' = "&amp;"
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc c   = [c]

-- | "Symphony No. 5" and ", op. 17" as one title: the opus is part of how
--   a work is named in a catalogue.
titleHtml :: CatalogEntry -> String
titleHtml e = ceTitle e ++ maybe ""
    (\o -> "<span class=\"cat-opus\">, op.&nbsp;" ++ escText o ++ "</span>") (ceOpus e)

-- | "for orchestra", "for alto and piano" — the catalogue's second line.
forcesText :: CatalogEntry -> String
forcesText e = maybe "" (\i -> "for " ++ i) (ceInstrumentation e)

-- | Sheets drawn beneath a face-out score: the same thresholds as the
--   composition page's frontispiece ('Contexts.compositionCtx').
stackOf :: Int -> Int
stackOf n
    | n < 2     = 0
    | n < 16    = 1
    | n < 64    = 2
    | otherwise = 3

-- | The shelf. Spines run oldest to newest; the featured work — else the
--   newest — stands face-out at the end, and music-shelf.js turns any
--   spine a reader points at face-out in its place. Without scripting
--   the face-out simply stays put and every spine is still a link.
renderShelf :: [CatalogEntry] -> String
renderShelf entries = concat
    [ "<div class=\"shelf\" data-shelf>"
    ,   "<div class=\"shelf-row\">"
    ,     "<ol class=\"shelf-spines\" aria-label=\"The works, oldest first\">"
    ,       concatMap spine chron
    ,     "</ol>"
    ,     maybe "" faceOut shown
    ,   "</div>"
    , "</div>"
    ]
  where
    chron = sortBy (comparing ceSortKey) entries
    -- Only a work whose pages are in this build can stand face-out: a
    -- featured or newest work without them would leave no face-out at
    -- all, and with it no shelf previews for any other work.
    shown = listToMaybe
        [ e | e <- filter ceFeatured (reverse chron) ++ reverse chron
            , Just _ <- [ceFirstPage e] ]

    spine e = concat
        [ "<li><a class=\"shelf-spine\" href=\"", safeHref (ceUrl e), "\""
        , " style=\"--pages: ", show (cePages e), "\""
        , maybe "" (\p -> " data-page=\"" ++ safeHref p ++ "\"") (ceFirstPage e)
        , maybe "" (\a -> " data-aspect=\"" ++ escapeHtml a ++ "\"") (ceAspect e)
        , " data-stack=\"", show (stackOf (cePages e)), "\""
        , " data-meta=\"", escapeHtml (metaLine e), "\">"
        , "<span class=\"shelf-spine-title\">", titleHtml e, "</span>"
        , maybe "" (\y -> "<span class=\"shelf-spine-year\">" ++ escText y ++ "</span>") (ceYear e)
        , "</a></li>"
        ]

    -- A preview of a spine that is itself a link, so it stays out of the
    -- tab order and the accessibility tree: the spines carry the names.
    faceOut e = case ceFirstPage e of
        Nothing -> ""
        Just p  -> concat
            [ "<a class=\"shelf-faceout\" href=\"", safeHref (ceUrl e), "\""
            , " tabindex=\"-1\" aria-hidden=\"true\""
            , " data-stack=\"", show (stackOf (cePages e)), "\""
            , maybe "" (\a -> " style=\"--aspect: " ++ escapeHtml a ++ "\"") (ceAspect e)
            , ">"
            , "<img src=\"", safeHref p, "\" alt=\"\" decoding=\"async\">"
            , caption e
            , "</a>"
            ]

    -- The shelf label under the face-out score, on the board's edge. Inside
    -- the face-out link so it moves with it; spans rather than a <p>,
    -- which an <a> may not hold here.
    caption e = concat
        [ "<span class=\"shelf-caption\">"
        , "<span class=\"shelf-caption-title\">", titleHtml e, "</span>"
        , "<span class=\"shelf-caption-meta\">", escText (metaLine e), "</span>"
        , "</span>"
        ]

    metaLine e = case (forcesText e, ceYear e) of
        ("", Just y) -> y
        (f, Just y)  -> f ++ ", " ++ y
        (f, Nothing) -> f

-- | One work in the catalogue: the year hangs in the margin, a dotted
--   leader runs from the title to the duration, and the forces follow on
--   a second line — the composition page's movement list, one level up.
--   "ca." is dropped here; every duration in a catalogue is approximate,
--   and the work's own page says so.
renderEntry :: CatalogEntry -> String
renderEntry e = concat
    [ "<li class=\"cat-work\">"
    ,   "<a class=\"cat-work-row\" href=\"", safeHref (ceUrl e), "\">"
    ,     "<span class=\"cat-work-year\">", maybe "" escText (ceYear e), "</span>"
    ,     "<span class=\"cat-work-title\">", titleHtml e, "</span>"
    ,     "<span class=\"cat-work-leader\" aria-hidden=\"true\"></span>"
    ,     "<span class=\"cat-work-dur\">"
    ,       maybe "" (escText . durationPrimes . dropCa) (ceDuration e)
    ,     "</span>"
    ,     "<span class=\"cat-work-forces\">", escText (forcesText e)
    ,       if ceHasRecording e then "<span class=\"cat-work-rec\">with recording</span>" else ""
    ,       withheldNote (ceWithheld e)
    ,     "</span>"
    ,   "</a>"
    , "</li>"
    ]
  where
    dropCa d = fromMaybe d (stripPrefix "ca. " d)
    -- A partial work says so where its duration would otherwise read as
    -- the whole: "III in revision", "III and IV in revision".
    withheldNote [] = ""
    withheldNote ms = "<span class=\"cat-work-note\">" ++ escText (names ms)
                      ++ " in revision</span>"
    names [m]    = m
    names [a, b] = a ++ " and " ++ b
    names ms     = intercalate ", " (init ms) ++ " and " ++ last ms

renderCategorySection :: String -> [CatalogEntry] -> String
renderCategorySection cat entries = concat
    [ "<section class=\"cat-section\">"
    ,   "<h2 class=\"cat-section-title\">", escText (categoryLabel cat), "</h2>"
    ,   "<ol class=\"cat-list\">"
    ,   concatMap renderEntry (sortBy (comparing (Down . ceSortKey)) entries)
    ,   "</ol>"
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

-- | @$music-shelf$@: the shelf of works; noResult when there are none.
musicShelfField :: [CatalogEntry] -> Context String
musicShelfField entries = field "music-shelf" $ \_ ->
    if null entries then noResult "no works" else return (renderShelf entries)

-- | @$catalog-by-category$@: HTML for all category sections.
-- Sorted by canonical category order; if no compositions exist yet,
-- returns a placeholder paragraph.
catalogByCategoryField :: [CatalogEntry] -> Context String
catalogByCategoryField entries = field "catalog-by-category" $ \_ ->
    if null entries
        then return "<p class=\"cat-empty\">Works forthcoming.</p>"
        else do
            let sorted  = sortBy (comparing (categoryRank . ceCategory)) entries
                grouped = groupBy (\a b -> ceCategory a == ceCategory b) sorted
            return $ concatMap renderGroup grouped
  where
    -- groupBy on a non-empty list yields non-empty sublists, so the
    -- (e:_) pattern is structurally guaranteed in this call site.
    renderGroup g@(e : _) = renderCategorySection (ceCategory e) g
    renderGroup []        = ""   -- unreachable; satisfies coverage checker

musicCatalogCtx :: Compiler (Context String)
musicCatalogCtx = do
    -- Loaded once for both fields, which used to load and parse every
    -- composition each.
    entries <- loadEntries
    return $ constField "catalog" "true"
          <> musicShelfField entries
          <> catalogByCategoryField entries
          <> siteCtx
