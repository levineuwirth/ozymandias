{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Photography section — routing and per-page compilation.
--
--   Phase 1 (current): single-photo entries in flat and directory form,
--   plus the @/photography/@ landing page that lists every entry.
--
--   Phase 5 will extend this module with:
--     * collection-photo files (@content/photography/<series>/<photo>.md@)
--     * series landing pages
--     * @/photography/by-year/@ chronological indexes
--     * @/photography/contact-sheet/@ alternate view
--     * @/photography/feed.xml@ Atom feed
--     * @/photography/map/@ Leaflet map (Phase 4)
--
--   See @PHOTOGRAPHY.md@ at the repo root for the full design and
--   phased implementation plan.
module Photography
    ( photographyRules
    ) where

import           Control.Applicative    ((<|>))
import           Control.Monad          (forM, forM_)
import           Data.List              (intercalate, nub, sort, sortBy)
import qualified Data.Map.Strict        as Map
import           Data.Map.Strict        (Map)
import           Data.Maybe             (mapMaybe, fromMaybe, catMaybes)
import qualified Data.Set               as Set
import           Data.Set               (Set)
import           Data.Ord               (Down (..), comparing)
import           System.FilePath        (takeBaseName, takeDirectory, takeFileName, replaceExtension, (</>))
import           System.Directory       (doesFileExist, getFileSize)
import           Text.Printf            (printf)
import           Text.Read              (readMaybe)
import qualified Data.Aeson             as Aeson
import           Data.Aeson             (Value (..), (.=))
import qualified Data.Aeson.KeyMap      as KM
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import qualified Data.Vector            as V
import qualified Data.Scientific        as Sci
import           Hakyll
import           Config                (SiteConfig (..), FeedSpec (..), loadConfig, loadFeed, renderSiteFeed, photoFeed)
import           Compilers              (pageCompiler, photographyCompiler)
import           Contexts               (feedTitleField, photographyCtx, pageCtx, siteCtx,
                                         recentFirstByDisplay, feedMetaFields,
                                         photoVariantName)
import qualified Patterns               as P
import           Utils                  (exposureISO, formatBytes, inDefault, stripPrefixRoute)

-- ---------------------------------------------------------------------------
-- Rules
-- ---------------------------------------------------------------------------

-- | All photography rules. Called from 'Site.rules' once.
--
--   Order is intentional:
--
--     1. Co-located assets first (so the photo file is in @_site/@
--        before any page that references it is compiled — Hakyll's
--        dependency tracker handles this anyway, but the surface
--        ordering reads top-down by data flow).
--     2. Single-photo entries (flat + directory form).
--     3. Section landing at @/photography/@ — loaded after the
--        photo entries so its @loadAll photographyPattern@ resolves
--        each photo's frontmatter through 'photographyCtx'.
photographyRules :: SiteConfig -> Rules ()
photographyRules cfg = do
    -- A directory is a "series" iff it has @.md@ siblings alongside
    -- its @index.md@. Collected once at rule-gen time so the entry
    -- rule can branch on series-landing template selection without
    -- re-globbing per item.
    siblingIds <- getMatches
        ( "content/photography/*/*.md"
        .&&. complement "content/photography/*/index.md"
        .&&. complement P.unpublished
        )
    let seriesSlugs :: Set String
        seriesSlugs = Set.fromList
            [ takeFileName (takeDirectory (toFilePath ident))
            | ident <- siblingIds
            ]

    photographyAssetRules

    -- Dimension sidecars are read by Filters.Images inside 'unsafeCompiler'
    -- (via 'photographyCompiler' → 'Filters.applyAll'), which Hakyll cannot
    -- see. Audit B11: editing a sidecar left the page byte-identical,
    -- serving the previous width/height. They are claimed here with a
    -- no-route rule — a dependency on an unclaimed file can never fire,
    -- because Hakyll's modified set is intersected with the identifiers
    -- some rule claims — and 'photographyCompiler' depends on them for the
    -- pages whose Markdown has an image (audit H06).
    match (   "content/photography/*.dims.yaml"
         .||. "content/photography/*/*.dims.yaml") $ compile getResourceLBS

    photographyEntryRules seriesSlugs
    photographySeriesPhotoRules
    photographyLandingRules
    photographyMapDataRule
    photographyMapPageRule
    forM_ (filter photoFeed (siteFeeds cfg)) photographyFeedRule
    photographyByYearRules
    photographyContactSheetRule

-- ---------------------------------------------------------------------------
-- Assets
-- ---------------------------------------------------------------------------

-- | Co-located assets — the photo file itself, and (Phase 3) the
--   generated @{photo}.exif.yaml@ + @{photo}.palette.yaml@ sidecars.
--   Two patterns are matched in sequence:
--
--     * @content/photography/<asset>@        — flat-single co-located assets
--     * @content/photography/<slug>/<asset>@ — directory-form co-located assets
--
--   Markdown files are excluded from both rules; they're compiled by
--   'photographyEntryRules' and 'photographyLandingRules'.
--
--   The @.exif.yaml@ / @.palette.yaml@ sidecars produced by Phase 3
--   tooling will be added to the @.gitignore@ defense-in-depth list,
--   but copying them through the asset rule is harmless if a stray
--   one slips into the repo. The build is not load-bearing on
--   sidecar absence.
photographyAssetRules :: Rules ()
photographyAssetRules = do
    -- Top-level non-Markdown files (flat-single co-located assets, plus
    -- any future top-level photography assets like a landing-page hero).
    --
    -- Sidecars produced by the Phase 3 Python tooling
    -- (@{photo}.exif.yaml@, @{photo}.palette.yaml@) are excluded —
    -- they're consumed by Hakyll at build time and have no role in
    -- the deployed site.
    match ("content/photography/*"
           .&&. complement "content/photography/*.md"
           .&&. complement "content/photography/*.exif.yaml"
           .&&. complement "content/photography/*.palette.yaml"
           .&&. complement "content/photography/*.dims.yaml") $ do
        route $ stripPrefixRoute "content/"
        compile copyFileCompiler

    -- Directory-form entries' co-located assets. Excludes the entry's
    -- @index.md@, any other Markdown sibling files (collection photos
    -- in Phase 5), and every build-time YAML sidecar (EXIF, palette,
    -- dimensions).
    match ("content/photography/*/*"
           .&&. complement "content/photography/*/index.md"
           .&&. complement "content/photography/*/*.md"
           .&&. complement "content/photography/*/*.exif.yaml"
           .&&. complement "content/photography/*/*.palette.yaml"
           .&&. complement "content/photography/*/*.dims.yaml") $ do
        route $ stripPrefixRoute "content/"
        compile copyFileCompiler

-- ---------------------------------------------------------------------------
-- Single-photo entries
-- ---------------------------------------------------------------------------

-- | Compile each single-photo entry. Routing follows the essay
--   convention so the URL shape is predictable:
--
--     * @content/photography/<slug>.md@        → @photography/<slug>.html@
--     * @content/photography/<slug>/index.md@  → @photography/<slug>/index.html@
--
--   The @"content"@ snapshot is saved so a future @/photography/feed.xml@
--   (Phase 5) can render the rendered body as feed entry content.
photographyEntryRules :: Set String -> Rules ()
photographyEntryRules seriesSlugs =
    match P.photographyPattern $ do
        route photoEntryRoute
        compile $ do
            ident <- getUnderlying
            let fp              = toFilePath ident
                isIndex         = takeFileName fp == "index.md"
                slug            = takeFileName (takeDirectory fp)
                isSeriesLanding = isIndex && slug `Set.member` seriesSlugs
                template
                    | isSeriesLanding = "templates/photography-series.html"
                    | otherwise       = "templates/photography.html"
                ctx
                    | isSeriesLanding = seriesCtx
                    | otherwise       = photographyCtx
            photographyCompiler
                >>= saveSnapshot "content"
                >>= inDefault template ctx

-- | Sibling photos inside a series directory:
--   @content/photography/<series>/<photo>.md@. Compiled with the
--   single-photo template; routed to @<series>/<photo>/index.html@
--   so the URL is the canonical directory form (matches the rest of
--   the photography section's URL shape).
--
--   Series landings (@<series>/index.md@) are handled by
--   'photographyEntryRules' with the @photographyPattern@ match;
--   they're explicitly excluded here so the two rules don't double-route.
photographySeriesPhotoRules :: Rules ()
photographySeriesPhotoRules =
    match ("content/photography/*/*.md"
           .&&. complement "content/photography/*/index.md") $ do
        route $ customRoute $ \ident ->
            -- Drop @"content/"@ prefix and @".md"@ suffix, then append
            -- @"/index.html"@ to get directory-style URLs.
            let fp       = toFilePath ident
                rel      = drop (length contentPrefix) fp
                stripped = take (length rel - 3) rel
            in  stripped ++ "/index.html"
        compile $ photographyCompiler
            >>= saveSnapshot "content"
            >>= inDefault "templates/photography.html" photographyCtx
  where
    contentPrefix = "content/" :: String

-- | Context for series-landing pages. Extends 'photographyCtx' with a
--   @series-photos@ list field that loads the directory's sibling
--   photos (the @<series>/<photo>.md@ files), most-recent-first.
--
--   The @is-series@ const flag lets the consuming template branch on
--   whether to render single-photo chrome (figure + EXIF dl + body)
--   or series chrome (intro + photo grid + body).
seriesCtx :: Context String
seriesCtx =
    constField "is-series" "true"
    <> listFieldWith "series-photos" photographyCtx
           (\parent -> recentFirstByDisplay =<< loadSeriesChildrenFor parent)
    <> seriesStatsCtx
    <> photographyCtx


-- | Statistics for a series landing, in the register of @/build/@ and
--   @/stats/@ — the site already treats counting things as worth showing, and
--   a body of photographs has plenty to count.
--
--   Everything is derived at build time from the children's frontmatter and
--   the delivery files on disk, so nothing has to be maintained by hand and
--   nothing can drift from the set it describes. Fields return 'noResult'
--   when a series cannot answer them (no lens recorded, one capture date, an
--   image that is gitignored and absent on this machine), so the template's
--   @$if(...)$@ guards drop the row rather than printing a blank.
seriesStatsCtx :: Context String
seriesStatsCtx =
    field "series-frame-count" (fmap (show . length) . children)
    <> field "series-bytes"  (\i -> do
        ms <- childMeta i
        dir <- return (takeDirectory (toFilePath (itemIdentifier i)))
        sizes <- unsafeCompiler $ mapM (fileSize dir) ms
        let total = sum sizes
        if total <= 0 then noResult "no delivery files on disk"
                      else return (formatBytes total))
    <> field "series-captured-span" capturedSpan
    <> distinctField "series-cameras" "camera"
    <> distinctField "series-lenses"  "lens"
    <> field "series-focal-span" (spanOf "focal-length")
    <> field "series-iso-span"   isoSpan
    <> field "series-locations" locationList
  where
    children i = loadSeriesChildrenFor i

    childMeta i = children i >>= mapM (getMetadata . itemIdentifier)

    fileSize dir meta = case lookupString "photo" meta of
        Just p | not (null p) -> do
            let fp = dir </> p
            ok <- doesFileExist fp
            if ok then fromIntegral <$> getFileSize fp else return (0 :: Integer)
        _ -> return 0

    values key i = mapMaybe (lookupString key) <$> childMeta i

    -- Distinct values, in order, comma-joined. One camera reads as a fact
    -- about the series; five read as a list, which is also a fact about it.
    distinctField name key = field name $ \i -> do
        vs <- nub <$> values key i
        if null vs then noResult (name ++ ": nothing recorded")
                   else return (intercalate ", " vs)

    -- Capture span in prose rather than ISO. Two dates in one month collapse
    -- to "13–14 August 2026" instead of repeating the month and year; a
    -- single date prints alone rather than as "13–13 August".
    capturedSpan i = do
        vs <- sort . nub <$> values "captured" i
        case vs of
            []  -> noResult "no capture dates recorded"
            [x] -> return (prettyDate x)
            xs  -> return (spanDates (head xs) (last xs))

    spanDates a b =
        case (splitDate a, splitDate b) of
            (Just (ya, ma, da), Just (yb, mb, _))
                | ya == yb && ma == mb ->
                    da ++ "–" ++ dayOf b ++ " " ++ monthName ma ++ " " ++ ya
                | ya == yb ->
                    da ++ " " ++ monthName ma ++ " – " ++ dayOf b ++ " "
                       ++ monthName mb ++ " " ++ ya
            _ -> prettyDate a ++ " – " ++ prettyDate b

    dayOf d = maybe d (\(_, _, dd) -> dd) (splitDate d)

    prettyDate d = case splitDate d of
        Just (y, m, dd) -> dd ++ " " ++ monthName m ++ " " ++ y
        Nothing         -> d

    splitDate d = case splitOnDash d of
        [y, m, dd] -> Just (y, m, dropWhile (== '0') dd)
        _          -> Nothing

    splitOnDash str = case break (== '-') str of
        (a, [])       -> [a]
        (a, _ : rest) -> a : splitOnDash rest

    monthName m = case m of
        "01" -> "January";   "02" -> "February"; "03" -> "March"
        "04" -> "April";     "05" -> "May";      "06" -> "June"
        "07" -> "July";      "08" -> "August";   "09" -> "September"
        "10" -> "October";   "11" -> "November"; "12" -> "December"
        _    -> m

    -- Locations are stored as "Munich, Germany"; joining the full strings
    -- with commas produced "Munich, Germany, Nuremberg, Germany", which reads
    -- as four places. Keep the leading component and separate with a middot.
    locationList i = do
        vs <- nub . map (takeWhile (/= ',')) <$> values "location" i
        if null vs then noResult "no locations recorded"
                   else return (intercalate " · " vs)

    -- Numeric span: strips units, compares as numbers, reprints with the
    -- unit recovered from the first value. "18mm" and "55mm" must not sort
    -- as strings, where "18" > "155".
    spanOf key i = do
        vs <- nub <$> values key i
        let parsed = mapMaybe numericPrefix vs
            unit   = case vs of (v:_) -> dropWhile (\c -> c `elem` ("0123456789." :: String)) v
                                []    -> ""
        case parsed of
            []  -> noResult "no numeric values"
            [x] -> return (trimNum x ++ unit)
            xs  -> let lo = minimum xs
                       hi = maximum xs
                   in  if lo == hi then return (trimNum lo ++ unit)
                       else return (trimNum lo ++ "–" ++ trimNum hi ++ unit)

    -- ISO sits inside exposure: ("1/200 f/7.1 ISO 100"); a hand-written
    -- iso: wins where present. Reading only iso:, which the importers never
    -- write, left the row off every series page.
    isoSpan i = do
        ms <- childMeta i
        let iso m = (lookupString "iso" m >>= readMaybe)
                    <|> (lookupString "exposure" m >>= exposureISO)
        case nub (mapMaybe iso ms) of
            [] -> noResult "no ISO recorded"
            xs -> let lo = minimum xs :: Int
                      hi = maximum xs
                  in  return (if lo == hi then show lo else show lo ++ "–" ++ show hi)

    -- readMaybe: "50." or "." is not a number, and must not crash the build.
    numericPrefix v = readMaybe (takeWhile (\c -> c `elem` ("0123456789." :: String)) v) :: Maybe Double

    trimNum x = let r = round x :: Integer
                in  if fromIntegral r == x then show r else printf "%.1f" x

-- | A series landing's children: the photographs 'seriesCtx' lists and the
--   statistics fields count.
loadSeriesChildrenFor :: Item a -> Compiler [Item String]
loadSeriesChildrenFor parent = do
    let ident = itemIdentifier parent
        slug  = takeFileName (takeDirectory (toFilePath ident))
        pat   = fromGlob ("content/photography/" ++ slug ++ "/*.md")
            .&&. complement
                    (fromGlob ("content/photography/" ++ slug ++ "/index.md"))
            .&&. hasNoVersion
    loadAll pat

-- | Route a photography entry to its public URL. The pattern check on
--   @takeFileName@ distinguishes flat (@content/photography/<slug>.md@)
--   from directory-form (@content/photography/<slug>/index.md@) without
--   re-globbing, since Hakyll has already pre-filtered to entries
--   matching 'P.photographyPattern'.
--
--   Mirrors the essay rule's customRoute (@Site.rules@) but stripped of
--   the dev-mode draft branch — drafts are an essay-only concept right
--   now.
photoEntryRoute :: Routes
photoEntryRoute = customRoute $ \ident ->
    let fp      = toFilePath ident
        fname   = takeFileName fp
        isIndex = fname == "index.md"
    in  if isIndex
            -- content/photography/<slug>/index.md
            --   → photography/<slug>/index.html
            then replaceExtension (drop (length contentPrefix) fp) "html"
            -- content/photography/<slug>.md → photography/<slug>.html
            else "photography/" ++ replaceExtension fname "html"
  where
    contentPrefix :: String
    contentPrefix = "content/"

-- ---------------------------------------------------------------------------
-- Landing page
-- ---------------------------------------------------------------------------

-- | Section landing at @/photography/@. Loads all photo entries
--   resolved against 'photographyCtx' so each card has access to
--   slug / photo-url / captured-display / palette swatches.
--
--   Sorts by display date (creation date, or most-recent revision
--   when the entry has a @revised:@ entry — same ordering authority
--   that essay listings use). Phase 2 will replace this listing with
--   the masonry/grid/chronological mode toggle, but the underlying
--   data feed stays the same — the toggle is a JS layer over the
--   already-rendered grid markup.
photographyLandingRules :: Rules ()
photographyLandingRules =
    match "content/photography/index.md" $ do
        route   $ constRoute "photography/index.html"
        compile $ do
            photos <- recentFirstByDisplay
                  =<< loadAll (P.photographyPattern .&&. hasNoVersion)
            let ctx =
                    listField "photos" photographyCtx (return photos)
                    <> constField "photography" "true"
                    <> constField "list-page"   "true"
                    <> pageCtx
            pageCompiler
                >>= inDefault "templates/photography-index.html" ctx

-- ---------------------------------------------------------------------------
-- Map data (Phase 4)
-- ---------------------------------------------------------------------------
--
-- Two artifacts together:
--
--   * @/photography/map.json@  — JSON array of pin objects, fetched
--     by @static/js/photography-map.js@ at view time. Built directly
--     from frontmatter; no Python dependency.
--   * @/photography/map/@      — the page that renders the Leaflet
--     viewport. Lightweight HTML; the heavy lifting lives in the JS.
--
-- Privacy: every coordinate is rounded to the precision the author
-- declares in @geo-precision:@ (default @"city"@) BEFORE it leaves
-- this build step. Full-precision coords never reach @map.json@.
-- @geo-precision: hidden@ omits the entry entirely.

-- | Strip a trailing @"index.html"@ component so a Hakyll route
--   like @"photography/foo/index.html"@ becomes @"photography/foo/"@.
--   Used for map.json click-through URLs.
stripIndexHtml :: String -> String
stripIndexHtml r
    | suffixMatches = take (length r - 10) r  -- 10 = length "index.html"
    | otherwise     = r
  where
    suffix         = "/index.html" :: String
    suffixMatches  = suffix == drop (length r - length suffix) r

-- | Round a decimal coordinate to the precision that matches the
--   author's @geo-precision:@ declaration.
--
--   * @exact@: 4 decimal places (~10 m)
--   * @km@   : 2 decimal places (~1 km)
--   * @city@ : 1 decimal place  (~10 km)  — default
--   * other  : treated as @city@ (defensive only — 'buildPin' validates
--     the precision and fails closed before consulting this function)
--
--   @hidden@ and unrecognised values are handled at the call site by
--   skipping the pin entirely; this function is not consulted then.
roundCoord :: String -> Double -> Double
roundCoord prec x =
    let n = case prec of
            "exact" -> 4
            "km"    -> 2
            "city"  -> 1
            _       -> 1
        scale = 10 ^^ (n :: Int) :: Double
    in  fromIntegral (round (x * scale) :: Integer) / scale

-- | Extract @[lat, lon]@ from a frontmatter @geo:@ list. Accepts only
--   exactly two numeric entries — anything else returns 'Nothing' so
--   the entry is silently skipped on the map.
parseGeo :: Aeson.Object -> Maybe (Double, Double)
parseGeo meta = case KM.lookup "geo" meta of
    Just (Array vec) | V.length vec == 2 ->
        case (asDouble (vec V.! 0), asDouble (vec V.! 1)) of
            (Just lat, Just lon) -> Just (lat, lon)
            _                    -> Nothing
    _ -> Nothing
  where
    asDouble (Number n) = Just (Sci.toRealFloat n)
    asDouble _          = Nothing

-- | Build a single pin object from a photo entry. Returns 'Nothing'
--   when:
--     * the entry has no @geo:@ frontmatter, or
--     * @geo-precision:@ is anything other than @exact@/@km@/@city@ —
--       @hidden@ and unrecognised values (typos, wrong case) alike.
--       Failing closed means a typo'd \"hidden\" can never publish
--       coordinates the author meant to suppress.
--     * the entry has no resolvable route (shouldn't happen for
--       photographyPattern items, but be defensive).
buildPin :: Item String -> Compiler (Maybe Value)
buildPin item = do
    let ident = itemIdentifier item
    meta  <- getMetadata ident
    mRoute <- getRoute ident
    case (parseGeo meta, lookupString "geo-precision" meta, mRoute) of
        (Just (lat, lon), prec, Just r)
          | maybe True (`elem` ["exact", "km", "city"]) prec -> do
            let prec'   = fromMaybe "city" prec
                rLat    = roundCoord prec' lat
                rLon    = roundCoord prec' lon
                fp      = toFilePath ident
                -- Directory entries (<slug>/index.md) and series children
                -- (<series>/<photo>.md) both key assets off the parent
                -- directory; a flat single (content/photography/foo.md)
                -- has no entry directory, so its slug is its basename and
                -- its co-located assets route to /photography/ directly.
                isFlat  = takeDirectory fp == "content/photography"
                          && takeFileName fp /= "index.md"
                slug    = if isFlat then takeBaseName fp
                                    else takeFileName (takeDirectory fp)
                title   = fromMaybe slug (lookupString "title" meta)
                photo   = lookupString "photo" meta
                -- Trim trailing "index.html" so the click-through URL
                -- is the canonical directory form (no implicit redirect).
                url     = "/" ++ stripIndexHtml r
                -- Co-located assets live beside the entry's Markdown and
                -- route to the directory the entry's own URL is under.
                assetDir = takeDirectory fp
                assetUrl = if isFlat then "/photography"
                                     else "/photography/" ++ slug
                captured = lookupString "captured" meta
                -- Location and series let the map aggregate: photographs
                -- rounded to the same city-level coordinate are one place,
                -- and if they also share a series there is somewhere better
                -- to send a click than an arbitrary one of them.
                place    = lookupString "location" meta
                series   = lookupString "series" meta
            -- P01: the map's two image slots are a 280px tooltip and a
            -- 46px group-strip square, and this used to hand both the full
            -- 2400px delivery source — a click on a ten-photograph cluster
            -- pulled ~8 MB to draw ten thumbnails. Prefer the 480px rung,
            -- checked on disk because the ladder is only as complete as
            -- the last tools/generate-thumbnails.py run, and fall back to
            -- the source so a map pin never loses its picture.
            thumb <- case photo of
                Just p | not (null p) -> do
                    let variant = photoVariantName p 480
                    hasVariant <- unsafeCompiler $
                        doesFileExist (assetDir </> variant)
                    return $ assetUrl ++ "/"
                             ++ (if hasVariant then variant else p)
                _ -> return ""
            return $ Just $ Aeson.object $
                    [ "slug"     .= slug
                    , "title"    .= title
                    , "url"      .= url
                    , "lat"      .= rLat
                    , "lon"      .= rLon
                    ] ++ (if null thumb then [] else ["thumb" .= thumb])
                      ++ maybe [] (\c -> ["captured" .= c]) captured
                      ++ maybe [] (\l -> ["location" .= l]) place
                      ++ maybe [] (\x -> ["series"   .= x]) series
        _ -> return Nothing

-- | @/photography/map.json@ — JSON array of geo-tagged photo pins
--   for the Leaflet client. Excludes entries with @geo-precision:
--   hidden@ and entries with no @geo:@ frontmatter. Walks
--   'allPhotoEntries' so series children with their own GPS land
--   on the map alongside top-level photos.
photographyMapDataRule :: Rules ()
photographyMapDataRule =
    create ["photography/map.json"] $ do
        route idRoute
        compile $ do
            photos <- loadAll (P.allPhotoEntries .&&. hasNoVersion)
                        :: Compiler [Item String]
            pins   <- mapMaybe id <$> mapM buildPin photos
            -- LBS.unpack truncates each UTF-8 byte to a Char (Latin-1
            -- mode), and Hakyll then re-encodes the String to UTF-8 on
            -- write — producing double-encoded mojibake for any non-
            -- ASCII title (em-dashes, accents, etc.). Decoding through
            -- Text gives Hakyll a String of Unicode code points it can
            -- re-encode cleanly.
            makeItem $ TL.unpack $ TLE.decodeUtf8 $ Aeson.encode pins

-- ---------------------------------------------------------------------------
-- Map page (Phase 4)
-- ---------------------------------------------------------------------------

-- | @/photography/map/@ — the Leaflet-driven map view. Synthesised
--   page; no Markdown source. The @photography-map@ context flag
--   gates Leaflet CSS / JS loading in @head.html@ and @default.html@,
--   so other photography pages stay lightweight.
photographyMapPageRule :: Rules ()
photographyMapPageRule =
    create ["photography/map/index.html"] $ do
        route idRoute
        compile $ do
            let ctx = constField "title"           "Map · Photography"
                   <> constField "description"
                        "Geotagged photographs plotted on a map."
                   <> constField "photography"     "true"
                   <> constField "photography-map" "true"
                   <> constField "portal"          "true"
                   <> siteCtx
            makeItem ""
                >>= inDefault "templates/photography-map.html" ctx

-- ---------------------------------------------------------------------------
-- Atom feed (Phase 5)
-- ---------------------------------------------------------------------------

-- | Description field for Atom feed entries: prepends an absolute-URL
--   @<img>@ tag (so the photograph displays inline in the reader) to
--   the rendered prose body. Composed ABOVE 'bodyField' so it wins
--   when @$description$@ is consumed by the Atom template.
photographyFeedDescription :: Context String
photographyFeedDescription = field "description" $ \item -> do
    cfg <- loadConfig
    let ident = itemIdentifier item
    body <- itemBody <$> (loadSnapshot ident "content" :: Compiler (Item String))
    meta <- getMetadata ident
    let fp     = toFilePath ident
        -- Same asset-path derivation as 'buildPin': directory entries
        -- (<slug>/index.md) and series children (<series>/<photo>.md)
        -- both key assets off the parent directory; a flat single
        -- (content/photography/foo.md) has no entry directory, so its
        -- co-located assets route to /photography/ directly.
        isFlat = takeDirectory fp == "content/photography"
                 && takeFileName fp /= "index.md"
        slug   = takeFileName (takeDirectory fp)
        imgTag = case lookupString "photo" meta of
            Just p | not (null p) ->
                let src = if isFlat then "/photography/" ++ p
                                    else "/photography/" ++ slug ++ "/" ++ p
                in  "<p><img src=\"" ++ siteUrl cfg
                    ++ src ++ "\" alt=\"\"></p>\n"
            _ -> ""
    return (imgTag ++ body)

-- | @/photography/feed.xml@ — Atom feed of the most recent 30 photo
--   entries, with each photograph embedded inline at the top of its
--   entry description.
photographyFeedRule :: FeedSpec -> Rules ()
photographyFeedRule feed =
    create [fromFilePath (feedPath feed)] $ do
        route idRoute
        compile $ do
            (cfg, spec) <- loadFeed (feedPath feed)
            photos <- fmap (maybe id take (feedLimit spec)) . recentFirst
                  =<< loadAllSnapshots
                          (P.allPhotoEntries .&&. hasNoVersion)
                          "content"
            -- 'feedMetaFields' carries the revision-aware `updated`, the
            -- creation-date `published`, and the directory-form entry URL
            -- (audits F11 and C03) — shared with the site feed so the two
            -- cannot disagree.
            let feedCtx =
                    feedTitleField
                    <> feedMetaFields
                    <> photographyFeedDescription
                    <> bodyField "description"
                    <> defaultContext
            renderSiteFeed cfg spec feedCtx photos

-- ---------------------------------------------------------------------------
-- By-year pages (Phase 5)
-- ---------------------------------------------------------------------------
--
-- @/photography/by-year/@ is the index of years that have photos;
-- @/photography/by-year/<year>/@ lists each year's photos
-- chronologically. Year is taken from @captured:@ frontmatter
-- (when present), falling back to @date:@. Photos with neither
-- field — or with a malformed date — are silently dropped from this
-- surface; they remain visible on the main grid and any tag pages
-- their frontmatter produces.

-- | Extract a four-digit year from a frontmatter @captured:@ or
--   @date:@ field. Returns 'Nothing' when neither is set or both are
--   shorter than four characters.
yearOfPhoto :: Metadata -> Maybe String
yearOfPhoto meta =
    let firstFour s = if length s >= 4 then Just (take 4 s) else Nothing
    in  case lookupString "captured" meta >>= firstFour of
            Just yr -> Just yr
            Nothing -> lookupString "date" meta >>= firstFour

-- | All by-year rules: collect (year, identifier) pairs once, then
--   build the index page and one page per year.
photographyByYearRules :: Rules ()
photographyByYearRules = do
    photoIds <- getMatches (P.allPhotoEntries .&&. hasNoVersion)
    pairs <- forM photoIds $ \ident -> do
        meta <- getMetadata ident
        return $ fmap (\yr -> (yr, ident)) (yearOfPhoto meta)
    let yearMap :: Map String [Identifier]
        yearMap = Map.fromListWith (++) [(yr, [i]) | (yr, i) <- catMaybes pairs]
        -- Years sorted descending so the most recent appear first.
        years = map fst $ sortBy (comparing (Down . fst)) (Map.toList yearMap)

    photographyByYearIndexRule yearMap years
    forM_ years $ \yr -> photographyByYearPageRule yr (yearMap Map.! yr)

-- | @/photography/by-year/@ — top-level index. Lists each year that
--   has photos with the count, linking to the per-year page.
photographyByYearIndexRule :: Map String [Identifier] -> [String] -> Rules ()
photographyByYearIndexRule yearMap years =
    create ["photography/by-year/index.html"] $ do
        route idRoute
        compile $ do
            let yearItems =
                    [ Item (fromFilePath ("year-" ++ yr))
                           (yr, length (Map.findWithDefault [] yr yearMap))
                    | yr <- years
                    ]
                yrCtx =
                    field "year"        (return . fst . itemBody)
                    <> field "year-url" (\i -> return $ "/photography/by-year/"
                                              ++ fst (itemBody i) ++ "/")
                    <> field "year-count"
                            (return . show . snd . itemBody)
                ctx =
                    listField "years" yrCtx (return yearItems)
                    <> constField "title"       "Photography by year"
                    <> constField "description"
                        "Photographs grouped by the year they were made."
                    <> constField "photography" "true"
                    <> siteCtx
            makeItem ""
                >>= inDefault "templates/photography-by-year-index.html" ctx

-- | @/photography/by-year/<year>/@ — list of photos captured that year.
photographyByYearPageRule :: String -> [Identifier] -> Rules ()
photographyByYearPageRule yr idents =
    create [fromFilePath ("photography/by-year/" ++ yr ++ "/index.html")] $ do
        route idRoute
        compile $ do
            photos <- recentFirstByDisplay
                  =<< mapM (\i -> load i :: Compiler (Item String)) idents
            let ctx =
                    listField "photos" photographyCtx (return photos)
                    <> constField "title"       ("Photography · " ++ yr)
                    <> constField "description"
                        ("Photographs made in " ++ yr ++ ".")
                    <> constField "year"        yr
                    <> constField "photography" "true"
                    <> constField "list-page"   "true"
                    <> siteCtx
            makeItem ""
                >>= inDefault "templates/photography-by-year.html" ctx

-- ---------------------------------------------------------------------------
-- Contact sheet (Phase 5)
-- ---------------------------------------------------------------------------

-- | @/photography/contact-sheet/@ — alternate view of every photo in
--   a film-strip aesthetic: thin white-bordered frames, frame numbers
--   in the corner, slightly grainy backdrop. Distinct from the main
--   grid views; deep cut rather than primary surface.
--
--   Sort order: chronological by display date (asc). The contact-sheet
--   convention reads top-to-bottom in capture order — a roll of film,
--   not a recency feed. Each frame's index doubles as its frame
--   number. The CSS handles the frame numbering via a CSS counter so
--   we don't have to thread the index through the template.
photographyContactSheetRule :: Rules ()
photographyContactSheetRule =
    create ["photography/contact-sheet/index.html"] $ do
        route idRoute
        compile $ do
            -- Reverse the recent-first sort to get oldest-first
            -- (capture chronology), matching the contact-sheet
            -- convention.
            photos <- reverse <$> (recentFirstByDisplay
                  =<< loadAll (P.allPhotoEntries .&&. hasNoVersion)
                       :: Compiler [Item String])
            let ctx =
                    listField "photos" photographyCtx (return photos)
                    <> constField "title"       "Contact sheet · Photography"
                    <> constField "description"
                        "Every photograph on the site as one contact sheet."
                    <> constField "photography" "true"
                    <> constField "portal"      "true"
                    <> siteCtx
            makeItem ""
                >>= inDefault "templates/photography-contact-sheet.html" ctx

