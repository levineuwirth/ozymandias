{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Photography section — routing and per-page compilation.
--
--   Activates only when @content/photography/@ exists in the project
--   tree (gated in 'Site.rules'). Photographers who don't want a
--   photography section simply leave the directory absent and pay zero
--   cost — no rules, no generated pages, no feed.
--
--   Surfaces:
--
--     * Single-photo entries — flat (@content/photography/<slug>.md@)
--       and directory form (@content/photography/<slug>/index.md@).
--     * Series — a directory with siblings, e.g.
--       @content/photography/<series>/<photo>.md@. Series detection is
--       structural; no @series: true@ frontmatter flag is needed.
--     * Section landing at @/photography/@.
--     * Map at @/photography/map/@ with @map.json@ for the Leaflet client.
--     * By-year indexes at @/photography/by-year/{year}/@.
--     * Contact sheet at @/photography/contact-sheet/@.
--     * Atom feed at @/photography/feed.xml@.
--
--   See @PHOTOGRAPHY.md@ in the upstream levineuwirth.org repo for the
--   full design rationale.
module Photography
    ( photographyRules
    ) where

import           Control.Monad          (forM, forM_)
import           Data.List              (sortBy)
import qualified Data.Map.Strict        as Map
import           Data.Map.Strict        (Map)
import           Data.Maybe             (mapMaybe, fromMaybe, catMaybes)
import qualified Data.Set               as Set
import           Data.Set               (Set)
import           Data.Ord               (Down (..), comparing)
import           System.FilePath        (takeDirectory, takeFileName, replaceExtension)
import qualified Data.Aeson             as Aeson
import           Data.Aeson             (Value (..), (.=))
import qualified Data.Aeson.KeyMap      as KM
import qualified Data.Text              as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import qualified Data.Vector            as V
import qualified Data.Scientific        as Sci
import           Hakyll
import qualified Config
import           Compilers              (pageCompiler, photographyCompiler)
import           Contexts               (photographyCtx, pageCtx, siteCtx)
import qualified Patterns               as P

-- ---------------------------------------------------------------------------
-- Rules
-- ---------------------------------------------------------------------------

-- | All photography rules. Called from 'Site.rules' once, and only
--   when @content/photography/@ exists.
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
photographyRules :: Rules ()
photographyRules = do
    -- A directory is a "series" iff it has @.md@ siblings alongside
    -- its @index.md@. Collected once at rule-gen time so the entry
    -- rule can branch on series-landing template selection without
    -- re-globbing per item.
    siblingIds <- getMatches
        ( "content/photography/*/*.md"
        .&&. complement "content/photography/*/index.md"
        )
    let seriesSlugs :: Set String
        seriesSlugs = Set.fromList
            [ takeFileName (takeDirectory (toFilePath ident))
            | ident <- siblingIds
            ]

    photographyAssetRules
    photographyEntryRules seriesSlugs
    photographySeriesPhotoRules
    photographyLandingRules
    photographyMapDataRule
    photographyMapPageRule
    photographyFeedRule
    photographyByYearRules
    photographyContactSheetRule

-- ---------------------------------------------------------------------------
-- Assets
-- ---------------------------------------------------------------------------

-- | Co-located assets — the photo file itself, and the
--   build-generated @{photo}.exif.yaml@ + @{photo}.palette.yaml@
--   sidecars. Two patterns are matched in sequence:
--
--     * @content/photography/<asset>@        — flat-single co-located assets
--     * @content/photography/<slug>/<asset>@ — directory-form co-located assets
--
--   Markdown files are excluded from both rules; they're compiled by
--   'photographyEntryRules' and 'photographyLandingRules'. The
--   @.exif.yaml@ / @.palette.yaml@ / @.dims.yaml@ sidecars are
--   excluded too — they're consumed by Hakyll at compile time and have
--   no role in the deployed site.
photographyAssetRules :: Rules ()
photographyAssetRules = do
    -- Top-level non-Markdown files (flat-single co-located assets).
    match ("content/photography/*"
           .&&. complement "content/photography/*.md"
           .&&. complement "content/photography/*.exif.yaml"
           .&&. complement "content/photography/*.palette.yaml"
           .&&. complement "content/photography/*.dims.yaml") $ do
        route $ gsubRoute "content/" (const "")
        compile copyFileCompiler

    -- Directory-form entries' co-located assets.
    match ("content/photography/*/*"
           .&&. complement "content/photography/*/index.md"
           .&&. complement "content/photography/*/*.md"
           .&&. complement "content/photography/*/*.exif.yaml"
           .&&. complement "content/photography/*/*.palette.yaml"
           .&&. complement "content/photography/*/*.dims.yaml") $ do
        route $ gsubRoute "content/" (const "")
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
--   The @"content"@ snapshot is saved so @/photography/feed.xml@ can
--   render the rendered body as feed entry content.
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
                >>= loadAndApplyTemplate template                         ctx
                >>= loadAndApplyTemplate "templates/default.html"         ctx
                >>= relativizeUrls

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
            >>= loadAndApplyTemplate "templates/photography.html" photographyCtx
            >>= loadAndApplyTemplate "templates/default.html"     photographyCtx
            >>= relativizeUrls
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
    <> listFieldWith "series-photos" photographyCtx loadSeriesChildren
    <> photographyCtx
  where
    loadSeriesChildren parent = do
        let ident = itemIdentifier parent
            slug  = takeFileName (takeDirectory (toFilePath ident))
            pat   = fromGlob ("content/photography/" ++ slug ++ "/*.md")
                .&&. complement
                        (fromGlob ("content/photography/" ++ slug ++ "/index.md"))
                .&&. hasNoVersion
        recentFirst =<< loadAll pat

-- | Route a photography entry to its public URL. The pattern check on
--   @takeFileName@ distinguishes flat (@content/photography/<slug>.md@)
--   from directory-form (@content/photography/<slug>/index.md@) without
--   re-globbing, since Hakyll has already pre-filtered to entries
--   matching 'P.photographyPattern'.
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
photographyLandingRules :: Rules ()
photographyLandingRules =
    match "content/photography/index.md" $ do
        route   $ constRoute "photography/index.html"
        compile $ do
            photos <- recentFirst
                  =<< loadAll (P.photographyPattern .&&. hasNoVersion)
            let ctx =
                    listField "photos" photographyCtx (return photos)
                    <> constField "photography" "true"
                    <> constField "list-page"   "true"
                    <> pageCtx
            pageCompiler
                >>= loadAndApplyTemplate "templates/photography-index.html" ctx
                >>= loadAndApplyTemplate "templates/default.html"           ctx
                >>= relativizeUrls

-- ---------------------------------------------------------------------------
-- Map data
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
--   * other  : treated as @city@
--
--   @hidden@ is handled at the call site by skipping the pin entirely.
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
--     * it has @geo-precision: hidden@, or
--     * the entry has no resolvable route.
buildPin :: Item String -> Compiler (Maybe Value)
buildPin item = do
    let ident = itemIdentifier item
    meta  <- getMetadata ident
    mRoute <- getRoute ident
    case (parseGeo meta, lookupString "geo-precision" meta, mRoute) of
        (_, Just "hidden", _) -> return Nothing
        (Just (lat, lon), prec, Just r) ->
            let prec'   = fromMaybe "city" prec
                rLat    = roundCoord prec' lat
                rLon    = roundCoord prec' lon
                fp      = toFilePath ident
                slug    = takeFileName (takeDirectory fp)
                title   = fromMaybe slug (lookupString "title" meta)
                photo   = lookupString "photo" meta
                url     = "/" ++ stripIndexHtml r
                thumb   = case photo of
                    Just p | not (null p) ->
                        "/photography/" ++ slug ++ "/" ++ p
                    _ -> ""
                captured = lookupString "captured" meta
            in  return $ Just $ Aeson.object $
                    [ "slug"     .= slug
                    , "title"    .= title
                    , "url"      .= url
                    , "lat"      .= rLat
                    , "lon"      .= rLon
                    ] ++ (if null thumb then [] else ["thumb" .= thumb])
                      ++ maybe [] (\c -> ["captured" .= c]) captured
        _ -> return Nothing

-- | @/photography/map.json@ — JSON array of geo-tagged photo pins
--   for the Leaflet client.
photographyMapDataRule :: Rules ()
photographyMapDataRule =
    create ["photography/map.json"] $ do
        route idRoute
        compile $ do
            photos <- loadAll (P.allPhotoEntries .&&. hasNoVersion)
                        :: Compiler [Item String]
            pins   <- mapMaybe id <$> mapM buildPin photos
            -- Decode through Text so non-ASCII (em-dashes, accents) are
            -- preserved correctly in titles instead of being mojibake'd.
            makeItem $ TL.unpack $ TLE.decodeUtf8 $ Aeson.encode pins

-- ---------------------------------------------------------------------------
-- Map page
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
                   <> constField "photography"     "true"
                   <> constField "photography-map" "true"
                   <> siteCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/photography-map.html" ctx
                >>= loadAndApplyTemplate "templates/default.html"          ctx
                >>= relativizeUrls

-- ---------------------------------------------------------------------------
-- Atom feed
-- ---------------------------------------------------------------------------

-- | Configuration for the photography-only Atom feed. Distinct from
--   the main feed so text-primary subscribers don't unexpectedly get
--   image-heavy entries in their reader.
photographyFeedConfig :: FeedConfiguration
photographyFeedConfig = FeedConfiguration
    { feedTitle       = T.unpack (Config.siteName    Config.siteConfig) ++ " — Photography"
    , feedDescription = "New photographs"
    , feedAuthorName  = T.unpack (Config.authorName  Config.siteConfig)
    , feedAuthorEmail = T.unpack (Config.authorEmail Config.siteConfig)
    , feedRoot        = T.unpack (Config.siteUrl     Config.siteConfig)
    }

-- | Description field for Atom feed entries: prepends an absolute-URL
--   @<img>@ tag (so the photograph displays inline in the reader) to
--   the rendered prose body. Composed ABOVE 'bodyField' so it wins
--   when @$description$@ is consumed by the Atom template.
photographyFeedDescription :: Context String
photographyFeedDescription = field "description" $ \item -> do
    let ident = itemIdentifier item
    body <- itemBody <$> (loadSnapshot ident "content" :: Compiler (Item String))
    meta <- getMetadata ident
    let fp     = toFilePath ident
        isDir  = takeFileName fp == "index.md"
        slug   = takeFileName (takeDirectory fp)
        photo  = lookupString "photo" meta
        siteUrlStr = T.unpack (Config.siteUrl Config.siteConfig)
        imgTag = case (isDir, photo) of
            (True, Just p) | not (null p) ->
                "<p><img src=\"" ++ siteUrlStr ++ "/photography/"
                ++ slug ++ "/" ++ p ++ "\" alt=\"\"></p>\n"
            _ -> ""
    return (imgTag ++ body)

-- | @/photography/feed.xml@ — Atom feed of the most recent 30 photo
--   entries, with each photograph embedded inline at the top of its
--   entry description.
photographyFeedRule :: Rules ()
photographyFeedRule =
    create ["photography/feed.xml"] $ do
        route idRoute
        compile $ do
            photos <- fmap (take 30) . recentFirst
                  =<< loadAllSnapshots
                          (P.allPhotoEntries .&&. hasNoVersion)
                          "content"
            let feedCtx =
                    dateField "updated"   "%Y-%m-%dT%H:%M:%SZ"
                    <> dateField "published" "%Y-%m-%dT%H:%M:%SZ"
                    <> photographyFeedDescription
                    <> bodyField "description"
                    <> defaultContext
            renderAtom photographyFeedConfig feedCtx photos

-- ---------------------------------------------------------------------------
-- By-year pages
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
                    <> constField "photography" "true"
                    <> siteCtx
            makeItem ""
                >>= loadAndApplyTemplate
                        "templates/photography-by-year-index.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

-- | @/photography/by-year/<year>/@ — list of photos captured that year.
photographyByYearPageRule :: String -> [Identifier] -> Rules ()
photographyByYearPageRule yr idents =
    create [fromFilePath ("photography/by-year/" ++ yr ++ "/index.html")] $ do
        route idRoute
        compile $ do
            photos <- recentFirst
                  =<< mapM (\i -> load i :: Compiler (Item String)) idents
            let ctx =
                    listField "photos" photographyCtx (return photos)
                    <> constField "title"       ("Photography · " ++ yr)
                    <> constField "year"        yr
                    <> constField "photography" "true"
                    <> constField "list-page"   "true"
                    <> siteCtx
            makeItem ""
                >>= loadAndApplyTemplate
                        "templates/photography-by-year.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

-- ---------------------------------------------------------------------------
-- Contact sheet
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
            photos <- reverse <$> (recentFirst
                  =<< loadAll (P.allPhotoEntries .&&. hasNoVersion)
                       :: Compiler [Item String])
            let ctx =
                    listField "photos" photographyCtx (return photos)
                    <> constField "title"       "Contact sheet · Photography"
                    <> constField "photography" "true"
                    <> siteCtx
            makeItem ""
                >>= loadAndApplyTemplate
                        "templates/photography-contact-sheet.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
