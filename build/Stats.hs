{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The two generated statistics pages.
--
--   * @/build/@ — build telemetry: corpus statistics, word-length
--     distribution, tag frequencies, link analysis, epistemic coverage,
--     output metrics, repository overview, and build timing.
--   * @/stats/@ — the work itself: writing activity, monthly volume,
--     corpus, notable pages, photography, and tags.
--
-- Both are rendered as full essays (3-column layout, TOC, metadata block).
module Stats (statsRules) where

import Control.Exception          (IOException, catch)
import Control.Monad              (forM)
import Data.Char                  (isDigit)
import Data.List                  (find, intercalate, isPrefixOf, isSuffixOf,
                                   sort, sortBy)
import qualified Data.Map.Strict  as Map
import Data.Maybe                 (catMaybes, fromMaybe, isJust, listToMaybe,
                                   mapMaybe)
import Data.Ord                   (comparing, Down (..))
import qualified Data.Set         as Set
import Data.String                (fromString)
import Data.Time                  (getCurrentTime, formatTime, defaultTimeLocale,
                                   Day, utctDay, addDays, diffDays)
import Data.Time.Calendar         (toGregorian, dayOfWeek)
import System.Directory           (doesDirectoryExist, getFileSize, listDirectory,
                                   pathIsSymbolicLink)
import System.Exit                (ExitCode (..))
import System.FilePath            (takeExtension, (</>))
import System.IO                  (readFile')
import System.Process             (readProcessWithExitCode)
import Text.Read                  (readMaybe)
import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Key   as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector      as V
import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import qualified Data.Text.Encoding as TE
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Internal         as BI
import Hakyll
import Archive                    (archiveBuildStats)
import Contexts                   (siteCtx, authorLinksField, canonicalUrlPath)
import Marks                      (hasMonogram, monogramSvgFieldFor,
                                   hasMonogramFieldFor)
import qualified Patterns         as P
import EngineFiles                (filesIn)
import FooterData                 (normaliseUrl)
import Utils                      (exposureISO, formatBytes, inDefault, isDevBuild, isSafeUrl, median, outputDirFor, parseIsoDate, readingTime)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data TypeRow = TypeRow
    { trLabel :: String
    , trCount :: Int
    , trWords :: Int
    , trProse :: Bool
      -- ^ Does a word count mean anything for this type? Photographs are
      -- corpus members with no prose in them; rendering them as @0 words,
      -- 0 min@ would read as a measurement rather than as inapplicable, so
      -- their two text columns become em-dashes instead.
    }

-- | One photographic frame, as recorded in its own frontmatter. Every field
-- is optional because the importer writes whatever EXIF it found and the
-- values are editable afterwards — a scan carrying no camera data is still
-- a frame, and must not drop out of the counts it does belong to.
data PhotoInfo = PhotoInfo
    { phCamera   :: Maybe String
    , phLens     :: Maybe String
    , phFocal    :: Maybe Int      -- ^ millimetres, as recorded
    , phAperture :: Maybe String   -- ^ e.g. @f\/1.8@
    , phISO      :: Maybe Int
    , phLocation :: Maybe String
    , phSeries   :: Maybe String
    , phOrient   :: Maybe String
    , phCaptured :: Maybe Day
    , phGeo      :: Bool
    }

data PageInfo = PageInfo
    { piTitle :: String
    , piUrl   :: String
    , piWC    :: Int
    }

-- ---------------------------------------------------------------------------
-- Hakyll helpers
-- ---------------------------------------------------------------------------

loadWC :: Item String -> Compiler Int
loadWC item = do
    snap <- loadSnapshot (itemIdentifier item) "word-count"
    return $ fromMaybe 0 (readMaybe (itemBody snap))

loadPI :: Item String -> Compiler (Maybe PageInfo)
loadPI item = do
    meta   <- getMetadata (itemIdentifier item)
    mRoute <- getRoute (itemIdentifier item)
    wc     <- loadWC item
    -- 'canonicalUrlPath', not "/" ++ route: a directory-routed page's
    -- route is <slug>/index.html and every link the site writes to it
    -- says <slug>/ (audit C03).
    return $ fmap (\r -> PageInfo
        { piTitle = fromMaybe "(untitled)" (lookupString "title" meta)
        , piUrl   = canonicalUrlPath r
        , piWC    = wc
        }) mRoute

-- ---------------------------------------------------------------------------
-- Formatting helpers
-- ---------------------------------------------------------------------------

commaInt :: Int -> String
commaInt n
    | n < 1000  = show n
    | otherwise = commaInt (n `div` 1000) ++ "," ++ pad3 (n `mod` 1000)
  where
    pad3 x
        | x < 10    = "00" ++ show x
        | x < 100   = "0"  ++ show x
        | otherwise = show x

-- | Reading time at 200 words a minute, rounded up like a page's own
--   ('readingTime'), so a total never reads shorter than its parts.
rtStr :: Int -> String
rtStr totalWords
    | mins < 60 = show mins ++ " min"
    | otherwise = show (mins `div` 60) ++ "h " ++ show (mins `mod` 60) ++ "m"
  where mins = (totalWords + 199) `div` 200

-- | The pages that count when describing length: average, median, longest
--   and shortest, on /stats/ and /build/ alike. Below 50 words are
--   placeholder pages ("a fuller write-up follows"), which are real pages
--   but are not what a reader means by the shortest thing here.
substantial :: [PageInfo] -> [PageInfo]
substantial = filter ((> 50) . piWC)

-- | Mean words per page; 0 for none.
averageWords :: [PageInfo] -> Int
averageWords [] = 0
averageWords ps = sum (map piWC ps) `div` length ps

pctStr :: Int -> Int -> String
pctStr _ 0     = "—"
pctStr n total = show (n * 100 `div` total) ++ "%"

-- | Frequency table over any orderable key, ascending by key.
tally :: Ord a => [a] -> [(a, Int)]
tally xs = Map.toAscList (Map.fromListWith (+) [(x, 1 :: Int) | x <- xs])

-- | Same, ordered by descending count.
tallyDesc :: Ord a => [a] -> [(a, Int)]
tallyDesc = sortBy (comparing (Down . snd)) . tally

-- | Assign each value to the first bucket whose predicate accepts it, and
-- count. Values matching no bucket are dropped; empty buckets are kept,
-- because a gap is part of a distribution's shape and dropping the row
-- would quietly rescale the axis around it.
--
-- First-match, so a spec need only name its upper bounds, and a label like
-- @f/2.8-4@ followed by @f/4-5.6@ resolves the shared endpoint the way a
-- photographer reads it: into the lower band.
bucketed :: [(String, a -> Bool)] -> [a] -> [(String, Int)]
bucketed spec xs =
    [ (lbl, Map.findWithDefault 0 i counts) | (i, (lbl, _)) <- indexed ]
  where
    indexed    = zip [0 :: Int ..] spec
    bucketOf x = listToMaybe [ i | (i, (_, p)) <- indexed, p x ]
    counts     = Map.fromListWith (+)
        [ (i, 1 :: Int) | x <- xs, Just i <- [bucketOf x] ]

-- | Drop content types that hold nothing.
--
-- The row list is spelled out in full at both call sites so that a type
-- gaining its first piece needs no code change to appear. Printing the
-- empty ones was the visible cost of that: /build/ carried three permanent
-- @0 / 0 / 0 min@ rows — blog posts, fiction, compositions — which said
-- only that the pattern had matched nothing.
liveRows :: [TypeRow] -> [TypeRow]
liveRows = filter ((> 0) . trCount)

-- | Strip HTML tags for plain-text word counting.
--
-- Handles:
--   * Tag bodies, including @>@ inside double-quoted attribute values
--     (so @\<img alt=\"a > b\"\>@ doesn't slice the surrounding text).
--   * HTML comments @\<!-- ... --\>@ as a unit.
--   * @\<![CDATA[ ... ]]\>@ sections.
--
-- This is still a heuristic — it does not validate the HTML — but it
-- closes the most common ways for "tag stripping" to leak content.
stripHtmlTags :: String -> String
stripHtmlTags = go
  where
    go []                                = []
    go ('<':'!':'-':'-':rest)            = go (dropComment rest)
    go ('<':'!':'[':'C':'D':'A':'T':'A':'[':rest)
                                         = go (dropCdata rest)
    go ('<':rest)                        = go (dropTag rest)
    go (c:rest)                          = c : go rest

    -- Drop everything up to and including "-->".
    dropComment ('-':'-':'>':rs) = rs
    dropComment (_:rs)           = dropComment rs
    dropComment []               = []

    -- Drop everything up to and including "]]>".
    dropCdata (']':']':'>':rs) = rs
    dropCdata (_:rs)           = dropCdata rs
    dropCdata []               = []

    -- Drop a tag body, respecting double-quoted attribute values.
    dropTag ('"':rs)  = dropTag (skipQuoted rs)
    dropTag ('\'':rs) = dropTag (skipApos   rs)
    dropTag ('>':rs)  = rs
    dropTag (_:rs)    = dropTag rs
    dropTag []        = []

    skipQuoted ('"':rs) = rs
    skipQuoted (_:rs)   = skipQuoted rs
    skipQuoted []       = []

    skipApos ('\'':rs) = rs
    skipApos (_:rs)    = skipApos rs
    skipApos []        = []

-- | A URL for a backlinks key that matches no page in the corpus: a
-- directory key, or one whose last segment has an extension, is already
-- the URL; any other lost its @.html@ to 'normaliseUrl'.
keyUrl :: String -> String
keyUrl k
    | "/" `isSuffixOf` k                         = k
    | '.' `elem` takeWhile (/= '/') (reverse k) = k
    | otherwise                                  = k ++ ".html"

pad2 :: (Show a, Integral a) => a -> String
pad2 n = if n < 10 then "0" ++ show n else show n



-- ---------------------------------------------------------------------------
-- Photograph frontmatter
-- ---------------------------------------------------------------------------

-- | Is the key present at all? @geo:@ holds a two-element list of numbers,
-- which neither 'lookupString' nor 'lookupStringList' can see, so presence
-- is the only question worth asking of it.
hasKey :: String -> Metadata -> Bool
hasKey k = KM.member (AK.fromString k)

-- | Leading integer of a recorded focal length: @\"24mm\"@ to @24@.
parseFocal :: String -> Maybe Int
parseFocal = readMaybe . takeWhile isDigit . dropWhile (not . isDigit)

-- | @exposure:@ is a single string holding shutter, aperture and
-- sensitivity — @\"1/200 f/7.1 ISO 100\"@ — because that is how the three
-- are read off the back of a camera. These pull them back apart. Each is
-- independent, so a partial or reordered string still yields what it has.
parseAperture :: String -> Maybe String
parseAperture s = listToMaybe [ w | w <- words s, "f/" `isPrefixOf` w ]


-- | Numeric value of an @f/@ label, so a histogram orders by stop rather
-- than by the string naming it (where @f/10@ sorts before @f/2@).
apertureValue :: String -> Double
apertureValue = fromMaybe 0 . readMaybe . drop 2

toPhotoInfo :: Metadata -> PhotoInfo
toPhotoInfo m = PhotoInfo
    { phCamera   = lookupString "camera"       m
    , phLens     = lookupString "lens"         m
    , phFocal    = lookupString "focal-length" m >>= parseFocal
    , phAperture = lookupString "exposure"     m >>= parseAperture
    , phISO      = lookupString "exposure"     m >>= exposureISO
    , phLocation = lookupString "location"     m
    , phSeries   = lookupString "series"       m
    , phOrient   = lookupString "orientation"  m
    , phCaptured = lookupString "captured"     m >>= parseIsoDate
    , phGeo      = hasKey "geo" m
    }

-- ---------------------------------------------------------------------------
-- Date helpers (for /stats/ page)
-- ---------------------------------------------------------------------------


-- | First Monday on or before 'day' (start of its ISO week).
--   'fromEnum' on 'DayOfWeek' is ISO-numbered (Monday=1 .. Sunday=7),
--   so Monday must subtract 0 days, Sunday 6.
weekStart :: Day -> Day
weekStart day =
    addDays (fromIntegral (negate (fromEnum (dayOfWeek day) - 1))) day

-- | Intensity class for the heatmap (hm0 … hm4).
heatClass :: Int -> String
heatClass 0             = "hm0"
heatClass n | n < 500  = "hm1"
heatClass n | n < 2000 = "hm2"
heatClass n | n < 5000 = "hm3"
heatClass _             = "hm4"

shortMonth :: Int -> String
shortMonth m = case m of
    1 -> "Jan"; 2 -> "Feb";  3 -> "Mar"; 4 -> "Apr"
    5 -> "May"; 6 -> "Jun";  7 -> "Jul"; 8 -> "Aug"
    9 -> "Sep"; 10 -> "Oct"; 11 -> "Nov"; 12 -> "Dec"
    _ -> ""

-- ---------------------------------------------------------------------------
-- URL sanitization and core HTML combinators
-- ---------------------------------------------------------------------------

-- | Defense-in-depth URL allowlist: reject anything that isn't an internal
-- path, a fragment, or an explicit safe scheme. Case-insensitive and
-- whitespace-tolerant to block @JavaScript:@, @\tjavascript:@, @data:@, etc.
-- @http://@ is intentionally excluded to avoid mixed-content warnings.
--
-- Protocol-relative URLs (@//evil.com@) are rejected because the leading
-- slash would otherwise admit them through the @\"\/\"@ prefix check.
safeHref :: String -> H.AttributeValue
safeHref u
  | isSafeUrl u = H.stringValue u
  | otherwise   = H.stringValue "#"

-- | Shorthand for 'H.toHtml' over a 'String'.
txt :: String -> H.Html
txt = H.toHtml

-- | Anchor element with escaped title text and URL sanitized via 'safeHref'.
-- Use for trusted plain-text labels such as tag slugs.
link :: String -> String -> H.Html
link url title = H.a H.! A.href (safeHref url) $ H.toHtml title

-- | Anchor for a content page, where the title comes from frontmatter and
-- may contain author-authored inline HTML (e.g. @<em>Book Title</em>@).
-- The URL is still sanitized via 'safeHref'; the title is emitted
-- pre-escaped, matching site convention that metadata titles are
-- author-controlled trusted HTML.
pageLink :: String -> String -> H.Html
pageLink url title = H.a H.! A.href (safeHref url) $ H.preEscapedToHtml title

-- | Typed section header followed by its body content.
section :: String -> String -> H.Html -> H.Html
section id_ title body = do
    H.h2 H.! A.id (H.stringValue id_) $ H.toHtml title
    body

-- | Build-telemetry table with header row, body rows, and an optional total
-- row. Cell contents are pre-rendered 'H.Html' so callers may embed links or
-- emphasis inside cells without risking double-escaping.
table :: [String] -> [[H.Html]] -> Maybe [H.Html] -> H.Html
table headers rows mFoot =
    H.table H.! A.class_ "build-table" $ do
        H.thead $ H.tr $ mapM_ (H.th . H.toHtml) headers
        H.tbody $ mapM_ renderRow rows
        maybe (return ()) renderFoot mFoot
  where
    renderRow cells  = H.tr $ mapM_ H.td cells
    renderFoot cells = H.tfoot $
        H.tr H.! A.class_ "build-total" $ mapM_ H.td cells

-- | Two-column metadata block: each pair becomes a @<dt>/<dd>@ entry. Values
-- are 'H.Html' to allow mixing links and plain text.
dl :: [(String, H.Html)] -> H.Html
dl pairs = H.dl H.! A.class_ "build-meta" $
    mapM_ (\(k, v) -> do H.dt (H.toHtml k); H.dd v) pairs

-- ---------------------------------------------------------------------------
-- SVG / custom element helpers (no blaze-svg dependency)
-- ---------------------------------------------------------------------------

svgTag, rectTag, textTag, titleTag :: H.Html -> H.Html
svgTag   = BI.customParent "svg"
rectTag  = BI.customParent "rect"
textTag  = BI.customParent "text"
titleTag = BI.customParent "title"

-- | Attach an attribute that isn't in 'Text.Blaze.Html5.Attributes' (e.g.
-- SVG @viewBox@, @x@, @y@, or @data-target@).
customAttr :: String -> String -> H.Attribute
customAttr name val = BI.customAttribute (fromString name) (fromString val)

-- ---------------------------------------------------------------------------
-- Heatmap SVG
-- ---------------------------------------------------------------------------

-- | 52-week writing activity heatmap. Styled via @.heatmap-svg@ rules in
-- static/css/build.css (no inline @<style>@).
renderHeatmap :: Map.Map Day Int -> Day -> H.Html
renderHeatmap wordsByDay today =
    let cellSz   = 10 :: Int
        gap      = 2  :: Int
        step     = cellSz + gap
        hdrH     = 22 :: Int              -- vertical space for month labels
        nWeeks   = 52
        -- First Monday of the 52-week window
        startDay = addDays (fromIntegral (-(nWeeks - 1)) * 7) (weekStart today)
        nDays    = diffDays today startDay + 1
        allDays  = [addDays i startDay | i <- [0 .. nDays - 1]]
        weekOf d = fromIntegral (diffDays d startDay `div` 7) :: Int
        dowOf  d = fromEnum (dayOfWeek d) - 1             -- ISO 1..7 -> Mon=0..Sun=6
        svgW     = (nWeeks - 1) * step + cellSz
        svgH     = 6 * step + cellSz + hdrH

        monthLabel d =
            let (_, mo, da) = toGregorian d
            in  if da == 1
                then textTag H.! A.class_ "hm-lbl"
                             H.! customAttr "x" (show (weekOf d * step))
                             H.! customAttr "y" "14"
                           $ txt (shortMonth mo)
                else mempty

        dayCell d =
            let wc           = fromMaybe 0 (Map.lookup d wordsByDay)
                (yr, mo, da) = toGregorian d
                x            = weekOf d * step
                y            = dowOf d * step + hdrH
                tip          = show yr ++ "-" ++ pad2 mo ++ "-" ++ pad2 da
                            ++ if wc > 0 then ": " ++ commaInt wc ++ " words" else ""
            in  rectTag H.! A.class_ (H.stringValue (heatClass wc))
                        H.! customAttr "x"      (show x)
                        H.! customAttr "y"      (show y)
                        H.! customAttr "width"  (show cellSz)
                        H.! customAttr "height" (show cellSz)
                        H.! customAttr "rx"     "2"
                      $ titleTag (txt tip)

        legendW = 5 * step - gap
        legendCell i =
            rectTag H.! A.class_ (H.stringValue ("hm" ++ show i))
                    H.! customAttr "x"      (show (i * step))
                    H.! customAttr "y"      "0"
                    H.! customAttr "width"  (show cellSz)
                    H.! customAttr "height" (show cellSz)
                    H.! customAttr "rx"     "2"
                  $ mempty

        legendSvg =
            svgTag H.! customAttr "width"   (show legendW)
                   H.! customAttr "height"  (show cellSz)
                   H.! customAttr "viewBox" ("0 0 " ++ show legendW ++ " " ++ show cellSz)
                   H.! customAttr "style"   "display:inline;vertical-align:middle"
                 $ mapM_ legendCell [0 .. 4 :: Int]

    in  H.figure H.! A.class_ "stats-heatmap" $ do
            svgTag H.! customAttr "width"      (show svgW)
                   H.! customAttr "height"     (show svgH)
                   H.! customAttr "viewBox"    ("0 0 " ++ show svgW ++ " " ++ show svgH)
                   H.! A.class_ "heatmap-svg"
                   H.! customAttr "role"       "img"
                   H.! customAttr "aria-label" "52-week writing activity heatmap"
                 $ do
                     mapM_ monthLabel allDays
                     mapM_ dayCell    allDays
            H.figcaption H.! A.class_ "heatmap-legend" $ do
                "Less\xA0"
                legendSvg
                "\xA0More"

-- ---------------------------------------------------------------------------
-- Stats page sections
-- ---------------------------------------------------------------------------

-- | Horizontal bar chart over pre-ordered @(label, value)@ pairs. Ordering
-- is the caller's business — months run chronologically, apertures by stop,
-- places by frequency — so this only scales and draws.
--
-- Bars are relative to the largest value in the set, and any non-zero value
-- claims at least 2% so that one frame beside two hundred is a mark rather
-- than nothing.
barChart :: [(String, Int)] -> H.Html
barChart rows
    | null rows = H.p (H.em "Nothing to chart yet.")
    | otherwise = H.div H.! A.class_ "build-bars" $ mapM_ bar rows
  where
    maxV = max 1 (maximum (map snd rows))
    bar (lbl, v) =
        let pct = if v == 0 then 0 else max 2 (v * 100 `div` maxV)
        in  H.div H.! A.class_ "build-bar-row" $ do
                H.span H.! A.class_ "build-bar-label" $ txt lbl
                H.span H.! A.class_ "build-bar-wrap" $
                    H.span H.! A.class_ "build-bar"
                           H.! A.style (H.stringValue ("width:" ++ show pct ++ "%"))
                         $ mempty
                H.span H.! A.class_ "build-bar-count" $ txt (commaInt v)

renderMonthlyVolume :: Map.Map Day Int -> H.Html
renderMonthlyVolume wordsByDay =
    section "volume" "Monthly volume" $
    let byMonth = Map.fromListWith (+)
            [ ((y, m), wc)
            | (day, wc) <- Map.toList wordsByDay
            , let (y, m, _) = toGregorian day
            ]
    in  if Map.null byMonth
        then H.p (H.em "No dated content yet.")
        else barChart
                [ (shortMonth m ++ " \x2019" ++ drop 2 (show y), wc)
                | ((y, m), wc) <- Map.toAscList byMonth
                ]

renderCorpus :: [TypeRow] -> [PageInfo] -> H.Html
renderCorpus typeRows allPIs =
    section "corpus" "Corpus" $ do
        -- Written and unwritten pages are counted separately here. Folding
        -- them into one "total pages" figure was what let the old page say
        -- "21 pages" three inches above a tag table listing 97 photographs
        -- from Copenhagen alone.
        dl $ [ ("Total words",        txt (commaInt totalWords))
             , ("Written pages",      txt (commaInt writtenPages))
             ] ++
             [ ("Photographs",        txt (commaInt unwrittenPages))
             | unwrittenPages > 0 ] ++
             [ ("Total reading time", txt (rtStr totalWords))
             , ("Average length",     txt (commaInt avgWC ++ " words"))
             , ("Median length",      txt (commaInt medWC ++ " words"))
             ]
        table ["Type", "Pages", "Words", "Reading time"]
              (map typeRowCells typeRows)
              (Just [ "Total"
                    , txt (commaInt (writtenPages + unwrittenPages))
                    , txt (commaInt totalWords)
                    , txt (rtStr totalWords)
                    ])
  where
    totalWords     = sum (map trWords typeRows)
    writtenPages   = sum [ trCount r | r <- typeRows,       trProse r ]
    unwrittenPages = sum [ trCount r | r <- typeRows, not (trProse r) ]
    avgWC          = averageWords (substantial allPIs)
    medWC          = median (map piWC (substantial allPIs))

renderNotable :: [PageInfo] -> H.Html
renderNotable allPIs =
    section "notable" "Notable" $ do
        H.p (H.strong "Longest")
        pageList (take 5 (sortBy (comparing (Down . piWC)) hasSomeWC))
        H.p (H.strong "Shortest")
        pageList (take 5 (sortBy (comparing piWC) hasSomeWC))
  where
    hasSomeWC = substantial allPIs
    pageList ps = H.ol H.! A.class_ "build-page-list" $
        mapM_ (\p -> H.li $ do
                  pageLink (piUrl p) (piTitle p)
                  txt (" \x2014 " ++ commaInt (piWC p) ++ " words")
              ) ps

-- | Everything the frames already say about themselves.
--
-- Nothing here is computed at import time or cached anywhere: each frame's
-- camera, lens, focal length, exposure, place and capture date were written
-- into its own frontmatter when it was imported, and are editable text from
-- then on. That makes this section a reading of the corpus rather than of
-- the EXIF — a frame whose recorded values were corrected by hand counts as
-- corrected, which is the behaviour a photographer expects.
--
-- The three histograms run along their own axis rather than by frequency,
-- so each reads as a distribution and keeps its shape as frames are added.
-- Bodies and places have no natural axis and run by count instead.
renderPhotography :: [PhotoInfo] -> H.Html
renderPhotography photos
    | null photos = mempty
    | otherwise = section "photography" "Photography" $ do
        dl $
            [ ("Frames",     txt (commaInt total)) ] ++
            [ ("Series",     txt (commaInt (length serieses)))  | not (null serieses) ] ++
            [ ("Places",     txt (commaInt (length places)))    | not (null places)   ] ++
            [ ("Bodies",     txt (commaInt (length cameras)))   | not (null cameras)  ] ++
            [ ("Geotagged",  txt (commaInt geoCount ++ " (" ++ pctStr geoCount total ++ ")")) ] ++
            [ ("Captured",   txt span_) | Just span_ <- [capturedSpan] ] ++
            [ ("Orientation", txt orientText) | not (null orients) ]

        subhead "Bodies"
        table ["Camera", "Lens", "Frames", "Share"]
              [ [ txt cam
                , txt (fromMaybe "\x2014" lens)
                , txt (commaInt n)
                , txt (pctStr n total)
                ]
              | ((cam, lens), n) <- tallyDesc bodyPairs
              ]
              Nothing

        histogram "Focal length" focalBands  (mapMaybe phFocal photos)
        histogram "Aperture"     apertureBands
                                 (map apertureValue (mapMaybe phAperture photos))
        histogram "Sensitivity"  isoBands    (mapMaybe phISO photos)

        if null places then mempty else do
            subhead "Places"
            table ["Place", "Frames"]
                  [ [txt place, txt (commaInt n)] | (place, n) <- take 12 places ]
                  Nothing
  where
    total     = length photos
    cameras   = tallyDesc (mapMaybe phCamera   photos)
    serieses  = tallyDesc (mapMaybe phSeries   photos)
    places    = tallyDesc (mapMaybe phLocation photos)
    orients   = tallyDesc (mapMaybe phOrient   photos)
    geoCount  = length (filter phGeo photos)

    -- Bands, not the 31 distinct focal lengths and 27 sensitivities the
    -- corpus actually holds: at one bar each that is a list, not a shape.
    -- The bounds are the ones a photographer already thinks in.
    focalBands =
        [ ("≤ 20mm",   (<= 20))
        , ("20–35mm",  (<= 35))
        , ("35–50mm",  (<= 50))
        , ("50–85mm",  (<= 85))
        , ("85–135mm", (<= 135))
        , ("135mm +",  const True)
        ]
    apertureBands =
        [ ("< f/2.8",  (< 2.8))
        , ("f/2.8–4",  (<= 4))
        , ("f/4–5.6",  (<= 5.6))
        , ("f/5.6–8",  (<= 8))
        , ("f/8–11",   (<= 11))
        , ("f/11 +",   const True)
        ]
    isoBands =
        [ ("≤ 100",     (<= 100))
        , ("100–200",   (<= 200))
        , ("200–400",   (<= 400))
        , ("400–800",   (<= 800))
        , ("800–1600",  (<= 1600))
        , ("1600 +",    const True)
        ]

    -- A body is grouped with the lens mounted on it: the same camera with a
    -- different lens is a different row, and a phone with no lens field
    -- reports one row rather than vanishing from the table.
    bodyPairs = [ (cam, phLens ph) | ph <- photos, Just cam <- [phCamera ph] ]

    capturedSpan = case sort (mapMaybe phCaptured photos) of
        []  -> Nothing
        [d] -> Just (show d)
        ds  -> Just (show (head ds) ++ " \x2013 " ++ show (last ds))

    orientText =
        intercalate " \x00b7 " [ o ++ " " ++ commaInt n | (o, n) <- orients ]

    subhead t = H.p (H.strong (txt t))

    -- Silent when nothing carries the field: a frame set imported without
    -- EXIF should not sprout three charts of zeros.
    histogram t bands vals
        | null vals = mempty
        | otherwise = do subhead t
                         barChart (bucketed bands vals)

-- | A page's table of contents, from its sections' ids and headings.
tocOf :: [(String, String)] -> H.Html
tocOf = H.ol . mapM_ item
  where
    item (id_, title) =
        H.li $ H.a H.! A.href (H.stringValue ("#" ++ id_))
                   H.! customAttr "data-target" id_
                 $ txt title

statsTOC :: H.Html
statsTOC = tocOf
              [ ("activity",    "Writing activity")
              , ("volume",      "Monthly volume")
              , ("corpus",      "Corpus")
              , ("notable",     "Notable")
              , ("photography", "Photography")
              , ("tags",        "Tags")
              ]

-- ---------------------------------------------------------------------------
-- IO: output directory walk
-- ---------------------------------------------------------------------------

-- | Recursively walk a directory, returning @(file, size)@ tuples for every
-- regular file beneath it.
--
-- Symlinks (both files and directories) are skipped, so a stray
-- @_site\/a -> _site@ doesn't trigger an infinite loop.
walkDir :: FilePath -> IO [(FilePath, Integer)]
walkDir dir = do
    entries <- listDirectory dir `catch` (\(_ :: IOException) -> return [])
    fmap concat $ forM entries $ \e -> do
        let path = dir </> e
        isLink <- pathIsSymbolicLink path
                    `catch` (\(_ :: IOException) -> return False)
        if isLink
            then return []
            else do
                isDir <- doesDirectoryExist path
                if isDir
                    then walkDir path
                    else do
                        sz <- getFileSize path
                                `catch` (\(_ :: IOException) -> return 0)
                        return [(path, sz)]

displayExt :: FilePath -> String
displayExt path = case takeExtension path of
    ".html"  -> ".html"
    ".css"   -> ".css"
    ".js"    -> ".js"
    ".woff2" -> ".woff2"
    ".svg"   -> ".svg"
    ".mp3"   -> ".mp3"
    ".pdf"   -> ".pdf"
    ".json"  -> ".json"
    ".xml"   -> ".xml"
    ".ico"   -> ".ico"
    ".png"   -> "image"
    ".jpg"   -> "image"
    ".jpeg"  -> "image"
    ".webp"  -> "image"
    _        -> "other"

getOutputStats :: IO (Map.Map String (Int, Integer), Int, Integer)
getOutputStats = do
    files <- walkDir . outputDirFor =<< isDevBuild
    let grouped = foldr (\(path, sz) acc ->
                    Map.insertWith (\(c1,s1) (c2,s2) -> (c1+c2, s1+s2))
                                   (displayExt path)
                                   (1, sz) acc)
                  Map.empty files
    return (grouped, length files, sum (map snd files))

-- ---------------------------------------------------------------------------
-- IO: lines of code
-- ---------------------------------------------------------------------------

-- | Files and lines directly in a directory, the site's and the engine's
--   together, each name counted once ("EngineFiles").
countLinesDir :: FilePath -> String -> (FilePath -> Bool) -> IO (Int, Int)
countLinesDir dir ext skipPred = do
    entries <- filesIn dir
    let files = filter (\e -> takeExtension e == ext && not (skipPred e)) entries
    -- Use strict text IO so the file handle is released as soon as the
    -- contents are read; the prior 'readFile' chained lazy IO under
    -- 'forM', leaving every handle open until the loop forced 'lines'.
    ls <- fmap sum $ forM files $ \e -> do
        content <- TIO.readFile e
                     `catch` (\(_ :: IOException) -> return T.empty)
        return (length (T.lines content))
    return (length files, ls)

getLocStats :: IO (Int, Int, Int, Int, Int, Int)
-- (hsFiles, hsLines, cssFiles, cssLines, jsFiles, jsLines)
getLocStats = do
    (hf, hl)  <- countLinesDir "build"      ".hs"  (const False)
    (cf, cl)  <- countLinesDir "static/css" ".css" (const False)
    (jf, jl)  <- countLinesDir "static/js"  ".js"  (".min.js" `isSuffixOf`)
    return (hf, hl, cf, cl, jf, jl)

-- ---------------------------------------------------------------------------
-- IO: git stats
-- ---------------------------------------------------------------------------

gitRun :: [String] -> IO String
gitRun args = do
    (ec, out, _) <- readProcessWithExitCode "git" args ""
    return $ if ec == ExitSuccess then out else ""

getGitStats :: IO (Int, String)
getGitStats = do
    countOut  <- gitRun ["rev-list", "--count", "HEAD"]
    firstOut  <- gitRun ["log", "--format=%ad", "--date=short", "--reverse"]
    let commits   = fromMaybe 0 (readMaybe (filter (/= '\n') countOut) :: Maybe Int)
        firstDate = case lines firstOut of { (d:_) -> d; _ -> "\x2014" }
    return (commits, firstDate)

-- ---------------------------------------------------------------------------
-- HTML rendering: build page sections
-- ---------------------------------------------------------------------------

-- | Corpus totals by content type.
--
--   The section anchor is @corpus@, not @content@: @templates/essay.html@
--   wraps every page (this one included) in @<div id="content">@, so a
--   section heading with that id produced two @id="content"@ elements on
--   /build/. The heading is the one that can move — the layout container
--   is shared by every page and is the skip-link/CSS target.
renderContent :: [TypeRow] -> H.Html
renderContent rows =
    section "corpus" "Content" $
    table ["Type", "Count", "Words", "Reading time"]
          (map typeRowCells rows)
          (Just [ "Total"
                , txt (commaInt totalCount)
                , txt (commaInt totalWords)
                , txt (rtStr    totalWords)
                ])
  where
    totalCount = sum (map trCount rows)
    totalWords = sum (map trWords rows)

-- | One corpus row. A type that carries no prose gets em-dashes rather than
-- zeros in the two text columns — see 'trProse'.
typeRowCells :: TypeRow -> [H.Html]
typeRowCells r =
    [ txt (trLabel r)
    , txt (commaInt (trCount r))
    , if trProse r then txt (commaInt (trWords r)) else txt "\x2014"
    , if trProse r then txt (rtStr    (trWords r)) else txt "\x2014"
    ]

renderPages :: [PageInfo]
            -> Maybe (String, String, String)
            -> Maybe (String, String, String)
            -> H.Html
renderPages allPIs mOldest mNewest =
    section "pages" "Pages" $ do
        dl $
            [ ("Written pages",  txt (commaInt (length allPIs)))
            , ("Average length", txt (commaInt avgWC ++ " words"))
            ] ++
            maybe [] (\(d,t,u) -> [("Oldest content", datedLink d t u)]) mOldest ++
            maybe [] (\(d,t,u) -> [("Newest content", datedLink d t u)]) mNewest
        H.p (H.strong "Longest")
        pageList (take 3 (sortBy (comparing (Down . piWC)) hasSomeWC))
        H.p (H.strong "Shortest")
        pageList (take 3 (sortBy (comparing piWC)         hasSomeWC))
  where
    hasSomeWC = substantial allPIs
    avgWC     = averageWords hasSomeWC
    datedLink d t u = do
        txt (d ++ " \x2014 ")
        pageLink u t
    pageList ps = H.ol H.! A.class_ "build-page-list" $
        mapM_ (\p -> H.li $ do
                  pageLink (piUrl p) (piTitle p)
                  txt (" \x2014 " ++ commaInt (piWC p) ++ " words")
              ) ps

renderDistribution :: [Int] -> H.Html
renderDistribution wcs =
    section "distribution" "Word-length distribution" $ barChart buckets
  where
    bucketOf w
        | w <  500 = 0
        | w < 1000 = 1
        | w < 2000 = 2
        | w < 5000 = 3
        | otherwise = 4
    labels = [ "< 500"
             , "500 \x2013 1k"
             , "1k \x2013 2k"
             , "2k \x2013 5k"
             , "\x2265 5k"
             ]
    counts = foldr (\w acc -> Map.insertWith (+) (bucketOf w) (1 :: Int) acc)
                   (Map.fromList [(i, 0 :: Int) | i <- [0 .. 4]]) wcs
    -- Pair labels with bucket indices via @zip@ rather than @(!!)@ to keep
    -- the function total even if the bucket count and @labels@ list ever
    -- drift out of sync (matching the discipline used in 'median').
    buckets = [(lbl, fromMaybe 0 (Map.lookup i counts)) | (i, lbl) <- zip [0 :: Int ..] labels]

renderTagsSection :: [(String, Int)] -> Int -> H.Html
renderTagsSection topTags uniqueCount =
    section "tags" "Tags" $ do
        dl [("Unique tags", txt (commaInt uniqueCount))]
        table ["Tag", "Items"] (map row topTags) Nothing
  where
    row (t, n) = [link ("/" ++ t ++ "/") t, txt (show n)]

renderLinks :: Maybe (String, Int, String) -> Int -> Int -> H.Html
renderLinks mMostLinked orphanCount total =
    section "links" "Links" $
    dl
        [ case mMostLinked of
            Nothing        -> ("Most-linked page", "\x2014")
            Just (u, n, t) ->
                ( "Most-linked page"
                , do pageLink u t
                     txt (" (" ++ show n ++ " inbound links)")
                )
        , ( "Orphan pages"
          , txt (commaInt orphanCount
                  ++ " of " ++ commaInt total
                  ++ " (" ++ pctStr orphanCount total ++ ")")
          )
        ]

renderEpistemic :: Int -> Int -> Int -> Int -> Int -> H.Html
renderEpistemic total ws wc wi we =
    section "epistemic" "Epistemic coverage" $
    table
        ["Field", "Set", "Coverage"]
        [ row "Status"     ws
        , row "Confidence" wc
        , row "Importance" wi
        , row "Evidence"   we
        ]
        Nothing
  where
    row label n = [ txt label
                  , txt (show n ++ " / " ++ show total)
                  , txt (pctStr n total)
                  ]

-- | Per-content-type counts feeding 'renderMarks'. @mrCount@ is the
-- denominator (total pieces of that type), @mrMonogram@ is the count
-- with a co-located @mark.svg@, and @mrFigure@ is the count with
-- @status:@ frontmatter (which is what triggers the epistemic figure
-- per MARKS.md §3.1).
data MarkRow = MarkRow
    { mrLabel    :: String
    , mrCount    :: Int
    , mrMonogram :: Int
    , mrFigure   :: Int
    }

-- | Drop mark rows for types that hold nothing, for the reason given on
-- 'liveRows'.
liveMarkRows :: [MarkRow] -> [MarkRow]
liveMarkRows = filter ((> 0) . mrCount)

renderMarks :: [MarkRow] -> H.Html
renderMarks rows =
    section "marks" "Marks coverage" $
    table
        ["Type", "Pieces", "Monogram", "Epistemic figure"]
        (map row rows)
        (Just [ "Total"
              , txt (commaInt totalCount)
              , txt (commaInt totalMono ++ " (" ++ pctStr totalMono totalCount ++ ")")
              , txt (commaInt totalFig  ++ " (" ++ pctStr totalFig  totalCount ++ ")")
              ])
  where
    totalCount = sum (map mrCount    rows)
    totalMono  = sum (map mrMonogram rows)
    totalFig   = sum (map mrFigure   rows)
    row r = [ txt (mrLabel r)
            , txt (commaInt (mrCount r))
            , txt (commaInt (mrMonogram r) ++ " (" ++ pctStr (mrMonogram r) (mrCount r) ++ ")")
            , txt (commaInt (mrFigure   r) ++ " (" ++ pctStr (mrFigure   r) (mrCount r) ++ ")")
            ]

renderOutput :: Map.Map String (Int, Integer) -> Int -> Integer -> H.Html
renderOutput grouped totalFiles totalSize =
    section "output" "Output" $
    table
        ["Type", "Files", "Size"]
        (map row (sortBy (comparing (Down . snd . snd)) (Map.toList grouped)))
        (Just [ "Total"
              , txt (commaInt totalFiles)
              , txt (formatBytes totalSize)
              ])
  where
    row (ext, (n, sz)) = [txt ext, txt (commaInt n), txt (formatBytes sz)]

renderRepository :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> String -> H.Html
renderRepository hf hl cf cl jf jl commits firstDate =
    section "repository" "Repository" $
    dl
        [ ("Haskell",            txt (commaInt hl ++ " lines across " ++ show hf ++ " files"))
        , ("CSS",                txt (commaInt cl ++ " lines across " ++ show cf ++ " files"))
        , ("JavaScript",         txt (commaInt jl ++ " lines across " ++ show jf ++ " files (excl. minified)"))
        , ("Total git commits",  txt (commaInt commits))
        , ("Repository started", txt firstDate)
        ]

renderBuild :: String -> String -> H.Html
renderBuild ts dur =
    section "build" "Build" $
    dl
        [ ("Generated",           txt ts)
        , ("Last build duration", txt dur)
        ]

-- | Link-archive coverage and health. The metric rows are computed by
-- 'Archive.archiveBuildStats' (count, size, link-rot status breakdown,
-- snapshot quality, visibility, orphans); this only lays them out.
renderArchive :: [(String, String)] -> H.Html
renderArchive metrics =
    section "archive" "Link archive" $
    dl [ (k, txt v) | (k, v) <- metrics ]

-- ---------------------------------------------------------------------------
-- Static TOC (matches the eleven h2 sections above)
-- ---------------------------------------------------------------------------

pageTOC :: H.Html
pageTOC = tocOf
        [ ("corpus",       "Content")
        , ("pages",        "Pages")
        , ("distribution", "Word-length distribution")
        , ("tags",         "Tags")
        , ("links",        "Links")
        , ("epistemic",    "Epistemic coverage")
        , ("marks",        "Marks coverage")
        , ("output",       "Output")
        , ("archive",      "Link archive")
        , ("repository",   "Repository")
        , ("build",        "Build")
        ]

-- ---------------------------------------------------------------------------
-- The corpus both pages count, and the frame both pages share
-- ---------------------------------------------------------------------------

-- | One kind of writing, its pages, and their word counts.
data WritingKind = WritingKind
    { wkLabel :: String
    , wkItems :: [Item String]
    , _wkWCs  :: [Int]
    }

data Corpus = Corpus
    { cKinds  :: [WritingKind]
    , cPhotos :: [PhotoInfo]
    }

-- | Everything /build/ and /stats/ count. Loading data/build-stamp.txt,
--   which Main.main rewrites on every invocation, makes both pages
--   recompile each build: otherwise they are served from cache whenever no
--   tracked content changed, and the heatmap's "today" and every
--   unsafeCompiler-sourced figure (timestamp, output stats, git, LOC) go
--   stale. The value itself is unused.
loadCorpus :: Compiler Corpus
loadCorpus = do
    _ <- load (fromFilePath "data/build-stamp.txt") :: Compiler (Item String)
    kinds <- forM writingKinds $ \(label, pat) -> do
        items <- loadAll (pat .&&. hasNoVersion)
        WritingKind label items <$> mapM loadWC items
    -- 'allPhotoEntries' minus the series landings: a landing is a cover
    -- for a roll, not a frame in it, and counting both would inflate every
    -- figure by one per series. Patterns.hs owns the enumeration; this
    -- only narrows it.
    frames <- loadAll (P.allPhotoEntries
                  .&&. complement "content/photography/*/index.md"
                  .&&. hasNoVersion) :: Compiler [Item String]
    photos <- map toPhotoInfo <$> mapM (getMetadata . itemIdentifier) frames
    return (Corpus kinds photos)
  where
    writingKinds =
        [ ("Essays",       P.essayPattern)
        , ("Blog posts",   P.blogPattern)
        , ("Poems",        P.poetryPattern)
        , ("Fiction",      P.fictionPattern)
        , ("Compositions", "content/music/*/index.md")
        ]

kindItems :: String -> Corpus -> [Item String]
kindItems label c = concat [ wkItems k | k <- cKinds c, wkLabel k == label ]

corpusItems :: Corpus -> [Item String]
corpusItems = concatMap wkItems . cKinds

corpusRows :: Corpus -> [TypeRow]
corpusRows c = liveRows $
       [ TypeRow label (length items) (sum wcs) True | WritingKind label items wcs <- cKinds c ]
    ++ [ TypeRow "Photographs" (length (cPhotos c)) 0 False ]

-- | Either page, framed as an essay: its table of contents, a word count
--   and reading time of the rendered tables, title, abstract, and the
--   page's mark. The abstract is a constField, not front matter, so
--   Contexts.descriptionField cannot see it and would fall through to a
--   body excerpt — a table cell — hence the explicit description (C01).
statsPage :: String -> String -> FilePath -> H.Html -> H.Html -> Compiler (Item String)
statsPage title abstract markPath toc htmlContent = do
    let contentString = renderHtml htmlContent
        plainText     = stripHtmlTags contentString
        ctx           = constField "toc"          (renderHtml toc)
                     <> constField "word-count"   (show (length (words plainText)))
                     <> constField "reading-time" (show (readingTime plainText))
                     <> constField "title"        title
                     <> constField "abstract"     abstract
                     <> constField "description"  abstract
                     <> constField "build"        "true"
                     <> monogramSvgFieldFor markPath
                     <> hasMonogramFieldFor markPath
                     <> authorLinksField
                     <> siteCtx
    makeItem contentString
        >>= inDefault "templates/essay.html" ctx

-- ---------------------------------------------------------------------------
-- Rules
-- ---------------------------------------------------------------------------

statsRules :: Tags -> Rules ()
statsRules tags = do

  -- -------------------------------------------------------------------------
  -- Build telemetry page (/build/)
  -- -------------------------------------------------------------------------
  create ["build/index.html"] $ do
        route idRoute
        compile $ do
            corpus <- loadCorpus
            let essays = kindItems "Essays" corpus
                posts  = kindItems "Blog posts" corpus
                allWCs = concat [ wcs | WritingKind _ _ wcs <- cKinds corpus ]
                rows   = corpusRows corpus
            allPIs <- catMaybes <$> mapM loadPI (corpusItems corpus)

            -- ----------------------------------------------------------------
            -- Dates (essays + posts only)
            -- ----------------------------------------------------------------
            let getDateMeta item = do
                    meta   <- getMetadata (itemIdentifier item)
                    mRoute <- getRoute (itemIdentifier item)
                    let d = fromMaybe "" (lookupString "date" meta)
                        t = fromMaybe "(untitled)" (lookupString "title" meta)
                        u = maybe "#" canonicalUrlPath mRoute
                    return (d, t, u)
            essayDates <- mapM getDateMeta essays
            postDates  <- mapM getDateMeta posts
            let allDates    = filter (\(d,_,_) -> not (null d)) (essayDates ++ postDates)
                sortedDates = sortBy (comparing (\(d,_,_) -> d)) allDates
                oldestDate  = listToMaybe sortedDates
                newestDate  = listToMaybe (reverse sortedDates)

            -- ----------------------------------------------------------------
            -- Tags
            -- ----------------------------------------------------------------
            let tagFreqs   = map (\(t, ids) -> (t, length ids)) (tagsMap tags)
                topTags    = take 15 (sortBy (comparing (Down . snd)) tagFreqs)
                uniqueTags = length tagFreqs

            -- ----------------------------------------------------------------
            -- Backlinks: most-linked page + orphan count
            -- ----------------------------------------------------------------
            blItem <- load (fromFilePath "data/backlinks.json") :: Compiler (Item String)
            let rawBL   = itemBody blItem
                mBLVal  = Aeson.decodeStrict (TE.encodeUtf8 (T.pack rawBL)) :: Maybe Aeson.Value
                blPairs = case mBLVal of
                    Just (Aeson.Object km) ->
                        [ (T.unpack (AK.toText k),
                           case v of Aeson.Array arr -> V.length arr; _ -> 0)
                        | (k, v) <- KM.toList km ]
                    _ -> []
                blSet       = Set.fromList (map fst blPairs)
                orphanCount = length
                    [ p | p <- allPIs
                    , not (Set.member (normaliseUrl (piUrl p)) blSet) ]
                mostLinked  = listToMaybe (sortBy (comparing (Down . snd)) blPairs)
                -- The link is the page's own URL, not the backlinks key: a
                -- key has lost its .html, so linking it 404s wherever the
                -- server does not try $uri.html (audit X5).
                mostLinkedInfo = mostLinked >>= \(key, ct) ->
                    case find (\p -> normaliseUrl (piUrl p) == key) allPIs of
                        Just p  -> Just (piUrl p, ct, piTitle p)
                        Nothing -> Just (keyUrl key, ct, key)

            -- ----------------------------------------------------------------
            -- Epistemic coverage (essays + posts)
            -- ----------------------------------------------------------------
            kindMetas <- forM (cKinds corpus) $ \k ->
                (,) k <$> mapM (getMetadata . itemIdentifier) (wkItems k)
            let epMetas    = concat [ ms | (k, ms) <- kindMetas
                                         , wkLabel k `elem` ["Essays", "Blog posts"] ]
                epTotal    = length epMetas
                ep f       = length (filter (isJust . f) epMetas)
                withStatus = ep (lookupString "status")
                withConf   = ep (lookupString "confidence")
                withImp    = ep (lookupString "importance")
                withEv     = ep (lookupString "evidence")

            -- ----------------------------------------------------------------
            -- Marks coverage (per-portal monogram + epistemic-figure counts)
            --
            -- Monogram presence is a disk lookup via Marks.hasMonogram;
            -- epistemic-figure presence is the same trigger as the figure
            -- generator itself (status: set in frontmatter).
            -- ----------------------------------------------------------------
            markRows <- fmap liveMarkRows $ forM kindMetas $ \(k, metas) -> do
                monos <- mapM hasMonogram (wkItems k)
                return $ MarkRow (wkLabel k) (length (wkItems k))
                                 (length (filter id monos))
                                 (length (filter (isJust . lookupString "status") metas))

            -- ----------------------------------------------------------------
            -- Output directory stats
            -- ----------------------------------------------------------------
            (outputGrouped, totalFiles, totalSize) <-
                unsafeCompiler getOutputStats

            -- ----------------------------------------------------------------
            -- Lines of code + git stats
            -- ----------------------------------------------------------------
            (hf, hl, cf, cl, jf, jl) <- unsafeCompiler getLocStats
            (commits, firstDate)      <- unsafeCompiler getGitStats

            -- ----------------------------------------------------------------
            -- Link-archive coverage + link-rot health
            -- ----------------------------------------------------------------
            archiveMetrics <- unsafeCompiler archiveBuildStats

            -- ----------------------------------------------------------------
            -- Build timestamp + last build duration
            -- ----------------------------------------------------------------
            buildTimestamp <- unsafeCompiler $
                formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC" <$> getCurrentTime
            lastBuildDur <- unsafeCompiler $
                (readFile' "data/last-build-seconds.txt" >>= \s ->
                    let secs = fromMaybe 0 (readMaybe (filter (/= '\n') s) :: Maybe Int)
                    in  return (show secs ++ "s"))
                `catch` (\(_ :: IOException) -> return "\x2014")

            -- ----------------------------------------------------------------
            -- Assemble page
            -- ----------------------------------------------------------------
            let htmlContent :: H.Html
                htmlContent = do
                    renderContent rows
                    renderPages allPIs oldestDate newestDate
                    renderDistribution allWCs
                    renderTagsSection topTags uniqueTags
                    renderLinks mostLinkedInfo orphanCount (length allPIs)
                    renderEpistemic epTotal withStatus withConf withImp withEv
                    renderMarks markRows
                    renderOutput outputGrouped totalFiles totalSize
                    renderArchive archiveMetrics
                    renderRepository hf hl cf cl jf jl commits firstDate
                    renderBuild buildTimestamp lastBuildDur
            statsPage "Build Telemetry"
                "Per-build corpus statistics, tag distribution, link analysis, \
                \epistemic coverage, output metrics, repository overview, and \
                \build timing."
                "content/build.mark.svg" pageTOC htmlContent

  -- -------------------------------------------------------------------------
  -- Writing statistics page (/stats/)
  -- -------------------------------------------------------------------------
  create ["stats/index.html"] $ do
        route idRoute
        compile $ do
            corpus <- loadCorpus
            let allItems = corpusItems corpus
                typeRows = corpusRows corpus
                photos   = cPhotos corpus
            allPIs <- catMaybes <$> mapM loadPI allItems

            -- Build wordsByDay: for each item with a parseable `date`, map that
            -- day to the item's word count (summing if multiple items share a date).
            datePairs <- fmap catMaybes $ forM allItems $ \item -> do
                meta <- getMetadata (itemIdentifier item)
                wc   <- loadWC item
                return $ case lookupString "date" meta >>= parseIsoDate of
                    Nothing -> Nothing
                    Just d  -> Just (d, wc)
            let wordsByDay = Map.fromListWith (+) datePairs

            let tagFreqs   = map (\(t, ids) -> (t, length ids)) (tagsMap tags)
                topTags    = take 15 (sortBy (comparing (Down . snd)) tagFreqs)
                uniqueTags = length tagFreqs

            today <- unsafeCompiler (utctDay <$> getCurrentTime)

            let htmlContent :: H.Html
                htmlContent = do
                    section "activity" "Writing activity" (renderHeatmap wordsByDay today)
                    renderMonthlyVolume wordsByDay
                    renderCorpus typeRows allPIs
                    renderNotable allPIs
                    renderPhotography photos
                    renderTagsSection topTags uniqueTags
            -- "Statistics", matching the only link into this page (the
            -- portal row on the home page). The page stopped being about the
            -- writing alone once the photographs were counted.
            statsPage "Statistics"
                "Writing activity, corpus breakdown, photography, and tag \
                \distribution — computed at build time."
                "content/stats.mark.svg" statsTOC htmlContent
