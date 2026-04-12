{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Build telemetry page (/build/): corpus statistics, word-length
-- distribution, tag frequencies, link analysis, epistemic coverage,
-- output metrics, repository overview, and build timing.
-- Rendered as a full essay (3-column layout, TOC, metadata block).
module Stats (statsRules) where

import Control.Exception          (IOException, catch)
import Control.Monad              (forM)
import Data.Char                  (isSpace, toLower)
import Data.List                  (find, isPrefixOf, isSuffixOf, sort, sortBy)
import qualified Data.Map.Strict  as Map
import Data.Maybe                 (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Ord                   (comparing, Down (..))
import qualified Data.Set         as Set
import Data.String                (fromString)
import Data.Time                  (getCurrentTime, formatTime, defaultTimeLocale,
                                   Day, parseTimeM, utctDay, addDays, diffDays)
import Data.Time.Calendar         (toGregorian, dayOfWeek)
import System.Directory           (doesDirectoryExist, getFileSize, listDirectory,
                                   pathIsSymbolicLink)
import System.Exit                (ExitCode (..))
import System.FilePath            (takeExtension, (</>))
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
import Contexts                   (siteCtx, authorLinksField)
import qualified Patterns         as P
import Utils                      (readingTime)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data TypeRow = TypeRow
    { trLabel :: String
    , trCount :: Int
    , trWords :: Int
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
    return $ fmap (\r -> PageInfo
        { piTitle = fromMaybe "(untitled)" (lookupString "title" meta)
        , piUrl   = "/" ++ r
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

formatBytes :: Integer -> String
formatBytes b
    | b < 1024      = show b ++ " B"
    | b < 1024*1024 = showD (b * 10 `div` 1024)       ++ " KB"
    | otherwise     = showD (b * 10 `div` (1024*1024)) ++ " MB"
  where showD n = show (n `div` 10) ++ "." ++ show (n `mod` 10)

rtStr :: Int -> String
rtStr totalWords
    | mins < 60 = show mins ++ " min"
    | otherwise = show (mins `div` 60) ++ "h " ++ show (mins `mod` 60) ++ "m"
  where mins = totalWords `div` 200

pctStr :: Int -> Int -> String
pctStr _ 0     = "—"
pctStr n total = show (n * 100 `div` total) ++ "%"

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

-- | Normalise a page URL for backlink map lookup (strip trailing .html).
normUrl :: String -> String
normUrl u
    | ".html" `isSuffixOf` u = take (length u - 5) u
    | otherwise              = u

pad2 :: (Show a, Integral a) => a -> String
pad2 n = if n < 10 then "0" ++ show n else show n

-- | Median of a non-empty list; returns 0 for empty. Uses 'drop' +
-- pattern match instead of @(!!)@ so the function is total in its
-- own implementation, not just by external invariant.
median :: [Int] -> Int
median [] = 0
median xs =
    case drop (length xs `div` 2) (sort xs) of
        (m : _) -> m
        []      -> 0   -- unreachable: length xs >= 1 above


-- ---------------------------------------------------------------------------
-- Date helpers (for /stats/ page)
-- ---------------------------------------------------------------------------

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- | First Monday on or before 'day' (start of its ISO week).
weekStart :: Day -> Day
weekStart day = addDays (fromIntegral (negate (fromEnum (dayOfWeek day)))) day

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
isSafeUrl :: String -> Bool
isSafeUrl u =
    let norm = map toLower (dropWhile isSpace u)
    in  not ("//" `isPrefixOf` norm)
        && any (`isPrefixOf` norm) ["/", "https://", "mailto:", "#"]

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
        dowOf  d = fromEnum (dayOfWeek d)                 -- Mon=0..Sun=6
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
        else
            let maxWC = max 1 $ maximum $ Map.elems byMonth
                bar (y, m) =
                    let wc  = fromMaybe 0 (Map.lookup (y, m) byMonth)
                        pct = if wc == 0 then 0 else max 2 (wc * 100 `div` maxWC)
                        lbl = shortMonth m ++ " \x2019" ++ drop 2 (show y)
                    in  H.div H.! A.class_ "build-bar-row" $ do
                            H.span H.! A.class_ "build-bar-label" $ txt lbl
                            H.span H.! A.class_ "build-bar-wrap" $
                                H.span H.! A.class_ "build-bar"
                                       H.! A.style (H.stringValue ("width:" ++ show pct ++ "%"))
                                     $ mempty
                            H.span H.! A.class_ "build-bar-count" $
                                if wc > 0 then txt (commaInt wc) else mempty
            in  H.div H.! A.class_ "build-bars" $
                    mapM_ bar (Map.keys byMonth)

renderCorpus :: [TypeRow] -> [PageInfo] -> H.Html
renderCorpus typeRows allPIs =
    section "corpus" "Corpus" $ do
        dl [ ("Total words",        txt (commaInt totalWords))
           , ("Total pages",        txt (commaInt (length allPIs)))
           , ("Total reading time", txt (rtStr totalWords))
           , ("Average length",     txt (commaInt avgWC ++ " words"))
           , ("Median length",      txt (commaInt medWC ++ " words"))
           ]
        table ["Type", "Pages", "Words", "Reading time"]
              (map row typeRows)
              (Just [ "Total"
                    , txt (commaInt (sum (map trCount typeRows)))
                    , txt (commaInt totalWords)
                    , txt (rtStr totalWords)
                    ])
  where
    hasSomeWC  = filter (\p -> piWC p > 0) allPIs
    totalWords = sum (map trWords typeRows)
    avgWC      = if null hasSomeWC then 0 else totalWords `div` length hasSomeWC
    medWC      = median (map piWC hasSomeWC)
    row r = [ txt (trLabel r)
            , txt (commaInt (trCount r))
            , txt (commaInt (trWords r))
            , txt (rtStr    (trWords r))
            ]

renderNotable :: [PageInfo] -> H.Html
renderNotable allPIs =
    section "notable" "Notable" $ do
        H.p (H.strong "Longest")
        pageList (take 5 (sortBy (comparing (Down . piWC)) hasSomeWC))
        H.p (H.strong "Shortest")
        pageList (take 5 (sortBy (comparing piWC) hasSomeWC))
  where
    hasSomeWC = filter (\p -> piWC p > 50) allPIs
    pageList ps = H.ol H.! A.class_ "build-page-list" $
        mapM_ (\p -> H.li $ do
                  pageLink (piUrl p) (piTitle p)
                  txt (" \x2014 " ++ commaInt (piWC p) ++ " words")
              ) ps

-- | Renamed/aliased to 'renderTagsSection' below — kept as a name for
-- legacy call sites until they are migrated. Defining it as the same
-- function (instead of an independent copy) prevents the two from
-- drifting silently.
renderStatsTags :: [(String, Int)] -> Int -> H.Html
renderStatsTags = renderTagsSection

statsTOC :: H.Html
statsTOC = H.ol $ mapM_ item entries
  where
    item (i, t) =
        H.li $ H.a H.! A.href (H.stringValue ("#" ++ i))
                   H.! customAttr "data-target" i
                 $ txt t
    entries = [ ("activity", "Writing activity")
              , ("volume",   "Monthly volume")
              , ("corpus",   "Corpus")
              , ("notable",  "Notable")
              , ("tags",     "Tags")
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
    files <- walkDir "_site"
    let grouped = foldr (\(path, sz) acc ->
                    Map.insertWith (\(c1,s1) (c2,s2) -> (c1+c2, s1+s2))
                                   (displayExt path)
                                   (1, sz) acc)
                  Map.empty files
    return (grouped, length files, sum (map snd files))

-- ---------------------------------------------------------------------------
-- IO: lines of code
-- ---------------------------------------------------------------------------

countLinesDir :: FilePath -> String -> (FilePath -> Bool) -> IO (Int, Int)
countLinesDir dir ext skipPred = do
    entries <- listDirectory dir `catch` (\(_ :: IOException) -> return [])
    let files = filter (\e -> takeExtension e == ext && not (skipPred e)) entries
    -- Use strict text IO so the file handle is released as soon as the
    -- contents are read; the prior 'readFile' chained lazy IO under
    -- 'forM', leaving every handle open until the loop forced 'lines'.
    ls <- fmap sum $ forM files $ \e -> do
        content <- TIO.readFile (dir </> e)
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

renderContent :: [TypeRow] -> H.Html
renderContent rows =
    section "content" "Content" $
    table ["Type", "Count", "Words", "Reading time"]
          (map row rows)
          (Just [ "Total"
                , txt (commaInt totalCount)
                , txt (commaInt totalWords)
                , txt (rtStr    totalWords)
                ])
  where
    totalCount = sum (map trCount rows)
    totalWords = sum (map trWords rows)
    row r = [ txt (trLabel r)
            , txt (commaInt (trCount r))
            , txt (commaInt (trWords r))
            , txt (rtStr    (trWords r))
            ]

renderPages :: [PageInfo]
            -> Maybe (String, String, String)
            -> Maybe (String, String, String)
            -> H.Html
renderPages allPIs mOldest mNewest =
    section "pages" "Pages" $ do
        dl $
            [ ("Total pages",    txt (commaInt (length allPIs)))
            , ("Average length", txt (commaInt avgWC ++ " words"))
            ] ++
            maybe [] (\(d,t,u) -> [("Oldest content", datedLink d t u)]) mOldest ++
            maybe [] (\(d,t,u) -> [("Newest content", datedLink d t u)]) mNewest
        H.p (H.strong "Longest")
        pageList (take 3 (sortBy (comparing (Down . piWC)) hasSomeWC))
        H.p (H.strong "Shortest")
        pageList (take 3 (sortBy (comparing piWC)         hasSomeWC))
  where
    hasSomeWC = filter (\p -> piWC p > 0) allPIs
    avgWC     = if null hasSomeWC then 0
                else sum (map piWC hasSomeWC) `div` length hasSomeWC
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
    section "distribution" "Word-length distribution" $
    H.div H.! A.class_ "build-bars" $ mapM_ bar buckets
  where
    bucketOf w
        | w <  500 = 0
        | w < 1000 = 1
        | w < 2000 = 2
        | w < 5000 = 3
        | otherwise = 4
    labels :: [H.Html]
    labels = [ "< 500"
             , "500 \x2013 1k"
             , "1k \x2013 2k"
             , "2k \x2013 5k"
             , "\x2265 5k"
             ]
    counts = foldr (\w acc -> Map.insertWith (+) (bucketOf w) (1 :: Int) acc)
                   (Map.fromList [(i, 0 :: Int) | i <- [0 .. 4]]) wcs
    buckets  = [(labels !! i, fromMaybe 0 (Map.lookup i counts)) | i <- [0 .. 4]]
    maxCount = max 1 (maximum (map snd buckets))
    bar (lbl, n) =
        let pct = n * 100 `div` maxCount
        in  H.div H.! A.class_ "build-bar-row" $ do
                H.span H.! A.class_ "build-bar-label" $ lbl
                H.span H.! A.class_ "build-bar-wrap" $
                    H.span H.! A.class_ "build-bar"
                           H.! A.style (H.stringValue ("width:" ++ show pct ++ "%"))
                         $ mempty
                H.span H.! A.class_ "build-bar-count" $ txt (show n)

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

-- ---------------------------------------------------------------------------
-- Static TOC (matches the nine h2 sections above)
-- ---------------------------------------------------------------------------

pageTOC :: H.Html
pageTOC = H.ol $ mapM_ item sections
  where
    item (id_, title) =
        H.li $ H.a H.! A.href (H.stringValue ("#" ++ id_))
                   H.! customAttr "data-target" id_
                 $ txt title
    sections =
        [ ("content",      "Content")
        , ("pages",        "Pages")
        , ("distribution", "Word-length distribution")
        , ("tags",         "Tags")
        , ("links",        "Links")
        , ("epistemic",    "Epistemic coverage")
        , ("output",       "Output")
        , ("repository",   "Repository")
        , ("build",        "Build")
        ]

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
            -- ----------------------------------------------------------------
            -- Load all content items
            -- ----------------------------------------------------------------
            essays  <- loadAll (P.essayPattern             .&&. hasNoVersion)
            posts   <- loadAll ("content/blog/*.md"        .&&. hasNoVersion)
            poems   <- loadAll ("content/poetry/*.md"      .&&. hasNoVersion)
            fiction <- loadAll ("content/fiction/*.md"     .&&. hasNoVersion)
            comps   <- loadAll ("content/music/*/index.md" .&&. hasNoVersion)

            -- ----------------------------------------------------------------
            -- Word counts
            -- ----------------------------------------------------------------
            essayWCs   <- mapM loadWC essays
            postWCs    <- mapM loadWC posts
            poemWCs    <- mapM loadWC poems
            fictionWCs <- mapM loadWC fiction
            compWCs    <- mapM loadWC comps

            let allWCs = essayWCs ++ postWCs ++ poemWCs ++ fictionWCs ++ compWCs
                rows =
                    [ TypeRow "Essays"       (length essays)  (sum essayWCs)
                    , TypeRow "Blog posts"   (length posts)   (sum postWCs)
                    , TypeRow "Poems"        (length poems)   (sum poemWCs)
                    , TypeRow "Fiction"      (length fiction) (sum fictionWCs)
                    , TypeRow "Compositions" (length comps)   (sum compWCs)
                    ]

            -- ----------------------------------------------------------------
            -- Per-page info (title + URL + word count)
            -- ----------------------------------------------------------------
            allItems <- return (essays ++ posts ++ poems ++ fiction ++ comps)
            allPIs   <- catMaybes <$> mapM loadPI allItems

            -- ----------------------------------------------------------------
            -- Dates (essays + posts only)
            -- ----------------------------------------------------------------
            let getDateMeta item = do
                    meta   <- getMetadata (itemIdentifier item)
                    mRoute <- getRoute (itemIdentifier item)
                    let d = fromMaybe "" (lookupString "date" meta)
                        t = fromMaybe "(untitled)" (lookupString "title" meta)
                        u = maybe "#" (\r -> "/" ++ r) mRoute
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
                    , not (Set.member (normUrl (piUrl p)) blSet) ]
                mostLinked  = listToMaybe (sortBy (comparing (Down . snd)) blPairs)
                mostLinkedInfo = mostLinked >>= \(url, ct) ->
                    let mTitle = piTitle <$> find (\p -> normUrl (piUrl p) == url) allPIs
                    in  Just (url, ct, fromMaybe url mTitle)

            -- ----------------------------------------------------------------
            -- Epistemic coverage (essays + posts)
            -- ----------------------------------------------------------------
            essayMetas <- mapM (getMetadata . itemIdentifier) essays
            postMetas  <- mapM (getMetadata . itemIdentifier) posts
            let epMetas    = essayMetas ++ postMetas
                epTotal    = length epMetas
                ep f       = length (filter (isJust . f) epMetas)
                withStatus = ep (lookupString "status")
                withConf   = ep (lookupString "confidence")
                withImp    = ep (lookupString "importance")
                withEv     = ep (lookupString "evidence")

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
            -- Build timestamp + last build duration
            -- ----------------------------------------------------------------
            buildTimestamp <- unsafeCompiler $
                formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC" <$> getCurrentTime
            lastBuildDur <- unsafeCompiler $
                (readFile "data/last-build-seconds.txt" >>= \s ->
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
                    renderOutput outputGrouped totalFiles totalSize
                    renderRepository hf hl cf cl jf jl commits firstDate
                    renderBuild buildTimestamp lastBuildDur
                contentString = renderHtml htmlContent
                plainText     = stripHtmlTags contentString
                wc            = length (words plainText)
                rt            = readingTime plainText
                ctx           = constField "toc"          (renderHtml pageTOC)
                             <> constField "word-count"   (show wc)
                             <> constField "reading-time" (show rt)
                             <> constField "title"        "Build Telemetry"
                             <> constField "abstract"     "Per-build corpus statistics, tag distribution, \
                                                          \link analysis, epistemic coverage, output metrics, \
                                                          \repository overview, and build timing."
                             <> constField "build"        "true"
                             <> authorLinksField
                             <> siteCtx

            makeItem contentString
                >>= loadAndApplyTemplate "templates/essay.html"   ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

  -- -------------------------------------------------------------------------
  -- Writing statistics page (/stats/)
  -- -------------------------------------------------------------------------
  create ["stats/index.html"] $ do
        route idRoute
        compile $ do
            essays  <- loadAll (P.essayPattern             .&&. hasNoVersion)
            posts   <- loadAll ("content/blog/*.md"        .&&. hasNoVersion)
            poems   <- loadAll ("content/poetry/*.md"      .&&. hasNoVersion)
            fiction <- loadAll ("content/fiction/*.md"     .&&. hasNoVersion)
            comps   <- loadAll ("content/music/*/index.md" .&&. hasNoVersion)

            essayWCs   <- mapM loadWC essays
            postWCs    <- mapM loadWC posts
            poemWCs    <- mapM loadWC poems
            fictionWCs <- mapM loadWC fiction
            compWCs    <- mapM loadWC comps

            let allItems = essays ++ posts ++ poems ++ fiction ++ comps
                typeRows =
                    [ TypeRow "Essays"       (length essays)  (sum essayWCs)
                    , TypeRow "Blog posts"   (length posts)   (sum postWCs)
                    , TypeRow "Poems"        (length poems)   (sum poemWCs)
                    , TypeRow "Fiction"      (length fiction) (sum fictionWCs)
                    , TypeRow "Compositions" (length comps)   (sum compWCs)
                    ]

            allPIs <- catMaybes <$> mapM loadPI allItems

            -- Build wordsByDay: for each item with a parseable `date`, map that
            -- day to the item's word count (summing if multiple items share a date).
            datePairs <- fmap catMaybes $ forM allItems $ \item -> do
                meta <- getMetadata (itemIdentifier item)
                wc   <- loadWC item
                return $ case lookupString "date" meta >>= parseDay of
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
                    renderStatsTags topTags uniqueTags
                contentString = renderHtml htmlContent
                plainText     = stripHtmlTags contentString
                wc            = length (words plainText)
                rt            = readingTime plainText
                ctx           = constField "toc"          (renderHtml statsTOC)
                             <> constField "word-count"   (show wc)
                             <> constField "reading-time" (show rt)
                             <> constField "title"        "Writing Statistics"
                             <> constField "abstract"     "Writing activity, corpus breakdown, \
                                                          \and tag distribution — computed at build time."
                             <> constField "build"        "true"
                             <> authorLinksField
                             <> siteCtx

            makeItem contentString
                >>= loadAndApplyTemplate "templates/essay.html"   ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
