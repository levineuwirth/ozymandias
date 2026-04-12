{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
module Contexts
    ( siteCtx
    , essayCtx
    , postCtx
    , pageCtx
    , poetryCtx
    , fictionCtx
    , compositionCtx
    , contentKindField
    , abstractField
    , tagLinksField
    , authorLinksField
    ) where

import Data.Aeson              (Value (..))
import qualified Data.Aeson.KeyMap  as KM
import qualified Data.Vector        as V
import Data.List               (intercalate, isPrefixOf)
import Data.Maybe              (fromMaybe)
import Data.Time.Calendar      (toGregorian)
import Data.Time.Clock         (getCurrentTime, utctDay)
import Data.Time.Format        (formatTime, defaultTimeLocale)
import System.FilePath         (takeDirectory, takeFileName)
import Text.Read               (readMaybe)
import qualified Data.Text     as T
import qualified Config
import Text.Pandoc             (runPure, readMarkdown, writeHtml5String, Pandoc(..), Block(..), Inline(..))
import Text.Pandoc.Options     (WriterOptions(..), HTMLMathMethod(..))
import Hakyll       hiding (trim)
import Backlinks    (backlinksField)
import SimilarLinks (similarLinksField)
import Stability    (stabilityField, lastReviewedField, versionHistoryField)
import Utils        (authorSlugify, authorNameOf, trim)

-- ---------------------------------------------------------------------------
-- Affiliation field
-- ---------------------------------------------------------------------------

-- | Parses the @affiliation@ frontmatter key and exposes each entry as
--   @affiliation-name@ / @affiliation-url@ pairs.
--
--   Accepts a scalar string or a YAML list. Each entry may use pipe syntax:
--     @"Brown University | https://cs.brown.edu"@
--   Entries without a URL still produce a row; @affiliation-url@ fails
--   (evaluates to noResult), so @$if(affiliation-url)$@ works in templates.
--
--   Usage:
--     $for(affiliation-links)$
--       $if(affiliation-url)$<a href="$affiliation-url$">$affiliation-name$</a>
--       $else$$affiliation-name$$endif$$sep$ · $endfor$
affiliationField :: Context a
affiliationField = listFieldWith "affiliation-links" ctx $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    let entries = case lookupStringList "affiliation" meta of
                    Just xs -> xs
                    Nothing -> maybe [] (:[]) (lookupString "affiliation" meta)
    return $ map (Item (fromFilePath "") . parseEntry) entries
  where
    ctx = field "affiliation-name" (return . fst . itemBody)
       <> field "affiliation-url"  (\i -> let u = snd (itemBody i)
                                           in if null u then noResult "no url" else return u)
    parseEntry s = case break (== '|') s of
        (name, '|' : url) -> (trim name, trim url)
        (name, _)         -> (trim name, "")

-- ---------------------------------------------------------------------------
-- Build time field
-- ---------------------------------------------------------------------------

-- | Resolves to the time the current item was compiled, formatted as
--   "Saturday, November 15th, 2025 15:05:55" (UTC).
buildTimeField :: Context String
buildTimeField = field "build-time" $ \_ ->
    unsafeCompiler $ do
        t <- getCurrentTime
        let (_, _, d) = toGregorian (utctDay t)
            prefix    = formatTime defaultTimeLocale "%A, %B " t
            suffix    = formatTime defaultTimeLocale ", %Y %H:%M:%S" t
        return (prefix ++ show d ++ ordSuffix d ++ suffix)
  where
    ordSuffix n
        | n `elem` [11,12,13] = "th"
        | n `mod` 10 == 1     = "st"
        | n `mod` 10 == 2     = "nd"
        | n `mod` 10 == 3     = "rd"
        | otherwise            = "th"

-- ---------------------------------------------------------------------------
-- Content kind field
-- ---------------------------------------------------------------------------

-- | @$item-kind$@: human-readable content type derived from the item's route.
-- Used on the New page to label each entry (Essay, Post, Poem, etc.).
contentKindField :: Context String
contentKindField = field "item-kind" $ \item -> do
    r <- getRoute (itemIdentifier item)
    return $ case r of
        Nothing -> "Page"
        Just r'
            | "essays/"  `isPrefixOf` r' -> "Essay"
            | "blog/"    `isPrefixOf` r' -> "Post"
            | "poetry/"  `isPrefixOf` r' -> "Poem"
            | "fiction/" `isPrefixOf` r' -> "Fiction"
            | "music/"   `isPrefixOf` r' -> "Composition"
            | otherwise                   -> "Page"

-- ---------------------------------------------------------------------------
-- Site-wide context
-- ---------------------------------------------------------------------------

-- | @$page-scripts$@ — list field providing @$script-src$@ for each entry
-- in the @js:@ frontmatter key (accepts a scalar string or a YAML list).
-- Returns an empty list when absent; $for iterates zero times, emitting nothing.
-- NOTE: do not use fail here — $for does not catch noResult the way $if does.
--
-- Each child Item is keyed on @<parent-identifier>#js-<index>@ so that two
-- pages referencing the same script path (e.g. @shared.js@) do not collide
-- in Hakyll's item store.
pageScriptsField :: Context String
pageScriptsField = listFieldWith "page-scripts" ctx $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    let scripts = case lookupStringList "js" meta of
                    Just xs -> xs
                    Nothing -> maybe [] (:[]) (lookupString "js" meta)
        parent  = toFilePath (itemIdentifier item)
    return $ zipWith
        (\i s -> Item (fromFilePath (parent ++ "#js-" ++ show (i :: Int))) s)
        [0 ..]
        scripts
  where
    ctx = field "script-src" (return . itemBody)

-- ---------------------------------------------------------------------------
-- Tag links field
-- ---------------------------------------------------------------------------

-- | List context field exposing an item's own (non-expanded) tags as
--   @tag-name@ / @tag-url@ objects.
--
--   $for(essay-tags)$<a href="$tag-url$">$tag-name$</a>$endfor$
tagLinksField :: String -> Context a
tagLinksField fieldName = listFieldWith fieldName ctx $ \item ->
    map toItem <$> getTags (itemIdentifier item)
  where
    toItem t = Item (fromFilePath (t ++ "/index.html")) t
    ctx      = field "tag-name" (return . itemBody)
            <> field "tag-url"  (\i -> return $ "/" ++ itemBody i ++ "/")

-- ---------------------------------------------------------------------------
-- Author links field
-- ---------------------------------------------------------------------------
--
-- 'authorSlugify' and 'authorNameOf' are imported from 'Utils' so that
-- they cannot drift from the copies in 'Authors'.

-- | Exposes each item's authors as @author-name@ / @author-url@ pairs.
--   Defaults to the site's configured @author-name@ when no "authors"
--   frontmatter key is present.
--
--   Entries that produce an empty name (e.g. @"| https://url"@) or an empty
--   slug (e.g. all-punctuation names) are dropped, so the field never emits
--   a @/authors//@ link.
--
--   $for(author-links)$<a href="$author-url$">$author-name$</a>$sep$, $endfor$
authorLinksField :: Context a
authorLinksField = listFieldWith "author-links" ctx $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    let entries = fromMaybe [] (lookupStringList "authors" meta)
        rawNames = if null entries then [Config.defaultAuthor] else map authorNameOf entries
        validNames = filter (\n -> not (null n) && not (null (authorSlugify n))) rawNames
        names = if null validNames then [Config.defaultAuthor] else validNames
    return $ map (\n -> Item (fromFilePath "") (n, "/authors/" ++ authorSlugify n ++ "/")) names
  where
    ctx = field "author-name" (return . fst . itemBody)
       <> field "author-url"  (return . snd . itemBody)

-- ---------------------------------------------------------------------------
-- Abstract field
-- ---------------------------------------------------------------------------

-- | Renders the abstract using Pandoc to support Markdown and LaTeX math.
--   Strips the outer @<p>@ wrapping. A single-paragraph abstract becomes a
--   bare @Plain@ so the rendered HTML is unwrapped inlines. A multi-paragraph
--   abstract (author used a blank line in the YAML literal block) is flattened
--   to a single @Plain@ with @LineBreak@ separators between what were
--   originally paragraph boundaries — the visual break is preserved without
--   emitting stray @<p>@ tags inside the metadata block. Mixed block content
--   (e.g. an abstract containing a blockquote) falls through unchanged.
abstractField :: Context String
abstractField = field "abstract" $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    case lookupString "abstract" meta of
        Nothing -> fail "no abstract"
        Just src -> do
            let pandocResult = runPure $ do
                    doc <- readMarkdown defaultHakyllReaderOptions (T.pack src)
                    let doc' = case doc of
                                 Pandoc m [Para ils] -> Pandoc m [Plain ils]
                                 Pandoc m blocks
                                   | all isPara blocks && not (null blocks) ->
                                       let joined = intercalate [LineBreak]
                                                      [ils | Para ils <- blocks]
                                       in Pandoc m [Plain joined]
                                 _ -> doc
                    let wOpts = defaultHakyllWriterOptions { writerHTMLMathMethod = MathML }
                    writeHtml5String wOpts doc'
            case pandocResult of
                Left err -> fail $ "Pandoc error rendering abstract: " ++ show err
                Right html -> return (T.unpack html)
  where
    isPara (Para _) = True
    isPara _        = False

siteCtx :: Context String
siteCtx =
    constField "site-title"          (T.unpack (Config.siteName        Config.siteConfig))
    <> constField "site-url"          (T.unpack (Config.siteUrl         Config.siteConfig))
    <> constField "site-description"  (T.unpack (Config.siteDescription Config.siteConfig))
    <> constField "site-language"     (T.unpack (Config.siteLanguage    Config.siteConfig))
    <> constField "author-name"       (T.unpack (Config.authorName      Config.siteConfig))
    <> constField "author-email"      (T.unpack (Config.authorEmail     Config.siteConfig))
    <> constField "license"           (T.unpack (Config.license         Config.siteConfig))
    <> optionalConstField "source-url"      (T.unpack (Config.sourceUrl      Config.siteConfig))
    <> optionalConstField "gpg-fingerprint" (T.unpack (Config.gpgFingerprint Config.siteConfig))
    <> optionalConstField "gpg-pubkey-url"  (T.unpack (Config.gpgPubkeyUrl   Config.siteConfig))
    <> navLinksField
    <> portalsField
    <> buildTimeField
    <> pageScriptsField
    <> abstractField
    <> defaultContext
  where
    optionalConstField name value
        | null value = field name (\_ -> fail (name ++ " is empty"))
        | otherwise  = constField name value

    navLinksField = listField "nav-links" navCtx (return navItems)
    navItems = zipWith
        (\i nl -> Item (fromFilePath ("nav-" ++ show (i :: Int))) nl)
        [0 :: Int ..]
        (Config.navLinks Config.siteConfig)
    navCtx = field "href"  (return . T.unpack . Config.navHref  . itemBody)
          <> field "label" (return . T.unpack . Config.navLabel . itemBody)

    portalsField = listField "portals" portalCtx (return portalItems)
    portalItems = zipWith
        (\i p -> Item (fromFilePath ("portal-" ++ show (i :: Int))) p)
        [0 :: Int ..]
        (Config.portals Config.siteConfig)
    portalCtx = field "portal-slug" (return . T.unpack . Config.portalSlug . itemBody)
             <> field "portal-name" (return . T.unpack . Config.portalName . itemBody)

-- ---------------------------------------------------------------------------
-- Helper: load a named snapshot as a context field
-- ---------------------------------------------------------------------------

-- | @snapshotField name snap@ creates a context field @name@ whose value is
--   the body of the snapshot @snap@ saved for the current item.
snapshotField :: String -> Snapshot -> Context String
snapshotField name snap = field name $ \item ->
    itemBody <$> loadSnapshot (itemIdentifier item) snap

-- ---------------------------------------------------------------------------
-- Essay context
-- ---------------------------------------------------------------------------

-- | Bibliography field: loads the citation HTML saved by essayCompiler.
--   Returns noResult (making $if(bibliography)$ false) when empty.
--   Also provides $has-citations$ for conditional JS loading.
bibliographyField :: Context String
bibliographyField = bibContent <> hasCitations
  where
    bibContent = field "bibliography" $ \item -> do
        bib <- itemBody <$> loadSnapshot (itemIdentifier item) "bibliography"
        if null bib then fail "no bibliography" else return bib
    hasCitations = field "has-citations" $ \item -> do
        bib <- itemBody <$> (loadSnapshot (itemIdentifier item) "bibliography"
                              :: Compiler (Item String))
        if null bib then fail "no citations" else return "true"

-- | Further-reading field: loads the further-reading HTML saved by essayCompiler.
--   Returns noResult (making $if(further-reading-refs)$ false) when empty.
furtherReadingField :: Context String
furtherReadingField = field "further-reading-refs" $ \item -> do
    fr <- itemBody <$> (loadSnapshot (itemIdentifier item) "further-reading-refs"
                          :: Compiler (Item String))
    if null fr then fail "no further reading" else return fr

-- ---------------------------------------------------------------------------
-- Epistemic fields
-- ---------------------------------------------------------------------------

-- | Render an integer 1–5 frontmatter key as filled/empty dot chars.
-- Returns @noResult@ when the key is absent or unparseable.
dotsField :: String -> String -> Context String
dotsField ctxKey metaKey = field ctxKey $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    case lookupString metaKey meta >>= readMaybe of
        Nothing -> fail (ctxKey ++ ": not set")
        Just (n :: Int) ->
            let v = max 0 (min 5 n)
            in  return (replicate v '\x25CF' ++ replicate (5 - v) '\x25CB')

-- | @$confidence-trend$@: ↑, ↓, or → derived from the last two entries
-- in the @confidence-history@ frontmatter list.  Returns @noResult@ when
-- there is no history or only a single entry.
--
-- The arrow flips when the absolute change crosses 'trendThreshold'
-- (currently 5 percentage points). Smaller swings count as flat.
confidenceTrendField :: Context String
confidenceTrendField = field "confidence-trend" $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    case lookupStringList "confidence-history" meta of
        Nothing -> fail "no confidence history"
        Just xs -> case lastTwo xs of
            Nothing            -> fail "no confidence history"
            Just (prevS, curS) ->
                let prev = readMaybe prevS :: Maybe Int
                    cur  = readMaybe curS  :: Maybe Int
                in  case (prev, cur) of
                        (Just p, Just c)
                            | c - p >  trendThreshold -> return "\x2191"   -- ↑
                            | p - c >  trendThreshold -> return "\x2193"   -- ↓
                            | otherwise               -> return "\x2192"   -- →
                        _ -> return "\x2192"
  where
    trendThreshold :: Int
    trendThreshold = 5

    -- Total replacement for @(xs !! (length xs - 2), last xs)@: returns
    -- the last two elements of a list, in order, or 'Nothing' when the
    -- list has fewer than two entries.
    lastTwo :: [a] -> Maybe (a, a)
    lastTwo []        = Nothing
    lastTwo [_]       = Nothing
    lastTwo [a, b]    = Just (a, b)
    lastTwo (_ : rest) = lastTwo rest

-- | @$overall-score$@: weighted composite of confidence (60 %) and
--   evidence quality (40 %), expressed as an integer on a 0–100 scale.
--
--   Importance is intentionally excluded from the score: it answers
--   "should you read this?", not "should you trust it?", and folding
--   the two together inflated the number and muddied its meaning.
--   It still appears in the footer as an independent orientation
--   signal — just not as a credibility input.
--
--   The 1–5 evidence scale is rescaled as @(ev − 1) / 4@ rather than
--   plain @ev / 5@. The naive form left a hidden +6 floor (since
--   @1/5 = 0.2@) and a midpoint of 0.6 instead of 0.5; the rescale
--   makes evidence=1 contribute zero and evidence=3 contribute exactly
--   half, so a "true midpoint" entry (conf=50, ev=3) lands on 50.
--
--   Returns @noResult@ when confidence or evidence is absent, so
--   @$if(overall-score)$@ guards the template safely.
--
--   Formula:  raw   = conf/100 · 0.6 + (ev − 1)/4 · 0.4   (0–1)
--             score = clamp₀₋₁₀₀(round(raw · 100))
overallScoreField :: Context String
overallScoreField = field "overall-score" $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    let readInt s = readMaybe s :: Maybe Int
    case ( readInt =<< lookupString "confidence" meta
         , readInt =<< lookupString "evidence"   meta
         ) of
        (Just conf, Just ev) ->
            let raw :: Double
                raw   = fromIntegral conf       / 100.0 * 0.6
                      + fromIntegral (ev - 1)   / 4.0   * 0.4
                score = max 0 (min 100 (round (raw * 100.0) :: Int))
            in  return (show score)
        _ -> fail "overall-score: confidence or evidence not set"

-- | All epistemic context fields composed.
epistemicCtx :: Context String
epistemicCtx =
    dotsField "importance-dots" "importance"
    <> dotsField "evidence-dots" "evidence"
    <> overallScoreField
    <> confidenceTrendField
    <> stabilityField
    <> lastReviewedField

-- ---------------------------------------------------------------------------
-- Essay context
-- ---------------------------------------------------------------------------

essayCtx :: Context String
essayCtx =
    authorLinksField
    <> affiliationField
    <> snapshotField "toc"          "toc"
    <> snapshotField "word-count"   "word-count"
    <> snapshotField "reading-time" "reading-time"
    <> bibliographyField
    <> furtherReadingField
    <> backlinksField
    <> similarLinksField
    <> epistemicCtx
    <> versionHistoryField
    <> dateField "date-created" "%-d %B %Y"
    <> dateField "date-modified" "%-d %B %Y"
    <> constField "math" "true"
    <> tagLinksField "essay-tags"
    <> siteCtx

-- ---------------------------------------------------------------------------
-- Post context
-- ---------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    authorLinksField
    <> affiliationField
    <> backlinksField
    <> similarLinksField
    <> dateField "date"     "%-d %B %Y"
    <> dateField "date-iso" "%Y-%m-%d"
    <> constField "math" "true"
    <> siteCtx

-- ---------------------------------------------------------------------------
-- Page context
-- ---------------------------------------------------------------------------

pageCtx :: Context String
pageCtx = authorLinksField <> affiliationField <> siteCtx

-- ---------------------------------------------------------------------------
-- Reading contexts (fiction + poetry)
-- ---------------------------------------------------------------------------

-- | Base reading context: essay fields + the "reading" flag (activates
--   reading.css / reading.js via head.html and body class via default.html).
readingCtx :: Context String
readingCtx = essayCtx <> constField "reading" "true"

-- | Poetry context: reading mode + "poetry" flag for CSS body class.
poetryCtx :: Context String
poetryCtx = readingCtx <> constField "poetry" "true"

-- | Fiction context: reading mode + "fiction" flag for CSS body class.
fictionCtx :: Context String
fictionCtx = readingCtx <> constField "fiction" "true"

-- ---------------------------------------------------------------------------
-- Composition context (music landing pages + score reader)
-- ---------------------------------------------------------------------------

data Movement = Movement
    { movName     :: String
    , movPage     :: Int
    , movDuration :: String
    , movAudio    :: Maybe String
    }

-- | Parse the @movements@ frontmatter key. Returns parsed movements and a
--   list of human-readable warnings for any entries that failed to parse.
--   Callers can surface the warnings via 'unsafeCompiler' so silent typos
--   don't strip movements without diagnostic.
parseMovementsWithWarnings :: Metadata -> ([Movement], [String])
parseMovementsWithWarnings meta =
    case KM.lookup "movements" meta of
        Just (Array v) ->
            let results = zipWith parseIndexed [1 :: Int ..] (V.toList v)
            in  ( [m | Right m <- results]
                , [w | Left  w <- results]
                )
        _ -> ([], [])
  where
    parseIndexed i value =
        case parseOne value of
            Just m  -> Right m
            Nothing -> Left $
                "movement #" ++ show i ++ " is missing a required field "
                ++ "(name, page, or duration) — entry skipped"

    parseOne (Object o) = Movement
        <$> (getString =<< KM.lookup "name"     o)
        <*> (getInt    =<< KM.lookup "page"     o)
        <*> (getString =<< KM.lookup "duration" o)
        <*> pure (getString =<< KM.lookup "audio" o)
    parseOne _ = Nothing

    getString (String t) = Just (T.unpack t)
    getString _          = Nothing

    getInt (Number n) = Just (floor (fromRational (toRational n) :: Double))
    getInt _          = Nothing

parseMovements :: Metadata -> [Movement]
parseMovements = fst . parseMovementsWithWarnings

-- | Extract the composition slug from an item's identifier.
--   "content/music/symphonic-dances/index.md" → "symphonic-dances"
compSlug :: Item a -> String
compSlug = takeFileName . takeDirectory . toFilePath . itemIdentifier

-- | Context for music composition landing pages and the score reader.
--   Extends essayCtx with composition-specific fields:
--     $slug$             — URL slug (e.g. "symphonic-dances")
--     $score-url$        — absolute URL of the score reader page
--     $has-score$        — present when score-pages frontmatter is non-empty
--     $score-page-count$ — total number of score pages
--     $score-pages$      — list of {score-page-url} items
--     $has-movements$    — present when movements frontmatter is non-empty
--     $movements$        — list of {movement-name, movement-page,
--                            movement-duration, movement-audio, has-audio}
--   All other frontmatter keys (instrumentation, duration, premiere,
--   commissioned-by, pdf, abstract, etc.) are available via defaultContext.
compositionCtx :: Context String
compositionCtx =
    constField "composition" "true"
    <> slugField
    <> scoreUrlField
    <> hasScoreField
    <> scorePageCountField
    <> scorePagesListField
    <> hasMovementsField
    <> movementsListField
    <> essayCtx
  where
    slugField = field "slug" (return . compSlug)

    scoreUrlField = field "score-url" $ \item ->
        return $ "/music/" ++ compSlug item ++ "/score/"

    hasScoreField = field "has-score" $ \item -> do
        meta <- getMetadata (itemIdentifier item)
        let pages = fromMaybe [] (lookupStringList "score-pages" meta)
        if null pages then fail "no score pages" else return "true"

    scorePageCountField = field "score-page-count" $ \item -> do
        meta <- getMetadata (itemIdentifier item)
        let pages = fromMaybe [] (lookupStringList "score-pages" meta)
        return $ show (length pages)

    scorePagesListField = listFieldWith "score-pages" spCtx $ \item -> do
        meta <- getMetadata (itemIdentifier item)
        let slug  = compSlug item
            base  = "/music/" ++ slug ++ "/"
            pages = fromMaybe [] (lookupStringList "score-pages" meta)
        return $ map (\p -> Item (fromFilePath p) (base ++ p)) pages
      where
        spCtx = field "score-page-url" (return . itemBody)

    hasMovementsField = field "has-movements" $ \item -> do
        meta <- getMetadata (itemIdentifier item)
        if null (parseMovements meta) then fail "no movements" else return "true"

    movementsListField = listFieldWith "movements" movCtx $ \item -> do
        meta <- getMetadata (itemIdentifier item)
        let (mvs, warnings) = parseMovementsWithWarnings meta
            ident = toFilePath (itemIdentifier item)
        unsafeCompiler $ mapM_
            (\w -> putStrLn $ "[Movements] " ++ ident ++ ": " ++ w)
            warnings
        return $ zipWith
            (\idx mv -> Item (fromFilePath ("mv" ++ show (idx :: Int))) mv)
            [1..] mvs
      where
        movCtx =
            field "movement-name"     (return . movName     . itemBody)
            <> field "movement-page"  (return . show . movPage . itemBody)
            <> field "movement-duration" (return . movDuration . itemBody)
            <> field "movement-audio"
                (\i -> maybe (fail "no audio") return (movAudio (itemBody i)))
            <> field "has-audio"
                (\i -> maybe (fail "no audio") (const (return "true"))
                             (movAudio (itemBody i)))
