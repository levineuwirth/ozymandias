{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Now page: loads data/now.yaml and renders the active-projects view
-- and the recently-shipped archive for /current.html. Page-level
-- "Last updated" stamp is exposed as an absolute date; now.js adds
-- relative time in the browser, so it cannot become stale between builds.
module Now
    ( nowCtx
    , nowLastUpdated
    ) where

import Data.Aeson         (FromJSON (..), withObject, (.:), (.:?), (.!=))
import Data.Char          (toUpper)
import Data.List          (nub, sortBy)
import Data.Maybe         (fromMaybe)
import Data.Ord           (Down (..), comparing)
import Data.Time.Calendar (Day, diffDays)
import Data.Time.Clock    (UTCTime (..), getCurrentTime)
import Hakyll hiding (escapeHtml)
import Contexts (siteCtx)
import Utils    (escapeHtml, isoToWriterly, loadYaml, parseIsoDate)

-- ---------------------------------------------------------------------------
-- Entry types
-- ---------------------------------------------------------------------------

data NowEntry = NowEntry
    { neTitle    :: String
    , neSection  :: String
    , neStatus   :: String
    , neUpdated  :: String
    , neLink     :: Maybe String
    , neNote     :: Maybe String
    , nePriority :: Int
    }

instance FromJSON NowEntry where
    parseJSON = withObject "NowEntry" $ \o -> NowEntry
        <$> o .:  "title"
        <*> o .:  "section"
        <*> o .:  "status"
        <*> o .:  "updated"
        <*> o .:? "link"
        <*> o .:? "note"
        <*> o .:? "priority" .!= 0

data NowShipped = NowShipped
    { nsTitle     :: String
    , nsCompleted :: String
    , nsLink      :: Maybe String
    , nsNote      :: Maybe String
    }

instance FromJSON NowShipped where
    parseJSON = withObject "NowShipped" $ \o -> NowShipped
        <$> o .:  "title"
        <*> o .:  "completed"
        <*> o .:? "link"
        <*> o .:? "note"

data NowDoc = NowDoc
    { nLastUpdated :: String
    , nEntries     :: [NowEntry]
    , nShipped     :: [NowShipped]
    }

instance FromJSON NowDoc where
    parseJSON = withObject "NowDoc" $ \o -> NowDoc
        <$> o .:  "last-updated"
        <*> o .:? "entries" .!= []
        <*> o .:? "shipped" .!= []

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Section ordering follows first-appearance in entries. Reorder the
--   YAML to reorder the page; no separate ordering key required.
sectionOrder :: [NowEntry] -> [String]
sectionOrder = nub . map neSection

-- | Status ordering — "how close to shipping." Lower rank sorts first.
--   Statuses not listed sort below all known ones (rank 99) so a typo
--   surfaces visibly at the bottom of its section instead of silently
--   ranking next-to-the-top.
statusRanks :: [(String, Int)]
statusRanks =
    [ ("accepted",    0)
    , ("in-review",   1)
    , ("revising",    2)
    , ("drafting",    3)
    , ("building",    4)
    , ("early-stage", 5)
    , ("paused",      6)
    ]

statusRank :: String -> Int
statusRank s = fromMaybe 99 (lookup s statusRanks)

-- | How long ago an entry last moved, as a coarse bucket. Lower sorts
--   first: 0 is "this fortnight", 1 "this quarter-ish", 2 "quiet".
--
--   Coarse on purpose. The bucket outranks 'statusRank' below, so a
--   fine-grained measure would flatten the ladder into a date sort and
--   a one-day difference would reorder the page. Within a bucket the
--   ladder still says how close to shipping each item is.
--
--   A date that fails to parse buckets as quiet, matching
--   'statusRank''s treatment of an unknown status: bad data sinks
--   where it is visible rather than floating silently.
stalenessBucket :: Day -> String -> Int
stalenessBucket today iso =
    case parseIsoDate iso :: Maybe Day of
        Nothing -> 2
        Just d
            | age <= 14 -> 0
            | age <= 60 -> 1
            | otherwise -> 2
          where
            -- A future date is not stale; clamp so it buckets as fresh.
            age = max 0 (diffDays today d)

-- | Four-tier sort key for active entries:
--     1. priority   — manual override; higher floats up (default 0)
--     2. staleness  — how recently it moved (lower is more recent)
--     3. statusRank — how close to shipping (lower is closer)
--     4. updated    — exact-date tiebreaker within the same bucket
--
--   Staleness sits above the ladder because the page's subject is what
--   is moving, not what is furthest along: an @in-review@ paper that
--   has not been touched in two months should not outrank work that
--   advanced this week merely because @in-review@ outranks @building@.
--
--   Sectioning is applied to the *unsorted* list so section ordering
--   continues to follow YAML source order; sorting happens within each
--   section's filtered slice.
entrySortKey :: Day -> NowEntry -> (Down Int, Int, Int, Down String)
entrySortKey today e =
    ( Down (nePriority e)
    , stalenessBucket today (neUpdated e)
    , statusRank (neStatus e)
    , Down (neUpdated e)
    )

-- | "early-stage" → "Early Stage", "research" → "Research".
titleCaseWords :: String -> String
titleCaseWords = unwords . map cap . wordsOnDash
  where
    cap []     = []
    cap (x:xs) = toUpper x : xs
    wordsOnDash s = case break (== '-') s of
        (a, [])     -> [a]
        (a, _:rest) -> a : wordsOnDash rest

-- ---------------------------------------------------------------------------
-- HTML rendering
-- ---------------------------------------------------------------------------

renderStatusChip :: String -> String
renderStatusChip s = concat
    [ "<span class=\"now-status now-status--", escapeHtml s, "\">"
    , escapeHtml (titleCaseWords s)
    , "</span>"
    ]

-- | Active-entry card. Reuses the .item-card / .item-card-* classes from
--   item-card.css so the Now page picks up the existing typographic
--   register; the .now-* classes layer status-chip + spacing on top.
renderEntry :: NowEntry -> String
renderEntry e =
    nowCard "item-card now-card" (neStatus e) (neLink e) (neTitle e) (neUpdated e) (neNote e)

renderShippedEntry :: NowShipped -> String
renderShippedEntry s =
    nowCard "item-card now-card now-card--shipped" "shipped"
            (nsLink s) (nsTitle s) (nsCompleted s) (nsNote s)

-- | The card both kinds of entry render as: classes, status chip, title
--   (linked when there is a URL), its ISO date, and an optional note.
nowCard :: String -> String -> Maybe String -> String -> String -> Maybe String -> String
nowCard classes status link title date note = concat
    [ "<li class=\"", classes, "\">"
    , "<span class=\"item-card-kind now-kind\">"
    , renderStatusChip status
    , "</span>"
    , "<div class=\"item-card-main\">"
    , "<div class=\"item-card-header\">"
    , renderTitle link title
    , "<time class=\"item-card-date\" datetime=\"", escapeHtml date, "\">"
    , escapeHtml date
    , "</time>"
    , "</div>"
    , maybe "" (\n -> "<p class=\"item-card-abstract is-full\">" ++ escapeHtml n ++ "</p>") note
    , "</div>"
    , "</li>"
    ]

renderTitle :: Maybe String -> String -> String
renderTitle mu title = case mu of
    Just url -> "<a class=\"item-card-title\" href=\"" ++ escapeHtml url ++ "\">" ++ escapeHtml title ++ "</a>"
    Nothing  -> "<span class=\"item-card-title\">" ++ escapeHtml title ++ "</span>"

renderSection :: String -> [NowEntry] -> String
renderSection sec es = concat
    [ "<section class=\"now-section library-section\">"
    , "<h2 class=\"now-section-heading\">"
    , escapeHtml (titleCaseWords sec)
    , "</h2>"
    , "<ul class=\"item-card-list\">"
    , concatMap renderEntry es
    , "</ul>"
    , "</section>"
    ]

-- | Render every section. Takes the build date because 'entrySortKey'
--   buckets entries by how long ago they moved — which means this page's
--   order can change with no edit to now.yaml, as an entry ages out of a
--   bucket. That is the intent, and it is the one place the rendering is
--   a function of when the build ran.
renderEntries :: Day -> [NowEntry] -> String
renderEntries _ [] = ""
renderEntries today entries = concatMap renderOne (sectionOrder entries)
  where
    renderOne sec =
        let inSec = filter ((== sec) . neSection) entries
            sorted = sortBy (comparing (entrySortKey today)) inSec
        in renderSection sec sorted

renderShippedAll :: [NowShipped] -> String
renderShippedAll [] = ""
renderShippedAll items = concat
    [ "<section class=\"now-section now-section--shipped library-section\">"
    , "<h2 class=\"now-section-heading\">Recently Shipped</h2>"
    , "<ul class=\"item-card-list\">"
    , concatMap renderShippedEntry sorted
    , "</ul>"
    , "</section>"
    ]
  where
    sorted = sortBy (comparing (Down . nsCompleted)) items

-- ---------------------------------------------------------------------------
-- Load
-- ---------------------------------------------------------------------------

-- The masthead describes the whole feed, so it cannot predate a dated item.
-- Check at the source boundary rather than relying on the editor to remember
-- to bump a second field whenever an entry moves.
validateNowDoc :: NowDoc -> Either String NowDoc
validateNowDoc doc = do
    stamp <- parseDate "last-updated" (nLastUpdated doc)
    entryDates <- traverse (\e -> parseDate ("updated for " ++ neTitle e) (neUpdated e)) (nEntries doc)
    shippedDates <- traverse (\s -> parseDate ("completed for " ++ nsTitle s) (nsCompleted s)) (nShipped doc)
    if any (> stamp) (entryDates ++ shippedDates)
        then Left "last-updated predates an entry's updated or completed date"
        else Right doc
  where
    parseDate label iso =
        case parseIsoDate iso :: Maybe Day of
            Nothing -> Left (label ++ " is not a valid YYYY-MM-DD date: " ++ iso)
            Just d  -> Right d

loadNow :: Compiler NowDoc
loadNow = do
    doc <- loadYaml "data/now.yaml"
    either (fail . ("now.yaml: " ++)) return (validateNowDoc doc)

-- ---------------------------------------------------------------------------
-- Context
-- ---------------------------------------------------------------------------

nowLastUpdated :: Compiler String
nowLastUpdated = nLastUpdated <$> loadNow

nowCtx :: Context String
nowCtx =
    constField "now" "true"
    <> field "now-last-updated" (\_ -> nowLastUpdated)
    <> field "now-last-updated-display" (\_ -> isoToWriterly . nLastUpdated <$> loadNow)
    <> field "now-entries-html" (\_ -> do
        doc  <- loadNow
        nowT <- unsafeCompiler getCurrentTime
        return (renderEntries (utctDay nowT) (nEntries doc))
      )
    <> field "now-shipped-html" (\_ -> renderShippedAll . nShipped <$> loadNow)
    <> siteCtx
