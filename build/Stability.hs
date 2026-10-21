{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Stability auto-calculation, last-reviewed derivation, and version history.
--
-- For each content page:
--   * If the page's source path appears in @IGNORE.txt@, the stability and
--     last-reviewed fields fall back to the frontmatter values.
--   * Otherwise, @git log --follow@ is used.  Stability is derived from
--     commit count + age; last-reviewed is the most-recent commit date.
--
-- Version history (@$version-history$@):
--   * Prioritises frontmatter @history:@ list (date + note pairs).
--   * Falls back to the raw git log dates (date-only, no message).
--   * Falls back to nothing (template shows created/modified dates instead).
--
-- @IGNORE.txt@ is cleared by the build target in the Makefile after
-- every successful build, so pins are one-shot.
--
-- A page's fields each asked git separately (stability, last-reviewed and
-- its ISO twin, and every version-history field, with templates
-- evaluating @$if(x)$@ and @$x$@ apart): about seven @git log --follow@
-- runs per page and pass (audit H11). Both inputs are now read once per
-- process: 'gitDates' memoizes per path, and 'readIgnore' rereads
-- @IGNORE.txt@ only when its modification time changes.
module Stability
    ( stabilityField
    , resolveStability
    , lastReviewedField
    , lastReviewedIsoField
    , versionHistoryField
    , versionHistoryPrimaryField
    , versionHistoryRestField
    , versionHistoryRangeField
    , versionHistoryRangeStartField
    , versionHistoryRangeEndField
    , versionHistoryCommitsField
    , stabilityLabels
    ) where

import Control.Exception        (catch, IOException)
import Control.Concurrent.MVar  (MVar, modifyMVar, newMVar)
import Data.IORef               (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict    as Map
import Data.Aeson               (Value (..))
import qualified Data.Aeson.KeyMap  as KM
import qualified Data.Vector        as V
import Data.List                (sortBy, nub)
import Data.Maybe               (catMaybes, fromMaybe, listToMaybe)
import Data.Ord                 (comparing, Down (..))
import Data.Time.Calendar       (Day, diffDays)
import Data.Time.Clock          (UTCTime, getCurrentTime, utctDay)
import qualified Data.Text      as T
import qualified Data.Text.IO   as TIO
import System.Directory         (getModificationTime)
import System.Exit              (ExitCode (..))
import System.IO                (hPutStrLn, stderr)
import System.IO.Unsafe         (unsafePerformIO)
import System.Process           (readProcessWithExitCode)
import Hakyll
import Utils (parseIsoDate, isoToWriterly)

-- ---------------------------------------------------------------------------
-- IGNORE.txt
-- ---------------------------------------------------------------------------

-- | Read @IGNORE.txt@ (paths relative to project root, one per line).
-- Returns an empty list when the file is absent or empty.
--
-- Uses strict text IO so the file handle is released immediately rather
-- than left dangling on the lazy spine of 'readFile'.
--
-- Cached by modification time: one stat per call instead of one read, and
-- an edit during a long @make watch@ session is still picked up.
readIgnore :: IO [FilePath]
readIgnore = do
    mtime <- (Just <$> getModificationTime "IGNORE.txt")
             `catch` \(_ :: IOException) -> return Nothing
    memo <- readIORef ignoreCacheRef
    case (mtime, memo) of
        (Nothing, _) -> return []
        (Just t, Just (t', paths)) | t == t' -> return paths
        (Just t, _) -> do
            paths <- (filter (not . null) . map T.unpack . T.lines <$> TIO.readFile "IGNORE.txt")
                     `catch` \(_ :: IOException) -> return []
            atomicModifyIORef' ignoreCacheRef (const (Just (t, paths), ()))
            return paths

{-# NOINLINE ignoreCacheRef #-}
ignoreCacheRef :: IORef (Maybe (UTCTime, [FilePath]))
ignoreCacheRef = unsafePerformIO (newIORef Nothing)

-- ---------------------------------------------------------------------------
-- Git helpers
-- ---------------------------------------------------------------------------

-- | Return commit dates (ISO "YYYY-MM-DD", newest-first) for @fp@.
--
-- Logs git's stderr to the build's stderr when present so the author
-- isn't left in the dark when a file isn't tracked yet (the warning
-- otherwise vanishes silently).
--
-- Memoized per path for the life of the process: a build compiles each
-- page against a fixed history (the content auto-commit runs before it),
-- so the answer cannot change mid-run. Only successful, nonempty histories
-- are cached: git exits successfully for an untracked file too. Concurrent
-- requests share a lookup rather than all observing an empty cache slot.
-- A file that git does not know yet is asked about again. A @make watch@
-- session keeps the dates it first read until it restarts; it never makes
-- commits of its own.
gitDates :: FilePath -> IO [String]
gitDates fp = modifyMVar gitDatesCacheRef $ \cache -> do
    case Map.lookup fp cache of
        Just dates -> return (cache, dates)
        Nothing    -> do
            r <- gitDatesUncached fp
            case r of
                Just dates | not (null dates) -> return (Map.insert fp dates cache, dates)
                _ -> return (cache, [])

{-# NOINLINE gitDatesCacheRef #-}
gitDatesCacheRef :: MVar (Map.Map FilePath [String])
gitDatesCacheRef = unsafePerformIO (newMVar Map.empty)

gitDatesUncached :: FilePath -> IO (Maybe [String])
gitDatesUncached fp = do
    (ec, out, err) <- readProcessWithExitCode
        "git" ["log", "--follow", "--format=%ad", "--date=short", "--", fp] ""
    case ec of
        ExitFailure _ -> do
            let msg = if null err then "git log failed" else err
            hPutStrLn stderr $ "[Stability] " ++ fp ++ ": " ++ msg
            return Nothing
        ExitSuccess   -> do
            case err of
                "" -> return ()
                _  -> hPutStrLn stderr $ "[Stability] " ++ fp ++ ": " ++ err
            return $ Just (filter (not . null) (lines out))

-- | Commit dates for @fp@, newest-first, with a frontmatter fallback.
--
-- The repository history was restarted, which left every file looking
-- like a single commit dated the day of the squash. Read literally that
-- makes a four-year-old essay \"volatile\" — the label tracks the age of
-- the /repository/ rather than the age of the /writing/, which is not
-- what any reader means by it.
--
-- So the two sources are unioned rather than chosen between: real commit
-- dates AND the dates in the page's own @history:@ frontmatter, deduped
-- and newest-first. A count-based \"prefer git when it has more\" rule does
-- not work here, because @make build@ auto-commits @content\/@ on every
-- build — git quickly reports several commits, all dated today, which is
-- exactly the misleading signal the fallback exists to correct. Files with
-- no @history:@ frontmatter are unaffected. Those are authored facts
-- about the document, they survive any future rewrite, and they feed
-- all three consumers ('resolveStability', @$last-reviewed$@, and the
-- version-history block) from one place. Git still wins whenever it has
-- real history to report, so nothing changes for normally-tracked files.
effectiveDates :: FilePath -> Metadata -> IO [String]
effectiveDates fp meta =
    (\gd -> sortBy (comparing Down) (nub (gd ++ fmDates))) <$> gitDates fp
  where
    fmDates = map vhDateIso (parseFmHistory meta)

-- | Every label 'stabilityFromDates' gives, least to most settled. The
--   figure's tick count ("Marks"), the search filters and the epistemic
--   vocabulary (@site shared-rules@) all read this order.
stabilityLabels :: [String]
stabilityLabels = ["volatile", "revising", "fairly stable", "stable", "established"]

-- | Derive stability label from commit dates (newest-first), judged as
-- of @today@.
--
-- Thresholds (commit count + age in days since first commit):
--
--   * @volatile@      — solo commit OR less than two weeks old.
--   * @revising@      — under six commits AND under three months old.
--   * @fairly stable@ — under sixteen commits OR under one year old.
--   * @stable@        — under thirty-one commits OR under two years old.
--   * @established@   — anything beyond.
--
-- These cliffs are deliberately conservative: a fast burst of commits
-- early in a piece's life looks volatile until enough time has passed
-- to demonstrate it has settled. Age is measured from the first commit
-- to /today/, not to the most recent commit — a piece written in a
-- one-week burst must be able to stabilise as quiet time accumulates.
stabilityFromDates :: Day -> [String] -> String
stabilityFromDates _ [] = "volatile"
stabilityFromDates today dates =
    classify (length dates) ageDays
  where
    -- 'last' is safe: the [] case is handled above.
    ageDays = case parseIsoDate (last dates) of
        Just firstDay -> fromIntegral (diffDays today firstDay)
        Nothing       -> 0
    classify n age
        | n <= 1 || age < volatileAge        = "volatile"
        | n <= 5  && age < revisingAge       = "revising"
        | n <= 15 || age < fairlyStableAge   = "fairly stable"
        | n <= 30 || age < stableAge         = "stable"
        | otherwise                          = "established"

    volatileAge, revisingAge, fairlyStableAge, stableAge :: Int
    volatileAge     = 14
    revisingAge     = 90
    fairlyStableAge = 365
    stableAge       = 730

-- ---------------------------------------------------------------------------
-- Stability and last-reviewed context fields
-- ---------------------------------------------------------------------------

-- | Resolve the stability label for an item — frontmatter override
-- when the source path is pinned via @IGNORE.txt@, else the heuristic
-- run over @git log --follow@ on the source path.
--
-- Used by 'stabilityField' (which exposes the label as a context field)
-- and by Marks.hs (which feeds the label into the epistemic figure's
-- outer-ring tick count).
resolveStability :: Item a -> Compiler String
resolveStability item = do
    let srcPath = toFilePath (itemIdentifier item)
    meta <- getMetadata (itemIdentifier item)
    unsafeCompiler $ do
        ignored <- readIgnore
        if srcPath `elem` ignored
            then return $ fromMaybe "volatile" (lookupString "stability" meta)
            else do
                today <- utctDay <$> getCurrentTime
                stabilityFromDates today <$> effectiveDates srcPath meta

-- | Context field @$stability$@.
-- Always resolves to a label; prefers frontmatter when the file is pinned.
stabilityField :: Context String
stabilityField = field "stability" resolveStability

-- | Context field @$last-reviewed$@.
-- Returns the formatted date of the most-recent commit, or @noResult@ when
-- unavailable (making @$if(last-reviewed)$@ false in templates).
lastReviewedField :: Context String
lastReviewedField = field "last-reviewed" $ \item ->
    -- Formatted the same way for a pinned page, whose front-matter value
    -- is ISO, so it doesn't render a raw "2026-05-01".
    maybe (fail "no last-reviewed") (return . isoToWriterly) =<< lastReviewedIso item

-- | Raw-ISO companion to @$last-reviewed$@ — for hover-popup
-- @data-date-start@ attribute. Falls back to the frontmatter value for
-- pinned files (which is expected to already be ISO, the same convention
-- used by 'lastReviewedField' before it applied 'isoToWriterly'). Reads the same
-- 'effectiveDates' as @$last-reviewed$@, so the two cannot disagree (they
-- did when a @history:@ date was newer than the last commit; audit H11).
lastReviewedIsoField :: Context String
lastReviewedIsoField = field "last-reviewed-iso" $ \item ->
    maybe (fail "no last-reviewed ISO") return =<< lastReviewedIso item

-- | The page's last-reviewed date, ISO: its front-matter @last-reviewed:@
-- when the file is pinned in IGNORE.txt, else the newest 'effectiveDates'
-- entry. Both fields read this, so they cannot disagree.
lastReviewedIso :: Item a -> Compiler (Maybe String)
lastReviewedIso item = do
    let srcPath = toFilePath (itemIdentifier item)
    meta <- getMetadata (itemIdentifier item)
    unsafeCompiler $ do
        pinned <- (srcPath `elem`) <$> readIgnore
        if pinned
            then return $ lookupString "last-reviewed" meta
            else listToMaybe <$> effectiveDates srcPath meta

-- ---------------------------------------------------------------------------
-- Version history
-- ---------------------------------------------------------------------------

data VHEntry = VHEntry
    { vhDate    :: String         -- human-readable, e.g. "12 April 2026"
    , vhDateIso :: String         -- raw ISO, e.g. "2026-04-12"
    , vhMessage :: Maybe String   -- Nothing for git-log-only entries
    }

-- | Parse the optional frontmatter @history:@ list.
-- Each item must have @date:@ and @note:@ keys.
parseFmHistory :: Metadata -> [VHEntry]
parseFmHistory meta =
    case KM.lookup "history" meta of
        Just (Array v) -> catMaybes (map parseOne (V.toList v))
        _              -> []
  where
    parseOne (Object o) =
        case getString =<< KM.lookup "date" o of
            Nothing -> Nothing
            Just d  -> Just $ VHEntry (isoToWriterly d) d (getString =<< KM.lookup "note" o)
    parseOne _ = Nothing

    getString (String t) = Just (T.unpack t)
    getString _          = Nothing

-- | Get git log for a file as version history entries (date-only, no message).
gitLogHistory :: FilePath -> Metadata -> IO [VHEntry]
gitLogHistory fp meta = map (\d -> VHEntry (isoToWriterly d) d Nothing) <$> effectiveDates fp meta

-- | Maximum entries shown by default in the version-history footer block.
-- The remainder is revealed via a <details>/<summary> expand affordance,
-- matching the cap on the RELATED column.
versionHistoryHeadCount :: Int
versionHistoryHeadCount = 3

-- | Load version-history entries for an item.
-- Priority: frontmatter @history:@ list → git log dates → empty.
--
-- Entries are sorted newest-first by ISO date regardless of authored
-- order: every consumer (primary/rest split, range fields) assumes the
-- head is the newest entry, and the @history:@ list may be authored in
-- either direction. Git dates already arrive newest-first; the sort is
-- idempotent there.
loadVersionHistory :: Item a -> Compiler [VHEntry]
loadVersionHistory item = do
    let srcPath = toFilePath (itemIdentifier item)
    meta <- getMetadata (itemIdentifier item)
    let newestFirst = sortBy (comparing (Down . vhDateIso))
        fmEntries   = newestFirst (parseFmHistory meta)
    if not (null fmEntries)
        then return fmEntries
        else unsafeCompiler (newestFirst <$> gitLogHistory srcPath meta)

-- | Wrap a list of 'VHEntry' as Hakyll Items with unique paths so the
-- list field works correctly inside @$for$@.
vhItems :: String -> [VHEntry] -> [Item VHEntry]
vhItems tag =
    zipWith (\i e -> Item (fromFilePath (tag ++ "-" ++ show (i :: Int))) e)
            [1..]

-- | Shared sub-context for version-history entries: @$vh-date$@,
-- @$vh-date-iso$@ (raw ISO for hover popups), and (optionally) @$vh-message$@.
vhEntryCtx :: Context VHEntry
vhEntryCtx =
    field "vh-date"     (return . vhDate    . itemBody)
    <> field "vh-date-iso" (return . vhDateIso . itemBody)
    <> field "vh-message" (\i -> case vhMessage (itemBody i) of
            Nothing -> fail "no message"
            Just m  -> return m)

-- | Context list field @$version-history$@ — full list, kept for callers
-- (e.g. feeds, stats) that want every entry in one pass.
versionHistoryField :: Context String
versionHistoryField =
    listFieldWith "version-history" vhEntryCtx $ \item -> do
        entries <- loadVersionHistory item
        if null entries
            then fail "no version history"
            else return (vhItems "vh" entries)

-- | @$version-history-primary$@ — first 'versionHistoryHeadCount' entries,
-- rendered outside the expand affordance.
versionHistoryPrimaryField :: Context String
versionHistoryPrimaryField =
    listFieldWith "version-history-primary" vhEntryCtx $ \item -> do
        entries <- loadVersionHistory item
        let primary = take versionHistoryHeadCount entries
        if null primary
            then fail "no version history"
            else return (vhItems "vh-p" primary)

-- | @$version-history-rest$@ — overflow entries (count > head cap), which
-- the template wraps in a <details> block. Fails (noResult) when the total
-- fits inside the head cap, so @$if(version-history-rest)$@ collapses
-- cleanly.
versionHistoryRestField :: Context String
versionHistoryRestField =
    listFieldWith "version-history-rest" vhEntryCtx $ \item -> do
        entries <- loadVersionHistory item
        let rest = drop versionHistoryHeadCount entries
        if null rest
            then fail "no overflow"
            else return (vhItems "vh-r" rest)

-- | @$version-history-range$@ — formatted span between the oldest and
-- newest entry. A single-date history renders as that date alone; a
-- multi-date history renders as "OLDEST \x2013 NEWEST" (en-dash).
-- Fails when no history is available so @$if(version-history-range)$@
-- in the template falls back to a literal label.
--
-- Dates in the underlying VHEntry list are already pre-formatted
-- ("12 April 2026") by 'parseFmHistory' / 'gitLogHistory'.
versionHistoryRangeField :: Context String
versionHistoryRangeField = field "version-history-range" $ \item -> do
    entries <- loadVersionHistory item
    case entries of
        []          -> fail "no version-history range"
        [one]       -> return (vhDate one)
        es@(newest:_) ->
            let oldest = last es           -- safe: es is non-empty by pattern
                newD   = vhDate newest
                oldD   = vhDate oldest
            in  if newD == oldD
                    then return newD
                    else return (oldD ++ " \x2013 " ++ newD)

-- | Raw-ISO start date (oldest entry) for hover-popup machine use.
versionHistoryRangeStartField :: Context String
versionHistoryRangeStartField =
    field "version-history-range-start" $ \item -> do
        entries <- loadVersionHistory item
        case entries of
            [] -> fail "no version-history start"
            _  -> return (vhDateIso (last entries))

-- | Raw-ISO end date (newest entry) for hover-popup machine use.
-- Only resolves when the range spans more than one calendar day — single-day
-- histories don't need an end attribute on the popup trigger.
versionHistoryRangeEndField :: Context String
versionHistoryRangeEndField =
    field "version-history-range-end" $ \item -> do
        entries <- loadVersionHistory item
        case entries of
            []          -> fail "no version-history end"
            [_]         -> fail "single-day history — no end"
            es@(newest:_) ->
                let oldest = last es       -- safe: es is non-empty by pattern
                in  if vhDateIso newest == vhDateIso oldest
                        then fail "single-day history — no end"
                        else return (vhDateIso newest)

-- | Commit count — used by the frontmatter popup to surface the *density*
-- of attention the piece has received. Deliberately only wired into the
-- metadata-strip date link, not the aftermatter list (where it would be
-- redundant next to the enumeration of entries).
versionHistoryCommitsField :: Context String
versionHistoryCommitsField =
    field "version-history-commits" $ \item -> do
        entries <- loadVersionHistory item
        case entries of
            [] -> fail "no commits"
            _  -> return (show (length entries))
