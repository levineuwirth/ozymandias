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
module Stability
    ( stabilityField
    , lastReviewedField
    , versionHistoryField
    ) where

import Control.Exception        (catch, IOException)
import Data.Aeson               (Value (..))
import qualified Data.Aeson.KeyMap  as KM
import qualified Data.Vector        as V
import Data.Maybe               (catMaybes, fromMaybe, listToMaybe)
import Data.Time.Calendar       (Day, diffDays)
import Data.Time.Format         (parseTimeM, formatTime, defaultTimeLocale)
import qualified Data.Text      as T
import qualified Data.Text.IO   as TIO
import System.Exit              (ExitCode (..))
import System.IO                (hPutStrLn, stderr)
import System.Process           (readProcessWithExitCode)
import Hakyll

-- ---------------------------------------------------------------------------
-- IGNORE.txt
-- ---------------------------------------------------------------------------

-- | Read @IGNORE.txt@ (paths relative to project root, one per line).
-- Returns an empty list when the file is absent or empty.
--
-- Uses strict text IO so the file handle is released immediately rather
-- than left dangling on the lazy spine of 'readFile'.
readIgnore :: IO [FilePath]
readIgnore =
    (filter (not . null) . map T.unpack . T.lines <$> TIO.readFile "IGNORE.txt")
    `catch` \(_ :: IOException) -> return []

-- ---------------------------------------------------------------------------
-- Git helpers
-- ---------------------------------------------------------------------------

-- | Return commit dates (ISO "YYYY-MM-DD", newest-first) for @fp@.
--
-- Logs git's stderr to the build's stderr when present so the author
-- isn't left in the dark when a file isn't tracked yet (the warning
-- otherwise vanishes silently).
gitDates :: FilePath -> IO [String]
gitDates fp = do
    (ec, out, err) <- readProcessWithExitCode
        "git" ["log", "--follow", "--format=%ad", "--date=short", "--", fp] ""
    case ec of
        ExitFailure _ -> do
            let msg = if null err then "git log failed" else err
            hPutStrLn stderr $ "[Stability] " ++ fp ++ ": " ++ msg
            return []
        ExitSuccess   -> do
            case err of
                "" -> return ()
                _  -> hPutStrLn stderr $ "[Stability] " ++ fp ++ ": " ++ err
            return $ filter (not . null) (lines out)

-- | Parse an ISO "YYYY-MM-DD" string to a 'Day'.
parseIso :: String -> Maybe Day
parseIso = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- | Approximate day-span between the oldest and newest ISO date strings.
daySpan :: String -> String -> Int
daySpan oldest newest =
    case (parseIso oldest, parseIso newest) of
        (Just o, Just n) -> fromIntegral (abs (diffDays n o))
        _                -> 0

-- | Derive stability label from commit dates (newest-first).
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
-- to demonstrate it has settled.
stabilityFromDates :: [String] -> String
stabilityFromDates [] = "volatile"
stabilityFromDates dates@(newest : _) =
    let oldest = case reverse dates of
                    (x : _) -> x
                    []      -> newest          -- unreachable; matched above
    in  classify (length dates) (daySpan oldest newest)
  where
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

-- | Format an ISO date as "%-d %B %Y" (e.g. "16 March 2026").
fmtIso :: String -> String
fmtIso s = case parseIso s of
    Nothing  -> s
    Just day -> formatTime defaultTimeLocale "%-d %B %Y" (day :: Day)

-- ---------------------------------------------------------------------------
-- Stability and last-reviewed context fields
-- ---------------------------------------------------------------------------

-- | Context field @$stability$@.
-- Always resolves to a label; prefers frontmatter when the file is pinned.
stabilityField :: Context String
stabilityField = field "stability" $ \item -> do
    let srcPath = toFilePath (itemIdentifier item)
    meta <- getMetadata (itemIdentifier item)
    unsafeCompiler $ do
        ignored <- readIgnore
        if srcPath `elem` ignored
            then return $ fromMaybe "volatile" (lookupString "stability" meta)
            else stabilityFromDates <$> gitDates srcPath

-- | Context field @$last-reviewed$@.
-- Returns the formatted date of the most-recent commit, or @noResult@ when
-- unavailable (making @$if(last-reviewed)$@ false in templates).
lastReviewedField :: Context String
lastReviewedField = field "last-reviewed" $ \item -> do
    let srcPath = toFilePath (itemIdentifier item)
    meta <- getMetadata (itemIdentifier item)
    mDate <- unsafeCompiler $ do
        ignored <- readIgnore
        if srcPath `elem` ignored
            then return $ lookupString "last-reviewed" meta
            else fmap fmtIso . listToMaybe <$> gitDates srcPath
    case mDate of
        Nothing -> fail "no last-reviewed"
        Just d  -> return d

-- ---------------------------------------------------------------------------
-- Version history
-- ---------------------------------------------------------------------------

data VHEntry = VHEntry
    { vhDate    :: String
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
            Just d  -> Just $ VHEntry (fmtIso d) (getString =<< KM.lookup "note" o)
    parseOne _ = Nothing

    getString (String t) = Just (T.unpack t)
    getString _          = Nothing

-- | Get git log for a file as version history entries (date-only, no message).
gitLogHistory :: FilePath -> IO [VHEntry]
gitLogHistory fp = map (\d -> VHEntry (fmtIso d) Nothing) <$> gitDates fp

-- | Context list field @$version-history$@ providing @$vh-date$@ and
-- (when present) @$vh-message$@ per entry.
--
-- Priority:
--   1. Frontmatter @history:@ list — dates + authored notes.
--   2. Git log dates — date-only, no annotation.
--   3. Empty list — template falls back to @$date-created$@ / @$date-modified$@.
versionHistoryField :: Context String
versionHistoryField = listFieldWith "version-history" vhCtx $ \item -> do
    let srcPath = toFilePath (itemIdentifier item)
    meta <- getMetadata (itemIdentifier item)
    let fmEntries = parseFmHistory meta
    entries <-
        if not (null fmEntries)
            then return fmEntries
            else unsafeCompiler (gitLogHistory srcPath)
    if null entries
        then fail "no version history"
        else return $ zipWith
                (\i e -> Item (fromFilePath ("vh" ++ show (i :: Int))) e)
                [1..] entries
  where
    vhCtx =
        field "vh-date"    (return . vhDate . itemBody)
        <> field "vh-message" (\i -> case vhMessage (itemBody i) of
                Nothing -> fail "no message"
                Just m  -> return m)
