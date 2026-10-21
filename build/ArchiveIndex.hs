{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | ArchiveIndex — shared read-only access to the archive's two JSON
--   sidecars: @data/archive-index.json@ (the @url\/alias -> slug@ map
--   written by @archive.py fetch@) and @data/archive-state.json@ (the
--   per-URL link-rot status written by @archive.py check@).
--
--   Consumers:
--
--     * @Filters.Archive@ — appends the archive affordance to body links
--       whose target is archived, and flips a @rotted@ link to the local
--       copy.
--     * @Backlinks@ — keeps archived external links through pass 1 and
--       canonicalises them to their @/archive/<slug>/@ page in pass 2.
--     * @Archive@ — surfaces each entry's rot status on its page, the
--       @/archive/@ index, and the @/build/@ telemetry.
--
--   Both files are loaded once per *process* via NOINLINE
--   @unsafePerformIO@ CAFs (as are the manifest/removed URL sets below).
--   An absent or malformed file degrades safely: an empty index makes the
--   link consumers no-op; an absent state file makes every entry @Live@
--   (the safe default — no link flip). @archive.py check@ is decoupled
--   from @make build@; a build consumes whatever state file exists.
--
--   Consequence of the once-per-process read (shared with the manifest
--   read in 'Archive.archiveRules'): under @site watch@, edits to
--   @manifest.yaml@, @removed.yaml@, or the regenerated state JSONs are
--   not re-read — the server renders stale archive state until restart.
--   One-shot builds (@make build@ / @make deploy@) are unaffected.
module ArchiveIndex
    ( ArchiveStatus (..)
    , statusName
    , archiveSlugFor
    , archiveStatusForSlug
    , archiveIndexIsEmpty
    , normalizeUrl
    , trackingParams
    ) where

import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Aeson             as A
import           Data.Aeson             ((.!=), (.:), (.:?))
import qualified Data.Yaml              as Y
import           System.Directory       (doesFileExist)
import           System.IO.Unsafe       (unsafePerformIO)

-- ---------------------------------------------------------------------------
-- Link-rot status
-- ---------------------------------------------------------------------------

-- | The link-rot status of an archived work's original URL, as set by
--   @archive.py check@. 'Live' is the safe default for an unscanned or
--   unknown entry.
data ArchiveStatus = Live | Moved | Rotted | Error
    deriving (Eq, Show)

-- | The lower-case wire name, matching @archive-state.json@ and the
--   @status:@ Pagefind filter tag.
statusName :: ArchiveStatus -> String
statusName Live   = "live"
statusName Moved  = "moved"
statusName Rotted = "rotted"
statusName Error  = "error"

parseStatus :: Text -> ArchiveStatus
parseStatus "moved"  = Moved
parseStatus "rotted" = Rotted
parseStatus "error"  = Error
parseStatus _        = Live

-- ---------------------------------------------------------------------------
-- JSON shapes
-- ---------------------------------------------------------------------------

-- | One @archive-index.json@ entry. Only @slug@ and @aliases@ are used.
data IdxEntry = IdxEntry
    { ieSlug    :: String
    , ieAliases :: [Text]
    }

instance A.FromJSON IdxEntry where
    parseJSON = A.withObject "IdxEntry" $ \o -> IdxEntry
        <$> o .:  "slug"
        <*> (o .:? "aliases" .!= [])

-- | One @archive-state.json@ entry — only the @status@ is consumed here.
newtype StateEntry = StateEntry { seStatus :: ArchiveStatus }

instance A.FromJSON StateEntry where
    parseJSON = A.withObject "StateEntry" $ \o ->
        StateEntry . parseStatus <$> (o .:? "status" .!= "live")

newtype UrlEntry = UrlEntry { ueUrl :: Text }

instance A.FromJSON UrlEntry where
    parseJSON = A.withObject "UrlEntry" $ \o ->
        UrlEntry <$> o .: "url"

-- ---------------------------------------------------------------------------
-- Loaded-once CAFs
-- ---------------------------------------------------------------------------

indexPath, statePath, manifestPath, removedPath :: FilePath
indexPath = "data/archive-index.json"
statePath = "data/archive-state.json"
manifestPath = "archive/manifest.yaml"
removedPath = "archive/removed.yaml"

readUrlSet :: FilePath -> IO (Set Text)
readUrlSet path = do
    exists <- doesFileExist path
    if not exists
        then return Set.empty
        else do
            decoded <- Y.decodeFileEither path
            case decoded of
                Right entries -> return . Set.fromList $
                    map (normalizeUrl . ueUrl) (entries :: [UrlEntry])
                Left e -> ioError . userError $
                    "[archive] FATAL: " ++ path ++ ": " ++ show e

-- | Normalised URLs recorded as deliberate takedowns in @removed.yaml@.
--   Consulted independently of 'activeUrls': a takedown must also knock
--   out any *alias* key a stale index still carries for another entry.
{-# NOINLINE removedUrls #-}
removedUrls :: Set Text
removedUrls = unsafePerformIO (readUrlSet removedPath)

-- | Canonical URLs still permitted to participate in link annotation.
--   Filtering the generated index at build time makes a direct Hakyll build
--   respect authored manifest/removal state even when archive.py did not run.
{-# NOINLINE activeUrls #-}
activeUrls :: Set Text
activeUrls = unsafePerformIO $ do
    manifest <- readUrlSet manifestPath
    return (manifest `Set.difference` removedUrls)

-- | @canonical-url -> entry@. Absent/malformed file -> empty; entries no
--   longer permitted by the authored manifest/removal state are removed.
{-# NOINLINE rawIndex #-}
rawIndex :: Map Text IdxEntry
rawIndex = unsafePerformIO $ do
    exists <- doesFileExist indexPath
    if not exists
        then return Map.empty
        else do
            decoded <- A.eitherDecodeFileStrict' indexPath
            let parsed = either (const Map.empty) id decoded
            return $ Map.filterWithKey
                (\canon _ -> normalizeUrl canon `Set.member` activeUrls)
                parsed

-- | @url -> status@. Absent/malformed file -> empty (every entry 'Live').
{-# NOINLINE rawState #-}
rawState :: Map Text ArchiveStatus
rawState = unsafePerformIO $ do
    exists <- doesFileExist statePath
    if not exists
        then return Map.empty
        else do
            decoded <- A.eitherDecodeFileStrict' statePath
            return $ either (const Map.empty) (Map.map seStatus) decoded

-- | @normalised-url -> slug@: the canonical key and every alias from
--   @archive-index.json@, each fed through 'normalizeUrl'. Both keys and
--   lookups are normalised, so a citation form the alias set cannot
--   enumerate (e.g. an unbounded arXiv version, or any tracking-laden
--   variant of a clean manifest URL) still resolves. Alias keys that
--   match a recorded takedown are dropped, so a stale index (archive.py
--   not run since removed.yaml changed) cannot keep annotating a removed
--   work through an alias of a still-active entry.
{-# NOINLINE flatIndex #-}
flatIndex :: Map Text String
flatIndex = Map.fromList
    [ (nkey, ieSlug e)
    | (canon, e) <- Map.toList rawIndex
    , key        <- canon : ieAliases e
    , let nkey = normalizeUrl key
    , nkey `Set.notMember` removedUrls
    ]

-- | @slug -> status@: each entry's status, looked up by its canonical URL
--   in the state file (the two files share the manifest URL as key).
{-# NOINLINE slugStatus #-}
slugStatus :: Map String ArchiveStatus
slugStatus = Map.fromList
    [ (ieSlug e, Map.findWithDefault Live canon rawState)
    | (canon, e) <- Map.toList rawIndex
    ]

-- ---------------------------------------------------------------------------
-- Public lookups
-- ---------------------------------------------------------------------------

-- | True when no archive index is available — the link consumers no-op.
archiveIndexIsEmpty :: Bool
archiveIndexIsEmpty = Map.null rawIndex

-- | The archive slug for an outbound URL, or 'Nothing'. Both the index
--   keys and the input go through 'normalizeUrl', so a citation form that
--   the alias set cannot enumerate — an unbounded arXiv version, or any
--   tracking-laden variant of a clean manifest URL — still resolves.
archiveSlugFor :: Text -> Maybe String
archiveSlugFor url = Map.lookup (normalizeUrl url) flatIndex

-- | The link-rot status of an archived entry, by slug. 'Live' for an
--   unknown slug or when no scan has run.
archiveStatusForSlug :: String -> ArchiveStatus
archiveStatusForSlug slug = Map.findWithDefault Live slug slugStatus

-- ---------------------------------------------------------------------------
-- URL normalisation (matching, not display)
-- ---------------------------------------------------------------------------

-- | Tracking-only query parameters: their presence or absence is
--   semantically irrelevant; the lookup strips them before matching.
--   @TRACKING_PARAMS@ in @tools/archive.py@ is held to this list, and
--   'normalizeUrl' to its Python twin, by tests/test_archive.py.
trackingParams :: [Text]
trackingParams =
    [ "utm_source", "utm_medium", "utm_campaign", "utm_term", "utm_content"
    , "fbclid", "gclid", "mc_eid", "mc_cid", "ref", "igshid"
    , "_hsenc", "_hsmi", "mkt_tok"
    ]

-- | Remove tracking-only query parameters; preserve every other parameter
--   in its original order.
stripTracking :: Text -> Text
stripTracking url = case T.breakOn "?" url of
    (_, "")    -> url
    (path, q)  ->
        let kept = filter notTracking (T.splitOn "&" (T.drop 1 q))
        in  if null kept then path
            else path <> "?" <> T.intercalate "&" kept
  where
    notTracking p = T.takeWhile (/= '=') p `notElem` trackingParams

-- | The canonical form of an arXiv URL: @https://arxiv.org/abs/<id>@ with
--   no version suffix and no @.pdf@. Maps every member of the
--   abs/pdf/versioned/@.pdf@ family to the same key. Non-arXiv passes through.
arxivCanonical :: Text -> Text
arxivCanonical url
    | Just rest <- T.stripPrefix "https://arxiv.org/" url
    , Just key  <- arxivKey rest = key
    | Just rest <- T.stripPrefix "http://arxiv.org/" url
    , Just key  <- arxivKey rest = key
    | otherwise                  = url
  where
    arxivKey rest = case T.breakOn "/" rest of
        (kind, slashId)
            | kind `elem` ["abs", "pdf"], not (T.null slashId) ->
                Just $ "https://arxiv.org/abs/"
                     <> stripVer (stripPdfSuf (T.tail slashId))
        _ -> Nothing
    stripPdfSuf t = fromMaybe t (T.stripSuffix ".pdf" t)
    stripVer t = case T.breakOnEnd "v" t of
        (before, ver)
            | not (T.null before)
            , not (T.null ver)
            , T.all isAsciiDigit ver
            -> T.dropEnd 1 before
        _ -> t
    isAsciiDigit c = c >= '0' && c <= '9'

-- | The full normalisation: drop fragment, strip tracking, fold
--   @http://@→@https://@, arXiv-canonicalise, trim a trailing slash. Both
--   'flatIndex' keys and 'archiveSlugFor' inputs go through this so the
--   index never misses a citation form the design promises to match.
normalizeUrl :: Text -> Text
normalizeUrl url =
    let noFrag = T.takeWhile (/= '#') url
        clean  = stripTracking noFrag
        https  = case T.stripPrefix "http://" clean of
            Just rest -> "https://" <> rest
            Nothing   -> clean
        arxiv  = arxivCanonical https
    in  T.dropWhileEnd (== '/') arxiv
