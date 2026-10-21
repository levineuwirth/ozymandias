{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Archive section — the link-archiving system. Phases 1-2: PDF and HTML.
--
--   Authored input:        archive/manifest.yaml (one line per archived link)
--   Generated, committed:  archive/<slug>/{document.pdf | snapshot.html}
--                          + PROVENANCE.json
--   Generated, gitignored: archive/<slug>/{document,snapshot}.txt
--                          + data/archive-index.json
--
--   @tools/archive.py fetch@ runs before the Hakyll build: it downloads
--   PDFs / snapshots HTML pages with @monolith@, extracts text, and writes
--   each PROVENANCE.json. This module then routes the artifacts and renders
--   one @/archive/<slug>/@ page per entry plus the @/archive/@ index.
--
--   An entry whose artifact has not been fetched (no PROVENANCE.json, or
--   no artifact file on disk) is skipped — it produces no page, and an
--   orphaned @archive/<slug>/@ directory with no manifest line is inert
--   (no page, not deployed). Artifact-integrity (SHA-256) verification
--   runs on both sides: @archive.py fetch@ re-hashes before the Hakyll
--   build, and 'verifyArtifactSha' (below) re-hashes again in
--   'loadArchiveEntries', once per process — so the guarantee holds even when @archive.py@
--   does not run first (no @.venv@, a direct @cabal run site -- build@,
--   or a deploy host without the Python toolchain).
--
--   See @ARCHIVE.md@ at the repo root for the full design and phase plan.
module Archive (archiveRules, archiveBuildStats) where

import           Control.Exception      (SomeException, catch, evaluate)
import           Control.Monad          (filterM, forM, forM_, when)
import           Data.Function          (on)
import           Data.List              (groupBy, intercalate, sort, sortBy)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (catMaybes, fromMaybe)
import           Data.Ord               (Down (..), comparing)
import qualified Data.Set               as Set
import qualified Data.Text              as T
import           Data.Time              (diffDays, getCurrentTime, utctDay)
import qualified Data.Aeson             as A
import           Data.Aeson             ((.:), (.:?))
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Yaml              as Y
import           System.Directory       (doesDirectoryExist, doesFileExist,
                                         listDirectory)
import           System.Exit            (exitFailure)
import           System.IO              (hPutStrLn, readFile', stderr)
import           System.IO.Unsafe       (unsafePerformIO)
import           System.Process         (readProcess)
import           Hakyll
import Utils (formatBytes, inDefault, median, parseIsoDate)
import           Contexts               (siteCtx)
import           Backlinks              (referencedByField, backlinkMathField)
import           SimilarLinks           (similarLinksField)
import           FooterData             (footerDepsField)
import           ArchiveIndex           (ArchiveStatus (..), statusName,
                                         archiveStatusForSlug, normalizeUrl)

-- ---------------------------------------------------------------------------
-- Data model
-- ---------------------------------------------------------------------------

-- | One authored entry in @archive/manifest.yaml@ — only the fields this
--   module consumes. @title:@, @type:@ and @tags:@ are read by
--   @tools/archive.py@ (title and type fold into PROVENANCE.json; tags are
--   Phase 4) and need no Haskell-side binding.
data ManifestEntry = ManifestEntry
    { meUrl        :: String
    , meAliases    :: [String]        -- ^ authored equivalent URLs of the
                                      --   same work (e.g. its DOI form);
                                      --   matching metadata, not identity
    , meNote       :: Maybe String
    , mePaywalled  :: Bool
    , meVisibility :: String          -- ^ "public" (default) | "private"
    }

instance A.FromJSON ManifestEntry where
    parseJSON = A.withObject "ManifestEntry" $ \o -> do
        url        <- o .:  "url"
        aliases    <- fromMaybe []       <$> o .:? "aliases"
        note       <- o .:? "note"
        paywalled  <- fromMaybe False    <$> o .:? "paywalled"
        visibility <- fromMaybe "public" <$> o .:? "visibility"
        -- A publication/privacy field must fail closed: an unknown value
        -- (e.g. a typo'd "privte") would otherwise be treated as public
        -- and publish an artifact the author intended to keep offline.
        when (visibility `notElem` ["public", "private"]) $ fail $
            "manifest entry " ++ url
            ++ ": visibility must be \"public\" or \"private\", got "
            ++ show visibility
        return (ManifestEntry url aliases note paywalled visibility)

newtype RemovedEntry = RemovedEntry { reUrl :: String }

instance A.FromJSON RemovedEntry where
    parseJSON = A.withObject "RemovedEntry" $ \o ->
        RemovedEntry <$> o .: "url"

-- | One generated @archive/<slug>/PROVENANCE.json@ — the immutable
--   record of an archival event, written by @tools/archive.py@.
data Provenance = Provenance
    { pvUrl      :: String
    , pvSlug     :: String
    , pvTitle    :: String
    , pvType     :: String          -- ^ "pdf" | "html"
    , pvArtifact :: String          -- ^ "document.pdf" | "snapshot.html"
    , pvSha256   :: String
    , pvBytes    :: Integer
    , pvArchived :: String
    , pvQuality  :: String          -- ^ "ok" | "degraded" | "js-required"
    , pvWayback  :: Maybe String
    }

instance A.FromJSON Provenance where
    parseJSON = A.withObject "Provenance" $ \o -> Provenance
        <$> o .:  "url"
        <*> o .:  "slug"
        <*> o .:  "title"
        <*> o .:  "type"
        <*> o .:  "artifact"
        <*> o .:  "sha256"
        <*> o .:  "bytes"
        <*> o .:  "archived"
        <*> (fromMaybe "ok" <$> o .:? "snapshot-quality")
        <*> o .:? "wayback"

-- | A renderable archive entry: the authored manifest line joined with
--   its generated provenance and extracted full text. @aeTextId@ is the
--   on-disk path of the extracted-text sidecar when it exists (it is
--   gitignored, so a no-@.venv@ build may lack it).
data ArchiveEntry = ArchiveEntry
    { aeManifest :: ManifestEntry
    , aeProv     :: Provenance
    , aeFulltext :: String
    , aeTextId   :: Maybe FilePath
    , aeStatus   :: ArchiveStatus     -- ^ link-rot status of the original
    }

-- | The extracted-text sidecar name for an artifact type.
textFileFor :: Provenance -> String
textFileFor pv
    | pvType pv == "html" = "snapshot.txt"
    | otherwise           = "document.txt"

-- | True for a @visibility: private@ entry — kept in-repo as a local
--   preservation copy, but its artifact is never routed to @_site/@ and
--   its extracted text is never rendered into the page.
isPrivate :: ArchiveEntry -> Bool
isPrivate = (== "private") . meVisibility . aeManifest

-- ---------------------------------------------------------------------------
-- Rule-generation-time IO (runs inside 'preprocess')
-- ---------------------------------------------------------------------------

manifestPath, removedPath :: FilePath
manifestPath = "archive/manifest.yaml"
removedPath = "archive/removed.yaml"

-- | Read @archive/manifest.yaml@. An absent file yields an empty list
--   (the archive degrades to invisible, matching the @.venv@-gated
--   silent-skip convention). A *parse error on a present file* halts the
--   build: the file exists but is broken — degrading to invisible would
--   swallow real errors like a typo'd @visibility@ value or a malformed
--   entry, both of which are publication-relevant.
readManifest :: IO [ManifestEntry]
readManifest = do
    exists <- doesFileExist manifestPath
    if not exists
        then return []
        else do
            parsed <- Y.decodeFileEither manifestPath
            case parsed of
                -- An empty or all-comments file decodes as YAML @Null@,
                -- not as a list. That is the legitimate "drained to zero
                -- entries" state, not a broken file — treat it as the
                -- empty manifest the absent-file branch already supports.
                Right A.Null -> return []
                Right v      -> case A.fromJSON v of
                    A.Success es -> return es
                    A.Error msg  -> fatal msg
                Left e -> fatal (show e)
  where
    fatal msg = do
        hPutStrLn stderr $ "[archive] FATAL: manifest.yaml: " ++ msg
        exitFailure

readRemovedUrls :: IO (Set.Set T.Text)
readRemovedUrls = do
    exists <- doesFileExist removedPath
    if not exists
        then return Set.empty
        else do
            parsed <- Y.decodeFileEither removedPath
            case parsed of
                Right entries -> return . Set.fromList $
                    map (normalizeUrl . T.pack . reUrl) (entries :: [RemovedEntry])
                Left e -> do
                    hPutStrLn stderr $
                        "[archive] FATAL: removed.yaml: " ++ show e
                    exitFailure

validateManifestEntries :: [ManifestEntry] -> Set.Set T.Text -> IO ()
validateManifestEntries manifest removed = go Map.empty manifest
  where
    go _ [] = return ()
    go seen (entry : rest) = do
        -- The canonical URL and every authored alias must be unique
        -- across the manifest and absent from removed.yaml — the same
        -- equivalence the Python pre-scan enforces, so a direct Hakyll
        -- build fails just as closed. Within-entry duplicates (an alias
        -- normalising to its own entry's URL) are deduped, not errors.
        let url   = meUrl entry
            norms = Set.toList . Set.fromList $
                map (normalizeUrl . T.pack) (url : meAliases entry)
        forM_ norms $ \norm -> do
            when (norm `Set.member` removed) $ do
                hPutStrLn stderr $
                    "[archive] FATAL: manifest entry " ++ show url
                    ++ " matches removed.yaml (directly or via `aliases:`); "
                    ++ "refusing to publish a deliberately removed work."
                exitFailure
            case Map.lookup norm seen of
                Just prior -> do
                    hPutStrLn stderr $
                        "[archive] FATAL: manifest entries " ++ show prior
                        ++ " and " ++ show url ++ " normalise to the same "
                        ++ "archive target (directly or via `aliases:`)."
                    exitFailure
                Nothing -> return ()
        go (foldr (\n m -> Map.insert n url m) seen norms) rest

-- | Scan @archive/<slug>/PROVENANCE.json@ into a @url -> (slug, Provenance)@
--   map. The directory name is the slug; the join key is the URL.
readProvenances :: IO (Map.Map String (String, Provenance))
readProvenances = do
    exists <- doesDirectoryExist "archive"
    if not exists
        then return Map.empty
        else do
            names <- listDirectory "archive"
            entries <- forM names $ \name -> do
                let provPath = "archive/" ++ name ++ "/PROVENANCE.json"
                isFile <- doesFileExist provPath
                if not isFile
                    then return Nothing
                    else do
                        decoded <- A.eitherDecodeFileStrict' provPath
                        case decoded of
                            Right p -> return (Just (pvUrl p, (name, p)))
                            Left e  -> do
                                hPutStrLn stderr $
                                    "[archive] FATAL: " ++ provPath ++ ": " ++ show e
                                exitFailure
            return (Map.fromList (catMaybes entries))

-- | Read a file, returning "" on any error (e.g. an absent text sidecar).
readFileSafe :: FilePath -> IO String
readFileSafe path =
    catch (readFile' path) (\(_ :: SomeException) -> return "")

-- | Verify a committed artifact's SHA-256 against its recorded value.
--   The build halts with a clear message on mismatch — so the integrity
--   guarantee holds even when @tools/archive.py@ does not run first
--   (e.g. no @.venv@, or a direct @cabal run site -- build@), and a
--   tampered or corrupted artifact can never be deployed.
--
--   Shells out to @sha256sum@ (GNU coreutils — same toolchain the rest of
--   the build assumes); a missing or non-zero @sha256sum@ surfaces as an
--   exception that also halts the build.
verifyArtifactSha :: String -> FilePath -> String -> IO ()
verifyArtifactSha slug path expected = do
    out <- readProcess "sha256sum" [path] ""
    let actual = takeWhile (/= ' ') out
    when (actual /= expected) $ do
        hPutStrLn stderr $
            "[archive] FATAL: " ++ slug ++ ": " ++ path
            ++ " SHA-256 mismatch (recorded " ++ expected
            ++ ", found " ++ actual
            ++ "). The committed artifact is corrupt or was replaced; "
            ++ "halting build."
        exitFailure

-- | 'loadArchiveEntries', once per process. The rules and the @/build/@
--   page's figures read the same entries, and every read re-hashes every
--   artifact, so the page used to repeat on each build what the rules had
--   just done. Fixed at first use, like the rest of the archive state (see
--   'archiveRules').
{-# NOINLINE archiveEntries #-}
archiveEntries :: [ArchiveEntry]
archiveEntries = unsafePerformIO loadArchiveEntries

-- | Join the authored manifest with generated provenance. A manifest
--   entry with no matching provenance — or whose artifact is not on disk
--   — is dropped, so it produces no page.
loadArchiveEntries :: IO [ArchiveEntry]
loadArchiveEntries = do
    manifest  <- readManifest
    removed   <- readRemovedUrls
    validateManifestEntries manifest removed
    provByUrl <- readProvenances
    -- Join on normalised URLs, like every other URL comparison in the
    -- archive system: editing a manifest URL to a normalisation-
    -- equivalent form (http->https, trailing slash, tracking params)
    -- must keep matching its provenance — an exact-string join would
    -- silently unpublish the page while ArchiveIndex's normalised
    -- filter keeps links pointing at it. Key collisions can't occur:
    -- validateManifestEntries rejects normalised duplicates.
    let normKey    = T.unpack . normalizeUrl . T.pack
        provByNorm = Map.mapKeys normKey provByUrl
    fmap catMaybes $ forM manifest $ \me ->
        case Map.lookup (normKey (meUrl me)) provByNorm of
            Nothing         -> return Nothing
            Just (slug, pv) -> do
                let dir     = "archive/" ++ slug
                    txtPath = dir ++ "/" ++ textFileFor pv
                let artPath = dir ++ "/" ++ pvArtifact pv
                artifactThere <- doesFileExist artPath
                if not artifactThere
                    then do
                        hPutStrLn stderr $
                            "[archive] FATAL: " ++ slug ++ ": " ++ artPath
                            ++ " is missing although PROVENANCE.json exists; "
                            ++ "restore the committed artifact before building."
                        exitFailure
                    else do
                        verifyArtifactSha slug artPath (pvSha256 pv)
                        txtThere <- doesFileExist txtPath
                        txt <- if txtThere then readFileSafe txtPath
                                           else return ""
                        return $ Just ArchiveEntry
                            { aeManifest = me
                            , aeProv     = pv
                            , aeFulltext = txt
                            , aeTextId   = if txtThere then Just txtPath
                                                       else Nothing
                            , aeStatus   = archiveStatusForSlug slug
                            }

-- ---------------------------------------------------------------------------
-- Rules
-- ---------------------------------------------------------------------------

-- | All archive rules. Called once from 'Site.rules'.
--
--   The manifest is read here in 'preprocess' (and 'ArchiveIndex' reads
--   its sidecars in once-per-process CAFs), so archive state is fixed at
--   rule-generation time: under @site watch@, edits to @manifest.yaml@,
--   @removed.yaml@, or the regenerated state JSONs are not picked up
--   until the process restarts. One-shot builds are unaffected.
archiveRules :: Rules ()
archiveRules = do
    entries <- preprocess (evaluate archiveEntries)

    -- Raw artifacts: the PDF / HTML snapshot of every *public* entry,
    -- served at its own path (/archive/<slug>/...). Routing this explicit
    -- list rather than a glob means a `visibility: private` entry's
    -- artifact is never deployed, and an orphan directory's artifact
    -- (no manifest line) is not deployed either.
    let publicArtifacts =
            [ fromFilePath ("archive/" ++ pvSlug (aeProv e)
                                       ++ "/" ++ pvArtifact (aeProv e))
            | e <- entries, not (isPrivate e) ]
    match (fromList publicArtifacts) $ do
        route   idRoute
        compile copyFileCompiler

    -- Provenance, extracted text, and the manifest: matched (not routed)
    -- so the generated pages can `load` them as dependencies and recompile
    -- when they change.
    match "archive/*/PROVENANCE.json" $ compile getResourceBody
    match "archive/*/document.txt"    $ compile getResourceBody
    match "archive/*/snapshot.txt"    $ compile getResourceBody
    match "archive/manifest.yaml"     $ compile getResourceBody

    mapM_ archiveEntryRule entries
    archiveIndexRule entries
    archiveMetaRule entries

-- | @data/archive-meta.json@ — routed page path -> link-rot status, the
--   client-side manifest behind the search page's archive filter (same
--   pattern as @data/epistemic-meta.json@). Keys use the routed
--   @.../index.html@ form to match @search-filters.js@'s @normUrl@.
--   Named @archive-meta@, and its field @status@ scoped under it, so the
--   epistemic @status@ filter namespace is untouched.
archiveMetaRule :: [ArchiveEntry] -> Rules ()
archiveMetaRule entries =
    create ["data/archive-meta.json"] $ do
        route idRoute
        compile $ do
            _ <- loadAll "archive/*/PROVENANCE.json" :: Compiler [Item String]
            let metaMap = Map.fromList
                    [ ( "/archive/" ++ pvSlug (aeProv e) ++ "/index.html"
                      , Map.singleton ("status" :: String)
                                      (statusName (aeStatus e)) )
                    | e <- entries ]
            makeItem (LBS.unpack (A.encode metaMap))

-- | One @/archive/<slug>/@ page.
archiveEntryRule :: ArchiveEntry -> Rules ()
archiveEntryRule ae =
    create [fromFilePath ("archive/" ++ slug ++ "/index.html")] $ do
        route idRoute
        compile $ do
            -- Dependency edges: recompile when provenance or the manifest
            -- changes. The extracted-text sidecar is gitignored and may be
            -- absent (no .venv / fetch never ran); load it as a dependency
            -- only when present, so the build never fails for a missing
            -- generated file.
            _ <- load provId     :: Compiler (Item String)
            _ <- load manifestId :: Compiler (Item String)
            case aeTextId ae of
                Just tp -> do
                    _ <- load (fromFilePath tp) :: Compiler (Item String)
                    return ()
                Nothing -> return ()
            makeItem ""
                >>= inDefault "templates/archive.html" ctx
  where
    slug       = pvSlug (aeProv ae)
    provId     = fromFilePath ("archive/" ++ slug ++ "/PROVENANCE.json")
    manifestId = fromFilePath manifestPath
    ctx        = archiveEntryCtx ae

-- | The @/archive/@ index — every archived work, newest snapshot first.
archiveIndexRule :: [ArchiveEntry] -> Rules ()
archiveIndexRule entries =
    create ["archive/index.html"] $ do
        route idRoute
        compile $ do
            -- Recompile when any provenance appears / changes, or the
            -- manifest changes.
            _ <- loadAll "archive/*/PROVENANCE.json" :: Compiler [Item String]
            _ <- load (fromFilePath manifestPath)    :: Compiler (Item String)
            let sorted = sortBy (comparing (Down . pvArchived . aeProv)) entries
                items  = map (\e -> Item (fromFilePath ("archive/" ++ pvSlug (aeProv e))) e)
                             sorted
                ctx    = listField "entries" entryListCtx (return items)
                      <> constField "title"   "Archive"
                      <> constField "archive" "true"
                      <> constField "noindex" "true"
                      <> constField "description"
                            ("Local preservation snapshots of works cited "
                             ++ "here, kept against link rot.")
                      <> (if null entries then mempty
                                          else constField "has-entries" "true")
                      <> siteCtx
            makeItem ""
                >>= inDefault "templates/archive-index.html" ctx

-- ---------------------------------------------------------------------------
-- Contexts
-- ---------------------------------------------------------------------------

-- | Per-entry context for the @/archive/<slug>/@ page.
archiveEntryCtx :: ArchiveEntry -> Context String
archiveEntryCtx ae = mconcat
    [ constField "title"            (pvTitle pv)
    , constField "archive"          "true"
    , constField "noindex"          "true"
    -- C01: without this the page falls through to a body excerpt, which
    -- on an archive wrapper is the banner label ("Archived copy").
    , constField "description"
        ("Local preservation snapshot of " ++ meUrl me
         ++ ", taken " ++ pvArchived pv ++ ".")
    , constField "original-url"     (meUrl me)
    , constField "archived"         (pvArchived pv)
    , constField "archive-type"     (pvType pv)
    , constField "sha-short"        (take 12 (pvSha256 pv))
    , constField "size"             (formatBytes (pvBytes pv))
    , constField "snapshot-quality" (pvQuality pv)
    , constField "status"           (statusName (aeStatus ae))
    , qualityFlag
    , maybeField "status-note" (statusNote (aeStatus ae))
    , maybeField "note"      (meNote me)
    , maybeField "wayback"   (pvWayback pv)
    , maybeField "paywalled" (if mePaywalled me then Just "true" else Nothing)
    , visibilityFields
    -- "Referenced by" (the pages that cite this work) and "Related"
    -- (semantically near content). Both resolve by this page's route, so
    -- they need no archive-specific wiring; each is a $if(...)$-guarded
    -- section in archive.html.
    , referencedByField
    -- A citing sentence can carry math. The KaTeX stylesheet and bootstrap
    -- are behind $if(math)$, which this page gets from nowhere else, so
    -- the field sets it only when some referenced-by context needs it.
    , backlinkMathField
    , similarLinksField
    , footerDepsField
    , siteCtx
    ]
  where
    me     = aeManifest ae
    pv     = aeProv ae
    slug   = pvSlug pv
    artUrl = "/archive/" ++ slug ++ "/" ++ pvArtifact pv
    -- A non-'ok' snapshot raises a visible flag on the page.
    qualityFlag
        | pvQuality pv == "ok" = mempty
        | otherwise            = constField "degraded" "true"
    -- A private entry keeps a local preservation copy but publishes none
    -- of it: no embed, no extracted text — only the provenance metadata
    -- and a 'held offline' note. A public entry embeds the artifact raw
    -- (the browser renders the PDF natively, the snapshot loads directly;
    -- no PDF.js wrapper) and renders its extracted text into the page.
    -- The is-pdf / is-html flag drives only the iframe sandbox: a
    -- third-party HTML snapshot is sandboxed, our own committed PDF is not.
    visibilityFields
        | isPrivate ae = constField "private" "true"
        | otherwise    = typeField
                      <> constField "artifact-url"  artUrl
                      <> constField "artifact-name" (pvArtifact pv)
                      <> fulltextField (pvType pv) (aeFulltext ae)
    typeField
        | pvType pv == "html" = constField "is-html" "true"
        | otherwise           = constField "is-pdf"  "true"

-- | Renders the extracted full text into the page DOM so embed.py and
--   Pagefind index real text, not an opaque iframe. PDF text keeps its
--   pdftotext layout in a @<pre>@; HTML text is block-separated prose, so
--   it renders as escaped @<p>@ paragraphs. Absent when the text is empty
--   / whitespace, so the @$if(fulltext)$@ guard hides the section.
fulltextField :: String -> String -> Context String
fulltextField ftype txt
    | all isBlank txt = mempty
    | ftype == "html" = constField "fulltext" (htmlParagraphs txt)
    | otherwise       = constField "fulltext" preBlock
  where
    isBlank c = c == ' ' || c == '\n' || c == '\t' || c == '\r'
    preBlock  = "<pre class=\"archive-fulltext\">"
             ++ escapeHtml txt ++ "</pre>"

-- | Block-separated text (paragraphs delimited by blank lines, as
--   @archive.py@'s HTML extractor writes it) → escaped @<p>@ elements.
htmlParagraphs :: String -> String
htmlParagraphs = concatMap para . paragraphsOf
  where
    para p       = "<p>" ++ escapeHtml p ++ "</p>\n"
    paragraphsOf = map (unwords . concatMap words)
                 . filter (not . blankGroup)
                 . groupBy ((==) `on` blankLine)
                 . lines
    blankGroup g = null g || blankLine (head g)
    blankLine    = all (`elem` (" \t\r" :: String))

-- | List-item context for the @/archive/@ index.
entryListCtx :: Context ArchiveEntry
entryListCtx = mconcat
    [ field "entry-title"    (return . pvTitle    . aeProv . itemBody)
    , field "entry-archived" (return . pvArchived . aeProv . itemBody)
    , field "entry-type"     (return . pvType     . aeProv . itemBody)
    , field "entry-quality"  (return . pvQuality  . aeProv . itemBody)
    , boolField "entry-degraded" ((/= "ok") . pvQuality . aeProv . itemBody)
    , boolField "entry-private"  (isPrivate . itemBody)
    , field "entry-status"   (return . statusName . aeStatus . itemBody)
    , boolField "entry-rotted"   ((== Rotted) . aeStatus . itemBody)
    , field "entry-url"      (\i -> return $
        "/archive/" ++ pvSlug (aeProv (itemBody i)) ++ "/")
    ]

-- | Provide a field only when the value is present; otherwise contribute
--   nothing, so the template's @$if(...)$@ guard is false.
maybeField :: String -> Maybe String -> Context String
maybeField k = maybe mempty (constField k)

-- | A prose note for a non-live link-rot status, shown on the archive
--   page; 'Nothing' for 'Live' / 'Error' (no note rendered).
statusNote :: ArchiveStatus -> Maybe String
statusNote Rotted = Just "The original is no longer reachable. This archived \
                         \copy is now the live link."
statusNote Moved  = Just "The original page has moved since this snapshot was \
                         \taken; the link above may redirect."
statusNote _      = Nothing

-- ---------------------------------------------------------------------------
-- Formatting
-- ---------------------------------------------------------------------------


-- ---------------------------------------------------------------------------
-- /build/ telemetry
-- ---------------------------------------------------------------------------

-- | Archive metrics for the @/build/@ telemetry page — count, total size,
--   median artifact age, breakdowns by link-rot status / snapshot quality
--   / visibility, the paywalled count, and any orphan directories.
--   Rendered by @Stats.hs@; an empty archive yields just the count.
archiveBuildStats :: IO [(String, String)]
archiveBuildStats = do
    entries <- evaluate archiveEntries
    today   <- utctDay <$> getCurrentTime
    orphans <- findOrphanDirs entries
    let n         = length entries
        bytes     = sum (map (pvBytes . aeProv) entries)
        ages      = [ fromInteger (diffDays today d)
                    | e <- entries
                    , Just d <- [parseIsoDate (pvArchived (aeProv e))] ]
        paywalled = length (filter (mePaywalled . aeManifest) entries)
    return $
        [ ("Entries", show n) ]
        ++ (if n == 0 then [] else
            [ ("Total size",    formatBytes bytes)
            , ("Median age",    medianAge ages)
            , ("By status",     tallyOf (map (statusName . aeStatus) entries))
            , ("By quality",    tallyOf (map (pvQuality . aeProv) entries))
            , ("By visibility", tallyOf (map (meVisibility . aeManifest) entries))
            ])
        ++ [ ("Paywalled", show paywalled) | paywalled > 0 ]
        ++ [ ("Orphan directories", unwords orphans) | not (null orphans) ]

-- | Directory names under @archive/@ that hold a @PROVENANCE.json@ but are
--   not a live manifest entry — drift the @/build/@ page should surface.
findOrphanDirs :: [ArchiveEntry] -> IO [String]
findOrphanDirs entries = do
    exists <- doesDirectoryExist "archive"
    if not exists
        then return []
        else do
            names <- listDirectory "archive"
            let live = map (pvSlug . aeProv) entries
            filterM
                (\name -> do
                    hasProv <- doesFileExist
                                   ("archive/" ++ name ++ "/PROVENANCE.json")
                    return (hasProv && name `notElem` live))
                (sort names)

-- | Format a multiset of string values as @"a 2  \183  b 1"@.
tallyOf :: [String] -> String
tallyOf xs = intercalate "  \183  "
    [ k ++ " " ++ show c
    | (k, c) <- Map.toList (Map.fromListWith (+) [ (x, 1 :: Int) | x <- xs ]) ]

-- | The median of a list of ages, as @"N days"@; an em dash when empty.
--   An even-length list takes the mean of the two middle elements,
--   rounded to the nearest whole day.
medianAge :: [Int] -> String
medianAge [] = "\8212"
medianAge xs =
    let m = median xs
    in  show m ++ if m == 1 then " day" else " days"

