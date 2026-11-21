{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Whether a file beside an image is that image's: its WebP companion
--   (@lake.webp@ of @lake.jpg@) or, for a photograph, one of its variants
--   (@view.w480.jpg@, @view.w480.webp@ of @view.jpg@).
--
--   The name does not say. @lake.webp@ may be a picture its author made
--   and shows as an image of its own, and a page that showed @lake.jpg@
--   with it would show another picture than the one it names. So a file
--   is taken for an image's on one of two grounds, and otherwise not at
--   all (the image is then shown alone):
--
--   * the ownership record says a tool made it from that image, its
--     bytes are still the ones the tool wrote, and the image is still the
--     one it was made from: an image replaced since (and not converted
--     again: a build without cwebp) is shown alone, not with the pixels
--     of the one before
--     (@data\/derived-images.json@, tools/derived_images.py: written by
--     tools/convert-images.sh and tools/generate-thumbnails.py);
--   * the author declared it the image's companion
--     (@data\/companions.json@: @{"content\/blog\/moving.webp":
--     "content\/blog\/moving.gif"}@). A declared file is that image's
--     and no other's, whatever the record says of it.
--
--   Each tree has its record and its declarations under its own root:
--   the site's, and the engine's (@engine\/data\/…@, for the images under
--   @engine\/static\/@), with paths relative to that root. A declaration
--   is read as the file it leads to (@.\/static\/lake.webp@ is
--   @static\/lake.webp@, and so is a path through a directory that is a
--   link to @static\/@), as the tools read it: a file is known by where
--   its directory really is ('located'), whoever asks and however. A
--   site's declaration of an engine file counts for nothing (the other
--   sites that link the engine would not know of it), and the tools stop
--   on one.
--
--   Everything that would use such a file asks here: "Filters.Images"
--   (a body image's @\<picture>@), "Contexts" and "Photography" (a
--   photograph's srcsets, its pin on the map) and "Drafts" (which of the
--   blog's images a published page shows).
--
--   A page is built with the answers of its day. So that it is built
--   again when one changes (a companion edited by hand is no longer the
--   image's; one adopted or declared now is), each build first writes
--   what holds, a file for each directory of outputs
--   (@data\/pairings\/states\/content\/blog.json@: the outputs there that
--   are their images', and the images), and writes one only when it
--   differs from what is there. A page depends on the files of the
--   directories it asked about ('pairedWithC', 'tellPairings'), which a
--   rule claims, and Hakyll rebuilds it when one of them changes, appears
--   or goes. A watch writes them again as it runs ('watchPairingStates').
--
--   @data\/pairings\/manifest.json@ lists the state files written, each
--   with the digest of what was written, and only a file that is still
--   those bytes is ever written over or deleted. Anything else where a
--   state file goes, a link on the way there, or a manifest that is not
--   one, stops the build ('refreshPairingStates'): a page whose
--   dependencies cannot be kept would go on showing what no longer holds.
module DerivedImages
    ( pairedWith
    , pairedFor
    , asksAbout
    , pairedWithC
    , tellPairings
    , refreshPairingStates
    , watchPairingStates
    , pairingStates
    ) where

import           Control.Concurrent    (ThreadId, threadDelay)
import           Control.Exception     (Exception, IOException, SomeException, catch, onException, throwIO,
                                        throwTo, try)
import           Control.Monad         (filterM, forM, forM_, unless, when)
import           Crypto.Hash           (Digest, SHA256, hash)
import qualified Data.Aeson            as A
import qualified Data.Aeson.Key        as K
import qualified Data.Aeson.KeyMap     as KM
import qualified Data.ByteString       as BS
import           Data.Int              (Int64)
import           Data.IORef            (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import           Data.List             (inits, intercalate, isPrefixOf, isSuffixOf, nub, stripPrefix)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (catMaybes)
import qualified Data.Scientific       as Sci
import qualified Data.Text             as T
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Set              as Set
import qualified Data.Vector           as V
import           Hakyll                (Compiler, Pattern, fromFilePath, fromGlob, fromList, getMatches,
                                        unsafeCompiler, (.||.))
import           Hakyll.Core.Compiler.Internal (compilerTellDependencies)
import           Hakyll.Core.Dependencies (Dependency (..))
import           System.Directory      (canonicalizePath, createDirectoryIfMissing, doesFileExist,
                                        getCurrentDirectory, getFileSize, getModificationTime,
                                        pathIsSymbolicLink, removeFile, renameFile)
import           System.Exit           (ExitCode (..), die)
import           System.FilePath       (isAbsolute, joinPath, makeRelative, splitDirectories, takeDirectory,
                                        takeFileName, (</>))
import           System.IO             (hClose, hPutStrLn, openBinaryTempFileWithDefaultPermissions, stderr)
import           System.IO.Unsafe      (unsafePerformIO)

-- | What a record says of one output: the image it was made from, the
--   bytes last written, and which image that was: its bytes, and its
--   time (in nanoseconds) and size then, which spare reading it.
data Made = Made
    { madeFrom         :: FilePath
    , madeDigest       :: String
    , madeSourceDigest :: Maybe String
    , madeSourceStamp  :: Maybe (Integer, Integer)
    }

-- | The records and declarations of both trees, with every path as the
--   build has it (an engine file's under @engine\/@).
data Pairings = Pairings
    { made     :: Map.Map FilePath Made
    , declared :: Map.Map FilePath FilePath   -- ^ a WebP, and the image it is for
    }

siteRecord, engineRecord, siteDeclared, engineDeclared, engineDir, stateRoot, stateDir, manifest, pending
    :: FilePath
siteRecord     = "data/derived-images.json"
engineRecord   = engineDir </> siteRecord
siteDeclared   = "data/companions.json"
engineDeclared = engineDir </> siteDeclared
engineDir      = "engine"
stateRoot      = "data/pairings"
stateDir       = stateRoot </> "states"
manifest       = stateRoot </> "manifest.json"
pending        = stateRoot </> "manifest.pending.json"

-- | Is @output@ the file of @image@ that its name says it is? Both are
--   paths in the site as the build has them (@content\/blog\/lake.jpg@,
--   @engine\/static\/images\/mark.png@), or absolute paths under it.
pairedWith :: FilePath -> FilePath -> IO Bool
pairedWith image output = do
    pairings <- currentPairings
    image'   <- asRecorded image
    output'  <- asRecorded output
    holds pairings image' output'

-- | Does the pairing hold now: declared, or recorded with the bytes that
--   are there, of the image that is there?
holds :: Pairings -> FilePath -> FilePath -> IO Bool
holds pairings image output = do
    there <- doesFileExist output
    case Map.lookup output (declared pairings) of
        _ | not there -> pure False
        -- The author's word for the file: this image's, and so no other's,
        -- whatever a tool once recorded of it.
        Just for      -> pure (for == image)
        Nothing       -> case Map.lookup output (made pairings) of
            Just m | madeFrom m == image -> do
                link <- pathIsSymbolicLink output
                same <- if link then pure False else (== Just (madeDigest m)) <$> digestOf output
                if same then madeOf m image else pure False
            _ -> pure False

-- | Is the image the one the output was made from? By its time and size,
--   or else by its bytes (a checkout changes the time alone). An entry
--   that says neither says nothing of the image, and pairs nothing.
madeOf :: Made -> FilePath -> IO Bool
madeOf m image = do
    now <- fmap inNanoseconds <$> stampOf image
    case now of
        Nothing                           -> pure False
        _ | now == madeSourceStamp m      -> pure True
        _ -> case madeSourceDigest m of
            Just d  -> (== Just d) <$> digestOf image
            Nothing -> pure False
  where
    inNanoseconds (time, size) = (floor (utcTimeToPOSIXSeconds time * 1000000000), size)

-- | 'pairedWith', and a note that whoever compiles under @key@ asked
--   about this output: "Filters.Images" runs in IO and cannot tell Hakyll
--   itself, so the page's compiler does when the filter has run
--   ('tellPairings', with the same key: its source directory).
pairedFor :: FilePath -> FilePath -> FilePath -> IO Bool
pairedFor key image output = asksAbout key output >> pairedWith image output

-- | The note alone: whoever compiles under @key@ is to be built again
--   when what is paired where @output@ is, or would be, changes. For a
--   file that is not there yet, too: its first appearance is such a
--   change.
asksAbout :: FilePath -> FilePath -> IO ()
asksAbout key output = do
    output' <- asRecorded output
    atomicModifyIORef' askedAbout
        (\m -> (Map.insertWith Set.union key (Set.singleton (stateFileOf output')) m, ()))

-- | Tell Hakyll what the pages compiled under @key@ asked about: the
--   state files of those directories. Pages that share a directory share
--   the note, and so depend on a little more than their own.
tellPairings :: FilePath -> Compiler ()
tellPairings key = do
    asked <- unsafeCompiler (Map.findWithDefault Set.empty key <$> readIORef askedAbout)
    dependOn (Set.toList asked)

-- | 'pairedWith' in a compiler, which is then rebuilt when the answer
--   may have changed.
pairedWithC :: FilePath -> FilePath -> Compiler Bool
pairedWithC image output = do
    output' <- unsafeCompiler (asRecorded output)
    dependOn [stateFileOf output']
    unsafeCompiler (pairedWith image output)

-- | A dependency on state files, there or not: one that appears or goes
--   changes the set the pattern matches, which puts the page out of date
--   as a change of content does.
dependOn :: [FilePath] -> Compiler ()
dependOn files = unless (null files) $ do
    let pattern = fromList (map fromFilePath files)
    present <- getMatches pattern
    compilerTellDependencies [PatternDependency pattern (Set.fromList present)]

-- | The state files and their manifest, for the rule that claims them: a
--   dependency on a file no rule claims never fires, and a watch wakes
--   for a claimed file only.
pairingStates :: Pattern
pairingStates = fromGlob (stateDir ++ "/**.json") .||. fromList [fromFilePath manifest]

-- | The state file of an output's directory.
stateFileOf :: FilePath -> FilePath
stateFileOf output = stateDir </> takeDirectory output ++ ".json"

-- | Write what holds now, a file for each directory of outputs, touching
--   none that already says it. Run before Hakyll reads the site, in every
--   build: what a cached page was built with is compared with this.
--
--   The manifest names each state file with the digest of what it says,
--   and is written last, whenever a state file was: it is in a directory
--   that was there when a watch began, so its change is seen where a
--   state file's in a directory made a moment ago may not be.
--
--   When the states cannot be kept (see 'writeStates'), the build stops
--   here, saying which file is in the way: built on, the pages would
--   keep whatever pairing they were last built with.
refreshPairingStates :: IO ()
refreshPairingStates = refreshed >>= either die pure

-- | Why the states could not be kept.
newtype Trouble = Trouble String deriving Show
instance Exception Trouble

trouble :: String -> IO a
trouble = throwIO . Trouble

refreshed :: IO (Either String ())
refreshed = (Right <$> refresh)
    `catch` (\(Trouble why) -> pure (Left why))
    `catch` (\err -> pure (Left (stateRoot ++ ": the pairing states could not be written ("
                                 ++ show (err :: IOException) ++ ")")))
  where
    refresh = do
        -- Ask again where the directories are, and so which files the
        -- declarations lead to: a watch outlives a link moved to another
        -- place. (The records themselves are read again when their files
        -- change: 'currentPairings'.)
        atomicModifyIORef' pairingsCache (const (Nothing, ()))
        atomicModifyIORef' realDirs (const (Map.empty, ()))
        pairings <- currentPairings
        let claimed = [ (output, madeFrom m) | (output, m) <- Map.toList (made pairings)
                                             , output `Map.notMember` declared pairings ]
                      ++ Map.toList (declared pairings)
        paired <- filterM (\(output, image) -> holds pairings image output) claimed
        let states = Map.map (BS.toStrict . A.encode . Map.toAscList) $ Map.fromListWith Map.union
                [ (stateFileOf output, Map.singleton output image) | (output, image) <- paired ]
        -- A watch asks every little while, and mostly nothing has changed.
        last' <- readIORef lastStates
        unless (last' == Just states) $ do
            writeStates states
            writeIORef lastStates (Just states)

-- | Put the state files as @states@ has them, and the manifest after.
--
--   Only the build's own files are touched. A file is its own while its
--   bytes are those the manifest names for it; one that is there and is
--   not (the author's, or a state file edited since) is in the way, and
--   is left as it is: the build stops. So does it for a link at
--   @data\/@, @data\/pairings\/@ or below, through which nothing is
--   written, and for a manifest that cannot be read as one.
--
--   The manifest is written after the files, so a build killed between
--   the two would leave files the manifest does not know for its own.
--   What is about to be written is therefore noted first
--   (@manifest.pending.json@: each file with the digests it may be found
--   with until the manifest says), and the note removed when it has.
writeStates :: Map.Map FilePath BS.ByteString -> IO ()
writeStates states = do
    free <- linkFree stateDir
    unless free $ trouble
        (stateRoot ++ ": it, or a directory on the way to it or just under it, is a link. The build keeps there what "
         ++ "each page was built with, and writes nothing through a link; without it a page would go on "
         ++ "showing a pairing that no longer holds. Make " ++ stateRoot ++ " a directory of the site's own.")
    was  <- readOwn manifest (\v -> case v of A.String d -> Just [T.unpack d]
                                              _          -> Nothing)
    pend <- readOwn pending (\v -> case v of A.Array ds -> Just [ T.unpack d | A.String d <- V.toList ds ]
                                             _          -> Nothing)
    let known file = Map.findWithDefault [] file was ++ Map.findWithDefault [] file pend
    plans <- forM (Set.toList (Map.keysSet states <> Map.keysSet was <> Map.keysSet pend)) $ \file -> do
        place <- ownPlace file
        unless place $ trouble (file ++ ": no place for a state file of the build's (a link is in the way).")
        there   <- doesFileExist file
        current <- if there then Just <$> BS.readFile file else pure Nothing
        unless (maybe True ((`elem` known file) . digestOfBytes) current) $ trouble (inTheWay file)
        pure (file, current, Map.lookup file states)
    let changes = [ plan | plan@(_, current, wanted) <- plans, current /= wanted ]
        now     = Map.map (pure . digestOfBytes) states
    unless (null changes && now == was && Map.null pend) $ do
        createDirectoryIfMissing True stateRoot
        writeWhole pending $ BS.toStrict $ A.encode $ Map.fromList
            [ (file, nub (map digestOfBytes (catMaybes [current, wanted]))) | (file, current, wanted) <- plans ]
        forM_ changes $ \(file, _, wanted) -> case wanted of
            Just text -> createDirectoryIfMissing True (takeDirectory file) >> writeWhole file text
            -- A directory with nothing paired any more has no file.
            Nothing   -> removeFile file
        when (now /= was) $ writeWhole manifest (BS.toStrict (A.encode (Map.map concat now)))
        removeFile pending
  where
    inTheWay file = file ++ ": not a file this build wrote, or changed since it did. The build keeps there what "
        ++ "the pages of a directory were built with, and writes over and deletes only its own files. Move it "
        ++ "away (or delete " ++ stateRoot ++ "/, which the next build writes again), and build again."
    -- The manifest or the note beside it: the build's by its name, and
    -- read as that or not at all.
    readOwn file digests = do
        link  <- not <$> notALink file
        there <- doesFileExist file
        when link $ trouble (file ++ ": a link, and the build writes nothing through one. Move it away.")
        if not there then pure Map.empty else do
            bytes <- BS.readFile file
            case A.decodeStrict bytes of
                Just (A.Object top) | Just entries <- mapM (\(k, v) -> (,) (K.toString k) <$> digests v)
                                                           (KM.toList top)
                                    , all (stateName . fst) entries
                    -> pure (Map.fromList entries)
                _   -> trouble (file ++ ": not the build's list of its state files, which goes there. Move it "
                                ++ "away (or delete " ++ stateRoot ++ "/, which the next build writes again).")

-- | In a watch: write the states again as the process lives. Hakyll's
--   watch wakes for a changed file a rule claims, which the state files
--   and the manifest are, and builds again the pages that depend on them.
--   Without this a watch would go on showing a pairing withdrawn since it
--   began (a declaration removed, a companion edited, an image replaced).
--
--   Every two seconds when a record or a declaration file has changed,
--   which is what a tool or the author's line does; every ten otherwise,
--   for the files themselves (an image saved again, a companion edited by
--   hand): looking at each recorded file is the work here, some thousands
--   of them on a site with photographs, and is not done five times as
--   often for an idle watch.
--
--   When the states can no longer be kept the watch ends, as a build
--   would not begin: @main@ is the thread to end it.
watchPairingStates :: ThreadId -> IO ()
watchPairingStates main = go (1 :: Int) Nothing
  where
    go n was = do
        threadDelay 2000000
        stamp <- recordStamps
        done  <- if n == 0 || Just stamp /= was
                    then refreshed `catch` (\err -> pure (Left ("pairing states: " ++ show (err :: SomeException))))
                    else pure (Right ())
        case done of
            Right () -> go ((n + 1) `mod` 5) (Just stamp)
            Left why -> hPutStrLn stderr why >> throwTo main (ExitFailure 1)

-- | May a state file be written or deleted at this path? It is under the
--   state directory by its name, a @.json@, and no link stands on the way
--   to it or in the file's own place: what a link leads to is not this
--   module's to write.
ownPlace :: FilePath -> IO Bool
ownPlace file = if stateName file then linkFree file else pure False

-- | Is this the name of a state file: under the state directory, a
--   @.json@, and written plainly?
stateName :: FilePath -> Bool
stateName file = (stateDir ++ "/") `isPrefixOf` file && ".json" `isSuffixOf` file
    && not (any (`elem` [".", ".."]) parts) && joinPath parts == file
  where parts = splitDirectories file

-- | Is no part of the path, from the site's root down, a link? (Parts
--   that are not there yet are no links.)
linkFree :: FilePath -> IO Bool
linkFree path = and <$> mapM notALink [ joinPath prefix | prefix <- drop 1 (inits (splitDirectories path)) ]

notALink :: FilePath -> IO Bool
notALink path = either (const True) not <$> (try (pathIsSymbolicLink path) :: IO (Either IOException Bool))

-- | The whole file or none of it, as a reader (a build under way) sees
--   it. Written beside it under a name of its own, new each time, so no
--   file that was there is written over; the name is not one a rule
--   claims or a build publishes.
writeWhole :: FilePath -> BS.ByteString -> IO ()
writeWhole file bytes = do
    (tmp, handle) <- openBinaryTempFileWithDefaultPermissions (takeDirectory file) ('.' : takeFileName file ++ ".tmp")
    (BS.hPut handle bytes >> hClose handle >> renameFile tmp file)
        `onException` (hClose handle >> (try (removeFile tmp) :: IO (Either IOException ())))

digestOfBytes :: BS.ByteString -> String
digestOfBytes bytes = "sha256:" ++ show (hash bytes :: Digest SHA256)

{-# NOINLINE lastStates #-}
lastStates :: IORef (Maybe (Map.Map FilePath BS.ByteString))
lastStates = unsafePerformIO (newIORef Nothing)

{-# NOINLINE askedAbout #-}
askedAbout :: IORef (Map.Map FilePath (Set.Set FilePath))
askedAbout = unsafePerformIO (newIORef Map.empty)

-- | Which tree a file is in, and its path there, as the tools have it
--   (tools/derived_images.py, @Tree.rel@): by where its directory really
--   is, so that a file reached through a link to its directory, or
--   through the site's engine link, is the same file to both. The
--   engine's first: inside a site its files are under the site's root
--   too, and they are the engine's. 'Nothing' for a file of neither.
data Tree = Site | Engine deriving Eq

located :: FilePath -> IO (Maybe (Tree, FilePath))
located path = do
    here <- realDir "."
    eng  <- realDir engineDir
    dir  <- realDir (takeDirectory path)
    let name = takeFileName path
        under root | dir == root = Just name
                   | otherwise   = (</> name) <$> stripPrefix (root ++ "/") dir
    pure $ if name `elem` ["", ".", ".."] then Nothing else case (under eng, under here) of
        (Just rel, _) -> Just (Engine, rel)
        (_, Just rel) -> Just (Site, rel)
        _             -> Nothing

-- | A file as the records have it here: its path in the site, an engine
--   file's under @engine\/@. (One of neither tree is in no record; it
--   keeps its path, without its @.@ and @..@.)
asRecorded :: FilePath -> IO FilePath
asRecorded path = do
    found <- located path
    case found of
        Just (Engine, rel) -> pure (engineDir </> rel)
        Just (Site, rel)   -> pure rel
        Nothing            -> (`inSite` path) <$> getCurrentDirectory

-- | Where a directory really is. Asked for every image of every page, and
--   so kept, until the states are next written.
realDir :: FilePath -> IO FilePath
realDir dir = do
    known <- Map.lookup dir <$> readIORef realDirs
    case known of
        Just real -> pure real
        Nothing   -> do
            real <- canonicalizePath dir
            atomicModifyIORef' realDirs (\m -> (Map.insert dir real m, ()))
            pure real

{-# NOINLINE realDirs #-}
realDirs :: IORef (Map.Map FilePath FilePath)
realDirs = unsafePerformIO (newIORef Map.empty)

inSite :: FilePath -> FilePath -> FilePath
inSite here path = intercalate "/" (foldl step [] (splitDirectories relative))
  where
    relative = if isAbsolute path then makeRelative here path else path
    step acc "."  = acc
    step acc ".." = if null acc then [] else init acc
    step acc part = acc ++ [part]

-- ---------------------------------------------------------------------------
-- The records, read again when one of their files has changed (a watch
-- outlives a conversion), and the digests of the files asked about.

type Stamp = [Maybe (UTCTime, Integer)]

{-# NOINLINE pairingsCache #-}
pairingsCache :: IORef (Maybe (Stamp, Pairings))
pairingsCache = unsafePerformIO (newIORef Nothing)

{-# NOINLINE digestCache #-}
digestCache :: IORef (Map.Map FilePath ((UTCTime, Integer), String))
digestCache = unsafePerformIO (newIORef Map.empty)

stampOf :: FilePath -> IO (Maybe (UTCTime, Integer))
stampOf path = either (const Nothing) Just <$>
    (try ((,) <$> getModificationTime path <*> getFileSize path) :: IO (Either IOException (UTCTime, Integer)))

recordStamps :: IO Stamp
recordStamps = mapM stampOf [siteRecord, engineRecord, siteDeclared, engineDeclared]

currentPairings :: IO Pairings
currentPairings = do
    stamp  <- recordStamps
    cached <- readIORef pairingsCache
    case cached of
        Just (was, pairings) | was == stamp -> pure pairings
        _ -> do
            (ours, theirs, mine, shared) <- currentRecords stamp
            mine'   <- declaredIn Site "" mine
            shared' <- declaredIn Engine engineDir shared
            let inEngine = (engineDir </>)
                -- A tree's files only: the engine's are not the site's to
                -- record.
                site m = Map.filterWithKey (\k _ -> not ((engineDir ++ "/") `isPrefixOf` k)) m
                pairings = Pairings
                    { made = site ours <> Map.fromList
                        [ (inEngine o, m { madeFrom = inEngine (madeFrom m) }) | (o, m) <- Map.toList theirs ]
                    , declared = mine' <> shared' }
            atomicModifyIORef' pairingsCache (const (Just (stamp, pairings), ()))
            pure pairings

-- | The four files as they read, kept while none of them has changed: a
--   site with photographs has some thousands of entries, and a watch
--   asks every few seconds.
type Records = (Map.Map FilePath Made, Map.Map FilePath Made, Map.Map FilePath FilePath, Map.Map FilePath FilePath)

{-# NOINLINE recordsCache #-}
recordsCache :: IORef (Maybe (Stamp, Records))
recordsCache = unsafePerformIO (newIORef Nothing)

currentRecords :: Stamp -> IO Records
currentRecords stamp = do
    cached <- readIORef recordsCache
    case cached of
        Just (was, records) | was == stamp -> pure records
        _ -> do
            records <- (,,,) <$> readMade siteRecord <*> readMade engineRecord
                             <*> readDeclared siteDeclared <*> readDeclared engineDeclared
            atomicModifyIORef' recordsCache (const (Just (stamp, records), ()))
            pure records

-- | A tree's declarations, each read as the files it leads to. A line
--   that leads out of its tree (a site's for an engine file), names no
--   file, or gives a file a second image, is not read at all: the tools
--   stop on each of these.
declaredIn :: Tree -> FilePath -> Map.Map FilePath FilePath -> IO (Map.Map FilePath FilePath)
declaredIn tree root raw = do
    found <- forM (Map.toList raw) $ \(output, image) -> do
        output' <- own output
        image'  <- own image
        pure ((,) <$> output' <*> ((: []) <$> image'))
    pure (Map.mapMaybe one (Map.fromListWith (++) (catMaybes found)))
  where
    own path
        | null path || isAbsolute path = pure Nothing
        | otherwise = do
            found <- located (root </> path)
            case found of
                Just (tree', _) | tree' == tree -> Just <$> asRecorded (root </> path)
                _                               -> pure Nothing
    one images = case nub images of
        [image] -> Just image
        _       -> Nothing

-- | The bytes of a file as the records name them, or 'Nothing' for one
--   that cannot be read.
digestOf :: FilePath -> IO (Maybe String)
digestOf path = do
    stamp <- stampOf path
    known <- Map.lookup path <$> readIORef digestCache
    case (stamp, known) of
        (Nothing, _)                         -> pure Nothing
        (Just s, Just (was, d)) | was == s   -> pure (Just d)
        (Just s, _) -> do
            read' <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
            case read' of
                Left _      -> pure Nothing
                Right bytes -> do
                    let d = digestOfBytes bytes
                    atomicModifyIORef' digestCache (\m -> (Map.insert path (s, d) m, ()))
                    pure (Just d)

-- | A record's outputs. One that is not there says nothing was made; one
--   that cannot be read pairs nothing, and says so once.
readMade :: FilePath -> IO (Map.Map FilePath Made)
readMade path = readJson path $ \value -> case value of
    A.Object top | Just (A.Object outputs) <- KM.lookup "outputs" top ->
        Just (Map.fromList
            [ (K.toString key, Made (T.unpack source) (T.unpack digest) sourceDigest sourceStamp)
            | (key, A.Object fields) <- KM.toList outputs
            , Just (A.String source) <- [KM.lookup "source" fields]
            , Just (A.String digest) <- [KM.lookup "digest" fields]
            , let sourceDigest = case KM.lookup "source-digest" fields of
                      Just (A.String d) -> Just (T.unpack d)
                      _                 -> Nothing
                  sourceStamp = case KM.lookup "source-stamp" fields of
                      Just (A.Array pair) | [A.Number time, A.Number size] <- V.toList pair ->
                          (,) <$> whole time <*> whole size
                      _ -> Nothing ])
    _ -> Nothing
  where
    whole n = toInteger <$> (Sci.toBoundedInteger n :: Maybe Int64)

readDeclared :: FilePath -> IO (Map.Map FilePath FilePath)
readDeclared path = readJson path $ \value -> case value of
    A.Object top -> Just (Map.fromList
        [ (K.toString key, T.unpack image) | (key, A.String image) <- KM.toList top ])
    _ -> Nothing

readJson :: FilePath -> (A.Value -> Maybe (Map.Map FilePath a)) -> IO (Map.Map FilePath a)
readJson path parse = do
    there <- doesFileExist path
    if not there then pure Map.empty else do
        read' <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
        case read' >>= maybe (Left (userError "not as expected")) Right . (>>= parse) . A.decodeStrict of
            Right made -> pure made
            Left _     -> do
                hPutStrLn stderr (path ++ ": cannot be read as it should be; no image is shown with a file it names")
                pure Map.empty
