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
--   @static\/lake.webp@). A site's declaration of an engine file counts
--   for nothing (the other sites that link the engine would not know of
--   it), and the tools stop on one.
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
--   @data\/pairings\/manifest.json@ lists the state files written, and
--   only a file it lists is ever deleted: anything else under
--   @data\/pairings\/@ is not this module's, and stays. Nothing is
--   written or deleted through a link.
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

import           Control.Concurrent    (threadDelay)
import           Control.Exception     (IOException, SomeException, try)
import           Control.Monad         (filterM, forM, forM_, unless, when)
import           Crypto.Hash           (Digest, SHA256, hash)
import qualified Data.Aeson            as A
import qualified Data.Aeson.Key        as K
import qualified Data.Aeson.KeyMap     as KM
import qualified Data.ByteString       as BS
import           Data.Int              (Int64)
import           Data.IORef            (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import           Data.List             (inits, intercalate, isPrefixOf, isSuffixOf)
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
import           System.Directory      (createDirectoryIfMissing, doesFileExist,
                                        getCurrentDirectory, getFileSize, getModificationTime,
                                        pathIsSymbolicLink, removeFile, renameFile)
import           System.FilePath       (isAbsolute, joinPath, makeRelative, splitDirectories, takeDirectory,
                                        (</>))
import           System.IO             (hPutStrLn, stderr)
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

siteRecord, engineRecord, siteDeclared, engineDeclared, engineDir, stateRoot, stateDir, manifest :: FilePath
siteRecord     = "data/derived-images.json"
engineRecord   = engineDir </> siteRecord
siteDeclared   = "data/companions.json"
engineDeclared = engineDir </> siteDeclared
engineDir      = "engine"
stateRoot      = "data/pairings"
stateDir       = stateRoot </> "states"
manifest       = stateRoot </> "manifest.json"

-- | Is @output@ the file of @image@ that its name says it is? Both are
--   paths in the site as the build has them (@content\/blog\/lake.jpg@,
--   @engine\/static\/images\/mark.png@), or absolute paths under it.
pairedWith :: FilePath -> FilePath -> IO Bool
pairedWith image output = do
    here <- getCurrentDirectory
    pairings <- currentPairings
    holds pairings (inSite here image) (inSite here output)

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
    here <- getCurrentDirectory
    atomicModifyIORef' askedAbout
        (\m -> (Map.insertWith Set.union key (Set.singleton (stateFileOf (inSite here output))) m, ()))

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
    here <- unsafeCompiler getCurrentDirectory
    dependOn [stateFileOf (inSite here output)]
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
refreshPairingStates :: IO ()
refreshPairingStates = do
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
        rootOurs <- noLinkIn stateRoot
        if not rootOurs
            then do
                -- Said once in a process: a watch comes by here again and again.
                warned <- atomicModifyIORef' warnedOfLink (\w -> (True, w))
                unless warned $
                    hPutStrLn stderr (stateRoot ++ ": a link, or a link leads to it; no pairing state is written "
                                      ++ "through one, and pages are not rebuilt when a pairing changes")
            else do
                was <- readManifest
                written <- fmap catMaybes $ forM (Map.toList states) $ \(file, text) -> do
                    ours <- ownPlace file
                    if not ours
                        then do
                            hPutStrLn stderr (file ++ ": not written (a link is in the way)")
                            pure Nothing
                        else do
                            there <- try (BS.readFile file) :: IO (Either IOException BS.ByteString)
                            when (there /= Right text) $ do
                                createDirectoryIfMissing True (takeDirectory file)
                                writeWhole file text
                            pure (Just (file, digestOfBytes text))
                -- A directory with nothing paired any more has no file. Only
                -- a file the manifest says was written here goes: whatever
                -- else is under data/pairings/ is someone's.
                forM_ [ file | file <- Map.keys was, file `Map.notMember` states ] $ \file -> do
                    ours  <- ownPlace file
                    there <- doesFileExist file
                    when (ours && there) (removeFile file)
                let now = Map.fromList written
                manifestOurs <- notALink manifest
                when (now /= was && manifestOurs) $ do
                    createDirectoryIfMissing True stateRoot
                    writeWhole manifest (BS.toStrict (A.encode now))
                writeIORef lastStates (Just states)
  where
    readManifest = readJson manifest $ \value -> case value of
        A.Object top -> Just (Map.fromList [ (K.toString key, T.unpack d) | (key, A.String d) <- KM.toList top ])
        _            -> Nothing

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
watchPairingStates :: IO ()
watchPairingStates = go (1 :: Int) Nothing
  where
    go n was = do
        threadDelay 2000000
        stamp <- recordStamps
        when (n == 0 || Just stamp /= was) $ do
            done <- try refreshPairingStates :: IO (Either SomeException ())
            either (\err -> hPutStrLn stderr ("pairing states: " ++ show err)) pure done
        go ((n + 1) `mod` 5) (Just stamp)

-- | May a state file be written or deleted at this path? It is under the
--   state directory by its name, a @.json@, and no link stands between
--   the two or in the file's own place: what a link leads to is not this
--   module's to write.
ownPlace :: FilePath -> IO Bool
ownPlace file
    | not ((stateDir ++ "/") `isPrefixOf` file) || not (".json" `isSuffixOf` file) = pure False
    | any (`elem` [".", ".."]) parts || joinPath parts /= file                    = pure False
    | otherwise                                                                    = noLinkIn file
  where parts = splitDirectories file

-- | Is no part of the path, from the pairings directory down, a link?
--   (Parts that are not there yet are no links.)
noLinkIn :: FilePath -> IO Bool
noLinkIn path = and <$> mapM notALink
    [ joinPath prefix | prefix <- drop (length (splitDirectories stateRoot)) (inits (splitDirectories path))
                      , not (null prefix) ]

notALink :: FilePath -> IO Bool
notALink path = either (const True) not <$> (try (pathIsSymbolicLink path) :: IO (Either IOException Bool))

-- | The whole file or none of it, as a reader (a build under way) sees it.
writeWhole :: FilePath -> BS.ByteString -> IO ()
writeWhole file bytes = do
    let tmp = file ++ ".tmp"
    _ <- try (removeFile tmp) :: IO (Either IOException ())
    BS.writeFile tmp bytes
    renameFile tmp file

digestOfBytes :: BS.ByteString -> String
digestOfBytes bytes = "sha256:" ++ show (hash bytes :: Digest SHA256)

{-# NOINLINE lastStates #-}
lastStates :: IORef (Maybe (Map.Map FilePath BS.ByteString))
lastStates = unsafePerformIO (newIORef Nothing)

{-# NOINLINE warnedOfLink #-}
warnedOfLink :: IORef Bool
warnedOfLink = unsafePerformIO (newIORef False)

{-# NOINLINE askedAbout #-}
askedAbout :: IORef (Map.Map FilePath (Set.Set FilePath))
askedAbout = unsafePerformIO (newIORef Map.empty)

-- | A path as the records have it: relative to the site, with no @.@ or
--   @..@ in it.
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
            ours   <- readMade siteRecord
            theirs <- readMade engineRecord
            mine   <- readDeclared siteDeclared
            shared <- readDeclared engineDeclared
            let inEngine = (engineDir </>)
                ofEngine = ((engineDir ++ "/") `isPrefixOf`)
                -- A tree's files only: the engine's are not the site's to
                -- record or to declare.
                site m = Map.filterWithKey (\k _ -> not (ofEngine k)) m
                -- A declaration is hand-written: read as the file it
                -- leads to, and not at all when it leads out of its tree.
                plain m = Map.fromList
                    [ (o', i') | (o, i) <- Map.toList m, Just o' <- [withinTree o], Just i' <- [withinTree i] ]
                pairings = Pairings
                    { made = site ours <> Map.fromList
                        [ (inEngine o, m { madeFrom = inEngine (madeFrom m) }) | (o, m) <- Map.toList theirs ]
                    , declared = Map.filter (not . ofEngine) (site (plain mine)) <> Map.fromList
                        [ (inEngine o, inEngine i) | (o, i) <- Map.toList (plain shared) ] }
            atomicModifyIORef' pairingsCache (const (Just (stamp, pairings), ()))
            pure pairings

-- | A path in a tree as written by hand, without its @.@ and @..@; none
--   for one that is absolute or leads out of the tree.
withinTree :: FilePath -> Maybe FilePath
withinTree path
    | isAbsolute path = Nothing
    | otherwise       = intercalate "/" <$> go [] (splitDirectories path)
  where
    go acc []            = if null acc then Nothing else Just (reverse acc)
    go acc ("."  : rest) = go acc rest
    go []  (".." : _)    = Nothing
    go acc (".." : rest) = go (drop 1 acc) rest
    go acc (part : rest) = go (part : acc) rest

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
