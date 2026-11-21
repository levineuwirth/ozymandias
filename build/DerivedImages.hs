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
--   * the ownership record says a tool made it from that image, and its
--     bytes are still the ones the tool wrote
--     (@data\/derived-images.json@, tools/derived_images.py: written by
--     tools/convert-images.sh and tools/generate-thumbnails.py);
--   * the author declared it the image's companion
--     (@data\/companions.json@: @{"content\/blog\/moving.webp":
--     "content\/blog\/moving.gif"}@).
--
--   Each tree has its record and its declarations under its own root:
--   the site's, and the engine's (@engine\/data\/…@, for the images under
--   @engine\/static\/@), with paths relative to that root. A site's
--   declaration of an engine file counts for nothing: the other sites
--   that link the engine would not know of it.
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
--   (@data\/pairings\/content\/blog.json@: the outputs there that are
--   their images', and the images), and writes one only when it differs
--   from what is there. A page depends on the files of the directories it
--   asked about ('pairedWithC', 'tellPairings'), which a rule claims, and
--   Hakyll rebuilds it when one of them changes, appears or goes.
module DerivedImages
    ( pairedWith
    , pairedFor
    , pairedWithC
    , tellPairings
    , refreshPairingStates
    , pairingStates
    ) where

import           Control.Exception     (IOException, try)
import           Control.Monad         (filterM, forM, forM_, unless, when)
import           Crypto.Hash           (Digest, SHA256, hash)
import qualified Data.Aeson            as A
import qualified Data.Aeson.Key        as K
import qualified Data.Aeson.KeyMap     as KM
import qualified Data.ByteString       as BS
import           Data.IORef            (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.List             (intercalate, isPrefixOf)
import qualified Data.Map.Strict       as Map
import qualified Data.Text             as T
import           Data.Time.Clock       (UTCTime)
import qualified Data.Set              as Set
import           Hakyll                (Compiler, Pattern, fromFilePath, fromGlob, fromList, getMatches,
                                        unsafeCompiler)
import           Hakyll.Core.Compiler.Internal (compilerTellDependencies)
import           Hakyll.Core.Dependencies (Dependency (..))
import           System.Directory      (createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
                                        getCurrentDirectory, getFileSize, getModificationTime,
                                        listDirectory, pathIsSymbolicLink, removeFile)
import           System.FilePath       (isAbsolute, makeRelative, splitDirectories, takeDirectory, (</>))
import           System.IO             (hPutStrLn, stderr)
import           System.IO.Unsafe      (unsafePerformIO)

-- | What a record says of one output: the image it was made from, and
--   the bytes last written.
data Made = Made { madeFrom :: FilePath, madeDigest :: String }

-- | The records and declarations of both trees, with every path as the
--   build has it (an engine file's under @engine\/@).
data Pairings = Pairings
    { made     :: Map.Map FilePath Made
    , declared :: Map.Map FilePath FilePath   -- ^ a WebP, and the image it is for
    }

siteRecord, engineRecord, siteDeclared, engineDeclared, engineDir, stateDir :: FilePath
siteRecord     = "data/derived-images.json"
engineRecord   = engineDir </> siteRecord
siteDeclared   = "data/companions.json"
engineDeclared = engineDir </> siteDeclared
engineDir      = "engine"
stateDir       = "data/pairings"

-- | Is @output@ the file of @image@ that its name says it is? Both are
--   paths in the site as the build has them (@content\/blog\/lake.jpg@,
--   @engine\/static\/images\/mark.png@), or absolute paths under it.
pairedWith :: FilePath -> FilePath -> IO Bool
pairedWith image output = do
    here <- getCurrentDirectory
    pairings <- currentPairings
    holds pairings (inSite here image) (inSite here output)

-- | Does the pairing hold now: declared, or recorded with the bytes that
--   are there?
holds :: Pairings -> FilePath -> FilePath -> IO Bool
holds pairings image output = do
    there <- doesFileExist output
    if not there then pure False
    else if Map.lookup output (declared pairings) == Just image then pure True
    else case Map.lookup output (made pairings) of
        Just m | madeFrom m == image -> do
            link <- pathIsSymbolicLink output
            if link then pure False else (== Just (madeDigest m)) <$> digestOf output
        _ -> pure False

-- | 'pairedWith', and a note that whoever compiles under @key@ asked
--   about this output: "Filters.Images" runs in IO and cannot tell Hakyll
--   itself, so the page's compiler does when the filter has run
--   ('tellPairings', with the same key: its source directory).
pairedFor :: FilePath -> FilePath -> FilePath -> IO Bool
pairedFor key image output = do
    here <- getCurrentDirectory
    atomicModifyIORef' askedAbout
        (\m -> (Map.insertWith Set.union key (Set.singleton (stateFileOf (inSite here output))) m, ()))
    pairedWith image output

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

-- | The state files, for the rule that claims them: a dependency on a
--   file no rule claims never fires.
pairingStates :: Pattern
pairingStates = fromGlob (stateDir ++ "/**.json")

-- | The state file of an output's directory.
stateFileOf :: FilePath -> FilePath
stateFileOf output = stateDir </> takeDirectory output ++ ".json"

-- | Write what holds now, a file for each directory of outputs, touching
--   none that already says it. Run before Hakyll reads the site, in every
--   build: what a cached page was built with is compared with this.
refreshPairingStates :: IO ()
refreshPairingStates = do
    pairings <- currentPairings
    let claimed = [ (output, madeFrom m) | (output, m) <- Map.toList (made pairings) ]
                  ++ Map.toList (declared pairings)
    paired <- filterM (\(output, image) -> holds pairings image output) claimed
    let states = Map.fromListWith Map.union
            [ (stateFileOf output, Map.singleton output image) | (output, image) <- paired ]
    forM_ (Map.toList states) $ \(file, pairs) -> do
        let text = A.encode (Map.toAscList pairs)
        was <- try (BS.readFile file) :: IO (Either IOException BS.ByteString)
        when (was /= Right (BS.toStrict text)) $ do
            createDirectoryIfMissing True (takeDirectory file)
            BS.writeFile file (BS.toStrict text)
    -- A directory with nothing paired any more has no file.
    old <- filesUnder stateDir
    forM_ [ f | f <- old, f `Map.notMember` states ] removeFile
  where
    filesUnder dir = do
        there <- doesDirectoryExist dir
        if not there then pure [] else do
            names <- listDirectory dir
            fmap concat $ forM names $ \name -> do
                let path = dir </> name
                isDir <- doesDirectoryExist path
                if isDir then filesUnder path else pure [path]

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

currentPairings :: IO Pairings
currentPairings = do
    stamp  <- mapM stampOf [siteRecord, engineRecord, siteDeclared, engineDeclared]
    cached <- readIORef pairingsCache
    case cached of
        Just (was, pairings) | was == stamp -> pure pairings
        _ -> do
            ours   <- readMade siteRecord
            theirs <- readMade engineRecord
            mine   <- readDeclared siteDeclared
            shared <- readDeclared engineDeclared
            let inEngine = (engineDir </>)
                -- A tree's files only: the engine's are not the site's to
                -- record or to declare.
                site m = Map.filterWithKey (\k _ -> not ((engineDir ++ "/") `isPrefixOf` k)) m
                pairings = Pairings
                    { made = site ours <> Map.fromList
                        [ (inEngine o, m { madeFrom = inEngine (madeFrom m) }) | (o, m) <- Map.toList theirs ]
                    , declared = site mine <> Map.fromList
                        [ (inEngine o, inEngine i) | (o, i) <- Map.toList shared ] }
            atomicModifyIORef' pairingsCache (const (Just (stamp, pairings), ()))
            pure pairings

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
                    let d = "sha256:" ++ show (hash bytes :: Digest SHA256)
                    atomicModifyIORef' digestCache (\m -> (Map.insert path (s, d) m, ()))
                    pure (Just d)

-- | A record's outputs. One that is not there says nothing was made; one
--   that cannot be read pairs nothing, and says so once.
readMade :: FilePath -> IO (Map.Map FilePath Made)
readMade path = readJson path $ \value -> case value of
    A.Object top | Just (A.Object outputs) <- KM.lookup "outputs" top ->
        Just (Map.fromList
            [ (K.toString key, Made (T.unpack source) (T.unpack digest))
            | (key, A.Object fields) <- KM.toList outputs
            , Just (A.String source) <- [KM.lookup "source" fields]
            , Just (A.String digest) <- [KM.lookup "digest" fields] ])
    _ -> Nothing

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
