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
--   Each tree has its record under its own root: the site's, and the
--   engine's (@engine\/data\/derived-images.json@, for the images under
--   @engine\/static\/@), with paths relative to that root.
--
--   Everything that would use such a file asks here: "Filters.Images"
--   (a body image's @\<picture>@), "Contexts" (a photograph's srcsets)
--   and "Drafts" (which of the blog's images a published page shows).
module DerivedImages
    ( pairedWith
    ) where

import           Control.Exception     (IOException, try)
import           Crypto.Hash           (Digest, SHA256, hash)
import qualified Data.Aeson            as A
import qualified Data.Aeson.Key        as K
import qualified Data.Aeson.KeyMap     as KM
import qualified Data.ByteString       as BS
import           Data.IORef            (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.List             (intercalate, stripPrefix)
import qualified Data.Map.Strict       as Map
import qualified Data.Text             as T
import           Data.Time.Clock       (UTCTime)
import           System.Directory      (doesFileExist, getCurrentDirectory, getFileSize,
                                        getModificationTime, pathIsSymbolicLink)
import           System.FilePath       (isAbsolute, makeRelative, splitDirectories, (</>))
import           System.IO             (hPutStrLn, stderr)
import           System.IO.Unsafe      (unsafePerformIO)

-- | What a record says of one output: the image it was made from, and
--   the bytes last written.
data Made = Made { madeFrom :: FilePath, madeDigest :: String }

data Pairings = Pairings
    { siteMade   :: Map.Map FilePath Made
    , engineMade :: Map.Map FilePath Made
    , declared   :: Map.Map FilePath FilePath   -- ^ a WebP, and the image it is for
    }

siteRecord, engineRecord, declarations, engineDir :: FilePath
siteRecord   = "data/derived-images.json"
engineRecord = engineDir </> "data/derived-images.json"
declarations = "data/companions.json"
engineDir    = "engine"

-- | Is @output@ the file of @image@ that its name says it is? Both are
--   paths in the site as the build has them (@content\/blog\/lake.jpg@,
--   @engine\/static\/images\/mark.png@), or absolute paths under it.
pairedWith :: FilePath -> FilePath -> IO Bool
pairedWith image output = do
    here <- getCurrentDirectory
    let image'  = inSite here image
        output' = inSite here output
    there <- doesFileExist output'
    if not there then pure False else do
        pairings <- currentPairings
        if Map.lookup output' (declared pairings) == Just image' then pure True else
            case recorded pairings image' output' of
                Nothing     -> pure False
                Just digest -> do
                    link <- pathIsSymbolicLink output'
                    if link then pure False else (== Just digest) <$> digestOf output'
  where
    -- The entry that binds this output to this image, in the record of
    -- the tree both are in.
    recorded pairings i o = case (stripPrefix (engineDir ++ "/") i, stripPrefix (engineDir ++ "/") o) of
        (Just i', Just o') -> entry (engineMade pairings) i' o'
        (Nothing, Nothing) -> entry (siteMade pairings) i o
        _                  -> Nothing
    entry made i o = case Map.lookup o made of
        Just m | madeFrom m == i -> Just (madeDigest m)
        _                        -> Nothing

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
    stamp  <- mapM stampOf [siteRecord, engineRecord, declarations]
    cached <- readIORef pairingsCache
    case cached of
        Just (was, pairings) | was == stamp -> pure pairings
        _ -> do
            pairings <- Pairings <$> readMade siteRecord <*> readMade engineRecord <*> readDeclared
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

readDeclared :: IO (Map.Map FilePath FilePath)
readDeclared = readJson declarations $ \value -> case value of
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
