-- | The generator's own sources as one digest: what a binary was compiled
--   from ("EngineVersion") and what an engine checkout holds now, which a
--   build compares before it runs (plan §3.7). A digest of the sources, not
--   the engine's commit: a commit that changes no Haskell leaves the binary
--   as it was, and as right for the checkout as before.
module SourceHash (sourceFiles, sourceHash) where

import           Crypto.Hash          (Digest, SHA256, hashlazy)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import           Data.List            (isSuffixOf, sort)
import           System.Directory     (doesDirectoryExist, listDirectory)
import           System.FilePath      ((</>))

-- | Under the engine's root, the files a binary is compiled from: every
--   Haskell file under @build/@, which cabal compiles, and the cabal file,
--   which says how. Each relative to the root, in a fixed order.
sourceFiles :: FilePath -> IO [FilePath]
sourceFiles root = (++ ["ozymandias.cabal"]) <$> walk "build"
  where
    walk rel = do
        entries <- sort <$> listDirectory (root </> rel)
        concat <$> mapM (\entry -> do
            let path = rel </> entry
            directory <- doesDirectoryExist (root </> path)
            if directory then walk path else pure [path | ".hs" `isSuffixOf` entry]) entries

-- | Each file's path and bytes, digested together.
sourceHash :: FilePath -> IO String
sourceHash root = do
    files <- sourceFiles root
    parts <- mapM (\file -> do
        bytes <- BS.readFile (root </> file)
        pure [BC.pack file, BS.singleton 0, bytes, BS.singleton 0]) files
    pure (show (hashlazy (LBS.fromChunks (concat parts)) :: Digest SHA256))
