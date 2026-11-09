-- | Compiler inputs, independent of the engine's Git revision.
module SourceHash (sourceFiles, sourceHash, projectFiles, projectHash, fingerprint,
                   compilationProject, compilerSettings) where

import Crypto.Hash (Digest, SHA256, hashlazy)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Char (isSpace)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.FilePath ((</>), takeDirectory)
import System.Info (compilerName, compilerVersion)
import Data.Version (showVersion)

sourceFiles :: FilePath -> IO [FilePath]
sourceFiles root = (++ ["ozymandias.cabal", "cabal.project.freeze"]) <$> walk "build"
  where
    walk rel = do
        entries <- sort <$> listDirectory (root </> rel)
        concat <$> mapM (\entry -> do
            let path = rel </> entry
            directory <- doesDirectoryExist (root </> path)
            if directory then walk path else pure [path | ".hs" `isSuffixOf` entry]) entries

hashFiles :: FilePath -> [FilePath] -> IO String
hashFiles root files = do
    parts <- mapM (\file -> do
        present <- doesFileExist (root </> file)
        bytes <- if present then BS.readFile (root </> file) else pure (BC.pack "<absent>")
        pure [BC.pack file, BS.singleton 0, bytes, BS.singleton 0]) files
    pure (show (hashlazy (LBS.fromChunks (concat parts)) :: Digest SHA256))

sourceHash :: FilePath -> IO String
sourceHash root = sourceFiles root >>= hashFiles root

-- Project settings include the compiler selection, optimisation and package
-- flags. The local file's absence is itself part of the fingerprint.
projectFiles :: [FilePath]
projectFiles = ["cabal.project", "cabal.project.local", "cabal.project.freeze"]

projectHash :: FilePath -> IO String
projectHash root = do
    values <- mapM (\file -> do
        present <- doesFileExist (root </> file)
        bytes <- if present then readFile (root </> file) else pure ""
        -- An absent local file and one containing only comments have the
        -- same settings. Cabal need not recompile for that transition.
        let meaningful = filter (\line -> not (null line) && not ("--" `isPrefixOf` line))
                         (map (dropWhile isSpace) (lines bytes))
        pure (file, meaningful)) projectFiles
    pure (show (hashlazy (LBS.fromStrict (BC.pack (show values))) :: Digest SHA256))

fingerprint :: String -> String -> [String] -> String
fingerprint sources project settings = show
    (hashlazy (LBS.fromStrict (BC.pack (show (sources, project, settings)))) :: Digest SHA256)

-- Cabal runs GHC in the package directory even for a sibling engine. Its
-- output directory identifies the site's project, without an environment
-- variable that direct `cabal build` callers could forget to set.
compilationProject :: [String] -> IO FilePath
compilationProject args = case dropWhile (/= "-outputdir") args of
    (_:directory:_) -> makeAbsolute directory >>= ascend
    _ -> fail "cannot locate Cabal's project: GHC has no -outputdir"
  where
    ascend path = do
        found <- doesFileExist (path </> "cabal.project")
        if found then pure path else
            if takeDirectory path == path
            then fail "cannot locate cabal.project above GHC's output directory"
            else ascend (takeDirectory path)

-- Record the actual GHC invocation, including CLI overrides and resolved
-- package IDs. Replace checkout paths so moving a checkout is immaterial.
compilerSettings :: FilePath -> FilePath -> [String] -> [String]
compilerSettings engine project args =
    (compilerName ++ "-" ++ showVersion compilerVersion) : map normalise args
  where
    normalise = T.unpack . T.replace (T.pack engine) (T.pack "<engine>")
                . T.replace (T.pack project) (T.pack "<project>") . T.pack
