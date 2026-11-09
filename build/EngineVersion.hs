{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fforce-recomp #-}
-- | Embed the inputs and actual settings used by this compilation.
module EngineVersion (compiledSourceHash, currentSourceHash) where

import Language.Haskell.TH.Syntax (addDependentFile, lift, makeRelativeToProject, runIO)
import GHC.ResponseFile (getArgsWithResponseFiles)
import SourceHash
import System.Directory (doesFileExist, makeAbsolute)
import System.FilePath ((</>))

compiledInputs :: (String, String, [String])
compiledInputs = $(do
    root <- makeRelativeToProject "." >>= runIO . makeAbsolute
    args <- runIO getArgsWithResponseFiles
    project <- runIO (compilationProject args)
    files <- runIO (sourceFiles root)
    mapM_ (addDependentFile . (root </>)) files
    -- Cabal itself recompiles when a newly created local file changes flags.
    mapM_ (\file -> do
        let path = project </> file
        present <- runIO (doesFileExist path)
        if present then addDependentFile path else pure ()) projectFiles
    sources <- runIO (sourceHash root)
    settings <- runIO (projectHash project)
    lift (sources, settings, compilerSettings root project args))

compiledSourceHash :: String
compiledSourceHash = let (sources, project, flags) = compiledInputs
                     in fingerprint sources project flags

currentSourceHash :: FilePath -> IO String
currentSourceHash root = do
    sources <- sourceHash root
    let (_, compiledProject, flags) = compiledInputs
    present <- doesFileExist "cabal.project"
    -- Synthetic render fixtures have no Cabal project. Real sites do, and
    -- must match it as well as the engine's sources and dependency freeze.
    project <- if present then projectHash "." else pure compiledProject
    pure (fingerprint sources project flags)
