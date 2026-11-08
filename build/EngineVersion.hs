{-# LANGUAGE TemplateHaskell #-}
-- | The digest of the sources this binary was compiled from ("SourceHash"),
--   taken as it compiles. Every source is a dependent file of this module,
--   so GHC recompiles it whenever cabal recompiles any of them.
module EngineVersion (compiledSourceHash) where

import Language.Haskell.TH.Syntax (addDependentFile, lift, makeRelativeToProject, runIO)
import SourceHash                 (sourceFiles, sourceHash)
import System.Directory           (makeAbsolute)
import System.FilePath            ((</>))

compiledSourceHash :: String
compiledSourceHash = $(do
    root  <- makeRelativeToProject "." >>= runIO . makeAbsolute
    files <- runIO (sourceFiles root)
    mapM_ (addDependentFile . (root </>)) files
    runIO (sourceHash root) >>= lift)
