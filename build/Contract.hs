{-# LANGUAGE OverloadedStrings #-}
-- | The override contract's renderer (engine/contract.yaml, §3.6
--   of the plan): @site render-template TEMPLATE CONTEXT WORKDIR@ prints
--   TEMPLATE as this site's build resolves it (its own file over the
--   engine's, and so for the partials it includes), rendered with the
--   fields the JSON object in CONTEXT gives and no others.
--   tools/override-review.py renders each override so, with the fixture
--   context the contract declares, and looks for the hooks it requires.
--
--   A string is a field; true a flag, for @$if$@; a list of objects a list,
--   for @$for$@; false, or no entry, no field at all. A field the template
--   uses outside @$if$@ that the context lacks fails the render, as it
--   fails the build. WORKDIR, empty and outside the site, holds Hakyll's
--   store and output and links to the two template directories, which are
--   all the provider scans.
module Contract (renderTemplate) where

import           Control.Monad        (forM_, when)
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Key       as Key
import qualified Data.Aeson.KeyMap    as KeyMap
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           Hakyll
import           Hakyll.Commands      (build)
import qualified Hakyll.Core.Logger   as Logger
import           Hakyll.Core.Runtime  (RunMode (..))
import           System.Directory     (createDirectoryIfMissing, createDirectoryLink,
                                       doesDirectoryExist, makeAbsolute)
import           System.Exit          (ExitCode (..), exitFailure)
import           System.FilePath      ((</>))
import           System.IO            (hPutStrLn, stderr)

import           EngineFiles          (engineDir, engineTwins, templateRules)

renderTemplate :: FilePath -> FilePath -> FilePath -> IO ()
renderTemplate template contextFile work = do
    context  <- Aeson.eitherDecodeFileStrict contextFile >>= either (die contextFile) pure
    provider <- makeAbsolute (work </> "provider")
    createDirectoryIfMissing True (provider </> engineDir)
    forM_ ["templates", engineDir </> "templates"] $ \dir -> do
        present <- doesDirectoryExist dir
        when present $ makeAbsolute dir >>= \source -> createDirectoryLink source (provider </> dir)
    let conf = defaultConfiguration
            { providerDirectory    = provider
            , destinationDirectory = work </> "out"
            , storeDirectory       = work </> "store"
            , tmpDirectory         = work </> "store" </> "tmp"
            }
    (logger, logged) <- Logger.newInMem
    code <- build RunModeNormal conf logger $ do
        engineTwins >>= templateRules
        create ["rendered.html"] $ do
            route idRoute
            compile $ makeItem (context :: Aeson.Object)
                >>= loadAndApplyTemplate (fromFilePath template) objectContext
    case code of
        ExitSuccess -> readFile (work </> "out" </> "rendered.html") >>= putStr
        _ -> do
            errors <- filter ((== Logger.Error) . fst) <$> logged
            die template (unlines [dropWhile (== ' ') m | (_, m) <- errors])
  where
    die what msg = hPutStrLn stderr ("render-template: " ++ what ++ ": " ++ msg) >> exitFailure

-- | The fields of the item's object, as 'renderTemplate' describes them.
objectContext :: Context Aeson.Object
objectContext = Context $ \key _ item -> case KeyMap.lookup (Key.fromString key) (itemBody item) of
    Just (Aeson.String s)  -> pure (StringField (T.unpack s))
    Just (Aeson.Bool True) -> pure EmptyField
    Just (Aeson.Array xs)  -> ListField objectContext <$> mapM makeItem [o | Aeson.Object o <- V.toList xs]
    _                      -> noResult ("no " ++ key ++ " in the fixture context")
