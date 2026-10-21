{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The engine's files inside a site's build.
--
-- A site is built from its own directory, which is also Hakyll's provider
-- directory, and the engine's templates, static files, tools and data sit
-- beneath it at @engine/@. Each engine file is the default for the path it
-- has without that prefix, and a site file at that path overrides it:
--
--   * __Templates__ are matched in both places, and an engine template no
--     site file overrides is also created under its site name, so the
--     @"templates/…"@ names the rules and @$partial()$@ calls use reach
--     whichever one wins. The alias loads the engine template, so an edit
--     to it reaches every page, and a site override arriving later is a new
--     identifier, modified whatever its mtime. One removed is not: the
--     alias returns with the override's body in the store and nothing it
--     depends on changed.
--   * __Static files__ are routed with @engine/@ stripped. One a site file
--     overrides is not matched at all, so the two never claim one route.
--   * __Raw reads__ outside Hakyll go through 'resolveFile'.
--
-- Which file wins is decided as the rules are evaluated, so adding or
-- removing an override needs a clean build (removing one always does):
-- tools/build-freshness.sh counts overrides and template aliases into its
-- rules hash. A root template moved into the engine must also acquire the
-- alias dependency instead of keeping its cached resource compiler result.
module EngineFiles
    ( engineDir
    , siteRelative
    , existingFile
    , resolveFile
    , filesIn
    , engineTwins
    , fromEngine
    , templateRules
    , staticRules
    , staticImageDimensions
    ) where

import           Control.Exception (IOException, catch)
import           Control.Monad     (forM_)
import           Data.List         (sort, stripPrefix)
import           Data.Maybe        (fromMaybe)
import qualified Data.Set          as Set
import           Hakyll
import           System.Directory  (doesFileExist, listDirectory)
import           System.FilePath   ((</>))

import           Utils             (stripHtmlComments, stripPrefixRoute)

-- | Where the engine sits in the site.
engineDir :: FilePath
engineDir = "engine"

-- | The site path an engine file stands in for: @engine/templates/x.html@
--   is @templates/x.html@. Any other path is returned unchanged.
siteRelative :: FilePath -> FilePath
siteRelative fp = fromMaybe fp (stripPrefix (engineDir ++ "/") fp)

-- | The file the build reads for a site path: the site's own, else the
--   engine's, else 'Nothing'.
existingFile :: FilePath -> IO (Maybe FilePath)
existingFile rel = do
    inSite <- doesFileExist rel
    if inSite then pure (Just rel) else do
        let fromEngineDir = engineDir </> rel
        inEngine <- doesFileExist fromEngineDir
        pure (if inEngine then Just fromEngineDir else Nothing)

-- | 'existingFile', or the site path when neither exists, so whatever
--   fails to read it names the file where a site would put it.
resolveFile :: FilePath -> IO FilePath
resolveFile rel = fromMaybe rel <$> existingFile rel

-- | The entries directly in a site directory and in the engine's, each
--   name once, at the path 'resolveFile' would give it. A missing directory
--   has none.
filesIn :: FilePath -> IO [FilePath]
filesIn dir = do
    site   <- names dir
    engine <- names (engineDir </> dir)
    let own = Set.fromList site
    pure $ [dir </> n | n <- site]
        ++ [engineDir </> dir </> n | n <- engine, not (Set.member n own)]
  where
    names d = sort <$> listDirectory d `catch` \(_ :: IOException) -> pure []

-- | The engine's files that no site file overrides, by a glob written as
--   for the site: @twins "templates/**"@ matches @engine/templates/**@.
--   Read from the provider once per rules run.
engineTwins :: Rules (String -> Pattern)
engineTwins = do
    engine     <- getMatches (fromGlob (engineDir ++ "/**"))
    overridden <- Set.fromList <$> getMatches (fromList (map twin engine))
    let live = [i | i <- engine, not (Set.member (twin i) overridden)]
    pure $ \glob -> fromList (filter (matches (fromGlob (engineDir ++ "/" ++ glob))) live)
  where
    twin = fromFilePath . siteRelative . toFilePath

-- | Route an engine file as its site twin would be.
fromEngine :: Routes -> Routes
fromEngine = composeRoutes (customRoute (siteRelative . toFilePath))

-- | Templates, the site's and the engine's. Their HTML comments are notes
--   for whoever edits them (audit IDs, why a partial is shaped as it is)
--   and are dropped as each template compiles: a comment in a partial
--   rendered inside a loop shipped once per item (about 750 copies of
--   photo-card's across the photography pages), 1.3 MB of comments across
--   the site. Comments in content are untouched.
templateRules :: (String -> Pattern) -> Rules ()
templateRules twins = do
    let engineTemplates = twins "templates/**"
    match ("templates/**" .||. engineTemplates) $ compile $ cached "Site.templateWithoutComments" $ do
        item <- getResourceBody
        tpl  <- compileTemplateItem (fmap stripHtmlComments item)
        return (itemSetBody tpl item)
    aliases <- getMatches engineTemplates
    forM_ aliases $ \engineTemplate ->
        create [fromFilePath (siteRelative (toFilePath engineTemplate))] $
            compile $ (loadBody engineTemplate :: Compiler Template) >>= makeItem

-- | Static files, the site's and the engine's, served without @static/@.
staticRules :: (String -> Pattern) -> Rules ()
staticRules twins = do
    let served glob = fromGlob glob .||. twins glob
        inBoth glob = fromGlob glob .||. fromGlob (engineDir ++ "/" ++ glob)

    -- CSS — must be matched before the broad static/** rule to avoid
    -- double-matching (compressCssCompiler vs. copyFileCompiler).
    match (served "static/css/*") $ do
        route   $ fromEngine (stripPrefixRoute "static/")
        compile compressCssCompiler

    -- All other static files (fonts, JS, images, …). Build-time
    -- sidecars produced by the Python tooling (.dims.yaml, .exif.yaml,
    -- .palette.yaml) are excluded — they're consumed by Hakyll at
    -- compile time and have no role in the deployed site.
    match (   served "static/**"
        .&&. complement (inBoth "static/css/*")
        .&&. complement (inBoth "static/**/*.dims.yaml")
        .&&. complement (inBoth "static/**/*.exif.yaml")
        .&&. complement (inBoth "static/**/*.palette.yaml")
        ) $ do
        route   $ fromEngine (stripPrefixRoute "static/")
        compile copyFileCompiler

    -- Static-image dimension sidecars (@static\/images\/foo.jpg.dims.yaml@).
    --
    -- Excluded from the copy rule above (they are compile-time inputs, not
    -- deliverables), which left them claimed by no rule at all and so
    -- outside Hakyll's universe — where a pattern dependency naming them
    -- could never fire, because the modified set is intersected with the
    -- claimed identifiers. Matched here without a route for exactly the
    -- reason the essay sidecars are: to make them trackable. Audit B11.
    -- Claim shadowed sidecars too: the dependency pattern covers both
    -- roots, and every identifier it sees must have an unversioned rule.
    match staticImageDimensions $ compile getResourceLBS

-- | Dimension inputs read outside Hakyll by Filters.Images. Both roots
-- are tracked, including new sidecars and site overrides.
staticImageDimensions :: Pattern
staticImageDimensions = "static/**/*.dims.yaml" .||. "engine/static/**/*.dims.yaml"
