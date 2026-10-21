{-# LANGUAGE GHC2021 #-}
-- | One output file per route.
--
--   Hakyll notices two items writing one file only when both are compiled
--   in the same run, and then only after both have written it and every
--   other item has compiled. An incremental build skips up-to-date items,
--   so a new page that takes an existing page's route replaces it with no
--   error: a page collection named @notes@ beside a @notes@ tag built,
--   exit 0, with the tag page gone. Routes are decided in places that know
--   nothing of each other (sections, page collections, tag pages, author
--   pages, photo series and their tags, the telemetry and archive pages,
--   copied static files). 'withUniqueRoutes' routes every item the rules
--   compile and refuses the build, before anything compiles, when two
--   claim one file; @site list-routes@ prints the same table, and
--   @site list-tags@ the tags that get a page, for the importers
--   (tools/import-content.py, tools/scaffold-photos.py).
--
--   Uses Hakyll's exposed "Hakyll.Core.Rules.Internal", as
--   'Drafts.withoutUnpublished' does; a Hakyll release that changes
--   'RuleSet' fails to compile here.
module RouteCheck
    ( withUniqueRoutes
    , siteRoutes
    , siteRulesValue
    ) where

import           Control.Monad              (forM, unless)
import           Control.Monad.Reader       (asks)
import           Control.Monad.RWS          (runRWST)
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Writer       (listen)
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Data.List                  (intercalate, sort)
import           System.FilePath            (normalise)
import           Hakyll                     (Configuration (..), Identifier, shouldIgnoreFile)
import           Hakyll.Core.Provider       (Provider, newProvider)
import           Hakyll.Core.Routes         (runRoutes)
import           Hakyll.Core.Rules.Internal (RuleSet (..), RulesRead (..), Rules (..),
                                             emptyRulesState, runRules)
import qualified Hakyll.Core.Store          as Store

-- | Every routed item: (output path, identifier), sorted. An item the
--   rules compile without a route is written nowhere and left out.
routedItems :: Provider -> RuleSet -> IO [(FilePath, Identifier)]
routedItems provider rs = do
    let ids = Set.toList (Set.fromList (map fst (rulesCompilers rs)))
    routed <- forM ids $ \i -> do
        (mroute, _) <- runRoutes (rulesRoutes rs) provider i
        return [(normalise r, i) | Just r <- [mroute]]
    return (sort (concat routed))

-- | Output paths claimed by more than one item, with the items.
routeClashes :: [(FilePath, Identifier)] -> [(FilePath, [Identifier])]
routeClashes routed =
    [ (path, sort is)
    | (path, is) <- Map.toList (Map.fromListWith (++) [(p, [i]) | (p, i) <- routed])
    , length is > 1 ]

-- | The rules, refusing to run when two items share a route. The check
--   runs once the rules have been evaluated, before anything is compiled
--   or written.
withUniqueRoutes :: Rules a -> Rules a
withUniqueRoutes (Rules m) = Rules $ do
    (a, rs)  <- listen m
    provider <- asks rulesProvider
    clashes  <- liftIO (routeClashes <$> routedItems provider rs)
    unless (null clashes) $ fail $ unlines $
        "Two or more items route to the same file; the build would keep only one:"
        : [ "  " ++ path ++ " <- " ++ intercalate ", " (map show is) | (path, is) <- clashes ]
    return a

-- | The routed items for @site list-routes@: the rules evaluated against
--   the same provider and store a build uses, without compiling anything.
siteRoutes :: Configuration -> Rules a -> IO [(FilePath, Identifier)]
siteRoutes config rules = do
    provider <- siteProvider config
    rs       <- runRules rules provider
    routedItems provider rs

-- | What a 'Rules' action returns (the tag index, say), evaluated as
--   'siteRoutes' evaluates the rules.
siteRulesValue :: Configuration -> Rules a -> IO a
siteRulesValue config (Rules m) = do
    provider  <- siteProvider config
    (a, _, _) <- runRWST m (RulesRead provider [] Nothing) emptyRulesState
    return a

siteProvider :: Configuration -> IO Provider
siteProvider config = do
    store <- Store.new (inMemoryCache config) (storeDirectory config)
    newProvider store (shouldIgnoreFile config) (providerDirectory config)
