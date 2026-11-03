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
--   copied static files). 'withCheckedRoutes' routes every item the rules
--   compile and refuses the build, before anything compiles, when two
--   claim one file, or when site.yaml links a page a switch it has off
--   would publish and nothing else builds; @site list-routes@ prints the
--   same table, and
--   @site list-tags@ the tags that get a page, for the importers
--   (tools/import-content.py, tools/scaffold-photos.py).
--
--   Uses Hakyll's exposed "Hakyll.Core.Rules.Internal", as
--   'Drafts.withoutUnpublished' does; a Hakyll release that changes
--   'RuleSet' fails to compile here.
module RouteCheck
    ( withCheckedRoutes
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
import           Data.List                  (intercalate, isSuffixOf, sort)
import           System.FilePath            (normalise, (</>))
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

-- | The rules, refusing to run when two items share a route, or when one
--   of @links@ (site.yaml's links to pages of switches it has off, each
--   with its switch: 'Config.siteSwitchedLinks') names a file no item
--   builds. Another item may build it: with stats-page off, a tag
--   @stats@ has its page at @/stats/@. The checks run once the rules have
--   been evaluated, before anything is compiled or written.
withCheckedRoutes :: [(String, String)] -> Rules a -> Rules a
withCheckedRoutes links (Rules m) = Rules $ do
    (a, rs)  <- listen m
    provider <- asks rulesProvider
    routed   <- liftIO (routedItems provider rs)
    let clashes = routeClashes routed
    unless (null clashes) $ fail $ unlines $
        "Two or more items route to the same file; the build would keep only one:"
        : [ "  " ++ path ++ " <- " ++ intercalate ", " (map show is) | (path, is) <- clashes ]
    let built   = Set.fromList (map fst routed)
        missing = [ href ++ " (the " ++ switch ++ " switch is off)"
                  | (href, switch) <- links, not (any (`Set.member` built) (linkFiles href)) ]
    unless (null missing) $ fail $
        "site.yaml: nav-primary and the library's see-also link pages that are not built: "
        ++ intercalate ", " missing
    return a

-- | The files a site path may be served from, a fragment or query aside:
--   @/a/@ is @a/index.html@, @/a.html@ that file (or a directory's index).
linkFiles :: String -> [FilePath]
linkFiles href = case takeWhile (`notElem` ("#?" :: String)) href of
    '/' : p | null p || "/" `isSuffixOf` p -> [normalise (p ++ "index.html")]
            | otherwise                     -> [normalise p, normalise (p </> "index.html")]
    _ -> []

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
