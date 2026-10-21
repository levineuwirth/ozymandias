{-# LANGUAGE GHC2021 #-}
-- | The site's taxonomy: its content sections, portals and standalone
--   pages (site.yaml, "Config") and the lists derived from them, which used
--   to be kept by hand in five modules and two templates.
--
--   * 'sectionOfRoute': a page's section, for its label on the New page
--     (@$item-kind$@, "Contexts") and its default ornament ("Dingbat").
--   * 'reservedSectionDirs': directories under @content/@ that a generic
--     page collection may not take ("Patterns", @site shared-rules@).
--   * 'sectionOwnedTopLevelTags': tags that may not have a tag page,
--     because a section or a generated page owns that URL ("Tags").
--   * 'directoryRouteSlugs': bare wikilink slugs whose page is a
--     directory index ("Filters.Wikilinks").
module Sections
    ( Section (..)
    , sectionOfRoute
    , reservedSectionDirs
    , sectionOwnedTopLevelTags
    , directoryRouteSlugs
    , generatedIndexes
    ) where

import Data.List  (find, isPrefixOf, isSuffixOf, nub, sort)
import Data.Maybe (mapMaybe)
import Config     (SiteConfig (..), Section (..), Portal (..), Standalone (..),
                   engineContentDirs, standaloneDir)

-- | The section a route belongs to.
sectionOfRoute :: SiteConfig -> FilePath -> Maybe Section
sectionOfRoute cfg r = find owns (siteSections cfg)
  where
    owns s = (sectionDir s ++ "/") `isPrefixOf` r

-- | The pages the engine generates at @<name>/index.html@.
--   @tests/test_wikilink_routes.py@ checks this against the routes in
--   @build/@.
generatedIndexes :: [String]
generatedIndexes = ["archive", "bibliography", "build", "stats"]

-- | Generated URL namespaces without an index of their own: an author's
--   page is @/authors/<slug>/@.
generatedNamespaces :: [String]
generatedNamespaces = ["authors"]

-- | The directories the standalone pages' sources sit in (@content/me/@),
--   and the URL namespaces their routes open (@/cv/projects/@).
standaloneDirs, standaloneNamespaces :: SiteConfig -> [String]
standaloneDirs cfg = mapMaybe standaloneDir (siteStandalone cfg)
standaloneNamespaces cfg =
    [ dir | p <- siteStandalone cfg, (dir, '/' : _) <- [break (== '/') (standaloneRoute p)] ]

landings :: SiteConfig -> [String]
landings = map sectionDir . siteSections

-- | The directories under @content/@ that belong to a section, a standalone
--   page or the engine, never to a generic page collection.
--   tools/import-content.py refuses a page collection by one of these
--   names, reading this list through @site shared-rules@.
reservedSectionDirs :: SiteConfig -> [String]
reservedSectionDirs cfg = sort . nub $
    engineContentDirs ++ map sectionDir (siteSections cfg) ++ standaloneDirs cfg

-- | Top-level tags that must NOT get a tag page, because a section landing
--   or a generated page already owns @<tag>/index.html@. The build refuses
--   two items on one route ("RouteCheck"), so an essay tagged e.g. @music@
--   would otherwise stop it by claiming @music/index.html@. The list holds
--   every such namespace, not just the tags in use: @photography@ (every
--   photo's @tags:@ list begins with it, per the section convention) plus
--   the other section landings and generated namespaces.
--
--   Sub-tags (@photography/landscape@, @photography/film@, …) are
--   unaffected; they keep their tag pages because no section landing
--   claims those URLs.
sectionOwnedTopLevelTags :: SiteConfig -> [String]
sectionOwnedTopLevelTags cfg = nub $
    landings cfg ++ standaloneNamespaces cfg ++ generatedIndexes ++ generatedNamespaces

-- | The bare wikilink slugs whose page is a directory index: the section
--   landings, the generated indexes, the portal tags, whose tag pages
--   are directories too, and the standalone pages routed to
--   @<slug>/index.html@ (@cv/projects@). Source-level preprocessors
--   resolve a slug without Hakyll's route table, so a slug missing here
--   links to @<slug>.html@, a page that does not exist (the @/build.html@
--   404 that motivated the list; @[[Essays]]@ went to @/essays.html@ the
--   same way).
directoryRouteSlugs :: SiteConfig -> [String]
directoryRouteSlugs cfg = nub $
    landings cfg ++ generatedIndexes ++ map portalTag (sitePortals cfg)
    ++ [ take (length r - length index) r
       | p <- siteStandalone cfg, let r = standaloneRoute p, index `isSuffixOf` r ]
  where
    index = "/index.html"
