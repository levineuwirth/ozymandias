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
    , listedSection
    , sectionLanding
    , contentRoute
    , contentAddress
    , contentPageRoute
    , reservedSectionDirs
    , sectionOwnedTopLevelTags
    , directoryRouteSlugs
    , generatedIndexes
    , generatedIndexPages
    , tagPageUrl
    ) where

import Data.List  (find, isPrefixOf, isSuffixOf, nub, sort, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import Hakyll     (Routes, composeRoutes, customRoute, setExtension, toFilePath)
import Config     (SiteConfig (..), Section (..), Portal (..), Standalone (..),
                   engineContentDirs, standaloneDir, switchOn, writingDirs)

-- | Where a tag's page is, @/<tag>/@: with the tag-pages switch on, or
--   when the tag names a section, whose landing is there. None otherwise,
--   and a tag is shown without a link.
tagPageUrl :: SiteConfig -> String -> Maybe String
tagPageUrl cfg t
    | switchOn cfg "tag-pages" || t `elem` landings cfg = Just ("/" ++ t ++ "/")
    | otherwise                                         = Nothing

-- | The section a route belongs to: the one at whose address it is.
sectionOfRoute :: SiteConfig -> FilePath -> Maybe Section
sectionOfRoute cfg r = find owns (siteSections cfg)
  where
    owns s = (sectionAddress s ++ "/") `isPrefixOf` r

-- | A section the site has, by its directory.
listedSection :: SiteConfig -> String -> Maybe Section
listedSection cfg dir = find ((== dir) . sectionDir) (siteSections cfg)

-- | A section's landing, @<address>/index.html@.
sectionLanding :: Section -> FilePath
sectionLanding s = sectionAddress s ++ "/index.html"

-- | Where a file under @content/@ is served: its path there, with a
--   section's directory given as the section's address
--   (@content/blog/a.md@ at @notebook/a.md@ for a blog at @notebook@).
--   Every other path is its own address, drafts' included
--   (@content/drafts/essays/…@ stays under @drafts/essays/@).
contentRoute :: SiteConfig -> Routes
contentRoute cfg = customRoute (contentAddress cfg . toFilePath)

-- | 'contentRoute' as a function of the path: where the file at @path@
--   (@content/…@) is served.
contentAddress :: SiteConfig -> FilePath -> FilePath
contentAddress cfg path = addressed (fromMaybe path (stripPrefix "content/" path))
  where
    addressed p = case break (== '/') p of
        (dir, rest@('/' : _)) | Just s <- listedSection cfg dir -> sectionAddress s ++ rest
        _                                                       -> p

-- | 'contentRoute' for a page: @.md@ → @.html@.
contentPageRoute :: SiteConfig -> Routes
contentPageRoute cfg = contentRoute cfg `composeRoutes` setExtension "html"

-- | The pages the engine generates at @<name>/index.html@, each with the
--   switch that builds it. @tests/test_wikilink_routes.py@ checks this
--   against the routes in @build/@ and the switches' in the registry.
generatedIndexPages :: [(String, String)]
generatedIndexPages =
    [ ("archive", "archive"), ("bibliography", "bibliography-index")
    , ("build", "build-page"), ("map", "tag-map"), ("stats", "stats-page") ]

-- | Those the site's switches build.
generatedIndexes :: SiteConfig -> [String]
generatedIndexes cfg = [ index | (index, switch) <- generatedIndexPages, switchOn cfg switch ]

-- | Generated URL namespaces without an index of their own: an author's
--   page is @/authors/<slug>/@ (with the authors switch on).
generatedNamespaces :: SiteConfig -> [String]
generatedNamespaces cfg = [ "authors" | switchOn cfg "authors" ]

-- | The directories the standalone pages' sources sit in (@content/me/@),
--   and the URL namespaces their routes open (@/cv/projects/@).
standaloneDirs, standaloneNamespaces :: SiteConfig -> [String]
standaloneDirs cfg = mapMaybe standaloneDir (siteStandalone cfg)
standaloneNamespaces cfg =
    [ dir | p <- siteStandalone cfg, (dir, '/' : _) <- [break (== '/') (standaloneRoute p)] ]

-- | The sections' landings, by their addresses: URL namespaces.
landings :: SiteConfig -> [String]
landings = map sectionAddress . siteSections

-- | The directories under @content/@ that belong to a section, a standalone
--   page or the engine, never to a generic page collection: the sections
--   of writing whether the site lists them or not (an unlisted one's
--   directory holds nothing), the others as listed, and the sections'
--   addresses, where a collection's pages would be served among theirs.
--   tools/import-content.py refuses a page collection by one of these
--   names, reading this list through @site shared-rules@.
reservedSectionDirs :: SiteConfig -> [String]
reservedSectionDirs cfg = sort . nub $
    engineContentDirs ++ writingDirs ++ map sectionDir (siteSections cfg) ++ landings cfg
    ++ standaloneDirs cfg

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
    landings cfg ++ standaloneNamespaces cfg ++ generatedIndexes cfg ++ generatedNamespaces cfg

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
    landings cfg ++ generatedIndexes cfg
    ++ [ portalTag p | p <- sitePortals cfg, switchOn cfg "tag-pages" ]
    ++ [ take (length r - length index) r
       | p <- siteStandalone cfg, let r = standaloneRoute p, index `isSuffixOf` r ]
  where
    index = "/index.html"
