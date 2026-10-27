module Main where

import Control.Concurrent    (setNumCapabilities)
import Control.Monad         (when)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory      (createDirectoryIfMissing)
import System.Environment    (getArgs)
import Config                (SiteConfig, readSiteConfig, configMake)
import SiteThreads           (siteThreads)
import Hakyll                (hakyllWith, toFilePath)
import Golden                (renderFixture)
import Contract              (renderTemplate)
import Site                  (refusedPath, rules, siteConfigurationFor)
import Drafts                (currentUnpublished, scanUnpublished, unpublishedSummary,
                              withoutUnpublished)
import BibExtras             (BibExtra (..), parseBibExtras)
import ArchiveIndex          (normalizeUrl, trackingParams)
import Marks                 (epistemicVocabulary)
import PageScan              (pageLinks, publishedPages)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Sections              (reservedSectionDirs, sectionOwnedTopLevelTags, directoryRouteSlugs)
import Contexts              (photoVariantWidths)
import qualified Data.Aeson  as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set    as Set
import Data.List             (intercalate)
import Data.Maybe            (fromMaybe)
import Utils                 (boolSpellings, isDevBuild, outputDirFor, stripHtmlComments)
import FooterData            (writeFooterData)
import RouteCheck            (siteRoutes, siteRulesValue, withUniqueRoutes)
import Tags                  (buildAllTags, pagedTags)
import Hakyll                (tagsMap)

-- | Stamp the start of this build into @data/build-stamp.txt@ before
-- Hakyll scans the provider directory. The file therefore always exists
-- and always differs from the previous run. The telemetry pages
-- (@/build/@, @/stats/@) @load@ it as a dependency so Hakyll recompiles
-- them on every build instead of serving a stale cached copy when no
-- tracked content changed. See build/Stats.hs and build/Site.hs.
writeBuildStamp :: IO ()
writeBuildStamp = do
    createDirectoryIfMissing True "data"
    t <- getPOSIXTime
    writeFile "data/build-stamp.txt" (show t ++ "\n")

-- | 'siteConfigurationFor' (not 'Hakyll.defaultConfiguration') is the
-- publication boundary: it extends Hakyll's @ignoreFile@ so that private
-- notes, key material, and editor/interpreter junk never become
-- identifiers, and therefore can never be routed into @_site/@. See
-- build/Site.hs. Pages flagged @draft: true@ are withheld after the rules
-- run instead ('withoutUnpublished', build/Drafts.hs): @ignoreFile@ sees
-- only bare file names, and a draft is known by its path.
--
-- Commands Hakyll does not see: @site config@ prints the typed site.yaml as
-- JSON; @site config --make@ prints an includable make fragment.
-- @site render-fixture <file.md>@ renders
-- a test fixture through the essay pipeline and writes nothing
-- (build/Golden.hs); @site render-template TEMPLATE CONTEXT WORKDIR@ prints
-- a template rendered with a fixture context, for the override contract's
-- hook check (build/Contract.hs); @site footer-data@ splits the backlinks and
-- similar-links maps into per-page files between the two compile passes
-- (build/FooterData.hs); @site list-unpublished@ prints what a production
-- build withholds for @draft: true@; @site bib-extras FILE@ prints what the
-- .bib scanner (build/BibExtras.hs) reads from a file, one key per line;
-- @site shared-rules@ prints 'sharedRules' as JSON; @site private-paths@
-- echoes each path on stdin that the build refuses ('Site.refusedPath');
-- @site normalize-url@ prints each URL on stdin as the archive matches it
-- ('ArchiveIndex.normalizeUrl'), for tools/archive.py's parity test;
-- @site list-links ROOT@ prints each distinct link target on the published
-- pages under ROOT ("PageScan"), for tools/code-refs.py; @site list-routes@
-- prints every output path and the item routed there, tab-separated, for
-- tools/import-content.py (build/RouteCheck.hs); @site list-tags@ each tag
-- that gets a page, and @site expand-tags@ the pages the tags on stdin would
-- get ('Tags.pagedTags'), for tools/scaffold-photos.py; @site
-- strip-template-comments@ filters stdin as templates are ('Utils.stripHtmlComments'),
-- for its test. A build refuses to
-- run when two items share an output path ('withUniqueRoutes').
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["config"] -> readSiteConfig >>= LBS.putStrLn . Aeson.encode
        ["config", "--make"] -> readSiteConfig >>= putStr . configMake
        ["render-fixture", path] -> renderFixture path
        ["render-template", template, context, work] -> renderTemplate template context work
        ["list-unpublished"] -> scanUnpublished "content" >>= mapM_ putStrLn . unpublishedSummary
        ["shared-rules"] -> readSiteConfig >>= LBS.putStrLn . Aeson.encode . sharedRules
        ["private-paths"] -> getContents >>= mapM_ putStrLn . filter refusedPath . lines
        ["normalize-url"] -> TIO.getContents >>= mapM_ (TIO.putStrLn . normalizeUrl) . T.lines
        ["list-links", root] -> do
            cfg <- readSiteConfig
            urls <- concat <$> (publishedPages root >>= mapM (pageLinks cfg))
            mapM_ TIO.putStrLn (Set.toAscList (Set.fromList urls))
        ["bib-extras", path] -> do
            extras <- parseBibExtras path
            mapM_ (\(k, e) -> putStrLn (intercalate "\t"
                      [k, fromMaybe "" (bibFile e), intercalate "," (bibKeywords e)]))
                  (Map.toList extras)
        ["list-unpublished", root] -> scanUnpublished root >>= mapM_ putStrLn . unpublishedSummary
        ["list-routes"] -> do
            dev <- isDevBuild
            routed <- siteRoutes (siteConfigurationFor dev) (withoutUnpublished currentUnpublished rules)
            mapM_ (\(path, ident) -> putStrLn (path ++ "\t" ++ toFilePath ident)) routed
        ["expand-tags"] -> do
            cfg <- readSiteConfig
            getContents >>= mapM_ putStrLn . pagedTags cfg . lines
        ["strip-template-comments"] -> interact stripHtmlComments
        ["list-tags"] -> do
            dev <- isDevBuild
            cfg <- readSiteConfig
            tags <- siteRulesValue (siteConfigurationFor dev) (buildAllTags cfg)
            mapM_ (putStrLn . fst) (tagsMap tags)
        ["footer-data"] -> do
            dev <- isDevBuild
            when dev $ fail "footer-data: production builds only (a dev build's backlinks include drafts)"
            writeFooterData (outputDirFor False)
        _ -> do
            siteThreads >>= setNumCapabilities
            writeBuildStamp
            dev <- isDevBuild
            hakyllWith (siteConfigurationFor dev)
                (withUniqueRoutes (withoutUnpublished currentUnpublished rules))

-- | The lists the Python tools and tests must agree with the generator on,
--   read through @site shared-rules@ (tools/shared_rules.py) rather than
--   copied: each list is defined once, in its module, because copies drifted.
sharedRules :: SiteConfig -> Map.Map String Aeson.Value
sharedRules cfg = Map.fromList
    [ ("epistemic-vocabulary", Aeson.toJSON (Map.fromList epistemicVocabulary))
    , ("reserved-sections",    Aeson.toJSON (reservedSectionDirs cfg))
    , ("section-owned-tags",   Aeson.toJSON (sectionOwnedTopLevelTags cfg))
    , ("directory-slugs",      Aeson.toJSON (directoryRouteSlugs cfg))
    , ("photo-variant-widths", Aeson.toJSON photoVariantWidths)
    , ("archive-tracking-params", Aeson.toJSON trackingParams)
    , ("boolean-spellings",    Aeson.toJSON (Map.fromList
          [ (name, [s | (s, b') <- boolSpellings, b' == b])
          | (name, b) <- [("true" :: String, True), ("false", False)] ]))
    ]
