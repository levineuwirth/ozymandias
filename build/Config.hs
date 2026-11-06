{-# LANGUAGE OverloadedStrings #-}
-- | Site-owned identity and feeds, and the site's switches over the
-- engine's registry (engine/switches.yaml). Rule construction reads this
-- explicitly in 'preprocess'; compilers use 'loadConfig', so site.yaml and
-- the registry are real Hakyll dependencies. There is deliberately no
-- process-global configuration.
module Config
    ( SiteConfig (..), FeedSpec (..), FeedSection (..), ManifestConfig (..)
    , Section (..), Portal (..), NavLink (..), LibraryConfig (..)
    , Standalone (..), PageKind (..), Season (..), standaloneDir, engineSources, engineContentDirs
    , configRules, readSiteConfig, loadConfig, configField, switchOn, switchContext, registryPath
    , renderSiteFeed, configMake, photoFeed, sectionName, loadFeed
    , siteOwnHosts, identityContext, identityFields, navigationContext, seasonContext
    , manifestRules
    , knownDingbats
    ) where

import Control.Monad (forM_, mfilter, unless, when)
import EngineFiles (assetsField)
import Data.Aeson (FromJSON (..), ToJSON (..), Object,
                   withObject, withText, object, (.:), (.:?), (.!=), (.=), encode)
import Data.Aeson.Types (Parser)
import Data.Bifunctor (first)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Char (isControl, isSpace, toLower, toUpper)
import Data.List (find, intercalate, isPrefixOf, isSuffixOf, nub, sortOn, stripPrefix)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Yaml as Yaml
import Hakyll hiding (escapeHtml)
import System.FilePath (splitDirectories)
import Utils (escapeHtml)

data SiteConfig = SiteConfig
    { siteTitle :: String
    , siteAuthor :: String
    , siteAuthorSort :: String
    , siteEmail :: String
    , siteContactEmail :: String
    , siteUrl :: String
    , siteHost :: String
    , siteSourceRepo :: String
    , siteSourceBranch :: String
    , siteForgeHost :: String
    , siteContentLicense :: String
    , siteCodeLicense :: String
    , siteSigningFingerprint :: String
    , siteManifest :: ManifestConfig
    , siteFeeds :: [FeedSpec]
    , siteSections :: [Section]
    , sitePortals :: [Portal]
    , siteNavPrimary :: [NavLink]
      -- | The library page's settings: needed with the library switch on,
      --   and its see-also also lists the bibliography pages' links.
    , siteLibrary :: Maybe LibraryConfig
    , siteStandalone :: [Standalone]
    , siteFontPreloads :: [String]
      -- | Where the site explains its epistemic profile (a path, with its
      --   anchor), which the footer's Epistemic heading links to; none, unlinked.
    , siteEpistemicAbout :: String
    , siteSeasons :: [Season]
    , siteSwitches :: Map.Map String Bool
      -- | The engine's static files (globs as served) of the switches the
      --   site has off, which it does not publish; set with the switches.
    , siteWithheld :: [String]
      -- | site.yaml's links (nav-primary, the library's see-also) to a page a
      --   switch the site has off publishes (its registry @routes@), each
      --   with that switch; set with the switches. The build refuses one
      --   that no other item builds ('RouteCheck.withCheckedRoutes'): a tag,
      --   a page collection or a standalone page may take the path.
    , siteSwitchedLinks :: [(String, String)]
    } deriving (Eq, Show)

-- | A seasonal dressing over one of the themes (site.yaml's @seasons@).
--   While its window is open, @from@ to @to@ inclusive (month-day; a window
--   whose @from@ is later than its @to@ runs through the new year), a
--   reader with no stored theme wears it and Settings offers it; once the
--   window closes, a reader who chose it gets @base@. @stylesheet@, screen
--   only, holds its tokens, scoped to @[data-season]@; @script@, if any, is
--   loaded only while the season is worn and registers decorations with
--   the stage (static/js/stage.js). theme.js and settings.js read them from
--   head.html's @ozy:season@ metas.
data Season = Season
    { seasonId :: String
    , seasonLabel :: String
    , seasonBase :: String
    , seasonFrom :: String
    , seasonTo :: String
    , seasonStylesheet :: String
    , seasonScript :: Maybe String
    } deriving (Eq, Show)

-- | The themes a season can dress: theme.js's, settings.js's and base.css's.
seasonBases :: [String]
seasonBases = ["light", "dark", "cappuccino"]

instance FromJSON Season where
    parseJSON = withObject "season" $ \o -> do
        onlyKeys ["id", "label", "base", "from", "to", "stylesheet", "script"] o
        s <- Season <$> o .: "id" <*> o .: "label" <*> o .: "base"
                    <*> o .: "from" <*> o .: "to" <*> o .: "stylesheet" <*> o .:? "script"
        unless (slug (seasonId s)) $
            fail ("season id must be lowercase letters, digits and hyphens: " ++ seasonId s)
        when (seasonId s `elem` seasonBases) $
            fail ("season id names a theme: " ++ seasonId s)
        singleLine "season label" (seasonLabel s)
        unless (seasonBase s `elem` seasonBases) $
            fail ("season base must be one of " ++ intercalate ", " seasonBases ++ ": " ++ seasonBase s)
        forM_ [("from", seasonFrom s), ("to", seasonTo s)] $ \(key, day) ->
            unless (monthDay day) $
                fail ("season " ++ key ++ " must be a day of the year as MM-DD: " ++ day)
        unless (siteAsset ".css" (seasonStylesheet s) && "/css/" `isPrefixOf` seasonStylesheet s) $
            fail ("season stylesheet must be the site's .css path under /css/: " ++ seasonStylesheet s)
        forM_ (seasonScript s) $ \path -> unless (siteAsset ".js" path && "/js/" `isPrefixOf` path) $
            fail ("season script must be the site's .js path under /js/: " ++ path)
        pure s

instance ToJSON Season where
    toJSON s = object
        [ "id" .= seasonId s, "label" .= seasonLabel s, "base" .= seasonBase s
        , "from" .= seasonFrom s, "to" .= seasonTo s
        , "stylesheet" .= seasonStylesheet s, "script" .= seasonScript s ]

-- | MM-DD naming a day some year has (02-29 included).
monthDay :: String -> Bool
monthDay value = case value of
    [m1, m2, '-', d1, d2] | all (`elem` ['0'..'9']) [m1, m2, d1, d2] ->
        let month = read [m1, m2] :: Int
            day = read [d1, d2] :: Int
        in month >= 1 && month <= 12 && day >= 1
           && day <= [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !! (month - 1)
    _ -> False

-- | A root-relative path to one of the site's files with the given
--   extension, safe to put in an attribute unescaped.
siteAsset :: String -> String -> Bool
siteAsset ext path = "/" `isPrefixOf` path && ext `isSuffixOf` path
    && safePath (drop 1 path)
    && all (\ch -> not (isSpace ch || isControl ch) && ch `notElem` ("\"'<>&" :: String)) path

data FeedSection = Essays | Blog | Fiction | Poetry | Music | Photos
    deriving (Eq, Show, Enum, Bounded)

sectionName :: FeedSection -> String
sectionName s = case s of
    Essays -> "essays"
    Blog -> "blog"
    Fiction -> "fiction"
    Poetry -> "poetry"
    Music -> "music"
    Photos -> "photography"

instance FromJSON FeedSection where
    parseJSON = withText "feed section" $ \s -> case s of
        "essays" -> pure Essays
        "blog" -> pure Blog
        "fiction" -> pure Fiction
        "poetry" -> pure Poetry
        "music" -> pure Music
        "photography" -> pure Photos
        _ -> fail ("unknown feed section: " ++ T.unpack s)

instance ToJSON FeedSection where
    toJSON = toJSON . sectionName

data FeedSpec = FeedSpec
    { feedPath :: FilePath
    , feedName :: String
    , feedSummary :: String
    , feedSections :: [FeedSection]
    , feedAdvertiseTitle :: String
    , feedAdvertise :: Bool
    , feedLimit :: Maybe Int
    } deriving (Eq, Show)

instance FromJSON FeedSpec where
    parseJSON = withObject "feed" $ \o -> do
        onlyKeys ["path", "title", "description", "sections", "advertise-title", "advertise", "limit"] o
        title <- o .: "title"
        f <- FeedSpec <$> o .: "path" <*> pure title <*> o .: "description"
                      <*> o .: "sections" <*> o .:? "advertise-title" .!= title
                      <*> o .:? "advertise" .!= True
                      <*> o .:? "limit"
        mapM_ (uncurry singleLine)
            [("feed path", feedPath f), ("feed title", feedName f),
             ("feed description", feedSummary f), ("feed advertise-title", feedAdvertiseTitle f)]
        unless (safePath (feedPath f) && ".xml" `isSuffixOf` feedPath f) $
            fail "feed path must be a relative .xml path without traversal"
        when (null (feedSections f) || nub (feedSections f) /= feedSections f) $
            fail "feed sections must be nonempty and unique"
        when (Photos `elem` feedSections f && not (photoFeed f)) $
            fail "photography feeds must contain only the photography section"
        when (maybe False (<= 0) (feedLimit f)) $
            fail "feed limit must be positive, or null for all entries"
        pure f

instance ToJSON FeedSpec where
    toJSON f = object
        [ "path" .= feedPath f, "title" .= feedName f
        , "description" .= feedSummary f, "sections" .= feedSections f
        , "advertise-title" .= feedAdvertiseTitle f
        , "advertise" .= feedAdvertise f, "limit" .= feedLimit f ]

-- | Identity in the web-app manifest. Icon routes and the application shell
-- live in its overridable template; text values are JSON-encoded at rendering.
data ManifestConfig = ManifestConfig
    { manifestName :: String
    , manifestShortName :: String
    , manifestDescription :: String
    , manifestThemeColor :: String
    , manifestBackgroundColor :: String
    } deriving (Eq, Show)

instance FromJSON ManifestConfig where
    parseJSON = withObject "manifest" $ \o -> do
        onlyKeys ["name", "short-name", "description", "theme-color", "background-color"] o
        m <- ManifestConfig <$> o .: "name" <*> o .: "short-name" <*> o .: "description"
                            <*> o .: "theme-color" <*> o .: "background-color"
        mapM_ (uncurry singleLine) (manifestValues m)
        pure m

manifestValues :: ManifestConfig -> [(String, String)]
manifestValues m = [("name", manifestName m), ("short-name", manifestShortName m),
                    ("description", manifestDescription m), ("theme-color", manifestThemeColor m),
                    ("background-color", manifestBackgroundColor m)]

instance ToJSON ManifestConfig where
    toJSON m = object [Key.fromString k .= v | (k, v) <- manifestValues m]

-- | A content section, keyed by the route its pages take (@<dir>/…@), with
--   a landing at @<dir>/index.html@. Its kind labels its pages
--   (@$item-kind$@); its dingbat is their default ornament. The reserved
--   content directories, the tags that may not have pages and the wikilink
--   directory slugs derive from these, the portals and the standalone pages
--   ("Sections").
data Section = Section
    { sectionDir     :: String        -- ^ its pages: @<dir>/…@
    , sectionKind    :: String        -- ^ @$item-kind$@
    , sectionDingbat :: Maybe String  -- ^ default ornament; 'Nothing' takes the fallback
    } deriving (Eq, Show)

-- The current rules own these section namespaces. This is a routing contract,
-- not another set of display defaults: names come from the engine's section
-- enumeration, while labels and ornaments remain site-owned. A section a
-- switch owns (its registry @section@) is routed only with the switch on
-- ('switchedSections').
sectionRoutes :: [String]
sectionRoutes = [sectionName s | s <- [minBound .. maxBound]]

instance FromJSON Section where
    parseJSON = withObject "section" $ \o -> do
        onlyKeys ["dir", "kind", "dingbat"] o
        s <- Section <$> o .: "dir" <*> o .: "kind" <*> o .:? "dingbat"
        unless (slug (sectionDir s)) $ fail "section dir must be a lowercase slug"
        unless (sectionDir s `elem` sectionRoutes) $
            fail ("unsupported section directory: " ++ sectionDir s)
        singleLine "section kind" (sectionKind s)
        forM_ (sectionDingbat s) $ \d -> unless (d `elem` knownDingbats) $
            fail ("unknown dingbat " ++ d ++ "; known: " ++ unwords knownDingbats)
        pure s

instance ToJSON Section where
    toJSON s = object [ "dir" .= sectionDir s, "kind" .= sectionKind s
                      , "dingbat" .= sectionDingbat s ]

-- | A home-page portal: a top-level tag whose page is @/<tag>/@, in
--   home-page order. The navigation lists them alphabetically.
data Portal = Portal
    { portalName :: String
    , portalTag  :: String
    } deriving (Eq, Show)

instance FromJSON Portal where
    parseJSON = withObject "portal" $ \o -> do
        onlyKeys ["name", "tag"] o
        p <- Portal <$> o .: "name" <*> o .: "tag"
        singleLine "portal name" (portalName p)
        unless (slug (portalTag p)) $ fail "portal tag must be a lowercase slug"
        pure p

instance ToJSON Portal where
    toJSON p = object ["name" .= portalName p, "tag" .= portalTag p]

-- | A labelled link in the navigation or among the library's see-also
--   links: root-relative, or an absolute HTTP(S) URL.
data NavLink = NavLink
    { navLabel :: String
    , navHref  :: String
    } deriving (Eq, Show)

instance FromJSON NavLink where
    parseJSON = withObject "link" $ \o -> do
        onlyKeys ["label", "href"] o
        l <- NavLink <$> o .: "label" <*> o .: "href"
        singleLine "link label" (navLabel l)
        let href = navHref l
            rootRelative = "/" `isPrefixOf` href && not ("//" `isPrefixOf` href)
        unless ((rootRelative || absoluteUrl href) && not (any isSpace href)) $
            fail "link href must be root-relative or an absolute HTTP(S) URL"
        pure l

instance ToJSON NavLink where
    toJSON l = object ["label" .= navLabel l, "href" .= navHref l]

-- | The library page: shelf sizes (a shelf shows the cap, raised by its
--   sidecar's @featured:@ list up to the maximum), the page's description
--   and its see-also links.
data LibraryConfig = LibraryConfig
    { libraryShelfCap     :: Int
    , libraryShelfMax     :: Int
    , libraryDescription  :: String
    , librarySeeAlso      :: [NavLink]
    } deriving (Eq, Show)

instance FromJSON LibraryConfig where
    parseJSON = withObject "library" $ \o -> do
        onlyKeys ["shelf-cap", "shelf-max", "description", "see-also"] o
        l <- LibraryConfig <$> o .: "shelf-cap" <*> o .: "shelf-max"
                           <*> o .: "description" <*> o .: "see-also"
        unless (1 <= libraryShelfCap l && libraryShelfCap l <= libraryShelfMax l) $
            fail "library shelf-cap must be at least 1 and at most shelf-max"
        singleLine "library description" (libraryDescription l)
        pure l

instance ToJSON LibraryConfig where
    toJSON l = object [ "shelf-cap" .= libraryShelfCap l, "shelf-max" .= libraryShelfMax l
                      , "description" .= libraryDescription l, "see-also" .= librarySeeAlso l ]

-- | The engine context a standalone page is compiled with: its compiler and
--   the fields its template reads ("Site").
data PageKind = EssayPage | PlainPage | NowPage | VitaPage | ProjectsPage
    deriving (Eq, Show, Enum, Bounded)

pageKindName :: PageKind -> String
pageKindName k = case k of
    EssayPage    -> "essay"     -- the essay pipeline: TOC, citations, sidenotes, scores
    PlainPage    -> "page"
    NowPage      -> "now"       -- data/now.yaml
    VitaPage     -> "vita"      -- yaml-source/data
    ProjectsPage -> "projects"  -- yaml-source/data/projects.yml

instance FromJSON PageKind where
    parseJSON = withText "page context" $ \t ->
        case lookup (T.unpack t) [(pageKindName k, k) | k <- [minBound .. maxBound]] of
            Just k  -> pure k
            Nothing -> fail ("unknown page context " ++ T.unpack t ++ "; known: "
                             ++ unwords (map pageKindName [minBound .. maxBound]))

instance ToJSON PageKind where
    toJSON = toJSON . pageKindName

-- | A one-off page: its source, the route it takes, its template (under
--   @templates/@) and the engine context it is compiled with. @scores@
--   copies and tracks the score fragments beside a source in its own
--   directory (@content/<dir>/scores/*.svg@, served at @/scores/@);
--   @dingbat@ is its default ornament; each flag is a template boolean
--   (@$if(<flag>)$@).
data Standalone = Standalone
    { standaloneSource   :: FilePath
    , standaloneRoute    :: FilePath
    , standaloneTemplate :: FilePath
    , standaloneContext  :: PageKind
    , standaloneScores   :: Bool
    , standaloneDingbat  :: Maybe String
    , standaloneFlags    :: [String]
    } deriving (Eq, Show)

instance FromJSON Standalone where
    parseJSON = withObject "standalone page" $ \o -> do
        onlyKeys ["source", "route", "template", "context", "scores", "dingbat", "flags"] o
        p <- Standalone <$> o .: "source" <*> o .: "route" <*> o .: "template"
                        <*> o .: "context" <*> o .:? "scores" .!= False
                        <*> o .:? "dingbat" <*> o .:? "flags" .!= []
        let source = standaloneSource p
        unless (safePath source && ".md" `isSuffixOf` source && '*' `notElem` source
                && take 1 (splitDirectories source) == ["content"]
                && length (splitDirectories source) `elem` [2, 3]) $
            fail "standalone source must be content/<page>.md or content/<dir>/<page>.md"
        when (source `elem` engineSources) $
            fail ("standalone source is one of the engine's own pages: " ++ source)
        forM_ [("route", standaloneRoute p), ("template", standaloneTemplate p)] $ \(name, path) ->
            unless (safePath path && ".html" `isSuffixOf` path) $
                fail ("standalone " ++ name ++ " must be a relative .html path without traversal")
        when (standaloneScores p && isNothing (standaloneDir p)) $
            fail "standalone scores need a source in its own directory"
        forM_ (standaloneDingbat p) $ \d -> unless (d `elem` knownDingbats) $
            fail ("unknown dingbat " ++ d ++ "; known: " ++ unwords knownDingbats)
        unless (all slug (standaloneFlags p)) $ fail "standalone flags must be lowercase slugs"
        pure p

instance ToJSON Standalone where
    toJSON p = object
        [ "source" .= standaloneSource p, "route" .= standaloneRoute p
        , "template" .= standaloneTemplate p, "context" .= standaloneContext p
        , "scores" .= standaloneScores p, "dingbat" .= standaloneDingbat p
        , "flags" .= standaloneFlags p ]

-- | The directory a standalone page's source sits in under @content/@,
--   if it has one of its own: @me@ for @content/me/index.md@.
standaloneDir :: Standalone -> Maybe String
standaloneDir p = case splitDirectories (standaloneSource p) of
    ["content", dir, _] -> Just dir
    _                   -> Nothing

-- | Top-level pages the engine compiles with rules of its own: the home
--   page, the library's intro and the commonplace book.
engineSources :: [FilePath]
engineSources = ["content/index.md", "content/library.md", "content/commonplace.md"]

-- | Directories under @content/@ the engine reads for itself: the tag and
--   bibliography sidecars, drafts, and per-page scripts.
engineContentDirs :: [String]
engineContentDirs = ["bibliography-meta", "drafts", "scripts", "tag-meta"]

-- | The ornaments the stylesheet draws (@body[data-dingbat="…"]@ in
--   typography.css), for a page's @dingbat:@ and a section's default.
--   Extend here when adding one ("Dingbat").
knownDingbats :: [String]
knownDingbats =
    [ "asterism"    -- ⁂ typographic asterism (neutral fallback)
    , "asterisks"   -- * * * spaced asterisks (classic scene break)
    , "fleuron"     -- Aldine leaf (literary essays)
    , "trefoil"     -- three-lobed ornament (poetry)
    , "lozenge"     -- diamond/rhombus (blog)
    , "clef"        -- musical ornament (music)
    , "memento"     -- mourning ornament (memento-mori)
    , "tech"        -- tech ornament
    , "ai"          -- AI ornament (the cute robot)
    ]

instance FromJSON SiteConfig where
    parseJSON = withObject "site config" $ \o -> do
        onlyKeys (map Key.fromString identityKeys
                  ++ ["feeds", "manifest", "signing-fingerprint", "sections", "portals",
                      "nav-primary", "library", "standalone", "font-preloads", "epistemic-about",
                      "seasons", "switches"]) o
        c <- SiteConfig <$> o .: "title" <*> o .: "author" <*> o .: "author-sort"
                        <*> o .: "email" <*> o .: "contact-email"
                        <*> o .: "url" <*> o .: "host"
                        <*> o .: "source-repo" <*> o .:? "source-branch" .!= "main"
                        <*> o .: "forge-host" <*> o .: "content-license"
                        <*> o .: "code-license" <*> o .:? "signing-fingerprint" .!= ""
                        <*> o .: "manifest" <*> o .: "feeds"
                        <*> o .: "sections" <*> o .: "portals"
                        <*> o .: "nav-primary" <*> o .:? "library"
                        <*> o .: "standalone" <*> o .:? "font-preloads" .!= []
                        <*> o .:? "epistemic-about" .!= ""
                        <*> o .:? "seasons" .!= []
                        <*> o .:? "switches" .!= Map.empty <*> pure [] <*> pure []
        mapM_ (uncurry singleLine) (identityValues c)
        forM_ (siteFontPreloads c) $ \href ->
            unless (siteAsset ".woff2" href) $
                fail ("font-preloads must be the site's .woff2 paths: " ++ href)
        let about = siteEpistemicAbout c
        unless (null about || ("/" `isPrefixOf` about
                               && all (\ch -> not (isSpace ch || isControl ch) && ch `notElem` ("\"'<>&" :: String)) about)) $
            fail ("epistemic-about must be a path on the site: " ++ about)
        let seasonIds = map seasonId (siteSeasons c)
        when (nub seasonIds /= seasonIds) $ fail "season ids must be unique"
        when (any isControl (siteSigningFingerprint c)) $
            fail "signing-fingerprint must be a single-line string"
        mapM_ (\(name, value) -> unless (absoluteUrl value && not ("/" `isSuffixOf` value)) $
            fail (name ++ " must be an absolute HTTP(S) URL without a trailing slash"))
            [("url", siteUrl c), ("source-repo", siteSourceRepo c)]
        mapM_ (\value -> unless (all (`elem` (['a'..'z'] ++ ['0'..'9'] ++ ".-")) value) $
            fail "host and forge-host must be lowercase hostnames") [siteHost c, siteForgeHost c]
        -- Links to the host, with or without www., are this site's own; a
        -- host that disagrees with url would mark the site's links external.
        when (null (bareHost c)) $ fail "host must name a domain, not just www."
        unless (urlHost (siteUrl c) `elem` siteOwnHosts c) $
            fail "url's hostname must be host, with or without a leading www."
        mapM_ (\value -> unless (all (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-._")) value) $
            fail "license identifiers must be safe file names") [siteContentLicense c, siteCodeLicense c]
        unless (safePath (siteSourceBranch c)) $ fail "source-branch must be a relative path without traversal"
        let paths = map feedPath (siteFeeds c)
        when (nub paths /= paths) $ fail "feed paths must be unique"
        let dirs = map sectionDir (siteSections c)
            tags = map portalTag (sitePortals c)
        when (nub dirs /= dirs) $ fail "section dirs must be unique"
        when (nub tags /= tags) $ fail "portal tags must be unique"
        let sources = map standaloneSource (siteStandalone c)
            routes  = map standaloneRoute (siteStandalone c)
        when (nub sources /= sources || nub routes /= routes) $
            fail "standalone sources and routes must be unique"
        forM_ (siteStandalone c) $ \p -> forM_ (standaloneDir p) $ \dir ->
            when (dir `elem` dirs ++ engineContentDirs) $
                fail ("standalone source is inside a section or engine directory: " ++ standaloneSource p)
        pure c

instance ToJSON SiteConfig where
    toJSON c = object ([Key.fromString k .= v | (k, v) <- identityValues c]
                       ++ ["feeds" .= siteFeeds c, "manifest" .= siteManifest c,
                           "signing-fingerprint" .= siteSigningFingerprint c,
                           "sections" .= siteSections c, "portals" .= sitePortals c,
                           "nav-primary" .= siteNavPrimary c, "library" .= siteLibrary c,
                           "standalone" .= siteStandalone c,
                           "font-preloads" .= siteFontPreloads c,
                           "epistemic-about" .= siteEpistemicAbout c,
                           "seasons" .= siteSeasons c,
                           "switches" .= siteSwitches c])

identityKeys :: [String]
identityKeys = ["title", "author", "author-sort", "email", "contact-email", "url", "host",
                "source-repo", "source-branch", "forge-host", "content-license", "code-license"]

identityValues :: SiteConfig -> [(String, String)]
identityValues c = zip identityKeys
    [siteTitle c, siteAuthor c, siteAuthorSort c, siteEmail c, siteContactEmail c,
     siteUrl c, siteHost c, siteSourceRepo c, siteSourceBranch c, siteForgeHost c,
     siteContentLicense c, siteCodeLicense c]

onlyKeys :: [Key.Key] -> Object -> Parser ()
onlyKeys allowed o = unless (null unknown) $
    fail ("unknown configuration keys: " ++ intercalate ", " (map Key.toString unknown))
  where unknown = filter (`notElem` allowed) (KM.keys o)

singleLine :: String -> String -> Parser ()
singleLine name value = when (null value || any isControl value) $
    fail (name ++ " must be a nonempty single-line string")

safePath :: FilePath -> Bool
safePath path = not (null path) && not ("/" `isPrefixOf` path)
    && all (`notElem` [".", "..", ""]) (splitDirectories path)
    && not (any (`elem` ("\\?#:" :: String)) path)

-- | A lowercase name for a URL path segment or a content directory.
slug :: String -> Bool
slug value = case value of
    c : _ -> c /= '-' && all (`elem` (['a'..'z'] ++ ['0'..'9'] ++ "-")) value
    []    -> False

absoluteUrl :: String -> Bool
absoluteUrl value = any (`isPrefixOf` value) ["https://", "http://"]
    && not (any (`elem` (" \t\r\n" :: String)) value)

-- | The lowercased hostname of an absolute HTTP(S) URL, without its
--   scheme, userinfo, port and path.
urlHost :: String -> String
urlHost url = map toLower (takeWhile (/= ':') (afterUserinfo authority))
  where
    authority = takeWhile (/= '/') (drop 3 (dropWhile (/= ':') url))
    afterUserinfo a = case break (== '@') a of
        (_, '@' : rest) -> rest
        _ -> a

-- | The hostnames that are this site's own: the configured host with and
--   without a leading @www.@, whichever of the two site.yaml names. Link
--   classification and the url check share this one definition.
siteOwnHosts :: SiteConfig -> [String]
siteOwnHosts c = [bareHost c, "www." ++ bareHost c]

bareHost :: SiteConfig -> String
bareHost c = fromMaybe (siteHost c) (stripPrefix "www." (siteHost c))

photoFeed :: FeedSpec -> Bool
photoFeed f = feedSections f == [Photos]

-- | The registry, then site.yaml: the site's config with its switches
--   resolved ('resolveSwitches').
decodeConfig :: BS.ByteString -> BS.ByteString -> Either String SiteConfig
decodeConfig registryBytes siteBytes = do
    registry <- decode registryPath registryBytes
    cfg      <- decode "site.yaml" siteBytes
    resolved <- first ("site.yaml: " ++) (resolveSwitches registry (siteSwitches cfg))
    first ("site.yaml: " ++) (switchedSections registry resolved cfg)
    first ("site.yaml: " ++) (switchedContexts registry resolved cfg)
    let withheld = [ glob | (name, spec) <- Map.toList registry
                          , not (Map.findWithDefault False name resolved), glob <- switchAssets spec ]
    when (Map.findWithDefault False "signing" resolved && null (siteSigningFingerprint cfg)) $
        Left "site.yaml: the signing switch needs a signing-fingerprint"
    when (Map.findWithDefault False "library" resolved && isNothing (siteLibrary cfg)) $
        Left "site.yaml: the library switch needs a library block"
    let ornamented = [ sectionDir s | s <- siteSections cfg, isJust (sectionDingbat s) ]
                  ++ [ standaloneSource p | p <- siteStandalone cfg, isJust (standaloneDingbat p) ]
    unless (null ornamented || Map.findWithDefault False "dingbats" resolved) $
        Left ("site.yaml: a default ornament needs the dingbats switch: " ++ intercalate ", " ornamented)
    let scored = [ standaloneSource p | p <- siteStandalone cfg, standaloneScores p ]
    unless (null scored || Map.findWithDefault False "music" resolved) $
        Left ("site.yaml: standalone scores need the music switch: " ++ intercalate ", " scored)
    pure cfg { siteSwitches = resolved, siteWithheld = withheld
             , siteSwitchedLinks = switchedLinks registry resolved cfg }
  where
    decode :: FromJSON a => FilePath -> BS.ByteString -> Either String a
    decode path = first ((path ++ ": ") ++) . first Yaml.prettyPrintParseException . Yaml.decodeEither'

readSiteConfig :: IO SiteConfig
readSiteConfig = do
    registry <- BS.readFile registryPath
    site     <- BS.readFile "site.yaml"
    either fail pure (decodeConfig registry site)

configRules :: Rules ()
configRules = match ("site.yaml" .||. fromGlob registryPath) $ compile getResourceBody

loadConfig :: Compiler SiteConfig
loadConfig = do
    registry <- loadBody (fromFilePath registryPath)
    site     <- loadBody "site.yaml"
    either fail pure (decodeConfig (utf8 registry) (utf8 site))
  where utf8 = TE.encodeUtf8 . T.pack

-- ---------------------------------------------------------------------------
-- Switches
-- ---------------------------------------------------------------------------

-- | The engine's switch registry, read from the site's root as the build
--   runs there (tools/sitelib.py reads the same file).
registryPath :: FilePath
registryPath = "engine/switches.yaml"

-- | One switch of the registry: its default, what it needs, the section
--   and standalone-page contexts it owns, its routes and its assets; its
--   kind and description are checked, not kept.
data SwitchSpec = SwitchSpec
    { switchDefault  :: Bool
    , switchNeeds    :: [String]
    , switchNeedsAny :: [String]
    , switchSection  :: Maybe String
    , switchContexts :: [String]
    , switchRoutes   :: [String]
    , switchAssets   :: [String]
    }

instance FromJSON SwitchSpec where
    parseJSON = withObject "switch" $ \o -> do
        onlyKeys ["kind", "default", "needs", "needs-any", "section", "contexts", "routes", "assets", "about"] o
        kind <- o .: "kind"
        unless (kind `elem` ["capability", "generated", "interface" :: String]) $
            fail ("a switch's kind is capability, generated or interface, not " ++ kind)
        _ <- o .: "about" :: Parser String
        spec <- SwitchSpec <$> o .: "default" <*> o .:? "needs" .!= [] <*> o .:? "needs-any" .!= []
                           <*> o .:? "section" <*> o .:? "contexts" .!= [] <*> o .:? "routes" .!= []
                           <*> o .:? "assets" .!= []
        forM_ (switchSection spec) $ \dir -> unless (dir `elem` sectionRoutes) $
            fail ("a switch's section is one the engine routes, not " ++ dir)
        forM_ (switchContexts spec) $ \name -> unless (name `elem` map pageKindName [minBound .. maxBound]) $
            fail ("a switch's contexts are standalone-page contexts, not " ++ name)
        forM_ (switchRoutes spec) $ \path -> unless ("/" `isPrefixOf` path && safePath (drop 1 path)) $
            fail ("a switch's routes are paths on the site, not " ++ path)
        forM_ (switchAssets spec) $ \glob -> unless (safePath glob) $
            fail ("a switch's assets are paths under static/, not " ++ glob)
        pure spec

-- | The site's switches: those its site.yaml sets, over the registry's
--   defaults. A switch the registry lacks, or one on without what it
--   needs (all of @needs@, one of @needs-any@), is refused here, and only
--   here: the tools resolve without checking.
resolveSwitches :: Map.Map String SwitchSpec -> Map.Map String Bool -> Either String (Map.Map String Bool)
resolveSwitches registry set = do
    let dangling = [ name ++ " needs " ++ other
                   | (name, spec) <- Map.toList registry
                   , other <- switchNeeds spec ++ switchNeedsAny spec
                   , not (Map.member other registry) ]
    unless (null dangling) $
        Left (registryPath ++ ": no such switch: " ++ intercalate "; " dangling)
    let unknown = Map.keys (Map.difference set registry)
    unless (null unknown) $
        Left ("switches: not in " ++ registryPath ++ ": " ++ intercalate ", " unknown)
    let resolved = Map.union set (Map.map switchDefault registry)
        on name  = Map.findWithDefault False name resolved
    forM_ (Map.toList registry) $ \(name, spec) -> when (on name) $ do
        let missing = filter (not . on) (switchNeeds spec)
        unless (null missing) $
            Left ("switches: " ++ name ++ " needs " ++ intercalate ", " missing)
        unless (null (switchNeedsAny spec) || any on (switchNeedsAny spec)) $
            Left ("switches: " ++ name ++ " needs one of " ++ intercalate ", " (switchNeedsAny spec))
    pure resolved

-- | The sections the engine routes, as the switches have them: a section a
--   switch owns is the site's only with that switch on. Then site.yaml
--   must list it, like any routed section; with it off, neither a section
--   entry nor a feed may name it.
switchedSections :: Map.Map String SwitchSpec -> Map.Map String Bool -> SiteConfig -> Either String ()
switchedSections registry on cfg = do
    let owners = Map.fromList [ (dir, name) | (name, spec) <- Map.toList registry
                                            , Just dir <- [switchSection spec] ]
        offSwitch dir = mfilter (not . flip (Map.findWithDefault False) on) (Map.lookup dir owners)
        dirs    = map sectionDir (siteSections cfg)
        missing = [ dir | dir <- sectionRoutes, isNothing (offSwitch dir), dir `notElem` dirs ]
        named   = nub (dirs ++ [ sectionName s | f <- siteFeeds cfg, s <- feedSections f ])
        refused = [ dir ++ " (the " ++ name ++ " switch is off)"
                  | dir <- named, Just name <- [offSwitch dir] ]
    unless (null missing) $
        Left ("sections must include the engine's routed sections: " ++ intercalate ", " missing)
    unless (null refused) $
        Left ("sections and feeds name a section the engine does not route: " ++ intercalate ", " refused)

-- | A standalone page's context a switch owns (its registry @contexts@) is
--   the site's only with that switch on.
switchedContexts :: Map.Map String SwitchSpec -> Map.Map String Bool -> SiteConfig -> Either String ()
switchedContexts registry on cfg = do
    let owners  = Map.fromList [ (context, name) | (name, spec) <- Map.toList registry
                                                 , context <- switchContexts spec ]
        refused = [ standaloneSource p ++ " (context " ++ context ++ ": the " ++ name ++ " switch is off)"
                  | p <- siteStandalone cfg
                  , let context = pageKindName (standaloneContext p)
                  , Just name <- [Map.lookup context owners]
                  , not (Map.findWithDefault False name on) ]
    unless (null refused) $
        Left ("standalone pages need their context's switch: " ++ intercalate ", " refused)

-- | site.yaml's links (the navigation, the library's see-also) to a page a
--   switch the site has off publishes (its registry @routes@; a fragment
--   or query aside), each with that switch: 'siteSwitchedLinks'.
switchedLinks :: Map.Map String SwitchSpec -> Map.Map String Bool -> SiteConfig -> [(String, String)]
switchedLinks registry on cfg =
    [ (navHref l, name)
    | l <- siteNavPrimary cfg ++ maybe [] librarySeeAlso (siteLibrary cfg)
    , Just name <- [Map.lookup (takeWhile (`notElem` ("#?" :: String)) (navHref l)) owners]
    , not (Map.findWithDefault False name on) ]
  where
    owners = Map.fromList [ (path, name) | (name, spec) <- Map.toList registry
                                         , path <- switchRoutes spec ]

-- | Whether the site has switch @name@ on. A name the registry lacks is the
--   engine's own mistake, which stops the build. The one way to ask: rules
--   with the config they read in 'preprocess', compilers with
--   'loadConfig'; only a module that already reads its files once per
--   process (ArchiveIndex) reads it so too.
switchOn :: SiteConfig -> String -> Bool
switchOn cfg name = fromMaybe (error ("switchOn: " ++ name ++ " is not in " ++ registryPath))
                              (Map.lookup name (siteSwitches cfg))

-- | @$switch-NAME$@, present when the site has switch NAME on: how a
--   template asks, where rules and compilers ask 'switchOn'. A name the
--   registry lacks stops the build, as it does there; in a context without
--   this one, every switch would read as off.
switchContext :: Context a
switchContext = Context $ \key _ _ -> case stripPrefix "switch-" key of
    Nothing   -> noResult ("not a switch: " ++ key)
    Just name -> do
        cfg <- loadConfig
        case Map.lookup name (siteSwitches cfg) of
            Nothing    -> fail ("a template asks for " ++ key ++ ": " ++ name ++ " is not in " ++ registryPath)
            Just True  -> pure (StringField "true")
            Just False -> noResult ("the " ++ name ++ " switch is off")

loadFeed :: FilePath -> Compiler (SiteConfig, FeedSpec)
loadFeed path = do
    cfg <- loadConfig
    case find ((== path) . feedPath) (siteFeeds cfg) of
        Just feed -> pure (cfg, feed)
        Nothing -> fail ("site.yaml: feed removed; rebuild rules: " ++ path)

configField :: String -> (SiteConfig -> String) -> Context a
configField name get = field name $ \_ -> get <$> loadConfig

-- | HTML-facing identity, escaped once at the output boundary. Keep the
-- underlying config raw for non-HTML consumers (JSON, filters, tools).
identityContext :: Context String
identityContext = mconcat [context | (_, _, context) <- identityFields]

-- | 'identityContext''s fields, by name, with a list's items' field names:
-- @site identity-fields@ prints the names, which the override contract's
-- identity group (engine/contract.yaml) must match.
identityFields :: [(String, [String], Context String)]
identityFields =
    [ (name, [], configField name (escapeHtml . get))
    | (name, get) <- [("site-title", siteTitle), ("site-url", siteUrl),
                     ("site-host", siteHost), ("site-email", siteEmail),
                     ("site-contact-email", siteContactEmail), ("site-source-repo", siteSourceRepo),
                     ("site-code-license", siteCodeLicense),
                     ("site-signing-fingerprint", siteSigningFingerprint)] ]
    ++ [ list "site-feeds"
              [ ("feed-title", pure . escapeHtml . feedAdvertiseTitle . itemBody)
              , ("feed-url", pure . escapeHtml . ("/" ++) . feedPath . itemBody) ]
              (loadConfig >>= mapM makeItem . filter feedAdvertise . siteFeeds)
       , list "font-preloads" [("href", pure . itemBody)]
              (loadConfig >>= mapM makeItem . siteFontPreloads) ]
  where
    list name fields items =
        (name, map fst fields, listField name (mconcat [field n f | (n, f) <- fields]) items)

-- | The navigation the templates loop over, escaped like 'identityContext':
--   @nav-primary@ (@$label$@, @$href$@) and @nav-portals@ (@$portal-name$@,
--   @$portal-url$@), the portals alphabetically by name.
navigationContext :: Context String
navigationContext =
       listField "nav-primary" linkContext (configItems siteNavPrimary)
    <> listField "nav-portals" portalContext
           (configItems (sortOn (map toLower . portalName) . sitePortals))
  where
    configItems get = loadConfig >>= mapM makeItem . get
    linkContext = field "label" (pure . escapeHtml . navLabel . itemBody)
               <> field "href" (pure . escapeHtml . navHref . itemBody)
    portalContext = field "portal-name" (pure . escapeHtml . portalName . itemBody)
                 <> field "portal-url" (pure . escapeHtml . (\t -> "/" ++ t ++ "/") . portalTag . itemBody)

-- | site.yaml's seasons, escaped like 'identityContext', for head.html's
--   metas and stylesheets and the Settings buttons: @$for(seasons)$@ with
--   @$season-id$@, @$season-label$@, @$season-base$@, @$season-from$@,
--   @$season-to$@, @$season-stylesheet$@ and @$season-script$@ (absent when
--   the season has none, so @$if(season-script)$@ asks).
seasonContext :: Context String
seasonContext = listField "seasons" fields (loadConfig >>= mapM makeItem . siteSeasons)
  where
    -- A loop's context is its own: the season's paths need the version too.
    fields = assetsField <> mconcat
        [ field name (pure . escapeHtml . get . itemBody)
        | (name, get) <- [ ("season-id", seasonId), ("season-label", seasonLabel)
                         , ("season-base", seasonBase), ("season-from", seasonFrom)
                         , ("season-to", seasonTo), ("season-stylesheet", seasonStylesheet) ] ]
        <> field "season-script"
               (maybe (noResult "season has no script") (pure . escapeHtml) . seasonScript . itemBody)

manifestRules :: Rules ()
manifestRules = create ["site.webmanifest"] $ do
    route idRoute
    compile $ do
        cfg <- loadConfig
        let jsonString = T.unpack . TE.decodeUtf8 . LBS.toStrict . encode
            ctx = mconcat [constField name (jsonString value)
                          | (name, value) <- manifestValues (siteManifest cfg)]
        makeItem ("" :: String) >>= loadAndApplyTemplate "templates/site.webmanifest" ctx

-- | Hakyll interpolates these into its feed templates as they are ("field
--   values are not escaped", @Hakyll.Web.Feed@), so a title such as
--   @Art & photography@ would make the feed invalid XML. Each is escaped
--   here, at the one place configuration meets the feed.
atomConfig :: SiteConfig -> FeedSpec -> FeedConfiguration
atomConfig c f = FeedConfiguration
    { feedTitle = escapeHtml (feedName f), feedDescription = escapeHtml (feedSummary f)
    , feedAuthorName = escapeHtml (siteAuthor c), feedAuthorEmail = escapeHtml (siteEmail c)
    , feedRoot = escapeHtml (siteUrl c) }

-- | Render a configured Atom feed, giving an empty one a valid @\<updated\>@.
--
--   Hakyll's 'renderAtom' derives the feed-level @updated@ from the first
--   item and falls back to the literal string @Unknown@ when the item list
--   is empty (@Hakyll.Web.Feed.renderFeed@). @Unknown@ is not an RFC 3339
--   timestamp: the feed is invalid, and the artifact gate
--   (@tools\/check-site.py@) rejects the build. A feed with no entries yet
--   is a legitimate state for any section of any site, so every empty feed
--   is kept and given the build time: honest, well formed, and it stops
--   changing the moment the first entry lands and a real date takes over.
--   A populated feed is never restamped.
renderSiteFeed :: SiteConfig -> FeedSpec -> Context String -> [Item String]
               -> Compiler (Item String)
renderSiteFeed c f ctx items = do
    feed <- renderAtom (atomConfig c f) ctx items
    if not (null items) then pure feed else do
        stamp <- unsafeCompiler $
            formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" <$> getCurrentTime
        pure (fmap (replaceFirst "<updated>Unknown</updated>"
                                 ("<updated>" ++ stamp ++ "</updated>")) feed)

-- | Replace the first occurrence of a literal substring. Returns the
--   input unchanged when the needle is absent.
replaceFirst :: String -> String -> String -> String
replaceFirst needle replacement = go
  where
    go [] = []
    go s@(c:cs) = case stripPrefix needle s of
        Just rest -> replacement ++ rest
        Nothing   -> c : go cs

-- | An includable GNU make fragment. Immediate defines keep '#' literal;
-- doubling '$' prevents make expansion, and an empty expression protects a
-- trailing backslash from consuming the newline. These are make values,
-- not shell-quoted arguments. JSON carries the complete typed config.
configMake :: SiteConfig -> String
configMake c = concatMap define
    ([("SITE_" ++ map (\x -> if x == '-' then '_' else toUpper x) k, v)
      | (k, v) <- identityValues c]
     ++ [("SITE_SIGNING_FINGERPRINT", siteSigningFingerprint c),
         ("SITE_FEEDS", unwords (map feedPath (siteFeeds c))),
         ("SITE_CONFIG_JSON", T.unpack (TE.decodeUtf8 (LBS.toStrict (encode c))))])
  where
    define (name, value) = "define " ++ name ++ " :=\n"
        ++ concatMap (\x -> if x == '$' then "$$" else [x]) value
        ++ "$(subst ,,)\nendef\n"
