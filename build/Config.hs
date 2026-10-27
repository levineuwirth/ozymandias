{-# LANGUAGE OverloadedStrings #-}
-- | Site-owned identity and feeds. Rule construction reads this explicitly
-- in 'preprocess'; compilers use 'loadConfig', so site.yaml is a real Hakyll
-- dependency. There is deliberately no process-global configuration.
module Config
    ( SiteConfig (..), FeedSpec (..), FeedSection (..), ManifestConfig (..)
    , Section (..), Portal (..), NavLink (..), LibraryConfig (..)
    , Standalone (..), PageKind (..), standaloneDir, engineSources, engineContentDirs
    , configRules, readSiteConfig, loadConfig, configField
    , renderSiteFeed, configMake, photoFeed, sectionName, loadFeed
    , siteOwnHosts, identityContext, navigationContext, manifestRules
    , knownDingbats
    ) where

import Control.Monad (forM_, unless, when)
import Data.Aeson (FromJSON (..), ToJSON (..), Object,
                   withObject, withText, object, (.:), (.:?), (.!=), (.=), encode)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isControl, isSpace, toLower, toUpper)
import Data.List (find, intercalate, isPrefixOf, isSuffixOf, nub, sortOn, stripPrefix)
import Data.Maybe (fromMaybe, isNothing)
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
    , siteLibrary :: LibraryConfig
    , siteStandalone :: [Standalone]
    , siteFontPreloads :: [String]
    } deriving (Eq, Show)

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
-- enumeration, while labels and ornaments remain site-owned. When rules become
-- conditional, their enabled sections must also determine this contract.
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
                      "nav-primary", "library", "standalone", "font-preloads"]) o
        c <- SiteConfig <$> o .: "title" <*> o .: "author" <*> o .: "author-sort"
                        <*> o .: "email" <*> o .: "contact-email"
                        <*> o .: "url" <*> o .: "host"
                        <*> o .: "source-repo" <*> o .:? "source-branch" .!= "main"
                        <*> o .: "forge-host" <*> o .: "content-license"
                        <*> o .: "code-license" <*> o .:? "signing-fingerprint" .!= ""
                        <*> o .: "manifest" <*> o .: "feeds"
                        <*> o .: "sections" <*> o .: "portals"
                        <*> o .: "nav-primary" <*> o .: "library"
                        <*> o .: "standalone" <*> o .:? "font-preloads" .!= []
        mapM_ (uncurry singleLine) (identityValues c)
        forM_ (siteFontPreloads c) $ \href ->
            unless ("/" `isPrefixOf` href && ".woff2" `isSuffixOf` href
                    && all (\ch -> not (isSpace ch || isControl ch) && ch `notElem` ("\"'<>&" :: String)) href) $
                fail ("font-preloads must be the site's .woff2 paths: " ++ href)
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
        let missing = filter (`notElem` dirs) sectionRoutes
        unless (null missing) $
            fail ("sections must include the engine's routed sections: " ++ intercalate ", " missing)
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
                           "font-preloads" .= siteFontPreloads c])

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

decodeConfig :: BS.ByteString -> Either String SiteConfig
decodeConfig = either (Left . ("site.yaml: " ++) . Yaml.prettyPrintParseException) Right
             . Yaml.decodeEither'

readSiteConfig :: IO SiteConfig
readSiteConfig = BS.readFile "site.yaml" >>= either fail pure . decodeConfig

configRules :: Rules ()
configRules = match "site.yaml" $ compile getResourceBody

loadConfig :: Compiler SiteConfig
loadConfig = loadBody "site.yaml" >>= either fail pure . decodeConfig . TE.encodeUtf8 . T.pack

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
identityContext = mconcat
    [ configField name (escapeHtml . get)
    | (name, get) <- [("site-title", siteTitle), ("site-url", siteUrl),
                     ("site-host", siteHost), ("site-email", siteEmail),
                     ("site-contact-email", siteContactEmail), ("site-source-repo", siteSourceRepo),
                     ("site-code-license", siteCodeLicense),
                     ("site-signing-fingerprint", siteSigningFingerprint)] ]
    <> listField "site-feeds" feedContext advertisedFeeds
    <> listField "font-preloads" (field "href" (pure . itemBody))
           (loadConfig >>= mapM makeItem . siteFontPreloads)
  where
    advertisedFeeds = do
        cfg <- loadConfig
        mapM makeItem (filter feedAdvertise (siteFeeds cfg))
    feedContext = field "feed-title" (pure . escapeHtml . feedAdvertiseTitle . itemBody)
               <> field "feed-url" (pure . escapeHtml . ("/" ++) . feedPath . itemBody)

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
