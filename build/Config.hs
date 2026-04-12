{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Site-wide configuration loaded from @site.yaml@ at the project root.
--
-- The config is exposed as top-level pure values via @unsafePerformIO@ +
-- @NOINLINE@. This is safe because:
--   1. The config is a build-time input that never changes during a build.
--   2. The static site generator is single-shot — no concurrency.
--   3. NOINLINE prevents GHC from duplicating the value.
--
-- 'Main.main' forces 'siteConfig' before Hakyll starts so a missing or
-- malformed @site.yaml@ fails loudly with a parse error rather than
-- crashing midway through a build.
module Config
    ( SiteConfig(..)
    , NavLink(..)
    , Portal(..)
    , siteConfig
    , siteHost
    , defaultAuthor
    ) where

import           Data.Aeson         (FromJSON(..), withObject, (.:), (.:?), (.!=))
import           Data.Yaml          (decodeFileThrow)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           System.IO.Unsafe   (unsafePerformIO)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data SiteConfig = SiteConfig
    { siteName        :: Text
    , siteUrl         :: Text
    , siteDescription :: Text
    , siteLanguage    :: Text
    , authorName      :: Text
    , authorEmail     :: Text
    , feedTitle       :: Text
    , feedDescription :: Text
    , license         :: Text
    , sourceUrl       :: Text
    , gpgFingerprint  :: Text
    , gpgPubkeyUrl    :: Text
    , navLinks        :: [NavLink]
    , portals         :: [Portal]
    } deriving (Show)

data NavLink = NavLink
    { navHref  :: Text
    , navLabel :: Text
    } deriving (Show)

data Portal = Portal
    { portalSlug :: Text
    , portalName :: Text
    } deriving (Show)

-- ---------------------------------------------------------------------------
-- JSON instances
-- ---------------------------------------------------------------------------

instance FromJSON SiteConfig where
    parseJSON = withObject "SiteConfig" $ \o -> SiteConfig
        <$> o .:  "site-name"
        <*> o .:  "site-url"
        <*> o .:  "site-description"
        <*> o .:? "site-language"    .!= "en"
        <*> o .:  "author-name"
        <*> o .:  "author-email"
        <*> o .:? "feed-title"       .!= ""
        <*> o .:? "feed-description" .!= ""
        <*> o .:? "license"          .!= ""
        <*> o .:? "source-url"       .!= ""
        <*> o .:? "gpg-fingerprint"  .!= ""
        <*> o .:? "gpg-pubkey-url"   .!= "/gpg/pubkey.asc"
        <*> o .:? "nav"              .!= []
        <*> o .:? "portals"          .!= []

instance FromJSON NavLink where
    parseJSON = withObject "NavLink" $ \o -> NavLink
        <$> o .: "href"
        <*> o .: "label"

instance FromJSON Portal where
    parseJSON = withObject "Portal" $ \o -> Portal
        <$> o .: "slug"
        <*> o .: "name"

-- ---------------------------------------------------------------------------
-- Global config value
-- ---------------------------------------------------------------------------

-- | Loaded from @site.yaml@ at the project root on first access.
-- @NOINLINE@ prevents GHC from duplicating the I/O. If @site.yaml@ is
-- missing or invalid, evaluation throws a parse exception.
{-# NOINLINE siteConfig #-}
siteConfig :: SiteConfig
siteConfig = unsafePerformIO (decodeFileThrow "site.yaml")

-- | The site's hostname, derived from 'siteUrl'. Used by 'Filters.Links'
-- to distinguish self-links from external links.
siteHost :: Text
siteHost = extractHost (siteUrl siteConfig)
  where
    extractHost url
        | Just rest <- T.stripPrefix "https://" url = hostOf rest
        | Just rest <- T.stripPrefix "http://"  url = hostOf rest
        | otherwise                                  = T.toLower url
    hostOf rest =
        let withPort = T.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#') rest
        in  T.toLower (T.takeWhile (/= ':') withPort)

-- | Default author name as a 'String', for Hakyll metadata APIs that use
-- 'String' rather than 'Text'.
defaultAuthor :: String
defaultAuthor = T.unpack (authorName siteConfig)
