{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | External link classification.
--
--   Walks all @Link@ inlines and:
--   * Adds @class="link-external"@ to any link whose URL starts with
--     @http://@ or @https://@ and is not on the site's own domain.
--   * Adds @data-link-icon@ / @data-link-icon-type@ attributes for
--     per-domain brand icons (see 'domainIcon' for the full list).
--   * Adds @target="_blank" rel="noopener noreferrer"@ to external links.
module Filters.Links (apply) where

import           Data.Text            (Text)
import qualified Data.Text            as T
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk     (walk)

import           Config               (siteHost)

-- | Apply link classification to the entire document.
--   Two passes: PDF links first (rewrites href to viewer URL), then external
--   link classification (operates on http/https, so no overlap).
apply :: Pandoc -> Pandoc
apply = walk classifyLink . walk classifyPdfLink

-- | Rewrite root-relative PDF links to open via the vendored PDF.js viewer.
--   Preserves the original path in @data-pdf-src@ so the popup thumbnail
--   provider can locate the corresponding @.thumb.png@ file.
--   Skips links that are already pointing at the viewer (idempotent).
--
--   Handles fragment identifiers (e.g. @/papers/foo.pdf#page=5@): the
--   fragment is stripped before the @.pdf@ suffix check and re-attached
--   after the viewer URL so PDF.js's anchor handling works.
classifyPdfLink :: Inline -> Inline
classifyPdfLink (Link (ident, classes, kvs) ils (url, title))
    | "/" `T.isPrefixOf` url
    , let (path, fragment) = T.break (== '#') url
    , ".pdf" `T.isSuffixOf` T.toLower path
    , "pdf-link" `notElem` classes =
        let viewerUrl = "/pdfjs/web/viewer.html?file="
                        <> encodeQueryValue path <> fragment
            classes'  = classes ++ ["pdf-link"]
            kvs'      = kvs ++ [("data-pdf-src", path)]
        in  Link (ident, classes', kvs') ils (viewerUrl, title)
classifyPdfLink x = x

classifyLink :: Inline -> Inline
classifyLink (Link (ident, classes, kvs) ils (url, title))
    | isExternal url =
        let icon     = domainIcon url
            classes' = classes ++ ["link-external"]
            kvs'     = kvs
                       ++ [("target",             "_blank")]
                       ++ [("rel",                "noopener noreferrer")]
                       ++ [("data-link-icon",      icon)]
                       ++ [("data-link-icon-type", "svg")]
        in  Link (ident, classes', kvs') ils (url, title)
classifyLink x = x

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | True if the URL points outside the site's domain.
--
--   Uses a strict hostname comparison rather than substring matching, so
--   that a hostile lookalike like @evil-example.com.attacker.com@ is
--   correctly classified as external (and gets @rel=noopener noreferrer@
--   plus @target=_blank@ applied).
isExternal :: Text -> Bool
isExternal url =
    case extractHost url of
        Nothing   -> False
        Just host ->
            not (host == siteHost || ("." <> siteHost) `T.isSuffixOf` host)

-- | Extract the lowercased hostname from an absolute http(s) URL.
--   Returns 'Nothing' for non-http(s) URLs (relative paths, mailto:, etc.).
extractHost :: Text -> Maybe Text
extractHost url
    | Just rest <- T.stripPrefix "https://" url = Just (hostOf rest)
    | Just rest <- T.stripPrefix "http://"  url = Just (hostOf rest)
    | otherwise                                  = Nothing
  where
    hostOf rest =
        let withPort = T.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#') rest
            host     = T.takeWhile (/= ':') withPort
        in  T.toLower host

-- | Icon name for the link, matching a file in /images/link-icons/<name>.svg.
domainIcon :: Text -> Text
domainIcon url
    -- Scholarly / reference
    | "wikipedia.org"       `T.isInfixOf` url = "wikipedia"
    | "arxiv.org"           `T.isInfixOf` url = "arxiv"
    | "doi.org"             `T.isInfixOf` url = "doi"
    | "worldcat.org"        `T.isInfixOf` url = "worldcat"
    | "orcid.org"           `T.isInfixOf` url = "orcid"
    | "archive.org"         `T.isInfixOf` url = "internet-archive"
    -- Code / software
    | "github.com"          `T.isInfixOf` url = "github"
    | "tensorflow.org"      `T.isInfixOf` url = "tensorflow"
    -- AI companies
    | "anthropic.com"       `T.isInfixOf` url = "anthropic"
    | "openai.com"          `T.isInfixOf` url = "openai"
    -- Social / media
    | "twitter.com"         `T.isInfixOf` url = "twitter"
    | "x.com"               `T.isInfixOf` url = "twitter"
    | "reddit.com"          `T.isInfixOf` url = "reddit"
    | "youtube.com"         `T.isInfixOf` url = "youtube"
    | "youtu.be"            `T.isInfixOf` url = "youtube"
    | "tiktok.com"          `T.isInfixOf` url = "tiktok"
    | "substack.com"        `T.isInfixOf` url = "substack"
    | "news.ycombinator.com" `T.isInfixOf` url = "hacker-news"
    -- News
    | "nytimes.com"         `T.isInfixOf` url = "new-york-times"
    -- Institutions
    | "nasa.gov"            `T.isInfixOf` url = "nasa"
    | "apple.com"           `T.isInfixOf` url = "apple"
    | otherwise                                = "external"

-- | Percent-encode characters that would break a @?file=@ query-string value.
--   Slashes are intentionally left unencoded so root-relative paths remain
--   readable and work correctly with PDF.js's internal fetch.
encodeQueryValue :: Text -> Text
encodeQueryValue = T.concatMap enc
  where
    enc ' ' = "%20"
    enc '&' = "%26"
    enc '?' = "%3F"
    enc '+' = "%2B"
    enc '"' = "%22"
    enc c   = T.singleton c
