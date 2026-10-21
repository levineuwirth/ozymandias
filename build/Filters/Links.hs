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
module Filters.Links (apply, pdfViewerTarget) where

import           Data.Text            (Text)
import qualified Data.Text            as T
import           Text.Pandoc.Definition
import           Text.Pandoc.Shared   (stringify)
import           Text.Pandoc.Walk     (walk)
import           Config               (SiteConfig (..), siteOwnHosts)
import           Utils                (isPdfUrl, pdfViewerUrl)

-- | Apply link classification to the entire document.
--   Two passes: PDF links first (rewrites href to the viewer URL and tags
--   the anchor @pdf-link@), then general classification. The second pass
--   explicitly skips anchors the PDF pass already claimed — the viewer URL
--   is root-relative, so without that guard it would also be classified as
--   an internal page link and get double chrome. Finally, author-marked
--   spans collect the classified icons at the end of their passage.
apply :: SiteConfig -> Pandoc -> Pandoc
apply cfg = walk deferIcons . walk (classifyLink cfg) . walk classifyPdfLink

-- | An author-marked span owns the trailing icons for its inline links.
-- Keep the original anchors (and their popup metadata) in the prose, and
-- emit independently named anchors after the span's closing punctuation.
-- Notes and nested opt-in spans own their links; never collect those here.
deferIcons :: Inline -> Inline
deferIcons (Span attr@(_, classes, _) ils)
    | "link-icons-end" `elem` classes =
        let (body, icons) = collectIcons ils
        in Span attr (body ++ [Span ("", ["link-icons-tail"], [])
                              (Str "\x2060" : icons) | not (null icons)])
deferIcons x = x

collectIcons :: [Inline] -> ([Inline], [Inline])
collectIcons ils = let (body, icons) = unzip (map collect ils)
                   in (body, concat icons)
  where
    collect (Link (ident, classes, kvs) label target@(url, _))
        | lookup "data-link-icon-type" kvs == Just "svg"
        , "link-icon-deferred" `notElem` classes
        , "link-icon-end" `notElem` classes =
            let name = T.strip (stringify label)
                iconAttrs = ("", classes ++ ["link-icon-end"],
                    ("aria-label", if T.null name then url else name)
                    : filter ((/= "aria-label") . fst) kvs)
            in (Link (ident, classes ++ ["link-icon-deferred"], kvs) label target,
                [Link iconAttrs [] target])
    collect x@(Span (_, classes, _) _)
        | any (`elem` classes) ["link-icons-end", "link-icons-tail"] = (x, [])
    collect (Span attr xs)    = descend (Span attr) xs
    collect (Emph xs)         = descend Emph xs
    collect (Underline xs)    = descend Underline xs
    collect (Strong xs)       = descend Strong xs
    collect (Strikeout xs)    = descend Strikeout xs
    collect (Superscript xs)  = descend Superscript xs
    collect (Subscript xs)    = descend Subscript xs
    collect (SmallCaps xs)    = descend SmallCaps xs
    collect (Quoted kind xs) = descend (Quoted kind) xs
    collect (Cite refs xs)    = descend (Cite refs) xs
    collect x                = (x, [])
    descend ctor xs = let (body, icons) = collectIcons xs
                      in (ctor body, icons)

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
    | "pdf-link" `notElem` classes
    , Just (viewerUrl, path) <- pdfViewerTarget url =
        let classes'  = classes ++ ["pdf-link"]
            kvs'      = kvs ++ [("data-pdf-src", path)]
        in  Link (ident, classes', kvs') ils (viewerUrl, title)
classifyPdfLink x = x

-- | For a root-relative PDF URL, the viewer URL to link to (fragment kept)
--   and the bare path for @data-pdf-src@; Nothing for anything else. Body
--   links and the bibliography's titles ('Citations') both open this way.
pdfViewerTarget :: Text -> Maybe (Text, Text)
pdfViewerTarget url
    | "/" `T.isPrefixOf` url, isPdfUrl (T.unpack url) =
        let (path, fragment) = T.break (== '#') url
        in  Just (T.pack (pdfViewerUrl (T.unpack path)) <> fragment, path)
    | otherwise = Nothing

classifyLink :: SiteConfig -> Inline -> Inline
classifyLink _ l@(Link (_, classes, _) _ _)
    -- Source-ref links are owned by Filters.SourceRefs: they keep the
    -- inline-code chrome of their body, must not receive an external
    -- brand icon stamp, and have their own popup provider. Leave them
    -- entirely alone.
    | "source-ref" `elem` classes = l
    -- PDF links were already rewritten to the (root-relative) viewer URL
    -- and given their own chrome by 'classifyPdfLink' in the preceding
    -- pass; without this guard they would be double-classified as
    -- internal page links.
    | "pdf-link" `elem` classes = l
classifyLink cfg (Link (ident, classes, kvs) ils (url, title))
    | isExternal cfg url =
        let icon     = domainIcon cfg url
            classes' = classes ++ ["link-external"]
            kvs'     = kvs
                       ++ [("target",             "_blank")]
                       ++ [("rel",                "noopener noreferrer")]
                       ++ [("data-link-icon",      icon)]
                       ++ [("data-link-icon-type", "svg")]
        in  Link (ident, classes', kvs') ils (url, title)
    | isInternalPage url =
        let classes' = classes ++ ["link-internal"]
            kvs'     = kvs
                       ++ [("data-link-icon",      "internal")]
                       ++ [("data-link-icon-type", "svg")]
        in  Link (ident, classes', kvs') ils (url, title)
classifyLink _ x = x

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | True if the URL is a root-relative or relative path to another page
--   (not an anchor-only link like @#section@ or @#ref-foo@).
isInternalPage :: Text -> Bool
isInternalPage url
    | T.null url                  = False
    | "#" `T.isPrefixOf` url      = False   -- anchor-only
    | "mailto:" `T.isPrefixOf` url = False
    | "http://"  `T.isPrefixOf` url = False  -- handled by isExternal
    | "https://" `T.isPrefixOf` url = False
    | otherwise                    = True

-- | True if the URL points outside the site's content host.
--
--   Only the configured host, with and without @www.@ ('siteOwnHosts'),
--   counts as the content site itself. Sibling subdomains like the configured forge host are
--   distinct services and are classified as external so they receive their
--   brand icon, @target=_blank@, and @rel=noopener noreferrer@.
--
--   Uses strict hostname comparison rather than substring matching, so a
--   hostile lookalike like @example.org.attacker.com@ is also
--   correctly classified as external.
isExternal :: SiteConfig -> Text -> Bool
isExternal cfg url =
    case extractHost url of
        Nothing   -> False
        Just host -> host `notElem` map T.pack (siteOwnHosts cfg)

-- | Extract the lowercased hostname from an absolute http(s) URL,
--   stripping any userinfo (@user:pass\@@) and port. Returns 'Nothing'
--   for non-http(s) URLs (relative paths, mailto:, etc.).
extractHost :: Text -> Maybe Text
extractHost url
    | Just rest <- T.stripPrefix "https://" url = Just (hostOf rest)
    | Just rest <- T.stripPrefix "http://"  url = Just (hostOf rest)
    | otherwise                                  = Nothing
  where
    hostOf rest =
        let authority = T.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#') rest
            -- 'T.breakOnEnd' yields the segment after the last @\@@, or
            -- the whole authority when there is no userinfo.
            (_, hostPort) = T.breakOnEnd "@" authority
            host          = T.takeWhile (/= ':') hostPort
        in  T.toLower host

-- | Icon name for the link, matching a file in /images/link-icons/<name>.svg.
--
--   Matches on the URL's host only, never on the full URL — a path like
--   @https://example.org/why-x.com-failed@ must not get the Twitter
--   icon. URLs with no extractable host get the generic icon.
domainIcon :: SiteConfig -> Text -> Text
domainIcon cfg url = maybe "external" (iconForHost cfg) (extractHost url)

iconForHost :: SiteConfig -> Text -> Text
iconForHost cfg host
    -- Scholarly / reference
    | m "wikipedia.org"        = "wikipedia"
    | m "arxiv.org"            = "arxiv"
    | m "doi.org"              = "doi"
    | m "worldcat.org"         = "worldcat"
    | m "orcid.org"            = "orcid"
    | m "archive.org"          = "internet-archive"
    -- Code / software
    | m "github.com"           = "github"
    | m (T.pack (siteForgeHost cfg)) = "forgejo"
    | m "tensorflow.org"       = "tensorflow"
    -- AI companies (consumer products share a brand icon with the lab)
    | m "anthropic.com"        = "anthropic"
    | m "claude.ai"            = "anthropic"
    | m "openai.com"           = "openai"
    | m "chatgpt.com"          = "openai"
    -- Social / media
    | m "twitter.com"          = "twitter"
    | m "x.com"                = "twitter"
    | m "reddit.com"           = "reddit"
    | m "youtube.com"          = "youtube"
    | m "youtu.be"             = "youtube"
    | m "tiktok.com"           = "tiktok"
    | m "substack.com"         = "substack"
    | m "news.ycombinator.com" = "hacker-news"
    | m "lesswrong.com"        = "lesswrong"
    -- News
    | m "nytimes.com"          = "new-york-times"
    -- Institutions
    | m "nasa.gov"             = "nasa"
    | m "apple.com"            = "apple"
    | otherwise                = "external"
  where
    -- Label-suffix match: the host is the domain itself or a subdomain
    -- of it. Never fires on a lookalike label (@notx.com@) or on text
    -- in the path or query.
    m d = host == d || ("." <> d) `T.isSuffixOf` host
