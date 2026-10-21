{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Similar-links field: injects a "Related" list into essay/page contexts.
--
-- @data/similar-links.json@ is produced by @tools/embed.py@ at build time
-- (stage 2 of @make build@, between the two compile passes). It is a plain
-- JSON object mapping root-relative URL paths to lists of similar pages:
--
--   { "/essays/my-essay/": [{"url": "...", "title": "...", "score": 0.87}] }
--
-- @site footer-data@ splits it into one file per page (build/FooterData.hs),
-- keeping each page's top 'maxRelated' entries without their scores, and
-- 'similarLinksField' renders the current page's.
--
-- If the file is absent (e.g. @.venv@ not set up, or first build) no page
-- has entries, and no "Related" section is rendered.
module SimilarLinks (similarLinksField) where

import qualified Data.Aeson                 as Aeson
import           Hakyll
import           FooterData                 (footerEntries, maxRelated)
import           Utils                      (isPdfUrl, pdfViewerUrl)

-- ---------------------------------------------------------------------------
-- JSON schema
-- ---------------------------------------------------------------------------

data SimilarEntry = SimilarEntry
    { seUrl   :: String
    , seTitle :: String
    } deriving (Show)

instance Aeson.FromJSON SimilarEntry where
    parseJSON = Aeson.withObject "SimilarEntry" $ \o ->
        SimilarEntry
            <$> o Aeson..: "url"
            <*> o Aeson..: "title"

-- ---------------------------------------------------------------------------
-- Context field
-- ---------------------------------------------------------------------------

-- | Provides @$similar-links$@, the page's Related list as HTML. Fails
-- (so the template's @$if(similar-links)$@ is false) when the page has no
-- entries in its footer file. The dependency that brings the section back
-- when entries appear is 'FooterData.footerDepsField'; the URL forms
-- @tools/embed.py@ emits are normalised when the footer files are written.
similarLinksField :: Context String
similarLinksField = field "similar-links" $ \item -> do
    entries <- footerEntries "related" item
    if null entries
        then fail "no similar links"
        else return (renderSimilarLinks (take maxRelated entries))

-- ---------------------------------------------------------------------------
-- HTML rendering
-- ---------------------------------------------------------------------------

-- | Render the Related block. Each anchor gets:
--   * @class="similar-link"@ — whitelist for popups.js so the default
--     footer-exclusion does not fire (content preview on hover).
--   * @data-link-icon@ / @data-link-icon-type@ — page or document icon,
--     rendered via the existing a[data-link-icon] mask-image system in
--     typography.css.
--   * For PDFs: @class="pdf-link"@ + @data-pdf-src@ + href rewritten to
--     the PDF.js viewer, matching the rest of the site. The pdfContent
--     provider in popups.js binds on @.pdf-link[data-pdf-src]@.
renderSimilarLinks :: [SimilarEntry] -> String
renderSimilarLinks entries =
    "<ul class=\"similar-links-list\">\n"
    ++ concatMap renderOne entries
    ++ "</ul>"
  where
    renderOne se
        | isPdfUrl (seUrl se) = renderPdf se
        | otherwise           = renderPage se

    renderPage se =
        "<li class=\"similar-links-item\">"
        ++ "<a class=\"similar-link\""
        ++ " href=\"" ++ escapeHtml (seUrl se) ++ "\""
        ++ " data-link-icon=\"internal\" data-link-icon-type=\"svg\">"
        ++ escapeHtml (seTitle se)
        ++ "</a></li>\n"

    renderPdf se =
        -- The PDF path becomes the @file=@ query value ('pdfViewerUrl'
        -- encodes what would break it), then HTML-escaped for the
        -- attribute. A @#page=N@ fragment stays a fragment of the viewer
        -- URL itself — PDF.js reads it from location.hash.
        let raw          = seUrl se
            (path, frag) = break (== '#') raw
            viewerUrl    = escapeHtml (pdfViewerUrl path ++ frag)
        in  "<li class=\"similar-links-item\">"
            ++ "<a class=\"similar-link pdf-link\""
            ++ " href=\"" ++ viewerUrl ++ "\""
            ++ " data-pdf-src=\"" ++ escapeHtml raw ++ "\""
            ++ " data-link-icon=\"document\" data-link-icon-type=\"svg\">"
            ++ escapeHtml (seTitle se)
            ++ "</a></li>\n"
