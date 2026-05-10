{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Similar-links field: injects a "Related" list into essay/page contexts.
--
-- @data/similar-links.json@ is produced by @tools/embed.py@ at build time
-- (called from the Makefile after pagefind, before sign).  It is a plain
-- JSON object mapping root-relative URL paths to lists of similar pages:
--
--   { "/essays/my-essay/": [{"url": "...", "title": "...", "score": 0.87}] }
--
-- This module loads that file with dependency tracking (so pages recompile
-- when embeddings change) and provides @similarLinksField@, which resolves
-- to an HTML list for the current page's URL.
--
-- If the file is absent (e.g. @.venv@ not set up, or first build) the field
-- returns @noResult@ — the @$if(similar-links)$@ guard in the template is
-- false and no "Related" section is rendered.
module SimilarLinks (similarLinksField) where

import           Data.Maybe                 (fromMaybe)
import qualified Data.Map.Strict            as Map
import           Data.Map.Strict            (Map)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Aeson                 as Aeson
import           Hakyll
import           Utils                      (percentDecode)

-- ---------------------------------------------------------------------------
-- JSON schema
-- ---------------------------------------------------------------------------

data SimilarEntry = SimilarEntry
    { seUrl   :: String
    , seTitle :: String
    , seScore :: Double
    } deriving (Show)

instance Aeson.FromJSON SimilarEntry where
    parseJSON = Aeson.withObject "SimilarEntry" $ \o ->
        SimilarEntry
            <$> o Aeson..: "url"
            <*> o Aeson..: "title"
            <*> o Aeson..: "score"

-- ---------------------------------------------------------------------------
-- Context field
-- ---------------------------------------------------------------------------

-- | Provides @$similar-links$@ (HTML list) and @$has-similar-links$@
-- (boolean flag for template guards).
-- Returns @noResult@ when the JSON file is absent, unparseable, or the
-- current page has no similar entries.
similarLinksField :: Context String
similarLinksField = field "similar-links" $ \item -> do
    -- Load with dependency tracking — pages recompile when the JSON changes.
    slItem <- load (fromFilePath "data/similar-links.json") :: Compiler (Item String)
    case Aeson.decodeStrict (TE.encodeUtf8 (T.pack (itemBody slItem)))
            :: Maybe (Map T.Text [SimilarEntry]) of
        Nothing  -> fail "similar-links: could not parse data/similar-links.json"
        Just slMap -> do
            mRoute <- getRoute (itemIdentifier item)
            case mRoute of
                Nothing -> fail "similar-links: item has no route"
                Just r  ->
                    let key     = T.pack (normaliseUrl ("/" ++ r))
                        entries = fromMaybe [] (Map.lookup key slMap)
                    in  if null entries
                        then fail "no similar links"
                        else return (renderSimilarLinks entries)

-- ---------------------------------------------------------------------------
-- URL normalisation (mirrors embed.py's URL derivation)
-- ---------------------------------------------------------------------------
--
-- Distinct from 'Utils.normaliseUrl' (used by Backlinks/Stats): that one
-- strips @.html@ unconditionally, producing keys like @"/blog/index"@.
-- 'embed.py' instead emits @"/blog/"@ for directory-style URLs, so we
-- strip @"index.html"@ separately first to preserve the trailing slash.
-- The percent-decoding step is shared (imported from 'Utils').

normaliseUrl :: String -> String
normaliseUrl url =
    let t  = T.pack url
        -- strip query + fragment
        t1 = fst (T.breakOn "?" (fst (T.breakOn "#" t)))
        -- ensure leading slash
        t2 = if T.isPrefixOf "/" t1 then t1 else "/" `T.append` t1
        -- strip trailing index.html → keep the directory slash
        t3 = fromMaybe t2 (T.stripSuffix "index.html" t2)
        -- strip bare .html extension only for non-index pages
        t4 = fromMaybe t3 (T.stripSuffix ".html" t3)
    in  percentDecode (T.unpack t4)

-- ---------------------------------------------------------------------------
-- HTML rendering
-- ---------------------------------------------------------------------------

renderSimilarLinks :: [SimilarEntry] -> String
renderSimilarLinks entries =
    "<ul class=\"similar-links-list\">\n"
    ++ concatMap renderOne entries
    ++ "</ul>"
  where
    renderOne se =
        "<li class=\"similar-links-item\">"
        ++ "<a href=\"" ++ escapeHtml (seUrl se) ++ "\">"
        ++ escapeHtml (seTitle se)
        ++ "</a>"
        ++ "</li>\n"
