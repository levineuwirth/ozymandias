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
import qualified Data.ByteString            as BS
import qualified Data.Map.Strict            as Map
import           Data.Map.Strict            (Map)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Encoding.Error   as TE
import qualified Data.Aeson                 as Aeson
import           Hakyll

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

-- | Percent-decode @%XX@ escapes (UTF-8) so percent-encoded paths
-- collide with their decoded form on map lookup. Mirrors
-- 'Backlinks.percentDecode'; the two implementations are intentionally
-- duplicated because they apply different normalisations *before*
-- decoding (Backlinks strips @.html@ unconditionally; SimilarLinks
-- preserves the trailing-slash form for index pages).
percentDecode :: String -> String
percentDecode = T.unpack . TE.decodeUtf8With TE.lenientDecode . BS.pack . go
  where
    go []                 = []
    go ('%':a:b:rest)
        | Just hi <- hexDigit a
        , Just lo <- hexDigit b
        = fromIntegral (hi * 16 + lo) : go rest
    go (c:rest)           = fromIntegral (fromEnum c) : go rest

    hexDigit c
        | c >= '0' && c <= '9' = Just (fromEnum c - fromEnum '0')
        | c >= 'a' && c <= 'f' = Just (fromEnum c - fromEnum 'a' + 10)
        | c >= 'A' && c <= 'F' = Just (fromEnum c - fromEnum 'A' + 10)
        | otherwise            = Nothing

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
