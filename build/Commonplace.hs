{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Commonplace book: loads data/commonplace.yaml and renders
-- themed and chronological HTML views for /commonplace.
module Commonplace
    ( commonplaceCtx
    ) where

import Data.Aeson    (FromJSON (..), withObject, (.:), (.:?), (.!=))
import Data.List     (nub, sortBy)
import Data.Ord      (comparing, Down (..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml             as Y
import Hakyll hiding (escapeHtml, renderTags)
import Contexts (siteCtx)
import Utils    (escapeHtml)

-- ---------------------------------------------------------------------------
-- Entry type
-- ---------------------------------------------------------------------------

data CPEntry = CPEntry
    { cpText        :: String
    , cpAttribution :: String
    , cpSource      :: Maybe String
    , cpSourceUrl   :: Maybe String
    , cpTags        :: [String]
    , cpCommentary  :: Maybe String
    , cpDateAdded   :: String
    }

instance FromJSON CPEntry where
    parseJSON = withObject "CPEntry" $ \o -> CPEntry
        <$> o .:  "text"
        <*> o .:  "attribution"
        <*> o .:? "source"
        <*> o .:? "source-url"
        <*> o .:? "tags"       .!= []
        <*> o .:? "commentary"
        <*> o .:? "date-added" .!= ""

-- ---------------------------------------------------------------------------
-- HTML rendering
-- ---------------------------------------------------------------------------

-- | Escape HTML, then replace newlines with <br> for multi-line verse.
renderText :: String -> String
renderText = concatMap tr . escapeHtml . stripTrailingNL
  where
    tr '\n' = "<br>\n"
    tr c    = [c]
    stripTrailingNL = reverse . dropWhile (== '\n') . reverse

renderAttribution :: CPEntry -> String
renderAttribution e =
    "<p class=\"cp-attribution\">\x2014\x202f"
    ++ escapeHtml (cpAttribution e)
    ++ maybe "" renderSource (cpSource e)
    ++ "</p>"
  where
    renderSource src = case cpSourceUrl e of
        Just url -> ", <a href=\"" ++ escapeHtml url ++ "\">"
                 ++ escapeHtml src ++ "</a>"
        Nothing  -> ", " ++ escapeHtml src

renderTags :: [String] -> String
renderTags [] = ""
renderTags ts =
    "<div class=\"cp-tags\">"
    ++ concatMap (\t -> "<span class=\"cp-tag\">" ++ escapeHtml t ++ "</span>") ts
    ++ "</div>"

renderEntry :: CPEntry -> String
renderEntry e = concat
    [ "<article class=\"cp-entry\">"
    ,   "<blockquote class=\"cp-quote\"><p>"
    ,   renderText (cpText e)
    ,   "</p></blockquote>"
    ,   renderAttribution e
    ,   maybe "" renderCommentary (cpCommentary e)
    ,   renderTags (cpTags e)
    , "</article>"
    ]
  where
    renderCommentary c =
        "<p class=\"cp-commentary\">" ++ escapeHtml c ++ "</p>"

-- ---------------------------------------------------------------------------
-- Themed view
-- ---------------------------------------------------------------------------

-- | All distinct tags in first-occurrence order (preserves YAML ordering).
allTags :: [CPEntry] -> [String]
allTags = nub . concatMap cpTags

renderTagSection :: String -> [CPEntry] -> String
renderTagSection tag entries = concat
    [ "<section class=\"cp-theme-section\">"
    ,   "<h2 class=\"cp-theme-heading\">" ++ escapeHtml tag ++ "</h2>"
    ,   concatMap renderEntry entries
    , "</section>"
    ]

renderThemedView :: [CPEntry] -> String
renderThemedView [] =
    "<div class=\"cp-themed\" id=\"cp-themed\">"
    ++ "<p class=\"cp-empty\">No entries yet.</p>"
    ++ "</div>"
renderThemedView entries =
    "<div class=\"cp-themed\" id=\"cp-themed\">"
    ++ concatMap renderSection (allTags entries)
    ++ (if null untagged then ""
        else renderTagSection "miscellany" untagged)
    ++ "</div>"
  where
    renderSection t =
        let es = filter (elem t . cpTags) entries
        in  if null es then "" else renderTagSection t es
    untagged = filter (null . cpTags) entries

-- ---------------------------------------------------------------------------
-- Chronological view
-- ---------------------------------------------------------------------------

renderChronoView :: [CPEntry] -> String
renderChronoView entries =
    "<div class=\"cp-chrono\" id=\"cp-chrono\" hidden>"
    ++ (if null sorted
            then "<p class=\"cp-empty\">No entries yet.</p>"
            else concatMap renderEntry sorted)
    ++ "</div>"
  where
    sorted = sortBy (comparing (Down . cpDateAdded)) entries

-- ---------------------------------------------------------------------------
-- Load entries from data/commonplace.yaml
-- ---------------------------------------------------------------------------

loadCommonplace :: Compiler [CPEntry]
loadCommonplace = do
    rawItem <- load (fromFilePath "data/commonplace.yaml") :: Compiler (Item String)
    let raw = itemBody rawItem
    case Y.decodeEither' (BS.pack raw) of
        Left  err     -> fail ("commonplace.yaml: " ++ show err)
        Right entries -> return entries

-- ---------------------------------------------------------------------------
-- Context
-- ---------------------------------------------------------------------------

commonplaceCtx :: Context String
commonplaceCtx =
    constField "commonplace" "true"
    <> themedField
    <> chronoField
    <> siteCtx
  where
    themedField = field "cp-themed-html" $ \_ ->
        renderThemedView <$> loadCommonplace
    chronoField = field "cp-chrono-html" $ \_ ->
        renderChronoView <$> loadCommonplace
