{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Hierarchical tag system.
--
--   Tags are slash-separated strings in YAML frontmatter:
--     tags: [research/mathematics, nonfiction/essays, typography]
--
--   "research/mathematics" expands to ["research", "research/mathematics"]
--   so /research/ aggregates everything tagged with any research/* sub-tag.
--
--   Pages live at /<tag>/index.html — no /tags/ namespace:
--     research              → /research/
--     research/mathematics  → /research/mathematics/
--     typography            → /typography/
module Tags
    ( buildAllTags
    , applyTagRules
    ) where

import Data.List   (intercalate, nub)
import Hakyll
import Pagination  (sortAndGroup)
import Patterns    (tagIndexable)
import Contexts    (abstractField, tagLinksField)


-- ---------------------------------------------------------------------------
-- Hierarchy expansion
-- ---------------------------------------------------------------------------

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s = case dropWhile p s of
    ""  -> []
    s'  -> w : wordsBy p rest
      where (w, rest) = break p s'

-- | "research/mathematics" → ["research", "research/mathematics"]
--   "a/b/c"                → ["a", "a/b", "a/b/c"]
--   "typography"           → ["typography"]
expandTag :: String -> [String]
expandTag t =
    let segs = wordsBy (== '/') t
    in  [ intercalate "/" (take n segs) | n <- [1 .. length segs] ]

-- | All expanded tags for an item (reads the "tags" metadata field).
getExpandedTags :: MonadMetadata m => Identifier -> m [String]
getExpandedTags ident = nub . concatMap expandTag <$> getTags ident


-- ---------------------------------------------------------------------------
-- Identifiers and URLs
-- ---------------------------------------------------------------------------

tagFilePath :: String -> FilePath
tagFilePath tag = tag ++ "/index.html"

tagIdentifier :: String -> Identifier
tagIdentifier = fromFilePath . tagFilePath


-- ---------------------------------------------------------------------------
-- Building the Tags index
-- ---------------------------------------------------------------------------

-- | Scan all essays and blog posts and build the Tags index.
buildAllTags :: Rules Tags
buildAllTags =
    buildTagsWith getExpandedTags tagIndexable tagIdentifier


-- ---------------------------------------------------------------------------
-- Tag index page rules
-- ---------------------------------------------------------------------------

tagItemCtx :: Context String
tagItemCtx =
    dateField "date" "%-d %B %Y"
    <> tagLinksField "item-tags"
    <> abstractField
    <> defaultContext

-- | Page identifier for a tag index page.
--   Page 1 → <tag>/index.html
--   Page N → <tag>/page/N/index.html
tagPageId :: String -> PageNumber -> Identifier
tagPageId tag 1 = fromFilePath $ tag ++ "/index.html"
tagPageId tag n = fromFilePath $ tag ++ "/page/" ++ show n ++ "/index.html"

-- | Generate paginated index pages for every tag.
--   @baseCtx@ should be @siteCtx@ (passed in to avoid a circular import).
applyTagRules :: Tags -> Context String -> Rules ()
applyTagRules tags baseCtx = tagsRules tags $ \tag pat -> do
    paginate <- buildPaginateWith sortAndGroup pat (tagPageId tag)
    paginateRules paginate $ \pageNum pat' -> do
        route idRoute
        compile $ do
            items <- recentFirst =<< loadAll (pat' .&&. hasNoVersion)
            let ctx = listField "items" tagItemCtx (return items)
                   <> paginateContext paginate pageNum
                   <> constField "tag"   tag
                   <> constField "title" tag
                   <> baseCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag-index.html"  ctx
                >>= loadAndApplyTemplate "templates/default.html"    ctx
                >>= relativizeUrls


