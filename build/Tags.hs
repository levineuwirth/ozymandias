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

-- | Top-level tags whose @<tag>/index.html@ route would collide with a
--   section landing page created by another rule. 'tagIdentifier' routes
--   every tag to @"<tag>/index.html"@, so any directory-style URL owned
--   elsewhere in the site is reserved.
--
--   The reserved set:
--
--     * @"photography"@ — owned by 'Photography.photographyLandingRules'.
--       Every photo's @tags:@ list begins with the bare @"photography"@
--       portal tag; without this exclusion, the tag system would clobber
--       the section landing.
--     * @"blog"@        — owned by 'Pagination.blogPaginateRules'
--                         (@/blog/index.html@ + @/blog/page/N/@).
--     * @"essays"@      — owned by the @essays/index.html@ create rule
--                         in 'Site.rules'.
--     * @"music"@       — owned by the music catalog at @/music/index.html@.
--     * @"authors"@     — owned by 'Authors.applyAuthorRules'
--                         (every author lives at @/authors/<slug>/@).
--     * @"build"@       — owned by 'Stats.statsRules' (@/build/index.html@).
--     * @"stats"@       — owned by 'Stats.statsRules' (@/stats/index.html@).
--
--   Sub-tags like @photography/landscape@ or @blog/announcements@ are
--   unaffected; their routes don't collide with any section landing.
--
--   If you add a new section that owns a single top-level URL segment,
--   add the segment here so a content tag of the same name doesn't
--   silently shadow it.
sectionOwnedTopLevelTags :: [String]
sectionOwnedTopLevelTags =
    [ "photography"
    , "blog"
    , "essays"
    , "music"
    , "authors"
    , "build"
    , "stats"
    ]

-- | All expanded tags for an item (reads the "tags" metadata field).
--   Filters out any 'sectionOwnedTopLevelTags' to prevent route
--   collisions with section landings.
getExpandedTags :: MonadMetadata m => Identifier -> m [String]
getExpandedTags ident =
    filter (`notElem` sectionOwnedTopLevelTags) . nub . concatMap expandTag
        <$> getTags ident


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


