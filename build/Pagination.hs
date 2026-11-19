{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Pagination helpers.
--
--   NOTE: This module must not import Contexts or Tags to avoid cycles.
--   Callers (Site.hs) pass contexts in as parameters.
module Pagination
    ( pageSize
    , sortAndGroup
    , sortAndGroupAt
    , blogPaginateRules
    ) where

import Hakyll
import Config (Section (..), sectionLandingTitle)
import Utils (inDefault)
import Patterns (blogPattern)


-- | Items per page across most paginated lists (e.g. the blog).
pageSize :: Int
pageSize = 20

-- | Sort identifiers by date (most recent first) and split into pages.
sortAndGroup :: (MonadMetadata m, MonadFail m) => [Identifier] -> m [[Identifier]]
sortAndGroup = sortAndGroupAt pageSize

-- | Like 'sortAndGroup' but with a caller-supplied page size. Used by
--   listings that want a different density than the blog default.
sortAndGroupAt :: (MonadMetadata m, MonadFail m) => Int -> [Identifier] -> m [[Identifier]]
sortAndGroupAt n ids = paginateEvery n <$> sortRecentFirst ids

-- | Page identifier for the blog index, at the section's address.
--   Page 1 → blog/index.html
--   Page N → blog/page/N/index.html
blogPageId :: Section -> PageNumber -> Identifier
blogPageId section 1 = fromFilePath $ sectionAddress section ++ "/index.html"
blogPageId section n = fromFilePath $ sectionAddress section ++ "/page/" ++ show n ++ "/index.html"

-- | Build and rule-ify a paginated blog index, for a site that has the
--   section.
--   @itemCtx@: context for individual posts (postCtx).
--   @baseCtx@: site-level context (siteCtx).
--
--   A blog with no posts still has its first page: Hakyll makes a rule
--   for each group of posts and so none for no posts, which left the
--   menu's link to the section with nothing behind it until the first
--   post was written. The other sections' landings are single rules, and
--   were always there.
blogPaginateRules :: Section -> Context String -> Context String -> Rules ()
blogPaginateRules section itemCtx baseCtx = do
    let grouped ids = (\pages -> if null pages then [[]] else pages) <$> sortAndGroup ids
    paginate <- buildPaginateWith grouped (blogPattern .&&. hasNoVersion) (blogPageId section)
    paginateRules paginate $ \pageNum pat -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll (pat .&&. hasNoVersion)
            let ctx = listField "posts" itemCtx (return posts)
                   <> paginateContext paginate pageNum
                   <> constField "title" (sectionLandingTitle section)
                   <> baseCtx
            makeItem ""
                >>= inDefault "templates/blog-index.html" ctx
