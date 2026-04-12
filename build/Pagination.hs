{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Pagination helpers.
--
--   NOTE: This module must not import Contexts or Tags to avoid cycles.
--   Callers (Site.hs) pass contexts in as parameters.
module Pagination
    ( pageSize
    , sortAndGroup
    , blogPaginateRules
    ) where

import Hakyll


-- | Items per page across all paginated lists.
pageSize :: Int
pageSize = 20

-- | Sort identifiers by date (most recent first) and split into pages.
sortAndGroup :: (MonadMetadata m, MonadFail m) => [Identifier] -> m [[Identifier]]
sortAndGroup ids = paginateEvery pageSize <$> sortRecentFirst ids

-- | Page identifier for the blog index.
--   Page 1 → blog/index.html
--   Page N → blog/page/N/index.html
blogPageId :: PageNumber -> Identifier
blogPageId 1 = fromFilePath "blog/index.html"
blogPageId n = fromFilePath $ "blog/page/" ++ show n ++ "/index.html"

-- | Build and rule-ify a paginated blog index.
--   @itemCtx@: context for individual posts (postCtx).
--   @baseCtx@: site-level context (siteCtx).
blogPaginateRules :: Context String -> Context String -> Rules ()
blogPaginateRules itemCtx baseCtx = do
    paginate <- buildPaginateWith sortAndGroup ("content/blog/*.md" .&&. hasNoVersion) blogPageId
    paginateRules paginate $ \pageNum pat -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll (pat .&&. hasNoVersion)
            let ctx = listField "posts" itemCtx (return posts)
                   <> paginateContext paginate pageNum
                   <> constField "title" "Blog"
                   <> baseCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/blog-index.html"  ctx
                >>= loadAndApplyTemplate "templates/default.html"     ctx
                >>= relativizeUrls
