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
    paginate <- buildPaginateWith sortAndGroup (blogPattern .&&. hasNoVersion) blogPageId
    paginateRules paginate $ \pageNum pat -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll (pat .&&. hasNoVersion)
            let ctx = listField "posts" itemCtx (return posts)
                   <> paginateContext paginate pageNum
                   <> constField "title" "Blog"
                   <> baseCtx
            makeItem ""
                >>= inDefault "templates/blog-index.html" ctx
