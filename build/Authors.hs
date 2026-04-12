{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Author system — treats authors like tags.
--
--   Author pages live at /authors/{slug}/index.html.
--   Items with no "authors" frontmatter key default to the site's
--   @author-name@ from @site.yaml@.
--
--   Frontmatter format (name-only or name|url — url part is ignored now):
--     authors:
--       - "Jane Doe"
--       - "Alice Smith | https://alice.example"   -- url ignored; link goes to /authors/alice-smith/
module Authors
    ( buildAllAuthors
    , applyAuthorRules
    ) where

import Data.Maybe           (fromMaybe)
import Hakyll
import qualified Config
import Pagination           (sortAndGroup)
import Patterns             (authorIndexable)
import Contexts             (abstractField, tagLinksField)
import Utils                (authorSlugify, authorNameOf)


-- ---------------------------------------------------------------------------
-- Slug helpers
--
-- The slugify and nameOf helpers used to live here in their own
-- definitions; they now defer to 'Utils' so that they cannot drift from
-- the 'Contexts' versions on Unicode edge cases.
-- ---------------------------------------------------------------------------

slugify :: String -> String
slugify = authorSlugify

nameOf :: String -> String
nameOf = authorNameOf


-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

defaultAuthor :: String
defaultAuthor = Config.defaultAuthor

-- | Content patterns indexed by author. Sourced from 'Patterns.authorIndexable'
-- so this stays in lockstep with Tags.hs and Backlinks.hs.
allContent :: Pattern
allContent = authorIndexable


-- ---------------------------------------------------------------------------
-- Tag-like helpers (mirror of Tags.hs)
-- ---------------------------------------------------------------------------

-- | Returns all author names for an identifier.
--   Defaults to the site's configured @author-name@ when no "authors" key
--   is present.
getAuthors :: MonadMetadata m => Identifier -> m [String]
getAuthors ident = do
    meta <- getMetadata ident
    let entries = fromMaybe [] (lookupStringList "authors" meta)
    return $ if null entries
        then [defaultAuthor]
        else map nameOf entries

-- | Canonical identifier for an author's index page (page 1).
authorIdentifier :: String -> Identifier
authorIdentifier name = fromFilePath $ "authors/" ++ slugify name ++ "/index.html"

-- | Paginated identifier: page 1 → authors/{slug}/index.html
--                         page N → authors/{slug}/page/N/index.html
authorPageId :: String -> PageNumber -> Identifier
authorPageId slug 1 = fromFilePath $ "authors/" ++ slug ++ "/index.html"
authorPageId slug n = fromFilePath $ "authors/" ++ slug ++ "/page/" ++ show n ++ "/index.html"


-- ---------------------------------------------------------------------------
-- Build + rules
-- ---------------------------------------------------------------------------

buildAllAuthors :: Rules Tags
buildAllAuthors = buildTagsWith getAuthors allContent authorIdentifier

applyAuthorRules :: Tags -> Context String -> Rules ()
applyAuthorRules authors baseCtx = tagsRules authors $ \name pat -> do
    let slug = slugify name
    paginate <- buildPaginateWith sortAndGroup pat (authorPageId slug)
    paginateRules paginate $ \pageNum pat' -> do
        route idRoute
        compile $ do
            items <- recentFirst =<< loadAll (pat' .&&. hasNoVersion)
            let ctx = listField "items" itemCtx (return items)
                   <> paginateContext paginate pageNum
                   <> constField "author" name
                   <> constField "title"  name
                   <> baseCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/author-index.html" ctx
                >>= loadAndApplyTemplate "templates/default.html"      ctx
                >>= relativizeUrls
  where
    itemCtx = dateField "date" "%-d %B %Y"
           <> tagLinksField "item-tags"
           <> abstractField
           <> defaultContext


