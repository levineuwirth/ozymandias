{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
module Site (rules) where

import Control.Monad (filterM, when)
import Data.List     (isPrefixOf)
import Data.Maybe    (fromMaybe)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, takeFileName, replaceExtension)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Config
import Hakyll
import Authors    (buildAllAuthors, applyAuthorRules)
import Backlinks  (backlinkRules)
import Compilers  (essayCompiler, postCompiler, pageCompiler, poetryCompiler, fictionCompiler,
                   compositionCompiler)
import Catalog      (musicCatalogCtx)
import Commonplace  (commonplaceCtx)
import Contexts   (siteCtx, essayCtx, postCtx, pageCtx, poetryCtx, fictionCtx, compositionCtx,
                   contentKindField)
import qualified Patterns as P
import Tags       (buildAllTags, applyTagRules)
import Pagination (blogPaginateRules)
import Stats      (statsRules)

-- Poems inside collection subdirectories, excluding their index pages.
collectionPoems :: Pattern
collectionPoems = "content/poetry/*/*.md" .&&. complement "content/poetry/*/index.md"

-- All poetry content (flat + collection), excluding collection index pages.
allPoetry :: Pattern
allPoetry = "content/poetry/*.md" .||. collectionPoems

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = T.unpack (Config.feedTitle       Config.siteConfig)
    , feedDescription = T.unpack (Config.feedDescription Config.siteConfig)
    , feedAuthorName  = T.unpack (Config.authorName      Config.siteConfig)
    , feedAuthorEmail = T.unpack (Config.authorEmail     Config.siteConfig)
    , feedRoot        = T.unpack (Config.siteUrl         Config.siteConfig)
    }

musicFeedConfig :: FeedConfiguration
musicFeedConfig = FeedConfiguration
    { feedTitle       = T.unpack (Config.siteName    Config.siteConfig) ++ " — Music"
    , feedDescription = "New compositions"
    , feedAuthorName  = T.unpack (Config.authorName  Config.siteConfig)
    , feedAuthorEmail = T.unpack (Config.authorEmail Config.siteConfig)
    , feedRoot        = T.unpack (Config.siteUrl     Config.siteConfig)
    }

rules :: Rules ()
rules = do
    -- ---------------------------------------------------------------------------
    -- Build mode. SITE_ENV=dev (set by `make dev` / `make watch`) includes
    -- drafts under content/drafts/**; anything else (unset, "deploy", "build")
    -- excludes them entirely from every match, listing, and asset rule below.
    -- ---------------------------------------------------------------------------
    isDev <- preprocess $ (== Just "dev") <$> lookupEnv "SITE_ENV"
    let allEssays = if isDev
                    then P.essayPattern .||. P.draftEssayPattern
                    else P.essayPattern

    -- ---------------------------------------------------------------------------
    -- Backlinks (pass 1: link extraction; pass 2: JSON generation)
    -- Must run before content rules so dependencies resolve correctly.
    -- ---------------------------------------------------------------------------
    backlinkRules

    -- ---------------------------------------------------------------------------
    -- Author index pages
    -- ---------------------------------------------------------------------------
    authors <- buildAllAuthors
    applyAuthorRules authors siteCtx

    -- ---------------------------------------------------------------------------
    -- Tag index pages
    -- ---------------------------------------------------------------------------
    tags <- buildAllTags
    applyTagRules tags siteCtx
    statsRules tags

    -- Per-page JS files — authored alongside content in content/**/*.js.
    -- Draft JS is handled by a separate dev-only rule below.
    match ("content/**/*.js" .&&. complement "content/drafts/**") $ do
        route   $ gsubRoute "content/" (const "")
        compile copyFileCompiler

    -- Per-page JS co-located with draft essays (dev-only).
    when isDev $ match "content/drafts/**/*.js" $ do
        route   $ gsubRoute "content/" (const "")
        compile copyFileCompiler

    -- CSS — must be matched before the broad static/** rule to avoid
    -- double-matching (compressCssCompiler vs. copyFileCompiler).
    match "static/css/*" $ do
        route   $ gsubRoute "static/" (const "")
        compile compressCssCompiler

    -- All other static files (fonts, JS, images, …)
    match ("static/**" .&&. complement "static/css/*") $ do
        route   $ gsubRoute "static/" (const "")
        compile copyFileCompiler

    -- Templates
    match "templates/**" $ compile templateBodyCompiler

    -- Link annotations — author-defined previews for any URL
    match "data/annotations.json" $ do
        route   idRoute
        compile copyFileCompiler

    -- Semantic search index — produced by tools/embed.py; fetched at runtime
    -- by static/js/semantic-search.js from /data/semantic-index.bin and
    -- /data/semantic-meta.json.
    match ("data/semantic-index.bin" .||. "data/semantic-meta.json") $ do
        route   idRoute
        compile copyFileCompiler

    -- Similar links — produced by tools/embed.py; absent on first build or
    -- when .venv is not set up.  Compiled as a raw string for similarLinksField.
    match "data/similar-links.json" $ compile getResourceBody

    -- Commonplace YAML — compiled as a raw string so it can be loaded
    -- with dependency tracking by the commonplace page compiler.
    match "data/commonplace.yaml" $ compile getResourceBody

    -- ---------------------------------------------------------------------------
    -- Homepage
    -- ---------------------------------------------------------------------------
    match "content/index.md" $ do
        route   $ constRoute "index.html"
        compile $ pageCompiler
            >>= loadAndApplyTemplate "templates/home.html"    pageCtx
            >>= loadAndApplyTemplate "templates/default.html" pageCtx
            >>= relativizeUrls

    -- ---------------------------------------------------------------------------
    -- Commonplace book
    -- ---------------------------------------------------------------------------
    match "content/commonplace.md" $ do
        route   $ constRoute "commonplace.html"
        compile $ pageCompiler
            >>= loadAndApplyTemplate "templates/commonplace.html" commonplaceCtx
            >>= loadAndApplyTemplate "templates/default.html"     commonplaceCtx
            >>= relativizeUrls

    match "content/colophon.md" $ do
        route   $ constRoute "colophon.html"
        compile $ essayCompiler
            >>= loadAndApplyTemplate "templates/essay.html"   essayCtx
            >>= loadAndApplyTemplate "templates/default.html" essayCtx
            >>= relativizeUrls

    match ("content/*.md"
            .&&. complement "content/index.md"
            .&&. complement "content/commonplace.md"
            .&&. complement "content/colophon.md") $ do
        route   $ gsubRoute "content/" (const "")
                  `composeRoutes` setExtension "html"
        compile $ pageCompiler
            >>= loadAndApplyTemplate "templates/page.html"    pageCtx
            >>= loadAndApplyTemplate "templates/default.html" pageCtx
            >>= relativizeUrls

    -- ---------------------------------------------------------------------------
    -- Essays — flat (content/essays/foo.md → essays/foo.html) and
    --          directory-based (content/essays/slug/index.md → essays/slug/index.html).
    --          In dev mode, drafts under content/drafts/essays/ route to
    --          drafts/essays/foo.html (flat) or drafts/essays/slug/index.html (dir).
    -- ---------------------------------------------------------------------------
    match allEssays $ do
        route $ customRoute $ \ident ->
            let fp       = toFilePath ident
                fname    = takeFileName fp
                isIndex  = fname == "index.md"
                isDraft  = "content/drafts/essays/" `isPrefixOf` fp
            in case (isDraft, isIndex) of
                -- content/drafts/essays/slug/index.md → drafts/essays/slug/index.html
                (True,  True)  -> replaceExtension (drop 8 fp) "html"
                -- content/drafts/essays/foo.md        → drafts/essays/foo.html
                (True,  False) -> "drafts/essays/" ++ replaceExtension fname "html"
                -- content/essays/slug/index.md        → essays/slug/index.html
                (False, True)  -> replaceExtension (drop 8 fp) "html"
                -- content/essays/foo.md               → essays/foo.html
                (False, False) -> "essays/" ++ replaceExtension fname "html"
        compile $ essayCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/essay.html"   essayCtx
            >>= loadAndApplyTemplate "templates/default.html" essayCtx
            >>= relativizeUrls

    -- Static assets co-located with directory-based essays (figures, data, PDFs, …)
    match ("content/essays/**"
           .&&. complement "content/essays/*.md"
           .&&. complement "content/essays/*/index.md") $ do
        route $ gsubRoute "content/" (const "")
        compile copyFileCompiler

    -- Static assets co-located with draft essays (dev-only).
    when isDev $ match ("content/drafts/essays/**"
                        .&&. complement "content/drafts/essays/*.md"
                        .&&. complement "content/drafts/essays/*/index.md") $ do
        route $ gsubRoute "content/" (const "")
        compile copyFileCompiler

    -- ---------------------------------------------------------------------------
    -- Blog posts
    -- ---------------------------------------------------------------------------
    match "content/blog/*.md" $ do
        route   $ gsubRoute "content/blog/" (const "blog/")
                  `composeRoutes` setExtension "html"
        compile $ postCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/blog-post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html"   postCtx
            >>= relativizeUrls

    -- ---------------------------------------------------------------------------
    -- Poetry
    -- ---------------------------------------------------------------------------
    -- Flat poems (e.g. content/poetry/sonnet-60.md)
    match "content/poetry/*.md" $ do
        route   $ gsubRoute "content/poetry/" (const "poetry/")
                  `composeRoutes` setExtension "html"
        compile $ poetryCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/reading.html"  poetryCtx
            >>= loadAndApplyTemplate "templates/default.html"  poetryCtx
            >>= relativizeUrls

    -- Collection poems (e.g. content/poetry/shakespeare-sonnets/sonnet-1.md)
    match collectionPoems $ do
        route   $ gsubRoute "content/poetry/" (const "poetry/")
                  `composeRoutes` setExtension "html"
        compile $ poetryCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/reading.html"  poetryCtx
            >>= loadAndApplyTemplate "templates/default.html"  poetryCtx
            >>= relativizeUrls

    -- Collection index pages (e.g. content/poetry/shakespeare-sonnets/index.md)
    match "content/poetry/*/index.md" $ do
        route   $ gsubRoute "content/poetry/" (const "poetry/")
                  `composeRoutes` setExtension "html"
        compile $ pageCompiler
            >>= loadAndApplyTemplate "templates/default.html"  pageCtx
            >>= relativizeUrls

    -- ---------------------------------------------------------------------------
    -- Fiction
    -- ---------------------------------------------------------------------------
    match "content/fiction/*.md" $ do
        route   $ gsubRoute "content/fiction/" (const "fiction/")
                  `composeRoutes` setExtension "html"
        compile $ fictionCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/reading.html"  fictionCtx
            >>= loadAndApplyTemplate "templates/default.html"  fictionCtx
            >>= relativizeUrls

    -- ---------------------------------------------------------------------------
    -- Music — catalog index
    -- ---------------------------------------------------------------------------
    match "content/music/index.md" $ do
        route   $ constRoute "music/index.html"
        compile $ pageCompiler
            >>= loadAndApplyTemplate "templates/music-catalog.html" musicCatalogCtx
            >>= loadAndApplyTemplate "templates/default.html"       musicCatalogCtx
            >>= relativizeUrls

    -- ---------------------------------------------------------------------------
    -- Music — composition landing pages + score reader
    -- ---------------------------------------------------------------------------

    -- Static assets (SVG score pages, audio, PDF) served unchanged.
    match "content/music/**/*.svg" $ do
        route   $ gsubRoute "content/" (const "")
        compile copyFileCompiler

    match "content/music/**/*.mp3" $ do
        route   $ gsubRoute "content/" (const "")
        compile copyFileCompiler

    match "content/music/**/*.pdf" $ do
        route   $ gsubRoute "content/" (const "")
        compile copyFileCompiler

    -- Landing page — full essay pipeline.
    match "content/music/*/index.md" $ do
        route   $ gsubRoute "content/" (const "")
                  `composeRoutes` setExtension "html"
        compile $ compositionCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/composition.html"  compositionCtx
            >>= loadAndApplyTemplate "templates/default.html"      compositionCtx
            >>= relativizeUrls

    -- Score reader — separate URL, minimal chrome.
    -- Compiled from the same source with version "score-reader".
    match "content/music/*/index.md" $ version "score-reader" $ do
        route $ customRoute $ \ident ->
            let slug = takeFileName . takeDirectory . toFilePath $ ident
            in  "music/" ++ slug ++ "/score/index.html"
        compile $ do
            makeItem ""
                >>= loadAndApplyTemplate "templates/score-reader.html"
                                         compositionCtx
                >>= loadAndApplyTemplate "templates/score-reader-default.html"
                                         compositionCtx
                >>= relativizeUrls

    -- ---------------------------------------------------------------------------
    -- Blog index (paginated)
    -- ---------------------------------------------------------------------------
    blogPaginateRules postCtx siteCtx

    -- ---------------------------------------------------------------------------
    -- Essay index
    -- ---------------------------------------------------------------------------
    create ["essays/index.html"] $ do
        route idRoute
        compile $ do
            essays <- recentFirst =<< loadAll (allEssays .&&. hasNoVersion)
            let ctx =
                    listField "essays" essayCtx (return essays)
                    <> constField "title" "Essays"
                    <> siteCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/essay-index.html" ctx
                >>= loadAndApplyTemplate "templates/default.html"      ctx
                >>= relativizeUrls

    -- ---------------------------------------------------------------------------
    -- New page — all content sorted by creation date, newest first
    -- ---------------------------------------------------------------------------
    create ["new.html"] $ do
        route idRoute
        compile $ do
            let allContent = (   allEssays
                             .||. "content/blog/*.md"
                             .||. "content/fiction/*.md"
                             .||. allPoetry
                             .||. "content/music/*/index.md"
                             ) .&&. hasNoVersion
            items <- recentFirst =<< loadAll allContent
            let itemCtx = contentKindField
                       <> dateField "date-iso" "%Y-%m-%d"
                       <> essayCtx
                ctx = listField "recent-items" itemCtx (return items)
                   <> constField "title" "New"
                   <> constField "new-page" "true"
                   <> siteCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/new.html"     ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    -- ---------------------------------------------------------------------------
    -- Library — comprehensive portal-grouped index of all content
    -- ---------------------------------------------------------------------------
    create ["library.html"] $ do
        route idRoute
        compile $ do
            -- A tag matches portal P if it equals "P" or starts with "P/".
            let hasPortal p item = do
                    meta <- getMetadata (itemIdentifier item)
                    let ts = fromMaybe [] (lookupStringList "tags" meta)
                    return $ any (\t -> t == p || (p ++ "/") `isPrefixOf` t) ts

                itemCtx = dateField "date-iso" "%Y-%m-%d" <> essayCtx

                buildPortal allItems portal = do
                    let slug = T.unpack (Config.portalSlug portal)
                    filtered <- filterM (hasPortal slug) allItems
                    sorted   <- recentFirst filtered
                    return $ Item (fromFilePath ("portal-" ++ slug)) (portal, sorted)

                portalCtx =
                        field "portal-slug" (return . T.unpack . Config.portalSlug . fst . itemBody)
                     <> field "portal-name" (return . T.unpack . Config.portalName . fst . itemBody)
                     <> listFieldWith "entries" itemCtx (return . snd . itemBody)

            let portalsField = listField "portals" portalCtx $ do
                    essays  <- loadAll (allEssays              .&&. hasNoVersion)
                    posts   <- loadAll ("content/blog/*.md"    .&&. hasNoVersion)
                    fiction <- loadAll ("content/fiction/*.md" .&&. hasNoVersion)
                    poetry  <- loadAll (allPoetry              .&&. hasNoVersion)
                    let allItems = essays ++ posts ++ fiction ++ poetry
                    mapM (buildPortal allItems) (Config.portals Config.siteConfig)

                ctx = portalsField
                   <> constField "title"   "Library"
                   <> constField "library" "true"
                   <> siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/library.html"  ctx
                >>= loadAndApplyTemplate "templates/default.html"  ctx
                >>= relativizeUrls

    -- ---------------------------------------------------------------------------
    -- Random page manifest — essays + blog posts only (no pagination/index pages)
    -- ---------------------------------------------------------------------------
    create ["random-pages.json"] $ do
        route idRoute
        compile $ do
            essays  <- loadAll (allEssays              .&&. hasNoVersion) :: Compiler [Item String]
            posts   <- loadAll ("content/blog/*.md"    .&&. hasNoVersion) :: Compiler [Item String]
            fiction <- loadAll ("content/fiction/*.md" .&&. hasNoVersion) :: Compiler [Item String]
            poetry  <- loadAll ("content/poetry/*.md"  .&&. hasNoVersion) :: Compiler [Item String]
            routes  <- mapM (getRoute . itemIdentifier) (essays ++ posts ++ fiction ++ poetry)
            let urls = [ "/" ++ r | Just r <- routes ]
            makeItem $ LBS.unpack (Aeson.encode urls)

    -- ---------------------------------------------------------------------------
    -- Atom feed — all content sorted by date
    -- ---------------------------------------------------------------------------
    create ["feed.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 30) . recentFirst
                        =<< loadAllSnapshots
                                (   (    allEssays
                                    .||. "content/blog/*.md"
                                    .||. "content/fiction/*.md"
                                    .||. allPoetry
                                    .||. "content/music/*/index.md"
                                    )
                                .&&. hasNoVersion
                                )
                                "content"
            let feedCtx =
                    dateField "updated"   "%Y-%m-%dT%H:%M:%SZ"
                    <> dateField "published" "%Y-%m-%dT%H:%M:%SZ"
                    <> bodyField "description"
                    <> defaultContext
            renderAtom feedConfig feedCtx posts

    -- ---------------------------------------------------------------------------
    -- Music feed — compositions only
    -- ---------------------------------------------------------------------------
    create ["music/feed.xml"] $ do
        route idRoute
        compile $ do
            compositions <- recentFirst
                        =<< loadAllSnapshots
                                ("content/music/*/index.md" .&&. hasNoVersion)
                                "content"
            let feedCtx =
                    dateField "updated"   "%Y-%m-%dT%H:%M:%SZ"
                    <> dateField "published" "%Y-%m-%dT%H:%M:%SZ"
                    <> bodyField "description"
                    <> defaultContext
            renderAtom musicFeedConfig feedCtx compositions
