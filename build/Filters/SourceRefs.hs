{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Detect repo-relative source-file references in prose and wrap them
--   in a link that triggers a hover-preview popup of the file's contents.
--
--   Two trigger forms:
--
--   * Inline @\`build\/Filters\/Links.hs\`@ — Markdown inline code whose
--     text passes a conservative source-path heuristic.
--   * A Markdown link to
--     the configured source repository, under @(src|raw)/branch/<branch>/<path>@.
--
--   Both produce
--   @\<a class="source-ref" data-source-path="…" href="…">@. The href
--   points to the Forgejo source viewer so a click without JS — or a
--   popup that fails to fetch — still resolves to a useful target.
--   An explicit link can use the same class and attribute to preview an
--   allowlisted file while retaining its local full-file destination.
--   The popup provider in @static\/js\/popups.js@ fetches
--   @\/source\/\<path\>@ (a same-origin copy emitted by the Hakyll
--   source-preview rule in 'Site.rules') and renders a
--   syntax-highlighted snippet via Prism.
--
--   Conservative-by-design: the trigger only fires on paths the
--   @/source/@ serving rule actually publishes ('sourcePreviewGlobs',
--   from which that rule builds its pattern). This keeps the parser
--   cheap, avoids false positives on words that happen to contain a
--   slash and a dot, and guarantees every wrapped path has a fetchable
--   @/source/…@ copy.
module Filters.SourceRefs
    ( apply
    , isSourcePath
    , forgejoSourceUrl
      -- * Shared with 'Site.rules'
    , sourcePreviewGlobs
    ) where

import           Config               (SiteConfig (..))
import           EngineFiles          (existingFile)
import           Control.Monad        (when)
import           Data.IORef           (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Hakyll               (Pattern, fromFilePath, fromGlob, matches)
import           Data.Maybe           (isJust)
import           System.IO.Unsafe     (unsafePerformIO)
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk     (walkM)

-- | Two passes: lift Forgejo source URLs in existing Markdown links
--   first, then wrap inline-code source paths. Both passes only add
--   the @source-ref@ class when it is not already present, so re-runs
--   are idempotent.
--
--   Runs in 'IO' because the heuristic confirms each candidate is a
--   real on-disk file before wrapping. This rules out paths like
--   @data/backlinks.json@ that look like source but are Hakyll build
--   artifacts produced into @_site/@ — wrapping those would emit a
--   link whose popup is guaranteed to 404.
apply :: SiteConfig -> Pandoc -> IO Pandoc
apply cfg doc = do
    afterLinks <- walkM (classifyExistingLink cfg) doc
    walkM (wrapInlineCode cfg) afterLinks

-- | Inline @`path`@ → @\<a class="source-ref" data-source-path="path"\>\<code\>path\<\/code\>\<\/a\>@.
--   The original 'Code' node is preserved as the link's body so the
--   inline-code chrome (mono font, background) survives unchanged.
wrapInlineCode :: SiteConfig -> Inline -> IO Inline
wrapInlineCode cfg orig@(Code (cIdent, cClasses, cKvs) txt)
    | "source-ref" `notElem` cClasses
    , isSourcePath txt = do
        exists <- existsCached txt
        if exists
            then pure $ Link
                    ( ""
                    , ["source-ref"]
                    , [ ("data-source-path", txt)
                      , ("target",           "_blank")
                      , ("rel",              "noopener noreferrer")
                      ]
                    )
                    [Code (cIdent, cClasses, cKvs) txt]
                    (sourceHref cfg txt, "")
            else pure orig
wrapInlineCode _ x = pure x

-- | Click-through target for a wrapped source path.
--
--   The Forgejo source viewer is the right destination for a file the
--   repository actually contains. It is the wrong destination for a
--   generated one: @data\/similar-links.json@ is produced by
--   @tools\/embed.py@ and Git-ignored, so the repository URL 404s — the
--   defect observed on @\/essays\/ozymandias.html@. Those link to the
--   published @\/source\/@ snapshot instead, which is the copy the hover
--   popup fetches anyway and the only public copy that exists.
--
--   The two categories are distinguished by 'isGeneratedPath' rather than
--   by asking Git, so this stays a pure string decision inside a filter
--   that already runs once per page.
sourceHref :: SiteConfig -> Text -> Text
sourceHref cfg path
    | isGeneratedPath path = "/source/" <> path
    | otherwise            = forgejoSourceUrl cfg path

-- | Paths that exist on disk during a build but are not tracked by Git —
--   the @data\/@ artifacts named in @.gitignore@'s \"generated at build
--   time\" block that are also in 'sourcePreviewGlobs' (anything not in
--   that allowlist is never wrapped in the first place).
isGeneratedPath :: Text -> Bool
isGeneratedPath path = path `elem` [ "data/similar-links.json" ]

-- | Existing Markdown link to a Forgejo source URL on this site's git
--   host → tagged @source-ref@ and given a @data-source-path@ pointing
--   at the same path the popup provider expects.
classifyExistingLink :: SiteConfig -> Inline -> IO Inline
classifyExistingLink cfg orig@(Link (ident, classes, kvs) ils (url, title))
    | "source-ref" `notElem` classes
    , Just path <- forgejoSourcePath cfg url
    , isSourcePath path = do
        exists <- existsCached path
        if exists
            then pure $ Link
                    ( ident
                    , classes ++ ["source-ref"]
                    , kvs ++ [("data-source-path", path)]
                    )
                    ils (url, title)
            else pure orig
classifyExistingLink _ x = pure x

-- ---------------------------------------------------------------------------
-- Heuristic
-- ---------------------------------------------------------------------------

-- | True when the text looks like a repo-relative path that the
--   @/source/@ serving rule actually publishes, ends in a known source
--   extension (a root file such as @Makefile@ need not), and contains
--   only safe path characters. Conservative by design — the goal is no
--   false positives on prose that incidentally contains a slash and a
--   dot, and no wrapped path whose popup fetch would 404.
isSourcePath :: Text -> Bool
isSourcePath t = and
    [ not (T.null t)
    , T.all safeChar t
    , any (`matches` fromFilePath (T.unpack t)) sourcePreviewPatterns
    , hasKnownExt t || not ("/" `T.isInfixOf` t)
    ]
  where
    safeChar c =
           ('a' <= c && c <= 'z')
        || ('A' <= c && c <= 'Z')
        || ('0' <= c && c <= '9')
        || c == '/' || c == '.' || c == '_' || c == '-' || c == '+'

-- | Everything the source-preview rule in 'Site.rules' copies to
--   @/source/<path>@, as Hakyll globs. The rule builds its pattern from
--   this list and 'isSourcePath' wraps only what it matches, so every
--   link the filter emits has a target for the popup to fetch; the two
--   used to keep separate copies. Content is not copied wholesale: only
--   the explicitly listed public Crawler Chronicles analysis code is
--   included. Anything unmatched stays unwrapped.
--
--   The JSON under @data\/@ is named file by file rather than globbed:
--   @data\/@ also holds build state and generated indexes that nothing
--   should link to (the former @data\/*.json@ published
--   @archive-state.json@ and the search metadata), and a future tool
--   dropping private JSON there must not become publishable by accident.
--
--   checklist.md is deliberately absent, and must stay absent: audit S01
--   removed it because .gitignore calls it a local planning document and
--   it was being served in full at /source/checklist.md.
sourcePreviewGlobs :: [String]
sourcePreviewGlobs =
    [ "build/**.hs"
    , "static/js/**"
    , "static/css/**"
    , "templates/**"
    , "tools/**.sh"
    , "tools/**.py"
    , "content/essays/crawler_chronicles/data/*.py"
    , "content/essays/crawler_chronicles/data/commit-response/source/*.py"
    , "content/essays/crawler_chronicles/data/commit-response/source/*.sh"
    , "nginx/**.conf"
    , "data/*.yaml"
    , "data/*.md"
    , "data/*.bib"
    , "data/annotations.json"      -- author-written link previews; also served at /data/
    , "data/similar-links.json"    -- generated by tools/embed.py; referenced in prose
    , "*.cabal"
    , "cabal.project"
    , "cabal.project.freeze"
    , "Makefile"
    , "engine.mk"
    , "pyproject.toml"
    , "uv.lock"
    , "LICENSE"
    , "WRITING.md"
    , "PHOTOGRAPHY.md"
    , "README.md"
    ]

sourcePreviewPatterns :: [Pattern]
sourcePreviewPatterns = map fromGlob sourcePreviewGlobs

hasKnownExt :: Text -> Bool
hasKnownExt t =
    let lower = T.toLower t
    in  any (`T.isSuffixOf` lower)
            [ ".hs", ".js", ".mjs", ".css", ".html"
            , ".py", ".cabal", ".md", ".yaml", ".yml"
            , ".toml", ".sh", ".bash", ".svg", ".conf"
            , ".json", ".ini", ".tex", ".bib"
            ]


-- ---------------------------------------------------------------------------
-- File existence cache
-- ---------------------------------------------------------------------------

-- | Process-wide memo of /positive/ @doesFileExist@ results, keyed by
--   the same path the popup will fetch. Hakyll runs this filter once
--   per compiled page and the same source-file references recur across
--   many pages (e.g. @build\/Filters\/Links.hs@ in the Links page,
--   the Colophon, several essays); the cache turns N stats into one
--   per distinct path. Only existence is memoized: a missing file is
--   re-stat'ed on every miss, so a source file created during a
--   long-lived @make watch@ session is picked up on the next rebuild
--   instead of staying "absent" for the process lifetime. (A file
--   /deleted/ mid-watch stays cached as present until restart — the
--   benign direction: the popup fetch 404s and simply never appears.)
--   The build's working directory is the site, and a path names the
--   site's file or, failing that, the engine's ("EngineFiles"), as the
--   @/source/@ rule serves it.
{-# NOINLINE existsCacheRef #-}
existsCacheRef :: IORef (Map.Map Text Bool)
existsCacheRef = unsafePerformIO (newIORef Map.empty)

existsCached :: Text -> IO Bool
existsCached path = do
    cache <- readIORef existsCacheRef
    case Map.lookup path cache of
        Just b  -> pure b
        Nothing -> do
            b <- isJust <$> existingFile (T.unpack path)
            when b $
                atomicModifyIORef' existsCacheRef (\m -> (Map.insert path b m, ()))
            pure b

-- ---------------------------------------------------------------------------
-- Forgejo URL helpers
-- ---------------------------------------------------------------------------

-- | Forgejo source-viewer URL for a repo-relative path, on the configured branch.
forgejoSourceUrl :: SiteConfig -> Text -> Text
forgejoSourceUrl cfg path =
    T.pack (siteSourceRepo cfg ++ "/src/branch/" ++ siteSourceBranch cfg ++ "/")
    <> path

-- | Inverse of 'forgejoSourceUrl': extract the repo-relative path from
--   a Forgejo URL on this site's git host. Recognises both the
--   @\/src\/branch\/<b>\/@ web view and the @\/raw\/branch\/<b>\/@
--   variants. Returns 'Nothing' for any other URL.
forgejoSourcePath :: SiteConfig -> Text -> Maybe Text
forgejoSourcePath cfg url = do
    rest <- T.stripPrefix repoBase url
    afterBranch <-
        case T.stripPrefix "src/branch/" rest of
            Just r  -> Just r
            Nothing -> T.stripPrefix "raw/branch/" rest
    -- The configured branch may itself contain slashes. Recognise it
    -- before the historical single-segment fallback for other branches.
    let path = case T.stripPrefix (T.pack (siteSourceBranch cfg) <> "/") afterBranch of
            Just p -> p
            Nothing -> T.drop 1 (snd (T.breakOn "/" afterBranch))
    if T.null path then Nothing else Just path
  where
    repoBase = T.pack (siteSourceRepo cfg ++ "/")
