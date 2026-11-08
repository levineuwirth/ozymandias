{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Detect repo-relative source-file references in prose and wrap them
--   in a link that triggers a hover-preview popup of the file's contents.
--
--   Two trigger forms:
--
--   * Inline @\`data\/bibliography.bib\`@ — Markdown inline code whose
--     text passes a conservative source-path heuristic.
--   * A Markdown link to
--     the configured source repository, under @(src|raw)/branch/<branch>/<path>@.
--
--   A path the site publishes (site.yaml's @source-preview@) becomes
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
--   A path naming one of the engine's own files, which no site publishes,
--   links to the engine's repository (site.yaml's @engine-repo@) as an
--   ordinary link, without a popup; a link to the site's repository naming
--   one is pointed there too. A site with no @engine-repo@ leaves them as
--   they are.
--
--   Conservative-by-design: the trigger only fires on paths the
--   @/source/@ serving rule actually publishes (the site's
--   @source-preview@, from which that rule builds its pattern), or on the
--   engine's files 'engineSourceGlobs' names. This keeps the parser
--   cheap, avoids false positives on words that happen to contain a
--   slash and a dot, and guarantees every wrapped path has a fetchable
--   @/source/…@ copy.
module Filters.SourceRefs
    ( apply
    , plain
    , forgejoSourceUrl
    , engineSourceGlobs
    ) where

import           Config               (SiteConfig (..))
import           EngineFiles          (engineDir, existingFile)
import           Data.IORef           (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Hakyll               (Pattern, fromFilePath, fromGlob, matches)
import           System.FilePath      ((</>))
import           System.IO.Unsafe     (unsafePerformIO)
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk     (walk, walkM)

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
    let published = map fromGlob (siteSourcePreview cfg)
    afterLinks <- walkM (classifyExistingLink cfg published) doc
    walkM (wrapInlineCode cfg published) afterLinks

-- | For a site without the source-refs switch, which publishes no
--   @/source/@ copies: a link an author marked @.source-ref@ (with its
--   @data-source-path@) becomes an ordinary link, which no popup binds to.
plain :: Pandoc -> Pandoc
plain = walk unmark
  where
    unmark (Link (ident, classes, kvs) ils target)
        | "source-ref" `elem` classes =
            Link (ident, filter (/= "source-ref") classes,
                  filter ((/= "data-source-path") . fst) kvs) ils target
    unmark x = x

-- | Whose file a path in prose names, if it is one to link.
data Owner
    = Site    -- ^ the site's, published under @/source/@
    | Engine  -- ^ the engine's, in the engine's repository

-- | A path the site publishes is the site's even when the engine has a
--   file there too (an override); a path with a site file the site does
--   not publish is not linked at all, so it never leads to the engine's
--   different file. Only a path with no site file is the engine's.
--   Only a path one of the two lists names is looked for on disk.
owner :: SiteConfig -> [Pattern] -> Text -> IO (Maybe Owner)
owner cfg published path
    | not (plausible path) || not (bySite || byEngine) = pure Nothing
    | otherwise = do
        found <- existsCached rel
        pure $ case found of
            Just file
                | file == rel, bySite -> Just Site
                | file == engineDir </> rel, byEngine -> Just Engine
            _ -> Nothing
  where
    rel        = T.unpack path
    matchesAny = any (`matches` fromFilePath rel)
    bySite     = matchesAny published
    byEngine   = not (null (siteEngineRepo cfg)) && matchesAny engineSourcePatterns

-- | Inline @`path`@ → @\<a class="source-ref" data-source-path="path"\>\<code\>path\<\/code\>\<\/a\>@
--   for the site's file, or a plain link to the engine's repository for
--   one of the engine's. The original 'Code' node is preserved as the
--   link's body so the inline-code chrome (mono font, background) survives
--   unchanged.
wrapInlineCode :: SiteConfig -> [Pattern] -> Inline -> IO Inline
wrapInlineCode cfg published orig@(Code (_, cClasses, _) txt)
    | "source-ref" `notElem` cClasses = do
        found <- owner cfg published txt
        pure $ case found of
            Just Site -> Link
                    ( ""
                    , ["source-ref"]
                    , [ ("data-source-path", txt)
                      , ("target",           "_blank")
                      , ("rel",              "noopener noreferrer")
                      ]
                    )
                    [orig]
                    (sourceHref cfg txt, "")
            -- Filters.Links makes it external, as any link to the forge.
            Just Engine -> Link nullAttr [orig] (engineSourceUrl cfg txt, "")
            Nothing -> orig
wrapInlineCode _ _ x = pure x

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

-- | Paths that exist on disk during a build but are not tracked by Git:
--   what the engine's own tools write into a site's @data\/@, which a site
--   may publish (anything not in its @source-preview@ is never wrapped in
--   the first place).
isGeneratedPath :: Text -> Bool
isGeneratedPath path = path `elem` [ "data/similar-links.json" ]

-- | Existing Markdown link to a Forgejo source URL on this site's git
--   host → tagged @source-ref@ and given a @data-source-path@ pointing
--   at the same path the popup provider expects, for a file the site
--   publishes; pointed at the engine's repository, for one of the
--   engine's, which the site's repository does not hold.
classifyExistingLink :: SiteConfig -> [Pattern] -> Inline -> IO Inline
classifyExistingLink cfg published orig@(Link (ident, classes, kvs) ils (url, title))
    | "source-ref" `notElem` classes
    , Just path <- forgejoSourcePath cfg url = do
        found <- owner cfg published path
        pure $ case found of
            Just Site -> Link
                    ( ident
                    , classes ++ ["source-ref"]
                    , kvs ++ [("data-source-path", path)]
                    )
                    ils (url, title)
            Just Engine -> Link (ident, classes, kvs) ils (engineSourceUrl cfg path, title)
            Nothing -> orig
classifyExistingLink _ _ x = pure x

-- ---------------------------------------------------------------------------
-- Heuristic
-- ---------------------------------------------------------------------------

-- | True when the text looks like a repo-relative path: it ends in a
--   known source extension (a root file such as @Makefile@ need not), and
--   contains only safe path characters. Conservative by design — the goal
--   is no false positives on prose that incidentally contains a slash and
--   a dot. Which files are linked at all is the site's @source-preview@'s
--   and 'engineSourceGlobs''s to say.
plausible :: Text -> Bool
plausible t = and
    [ not (T.null t)
    , T.all safeChar t
    , hasKnownExt t || not ("/" `T.isInfixOf` t)
    ]
  where
    safeChar c =
           ('a' <= c && c <= 'z')
        || ('A' <= c && c <= 'Z')
        || ('0' <= c && c <= '9')
        || c == '/' || c == '.' || c == '_' || c == '-' || c == '+'

-- | The engine's files prose may name, as Hakyll globs over the engine's
--   own paths: its code, its templates, scripts and stylesheets, its tools
--   and its packaging. They link to its repository, which holds them;
--   files it downloads or generates are not in these directories.
engineSourceGlobs :: [String]
engineSourceGlobs =
    [ "build/**.hs"
    , "static/js/**"
    , "static/css/**"
    , "templates/**"
    , "tools/**.sh"
    , "tools/**.py"
    , "*.cabal"
    , "engine.mk"
    , "pyproject.toml"
    , "uv.lock"
    , "LICENSE"
    , "README.md"
    ]

engineSourcePatterns :: [Pattern]
engineSourcePatterns = map fromGlob engineSourceGlobs

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

-- | Process-wide memo of /positive/ 'existingFile' results, keyed by the
--   path prose names. Hakyll runs this filter once per compiled page and
--   the same source-file references recur across many pages; the cache
--   turns N stats into one per distinct path. Only existence is memoized:
--   a missing file is re-stat'ed on every miss, so a source file created
--   during a long-lived @make watch@ session is picked up on the next
--   rebuild instead of staying "absent" for the process lifetime. (A file
--   /deleted/ mid-watch stays cached as present until restart — the
--   benign direction: the popup fetch 404s and simply never appears.)
--   The build's working directory is the site, and a path names the
--   site's file or, failing that, the engine's ("EngineFiles").
{-# NOINLINE existsCacheRef #-}
existsCacheRef :: IORef (Map.Map FilePath FilePath)
existsCacheRef = unsafePerformIO (newIORef Map.empty)

existsCached :: FilePath -> IO (Maybe FilePath)
existsCached path = do
    cache <- readIORef existsCacheRef
    case Map.lookup path cache of
        Just file -> pure (Just file)
        Nothing -> do
            found <- existingFile path
            case found of
                Just file -> atomicModifyIORef' existsCacheRef (\m -> (Map.insert path file m, ()))
                Nothing   -> pure ()
            pure found

-- ---------------------------------------------------------------------------
-- Forgejo URL helpers
-- ---------------------------------------------------------------------------

-- | Forgejo source-viewer URL for a repo-relative path, on the configured branch.
forgejoSourceUrl :: SiteConfig -> Text -> Text
forgejoSourceUrl cfg path =
    T.pack (siteSourceRepo cfg ++ "/src/branch/" ++ siteSourceBranch cfg ++ "/")
    <> path

-- | The engine's repository's source-viewer URL for one of its files, on
--   its main branch, which every site builds from (no pins).
engineSourceUrl :: SiteConfig -> Text -> Text
engineSourceUrl cfg path = T.pack (siteEngineRepo cfg ++ "/src/branch/main/") <> path

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
