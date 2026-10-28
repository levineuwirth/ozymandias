{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
module Site (rules, siteConfiguration, siteConfigurationFor, refusedPath) where

import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Except (catchError)
import Data.Char     (isSpace, toUpper)
import Data.List     (groupBy, isInfixOf, isPrefixOf, isSuffixOf, nub, sort, sortBy)
import Data.Map.Strict (Map)
import Data.Maybe    (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Ord      (Down (..), comparing)
import Data.Set      (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import System.Directory (listDirectory)
import System.FilePath (splitDirectories, takeDirectory, takeFileName, takeExtension,
                        dropExtension, (</>))
import Text.Read     (readMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import Hakyll hiding (escapeHtml)
import Config     (SiteConfig (..), FeedSpec (..), FeedSection (..), LibraryConfig (..),
                   NavLink (..), Portal (..), Standalone (..), PageKind (..),
                   engineSources, standaloneDir, configRules, manifestRules,
                   readSiteConfig, loadConfig, loadFeed, renderSiteFeed, photoFeed)
import Archive    (archiveRules)
import Authors    (buildAllAuthors, applyAuthorRules)
import Backlinks  (backlinkRules)
import FooterData (footerRules)
import BibExtras  (BibExtra (..), emptyBibExtra, firstAuthorSurname, parseBibExtras)
import Citations  (cslStyles, renderBibliographyHtml)
import Compilers  (essayCompiler, postCompiler, pageCompiler, poetryCompiler, fictionCompiler,
                   compositionCompiler, sidecarCompiler)
import Catalog      (musicCatalogCtx)
import Commonplace  (commonplaceCtx)
import EngineFiles  (engineTwins, siteRelative, staticImageDimensions, staticRules, templateRules)
import Now          (nowCtx, nowLastUpdated)
import Vita         (vitaCtx, projectsCtx)
import Contexts   (feedTitleField, siteCtx, essayCtx, postCtx, pageCtx, poetryCtx, fictionCtx, compositionCtx,
                   contentKindField, declaresScore, recentFirstByDisplay,
                   tagLinksFieldExcludingTopSegment,
                   canonicalUrlPath, feedMetaFields, identifierDisplayUTC)
import qualified Filters.SourceRefs as SR
import MarkdownSections (markdownSectionsCompiler)
import Marks      (monogramPattern)
import qualified Patterns as P
import Photography (photographyRules)
import Tags       (buildAllTags, applyTagRules, sidecarIdentifier,
                   portalIntroField, portalTooltipField)
import Pagination (blogPaginateRules)
import Stability  (resolveStability)
import Stats      (statsRules)
import Utils      (cacheDirFor, confidencePercent, contentPageRoute, escapeHtml, formatIso, inDefault,
                   isDevBuild, metadataKeywords, outputDirFor, stripPrefixRoute,
                   trustScore)

-- ---------------------------------------------------------------------------
-- Publication boundary
-- ---------------------------------------------------------------------------

-- | Hakyll provider configuration.
--
--   @.gitignore@ and this function protect two different things and
--   neither substitutes for the other: @.gitignore@ keeps a file out of
--   *commits*; 'ignoreFile' keeps it out of the *deployment*. A file Git
--   ignores is still sitting in the provider directory, and the broad
--   rules below (@static\/**@, the co-located essay-asset rule, the
--   per-page JS rule) would otherwise turn it into an identifier, route
--   it, and copy it into @_site\/@ — from where @make deploy@ rsyncs it
--   to the public document root. A probe confirmed exactly that for a
--   @.key@ file and a @.local.md@ note, both correctly Git-ignored.
--
--   Ignoring at the provider boundary is stronger than excluding at each
--   rule: an ignored path never becomes an identifier at all, so no
--   present or future rule, listing, feed, search index, or @\/source\/@
--   copy can reach it.
--
--   The patterns mirror the credential / private-note / editor-junk block
--   of @.gitignore@; the two lists are meant to agree, so extend both.
--   Hakyll's own defaults (dotfiles, @#…#@ autosaves, @…~@ backups,
--   @.swp@) are preserved by delegating to 'defaultConfiguration' first
--   rather than replacing the function.
siteConfiguration :: Configuration
siteConfiguration = siteConfigurationFor False

-- | 'siteConfiguration' with the output and store a dev build uses when
--   given True (see 'Utils.outputDirFor').
siteConfigurationFor :: Bool -> Configuration
siteConfigurationFor dev = defaultConfiguration
    { ignoreFile = \path ->
        ignoreFile defaultConfiguration path || neverPublish path || notSiteInput path
    , destinationDirectory = outputDirFor dev
    , storeDirectory       = cacheDirFor dev
    , tmpDirectory         = cacheDirFor dev ++ "/tmp"
    }

-- | Whether the build refuses a path: Hakyll applies 'ignoreFile' to each
--   file and directory name as it walks the tree, so one refused component
--   keeps everything beneath it out. @site private-paths@ answers this for
--   the tools and tests, which used to approximate it with copies of the
--   lists below.
refusedPath :: FilePath -> Bool
refusedPath = any (ignoreFile siteConfiguration) . splitDirectories

-- | Paths that must never become Hakyll identifiers. See
--   'siteConfiguration'. Everything tools/check-site.py refuses at the end
--   is refused here first, so a private file fails the build where it
--   starts, not the gate after it has been rendered.
neverPublish :: FilePath -> Bool
neverPublish path =
       "__pycache__" `elem` splitDirectories path
    || any (`isSuffixOf` name) suffixes
    || any (`isPrefixOf` name) prefixes
    || any (`isInfixOf` name) infixes
    || name `elem` exactNames
  where
    name = takeFileName path

    suffixes =
        -- Private working notes.
        [ ".local.md", ".local.html"
        -- Key material and credential bundles.
        , ".key", ".pem", ".p12", ".pfx", ".env"
        -- Editor and interpreter junk.
        , "~", ".swp", ".swo", ".pyc", ".pyo"
        -- Interrupted or in-flight writes.
        , ".tmp", ".part", ".partial", ".log"
        ]

    prefixes =
        -- id_rsa, id_rsa.pub, id_ed25519, credentials.json, .env.local, …
        [ "id_rsa", "id_dsa", "id_ecdsa", "id_ed25519", "credentials", ".env." ]

    infixes =
        -- Unfinished drafts, whatever their type: notes.draft.md, fig.draft.svg.
        [ ".draft." ]

    -- Dotfiles are already covered by Hakyll's default predicate; naming
    -- them keeps the intent legible if that default ever changes.
    exactNames =
        [ ".env", ".DS_Store", ".netrc", ".npmrc", ".pypirc"
        -- Planning checklists, at any depth.
        , "checklist.md" ]

-- | Directories in the site's tree that are not its input: the engine's
--   demonstration site, which has content of its own, and cabal's build
--   directory under @engine/@ as at the root. Unlike 'neverPublish' they
--   are not private, so Git keeps them. Hakyll passes 'ignoreFile' a bare
--   name, never a path, so they are refused by name at any depth. The
--   engine's @tests/@ cannot be told from a site's that way: it is walked,
--   and no rule claims it.
notSiteInput :: FilePath -> Bool
notSiteInput path = takeFileName path `elem` ["fixture-site", "dist-newstyle"]

-- | One library shelf: a portal and the cards it shows, and whether the
--   portal holds more than that.
data Shelf = Shelf
    { shelfPortal  :: Portal
    , shelfEntries :: [Item String]
    , shelfHasMore :: Bool
    }

-- | A portal as the home page and the tag pages take it: (name, tag).
portalPair :: Portal -> (String, String)
portalPair p = (portalName p, portalTag p)

-- | The rules for one of site.yaml's standalone pages: the page at its
--   route, compiled with its context's compiler and rendered into its
--   template, and, with @scores@, the score fragments beside its source.
--
--   Co-located score fragments are inlined by Filters.Score inside
--   'unsafeCompiler' — Hakyll sees the page depending on its source and
--   nothing else, so re-engraving a score left the page serving the old
--   SVG (audit B11; the essay figure dependency is the model). The page
--   depends on the fragments' pattern, which their copy rule claims. The
--   Score filter reads paths relative to the source's directory, which is
--   why a page with scores lives in a directory of its own.
standaloneRules :: Standalone -> Rules ()
standaloneRules page = do
    scoreDeps <- case (standaloneScores page, standaloneDir page) of
        (True, Just dir) -> do
            let scores = fromGlob ("content/" ++ dir ++ "/scores/*.svg")
            match scores $ do
                route   $ stripPrefixRoute ("content/" ++ dir ++ "/")
                compile copyFileCompiler
            (: []) <$> makePatternDependency scores
        _ -> return []
    rulesExtraDependencies scoreDeps $ match (fromList [fromFilePath (standaloneSource page)]) $ do
        route   $ constRoute (standaloneRoute page)
        compile $ compiler
            >>= inDefault (fromFilePath ("templates/" ++ standaloneTemplate page)) (flags <> ctx)
  where
    flags = mconcat [constField flag "true" | flag <- standaloneFlags page]
    (compiler, ctx) = case standaloneContext page of
        EssayPage    -> (essayCompiler, essayCtx)
        PlainPage    -> (pageCompiler, pageCtx)
        NowPage      -> (pageCompiler, nowCtx)
        VitaPage     -> (pageCompiler, vitaCtx)
        ProjectsPage -> (pageCompiler, projectsCtx)

-- | Optional prose intro lifted into @$library-intro$@ on the library
--   page. Matched but not routed; consumed via the @"body"@ snapshot.
libraryIntroId :: Identifier
libraryIntroId = fromFilePath "content/library.md"

-- | Item context shared by both Atom feeds.
--
--   Three timestamps, three meanings (audit F11):
--
--     * @published@ — the creation date. Never moves.
--     * @updated@   — the most recent /substantive/ revision, from the
--                     @revised:@ frontmatter the cards already read
--                     ('itemDisplayUTC'), falling back to the creation
--                     date for a piece that has not been revised.
--     * the build time — kept out of both. A rebuild is not a revision,
--       and stamping one here would push every entry to the top of every
--       subscriber's reader on every deploy.
--
--   Before this, @updated@ was @dateField@ — the creation date — so the
--   branch-capture essay advertised its July original as its September
--   revision and no subscriber could see that it had changed.
feedCtx :: Context String
feedCtx =
    feedTitleField
    <> feedMetaFields
    <> bodyField "description"
    <> defaultContext

-- | Context for the home page. Extends 'pageCtx' with a @portals@
-- listField iterating site.yaml's portals in order. Each item exposes
-- @$portal-name$@, @$portal-url$@, and (if the sidecar's tooltip is
-- populated) @$portal-tooltip$@, consumed by @templates/home.html@.
-- Tooltip lookup uses 'portalTooltipField' — the same function
-- 'Tags.applyTagRules' uses on per-tag pages — so the two surfaces
-- stay in lockstep on suppression and missing-file semantics.
homeCtx :: Context String
homeCtx = listField "portals" portalItemCtx portalItems <> pageCtx
  where
    portalItems :: Compiler [Item (String, String)]
    portalItems = map (Item (fromFilePath "") . portalPair) . sitePortals <$> loadConfig

    portalItemCtx :: Context (String, String)
    portalItemCtx =
           field "portal-name" (return . escapeHtml . fst . itemBody)
        <> field "portal-url"  (\i -> return $ "/" ++ snd (itemBody i) ++ "/")
        <> portalTooltipField (sidecarIdentifier . snd . itemBody)

rules :: Rules ()
rules = do
    cfg <- preprocess readSiteConfig
    configRules
    manifestRules
    -- ---------------------------------------------------------------------------
    -- Build mode. SITE_ENV=dev (set by `make dev` / `make watch`) includes
    -- drafts under content/drafts/**; anything else (unset, "deploy", "build")
    -- excludes them entirely from every match, listing, and asset rule below.
    -- ---------------------------------------------------------------------------
    isDev <- preprocess isDevBuild
    twins <- engineTwins
    let allEssays = if isDev
                    then P.essayPattern .||. P.draftEssayPattern
                    else P.essayPattern

        -- Everything compiled through 'Compilers.essayCompilerWith', which
        -- saves each page's "cite-keys" snapshot for /bibliography/.
        essayPipeline = allEssays .||. P.blogPattern .||. P.poetryPattern
                   .||. P.fictionPattern .||. "content/music/*/index.md"
                   .||. fromList [ fromFilePath (standaloneSource page)
                                 | page <- siteStandalone cfg
                                 , standaloneContext page == EssayPage ]

        -- Co-located assets copied as they are (rules below). Named because
        -- the monogram rule must not claim what these already do.
        essayAssets = "content/essays/**"
                      .&&. complement P.essayPattern
                      .&&. complement "content/essays/**/*.dims.yaml"
        musicAssets = foldr1 (.||.) [ fromGlob ("content/music/**/*." ++ ext)
                                    | ext <- ["svg", "mp3", "pdf", "webp", "json"] ]

    -- ---------------------------------------------------------------------------
    -- Backlinks (pass 1: link extraction; pass 2: JSON generation)
    -- Must run before content rules so dependencies resolve correctly.
    -- ---------------------------------------------------------------------------
    backlinkRules cfg

    -- ---------------------------------------------------------------------------
    -- Author index pages
    -- ---------------------------------------------------------------------------
    authors <- buildAllAuthors (siteAuthor cfg)
    applyAuthorRules authors siteCtx

    -- ---------------------------------------------------------------------------
    -- Tag-meta sidecars — optional prose intros + tooltips for tag index
    -- pages and the home-page portal grid. Matched but not routed: the
    -- rendered body is exposed only via the @"body"@ snapshot and the
    -- @tooltip:@ frontmatter key is read through 'getMetadata' by the
    -- consumers (Tags.hs, home-page rule). Registered before tag rules so
    -- snapshot loads during tag-page compilation find a compiled target.
    --
    -- Two-pattern union: Hakyll's @**/*@ glob requires at least one
    -- subdirectory level, so flat sidecars (@content/tag-meta/nonfiction.md@)
    -- and nested sidecars (@content/tag-meta/nonfiction/philosophy.md@) must
    -- each be named by their own level-specific pattern.
    -- ---------------------------------------------------------------------------
    match ("content/tag-meta/*.md" .||. "content/tag-meta/**/*.md") $
        compile sidecarCompiler

    -- ---------------------------------------------------------------------------
    -- Tag index pages
    -- ---------------------------------------------------------------------------
    tags <- buildAllTags cfg
    applyTagRules tags (map portalPair (sitePortals cfg)) siteCtx
    statsRules tags

    -- Per-page JS files — authored alongside content in content/**/*.js.
    -- Draft JS is handled by a separate dev-only rule below.
    match ("content/**/*.js" .&&. complement "content/drafts/**") $ do
        route   $ stripPrefixRoute "content/"
        compile copyFileCompiler

    -- Per-page JS co-located with draft essays (dev-only).
    when isDev $ match "content/drafts/**/*.js" $ do
        route   $ stripPrefixRoute "content/"
        compile copyFileCompiler

    -- Static files and templates, the site's and the engine's that no site
    -- file overrides ("EngineFiles").
    staticRules twins
    templateRules twins

    -- The repository and the public site carry the same licence texts.
    match (fromList ["LICENSE", "LICENSE-CONTENT"]) $ do
        route $ customRoute $ \ident -> "licenses/" ++
            (if toFilePath ident == "LICENSE" then siteCodeLicense cfg else siteContentLicense cfg) ++ ".txt"
        compile copyFileCompiler
    match "THIRD-PARTY.md" $ do
        route $ constRoute "licenses.html"
        compile $ pageCompiler
            >>= inDefault "templates/page.html" pageCtx

    -- ---------------------------------------------------------------------------
    -- Source-preview corpus — raw copies of source files, served at
    -- @/source/<path>@, fetched on hover by the popup provider in
    -- @static/js/popups.js@ (sourceContent → Prism highlighting).
    --
    -- The whitelist is 'Filters.SourceRefs.sourcePreviewGlobs', which is
    -- also what 'isSourcePath' matches, so every link the filter emits has
    -- a @/source/…@ target. Files in @static/@ are also served under their
    -- normal /js/, /css/ paths via a separate rule above; the
    -- @"source-preview"@ version lets Hakyll compile the same identifier
    -- twice without conflict. An engine file is served at its site path,
    -- unless a site file there overrides it.
    -- ---------------------------------------------------------------------------
    let sourcePreviewable = foldr1 (.||.) [fromGlob g .||. twins g | g <- SR.sourcePreviewGlobs]
    match sourcePreviewable $ version "source-preview" $ do
        route   $ customRoute (\ident -> "source/" ++ siteRelative (toFilePath ident))
        compile copyFileCompiler

    -- Markdown among them also gets a sectioned rendering, so a popup can
    -- show a .md file as prose, or just the section a #fragment names
    -- (MarkdownSections; renderMarkdownPopup in static/js/popups.js).
    match (sourcePreviewable .&&. "**.md") $ version "source-sections" $ do
        route   $ customRoute (\ident -> "source/" ++ siteRelative (toFilePath ident) ++ ".sections.json")
        compile markdownSectionsCompiler

    -- Code-reference snapshots — GitHub blobs, trees and commits that prose
    -- links to, fetched by tools/code-refs.py and served at /code-refs/…
    -- for the popup (codeRefContent in static/js/popups.js). Blob bodies
    -- carry a .txt suffix so nothing from another repository is ever served
    -- as a document on this origin. The index is read directly by
    -- Filters.CodeRefs and is not published.
    match ("code-refs/**" .&&. complement "code-refs/index.json") $ do
        route   idRoute
        compile copyFileCompiler

    -- …and each Markdown blob's sectioned rendering beside it:
    -- <path>.md.txt -> <path>.md.sections.json.
    match "code-refs/**.md.txt" $ version "sections" $ do
        route   $ customRoute (\ident ->
            dropExtension (toFilePath ident) ++ ".sections.json")
        compile markdownSectionsCompiler

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

    -- Each page's Backlinks and Related entries, split out of
    -- data/backlinks.json and tools/embed.py's data/similar-links.json by
    -- `site footer-data` between the two compile passes (build/FooterData.hs).
    footerRules

    -- Bibliography inputs — the @.bib@ databases and the CSL style.
    --
    -- Matched (not routed) so that every consumer can 'load' them and get
    -- real dependency tracking. Everything that renders bibliography HTML
    -- reads these through citeproc inside 'unsafeCompiler', which Hakyll
    -- cannot see; before this rule the CSL file was claimed by no rule at
    -- all and the @.bib@ files only in their @"source-preview"@ version,
    -- so neither could ever be a dependency. (See the note above the
    -- @*.dims.yaml@ rule: an identifier no rule claims never enters
    -- Hakyll's universe, and a dependency on it can never fire.)
    --
    -- Consumers: the essay pipeline (Compilers.trackBibliographyInputs)
    -- and the synthetic /bibliography/ index and keyword pages below.
    match ("data/*.bib" .||. cslStyles) $ compile getResourceString

    -- Commonplace YAML — compiled as a raw string so it can be loaded
    -- with dependency tracking by the commonplace page compiler.
    match "data/commonplace.yaml" $ compile getResourceBody

    -- Now YAML — same pattern as commonplace. Loaded by Now.nowCtx for
    -- /current.html. Re-compiles current.html when the YAML changes.
    match "data/now.yaml" $ compile getResourceBody

    -- CV/résumé YAML — the same files the PDF pipeline builds from
    -- (yaml-source/data/). Loaded by Vita.vitaCtx so /about.html renders
    -- education, publications, presentations, and experience from one
    -- source instead of a hand-typed copy. Matching them here is what
    -- makes about.html recompile when the CV data changes.
    match "yaml-source/data/*.yml" $ compile getResourceBody

    -- Per-build stamp — written by Main.main before Hakyll starts, so it
    -- always exists and always differs from the previous run. Matched
    -- (not routed) purely so the telemetry pages can `load` it as a
    -- dependency and thus recompile every build instead of serving a
    -- stale cached copy. See build/Stats.hs.
    match "data/build-stamp.txt" $ compile getResourceBody

    -- ---------------------------------------------------------------------------
    -- Homepage
    -- ---------------------------------------------------------------------------
    match "content/index.md" $ do
        route   $ constRoute "index.html"
        compile $ pageCompiler
            >>= inDefault "templates/home.html" homeCtx

    -- ---------------------------------------------------------------------------
    -- Standalone pages: site.yaml's table (me, colophon, current, …)
    -- ---------------------------------------------------------------------------
    forM_ (siteStandalone cfg) standaloneRules

    -- A Markdown file in a standalone page's own directory that no entry
    -- claims would be compiled by no rule at all (the directory is kept
    -- from page collections), so refuse it instead of dropping it.
    let claimed = map standaloneSource (siteStandalone cfg)
    forM_ (nub (mapMaybe standaloneDir (siteStandalone cfg))) $ \dir -> do
        pages <- getMatches (fromGlob ("content/" ++ dir ++ "/*.md"))
        let unclaimed = filter (`notElem` claimed) (map toFilePath pages)
        unless (null unclaimed) $ preprocess $ ioError $ userError $
            "site.yaml: no standalone entry for " ++ unwords unclaimed
            ++ " (content/" ++ dir ++ "/ belongs to a standalone page)"

    -- ---------------------------------------------------------------------------
    -- Commonplace book
    -- ---------------------------------------------------------------------------
    match "content/commonplace.md" $ do
        route   $ constRoute "commonplace.html"
        compile $ pageCompiler
            >>= inDefault "templates/commonplace.html" commonplaceCtx

    -- Every other top-level page.
    match ("content/*.md" .&&. complement (fromList (map fromFilePath engineSources)
                                             .||. P.standaloneSources cfg)) $ do
        route   contentPageRoute
        compile $ pageCompiler
            >>= inDefault "templates/page.html" pageCtx

    -- Generic page collections
    -- (content/<collection>/<slug>.md → <collection>/<slug>.html).
    match (P.pageCollectionPattern cfg) $ do
        route   contentPageRoute
        compile $ pageCompiler
            >>= inDefault "templates/page.html" pageCtx

    -- ---------------------------------------------------------------------------
    -- Essays — flat (content/essays/foo.md → essays/foo.html) and
    --          directory-based (content/essays/slug/index.md → essays/slug/index.html).
    --          In dev mode, drafts under content/drafts/essays/ route to
    --          drafts/essays/foo.html (flat) or drafts/essays/slug/index.html (dir).
    -- ---------------------------------------------------------------------------
    -- Figures are executed at compile time by Filters.Viz, but nothing in
    -- Hakyll's graph connects an essay to the scripts and data those figures
    -- read: the page depends on index.md alone. Editing a benchmark CSV
    -- therefore republished the *data file* (it is copied by the asset rule
    -- below) while leaving the chart drawn from it untouched — a page whose
    -- figure contradicts the numbers sitting next to it, with nothing said
    -- on stdout. tools/build-freshness.sh does not cover this either: it
    -- hashes build/**/*.hs, not content assets or tools/.
    --
    -- tools/viz_theme.py is in the pattern for the same reason. Every figure
    -- script imports it, so a change to the shared theme has to invalidate
    -- every page that draws one.
    --
    -- This is deliberately coarse: one dependency for all essays, so any
    -- figure edit rebuilds every essay rather than only its own. At the
    -- current corpus that is a few seconds and it cannot be wrong. Making it
    -- per-essay is worth doing when figure count makes it hurt, and wants the
    -- content-addressed cache alongside it.
    -- The same hole, one filter over: Filters.Images reads *.dims.yaml
    -- sidecars at compile time to attach width/height. Verified before the
    -- fix — editing a sidecar from width: 1176 to width: 999 and rebuilding
    -- left the page byte-identical, still serving the old dimensions. Milder
    -- than the figure case (a wrong layout hint, not a wrong claim) but the
    -- same silence, so it belongs in the same dependency.
    --
    -- Hakyll records a pattern dependency by matching the files on disk
    -- (unversioned), and re-checks it every run against the identifiers some
    -- rule claims, where a glob also matches versioned ones. So the pattern is
    -- restricted to unversioned identifiers, every file in it must be claimed
    -- without a version (viz_theme.py is, just below, besides its
    -- source-preview copy), and the drafts globs apply only in dev, where a
    -- rule claims drafts. Otherwise the two sets differ on every run and
    -- every essay is out of date on every build (audit H05).
    figureDep <- makePatternDependency $ hasNoVersion .&&.
                     (     "content/essays/**/figures/**"
                      .||. "content/essays/**/*.dims.yaml"
                      -- Essays embed site-wide images from /images/; their
                      -- dimension sidecars live under either static/ root and are read by
                      -- the same filter (B11).
                      .||. staticImageDimensions
                      .||. "tools/viz_theme.py"
                      .||. "engine/tools/viz_theme.py"
                      .||. (if isDev
                              then "content/drafts/essays/**/figures/**"
                                   .||. "content/drafts/essays/**/*.dims.yaml"
                              else fromList []))
    match ("tools/viz_theme.py" .||. "engine/tools/viz_theme.py") $ compile getResourceLBS

    rulesExtraDependencies [figureDep] $ match allEssays $ do
        -- content/essays/foo.md → essays/foo.html, a directory essay or
        -- collection entry keeps its path below content/, and a dev draft
        -- under content/drafts/essays/ lands under drafts/essays/.
        route contentPageRoute
        compile $ essayCompiler
            >>= saveSnapshot "content"
            >>= inDefault "templates/essay.html" essayCtx

    -- Dimension sidecars: tracked, but not shipped.
    --
    -- These are consumed by Filters/Images.hs at compile time, so the asset
    -- rule below deliberately excludes them from _site — which left them
    -- matched by no rule at all, and therefore outside Hakyll's universe.
    -- A makePatternDependency naming them was consequently inert. Hakyll
    -- computes its modified set as
    --   Set.filter (resourceModified provider) (Map.keysSet universe)
    -- (Core/Runtime.hs), and the universe is the identifiers some rule
    -- claims — so an unclaimed file can never enter it, and a dependency
    -- pointing at one can never fire. tools/viz_theme.py works in that same
    -- dependency only because "tools/**.py" is claimed by the source-preview
    -- rule above.
    --
    -- Compiling with no route registers them as tracked inputs and writes
    -- nothing.
    match "content/essays/**/*.dims.yaml" $ compile getResourceLBS
    when isDev $
        match "content/drafts/essays/**/*.dims.yaml" $ compile getResourceLBS

    -- Monograms: the same blind spot. Marks.hs inlines them inside
    -- 'unsafeCompiler' and records a dependency on each candidate path,
    -- which fires only for a path some rule claims. The essay, music and
    -- photography asset rules claim (and copy) theirs; this claims the rest
    -- (standalone pages, /me/, /music/, poetry), with no route. Drafts' are
    -- claimed by the dev asset rule, or not built.
    match (monogramPattern
           .&&. complement (essayAssets .||. musicAssets
                            .||. "content/photography/**" .||. "content/drafts/**")) $
        compile getResourceLBS

    -- Static assets co-located with directory-based essays (figures, data, PDFs, …).
    -- Build-time dimension sidecars are excluded; they're consumed by
    -- Filters/Images.hs at compile time, not shipped.
    match essayAssets $ do
        route $ stripPrefixRoute "content/"
        compile copyFileCompiler

    -- Static assets co-located with draft essays (dev-only).
    when isDev $ match ("content/drafts/essays/**"
                        .&&. complement "content/drafts/essays/*.md"
                        .&&. complement "content/drafts/essays/*/index.md"
                        .&&. complement "content/drafts/essays/**/*.dims.yaml") $ do
        route $ stripPrefixRoute "content/"
        compile copyFileCompiler

    -- ---------------------------------------------------------------------------
    -- Blog posts
    -- ---------------------------------------------------------------------------
    match P.blogPattern $ do
        route   contentPageRoute
        compile $ postCompiler
            >>= saveSnapshot "content"
            >>= inDefault "templates/blog-post.html" postCtx

    -- Blog collection index pages
    -- (content/blog/<collection>/index.md → blog/<collection>/index.html).
    match "content/blog/*/index.md" $ do
        route   contentPageRoute
        compile $ pageCompiler
            >>= loadAndApplyTemplate "templates/default.html" pageCtx
            >>= relativizeUrls

    -- ---------------------------------------------------------------------------
    -- Poetry
    -- ---------------------------------------------------------------------------
    -- All poems — flat (content/poetry/sonnet-60.md) and collection
    -- (content/poetry/shakespeare-sonnets/sonnet-1.md) forms share one
    -- rule; collection index pages are excluded by 'P.poetryPattern'
    -- itself and matched separately below.
    match P.poetryPattern $ do
        route   contentPageRoute
        compile $ poetryCompiler
            >>= saveSnapshot "content"
            >>= inDefault "templates/reading.html" poetryCtx

    -- Collection index pages (e.g. content/poetry/shakespeare-sonnets/index.md).
    -- See 'collectionCtx' for the rendering contract. A landing that is
    -- still scaffolding carries @draft: true@ and never reaches this rule in
    -- production (build/Drafts.hs; audit C05, C06).
    match "content/poetry/*/index.md" $ do
        route   contentPageRoute
        compile $ do
            ctx <- collectionCtx P.poetryPattern poetryCtx pageCtx
            pageCompiler
                >>= inDefault "templates/collection.html" ctx

    -- ---------------------------------------------------------------------------
    -- Fiction
    -- ---------------------------------------------------------------------------
    match P.fictionPattern $ do
        route   contentPageRoute
        compile $ fictionCompiler
            >>= saveSnapshot "content"
            >>= inDefault "templates/reading.html" fictionCtx

    -- Fiction collection index pages
    -- (content/fiction/<collection>/index.md → fiction/<collection>/index.html).
    match "content/fiction/*/index.md" $ do
        route   contentPageRoute
        compile $ do
            ctx <- collectionCtx P.fictionPattern fictionCtx pageCtx
            pageCompiler
                >>= inDefault "templates/collection.html" ctx

    -- ---------------------------------------------------------------------------
    -- Music — catalog index
    -- ---------------------------------------------------------------------------
    -- Every rule below whose context reads scores carries this one pattern.
    -- Same unsafeCompiler blind spot as the standalone pages' score
    -- fragments (B11): Filters.Score reads content/music/<slug>/scores/*.svg
    -- relative to the source file. The SVG copy rule claims them, so the
    -- dependency can fire. The timing map is in the pattern too: the
    -- reader's 'realization' field only asks whether it exists, so adding or
    -- removing a realization would otherwise leave the reader stale. It is
    -- also the only dependency on the score pages that 'scorePageList'
    -- lists, which records none of its own (audit H06).
    musicScoreDep <- makePatternDependency $
                         "content/music/**/*.svg" .||. "content/music/**/*.json"
                         .||. "content/music/**/*.webp"

    rulesExtraDependencies [musicScoreDep] $ match "content/music/index.md" $ do
        route   $ constRoute "music/index.html"
        compile $ do
            ctx <- musicCatalogCtx
            pageCompiler >>= inDefault "templates/music-catalog.html" ctx

    -- ---------------------------------------------------------------------------
    -- Music — composition landing pages + score reader
    -- ---------------------------------------------------------------------------

    -- Static assets served unchanged: SVG score pages, audio, PDF; page 1's
    -- thumbnail (tools/music-import.py), for the frontispiece and the
    -- /music/ shelf; and a realization's timing map (music-import.py
    -- --audio): when each bar sounds and where it stands on its page, read
    -- by score-follow.js.
    match musicAssets $ do
        route   $ stripPrefixRoute "content/"
        compile copyFileCompiler

    -- Landing page — full essay pipeline. Its score dependency is
    -- 'musicScoreDep', above the catalog.

    rulesExtraDependencies [musicScoreDep] $ match "content/music/*/index.md" $ do
        route   contentPageRoute
        compile $ compositionCompiler
            >>= saveSnapshot "content"
            >>= inDefault "templates/composition.html" compositionCtx

    -- Score reader — separate URL, minimal chrome.
    -- Compiled from the same source with version "score-reader".
    --
    -- Gated on the composition declaring a score. Ungated, every
    -- composition got a live /music/<slug>/score/ whose reader had nothing
    -- to show: an empty <img>, a "p. 1 / 0" counter, and an enabled Next
    -- button. 'declaresScore' is metadata-only because 'matchMetadata'
    -- runs before the Compiler monad exists — a 'score-dir' naming an
    -- empty directory still routes a page, which 'scorePageList' then
    -- reports honestly as zero pages.
    rulesExtraDependencies [musicScoreDep] $
      matchMetadata "content/music/*/index.md" declaresScore $
        version "score-reader" $ do
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
    -- Photography — single-photo entries, asset copy, and section landing.
    -- See build/Photography.hs and PHOTOGRAPHY.md for the design.
    -- ---------------------------------------------------------------------------
    photographyRules cfg

    -- ---------------------------------------------------------------------------
    -- Archive — link-archiving system: per-entry /archive/<slug>/ pages and
    -- the /archive/ index, driven by archive/manifest.yaml + PROVENANCE.json.
    -- See build/Archive.hs and ARCHIVE.md for the design.
    -- ---------------------------------------------------------------------------
    archiveRules

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
                    <> constField "title"  "Essays"
                    <> constField "portal" "true"
                    -- C01: an explicit description, ahead of siteCtx's
                    -- fallback. Left to the fallback, a list page's
                    -- description is the first *item's* opening
                    -- paragraph, which describes one essay and claims
                    -- to describe the index.
                    <> constField "description"
                        "Every essay published here, newest first."
                    <> siteCtx
            makeItem ""
                >>= inDefault "templates/essay-index.html" ctx

    -- ---------------------------------------------------------------------------
    -- Poetry index
    -- ---------------------------------------------------------------------------
    -- Nav, the home portal grid, and the library all link /poetry/; this
    -- rule is what keeps those links from 404ing. Lists flat poems and
    -- collection poems alike; collection index pages are excluded by
    -- 'P.poetryPattern' itself.
    create ["poetry/index.html"] $ do
        route idRoute
        compile $ do
            poems <- recentFirst =<< loadAll (P.poetryPattern .&&. hasNoVersion)
            let ctx =
                    listField "essays" poetryCtx (return poems)
                    <> constField "title"  "Poetry"
                    <> constField "portal" "true"
                    <> constField "description"
                        "Every poem published here, newest first."
                    <> siteCtx
            makeItem ""
                >>= inDefault "templates/essay-index.html" ctx

    -- ---------------------------------------------------------------------------
    -- Fiction index
    -- ---------------------------------------------------------------------------
    -- Same rationale as the poetry index. content/fiction/ has no entries
    -- yet; an empty match list renders an empty index rather than a 404.
    create ["fiction/index.html"] $ do
        route idRoute
        compile $ do
            stories <- recentFirst =<< loadAll (P.fictionPattern .&&. hasNoVersion)
            let ctx =
                    listField "essays" fictionCtx (return stories)
                    <> constField "title"  "Fiction"
                    <> constField "portal" "true"
                    <> constField "description"
                        "Every piece of fiction published here, newest first."
                    <> siteCtx
            makeItem ""
                >>= inDefault "templates/essay-index.html" ctx

    -- ---------------------------------------------------------------------------
    -- New page — all content sorted by creation date, newest first
    -- ---------------------------------------------------------------------------
    create ["new.html"] $ do
        route idRoute
        compile $ do
            let allContent = (   allEssays
                             .||. P.blogPattern
                             .||. P.fictionPattern
                             .||. P.poetryPattern
                             .||. P.musicPattern
                             ) .&&. hasNoVersion
            items <- recentFirstByDisplay =<< loadAll allContent
            let itemCtx = contentKindField
                       <> essayCtx
                ctx = listField "recent-items" itemCtx (return items)
                   <> constField "title" "New"
                   <> constField "list-page" "true"
                   <> constField "description"
                        "Everything published or revised here, newest first."
                   <> siteCtx
            makeItem ""
                >>= inDefault "templates/new.html" ctx

    -- ---------------------------------------------------------------------------
    -- Library intro — optional prose block (typically a blockquote) lifted
    -- into @$library-intro$@ at the top of /library.html. Matched but not
    -- routed; the body snapshot is consumed by the library rule below.
    -- ---------------------------------------------------------------------------
    match "content/library.md" $ compile sidecarCompiler

    -- ---------------------------------------------------------------------------
    -- Library — portal-grouped view over the /new.html dataset, deduplicated
    -- by primary portal. An item's primary portal is the top segment of the
    -- first tag in its frontmatter 'tags:' list whose top segment matches a
    -- known portal (those in site.yaml). Items with no such tag are
    -- silently dropped from the library (they remain on /new.html and on any
    -- tag pages their frontmatter produces).
    --
    -- Each shelf is capped at the library's shelf-cap by default. A portal's
    -- tag-meta sidecar may carry a 'featured:' list of content-rooted paths
    -- (e.g. @content/essays/foo.md@); featured items are placed first, in
    -- listed order, and the remainder is filled by recency up to a hard
    -- ceiling of its shelf-max. Featured paths that don't resolve to an
    -- item in the portal (wrong primary portal, or typo) are silently
    -- dropped. When the unfiltered portal has more items than the shelf
    -- shows, the shelf's @$has-more$@ is set so the template can render a
    -- "More on this shelf →" affordance linking to the portal's tag page.
    -- The template loops over @library-shelves@ in portal order; a shelf
    -- with no cards has no @$entries$@ and renders nothing.
    --
    -- Each card uses the shared item-card partial, with cross-portal filings
    -- rendered in the card's tag footer via 'tagLinksFieldExcludingTopSegment',
    -- scoped to the section's portal so the portal's own tag is suppressed.
    -- ---------------------------------------------------------------------------
    create ["library.html"] $ do
        route idRoute
        compile $ do
            current <- loadConfig
            sidecarIds <- getMatches ("content/tag-meta/*.md"
                                 .||. "content/tag-meta/**/*.md")
            let library      = siteLibrary current
                sidecarSet   = Set.fromList sidecarIds
                knownPortals = map portalTag (sitePortals current)

                -- Top segment of the first tag that names a known portal.
                -- Nothing when no tag matches — item is excluded from library.
                -- Reads tags via 'getTags' (not lookupStringList) so the
                -- scalar comma form ("tags: research, ai") is accepted with
                -- the same semantics the tag pages use.
                primaryPortalOf item = do
                    ts <- getTags (itemIdentifier item)
                    return $ listToMaybe
                        [ p | t <- ts
                            , let p = takeWhile (/= '/') t
                            , p `elem` knownPortals ]

                -- Per-section item context: kind badge, ISO date for datetime
                -- attr, human-readable display date via essayCtx's dateDisplayField,
                -- abstract via siteCtx's abstractField, and cross-portal filings
                -- in the footer. Suppression is top-segment-based (hide every
                -- tag under the section's portal, not just the exact match) so
                -- a Research-section card doesn't re-list its research/* filings
                -- alongside the section heading. @full-abstract@ unclamps the
                -- card's 2-line abstract truncation — Library is the canonical
                -- browsing surface and shows full abstracts.
                portalItemCtx p =
                    contentKindField
                    <> tagLinksFieldExcludingTopSegment "item-tags" p
                    <> constField "full-abstract" "true"
                    <> essayCtx

            -- Load every content item once, then partition by primary portal
            -- so each shelf draws from a pre-filtered list rather than
            -- re-scanning the whole corpus once per portal.
            essays  <- loadAll (allEssays            .&&. hasNoVersion)
            posts   <- loadAll (P.blogPattern        .&&. hasNoVersion)
            fiction <- loadAll (P.fictionPattern     .&&. hasNoVersion)
            poetry  <- loadAll (P.poetryPattern      .&&. hasNoVersion)
            music   <- loadAll (P.musicPattern       .&&. hasNoVersion)
            photos  <- loadAll (P.photographyPattern .&&. hasNoVersion)
            let allContent = essays ++ posts ++ fiction ++ poetry ++ music ++ photos
                                :: [Item String]
            tagged <- mapM (\i -> (,i) <$> primaryPortalOf i) allContent
            let itemsByPortal :: Map.Map String [Item String]
                itemsByPortal =
                    Map.fromListWith (++) [(p, [i]) | (Just p, i) <- tagged]

            -- Existence-guarded, like the sidecar contexts in Tags.hs:
            -- deleting content/library.md degrades to a library page with
            -- no intro block rather than failing the whole compile. When
            -- the file exists, the eager snapshot load registers the
            -- library-intro dependency unconditionally, so a first-populate
            -- of content/library.md re-renders the library page even when
            -- the gate was previously false (see 'sidecarContext' in
            -- Tags.hs for the same pattern).
            introIds <- getMatches "content/library.md"
            libraryIntroFld <-
                if libraryIntroId `elem` introIds
                    then do
                        _ <- loadSnapshot libraryIntroId "body" :: Compiler (Item String)
                        return $ field "library-intro" $ \_ -> do
                            html <- itemBody <$> loadSnapshot libraryIntroId "body"
                            if all isSpace html
                                then noResult "empty library intro"
                                else return html
                    else return mempty

            -- One shelf: its cards, featured first, and whether the portal
            -- holds more than the shelf shows.
            let shelfOf portal = do
                    let p           = portalTag portal
                        portalItems = fromMaybe [] (Map.lookup p itemsByPortal)
                    sorted <- recentFirstByDisplay portalItems

                    featuredPaths <-
                        if sidecarIdentifier p `Set.member` sidecarSet
                            then do
                                meta <- getMetadata (sidecarIdentifier p)
                                return (fromMaybe [] (lookupStringList "featured" meta))
                            else return []

                    let portalIdSet =
                            Set.fromList (map itemIdentifier portalItems)
                        featuredItems =
                            [ i
                            | path <- featuredPaths
                            , let ident = fromFilePath path
                            , ident `Set.member` portalIdSet
                            , Just i <- [listToMaybe
                                (filter ((== ident) . itemIdentifier) portalItems)]
                            ]
                        cap = min (libraryShelfMax library)
                                  (max (libraryShelfCap library) (length featuredItems))
                        featuredIds =
                            Set.fromList (map itemIdentifier featuredItems)
                        rest = filter (\i -> not (itemIdentifier i `Set.member` featuredIds)) sorted
                        merged = take cap (featuredItems ++ rest)

                    return (Shelf portal merged (length portalItems > length merged))

            -- Shelf order follows the portals — single ordering authority.
            shelves <- mapM shelfOf (sitePortals current)

            -- @$entries$@ is a list whose cards hide the shelf's own portal
            -- tag, so its context depends on the shelf; absent when empty,
            -- so @$if(entries)$@ skips the shelf.
            let shelfCtx =
                       field "tag"  (return . escapeHtml . portalTag . shelfPortal . itemBody)
                    <> field "name" (return . escapeHtml . portalName . shelfPortal . itemBody)
                    <> boolField "has-more" (shelfHasMore . itemBody)
                    <> Context (\key _ item ->
                           let shelf = itemBody item
                           in  if key == "entries" && not (null (shelfEntries shelf))
                                   then return (ListField (portalItemCtx (portalTag (shelfPortal shelf)))
                                                          (shelfEntries shelf))
                                   else noResult ("no " ++ key ++ " on this shelf"))
                seeAlsoCtx =
                       field "label" (return . escapeHtml . navLabel . itemBody)
                    <> field "href"  (return . escapeHtml . navHref . itemBody)

            let ctx = listField "library-shelves" shelfCtx (mapM makeItem shelves)
                   <> listField "library-see-also" seeAlsoCtx (mapM makeItem (librarySeeAlso library))
                   <> libraryIntroFld
                   <> constField "title"   "Library"
                   <> constField "description" (escapeHtml (libraryDescription library))
                   <> constField "library" "true"
                   <> constField "portal"  "true"
                   <> siteCtx

            makeItem ""
                >>= inDefault "templates/library.html" ctx

    -- ---------------------------------------------------------------------------
    -- Bibliography — synthetic index + per-keyword pages (Phase 6b).
    -- ---------------------------------------------------------------------------
    -- Bibliography-meta sidecars: same shape as tag-meta, used by the
    -- per-keyword pages for the prose intro and (future) tooltips. No
    -- route; body snapshot consumed by the keyword rule.
    match ("content/bibliography-meta/*.md" .||. "content/bibliography-meta/**/*.md") $
        compile sidecarCompiler

    -- Collect the universe of keywords at rule-gen time. Two sources:
    --   * @keywords:@ fields across all @data/*.bib@ entries
    --   * @keywords:@ frontmatter across all essays + blog + poetry +
    --     fiction + music-composition pages
    -- The union drives which @/bibliography/\<kw\>/@ pages get generated;
    -- keywords with no referents anywhere are not synthesized into pages.
    bibFilePaths <- preprocess $ do
        files <- listDirectory "data"
        return $ sort [ "data" </> f | f <- files, takeExtension f == ".bib" ]

    bibExtrasAll <- preprocess $
        Map.unions <$> mapM parseBibExtras bibFilePaths

    let bibKwMap :: Map String [String]
        bibKwMap = invertKeywordsBib bibExtrasAll

    writingIds <- getMatches $ (P.essayPattern
                            .||. P.blogPattern
                            .||. P.fictionPattern
                            .||. P.poetryPattern
                            .||. P.musicPattern)
                          .&&. hasNoVersion

    writingKwPairs <- forM writingIds $ \ident -> do
        meta <- getMetadata ident
        let kws = metadataKeywords meta
        return (ident, kws)

    let writingKwMap :: Map String [Identifier]
        writingKwMap = invertKeywordsWritings writingKwPairs

        -- Keywords with at least one referent (writing OR bib entry).
        allKeywords :: Set String
        allKeywords = Set.union (Map.keysSet bibKwMap) (Map.keysSet writingKwMap)

    -- Identifiers of bibliography-meta sidecars that exist on disk,
    -- used to optionally inject $portal-intro$ + $portal-tooltip$ on
    -- keyword pages when the author populates a sidecar.
    bibMetaIds <- getMatches ("content/bibliography-meta/*.md"
                         .||. "content/bibliography-meta/**/*.md")
    let bibMetaSet = Set.fromList bibMetaIds

    -- /bibliography/index.html — every entry across every .bib file.
    -- Sort: ascending by first-author surname, year-descending within
    -- author (scholarly convention).
    -- Register data/*.bib + data/*.csl as tracked inputs of a synthetic
    -- bibliography page.
    --
    -- 'bibFilePaths' and 'bibExtrasAll' above are read at rule-generation
    -- time through 'preprocess', and the entry HTML is rendered by
    -- citeproc inside 'unsafeCompiler'. Both are invisible to Hakyll, so
    -- an edit to a .bib title updated the essays that cite it and left
    -- /bibliography/ serving its cached copy. 'loadAll' both registers a
    -- pattern dependency (a new or deleted .bib file changes the match
    -- set) and depends on each matched identifier (an edited .bib file is
    -- out of date), which is what forces the recompile. The loaded bodies
    -- are deliberately unused — 'renderBibliographyHtml' re-reads the
    -- files itself, because citeproc wants paths, not contents.
    let trackBibInputs :: Compiler ()
        trackBibInputs = void
            (loadAll (("data/*.bib" .||. cslStyles) .&&. hasNoVersion)
                :: Compiler [Item String])

    create ["bibliography/index.html"] $ do
        route idRoute
        compile $ do
            trackBibInputs
            -- Only the works a published page cites or lists as further
            -- reading: the page says "every work cited across this site", and
            -- listing every .bib entry put nine uncited ones on it, some kept
            -- only for a draft (audit C07).
            cited <- Set.fromList . concatMap (words . itemBody) <$>
                (loadAllSnapshots (essayPipeline .&&. hasNoVersion) "cite-keys"
                    :: Compiler [Item String])
            let sortedKeys = filter (`Set.member` cited) (bibliographyIndexOrder bibExtrasAll)
                grouped    = groupByLetter bibExtrasAll sortedKeys
                present    = map fst grouped
            html <- unsafeCompiler $ do
                parts <- forM grouped $ \(letter, keys) -> do
                    body <- renderBibliographyHtml bibFilePaths bibExtrasAll keys
                    return (renderLetterHeader letter <> body)
                return (renderBibliographyAlphabet present <> T.concat parts)
            let mathFld = katexFallbackField html
            let ctx = constField "title"          "Bibliography"
                   <> constField "bibliography-index" "true"
                   <> constField "description"
                        ("Every work cited across this site, in one list, "
                         ++ "alphabetical by author.")
                   <> mathFld
                   <> constField "bibliography-entries" (T.unpack html)
                   <> bibliographySeeAlso
                   <> constField "library" "true"  -- reuse flag to load library.css + item-card.css
                   <> constField "portal"  "true"
                   <> siteCtx
            makeItem ""
                >>= inDefault "templates/bibliography-index.html" ctx

    -- /bibliography/<keyword>/index.html for each keyword in the union.
    forM_ (Set.toList allKeywords) $ \kw ->
        create [fromFilePath ("bibliography/" ++ kw ++ "/index.html")] $ do
            route idRoute
            compile $ do
                trackBibInputs
                -- Writings section
                let wIds = fromMaybe [] (Map.lookup kw writingKwMap)
                writingItems <- case wIds of
                    [] -> return []
                    _  -> recentFirstByDisplay
                            =<< mapM (\i -> load i :: Compiler (Item String)) wIds
                let writingsCtx
                        | null writingItems = mempty
                        | otherwise = listField "writings" portalWritingCtx
                                        (return writingItems)
                                   <> constField "has-writings" "true"

                -- References section
                let refKeys = keywordReferencesOrder bibExtrasAll kw
                refsHtml <- unsafeCompiler $
                    renderBibliographyHtml bibFilePaths bibExtrasAll refKeys
                let mathFld = katexFallbackField refsHtml
                let referencesCtx
                        | null refKeys = mempty
                        | otherwise =
                            constField "references" (T.unpack refsHtml)
                            <> mathFld

                -- Sidecar (tooltip + optional prose intro)
                let sidecarId = bibliographyMetaIdentifier kw
                    hasSidecar = sidecarId `Set.member` bibMetaSet
                scCtx <- if hasSidecar
                    then do
                        _ <- loadSnapshot sidecarId "body" :: Compiler (Item String)
                        return (portalIntroField   (const sidecarId)
                             <> portalTooltipField (const sidecarId))
                    else return mempty

                let ctx = constField "title"          kw
                       <> constField "keyword"        kw
                       <> constField "bibliography-keyword" "true"
                       <> constField "library" "true"  -- reuse flag to load library.css + item-card.css
                       <> writingsCtx
                       <> referencesCtx
                       <> scCtx
                       <> bibliographySeeAlso
                       <> siteCtx
                makeItem ""
                    >>= inDefault "templates/bibliography-keyword.html" ctx

    -- ---------------------------------------------------------------------------
    -- Random page manifest — essays, blog posts, fiction, and poetry (flat
    -- and collection poems alike). No pagination/index pages; music and
    -- photography landings are also excluded.
    -- ---------------------------------------------------------------------------
    create ["random-pages.json"] $ do
        route idRoute
        compile $ do
            essays  <- loadAll (allEssays        .&&. hasNoVersion) :: Compiler [Item String]
            posts   <- loadAll (P.blogPattern    .&&. hasNoVersion) :: Compiler [Item String]
            fiction <- loadAll (P.fictionPattern .&&. hasNoVersion) :: Compiler [Item String]
            poetry  <- loadAll (P.poetryPattern  .&&. hasNoVersion) :: Compiler [Item String]
            routes  <- mapM (getRoute . itemIdentifier) (essays ++ posts ++ fiction ++ poetry)
            let urls = [ "/" ++ r | Just r <- routes ]
            makeItem $ LBS.unpack (Aeson.encode urls)

    -- ---------------------------------------------------------------------------
    -- Epistemic metadata manifest — maps page URLs to epistemic fields
    -- (status, confidence, importance, evidence, scope, novelty, practicality,
    -- stability, score) for client-side search filtering.
    -- ---------------------------------------------------------------------------
    create ["data/epistemic-meta.json"] $ do
        route idRoute
        compile $ do
            essays  <- loadAll (allEssays        .&&. hasNoVersion) :: Compiler [Item String]
            posts   <- loadAll (P.blogPattern    .&&. hasNoVersion) :: Compiler [Item String]
            fiction <- loadAll (P.fictionPattern .&&. hasNoVersion) :: Compiler [Item String]
            poetry  <- loadAll (P.poetryPattern  .&&. hasNoVersion) :: Compiler [Item String]
            music   <- loadAll (P.musicPattern   .&&. hasNoVersion) :: Compiler [Item String]
            let items = essays ++ posts ++ fiction ++ poetry ++ music
            pairs <- mapM epistemicEntry items
            let metaMap = Map.fromList (catMaybes pairs)
            makeItem $ LBS.unpack (Aeson.encode metaMap)

    -- ---------------------------------------------------------------------------
    -- Text feeds. The site owns their routes, section sets, limits and identity.
    -- Photography uses its image-bearing entry context in Photography.hs.
    -- ---------------------------------------------------------------------------
    forM_ (filter (not . photoFeed) (siteFeeds cfg)) $ \feed ->
        create [fromFilePath (feedPath feed)] $ do
            route idRoute
            compile $ do
                (current, spec) <- loadFeed (feedPath feed)
                let sectionPattern section = case section of
                        Essays -> allEssays
                        Blog -> P.blogPattern
                        Fiction -> P.fictionPattern
                        Poetry -> P.poetryPattern
                        Music -> P.musicPattern
                        Photos -> P.allPhotoEntries
                    pat = foldr1 (.||.) (map sectionPattern (feedSections spec))
                items <- recentFirst =<< loadAllSnapshots (pat .&&. hasNoVersion) "content"
                let posts = maybe id take (feedLimit spec) items
                renderSiteFeed current spec feedCtx posts

    -- ---------------------------------------------------------------------------
    -- robots.txt — minimal, just points crawlers at the sitemap
    -- ---------------------------------------------------------------------------
    create ["robots.txt"] $ do
        route idRoute
        compile $ do
            current <- loadConfig
            makeItem $ unlines
                -- /archive/ is *deliberately not* disallowed. Crawlers must be
                -- able to reach the wrapper pages (and snapshot.html) to see
                -- their <meta name=robots content="noindex, noarchive">; a
                -- robots.txt Disallow would block that and a URL blocked only
                -- by robots.txt can still appear in results when linked. The
                -- raw PDFs cannot carry meta — they need an `X-Robots-Tag`
                -- HTTP header from the deploy webserver (see nginx/archive.conf).
                --
                -- /proxy/ *is* disallowed, and for the opposite reason. Those
                -- locations (nginx/popup-proxy.conf) fetch and re-serve
                -- third-party pages — arXiv, archive.org, PubMed — under this
                -- origin so the hover popups can read them same-origin. They
                -- are a reading affordance, not this site's content: indexing
                -- them would put someone else's pages in the index under
                -- this site's URLs. Unlike the archive wrappers there is
                -- no HTML of ours to carry a noindex meta, so robots.txt is
                -- the only control available at build time.
                [ "User-agent: *"
                , "Allow: /"
                , "Disallow: /proxy/"
                , ""
                , "Sitemap: " ++ siteUrl current ++ "/sitemap.xml"
                ]

    -- ---------------------------------------------------------------------------
    -- 404.html — custom not-found page.
    --
    -- nginx/vhost.conf.example declares `error_page 404 /404.html` and
    -- serves it with the 404 status; nothing generated the file, so the
    -- deployment fell back to nginx's stock response with no navigation
    -- and no way back into the site. Rendered through the ordinary page
    -- shell so the reader keeps the nav, search, and footer, and marked
    -- noindex so the error page itself never enters an index.
    -- ---------------------------------------------------------------------------
    create ["404.html"] $ do
        route idRoute
        compile $ do
            let ctx = constField "title"   "Page not found"
                   <> constField "noindex" "true"
                   <> constField "description"
                        "No page exists at this address."
                   -- Keeps the body out of the keyword index; see the
                   -- comment in templates/page.html.
                   <> constField "search-exclude" "true"
                   <> pageCtx
            -- Deliberately NOT relativized. Every other page is rewritten
            -- to root-relative-from-here URLs, which is correct because
            -- each is served from its own path. The 404 body is served
            -- for *any* missing path, so `./css/base.css` would resolve
            -- against whatever directory the reader mistyped. Absolute
            -- URLs are the only form that works from every depth.
            makeItem ""
                >>= loadAndApplyTemplate "templates/404.html"     ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx

    -- ---------------------------------------------------------------------------
    -- sitemap.xml — every dated content page (essays, blog, poetry, fiction,
    -- music) plus the photography section and the standalone pages.
    --
    -- The previous version listed only the 22 dated writing pages, on the
    -- reasoning that a page without `date:` would force an invented
    -- `lastmod`. That trade was the wrong way round: `lastmod` is optional
    -- per the sitemaps protocol, so a page with no known revision date can
    -- simply be listed without one — whereas leaving 400-odd photography
    -- URLs and every standalone page out of the sitemap entirely is a real
    -- omission on a media-heavy site.
    --
    -- Deliberately excluded, and why:
    --   * drafts                — unpublished (they are also dev-only)
    --   * /source/              — raw source copies, not readable pages
    --   * /archive/             — wrappers already carry `noindex`
    --   * feeds, search, JSON   — not HTML documents a reader lands on
    --   * tag / author / keyword indexes — list variants of pages that are
    --     already here under their own canonical URLs
    --   * content/library.md    — an intro fragment with no route of its
    --     own (it is snapshot-loaded into /library.html)
    -- ---------------------------------------------------------------------------
    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            -- Dated writing. P.essayPattern, not `allEssays`: drafts stay
            -- out of the sitemap even in a dev build.
            datedIds <- getMatches $
                (    P.essayPattern
                .||. P.blogPattern
                .||. P.fictionPattern
                .||. P.poetryPattern
                .||. P.musicPattern
                ) .&&. hasNoVersion

            -- Every photographic entry: flat frames, series landings, and
            -- the sibling frames inside a series.
            photoIds <- getMatches (P.allPhotoEntries .&&. hasNoVersion)

            -- Generated photography indexes: /photography/by-year/,
            -- /photography/by-year/<year>/, /photography/map/,
            -- /photography/contact-sheet/. These are `create`d, so their
            -- identifier is their route; map.json and feed.xml are dropped
            -- by the .html filter.
            photoIndexIds <- filter (isHtmlIdentifier . toFilePath)
                                <$> getMatches ("photography/**" .&&. hasNoVersion)

            -- site.yaml's standalone pages; a page compiled from now.yaml
            -- takes its date from there.
            current <- loadConfig
            let standaloneIds = map (fromFilePath . standaloneSource) (siteStandalone current)
                nowIds = [ fromFilePath (standaloneSource page)
                         | page <- siteStandalone current, standaloneContext page == NowPage ]
                otherIds = map fromFilePath
                    -- Other authored top-level pages.
                    [ "content/work.md"
                    , "content/links.md"
                    , "content/gpg.md"
                    , "content/commonplace.md"
                    -- Section landings.
                    , "content/photography/index.md"
                    , "content/music/index.md"
                    -- Generated landings and indexes.
                    , "essays/index.html"
                    , "poetry/index.html"
                    , "fiction/index.html"
                    , "library.html"
                    , "new.html"
                    , "bibliography/index.html"
                    ]

                corpus = datedIds ++ photoIds ++ photoIndexIds
                      ++ standaloneIds ++ otherIds

            entries <- sitemapEntries nowIds corpus

            let siteRoot = siteUrl current
                sitemapItemCtx =
                    constField "root" siteRoot
                    <> field "url" (return . fst . itemBody)
                    <> field "lastmod"
                        (maybe (noResult "no known revision date") return
                            . snd . itemBody)
                sitemapCtx =
                    constField "root" siteRoot
                    <> listField "entries" sitemapItemCtx (return entries)
            makeItem ("" :: String)
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

-- ---------------------------------------------------------------------------
-- Compile-time filesystem reads and their dependency coverage (audit B11)
-- ---------------------------------------------------------------------------
--
-- Several filters read the filesystem from inside 'unsafeCompiler', which
-- is invisible to Hakyll's dependency graph: the compiled page depends on
-- its Markdown source and on nothing those filters touched. The inventory
-- below is the full list, so that the next filter added here has somewhere
-- to declare itself rather than quietly joining the untracked set.
--
-- Covered by an explicit dependency:
--
--   * "Filters.Viz" figure scripts and their data, plus @tools/viz_theme.py@
--     — 'figureDep', over every essay. Coarse on purpose: one dependency
--     for all essays.
--   * @*.dims.yaml@ sidecars read by "Filters.Images" — 'figureDep' for
--     essays and @static/@; a sibling dependency in "Photography" for the
--     photography corpus. Both needed a matched-but-unrouted rule first:
--     Hakyll intersects its modified set with the identifiers some rule
--     claims, so a dependency on an unclaimed file can never fire.
--   * Score fragments read by "Filters.Score" — 'standaloneRules' (pages
--     with @scores@) and 'musicScoreDep' (music compositions). Essay score
--     fragments live under @content/essays/**/figures/**@ or beside the
--     essay and are carried by 'figureDep'.
--   * @.bib@ databases and the CSL style read by citeproc —
--     'Compilers.trackBibliographyInputs' per page, and 'trackBibInputs'
--     for the synthetic bibliography pages.
--   * @data/*.yaml@, @yaml-source/data/*.yml@, @data/build-stamp.txt@ —
--     matched (unrouted) and 'load'ed by their consumers.
--
-- Deliberately not covered, and why:
--
--   * "Filters.SourceRefs" probes @doesFileExist@ for every repository
--     path mentioned in prose, to decide whether to emit a @/source/@
--     hover link. The interesting change is a file appearing or
--     disappearing, which changes the /match set/ of the source-preview
--     rule and is therefore a rule-generation input, not a compiler input.
--     A pattern dependency over the whole source-previewable corpus would
--     rebuild every page on any edit to any @.hs@, @.js@, or @.css@ file —
--     an entire-site rebuild on every commit, to keep one hover link
--     honest. The seven-day full-build fallback bounds the staleness.
--   * "ArchiveIndex" reads @archive/manifest.yaml@ and
--     @data/archive-state.json@ once per process and caches the result in
--     a CAF, so a watch session does not see archive state change until it
--     is restarted. Left as is deliberately (it is what keeps the archive
--     filter from re-reading the manifest on every link of every page);
--     the workaround is to restart @make watch@ after an archive edit.
--   * Blog, poetry, and fiction entries can in principle carry figures and
--     score fragments; none currently do, and neither section has a
--     figures/ directory. Add them to 'figureDep' if that changes.
--   * Backlinks and Related come from per-page files that the Makefile
--     writes between the two compile passes (build/FooterData.hs), so the
--     first pass renders the previous build's, and @make watch@ and dev
--     builds render the last production build's.

-- ---------------------------------------------------------------------------
-- Collections (poetry / fiction landing pages)
-- ---------------------------------------------------------------------------

-- | Context for a collection landing page: the ordinary page fields plus
--   the collection's own children.
--
--   @childPattern@ is the section's entry pattern (which already excludes
--   @index.md@); it is intersected with a glob for /this/ collection's
--   directory, so a landing page lists its siblings and nothing else.
--   Entries are ordered by display date, newest first — the same ordering
--   the library and @\/new.html@ use, so a revised poem does not sort
--   under its original date on one surface and its revision date on
--   another.
--
--   @has-collection-entries@ gates the template's empty state. An empty
--   'listField' renders as nothing at all rather than as "no items", which
--   is how the reader ended up with a landing page that promised child
--   links and showed none.
collectionCtx :: Pattern            -- ^ the section's entry pattern
              -> Context String     -- ^ context for a child entry
              -> Context String     -- ^ context for the landing page itself
              -> Compiler (Context String)
collectionCtx childPattern childCtx baseCtx = do
    ident <- getUnderlying
    let dir = takeDirectory (toFilePath ident)
    children <- recentFirstByDisplay
                    =<< loadAll (childPattern
                                  .&&. fromGlob (dir ++ "/*.md")
                                  .&&. hasNoVersion)
    let entriesFld
            | null children = mempty
            | otherwise =
                  listField "collection-entries" childCtx (return children)
                  <> constField "has-collection-entries" "true"
    return (entriesFld <> constField "collection" "true" <> baseCtx)

-- ---------------------------------------------------------------------------
-- Sitemap
-- ---------------------------------------------------------------------------

-- | Turn a list of identifiers into @(url, maybe lastmod)@ sitemap rows.
--
--   Identifiers with no route are dropped (an unrouted sidecar is not a
--   page), duplicates are collapsed, and the site root is dropped because
--   @templates\/sitemap.xml@ emits it unconditionally. Rows come back in
--   URL order, which keeps the generated file stable across builds.
sitemapEntries :: [Identifier] -> [Identifier] -> Compiler [Item (String, Maybe String)]
sitemapEntries nowIds idents = do
    rows <- mapM (sitemapEntry nowIds) idents
    let unique = Map.toAscList (Map.fromList
                    [ (u, d) | Just (u, d) <- rows, u /= "/" ])
    return [ Item (fromFilePath u) (u, d) | (u, d) <- unique ]

-- | One sitemap row, or 'Nothing' when the identifier has no route.
sitemapEntry :: [Identifier] -> Identifier -> Compiler (Maybe (String, Maybe String))
sitemapEntry nowIds ident = do
    mRoute <- getRoute ident
    case mRoute of
        Nothing -> return Nothing
        Just r  -> do
            -- `lastmod` is optional: a page with no `date:` is listed
            -- without one rather than dropped or given an invented date.
            -- getItemUTC throws for a created identifier that carries no
            -- metadata at all, which is the same "unknown" answer.
            --
            -- F11: the date is the *revision-aware* one, so a page that
            -- has been substantively revised advertises the revision
            -- rather than repeating its creation date to crawlers.
            -- 'identifierDisplayUTC' falls back to the creation date when
            -- there is no `revised:` entry.
            mDay <- if ident `elem` nowIds
                -- Such a page's content lives in now.yaml, which also owns
                -- the date shown in its masthead. Loading it tracks the sitemap
                -- dependency when that date changes.
                then Just <$> nowLastUpdated
                else fmap Just (formatIso
                                    <$> identifierDisplayUTC ident)
                         `catchError` const (return Nothing)
            return (Just (canonicalUrlPath r, mDay))

-- | True for routes that name an HTML document. Used to keep @map.json@
--   and @feed.xml@ out of the photography-index sweep.
isHtmlIdentifier :: FilePath -> Bool
isHtmlIdentifier = isSuffixOf ".html"

-- ---------------------------------------------------------------------------
-- Epistemic metadata extraction
-- ---------------------------------------------------------------------------

-- | Extract epistemic metadata from a content item's frontmatter.
--   Returns Nothing if the item has no route or no epistemic fields.
epistemicEntry :: Item String -> Compiler (Maybe (String, Map.Map String String))
epistemicEntry item = do
    let ident = itemIdentifier item
    mRoute <- getRoute ident
    case mRoute of
        Nothing -> return Nothing
        Just r  -> do
            meta <- getMetadata ident
            -- Stability is derived from history, not written in front
            -- matter, so it must be resolved as the footer resolves it,
            -- and for the same pages: those with a status. Read from front
            -- matter it was absent from every entry, and the stability
            -- filter hid every page.
            stability <- case lookupString "status" meta of
                Just _  -> Just . (,) "stability" <$> resolveStability item
                Nothing -> return Nothing
            let url = "/" ++ r
                fields = catMaybes
                    [ grab "status"       meta
                    , grab "confidence"   meta
                    , grab "importance"   meta
                    , grab "evidence"     meta
                    , grab "scope"        meta
                    , grab "novelty"      meta
                    , grab "practicality" meta
                    , stability
                    ]
                obj = Map.fromList fields
                obj' = case trustScore (confidencePercent (lookupString "confidence" meta))
                                       (readMaybe =<< lookupString "evidence" meta) of
                    Just score -> Map.insert "score" (show score) obj
                    Nothing    -> obj
            if Map.null obj'
                then return Nothing
                else return (Just (url, obj'))
  where
    grab name meta = case lookupString name meta of
        Just v  -> Just (name, v)
        Nothing -> Nothing


-- ---------------------------------------------------------------------------
-- Bibliography helpers (Phase 6b)
-- ---------------------------------------------------------------------------

-- | Invert a @citekey -> BibExtra@ map into @keyword -> [citekey]@
--   using each entry's 'bibKeywords' list as the inversion source.
invertKeywordsBib :: Map String BibExtra -> Map String [String]
invertKeywordsBib =
    Map.fromListWith (++) . concatMap flatten . Map.toList
  where
    flatten (k, e) = [ (kw, [k]) | kw <- bibKeywords e ]

-- | Invert a @[(Identifier, [keyword])]@ association into
--   @keyword -> [Identifier]@. Identifiers can appear under multiple
--   keywords (multi-keyword items).
invertKeywordsWritings :: [(Identifier, [String])] -> Map String [Identifier]
invertKeywordsWritings pairs =
    Map.fromListWith (++)
        [ (kw, [ident]) | (ident, kws) <- pairs, kw <- kws ]

-- | Sort citekeys for the /bibliography/ index: ascending first-author
--   surname, year-descending within author.
bibliographyIndexOrder :: Map String BibExtra -> [String]
bibliographyIndexOrder extras =
    map fst $ sortBy (comparing sortKey) (Map.toList extras)
  where
    sortKey (_, e) = (firstAuthorSurname e, Down (bibYear e))

-- | Sort citekeys for a /bibliography/<kw>/ References section: year
--   descending, then alphabetical by first-author surname within the
--   year. Filtered to only entries whose 'bibKeywords' includes @kw@.
keywordReferencesOrder :: Map String BibExtra -> String -> [String]
keywordReferencesOrder extras kw =
    map fst $ sortBy (comparing sortKey)
        [ (k, e) | (k, e) <- Map.toList extras, kw `elem` bibKeywords e ]
  where
    sortKey (_, e) = (Down (bibYear e), firstAuthorSurname e)

-- | Identifier of a bibliography-meta sidecar for a given keyword.
--   Parallels 'Tags.sidecarIdentifier' but under
--   @content/bibliography-meta/@ rather than @content/tag-meta/@.
bibliographyMetaIdentifier :: String -> Identifier
bibliographyMetaIdentifier kw =
    fromFilePath ("content/bibliography-meta/" ++ kw ++ ".md")

-- | Group an alphabetically-sorted list of citekeys into letter buckets
--   keyed by the uppercase first letter of each entry's first-author
--   surname (falling back to the citekey's first letter when no author
--   was parsed — edge case, shouldn't occur in current content).
--
--   Because @sortedKeys@ is already alphabetical, 'Data.List.groupBy'
--   produces contiguous same-letter runs in one pass.
groupByLetter :: Map String BibExtra -> [String] -> [(Char, [String])]
groupByLetter extras sortedKeys =
    let withLetters = [ (k, letterOf k) | k <- sortedKeys ]
        grouped     = groupBy (\(_, a) (_, b) -> a == b) withLetters
    in  [ (letter, map fst grp) | grp <- grouped
                                , (_, letter) : _ <- [grp] ]
  where
    letterOf k =
        let e = fromMaybe emptyBibExtra (Map.lookup k extras)
        in case firstAuthorSurname e of
            (c:_) -> toUpper c
            _     -> case k of
                (c:_) -> toUpper c
                _     -> '?'

-- | The A–Z jump strip above the entry list. Present letters render as
--   anchor links to their section heading; absent letters render as
--   muted, non-linked spans so the alphabet reads as a complete strip
--   regardless of content gaps.
renderBibliographyAlphabet :: [Char] -> T.Text
renderBibliographyAlphabet presentList =
    let present = Set.fromList presentList
        cell c
          | c `Set.member` present =
              "<a href=\"#" <> T.singleton c
              <> "\" class=\"alpha\">" <> T.singleton c <> "</a>"
          | otherwise =
              "<span class=\"alpha alpha-empty\" aria-hidden=\"true\">"
              <> T.singleton c <> "</span>"
    in  "<nav class=\"bibliography-alphabet\" aria-label=\"Jump to letter\">"
     <> T.concat (map cell ['A' .. 'Z'])
     <> "</nav>\n"

-- | Letter-group heading inserted between entry groups on the
--   bibliography index. The @id@ is the anchor target for
--   'renderBibliographyAlphabet' jump-links.
renderLetterHeader :: Char -> T.Text
renderLetterHeader c =
    "<h2 id=\"" <> T.singleton c
    <> "\" class=\"bibliography-letter\">"
    <> T.singleton c <> "</h2>\n"

-- | @$math$@ for rendered bibliography HTML that still holds LaTeX.
--   C07: entry math is rendered to MathML at build time by
--   'Citations.renderEntries', so a bibliography needs no runtime
--   typesetter. This is the safety net: if the MathML writer ever declines
--   a construct it falls back to a raw @class="math …"@ span holding LaTeX
--   source, and a page with one of those does need KaTeX after all.
--   Nothing currently in the corpus trips it.
katexFallbackField :: T.Text -> Context String
katexFallbackField html
    | any (`T.isInfixOf` html) ["class=\"math inline\"", "class=\"math display\""]
                = constField "math" "true"
    | otherwise = mempty

-- | Item-level context for Writings-section cards on a keyword page.
--   Same fields as the library's 'portalItemCtx' but with tag-footer
--   suppression tuned to the keyword context rather than a portal
--   (nothing to suppress here — writings keep their full tag list so
--   readers can see the item's own portal filings).
portalWritingCtx :: Context String
portalWritingCtx =
    contentKindField
    <> constField "full-abstract" "true"
    <> essayCtx


-- | The bibliography pages' links on: the library, then the library's own
--   (site.yaml's library see-also), all but the bibliography's.
bibliographySeeAlso :: Context String
bibliographySeeAlso = listField "library-see-also" linkCtx $ do
    library <- siteLibrary <$> loadConfig
    mapM makeItem (NavLink "Library" "/library.html"
                   : filter ((/= "/bibliography/") . navHref) (librarySeeAlso library))
  where
    linkCtx = field "label" (return . escapeHtml . navLabel . itemBody)
           <> field "href"  (return . escapeHtml . navHref . itemBody)
