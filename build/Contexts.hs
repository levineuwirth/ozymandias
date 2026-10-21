{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
module Contexts
    ( siteCtx
    , essayCtx
    , postCtx
    , pageCtx
    , poetryCtx
    , fictionCtx
    , compositionCtx
    , declaresScore
    , scorePageList
    , scoreThumb
    , durationPrimes
    , svgAspect
    , photographyCtx
    , photoVariantName
    , contentKindField
    , abstractField
    , descriptionField
    , plainField
    , feedTitleField
    , tagLinksField
    , tagLinksFieldExcludingScope
    , tagLinksFieldExcludingTopSegment
    , keywordLinksField
    , authorLinksField
    , dateDisplayField
    , revisionDateFields
    , recentFirstByDisplay
    , canonicalUrlField
    , feedMetaFields
    , canonicalUrlPath
    , itemDisplayUTC
    , identifierDisplayUTC
    , Revision (..)
    , getRevisions
    , photoVariantWidths
    ) where

import Control.Exception       (IOException, catch, try)
import Data.Aeson              (Value (..))
import qualified Data.Aeson         as Aeson
import qualified Data.Aeson.Key     as AK
import qualified Data.Aeson.KeyMap  as KM
import qualified Data.Vector        as V
import Data.Char               (isDigit, isSpace, toLower, toUpper)
import Data.List               (intercalate, isInfixOf, isPrefixOf, sortBy,
                                stripPrefix)
import Data.Maybe              (fromMaybe, mapMaybe)
import Data.Ord                (Down (..), comparing)
import qualified Data.Scientific    as Sci
import qualified Data.Set           as Set
import Data.Time.Clock         (UTCTime, utctDay)
import Data.Time.Format        (formatTime, defaultTimeLocale)
import Data.IORef              (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict    as Map
import System.Directory        (doesFileExist, getModificationTime)
import System.IO.Unsafe        (unsafePerformIO)
import System.FilePath         (makeRelative, takeDirectory, takeFileName, (</>))
import System.IO               (hPutStrLn, stderr)
import Text.Read               (readMaybe)
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import qualified Data.Yaml     as Y
import Text.Pandoc             (runPure, readMarkdown, writeHtml5String, writePlain, Pandoc(..), Block(..), Inline(..))
import Text.Pandoc.Options     (WriterOptions(..), HTMLMathMethod(..))
import Text.Pandoc.Extensions  (Extension(..), disableExtension)
import Hakyll       hiding (escapeHtml, trim)
import Hakyll.Core.Compiler.Internal (compilerAsk, compilerUniverse)
import Backlinks    (backlinksField)
import Dingbat      (dingbatField)
import Marks        (monogramSvgField, hasMonogramField, epistemicSvgField, peerStatusValues)
import Sections     (Section (..), sectionOfRoute)
import SimilarLinks (similarLinksField)
import FooterData   (footerDepsField)
import Stability    (stabilityField, lastReviewedField, lastReviewedIsoField,
                     versionHistoryField,
                     versionHistoryPrimaryField, versionHistoryRestField,
                     versionHistoryRangeField, versionHistoryRangeStartField,
                     versionHistoryRangeEndField, versionHistoryCommitsField)
import Config       (loadConfig, identityContext, navigationContext, siteAuthor)
import Utils        (authorUrl, canonicalUrlPath, confidencePercent, escapeHtml, formatIso, formatWriterly, isoDate, isProvedConfidence, itemAuthors, metadataKeywords, parseIsoDate, trim, trustScore, writerlyDate)

-- ---------------------------------------------------------------------------
-- Affiliation field
-- ---------------------------------------------------------------------------

-- | Parses the @affiliation@ frontmatter key and exposes each entry as
--   @affiliation-name@ / @affiliation-url@ pairs.
--
--   Accepts a scalar string or a YAML list. Each entry may use pipe syntax:
--     @"Brown University | https://cs.brown.edu"@
--   Entries without a URL still produce a row; @affiliation-url@ fails
--   (evaluates to noResult), so @$if(affiliation-url)$@ works in templates.
--
--   Usage:
--     $for(affiliation-links)$
--       $if(affiliation-url)$<a href="$affiliation-url$">$affiliation-name$</a>
--       $else$$affiliation-name$$endif$$sep$ · $endfor$
affiliationField :: Context a
affiliationField = listFieldWith "affiliation-links" ctx $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    let entries = case lookupStringList "affiliation" meta of
                    Just xs -> xs
                    Nothing -> maybe [] (:[]) (lookupString "affiliation" meta)
    -- noResult, not an empty list: Hakyll's $if$ treats an empty
    -- ListField as truthy, so returning [] would render the wrapper
    -- markup (an empty .meta-affiliation row) on every page.
    if null entries
        then noResult "no affiliation"
        else return $ map (Item (fromFilePath "") . parseEntry) entries
  where
    ctx = field "affiliation-name" (return . fst . itemBody)
       <> field "affiliation-url"  (\i -> let u = snd (itemBody i)
                                           in if null u then noResult "no url" else return u)
    parseEntry s = case break (== '|') s of
        (name, '|' : url) -> (trim name, trim url)
        (name, _)         -> (trim name, "")

-- ---------------------------------------------------------------------------
-- Content kind field
-- ---------------------------------------------------------------------------

-- | @$item-kind$@: human-readable content type derived from the item's route.
-- Used on the New page to label each entry (Essay, Post, Poem, etc.).
contentKindField :: Context String
contentKindField = field "item-kind" $ \item -> do
    cfg <- loadConfig
    r <- getRoute (itemIdentifier item)
    return $ escapeHtml $ maybe "Page" sectionKind (sectionOfRoute cfg =<< r)

-- ---------------------------------------------------------------------------
-- Site-wide context
-- ---------------------------------------------------------------------------

-- | @$page-scripts$@ — list field providing @$script-src$@ for each entry
-- in the @js:@ frontmatter key (accepts a scalar string or a YAML list).
-- Returns an empty list when absent; $for iterates zero times, emitting nothing.
-- NOTE: do not use fail here — $for does not catch noResult the way $if does.
--
-- Each child Item is keyed on @<parent-identifier>#js-<index>@ so that two
-- pages referencing the same script path (e.g. @shared.js@) do not collide
-- in Hakyll's item store.
pageScriptsField :: Context String
pageScriptsField = listFieldWith "page-scripts" ctx $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    let scripts = case lookupStringList "js" meta of
                    Just xs -> xs
                    Nothing -> maybe [] (:[]) (lookupString "js" meta)
        parent  = toFilePath (itemIdentifier item)
    return $ zipWith
        (\i s -> Item (fromFilePath (parent ++ "#js-" ++ show (i :: Int))) s)
        [0 ..]
        scripts
  where
    ctx = field "script-src" (return . itemBody)

-- ---------------------------------------------------------------------------
-- Tag links field
-- ---------------------------------------------------------------------------

-- | List context field exposing an item's own (non-expanded) tags as
--   @tag-name@ / @tag-url@ objects.
--
--   Fails with 'noResult' when the item has no tags — same discipline
--   as the @Excluding@ variants below — so @$if(...)$@ gates are false
--   and templates don't emit empty tag-wrapper markup.
--
--   $for(essay-tags)$<a href="$tag-url$">$tag-name$</a>$endfor$
tagLinksField :: String -> Context a
tagLinksField fieldName = tagLinksWhere fieldName (const True) "no tags"

-- | The tag-link list the three fields here share: the item's tags that
--   pass @keep@, as @tag-name@ / @tag-url@ objects, or 'noResult' with
--   @why@ when none do.
tagLinksWhere :: String -> (String -> Bool) -> String -> Context a
tagLinksWhere fieldName keep why = listFieldWith fieldName ctx $ \item -> do
    ts <- filter keep <$> getTags (itemIdentifier item)
    if null ts
        then noResult why
        else return (map toItem ts)
  where
    toItem t = Item (fromFilePath (t ++ "/index.html")) t
    ctx      = field "tag-name" (return . itemBody)
            <> field "tag-url"  (\i -> return $ "/" ++ itemBody i ++ "/")

-- | Variant of 'tagLinksField' that suppresses tags equal to or ancestral
--   to the given scope. Used on tag index pages to hide the redundant
--   filing ribbon entry for the current page's own scope.
--
--   Suppression is equality-based on the scope plus its prefix-ancestors:
--   on @\/nonfiction\/@ (scope = @"nonfiction"@) only the literal
--   @"nonfiction"@ tag is hidden; @"nonfiction/philosophy"@ still renders.
--   On @\/nonfiction\/philosophy\/@ both @"nonfiction"@ and
--   @"nonfiction/philosophy"@ are hidden; sibling and cross-filed tags
--   remain.
--
--   When every tag is suppressed, the field fails with 'noResult' so
--   @$if(...)$@ is false and the tag-ribbon wrapper is omitted entirely
--   instead of rendering as an empty @<div>@.
tagLinksFieldExcludingScope :: String -> String -> Context a
tagLinksFieldExcludingScope fieldName scope =
    tagLinksWhere fieldName (not . isScopeOrAncestor)
        "no visible tags after scope suppression"
  where
    -- Hide tag t when t == scope, or when t is a strict prefix-ancestor
    -- of scope (i.e., scope starts with t ++ "/"). Descendants of scope
    -- (e.g., "nonfiction/philosophy" when scope="nonfiction") are kept.
    isScopeOrAncestor t = t == scope || (t ++ "/") `isPrefixOf` scope

-- | Variant of 'tagLinksField' that suppresses any tag whose top
--   (slash-separated) segment equals the given scope. Used by the
--   Library page: an item rendered under the "Research" section
--   should not re-list its own @research\/*@ filings in the tag
--   footer (the section heading makes those structurally implied),
--   but should still list @tech\/*@ cross-filings.
--
--   This is distinct from 'tagLinksFieldExcludingScope', which
--   suppresses only exact-match and strict ancestors. Library's
--   redundancy goal is broader: hide the whole subtree rooted at
--   the section's portal, not just the portal tag itself.
--
--   @
--   scope = "research"
--     t = "research"              → hide  (top = "research" == scope)
--     t = "research/cryptography" → hide  (top = "research" == scope)
--     t = "tech"                  → show  (top = "tech"     /= scope)
--     t = "tech/hpc"              → show  (top = "tech"     /= scope)
--   @
--
--   'noResult' fires when every tag is suppressed so
--   @$if(item-tags)$@ gates off an empty footer wrapper, same
--   discipline as 'tagLinksFieldExcludingScope'.
tagLinksFieldExcludingTopSegment :: String -> String -> Context a
tagLinksFieldExcludingTopSegment fieldName scope =
    tagLinksWhere fieldName (not . matchesTopSegment)
        "no cross-portal tags after top-segment suppression"
  where
    matchesTopSegment t = takeWhile (/= '/') t == scope

-- ---------------------------------------------------------------------------
-- Keyword links field (bibliography-scoped vocabulary, Phase 6a)
-- ---------------------------------------------------------------------------

-- | List context field exposing an item's @keywords:@ frontmatter as
--   @$kw-name$@ / @$kw-url$@ pairs. URL targets @/bibliography/\<kw\>/@,
--   the per-keyword bibliography pages (built by Phase 6b; links will
--   404 until then, deliberately — the mechanism has to be in place
--   before the pages can be populated).
--
--   Shared vocabulary with bib-entry @keywords:@ fields parsed by
--   'BibExtras.parseBibExtras'. An essay tagged with the same keyword
--   as a bib entry will appear alongside that entry on the keyword
--   page.
--
--   Accepts both YAML list and comma-separated scalar forms:
--
--   @
--   keywords: [crypto, lattices]
--   keywords:
--     - crypto
--     - lattices
--   keywords: "crypto, lattices"
--   @
--
--   Returns @noResult@ when absent or empty so the template's
--   @$if(essay-keywords)$@ gate suppresses the meta row.
--
--   Usage in metadata.html:
--
--   @
--   $for(essay-keywords)$\<a class="meta-keyword" href="$kw-url$"\>$kw-name$\</a\>$endfor$
--   @
keywordLinksField :: String -> Context a
keywordLinksField fieldName = listFieldWith fieldName ctx $ \item -> do
    kws <- metadataKeywords <$> getMetadata (itemIdentifier item)
    if null kws
        then noResult "no keywords"
        else return (map toItem kws)
  where
    toItem k = Item (fromFilePath (k ++ "/index.html")) k
    ctx = field "kw-name" (return . itemBody)
       <> field "kw-url"  (\i -> return $ "/bibliography/" ++ itemBody i ++ "/")

-- ---------------------------------------------------------------------------
-- Author links field
-- ---------------------------------------------------------------------------

-- | Exposes each item's authors ('Utils.itemAuthors') as @author-name@ /
--   @author-url@ pairs.
--
--   $for(author-links)$<a href="$author-url$">$author-name$</a>$sep$, $endfor$
authorLinksField :: Context a
authorLinksField = listFieldWith "author-links" ctx $ \item -> do
    cfg <- loadConfig
    names <- itemAuthors (siteAuthor cfg) <$> getMetadata (itemIdentifier item)
    return $ map (\n -> Item (fromFilePath "") (n, authorUrl n)) names
  where
    ctx = field "author-name" (return . fst . itemBody)
       <> field "author-url"  (return . snd . itemBody)

-- ---------------------------------------------------------------------------
-- Abstract field
-- ---------------------------------------------------------------------------

-- | Renders the abstract using Pandoc to support Markdown and LaTeX math.
--   Strips the outer @<p>@ wrapping. A single-paragraph abstract becomes a
--   bare @Plain@ so the rendered HTML is unwrapped inlines. A multi-paragraph
--   abstract (author used a blank line in the YAML literal block) is flattened
--   to a single @Plain@ with @LineBreak@ separators between what were
--   originally paragraph boundaries — the visual break is preserved without
--   emitting stray @<p>@ tags inside the metadata block. Mixed block content
--   (e.g. an abstract containing a blockquote) falls through unchanged.
abstractField :: Context String
abstractField = field "abstract" $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    case lookupString "abstract" meta of
        Nothing -> noResult "no abstract"
        Just src -> do
            let pandocResult = runPure $ do
                    doc <- readMarkdown defaultHakyllReaderOptions (T.pack src)
                    let doc' = case doc of
                                 Pandoc m [Para ils] -> Pandoc m [Plain ils]
                                 Pandoc m blocks
                                   | all isPara blocks && not (null blocks) ->
                                       let joined = intercalate [LineBreak]
                                                      [ils | Para ils <- blocks]
                                       in Pandoc m [Plain joined]
                                 _ -> doc
                    let wOpts = defaultHakyllWriterOptions { writerHTMLMathMethod = MathML }
                    writeHtml5String wOpts doc'
            case pandocResult of
                Left err -> fail $ "Pandoc error rendering abstract: " ++ show err
                Right html -> return (T.unpack html)
  where
    isPara (Para _) = True
    isPara _        = False

-- ---------------------------------------------------------------------------
-- Description field
-- ---------------------------------------------------------------------------

-- | Plain-text page summary for @<meta name="description">@,
--   @og:description@, and @twitter:description@.
--
--   Precedence, first hit wins (audit C01 — 453 of 484 pages had no
--   description at all, because the field resolved from @abstract:@ and
--   nothing else):
--
--     1. @description:@ frontmatter — the explicit override, for when the
--        unfurl summary should differ from the on-page abstract;
--     2. @abstract:@ frontmatter, rendered through Pandoc to plain text;
--     3. for photography pages (anything carrying @photo:@), a factual
--        sentence composed from @location:@, @captured:@, and @series:@;
--     4. a plain-text excerpt of the page's own first body paragraph.
--
--   Every branch collapses whitespace, truncates on a word boundary, and
--   HTML-escapes attribute-special characters. 'noResult' only when all
--   four fail, so @$if(description)$@ still short-circuits on a page with
--   no prose at all (an empty list index, say) rather than emitting an
--   empty meta tag.
descriptionField :: Context String
descriptionField = field "description" $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    case lookupString "description" meta >>= nonEmpty of
        Just d  -> return (finishDescription (T.pack d))
        Nothing -> case lookupString "abstract" meta >>= nonEmpty of
            Just src -> finishDescription <$> plainOfMarkdown src
            Nothing  -> do
                mPhoto <- photoDescription meta
                case mPhoto of
                    Just d  -> return (finishDescription d)
                    Nothing -> case firstParagraphText (itemBody item) of
                        Just p  -> return (finishDescription p)
                        Nothing -> noResult "no description, abstract, photo metadata, or body prose"
  where
    nonEmpty s = if null (trim s) then Nothing else Just s

-- | Render a Markdown fragment to plain text through Pandoc.
--
--   @tex_math_dollars@ is disabled on the /writer/ deliberately. With it
--   on, the plain writer round-trips @$t$@ back out as @$t$@ — correct
--   Markdown, but a meta description reading
--   "length-$t$ nonbacktracking robber paths" is raw notation in a search
--   result. Off, the writer falls back to texmath's plain rendering, which
--   is what a description wants. @subscript@ / @superscript@ are off for
--   the same reason — with them on, texmath's @v@ + subscript @0@ is
--   written back out as @v~0~@; with them off the writer falls through to
--   @\<sub\>0\<\/sub\>@, and 'unwrapSubSup' takes the tags off to leave
--   @v0@. Both markers are noise in a one-line summary.
plainOfMarkdown :: String -> Compiler T.Text
plainOfMarkdown src =
    case runPure (readMarkdown defaultHakyllReaderOptions (T.pack src)
                    >>= writePlain plainOpts) of
        Left err  -> fail $ "Pandoc error rendering description: " ++ show err
        Right txt -> return (unwrapSubSup txt)
  where
    plainOpts = defaultHakyllWriterOptions
        { writerExtensions =
            disableExtension Ext_tex_math_dollars
            . disableExtension Ext_tex_math_single_backslash
            . disableExtension Ext_tex_math_double_backslash
            . disableExtension Ext_subscript
            . disableExtension Ext_superscript
            $ writerExtensions defaultHakyllWriterOptions
        }

-- | Drop the @\<sub\>@ / @\<sup\>@ wrappers the plain writer falls back to
--   for sub- and superscripts, keeping their content. Targeted at exactly
--   those four literals so it cannot swallow a @\<@ that is ordinary prose.
unwrapSubSup :: T.Text -> T.Text
unwrapSubSup =
      T.replace "<sub>" "" . T.replace "</sub>" ""
    . T.replace "<sup>" "" . T.replace "</sup>" ""

-- | Collapse whitespace, cut to ~155 characters on a word boundary, and
--   escape for an attribute value. 155 is the length search engines and
--   unfurlers typically show; cutting mid-word is worse than cutting early.
finishDescription :: T.Text -> String
finishDescription raw =
    let collapsed = T.unwords (T.words raw)
    in  escapeHtml (T.unpack (truncateWords 155 collapsed))

-- | Truncate to at most @n@ characters, backing up to the last word
--   boundary and appending an ellipsis. Text already short enough is
--   returned unchanged.
truncateWords :: Int -> T.Text -> T.Text
truncateWords n t
    | T.length t <= n = t
    | otherwise =
        let cut     = T.take n t
            trimmed = T.dropWhileEnd (/= ' ') cut
            body    = if T.length trimmed < n `div` 2 then cut else trimmed
        in  T.dropWhileEnd (== ' ') (T.dropWhileEnd isTrailingPunct body)
              <> T.pack "\x2026"
  where
    isTrailingPunct c = c `elem` (" ,;:" :: String)

-- | A neutral, factual description for a photograph page, composed from
--   the frontmatter the photography pipeline already requires. Only fires
--   for pages carrying @photo:@ — every photography entry has one, and
--   nothing else does.
photoDescription :: Metadata -> Compiler (Maybe T.Text)
photoDescription meta
    | Nothing <- lookupString "photo" meta = return Nothing
    | otherwise = do
        serName <- traverse seriesTitle (lookupString "series" meta >>= nonEmptyT)
        let loc     = lookupString "location" meta >>= nonEmptyT
            capt    = lookupString "captured" meta >>= formatCaptured
            clauses = concat
                [ [ "made at " <> l | Just l <- [loc]  ]
                , [ "on " <> c      | Just c <- [capt] ]
                , [ "from the " <> s <> " series" | Just s <- [serName] ]
                ]
        return $ if null clauses
            then Nothing
            else Just ("Photograph " <> T.intercalate ", " clauses <> ".")
  where
    nonEmptyT s = if null (trim s) then Nothing else Just (T.pack (trim s))
    formatCaptured s =
        case parseIsoDate (trim s) :: Maybe UTCTime of
            Just d  -> Just (T.pack (formatWriterly d))
            Nothing -> nonEmptyT s

    -- Prefer the series landing page's own title ("Germany, August 2026")
    -- over a mechanical de-slugging of the directory name ("Germany
    -- 082026"). 'getMetadata' on a path no rule claims returns mempty
    -- rather than failing, so a stale or hand-written `series:` value
    -- falls back to the slug instead of breaking the build. The read is
    -- untracked, like every other metadata read: renaming a series
    -- retitles its own page immediately and its frames' descriptions on
    -- their next rebuild.
    seriesTitle slug = do
        m <- getMetadata (fromFilePath
                ("content/photography/" ++ T.unpack slug ++ "/index.md"))
        return $ case lookupString "title" m >>= nonEmptyT of
            Just t  -> t
            Nothing -> humanizeSlug slug

-- | @"europa-2024"@ → @"Europa 2024"@. Series directories are slugs; this
--   is the fallback when a series has no landing page to name it, and is
--   only ever used for prose inside a meta description.
humanizeSlug :: T.Text -> T.Text
humanizeSlug = T.unwords . map capitalise . T.words . T.map dash
  where
    dash '-' = ' '
    dash '_' = ' '
    dash c   = c
    capitalise w = case T.uncons w of
        Just (c, rest) -> T.cons (toUpper c) rest
        Nothing        -> w

-- | Plain text of the first paragraph of a rendered page body.
--
--   The item reaching @templates/default.html@ (where the head partial
--   asks for @$description$@) carries the page's own rendered HTML, so
--   the excerpt is taken from the finished document rather than re-parsing
--   the source — which also means generated pages with no Markdown file of
--   their own (the library, /new.html, the bibliography index) get one.
--
--   Scoped to @#markdownBody@ when present so the excerpt is body prose
--   rather than a metadata strip, skips math spans (their text content is
--   LaTeX source), and skips paragraphs that carry no words.
firstParagraphText :: String -> Maybe T.Text
firstParagraphText html =
    let scoped = case breakOnAfter "id=\"markdownBody\"" html of
                    Just rest -> rest
                    Nothing   -> html
    in  firstNonEmpty (dropNonProse scoped)
  where
    -- Authoring comments, no-JS fallbacks, and inline script/style are not
    -- the page's prose. Comments in particular can *contain* markup — the
    -- search page's opening comment mentions a literal @\<p\>@ — so they
    -- have to go before anything looks for a paragraph, not after.
    dropNonProse =
          dropRegion "<!--"      "-->"
        . dropRegion "<noscript" "</noscript>"
        . dropRegion "<script"   "</script>"
        . dropRegion "<style"    "</style>"

    -- Remove every @open … close@ region. An unterminated region swallows
    -- the rest of the document, which is the conservative answer: better
    -- no description than one quoting a half-parsed comment.
    dropRegion open close s = case breakOnList open s of
        (_,   "")  -> s
        (pre, mid) -> pre ++ dropRegion open close (snd (breakOnList close mid))

    firstNonEmpty s = do
        (para, rest) <- nextParagraph s
        let txt = T.unwords (T.words (T.pack (htmlToText (dropMath para))))
        if T.null txt || T.length txt < 8 then firstNonEmpty rest else Just txt

    nextParagraph s = do
        afterOpen <- breakOnAfter "<p" s
        -- "<p>" or "<p class=…>", but not "<pre>" or "<param>".
        -- 'breakOnAfter' already consumes the ">" it matched, so the
        -- attribute branch must not drop another character.
        rest <- case afterOpen of
                    ('>' : r)         -> Just r
                    c : _ | isSpace c -> breakOnAfter ">" afterOpen
                    _                 -> Nothing
        let (body, after) = breakOnList "</p>" rest
        return (body, after)

    -- Drop <span class="math …">…</span> runs wholesale.
    dropMath s = case breakOnList "class=\"math" s of
        (_,  "")   -> s
        (pre, mid) -> case breakOnList "</span>" mid of
            (_, "")     -> pre
            (_, after)  -> pre ++ dropMath (drop (length ("</span>" :: String)) after)

-- | @breakOnAfter needle haystack@ — everything after the first occurrence
--   of @needle@, or 'Nothing' when it does not occur.
breakOnAfter :: String -> String -> Maybe String
breakOnAfter needle = go
  where
    go [] = Nothing
    go s@(_ : cs) = case stripPrefix needle s of
        Just rest -> Just rest
        Nothing   -> go cs

-- | Split at the first occurrence of a literal, dropping the literal.
--   The second component is empty when the literal does not occur.
breakOnList :: String -> String -> (String, String)
breakOnList needle = go
  where
    go [] = ([], [])
    go s@(c : cs) = case stripPrefix needle s of
        Just rest -> ([], rest)
        Nothing   -> let (a, b) = go cs in (c : a, b)

-- | Remove HTML tags and decode the handful of entities Pandoc emits, so
--   the result is the reader-visible text. Tag removal is Hakyll's
--   'stripTags' (which closes up around inline markup rather than
--   inserting a space, so @\<em\>word\<\/em\>s@ stays one word).
htmlToText :: String -> String
htmlToText = decodeEntities . stripTags
  where
    decodeEntities [] = []
    decodeEntities s@('&' : _) =
        case firstJust [ (repl, ) <$> stripPrefix ent s | (ent, repl) <- entities ] of
            Just (repl, rest) -> repl ++ decodeEntities rest
            Nothing           -> '&' : decodeEntities (drop 1 s)
    decodeEntities (c : cs) = c : decodeEntities cs

    entities =
        [ ("&amp;", "&"), ("&lt;", "<"), ("&gt;", ">")
        , ("&quot;", "\""), ("&#39;", "'"), ("&apos;", "'")
        , ("&nbsp;", " "), ("&hellip;", "\x2026"), ("&mdash;", "\x2014")
        , ("&ndash;", "\x2013"), ("&rsquo;", "\x2019"), ("&lsquo;", "\x2018")
        , ("&ldquo;", "\x201C"), ("&rdquo;", "\x201D")
        ]

    firstJust xs = case [ x | Just x <- xs ] of
        (x : _) -> Just x
        []      -> Nothing

-- | What a page needs loaded, read from its rendered body, so each script
--   and stylesheet goes only where it does something (audit X6). Every
--   essay and post used to load KaTeX, a render-blocking stylesheet among
--   it, and every page Prism, the gallery and the sidenote layout, which
--   12, 6, 9 and 14 pages of 548 use. A context earlier in the chain that
--   sets one of these (the bibliography's @math@, an archive page's
--   backlink math) still wins. The body at the outer template is the
--   inner template's output, footer included, so math that arrives only in
--   a backlink excerpt counts.
--
--   * @math@: KaTeX (Pandoc's KaTeX writer marks math with these classes)
--   * @has-code@: Prism and syntax.css
--   * @has-sidenotes@: sidenotes.js and .css, also where a transclusion may
--     bring sidenotes in
--   * @has-exhibits@: gallery.js (display math, score fragments, exhibits)
pageFeatureFields :: Context String
pageFeatureFields = mconcat
    [ flag "math"          ["class=\"math inline", "class=\"math display"]
    , flag "has-code"      ["class=\"language-", "class=\"sourceCode"]
    , flag "has-sidenotes" ["class=\"sidenote", "data-transclude", "class=\"transclude"]
    , flag "has-exhibits"  ["class=\"math display", "score-fragment", "class=\"exhibit"]
    ]
  where
    flag name markers = field name $ \item ->
        if any (`isInfixOf` itemBody item) markers
            then return "true"
            else noResult ("no " ++ name)

-- | @$plain(field)$@: a field as text that is safe in an attribute or in
--   @<title>@: tags stripped, whitespace collapsed, @& < > " '@ escaped.
--   Hakyll hands metadata to templates as it is written. A title may carry
--   @<em>@ for its heading, which then showed as literal tags in browser
--   tabs, link previews and the feed; and a quotation mark in any
--   free-text field would end the attribute it was put in (audit V04).
plainField :: Context a
plainField = functionField "plain" $ \args _ -> case args of
    [v] -> return (plainText v)
    _   -> fail "plain takes one field"

plainText :: String -> String
plainText = escapeHtml . unwords . words . stripTags

-- | An Atom entry's @title@ as plain text. Hakyll's template writes
--   @$title$@ into a text-type @<title>@, where markup is not allowed
--   (RFC 4287 3.1.1.1); an @<em>@ in one made that entry invalid. No title
--   falls through to the default context's.
feedTitleField :: Context String
feedTitleField = field "title" $ \item -> do
    t <- getMetadataField (itemIdentifier item) "title"
    maybe (noResult "no title") (return . plainText) t

-- ---------------------------------------------------------------------------
-- Summary field
-- ---------------------------------------------------------------------------

-- | Renders the @summary@ frontmatter key through Pandoc, preserving full
--   block structure (paragraphs, bold, lists). Unlike 'abstractField', no
--   paragraph flattening is applied because the summary renders inside its
--   own styled box rather than inline in the metadata strip.
summaryField :: Context String
summaryField = field "summary" $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    case lookupString "summary" meta of
        Nothing -> noResult "no summary"
        Just src -> do
            let pandocResult = runPure $ do
                    doc <- readMarkdown defaultHakyllReaderOptions (T.pack src)
                    let wOpts = defaultHakyllWriterOptions { writerHTMLMathMethod = MathML }
                    writeHtml5String wOpts doc
            case pandocResult of
                Left err -> fail $ "Pandoc error rendering summary: " ++ show err
                Right html -> return (T.unpack html)

siteCtx :: Context String
siteCtx =
    identityContext
    <> navigationContext
    <> plainField
    <> canonicalUrlField
    <> pageScriptsField
    <> pageFeatureFields
    <> abstractField
    <> descriptionField
    <> summaryField
    <> dingbatField
    <> monogramSvgField
    <> hasMonogramField
    <> defaultContext

-- ---------------------------------------------------------------------------
-- Helper: load a named snapshot as a context field
-- ---------------------------------------------------------------------------

-- | @snapshotField name snap@ creates a context field @name@ whose value is
--   the body of the snapshot @snap@ saved for the current item.
snapshotField :: String -> Snapshot -> Context String
snapshotField name snap = field name $ \item ->
    itemBody <$> loadSnapshot (itemIdentifier item) snap

-- ---------------------------------------------------------------------------
-- Essay context
-- ---------------------------------------------------------------------------

-- | Bibliography field: loads the citation HTML saved by essayCompiler.
--   Returns noResult (making $if(bibliography)$ false) when empty.
--   Also provides $has-citations$ for conditional JS loading.
bibliographyField :: Context String
bibliographyField = bibContent <> hasCitations
  where
    bibContent = field "bibliography" $ \item -> do
        bib <- itemBody <$> loadSnapshot (itemIdentifier item) "bibliography"
        if null bib then noResult "no bibliography" else return bib
    hasCitations = field "has-citations" $ \item -> do
        bib <- itemBody <$> (loadSnapshot (itemIdentifier item) "bibliography"
                              :: Compiler (Item String))
        if null bib then noResult "no citations" else return "true"

-- | Further-reading field: loads the further-reading HTML saved by essayCompiler.
--   Returns noResult (making $if(further-reading-refs)$ false) when empty.
furtherReadingField :: Context String
furtherReadingField = field "further-reading-refs" $ \item -> do
    fr <- itemBody <$> (loadSnapshot (itemIdentifier item) "further-reading-refs"
                          :: Compiler (Item String))
    if null fr then noResult "no further reading" else return fr

-- ---------------------------------------------------------------------------
-- Epistemic fields
-- ---------------------------------------------------------------------------

-- | Render an integer 1–5 frontmatter key as filled/empty dot chars.
-- Returns @noResult@ when the key is absent, unparseable, or below 1
-- (a zero would otherwise render five empty circles); values above 5
-- clamp to 5.
dotsField :: String -> String -> Context String
dotsField ctxKey metaKey = field ctxKey $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    case lookupString metaKey meta >>= readMaybe of
        Nothing -> noResult (ctxKey ++ ": not set")
        Just (n :: Int)
            | n < 1 -> noResult (ctxKey ++ ": value below the 1-5 scale")
            | otherwise ->
                let v = min 5 n
                in  return (replicate v '\x25CF' ++ replicate (5 - v) '\x25CB')

-- | @$confidence-trend$@: ↑, ↓, or → derived from the last two entries
-- in the @confidence-history@ frontmatter list.  Returns @noResult@ when
-- there is no history or only a single entry.
--
-- The arrow flips when the absolute change crosses 'trendThreshold'
-- (currently 5 percentage points). Smaller swings count as flat.
--
-- When @confidence: proved@ (or @proven@) is in effect, the arrow is
-- suppressed: a proof either holds or it does not, so tracking trend on
-- a binary-after-the-fact value is incoherent (MARKS.md §4.3). If the
-- frontmatter sets both @confidence: proved@ and @confidence-history:@
-- the build emits a warning and the history is ignored.
confidenceTrendField :: Context String
confidenceTrendField = field "confidence-trend" $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    if isProvedConfidence (lookupString "confidence" meta)
        then do
            case lookupStringList "confidence-history" meta of
                Just _ -> unsafeCompiler $ hPutStrLn stderr $
                    "[Marks] " ++ toFilePath (itemIdentifier item) ++
                    ": confidence: proved is incompatible with confidence-history; ignoring history"
                Nothing -> return ()
            noResult "confidence is proved; trend suppressed"
        else case lookupStringList "confidence-history" meta of
            Nothing -> noResult "no confidence history"
            Just xs -> case lastTwo xs of
                Nothing            -> noResult "no confidence history"
                Just (prevS, curS) ->
                    let prev = readMaybe prevS :: Maybe Int
                        cur  = readMaybe curS  :: Maybe Int
                    in  case (prev, cur) of
                            (Just p, Just c)
                                | c - p >  trendThreshold -> return "\x2191"   -- ↑
                                | p - c >  trendThreshold -> return "\x2193"   -- ↓
                                | otherwise               -> return "\x2192"   -- →
                            _ -> return "\x2192"
  where
    trendThreshold :: Int
    trendThreshold = 5

    -- Total replacement for @(xs !! (length xs - 2), last xs)@: returns
    -- the last two elements of a list, in order, or 'Nothing' when the
    -- list has fewer than two entries.
    lastTwo :: [a] -> Maybe (a, a)
    lastTwo []        = Nothing
    lastTwo [_]       = Nothing
    lastTwo [a, b]    = Just (a, b)
    lastTwo (_ : rest) = lastTwo rest

-- | @$overall-score$@: weighted composite of confidence (60 %) and
--   evidence quality (40 %), expressed as an integer on a 0–100 scale.
--
--   Importance is intentionally excluded from the score: it answers
--   "should you read this?", not "should you trust it?", and folding
--   the two together inflated the number and muddied its meaning.
--   It still appears in the footer as an independent orientation
--   signal — just not as a credibility input.
--
--   The 1–5 evidence scale is rescaled as @(ev − 1) / 4@ rather than
--   plain @ev / 5@. The naive form left a hidden +6 floor (since
--   @1/5 = 0.2@) and a midpoint of 0.6 instead of 0.5; the rescale
--   makes evidence=1 contribute zero and evidence=3 contribute exactly
--   half, so a "true midpoint" entry (conf=50, ev=3) lands on 50.
--
--   Returns @noResult@ when confidence or evidence is absent, so
--   @$if(overall-score)$@ guards the template safely.
--
--   Formula:  raw   = conf/100 · 0.6 + (ev − 1)/4 · 0.4   (0–1)
--             score = clamp₀₋₁₀₀(round(raw · 100))
--
--   The @confidence: proved@ (or @proven@) sentinel — see MARKS.md §4.3 —
--   substitutes @conf = 100@ in the formula. Evidence still varies, so
--   trust is not pinned to 100; a complete proof with weak supporting
--   apparatus (evidence=1) lands at 60, the same as a numeric
--   confidence=100, evidence=1 entry would.
overallScoreField :: Context String
overallScoreField = field "overall-score" $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    case trustScore (confidencePercent (lookupString "confidence" meta))
                    (readMaybe =<< lookupString "evidence" meta) of
        Just score -> return (show score)
        Nothing    -> noResult "overall-score: confidence or evidence not set"

-- | @$confidence$@: numeric override that suppresses the @proved@ /
--   @proven@ sentinel. When the frontmatter value is parseable as an
--   integer this returns its 'show' form; otherwise 'noResult' so the
--   template's @$if(confidence)$@ guard collapses cleanly. The sentinel
--   case is surfaced via 'confidenceProvedField' instead.
--
--   Composed before 'defaultContext' so this override wins; without it
--   @$confidence$@ would render the literal string @"proved"@ and the
--   template's @$confidence$% confidence@ would print @"proved%
--   confidence"@.
confidenceField :: Context String
confidenceField = field "confidence" $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    case lookupString "confidence" meta of
        Nothing -> noResult "no confidence"
        Just s  -> case readMaybe (trim s) :: Maybe Int of
            Just n  -> return (show n)
            Nothing -> noResult "confidence not numeric"

-- | @$confidence-proved$@: present (renders as @"true"@) when
--   @confidence:@ is the @proved@ / @proven@ sentinel; 'noResult'
--   otherwise. Templates branch on this to render @"proved confidence"@
--   in place of the @"XX% confidence"@ chip.
confidenceProvedField :: Context String
confidenceProvedField = field "confidence-proved" $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    if isProvedConfidence (lookupString "confidence" meta)
        then return "true"
        else noResult "confidence is not proved"

-- | @$peer-status$@: validated raw value (slug form) from the
--   @peer-status:@ frontmatter. Used by the template as a class-attribute
--   modifier (@ep-peer-status--retracted@ etc.). Invalid values warn and
--   degrade to 'noResult', so a typo doesn't render an unstyled chip.
--   Absent and @unreviewed@ both produce 'noResult' — the chip is the
--   exception, not the default.
peerStatusField :: Context String
peerStatusField = field "peer-status" $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    case lookupString "peer-status" meta of
        Nothing -> noResult "no peer-status"
        Just raw ->
            let s = map toLower (trim raw)
            in  if s `elem` peerStatusValues
                    then if s == "unreviewed"
                             then noResult "peer-status is unreviewed (default)"
                             else return s
                    else do
                        unsafeCompiler $ hPutStrLn stderr $
                            "[Marks] " ++ toFilePath (itemIdentifier item) ++
                            ": invalid peer-status value \"" ++ raw ++
                            "\"; treating as unreviewed"
                        noResult "invalid peer-status"

-- | @$peer-status-display$@: human-readable form of the @peer-status@
--   value, suitable for the compact-row chip text. Per MARKS.md §4.1 the
--   display strings are:
--
--     * @under-review@   → @"under review"@   (hyphen → space)
--     * @peer-reviewed@  → @"peer-reviewed"@  (kept as-is)
--     * @published@      → @"published"@
--     * @retracted@      → @"retracted"@
--
--   'noResult' for absent, invalid, or @unreviewed@ values, mirroring
--   'peerStatusField'.
peerStatusDisplayField :: Context String
peerStatusDisplayField = field "peer-status-display" $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    case lookupString "peer-status" meta of
        Nothing -> noResult "no peer-status"
        Just raw ->
            case lookup (map toLower (trim raw)) displayMap of
                Just disp -> return disp
                Nothing   -> noResult "no display form (absent / unreviewed / invalid)"
  where
    displayMap =
        [ ("under-review",   "under review")
        , ("peer-reviewed",  "peer-reviewed")
        , ("published",      "published")
        , ("retracted",      "retracted")
        ]

-- | All epistemic context fields composed.
epistemicCtx :: Context String
epistemicCtx =
    dotsField "importance-dots" "importance"
    <> dotsField "evidence-dots" "evidence"
    <> confidenceField
    <> confidenceProvedField
    <> peerStatusField
    <> peerStatusDisplayField
    <> overallScoreField
    <> confidenceTrendField
    <> stabilityField
    <> lastReviewedField
    <> lastReviewedIsoField
    <> epistemicSvgField

-- ---------------------------------------------------------------------------
-- Essay context
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Display date (revision-aware)
-- ---------------------------------------------------------------------------

-- | Resolve an item's display date as a 'UTCTime': the most-recent
--   'revisionDateISO' if the item has a 'revised:' entry, else the
--   creation date via 'getItemUTC'. Falls back to the creation date
--   when a revision's ISO string fails to parse.
--
--   Shared by every revision-aware field below and by
--   'recentFirstByDisplay', so they always agree on what the item's
--   display date is.
itemDisplayUTC :: Item a -> Compiler UTCTime
itemDisplayUTC = identifierDisplayUTC . itemIdentifier

-- | 'itemDisplayUTC' for a bare 'Identifier'. The sitemap works from
--   identifiers rather than items (nothing is loaded for a URL row), and
--   the paginate grouper ("Tags") runs in 'Rules', where no 'Item's exist
--   yet, so this needs only 'MonadMetadata'.
identifierDisplayUTC :: (MonadMetadata m, MonadFail m) => Identifier -> m UTCTime
identifierDisplayUTC ident = do
    meta <- getMetadata ident
    case getRevisions meta of
        (r:_) | Just utc <- (parseIsoDate (revisionDateISO r) :: Maybe UTCTime)
              -> return utc
        _ -> getItemUTC defaultTimeLocale ident

-- | @$date-display$@ — the date shown next to an item in list renderings.
--   Most-recent revision date if the item has a 'revised:' entry, else
--   its creation date. Formatted "17 April 2026".
dateDisplayField :: Context String
dateDisplayField = field "date-display" $ \item ->
    formatWriterly <$> itemDisplayUTC item

-- | @$date-iso$@ — ISO-8601 form of the display date, for
--   @<time datetime="...">@ attributes. Same revision-aware
--   semantics as 'dateDisplayField'.
dateDisplayIsoField :: Context String
dateDisplayIsoField = field "date-iso" $ \item ->
    formatIso <$> itemDisplayUTC item

-- | @$date-original$@ — the item's creation date, present in the
--   context only when the most-recent revision date differs from it.
--   Consumed by the card partial's revision marker, which shows it
--   inline on touch devices and defers it to the hover popup elsewhere.
--   'noResult' otherwise (so the annotation is simply absent for
--   never-revised items).
dateOriginalField :: Context String
dateOriginalField = field "date-original" $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    case getRevisions meta of
        [] -> noResult "no revisions"
        (r:_) -> do
            created <- getItemUTC defaultTimeLocale (itemIdentifier item)
            let createdIso = formatIso created
            if revisionDateISO r == createdIso
                then noResult "revision date equals creation date"
                else return (formatWriterly created)

-- | @$revision-note$@ — prose note attached to the most-recent
--   'revised:' entry, if any. Rendered under the abstract on the item
--   card, where item-card.css hides it visually on pointer devices and
--   the marker's popup carries it instead. 'noResult' when there's no
--   revision, or when the most-recent revision has no note.
revisionNoteField :: Context String
revisionNoteField = field "revision-note" $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    case getRevisions meta of
        (r:_) | Just note <- revisionNote r, not (null (trim note)) -> return note
        _ -> noResult "no revision note"

-- | @$has-revision$@ — present (renders as @"true"@) only when the item
--   has a revision worth annotating: either its display date differs
--   from its creation date, or the most-recent revision carries a note.
--   The card gates its "Revised" marker on this, so a @revised:@ entry
--   that adds neither a new date nor prose stays invisible rather than
--   producing a marker whose popup would be empty.
hasRevisionField :: Context String
hasRevisionField = field "has-revision" $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    case getRevisions meta of
        [] -> noResult "no revisions"
        (r:_) -> do
            created <- getItemUTC defaultTimeLocale (itemIdentifier item)
            let createdIso = formatIso created
                hasNote    = maybe False (not . null . trim) (revisionNote r)
            if revisionDateISO r /= createdIso || hasNote
                then return "true"
                else noResult "revision adds neither a date nor a note"

-- | Bundle of revision-aware fields consumed by the item-card partial:
--   @$date-display$@, @$date-iso$@, @$date-original$@, @$revision-note$@,
--   @$has-revision$@. Compose once on any surface that renders item cards.
revisionDateFields :: Context String
revisionDateFields =
       dateDisplayField
    <> dateDisplayIsoField
    <> dateOriginalField
    <> revisionNoteField
    <> hasRevisionField

-- | @$date-modified$@ — the date of the item's most recent substantive
--   revision, formatted "5 September 2026".
--
--   Audit F11: this used to be @dateField "date-modified"@, i.e. the
--   creation date, so the page footer's "Last modified" line repeated
--   "Created" verbatim and a revised essay claimed never to have changed.
--   The value comes from 'itemDisplayUTC', the same source the cards, the
--   feed's @updated@, and the sitemap's @lastmod@ now use, so the four
--   surfaces cannot disagree.
--
--   Deliberately *not* the build time: a rebuild is not a modification.
dateModifiedField :: Context String
dateModifiedField = field "date-modified" $ \item ->
    formatWriterly <$> itemDisplayUTC item

-- ---------------------------------------------------------------------------
-- Canonical URL
-- ---------------------------------------------------------------------------
--
-- 'canonicalUrlPath' itself lives in "Utils": "Backlinks" needs it to
-- write the href of a backlink source, and "Backlinks" is imported by
-- this module, so it cannot import back. It is re-exported here because
-- this is where every other caller already looks for it.

-- | The three fields every Atom feed on the site needs, so the two feed
--   rules cannot drift apart.
--
--   @$updated$@ is the revision-aware date (audit F11): the most recent
--   @revised:@ entry, or the creation date for a piece never revised. It
--   is deliberately not the build time — a rebuild is not a revision, and
--   stamping one here would push every entry back to the top of every
--   subscriber's reader on every deploy.
--
--   @$published$@ is the creation date and never moves.
--
--   @$url$@ overrides Hakyll's default so the entry's @\<id\>@ and
--   @\<link\>@ carry the same directory-form URL as the canonical link,
--   the sitemap, and the site's own navigation (audit C03), rather than
--   @\/essays\/foo\/index.html@.
feedMetaFields :: Context String
feedMetaFields =
       field "updated"
           (fmap (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ")
                . itemDisplayUTC)
    <> dateField "published" "%Y-%m-%dT%H:%M:%SZ"
    <> field "url" (\item -> do
           mRoute <- getRoute (itemIdentifier item)
           case mRoute of
               Just r  -> return (canonicalUrlPath r)
               Nothing -> noResult "feed item has no route")

-- | @$canonical-url$@ — 'canonicalUrlPath' of the item's own route.
--
--   Audit C03: the head emitted @$url$@ (Hakyll's raw route) into both
--   @rel=canonical@ and @og:url@, so a page advertised
--   @\/essays\/foo\/index.html@ as its identity while every link to it,
--   the sitemap, and the search metadata said @\/essays\/foo\/@.
--
--   Always resolves — an unrouted item would otherwise break the head
--   template for every page that shares it — falling back to @"\/"@.
canonicalUrlField :: Context String
canonicalUrlField = field "canonical-url" $ \item ->
    maybe "/" canonicalUrlPath <$> getRoute (itemIdentifier item)

-- | Sort items most-recent-first by 'itemDisplayUTC' — same ordering
--   the card shows in its date gutter, so items with recent revisions
--   move to the top without divorcing the sort key from the visible
--   date. Callers: the @/new.html@ rule, 'Tags.applyTagRules', and
--   the library rule.
--
--   Within a single day the tie is broken by 'isNewAtDisplay': a piece
--   published that day sorts above a piece that merely picked the day
--   up from a @revised:@ entry. A day that carries both should read as
--   "here is the new thing, and here is what else moved", not as an
--   arbitrary interleaving. Ties beyond that stay in input order
--   ('sortBy' is stable).
recentFirstByDisplay :: [Item a] -> Compiler [Item a]
recentFirstByDisplay items = do
    keyed <- mapM key items
    return $ map snd $ sortBy (comparing fst) keyed
  where
    key i = do
        displayed <- itemDisplayUTC i
        fresh     <- isNewAtDisplay i
        -- Day first so "same day" survives any time component on the
        -- date; the full timestamp only orders across distinct days.
        return ((Down (utctDay displayed), Down fresh, Down displayed), i)

-- | Is the item at its display date because it was /published/ then,
--   rather than revised then? True for never-revised pieces, for a
--   revision dated the same day as publication, and for the fallback
--   'itemDisplayUTC' takes when a revision's date fails to parse —
--   in every one of those cases the displayed date /is/ the creation
--   date, which is exactly what the comparison asks.
isNewAtDisplay :: Item a -> Compiler Bool
isNewAtDisplay item = do
    displayed <- itemDisplayUTC item
    created   <- getItemUTC defaultTimeLocale (itemIdentifier item)
    return (utctDay displayed == utctDay created)

-- ---------------------------------------------------------------------------
-- Revised: frontmatter schema
-- ---------------------------------------------------------------------------

-- | A single entry from a @revised:@ frontmatter list. Exposed so
--   downstream Phase-5 consumers (the 'dateDisplayField' implementation
--   and the revision-annotation fields on the item card) can all read
--   the same canonical form.
data Revision = Revision
    { revisionDateISO :: String        -- ^ ISO-8601 date, e.g. "2026-04-10"
    , revisionNote    :: Maybe String  -- ^ optional prose note for the entry
    }

-- | Parse and normalize the @revised:@ frontmatter field into a list
--   of 'Revision' entries, sorted most-recent-first (ISO @YYYY-MM-DD@
--   strings sort lexicographically in chronological order, so
--   reverse-sorting them yields most-recent-first).
--
--   Accepted frontmatter shapes:
--
--   @
--   -- Scalar shorthand (normalized to one entry with no note)
--   revised: "2026-04-10"
--
--   -- Canonical list of objects
--   revised:
--     - date: "2026-04-10"
--       note: "expanded §3 on Shestov"
--     - date: "2025-12-03"              -- note optional per-entry
--   @
--
--   The two shapes normalize to the same list-of-'Revision' form here.
--   No other site code should branch on the frontmatter shape —
--   everything downstream reads this function's output.
--
--   Entries that fail to parse (missing @date:@, non-string values,
--   unexpected types) are silently dropped rather than erroring the
--   whole build; the site still compiles with a malformed @revised:@.
getRevisions :: Metadata -> [Revision]
getRevisions meta =
    sortBy (flip (comparing revisionDateISO)) $
    case KM.lookup "revised" meta of
        Just (String t) -> [Revision (T.unpack t) Nothing]
        Just (Array v)  -> mapMaybe parseEntry (V.toList v)
        _               -> []
  where
    parseEntry (Object o) = do
        d <- getString =<< KM.lookup "date" o
        return (Revision d (getString =<< KM.lookup "note" o))
    parseEntry _ = Nothing

    getString (String t) = Just (T.unpack t)
    getString _          = Nothing

essayCtx :: Context String
essayCtx =
    authorLinksField
    <> affiliationField
    <> snapshotField "toc"          "toc"
    <> snapshotField "word-count"   "word-count"
    <> snapshotField "reading-time" "reading-time"
    <> bibliographyField
    <> furtherReadingField
    <> backlinksField
    <> similarLinksField
    <> footerDepsField
    <> epistemicCtx
    <> versionHistoryField
    <> versionHistoryPrimaryField
    <> versionHistoryRestField
    <> versionHistoryRangeField
    <> versionHistoryRangeStartField
    <> versionHistoryRangeEndField
    <> versionHistoryCommitsField
    <> dateField "date-created" writerlyDate
    -- "Last modified" is the revision date, not the creation date (F11).
    <> dateModifiedField
    <> revisionDateFields
    <> tagLinksField "essay-tags"
    <> keywordLinksField "essay-keywords"
    <> siteCtx

-- ---------------------------------------------------------------------------
-- Post context
-- ---------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    authorLinksField
    <> affiliationField
    <> backlinksField
    <> similarLinksField
    <> footerDepsField
    <> dateField "date"     writerlyDate
    <> dateField "date-iso" isoDate
    -- Blog posts can opt in to the epistemic figure / chips by setting
    -- the relevant frontmatter fields, the same ones essays carry. The
    -- Marks module's epistemic SVG field returns 'noResult' when
    -- @status:@ is absent, so unstatused posts render unchanged.
    <> epistemicCtx
    <> siteCtx

-- ---------------------------------------------------------------------------
-- Page context
-- ---------------------------------------------------------------------------

pageCtx :: Context String
pageCtx = authorLinksField <> affiliationField <> siteCtx

-- ---------------------------------------------------------------------------
-- Reading contexts (fiction + poetry)
-- ---------------------------------------------------------------------------

-- | Base reading context: essay fields + the "reading" flag (activates
--   reading.css / reading.js via head.html and body class via default.html).
readingCtx :: Context String
readingCtx = essayCtx <> constField "reading" "true"

-- | Poetry context: reading mode + "poetry" flag for CSS body class.
poetryCtx :: Context String
poetryCtx = readingCtx <> constField "poetry" "true"

-- | Fiction context: reading mode + "fiction" flag for CSS body class.
fictionCtx :: Context String
fictionCtx = readingCtx <> constField "fiction" "true"

-- ---------------------------------------------------------------------------
-- Composition context (music landing pages + score reader)
-- ---------------------------------------------------------------------------

data Movement = Movement
    { movName     :: String
    , movPage     :: Int
    , movDuration :: String
    , movAudio    :: Maybe String
      -- | A tempo or character marking shown beside the movement's name
      --   (@♩ = 80@, @Molto rubato@) — optional, for the landing page.
    , movTempo    :: Maybe String
      -- | A movement listed but not published — @status: in revision@.
      --   It keeps its place in the list, without a page, a duration, or a
      --   button in the reader.
    , movStatus   :: Maybe String
      -- | Offset of this movement into the composition's full @recording@,
      --   normalised to whole seconds. Parsed and carried through the
      --   context now so the schema is settled before any composition
      --   exists; no template consumes it yet.
    , movTime     :: Maybe Int
    }

-- | A movement together with the URLs its template row needs.
--
--   Inside @$for(movements)$@ a field lookup resolves against the movement
--   sub-context /alone/ — the composition's own fields (@$slug$@,
--   @$has-score$@, @$score-url$@) are not in scope there. Anything a
--   movement row needs from its parent must therefore be resolved while
--   building the list and carried in, which is what this wrapper is for.
data MovementView = MovementView
    { mvMovement :: Movement
    , mvScoreUrl :: Maybe String   -- ^ deep link into the reader at this movement
    , mvAudioUrl :: Maybe String   -- ^ absolute URL of this movement's recording
    }

-- | Parse the @movements@ frontmatter key. Returns parsed movements and a
--   list of human-readable warnings for any entries that failed to parse.
--   Callers can surface the warnings via 'unsafeCompiler' so silent typos
--   don't strip movements without diagnostic.
--
--   A movement is dropped only when a /required/ field (name, page,
--   duration) is missing. A malformed optional @time@ warns but keeps the
--   movement — losing a whole movement over a mistyped timecode would be
--   a worse failure than ignoring the timecode.
parseMovementsWithWarnings :: Metadata -> ([Movement], [String])
parseMovementsWithWarnings meta =
    case KM.lookup "movements" meta of
        Just (Array v) ->
            let results = zipWith parseIndexed [1 :: Int ..] (V.toList v)
            in  ( [m | (Just m, _) <- results]
                , concatMap snd results
                )
        _ -> ([], [])
  where
    parseIndexed i (Object o) =
        case mkMovement o of
            Nothing -> (Nothing, [missingRequired i])
            Just mk ->
                let raw    = KM.lookup "time" o
                    parsed = raw >>= scalarText >>= parseTimecode
                in  case (raw, parsed) of
                        (Just _, Nothing) -> (Just (mk Nothing),  [badTime i])
                        _                 -> (Just (mk parsed), [])
    parseIndexed i _ = (Nothing, [missingRequired i])

    missingRequired i =
        "movement #" ++ show i ++ " is missing a required field "
        ++ "(name, page, or duration — or name and status) — entry skipped"

    badTime i =
        "movement #" ++ show i ++ " has an unparseable time: — expected "
        ++ "H:MM:SS, M:SS, or a plain second count; offset ignored"

    -- Every field but the time offset, which 'parseIndexed' applies so a
    -- bad timecode can warn without discarding the movement. A movement
    -- with a status is listed, not published: it needs only a name.
    mkMovement o =
        let status = getString =<< KM.lookup "status" o
            page   = getInt    =<< KM.lookup "page" o
            dur    = getString =<< KM.lookup "duration" o
        in  Movement
                <$> (getString =<< KM.lookup "name" o)
                <*> maybe (0  <$ status) Just page    -- optional only with a status
                <*> maybe ("" <$ status) Just dur
                <*> pure (getString =<< KM.lookup "audio" o)
                <*> pure (getString =<< KM.lookup "tempo" o)
                <*> pure status

    getString (String t) = Just (T.unpack t)
    getString _          = Nothing

    getInt (Number n) = Just (floor (fromRational (toRational n) :: Double))
    getInt _          = Nothing

-- | A YAML scalar as text, so @time: "3:41"@ and @time: 221@ both reach
--   'parseTimecode' by the same route.
scalarText :: Value -> Maybe String
scalarText (String t) = Just (T.unpack t)
scalarText (Number n) =
    Just (show (floor (fromRational (toRational n) :: Double) :: Int))
scalarText _          = Nothing

-- | Parse a recording offset to whole seconds. Accepts @H:MM:SS@, @M:SS@,
--   or a plain second count. Normalising at build time keeps author-facing
--   time formats out of the reader's JavaScript entirely.
parseTimecode :: String -> Maybe Int
parseTimecode s =
    case (traverse readMaybe parts :: Maybe [Int]) of
        Just [sec]       -> Just sec
        Just [m, sec]    -> Just (m * 60 + sec)
        Just [h, m, sec] -> Just (h * 3600 + m * 60 + sec)
        _                -> Nothing
  where
    parts = map T.unpack (T.splitOn ":" (T.pack (trim s)))

parseMovements :: Metadata -> [Movement]
parseMovements = fst . parseMovementsWithWarnings

-- | Split a movement name into its roman numeral and the rest, so the
--   landing page can hang the numerals in their own column:
--   @"III. Sempre molto espressivo"@ → @("III.", "Sempre molto espressivo")@.
--   A name with no leading numeral is all title.
splitNumeral :: String -> (String, String)
splitNumeral name =
    let (num, rest) = span (`elem` ("IVXLC" :: String)) name
    in  case rest of
            '.' : rest' | not (null num) -> (num ++ ".", trim rest')
            _ | not (null num), all isSpace (take 1 rest) -> (num ++ ".", trim rest)
            _ -> ("", name)

-- | Straight quotes in a duration become the primes they stand for:
--   @ca. 35'@ → @ca. 35′@, @4'30"@ → @4′30″@.
durationPrimes :: String -> String
durationPrimes = map prime
  where
    prime '\'' = '′'
    prime '"'  = '″'
    prime c    = c

-- | Extract the composition slug from an item's identifier.
--   "content/music/symphonic-dances/index.md" → "symphonic-dances"
compSlug :: Item a -> String
compSlug = takeFileName . takeDirectory . toFilePath . itemIdentifier

-- | Whether a composition declares a score at all — either an explicit
--   @score-pages@ list or a @score-dir@ to be globbed.
--
--   Metadata-only by necessity: 'Site.rules' consults this through
--   'matchMetadata' at rule-generation time, where the 'Compiler' monad
--   (and so 'getMatches') does not exist yet. A @score-dir@ naming an
--   empty directory therefore still routes a reader page; 'scorePageList'
--   is the authority on which pages actually exist, and @$has-score$@
--   goes through it.
declaresScore :: Metadata -> Bool
declaresScore meta =
    not (null (fromMaybe [] (lookupStringList "score-pages" meta)))
    || maybe False (not . null . trim) (lookupString "score-dir" meta)

-- | A composition's score pages as source-relative paths, in reading order.
--
--   An explicit @score-pages@ list wins when present — it is the escape
--   hatch for an order no filename convention can express. Otherwise
--   @score-dir@ is globbed against Hakyll's identifiers. Hand-listing sixty
--   orchestral pages is not viable, and a hand-list that falls out of sync
--   with the directory fails silently.
--
--   The glob records no dependency of its own. Every rule whose context
--   calls this carries @musicScoreDep@ (build/Site.hs), one pattern over
--   all of content/music, so dropping a page into the directory still
--   triggers a rebuild. Through 'getMatches', each call recorded its own
--   copy of the glob, up to 29 per page, and Hakyll re-matches every copy
--   against every identifier on every build: 3 s of each pass's
--   out-of-date check (audit H06).
scorePageList :: Item a -> Compiler [FilePath]
scorePageList item = do
    meta <- getMetadata (itemIdentifier item)
    let explicit = fromMaybe [] (lookupStringList "score-pages" meta)
    if not (null explicit)
        then return explicit
        else case fmap trim (lookupString "score-dir" meta) of
            Just dir | not (null dir) -> do
                let srcDir = takeDirectory (toFilePath (itemIdentifier item))
                universe <- compilerUniverse <$> compilerAsk
                let ids = filterMatches (fromGlob (srcDir </> dir </> "*.svg"))
                                        (Set.toList universe)
                return $ sortBy compareNatural
                    [ makeRelative srcDir (toFilePath i) | i <- ids ]
            _ -> return []

-- | Page 1's thumbnail, @thumb.webp@ beside the pages, as a path relative
--   to the composition's directory, when the importer has written one
--   (tools/music-import.py, audit MO4). The frontispiece and the /music/
--   shelf show it instead of the page itself: an image to decode, not
--   2,000 outlines to rasterise at a fifth of their size. Read through
--   'unsafeCompiler' like the timing map; @musicScoreDep@ covers it.
scoreThumb :: Item a -> Compiler (Maybe String)
scoreThumb item = do
    pages <- scorePageList item
    case pages of
        []      -> return Nothing
        (p : _) -> do
            let rel    = takeDirectory p </> "thumb.webp"
                srcDir = takeDirectory (toFilePath (itemIdentifier item))
            exists <- unsafeCompiler (doesFileExist (srcDir </> rel))
            return (if exists then Just rel else Nothing)

-- | Filename ordering that compares digit runs numerically, so
--   @page-2.svg@ sorts before @page-10.svg@ whether or not the author
--   zero-padded. Plain lexicographic order silently interleaves the pages
--   of any unpadded score — a failure that reads as an engraving mistake
--   rather than a build one. The raw path breaks ties, so the ordering is
--   total even when padding makes two names numerically equal.
-- | The width-to-height ratio of an SVG, as a bare decimal. A plain
--   number rather than an @aspect-ratio@ pair because the stylesheet needs
--   it in @calc()@ too — deriving the fit-to-height width from the viewport
--   is what lets the reader size a page correctly before its script runs.
--   Prefers @viewBox@; falls back to the @width@ and
--   @height@ attributes, whose ratio needs no unit conversion because both
--   carry the same unit (@279.4mm@ / @215.9mm@ is as usable as a viewBox).
--
--   Only the head of the file is scanned: the root element's attributes are
--   the first thing in the document, and a sixty-page orchestral score is
--   not worth reading into memory to learn its page shape.
svgAspect :: FilePath -> IO (Maybe String)
svgAspect path = do
    r <- try (TIO.readFile path) :: IO (Either IOException T.Text)
    return $ case r of
        Left _     -> Nothing
        Right body -> ratioOf (T.take 2048 body)
  where
    ratioOf head' =
        case viewBoxRatio head' of
            Just x  -> Just x
            Nothing -> do
                w <- number =<< attrValue " width" head'
                h <- number =<< attrValue " height" head'
                format w h

    viewBoxRatio head' = do
        vb <- attrValue "viewBox" head'
        case mapMaybe number (T.words (T.map commaToSpace vb)) of
            [_, _, w, h] -> format w h
            _            -> Nothing

    commaToSpace c = if c == ',' then ' ' else c

    format w h
        | w > 0 && h > 0 = Just (showFFloat' (w / h))
        | otherwise      = Nothing

    -- 4 decimals is finer than any display can resolve and keeps the
    -- generated attribute short.
    showFFloat' x = show (fromIntegral (round (x * 10000) :: Integer) / 10000
                            :: Double)

    -- Leading numeric run of an attribute value, so "279.4mm" parses.
    number t =
        let (digits, _) = T.span (\c -> isDigit c || c == '.' || c == '-') (T.strip t)
        in  if T.null digits then Nothing
                             else readMaybe (T.unpack digits) :: Maybe Double

-- | The value of an XML attribute, found by literal scan. @name@ should
--   carry its own leading space where the bare name would otherwise match a
--   longer one (@\" width\"@ must not match @stroke-width@).
attrValue :: T.Text -> T.Text -> Maybe T.Text
attrValue name body =
    let (_, rest) = T.breakOn (name <> "=\"") body
    in  if T.null rest
            then Nothing
            else let v          = T.drop (T.length name + 2) rest
                     (val, end) = T.breakOn "\"" v
                 in  if T.null end then Nothing else Just val

compareNatural :: FilePath -> FilePath -> Ordering
compareNatural = comparing (\path -> (chunks path, path))
  where
    chunks :: String -> [Either Integer String]
    chunks [] = []
    chunks cs@(c : _)
        | isDigit c = let (d, r) = span  isDigit cs in Left  (read d)          : chunks r
        | otherwise = let (t, r) = break isDigit cs in Right (map toLower t) : chunks r

-- | Context for music composition landing pages and the score reader.
--   Extends essayCtx with composition-specific fields:
--     $slug$             — URL slug (e.g. "symphonic-dances")
--     $score-url$        — absolute URL of the score reader page
--     $has-score$        — present when the composition resolves to >=1 page
--     $score-page-count$ — total number of score pages
--     $score-pages$      — list of {score-page-url} items
--     $first-score-page$ — URL of page 1, for the scripting-off fallback
--     $score-aspect$     — page 1's width/height as a bare decimal
--     $score-portrait$   — present when page 1 is taller than it is wide
--     $has-movements$    — present when movements frontmatter is non-empty
--     $movements$        — list of {movement-name, movement-page,
--                            movement-duration, movement-time, has-audio,
--                            movement-audio-url, movement-score-url}
--   All other frontmatter keys (instrumentation, duration, premiere,
--   commissioned-by, pdf, abstract, etc.) are available via defaultContext.
compositionCtx :: Context String
compositionCtx =
    constField "composition" "true"
    <> slugField
    <> durationField
    <> scoringField
    <> scoreStackField
    <> hasNotesField
    <> realizationField
    <> scoreUrlField
    <> hasScoreField
    <> scorePageCountField
    <> scorePagesListField
    <> firstScorePageField
    <> scoreThumbField
    <> scoreAspectField
    <> scorePortraitField
    <> hasMovementsField
    <> movementsListField
    <> essayCtx
  where
    slugField = field "slug" (return . compSlug)

    -- Authors type durations with the keyboard's straight quotes; the
    -- page sets them as the minute and second primes they stand for.
    durationField = field "duration" $ \item -> do
        meta <- getMetadata (itemIdentifier item)
        maybe (noResult "no duration") (return . durationPrimes)
              (lookupString "duration" meta)

    -- The full scoring, one line per instrument or section, as a score's
    -- inside cover lists it. Optional: `instrumentation` stays the short
    -- form the catalog and the title block use.
    scoringField = listFieldWith "scoring" (field "scoring-line" (return . itemBody)) $ \item -> do
        meta <- getMetadata (itemIdentifier item)
        case lookupStringList "scoring" meta of
            Just ls@(_ : _) ->
                return [ Item (fromFilePath ("scoring" ++ show i)) l
                       | (i, l) <- zip [1 :: Int ..] ls ]
            _ -> noResult "no scoring"

    -- Programme notes are optional, and an empty body still renders as
    -- a present (empty) @$body$@, so the divider above the notes would
    -- otherwise stand over nothing.
    hasNotesField = field "has-notes" $ \item ->
        if all isSpace (itemBody item) then noResult "no notes" else return "true"

    -- A realization is present when the importer has left its timing map
    -- beside the pages; the map names its own audio file. Present only
    -- with pages too, since following needs a score to follow.
    realizationField = field "realization" $ \item -> do
        meta  <- getMetadata (itemIdentifier item)
        pages <- scorePageList item
        let dir    = maybe "scores" (reverse . dropWhile (== '/') . reverse . trim)
                           (lookupString "score-dir" meta)
            srcDir = takeDirectory (toFilePath (itemIdentifier item))
        exists <- unsafeCompiler (doesFileExist (srcDir </> dir </> "timing.json"))
        if exists && not (null pages)
            then return ("/music/" ++ compSlug item ++ "/" ++ dir ++ "/timing.json")
            else noResult "no realization"

    -- How many sheets show beneath the frontispiece: a sonata is a thin
    -- sheaf, a symphony a thick one. Logarithmic, so the stack stays a
    -- detail rather than a bar chart.
    scoreStackField = field "score-stack" $ \item -> do
        n <- length <$> scorePageList item
        if n == 0 then noResult "no score pages" else return . show $
            (if n < 2 then 0 else if n < 16 then 1 else if n < 64 then 2 else 3 :: Int)

    scoreUrlField = field "score-url" $ \item ->
        return $ "/music/" ++ compSlug item ++ "/score/"

    -- All four score fields resolve through 'scorePageList', so an
    -- explicit list and a globbed directory behave identically and
    -- @$has-score$@ can never disagree with @$score-page-count$@.
    hasScoreField = field "has-score" $ \item -> do
        pages <- scorePageList item
        if null pages then noResult "no score pages" else return "true"

    scorePageCountField = field "score-page-count" $ \item ->
        show . length <$> scorePageList item

    scorePagesListField = listFieldWith "score-pages" spCtx $ \item -> do
        let base = "/music/" ++ compSlug item ++ "/"
        pages <- scorePageList item
        return $ map (\p -> Item (fromFilePath p) (base ++ p)) pages
      where
        spCtx = field "score-page-url" (return . itemBody)

    -- Page 1's URL, so the template can render it into the markup instead
    -- of leaving an empty src for JavaScript to fill: that is what makes
    -- the reader show a score with scripting off, paint the first page
    -- without waiting on a fetch, and reserve its box before layout.
    firstScorePageField = field "first-score-page" $ \item -> do
        pages <- scorePageList item
        case pages of
            (p : _) -> return $ "/music/" ++ compSlug item ++ "/" ++ p
            []      -> noResult "no score pages"

    scoreThumbField = field "score-thumb" $ \item -> do
        thumb <- scoreThumb item
        case thumb of
            Just t  -> return $ "/music/" ++ compSlug item ++ "/" ++ t
            Nothing -> noResult "no thumbnail"

    -- Present when page 1 is taller than it is wide. Portrait pages default
    -- to fit-width rather than fit-height: a portrait sheet in a landscape
    -- window leaves most of the screen empty at fit-height, and on a
    -- large-ensemble score that shrinks the staves past readability. A
    -- 33-stave concert band page is legible at fit-width and is not at
    -- fit-height; a 13-stave landscape page is legible at either.
    scorePortraitField = field "score-portrait" $ \item -> do
        pages <- scorePageList item
        case pages of
            []      -> noResult "no score pages"
            (p : _) -> do
                let srcDir = takeDirectory (toFilePath (itemIdentifier item))
                aspect <- unsafeCompiler (svgAspect (srcDir </> p))
                case aspect >>= (readMaybe :: String -> Maybe Double) of
                    Just r | r < 1 -> return "true"
                    _              -> noResult "page 1 is not portrait"

    -- The sheet's shape, read from page 1 at build time so its box is
    -- reserved before anything loads. That removes the first-paint layout
    -- shift for everyone, and gives the scripting-off fallback a correctly
    -- proportioned page instead of the stylesheet's generic portrait
    -- default.
    --
    -- Read through 'unsafeCompiler' rather than 'load': score pages are
    -- copied byte-for-byte by 'copyFileCompiler', so no String item exists
    -- to depend on. 'scorePageList' registers the directory glob, so adding
    -- or removing pages rebuilds this — but re-exporting the same filenames
    -- at a different page size does not, and needs a clean build to show up.
    scoreAspectField = field "score-aspect" $ \item -> do
        pages <- scorePageList item
        case pages of
            []      -> noResult "no score pages"
            (p : _) -> do
                let srcDir = takeDirectory (toFilePath (itemIdentifier item))
                aspect <- unsafeCompiler (svgAspect (srcDir </> p))
                maybe (noResult "page 1 declares no usable dimensions")
                      return aspect

    hasMovementsField = field "has-movements" $ \item -> do
        meta <- getMetadata (itemIdentifier item)
        if null (parseMovements meta) then noResult "no movements" else return "true"

    movementsListField = listFieldWith "movements" movCtx $ \item -> do
        meta <- getMetadata (itemIdentifier item)
        pageCount <- length <$> scorePageList item
        let (mvs, warnings) = parseMovementsWithWarnings meta
            ident = toFilePath (itemIdentifier item)
            -- `page` indexes the reader (1..n), not the printed folio. The
            -- two diverge the moment a score has front matter, and the only
            -- symptom is a movement button that jumps to the wrong place —
            -- so an out-of-range page is worth saying out loud.
            outOfRange =
                [ "movement \"" ++ movName mv ++ "\" starts at page "
                  ++ show (movPage mv) ++ ", but the score has "
                  ++ show pageCount ++ " page(s) — `page:` is the reader's "
                  ++ "1-based index, not the printed page number"
                | pageCount > 0, mv <- mvs
                , movStatus mv == Nothing
                , movPage mv < 1 || movPage mv > pageCount
                ]
        unsafeCompiler $ mapM_
            (\w -> putStrLn $ "[Movements] " ++ ident ++ ": " ++ w)
            (warnings ++ outOfRange)
        let slug = compSlug item
            viewOf m = MovementView
                { mvMovement = m
                , mvScoreUrl =
                    if pageCount > 0 && movStatus m == Nothing
                        then Just $ "/music/" ++ slug ++ "/score/?p="
                                     ++ show (movPage m)
                        else Nothing
                , mvAudioUrl =
                    (\a -> "/music/" ++ slug ++ "/" ++ a) <$> movAudio m
                }
        return $ zipWith
            (\idx m -> Item (fromFilePath ("mv" ++ show (idx :: Int))) (viewOf m))
            [1..] mvs
      where
        movCtx =
            plainField
            <> field "movement-name"        (return . movName     . mvOf)
            <> field "movement-numeral"
                (\i -> case fst (splitNumeral (movName (mvOf i))) of
                          "" -> noResult "no numeral"
                          n  -> return n)
            <> field "movement-title"
                (\i -> case snd (splitNumeral (movName (mvOf i))) of
                          "" -> noResult "no title"
                          t  -> return t)
            <> field "movement-tempo"
                (\i -> maybe (noResult "no tempo") return (movTempo (mvOf i)))
            <> field "movement-status"
                (\i -> maybe (noResult "published") return (movStatus (mvOf i)))
            <> field "movement-page"     (return . show . movPage . mvOf)
            <> field "movement-duration" (return . durationPrimes . movDuration . mvOf)
            <> field "movement-time"
                (\i -> maybe (noResult "no time offset") (return . show)
                             (movTime (mvOf i)))
            <> field "movement-audio-url"
                (\i -> maybe (noResult "no audio") return (mvAudioUrl (itemBody i)))
            <> field "has-audio"
                (\i -> maybe (noResult "no audio") (const (return "true"))
                             (mvAudioUrl (itemBody i)))
            <> field "movement-score-url"
                (\i -> maybe (noResult "no score") return (mvScoreUrl (itemBody i)))
          where
            mvOf = mvMovement . itemBody

-- ---------------------------------------------------------------------------
-- Photography context
-- ---------------------------------------------------------------------------

-- | Extract the photo entry's slug from its identifier.
--
--   * Flat single   @content/photography/<slug>.md@      → @<slug>@
--   * Directory     @content/photography/<slug>/index.md@ → @<slug>@
--
--   The slug is the URL segment under @/photography/@ and the directory
--   name into which co-located assets (the photo, future EXIF + palette
--   sidecars) are copied by the asset rule.
photoSlug :: Item a -> String
photoSlug item =
    let fp     = toFilePath (itemIdentifier item)
        fname  = takeFileName fp
    in  if fname == "index.md"
        then takeFileName (takeDirectory fp)
        else takeWhile (/= '.') fname

-- ---------------------------------------------------------------------------
-- Sidecar reader (Phase 3)
-- ---------------------------------------------------------------------------
--
-- @{photo}.exif.yaml@ and @{photo}.palette.yaml@ are produced by the
-- Python tools at @make build@ time (see @tools/extract-exif.py@ and
-- @tools/extract-palette.py@). They live alongside the photo file
-- under @content/photography/<slug>/@ and back-fill metadata that the
-- author chose not to write in frontmatter.
--
-- Read strategy: 'unsafeCompiler' + 'doesFileExist'. Sidecars are NOT
-- registered as Hakyll items, so this read bypasses the dependency
-- tracker. That is acceptable because:
--
--   * The Python tools always run before @cabal run site -- build@
--     (the Makefile orders them that way).
--   * Re-running the EXIF / palette extractor invalidates only those
--     fields' rendered output; rebuilding @make build@ from scratch
--     covers the dependency-edge case for free.
--
-- Resolution rule for every sidecar-backed field: frontmatter wins;
-- if frontmatter is absent OR empty, fall back to sidecar; if neither
-- supplies a value, return 'noResult' so the consuming template's
-- @$if(...)$@ guard suppresses the row.

-- | Compute the sidecar path for a photo entry.
--
--   @suffix@ is @".exif.yaml"@ or @".palette.yaml"@.
--   Returns @Nothing@ when the entry has no @photo:@ frontmatter or
--   when the entry is flat-form (no co-located asset directory).
photoSidecarPath :: String -> Item a -> Compiler (Maybe FilePath)
photoSidecarPath suffix item = do
    meta <- getMetadata (itemIdentifier item)
    let fp      = toFilePath (itemIdentifier item)
        isFlat  = takeDirectory fp == "content/photography"
    -- Same test as photoUrlField, and for the same reason: what matters is
    -- whether the entry has a co-located asset directory, which is true of
    -- directory-form singles AND series children. Gating on index.md meant
    -- series children resolved no sidecar at all — so every photograph in a
    -- series silently lost its width and height attributes, and with them
    -- the layout-shift protection those exist to provide.
    case (isFlat, lookupString "photo" meta) of
        (False, Just photo) | not (null photo) ->
            return $ Just $ takeDirectory fp </> photo ++ suffix
        _ -> return Nothing

-- | Load a sidecar YAML file as an Aeson Object (same shape Hakyll
--   uses for frontmatter). Returns 'Aeson.empty' when the file is
--   missing or fails to parse — sidecars are advisory, never fatal.
--
--   Parsed once per modification time: a photograph's page reads its
--   sidecars for a dozen fields (camera, lens, exposure, palette,
--   dimensions, …), and each used to parse the file again. Keyed on the
--   mtime, not just the path, so a sidecar rewritten under @make watch@
--   is read afresh.
loadSidecar :: FilePath -> IO Aeson.Object
loadSidecar path = do
    mtime <- (Just <$> getModificationTime path)
                `catch` \(_ :: IOException) -> return Nothing
    case mtime of
        Nothing -> return KM.empty
        Just t  -> do
            seen <- Map.lookup path <$> readIORef sidecarCache
            case seen of
                Just (t', obj) | t' == t -> return obj
                _ -> do
                    decoded <- Y.decodeFileEither path
                    let obj = case decoded of
                            Right (Object o) -> o
                            _                -> KM.empty
                    atomicModifyIORef' sidecarCache (\m -> (Map.insert path (t, obj) m, ()))
                    return obj

{-# NOINLINE sidecarCache #-}
sidecarCache :: IORef (Map.Map FilePath (UTCTime, Aeson.Object))
sidecarCache = unsafePerformIO (newIORef Map.empty)

-- | Read a sidecar object for a given suffix. Returns the empty object
--   when the entry has no resolvable sidecar path or when the file is
--   absent / malformed.
readPhotoSidecar :: String -> Item a -> Compiler Aeson.Object
readPhotoSidecar suffix item = do
    mPath <- photoSidecarPath suffix item
    case mPath of
        Nothing   -> return KM.empty
        Just path -> unsafeCompiler (loadSidecar path)

-- | Coerce a YAML scalar value to a plain String for template
--   interpolation. Integers render without a trailing @.0@; structures
--   and arrays return 'Nothing' (callers needing those should branch
--   on 'Value' directly).
yamlAsString :: Value -> Maybe String
yamlAsString (String t) =
    let s = T.unpack t
    in  if null (trim s) then Nothing else Just (trim s)
yamlAsString (Number n) =
    case Sci.floatingOrInteger n :: Either Double Integer of
        Right i -> Just (show i)
        Left  d -> Just (show d)
yamlAsString _ = Nothing

-- | Look up a key in a sidecar object, coercing scalar values to
--   String. Returns 'Nothing' for missing keys, empty strings, and
--   structural values (arrays / nested objects).
sidecarLookupString :: String -> Aeson.Object -> Maybe String
sidecarLookupString key obj = yamlAsString =<< KM.lookup (AK.fromString key) obj

-- ---------------------------------------------------------------------------
-- Responsive delivery variants (audit P01)
-- ---------------------------------------------------------------------------
--
-- Every photography surface used to point one @<img src>@ at the full
-- 2400px delivery JPEG, whatever size the slot actually was — a 210px
-- contact-sheet frame cost the same ~800 KB as the detail page's hero.
-- @tools\/generate-thumbnails.py@ writes a ladder of siblings beside each
-- source (@\<name\>.w480.jpg@, @.w960@, @.w1440@, emitted only when the
-- source is strictly wider), and these fields offer that ladder to the
-- browser through @srcset@.
--
-- Every candidate is checked on disk before it is offered. Two reasons
-- this is load-bearing rather than defensive: a browser does not fall
-- back from a 404'd @srcset@ candidate the way it falls back from a
-- missing @<source>@, and @tools\/check-site.py@ fails the build on any
-- @srcset@ target absent from @_site@. The ladder is also incomplete by
-- design — a 900px source has no @.w960@ — and @.webp@ companions exist
-- only where @cwebp@ ran.

-- | The widths @tools\/generate-thumbnails.py@ emits (@WIDTHS@ in
--   @tools\/photo_naming.py@, held to this list through @site shared-rules@
--   by @tests\/test_photo_naming.py@); adding a rung is a change on both
--   sides.
photoVariantWidths :: [Int]
photoVariantWidths = [480, 960, 1440]

-- | Split a filename at its final extension, not crossing a path
--   separator: @\"a\/foo.jpg\"@ → @(\"a\/foo\", \".jpg\")@, and
--   @\"foo\"@ → @(\"foo\", \"\")@.
photoSplitExt :: String -> (String, String)
photoSplitExt p = case lastDot (length p - 1) of
    Just i  -> (take i p, drop i p)
    Nothing -> (p, "")
  where
    lastDot i
        | i < 0         = Nothing
        | p !! i == '/' = Nothing   -- crossed a path boundary
        | p !! i == '.' = Just i
        | otherwise     = lastDot (i - 1)

-- | @foo.jpg@ at rung 960 → @foo.w960.jpg@. Same extension, same
--   directory: the naming contract in @tools\/generate-thumbnails.py@.
photoVariantName :: String -> Int -> String
photoVariantName p w =
    let (base, ext) = photoSplitExt p in base ++ ".w" ++ show w ++ ext

-- | @foo.jpg@ → @foo.webp@ (and @foo.w960.jpg@ → @foo.w960.webp@).
photoWebpName :: String -> String
photoWebpName p = let (base, _) = photoSplitExt p in base ++ ".webp"

-- | Disk directory, URL directory, and filename of an entry's
--   co-located photograph. 'Nothing' for a flat single (no co-located
--   asset directory) or an entry with no @photo:@ key — the same test,
--   for the same reason, as 'photoSidecarPath'.
photoAssetPaths :: Item a -> Compiler (Maybe (FilePath, String, String))
photoAssetPaths item = do
    meta <- getMetadata (itemIdentifier item)
    let fp      = toFilePath (itemIdentifier item)
        isFlat  = takeDirectory fp == "content/photography"
        dirName = takeFileName (takeDirectory fp)
    case (isFlat, lookupString "photo" meta) of
        (False, Just photo) | not (null photo) ->
            return $ Just (takeDirectory fp, "/photography/" ++ dirName, photo)
        _ -> return Nothing

-- | True pixel width of the delivery source: frontmatter @width:@,
--   then @{photo}.exif.yaml@, then @{photo}.dims.yaml@. Needed because
--   the source is itself a @srcset@ candidate and a candidate list that
--   uses @w@ descriptors must give one to every entry.
resolvePhotoWidth :: Item a -> Compiler (Maybe Int)
resolvePhotoWidth item = do
    meta <- getMetadata (itemIdentifier item)
    case readMaybe . trim =<< lookupString "width" meta of
        Just w  -> return (Just w)
        Nothing -> do
            exif <- readPhotoSidecar ".exif.yaml" item
            case readMaybe =<< sidecarLookupString "width" exif of
                Just w  -> return (Just w)
                Nothing -> do
                    dims <- readPhotoSidecar ".dims.yaml" item
                    return (readMaybe =<< sidecarLookupString "width" dims)

-- | Keep only the candidates whose file is on disk, preserving order.
photoCandidatesOnDisk :: [(FilePath, String, Int)]
                      -> Compiler [(FilePath, String, Int)]
photoCandidatesOnDisk cands = do
    marked <- unsafeCompiler $
        mapM (\c@(disk, _, _) -> (,) c <$> doesFileExist disk) cands
    return [ c | (c, True) <- marked ]

-- | Render a candidate list as a @srcset@ attribute value.
photoSrcsetString :: [(FilePath, String, Int)] -> String
photoSrcsetString =
    intercalate ", " . map (\(_, url, w) -> url ++ " " ++ show w ++ "w")

-- | Generic frontmatter > EXIF-sidecar fallback field.
--
--   @key@ is the YAML key — same name on both sides. Frontmatter
--   wins when present and non-empty; otherwise the matching key in
--   @{photo}.exif.yaml@. 'noResult' fires when neither supplies a
--   value, so the consuming template's @$if(key)$@ guard suppresses
--   the row.
exifBackedField :: String -> Context String
exifBackedField key = field key $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    case lookupString key meta of
        Just v | not (null (trim v)) -> return (trim v)
        _ -> do
            obj <- readPhotoSidecar ".exif.yaml" item
            case sidecarLookupString key obj of
                Just v  -> return v
                Nothing -> noResult ("no " ++ key ++ " in frontmatter or EXIF sidecar")

-- | Canonical URL for a known license name.
--
--   The frontmatter @license:@ string is normalized — lowercased, with
--   internal whitespace collapsed — before lookup, so any of these all
--   resolve identically:
--
--     * @"CC BY-SA 4.0"@
--     * @"cc by-sa 4.0"@
--     * @"  CC  BY-SA   4.0  "@
--
--   For licenses not in this table (e.g. a custom license, or "All
--   Rights Reserved" which has no URL), the author can supply their
--   own @license-url:@ frontmatter field; the field-level resolver
--   (@licenseUrlField@) prefers explicit @license-url@ and falls back
--   to this lookup only when the author hasn't provided one.
canonicalLicenseUrl :: String -> Maybe String
canonicalLicenseUrl raw =
    case unwords (words (map (\c -> if c == '_' then ' ' else toLowerC c) raw)) of
        "cc by 4.0"        -> Just "https://creativecommons.org/licenses/by/4.0/"
        "cc by-sa 4.0"     -> Just "https://creativecommons.org/licenses/by-sa/4.0/"
        "cc by-nc 4.0"     -> Just "https://creativecommons.org/licenses/by-nc/4.0/"
        "cc by-nc-sa 4.0"  -> Just "https://creativecommons.org/licenses/by-nc-sa/4.0/"
        "cc by-nd 4.0"     -> Just "https://creativecommons.org/licenses/by-nd/4.0/"
        "cc by-nc-nd 4.0"  -> Just "https://creativecommons.org/licenses/by-nc-nd/4.0/"
        "cc0"              -> Just "https://creativecommons.org/publicdomain/zero/1.0/"
        "cc0 1.0"          -> Just "https://creativecommons.org/publicdomain/zero/1.0/"
        "public domain"    -> Just "https://creativecommons.org/publicdomain/mark/1.0/"
        _                  -> Nothing
  where
    toLowerC c
        | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
        | otherwise            = c

resolveLicenseUrl :: Maybe String -> String -> Maybe String
resolveLicenseUrl (Just u) _ | not (null (trim u)) = Just (trim u)
resolveLicenseUrl _ name = canonicalLicenseUrl name

-- | Context for photography pages and photo cards.
--
--   Phase 1: frontmatter-only. Auto-extracted EXIF + palette sidecars
--   land in Phase 3, where this context will gain a merge step that
--   reads @{photo}.exif.yaml@ / @{photo}.palette.yaml@ and exposes
--   their fields under the same template variables. Frontmatter wins.
--
--   Photography pages do not include the essay context's epistemic,
--   bibliography, backlinks, similar-links, TOC, word-count, or
--   reading-time fields — none of those apply to visual content. See
--   @PHOTOGRAPHY.md@ for the design rationale.
--
--   Exposed template variables:
--     @$photography$@   — flag, gates @photography.css@ in head.html
--                         and the @data-page-type@ body attribute used
--                         by the Phase 2 darkroom-mode lightbox
--     @$slug$@          — URL slug under @/photography/@
--     @$photo-url$@     — absolute URL of the photo file. Built as
--                         @/photography/<dir>/<photo>@ for any entry with
--                         a co-located image — directory-form singles and
--                         series children alike; @noResult@ for flat
--                         singles (templates use the @photo@
--                         frontmatter directly there).
--     @$captured-display$@, @$captured-iso$@ — capture date in
--                         human-readable and ISO forms; @noResult@
--                         when @captured:@ is absent. Distinct from
--                         the publication @date:@ shown in card lists.
--     @$photo-srcset$@  — the responsive ladder for @$photo-url$@:
--                         every existing @\<name\>.w480\/.w960\/.w1440@
--                         sibling in ascending width, then the source
--                         itself at its true pixel width. @noResult@
--                         when no variant exists or the source width is
--                         unknown, so the template falls back to a plain
--                         @src@.
--     @$photo-webp-srcset$@ — the same ladder in @.webp@, each rung
--                         independently existence-checked. @noResult@
--                         when none exist, which is what gates the
--                         @\<source type="image\/webp"\>@.
--     @$photography-tags$@ — listField of @{tag-name, tag-url}@.
--     @$palette-swatches$@ — listField of @{swatch}@ (hex string).
--                         @noResult@ when the @palette:@ frontmatter
--                         is absent or empty so the template's
--                         @$if(palette-swatches)$@ gate suppresses an
--                         empty strip.
photographyCtx :: Context String
photographyCtx =
    constField "photography" "true"
    <> slugField
    <> photoUrlField
    <> photoWebpUrlField
    <> photoSrcsetField
    <> photoWebpSrcsetField
    -- EXIF-backed fields. Each prefers frontmatter and falls back to
    -- @{photo}.exif.yaml@ produced by @tools/extract-exif.py@. Sidecars
    -- absent on film scans (no EXIF on a film negative) is fine —
    -- noResult propagates and the template's @$if(...)$@ gate hides
    -- the row.
    <> exifBackedField "camera"
    <> exifBackedField "lens"
    <> exifBackedField "exposure"
    <> exifBackedField "shutter"
    <> exifBackedField "aperture"
    <> exifBackedField "iso"
    <> exifBackedField "focal-length"
    -- Pixel dimensions for CLS-prevention width/height attrs on every
    -- <img>. Read from the EXIF sidecar produced by extract-exif.py;
    -- frontmatter wins if the author wants to override (e.g., to
    -- declare a different rendered size).
    <> exifBackedField "width"
    <> exifBackedField "height"
    <> capturedDisplayField
    <> capturedIsoField
    <> paletteSwatchesField
    <> licenseUrlField
    <> photoLinksField
    -- Backlinks. A photograph is a link *target* like any other page: a
    -- poem or essay that references the frame produces an entry keyed to
    -- this page's URL, and the join is symmetric because both sides go
    -- through 'FooterData.normaliseUrl'. Photography stays out of
    -- 'Patterns.allContent' — that governs link *sources*, and
    -- caption-scale entries have no outbound prose to contribute.
    <> footerDepsField
    <> backlinksField
    <> tagLinksField "photography-tags"
    <> authorLinksField
    <> affiliationField
    <> dateField "date"     writerlyDate
    <> dateField "date-iso" isoDate
    <> revisionDateFields
    <> siteCtx
  where
    slugField :: Context String
    slugField = field "slug" (return . photoSlug)

    -- Build @/photography/<slug>/<photo>@ when both the directory-form
    -- entry and a @photo:@ frontmatter key are present. Flat singles
    -- have no co-located asset directory, so @noResult@ there — the
    -- template falls back to interpreting the @photo:@ frontmatter
    -- as a literal URL.
    -- The test is "does this entry have a co-located asset directory", which
    -- is true of BOTH directory-form singles (<slug>/index.md) and series
    -- children (<series>/<photo>.md) — the latter keep their image beside
    -- their markdown, in the series directory. Gating on index.md alone sent
    -- series children down the flat-single fallback, where the template used
    -- the bare `photo:` filename as a URL; since a child page is served from
    -- <series>/<photo>/, that resolved one directory too deep and 404'd.
    --
    -- photoSlug is deliberately not reused here: for a non-index.md file it
    -- returns the file's own basename, whereas the image lives under the
    -- containing directory's name.
    photoUrlField :: Context String
    photoUrlField = field "photo-url" $ \item -> do
        mPaths <- photoAssetPaths item
        case mPaths of
            Just (_, urlDir, photo) -> return (urlDir ++ "/" ++ photo)
            Nothing -> noResult "no co-located photo (flat single, or photo: key absent)"

    -- WebP companion URL, mirroring 'photoUrlField'. Returns 'noResult'
    -- when the @.webp@ companion doesn't exist on disk at compile time
    -- (cwebp not installed, conversion not yet run, or this image
    -- failed to convert) so the template's @$if(photo-webp-url)$@
    -- guard suppresses the @<source>@ — the @<picture>@ then degrades
    -- to a plain @<img>@ on the original-format src. Browsers do NOT
    -- fall back from a 404'd @<source>@ to the nested @<img>@; the
    -- file-existence check at build time is load-bearing.
    photoWebpUrlField :: Context String
    photoWebpUrlField = field "photo-webp-url" $ \item -> do
        mPaths <- photoAssetPaths item
        case mPaths of
            Nothing -> noResult "no co-located photo (flat single, or photo: key absent)"
            Just (dir, urlDir, photo) -> do
                let webp = photoWebpName photo
                exists <- unsafeCompiler (doesFileExist (dir </> webp))
                if exists
                    then return (urlDir ++ "/" ++ webp)
                    else noResult "no webp companion on disk"

    -- Responsive ladder for the original delivery format (audit P01).
    -- Ascending width, source last at its true pixel width, so a browser
    -- reading right-to-left finds the smallest candidate that satisfies
    -- the slot. 'noResult' when there is no rung below the source (a
    -- small source needs no ladder) or when the source width is unknown
    -- — a `w` descriptor list may not have an entry without one, and
    -- guessing the source's width would mislead the selection algorithm
    -- in exactly the direction that costs bytes.
    photoSrcsetField :: Context String
    photoSrcsetField = field "photo-srcset" $ \item -> do
        mPaths <- photoAssetPaths item
        mWidth <- resolvePhotoWidth item
        case (mPaths, mWidth) of
            (Just (dir, urlDir, photo), Just srcW) -> do
                let rungs = [ ( dir </> photoVariantName photo w
                              , urlDir ++ "/" ++ photoVariantName photo w
                              , w )
                            | w <- photoVariantWidths, w < srcW ]
                kept <- photoCandidatesOnDisk rungs
                if null kept
                    then noResult "no delivery variants on disk"
                    else return $ photoSrcsetString $
                            kept ++ [ ( dir </> photo
                                      , urlDir ++ "/" ++ photo
                                      , srcW ) ]
            (Just _, Nothing) -> noResult "no source width; cannot write w descriptors"
            _                 -> noResult "no co-located photo"

    -- The same ladder in WebP. Each rung is checked independently: a
    -- half-finished `tools/convert-images.sh` run leaves some companions
    -- and not others, and offering one that is not there would 404 with
    -- no fallback (a <source> that resolves is committed to). 'noResult'
    -- when nothing exists, which is what suppresses the <source>.
    --
    -- With the source width unknown the single base companion is still
    -- offered, bare and descriptorless — that is exactly the pre-P01
    -- behaviour, so nothing regresses on an entry with no dimensions.
    photoWebpSrcsetField :: Context String
    photoWebpSrcsetField = field "photo-webp-srcset" $ \item -> do
        mPaths <- photoAssetPaths item
        mWidth <- resolvePhotoWidth item
        case mPaths of
            Nothing -> noResult "no co-located photo"
            Just (dir, urlDir, photo) -> do
                let webpOf p = photoWebpName p
                    baseCand = ( dir </> webpOf photo
                               , urlDir ++ "/" ++ webpOf photo
                               , fromMaybe 0 mWidth )
                case mWidth of
                    Nothing -> do
                        kept <- photoCandidatesOnDisk [baseCand]
                        case kept of
                            [(_, url, _)] -> return url
                            _             -> noResult "no webp companion on disk"
                    Just srcW -> do
                        let cands = [ ( dir </> webpOf (photoVariantName photo w)
                                      , urlDir ++ "/" ++ webpOf (photoVariantName photo w)
                                      , w )
                                    | w <- photoVariantWidths, w < srcW ]
                                    ++ [ (dir </> webpOf photo, urlDir ++ "/" ++ webpOf photo, srcW) ]
                        kept <- photoCandidatesOnDisk cands
                        if null kept
                            then noResult "no webp companions on disk"
                            else return (photoSrcsetString kept)

    -- Resolve the @captured:@ ISO date with frontmatter > sidecar
    -- precedence. Centralised so the display and ISO fields stay in
    -- agreement on which source they read from.
    resolveCapturedIso :: Item a -> Compiler (Maybe String)
    resolveCapturedIso item = do
        meta <- getMetadata (itemIdentifier item)
        case lookupString "captured" meta of
            Just v | not (null (trim v)) -> return (Just (trim v))
            _ -> do
                obj <- readPhotoSidecar ".exif.yaml" item
                return (sidecarLookupString "captured" obj)

    -- @captured:@ as "15 March 2026". Reads frontmatter, falls back to
    -- the EXIF sidecar's @captured:@ key. Returns @noResult@ when
    -- absent so @$if(captured-display)$@ gates the metadata row.
    capturedDisplayField :: Context String
    capturedDisplayField = field "captured-display" $ \item -> do
        mIso <- resolveCapturedIso item
        case mIso of
            Nothing -> noResult "no captured date in frontmatter or EXIF sidecar"
            Just iso ->
                case parseIsoDate iso
                       :: Maybe UTCTime of
                    Just t  -> return (formatWriterly t)
                    Nothing -> noResult "captured date does not parse as YYYY-MM-DD"

    -- ISO form passed through unchanged (after a parse-validate round-trip
    -- so a malformed value in either source doesn't reach the template).
    capturedIsoField :: Context String
    capturedIsoField = field "captured-iso" $ \item -> do
        mIso <- resolveCapturedIso item
        case mIso of
            Nothing -> noResult "no captured date in frontmatter or EXIF sidecar"
            Just iso ->
                case parseIsoDate iso
                       :: Maybe UTCTime of
                    Just t  -> return (formatIso t)
                    Nothing -> noResult "captured date does not parse as YYYY-MM-DD"

    -- @palette:@ list field. Frontmatter wins; otherwise pull the
    -- list from @{photo}.palette.yaml@ (the @palette:@ key, an array
    -- of hex strings produced by @tools/extract-palette.py@). Each
    -- swatch exposes @$swatch$@.
    paletteSwatchesField :: Context String
    paletteSwatchesField = listFieldWith "palette-swatches" swCtx $ \item -> do
        meta <- getMetadata (itemIdentifier item)
        let fmEntries = fromMaybe [] (lookupStringList "palette" meta)
            fmVisible = filter (not . null . trim) fmEntries
        swatches <- if null fmVisible
            then do
                obj <- readPhotoSidecar ".palette.yaml" item
                case KM.lookup "palette" obj of
                    Just (Array vec) ->
                        return [ trim s
                               | val <- V.toList vec
                               , Just s <- [yamlAsString val]
                               , not (null (trim s)) ]
                    _ -> return []
            else return fmVisible
        if null swatches
            then noResult "no palette swatches in frontmatter or palette sidecar"
            else return $ zipWith
                (\i s -> Item (fromFilePath ("palette-" ++ show i)) s)
                ([0 ..] :: [Int])
                swatches
      where
        swCtx = field "swatch" (return . itemBody)

    -- @$license-url-resolved$@: an explicit @license-url:@ frontmatter
    -- value when present, otherwise a canonical URL looked up from the
    -- @license:@ string for known licenses (CC variants, CC0, public
    -- domain). Returns @noResult@ when neither is set, so
    -- @$if(license-url-resolved)$@ gates the link wrapper.
    --
    -- Frontmatter @license:@ itself flows through @defaultContext@ as
    -- @$license$@; the template renders the license name as link text
    -- and uses @$license-url-resolved$@ as @href@.
    licenseUrlField :: Context String
    licenseUrlField = field "license-url-resolved" $ \item -> do
        meta <- getMetadata (itemIdentifier item)
        case resolveLicenseUrl (lookupString "license-url" meta)
                (fromMaybe "" (lookupString "license" meta)) of
            Just u -> return u
            Nothing -> noResult "no license URL"

    -- @links:@ frontmatter — outbound links to other surfaces where
    -- the photograph appears or can be acquired (Wikimedia Commons,
    -- Flickr, exhibition catalog, print-sale page, etc.). Each entry
    -- uses the same @"Name | URL"@ pipe syntax as @authors:@ /
    -- @affiliation:@ — the existing site convention.
    --
    -- Each item exposes @$link-name$@ and @$link-url$@. Entries
    -- without a URL are dropped (no point linking to nothing). Returns
    -- @noResult@ on empty so @$if(photo-links)$@ guards the wrapper.
    photoLinksField :: Context String
    photoLinksField = listFieldWith "photo-links" lkCtx $ \item -> do
        meta <- getMetadata (itemIdentifier item)
        let entries = fromMaybe [] (lookupStringList "links" meta)
            parsed  = filter (not . null . snd) (map parseEntry entries)
        if null parsed
            then noResult "no outbound links"
            else return $ map (Item (fromFilePath "")) parsed
      where
        lkCtx = field "link-name" (return . fst . itemBody)
             <> field "link-url"  (return . snd . itemBody)
        parseEntry s = case break (== '|') s of
            (name, '|' : url) -> (trim name, trim url)
            (name, _)         -> (trim name, "")
