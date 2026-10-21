{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Shared utilities used across the build system.
--
-- The HTML escapers (one for 'String', one for 'Text') live here so that
-- every filter, context, and renderer goes through the same definition.
-- The expansion order matters: @&@ MUST be replaced first, otherwise the
-- @&amp;@ injected by other rules gets re-escaped to @&amp;amp;@. The
-- pure-character-by-character implementation used here avoids that hazard
-- entirely (each character is mapped exactly once).
module Utils
    ( wordCount
    , readingTime
    , escapeHtml
    , escapeHtmlText
    , inlinesText
    , stripHtmlComments
    , trim
    , splitOn
    , authorSlugify
    , authorNameOf
    , itemAuthors
    , authorUrl
    , metadataKeywords
    , exposureISO
    , isoDate
    , writerlyDate
    , parseIsoDate
    , formatIso
    , formatWriterly
    , isoToWriterly
    , isProvedConfidence
    , confidencePercent
    , trustScore
    , formatBytes
    , median
    , isSafeUrl
    , stripPrefixRoute
    , contentPageRoute
    , loadYaml
    , pdfViewerUrl
    , encodeQueryValue
    , isPdfUrl
    , inDefault
    , boolSpellings
    , parseBool
    , canonicalUrlPath
    , isDevBuild
    , outputDirFor
    , cacheDirFor
    ) where

import           Data.Char (isAlphaNum, isSpace, toLower)
import           Data.List (dropWhileEnd, isPrefixOf, isSuffixOf, sort, stripPrefix)
import           Data.Maybe (fromMaybe)
import           Data.Time.Calendar (Day)
import qualified Data.Text as T
import           Data.Time.Format (FormatTime, ParseTime, defaultTimeLocale, formatTime,
                                   parseTimeM)
import           Data.Aeson (FromJSON)
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Y
import           Hakyll (Compiler, Context, Identifier, Item, Metadata, Routes, composeRoutes,
                         customRoute, fromFilePath, itemBody, load, loadAndApplyTemplate,
                         lookupString, lookupStringList, relativizeUrls, setExtension,
                         stripTags, toFilePath)
import           System.Environment (lookupEnv)
import           Text.Pandoc.Definition (Format (..), Inline (..), QuoteType (..))
import           Text.Printf (printf)
import           Text.Read (readMaybe)

-- | Whether this run is a dev build: @SITE_ENV=dev@, set by @make dev@ and
--   @make watch@ (and forced off by @make build@).
isDevBuild :: IO Bool
isDevBuild = (== Just "dev") <$> lookupEnv "SITE_ENV"

-- | Where a build writes, and where Hakyll keeps its store. A dev build has
--   its own pair, so a draft or a dev-only route (a draft collection is
--   routed outside @/drafts/@) can never be left in the tree that
--   @make deploy@ signs and publishes, and a dev session's clean never
--   throws away the production build (audit D06).
outputDirFor, cacheDirFor :: Bool -> FilePath
outputDirFor dev = if dev then "_site-dev" else "_site"
cacheDirFor dev = if dev then "_cache-dev" else "_cache"

-- | Public URL for a Hakyll route, in the directory form the site's own
--   navigation, sitemap, and generated semantic metadata use:
--   @essays\/foo\/index.html@ is the route, @\/essays\/foo\/@ the URL.
--   Deliberate @.html@ routes (@\/about.html@, @\/new.html@) keep their
--   extension — this normalizes one spelling of the same page, it does not
--   restyle the site's URL scheme.
--
--   Lives here rather than in "Contexts" so that "Backlinks" — which
--   "Contexts" imports, and which writes the href of every backlink
--   source — can reach it without an import cycle.
canonicalUrlPath :: FilePath -> String
canonicalUrlPath r
    | "index.html" `isSuffixOf` withSlash =
        take (length withSlash - length ("index.html" :: String)) withSlash
    | otherwise = withSlash
  where
    withSlash = case r of
        ('/' : _) -> r
        _         -> '/' : r

-- | Count the number of words in a string (split on whitespace).
wordCount :: String -> Int
wordCount = length . words

-- | Estimate reading time in minutes (assumes 200 words per minute).
-- Rounds up — 399 words is 2 minutes, not 1. Minimum is 1 minute.
readingTime :: String -> Int
readingTime s = max 1 ((wordCount s + 199) `div` 200)

-- | Escape HTML special characters: @&@, @<@, @>@, @\"@, @\'@.
--
-- Safe for use in attribute values and text content. The order of the
-- @case@ branches is irrelevant — each input character maps to exactly
-- one output sequence.
escapeHtml :: String -> String
escapeHtml = concatMap escChar
  where
    escChar '&'  = "&amp;"
    escChar '<'  = "&lt;"
    escChar '>'  = "&gt;"
    escChar '"'  = "&quot;"
    escChar '\'' = "&#39;"
    escChar c    = [c]

-- | 'Text' counterpart of 'escapeHtml'.
escapeHtmlText :: T.Text -> T.Text
escapeHtmlText = T.concatMap escChar
  where
    escChar '&'  = "&amp;"
    escChar '<'  = "&lt;"
    escChar '>'  = "&gt;"
    escChar '"'  = "&quot;"
    escChar '\'' = "&#39;"
    escChar c    = T.singleton c

-- | Inlines as plain text, as a reader sees them: a heading's label in the
--   TOC, an image's alt text. Quotation marks stay (“consumed” is not
--   consumed), math keeps its source, and raw HTML keeps its text but not
--   its tags: Typography wraps "e.g." in an <abbr>, which used to reach the
--   TOC escaped and visible (audit H03) and dropped out of alt text.
inlinesText :: [Inline] -> T.Text
inlinesText = T.concat . map go
  where
    go (Str t)                     = t
    go Space                       = " "
    go SoftBreak                   = " "
    go LineBreak                   = " "
    go (Emph ils)                  = inlinesText ils
    go (Strong ils)                = inlinesText ils
    go (Underline ils)             = inlinesText ils
    go (Strikeout ils)             = inlinesText ils
    go (Superscript ils)           = inlinesText ils
    go (Subscript ils)             = inlinesText ils
    go (SmallCaps ils)             = inlinesText ils
    go (Quoted DoubleQuote ils)    = "\8220" <> inlinesText ils <> "\8221"
    go (Quoted SingleQuote ils)    = "\8216" <> inlinesText ils <> "\8217"
    go (Cite _ ils)                = inlinesText ils
    go (Code _ t)                  = t
    go (Math _ t)                  = t
    go (RawInline (Format "html") t) = T.pack (stripTags (T.unpack t))
    go (RawInline _ _)             = ""
    go (Link _ ils _)              = inlinesText ils
    go (Image _ ils _)             = inlinesText ils
    go (Note _)                    = ""
    go (Span _ ils)                = inlinesText ils

-- | Drop the HTML comments from a template's source. A comment that has
--   its lines to itself goes with them, so no blank, indented line is left
--   where it stood; one beside markup goes alone. An unterminated @<!--@ is
--   left as it is.
stripHtmlComments :: String -> String
stripHtmlComments = T.unpack . go . T.pack
  where
    go s = case T.breakOn "<!--" s of
        (before, rest)
            | T.null rest -> before
            | otherwise -> case T.breakOn "-->" rest of
                (_, close)
                    | T.null close -> s
                    | otherwise ->
                        let after         = T.drop 3 close
                            before'       = T.dropWhileEnd blank before
                            (_, after')   = T.span blank after
                            ownsLine      = (T.null before' || "\n" `T.isSuffixOf` before')
                                         && (T.null after' || "\n" `T.isPrefixOf` after')
                        in  if ownsLine then before' <> go (T.drop 1 after')
                                        else before <> go after
    blank c = c == ' ' || c == '\t'

-- | Strip leading and trailing whitespace.
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Lowercase a string, drop everything that isn't alphanumeric or
-- space, then replace each space with a hyphen. Note that a run of
-- spaces therefore becomes a run of hyphens (@"A  B" → "a--b"@) —
-- deliberately left as-is, since every slug on the site is generated
-- by this one function and collapsing runs now would move existing
-- author URLs.
--
-- Used for author URL slugs (e.g. @"Example Author" → "example-author"@).
-- Centralised here so 'Authors' and 'Contexts' cannot drift on Unicode
-- edge cases.
authorSlugify :: String -> String
authorSlugify = map (\c -> if c == ' ' then '-' else c)
              . filter (\c -> isAlphaNum c || c == ' ')
              . map toLower

-- | Extract the author name from a "Name | url" frontmatter entry.
-- The URL portion is dropped (it's no longer used by the author system,
-- which routes everything through @/authors/{slug}/@).
authorNameOf :: String -> String
authorNameOf s = trim (takeWhile (/= '|') s)

-- | Split on every occurrence of a separator: @splitOn ',' "a,,b"@ is
-- @["a", "", "b"]@.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c xs = case break (== c) xs of
    (before, [])       -> [before]
    (before, _ : rest) -> before : splitOn c rest

-- | The authors an item credits: the name of each @authors:@ entry (the
-- part before any @| url@), dropping entries with no usable name or slug,
-- and the site's author when none remain. The byline and the author index
-- pages both read this, so a malformed entry can no longer build an
-- @/authors//@ page that no byline links to.
itemAuthors :: String -> Metadata -> [String]
itemAuthors defaultAuthor meta =
    let entries = fromMaybe [] (lookupStringList "authors" meta)
        usable  = filter (\n -> not (null n) && not (null (authorSlugify n)))
                         (map authorNameOf entries)
    in  if null usable then [defaultAuthor] else usable

-- | An author's index page.
authorUrl :: String -> String
authorUrl name = "/authors/" ++ authorSlugify name ++ "/"

-- | A page's @keywords:@, given as a YAML list or a comma-separated
-- string, each trimmed, empties dropped. The keyword links on a page and
-- the keyword pages that exist are both built from this, so a padded
-- keyword can no longer link to a page that was never generated.
metadataKeywords :: Metadata -> [String]
metadataKeywords meta = filter (not . null) . map trim $
    case lookupStringList "keywords" meta of
        Just xs -> xs
        Nothing -> maybe [] (splitOn ',') (lookupString "keywords" meta)

-- | The sensitivity in an @exposure:@ string, which holds shutter,
-- aperture and ISO as read off a camera: @"1/200 f/7.1 ISO 100"@ gives 100.
exposureISO :: String -> Maybe Int
exposureISO s = case dropWhile (/= "ISO") (words s) of
    (_ : v : _) -> readMaybe v
    _           -> Nothing

-- | The spellings a front-matter boolean may take, as YAML 1.1 readers
-- (PyYAML, the tests) take them, compared without case or whitespace.
-- @figure-numbering@ and the draft flag ("Drafts") are both read through
-- 'parseBool'; they disagreed about @on@ until 2026-10-04. @site
-- shared-rules@ gives the tests this list.
boolSpellings :: [(String, Bool)]
boolSpellings =
    [ ("true", True), ("yes", True), ("on", True), ("1", True)
    , ("false", False), ("no", False), ("off", False), ("0", False) ]

-- | A front-matter boolean that arrives as a string; any other spelling
-- is unset.
parseBool :: String -> Maybe Bool
parseBool v = lookup (map toLower (filter (not . isSpace) v)) boolSpellings

-- | The site's two date spellings: @16 March 2026@ for readers, ISO 8601
-- for machines and front matter. Pages, feeds and sidecars all format and
-- parse dates through these rather than repeating the format strings.
isoDate, writerlyDate :: String
isoDate      = "%Y-%m-%d"
writerlyDate = "%-d %B %Y"

-- | A @YYYY-MM-DD@ date, surrounding whitespace allowed, as a 'Day' or a
-- midnight 'UTCTime'.
parseIsoDate :: ParseTime t => String -> Maybe t
parseIsoDate = parseTimeM True defaultTimeLocale isoDate

formatIso, formatWriterly :: FormatTime t => t -> String
formatIso      = formatTime defaultTimeLocale isoDate
formatWriterly = formatTime defaultTimeLocale writerlyDate

-- | @"2026-04-26"@ → @"26 April 2026"@. An unparseable value comes back
-- as it was, so a typo surfaces on the page instead of failing the build.
isoToWriterly :: String -> String
isoToWriterly iso = maybe iso (formatWriterly :: Day -> String) (parseIsoDate iso)

-- | @confidence: proved@ (or @proven@), case-insensitive: the §4.3
-- carve-out for formal proofs that opt out of a numeric credence. The
-- figure, the footer and the search index all treat it as 100.
isProvedConfidence :: Maybe String -> Bool
isProvedConfidence (Just s) = map toLower (trim s) `elem` ["proved", "proven"]
isProvedConfidence _        = False

-- | A page's @confidence:@ as a percentage, @proved@ counting as 100.
confidencePercent :: Maybe String -> Maybe Int
confidencePercent raw
    | isProvedConfidence raw = Just 100
    | otherwise              = readMaybe =<< raw

-- | The trust score: a 60/40 weighted composite of confidence (0–100) and
-- evidence (1–5), clamped to 0–100. 'Nothing' when either is missing, so
-- nothing renders rather than a zero indistinguishable from an authored
-- one. The epistemic figure, @$overall-score$@ and the search index's
-- @score@ all read this.
trustScore :: Maybe Int -> Maybe Int -> Maybe Int
trustScore (Just c) (Just e) =
    let raw :: Double
        raw = fromIntegral c / 100.0 * 0.6 + fromIntegral (e - 1) / 4.0 * 0.4
    in  Just (max 0 (min 100 (round (raw * 100.0))))
trustScore _ _ = Nothing

-- | A byte count for readers, rounded: @512 B@, @3 KB@, @12.4 MB@,
-- @1.3 GB@. Kilobytes are whole; a tenth of one says nothing.
formatBytes :: Integer -> String
formatBytes n
    | n >= gb   = printf "%.1f GB" (fromIntegral n / fromIntegral gb :: Double)
    | n >= mb   = printf "%.1f MB" (fromIntegral n / fromIntegral mb :: Double)
    | n >= kb   = printf "%.0f KB" (fromIntegral n / fromIntegral kb :: Double)
    | otherwise = show n ++ " B"
  where
    kb = 1024 :: Integer
    mb = kb * 1024
    gb = mb * 1024

-- | The median; 0 for an empty list. An even-length list takes the mean
-- of the two middle elements, rounded half up.
median :: [Int] -> Int
median [] = 0
median xs
    | odd n     = upper
    | otherwise = (lower + upper + 1) `div` 2
  where
    -- In range for a non-empty list: lower is forced only when n >= 2.
    sorted = sort xs
    n      = length sorted
    upper  = sorted !! (n `div` 2)
    lower  = sorted !! (n `div` 2 - 1)

-- | Whether a URL from data or front matter may go into an @href@ or
-- @src@ the generator writes by hand: site-relative, @https:@, @mailto:@
-- or a fragment, never protocol-relative.
isSafeUrl :: String -> Bool
isSafeUrl u =
    let norm = map toLower (dropWhile isSpace u)
    in  not ("//" `isPrefixOf` norm)
        && any (`isPrefixOf` norm) ["/", "https://", "mailto:", "#"]

-- | Route that strips a literal prefix from the identifier's path.
-- Hakyll's @gsubRoute@ replaces /every/ occurrence of its pattern, so
-- @gsubRoute "content/"@ would also mangle a co-located directory that
-- happened to be named @content@ deeper in the path
-- (@content/essays/slug/content/data.csv@ → @essays/slug/data.csv@).
-- This touches only the leading occurrence; identifiers that don't start
-- with the prefix pass through unchanged.
stripPrefixRoute :: String -> Routes
stripPrefixRoute prefix = customRoute $ \ident ->
    let fp = toFilePath ident
    in  fromMaybe fp (stripPrefix prefix fp)

-- | A YAML data file the rules have matched, decoded. Hakyll hands back a
-- 'String' of Unicode code points and the yaml library wants UTF-8 bytes:
-- 'Data.ByteString.Char8.pack' would truncate every 'Char' to 8 bits and
-- silently mangle an em dash (0x2014 → control character 0x14).
loadYaml :: FromJSON a => FilePath -> Compiler a
loadYaml path = do
    raw <- load (fromFilePath path) :: Compiler (Item String)
    case Y.decodeEither' (TE.encodeUtf8 (T.pack (itemBody raw))) of
        Left  err -> fail (path ++ ": " ++ show err)
        Right doc -> return doc

-- | The vendored PDF.js viewer opening a site PDF. Takes the path without
-- its fragment: a @#page=N@ goes after the result, where PDF.js reads it
-- from the location hash. Prose links ("Filters.Links"), @{{pdf:…}}@ embeds
-- ("Filters.EmbedPdf") and the related-pages list ("SimilarLinks") all
-- build the viewer URL here; they had three encoders between them.
pdfViewerUrl :: String -> String
pdfViewerUrl path = "/pdfjs/web/viewer.html?file=" ++ encodeQueryValue path

-- | Percent-encode the characters that would break a query-string value.
-- Slashes are left alone, so a root-relative path stays readable and
-- PDF.js's own fetch resolves it. Percent signs must be encoded too:
-- decoding the viewer's @file=@ parameter must preserve the original path.
encodeQueryValue :: String -> String
encodeQueryValue = concatMap enc
  where
    enc '%' = "%25"
    enc ' ' = "%20"
    enc '&' = "%26"
    enc '?' = "%3F"
    enc '+' = "%2B"
    enc '"' = "%22"
    enc '#' = "%23"
    enc c   = [c]

-- | Whether a URL names a PDF: @.pdf@, in any case, before any fragment.
isPdfUrl :: String -> Bool
isPdfUrl u = ".pdf" `isSuffixOf` map toLower (takeWhile (/= '#') u)

-- | A Markdown page's route: its path below @content/@, as @.html@.
contentPageRoute :: Routes
contentPageRoute = stripPrefixRoute "content/" `composeRoutes` setExtension "html"

-- | The tail every page shares: its own template, then the site frame
-- (@templates/default.html@), with the same context, and URLs made
-- relative.
inDefault :: Identifier -> Context String -> Item String -> Compiler (Item String)
inDefault tpl ctx item =
    loadAndApplyTemplate tpl ctx item
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
