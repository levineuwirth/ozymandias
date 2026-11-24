{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Citation metadata for scholarly papers, in the Highwire Press
--   @citation_*@ tags Google Scholar reads
--   (https://scholar.google.com/intl/en/scholar/inclusion.html).
--
--   Scholar asks that only scholarly papers carry them, so a page opts in:
--
--   > scholarly: true
--   > citation-title: …          # default: title (markup stripped)
--   > citation-authors: [ … ]    # default: authors (the names, in order)
--   > citation-date: 2026-11-13  # default: date; YYYY-MM-DD, YYYY-MM or YYYY
--   > citation-pdf: /papers/x.pdf  # default: preprint
--   > citation-doi: "10.1234/abcd"  # optional, the bare DOI
--   > citation-arxiv: "2611.01230"  # optional, quoted; the page links its abstract
--   > citation-institution: …    # optional; one per author, as a list in
--   >                            # author order, or a string for one author;
--   >                            # default for one author: affiliation
--
--   'scholarlyPapers' reads every flagged page as the rules are made and
--   refuses the build when one lacks a field, names no PDF that the site
--   publishes, or asks for something Scholar cannot take: a flagged page
--   always has its tags, or the site does not build.
--
--   Scholar takes a @citation_pdf_url@ only in the same directory as the
--   page that names it. A PDF published elsewhere (a research export's
--   @/papers/@) is published again beside its page ('placePdfs'), and the
--   tags and the page's Preprint link name that copy.
module Scholar
    ( Paper (..)
    , Placement (..)
    , scholarlyPapers
    , scholarPdfRules
    , scholarFields
    , readPaper
    , placePdfs
    , citationDate
    , citationTitle
    , encodeUrlPath
    ) where

import           Control.Monad        (filterM, forM, forM_, unless, void, when)
import           Control.Monad.Except (catchError)
import qualified Data.Aeson           as A
import qualified Data.Aeson.Key       as Key
import qualified Data.Aeson.KeyMap    as KM
import qualified Data.ByteString      as BS
import           Data.Char            (chr, isAlphaNum, isAsciiLower, isAsciiUpper, isDigit,
                                       isSpace, ord, toLower)
import           Data.List            (intercalate, isPrefixOf, nub, sortOn, stripPrefix)
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import           Data.Maybe           (catMaybes, fromMaybe)
import           Data.Bits            (xor)
import           Data.Scientific      (floatingOrInteger)
import           Data.Word            (Word64)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Data.Time            (Day, defaultTimeLocale, parseTimeM)
import qualified Data.Vector          as V
import           Numeric              (showHex)
import           System.Directory     (doesFileExist, getFileSize)
import           System.FilePath      (normalise, replaceExtension, splitDirectories,
                                       takeBaseName, takeDirectory, takeExtension, takeFileName,
                                       (</>))
import           Hakyll               hiding (escapeHtml, renderTags, trim)
import           Text.Pandoc.XML      (fromEntities)
import           Config               (SiteConfig (..), Section (..), loadConfig)
import           Drafts               (currentUnpublished, isUnpublished)
import           Sections             (contentAddress)
import           Utils                (authorNameOf, escapeHtml, metadataFlag, trim)

-- | A flagged page's citation, as its front matter gives it.
data Paper = Paper
    { paperTitle      :: String                -- ^ plain text
    , paperAuthors    :: [(String, [String])]  -- ^ in order, each with its institutions
    , paperDate       :: String                -- ^ as written: YYYY-MM-DD, YYYY-MM or YYYY
    , paperPdf        :: String                -- ^ as written
    , paperPdfSources :: [FilePath]            -- ^ the files it may name, first first
    , paperDoi        :: Maybe String
    , paperArxiv      :: Maybe String
    } deriving (Eq, Show)

-- | Where a paper's PDF is published beside its page.
data Placement = Placement
    { placedSource :: FilePath        -- ^ the file (@static/papers/x.pdf@)
    , placedPath   :: FilePath        -- ^ where it is served, beside the page (@essays/x.pdf@)
    , placedCopy   :: Bool            -- ^ published there by 'scholarPdfRules', not by its own rule
    } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Reading the front matter
-- ---------------------------------------------------------------------------

flagged :: Metadata -> Bool
flagged = metadataFlag "scholarly"

-- | A string field, trimmed; empty is absent. An integer is taken as
--   written (an unquoted year); other numbers are 'numeric' problems.
str :: String -> Metadata -> Maybe String
str k meta = case KM.lookup (Key.fromString k) meta of
    Just (A.String t) | s <- trim (T.unpack t), not (null s) -> Just s
    Just (A.Number n) | Right (i :: Integer) <- (floatingOrInteger n :: Either Double Integer)
                      -> Just (show i)
    _                 -> Nothing

-- | A field given as a list of strings, or as one string.
strs :: String -> Metadata -> Maybe [String]
strs k meta = case KM.lookup (Key.fromString k) meta of
    Just (A.Array v)  -> Just [ trim (T.unpack t) | A.String t <- V.toList v ]
    Just (A.String t) -> Just [trim (T.unpack t)]
    _                 -> Nothing

-- | A field YAML read as a number, where the value is an identifier: an
--   unquoted @2611.01230@ arrives as 2611.0123, its last digit gone.
numeric :: String -> Metadata -> Bool
numeric k meta = case KM.lookup (Key.fromString k) meta of
    Just (A.Number _) -> True
    _                 -> False

-- | The page's citation, or what is wrong with it. Pure: the PDF's path is
--   resolved to the files it may name ('pdfSources'), not looked for;
--   'placePdfs' does that.
readPaper :: SiteConfig -> Identifier -> Metadata -> Either [String] Paper
readPaper cfg ident meta =
    case problems of
        [] -> Right Paper
                { paperTitle      = fromMaybe "" title
                , paperAuthors    = zip authors institutions
                , paperDate       = fromMaybe "" date
                , paperPdf        = fromMaybe "" pdf
                , paperPdfSources = either (const []) id sources
                , paperDoi        = doi
                , paperArxiv      = arxiv
                }
        ps -> Left ps
  where
    title   = citationTitle <$> (str "citation-title" meta `orElse` str "title" meta)
    -- The names only: @authors:@ carries "Name | /page" links. No fallback
    -- to the site's author, which the byline uses: a citation names whom
    -- the page names.
    authors = filter (not . null) $ fromMaybe [] $
                  strs "citation-authors" meta `orElse` (map authorNameOf <$> strs "authors" meta)
    date    = str "citation-date" meta `orElse` str "date" meta
    pdf     = str "citation-pdf" meta `orElse` str "preprint" meta
    doi     = str "citation-doi" meta
    arxiv   = str "citation-arxiv" meta
    sources = maybe (Left "no PDF") (pdfSources cfg (toFilePath ident)) pdf

    -- One list of institutions per author. A string is the one author's;
    -- a list goes author by author, "" for none. Without the field, a
    -- single author takes the page's affiliation names.
    institutions :: [[String]]
    institutions = case strs "citation-institution" meta of
        Just xs | length xs == length authors -> map (\x -> [x | not (null x)]) xs
                | length authors == 1         -> [filter (not . null) xs]
        Just _  -> replicate (length authors) []
        Nothing -> case (authors, strs "affiliation" meta) of
            ([_], Just affs) -> [filter (not . null) (map authorNameOf affs)]
            _                -> replicate (length authors) []

    problems = catMaybes
        [ case title of
              Nothing -> Just "no title: give title: or citation-title:"
              Just "" -> Just "the title is empty once its markup is taken off"
              _       -> Nothing
        , if null authors
              then Just "no authors: give authors: or citation-authors:, in author order"
              else Nothing
        , case date of
              Nothing -> Just "no date: give date: or citation-date: (YYYY-MM-DD)"
              Just d | Nothing <- citationDate d ->
                  Just ("citation date " ++ show d ++ " is not YYYY-MM-DD, YYYY-MM or YYYY")
              _ -> Nothing
        , case pdf of
              Nothing -> Just "no PDF: give preprint: or citation-pdf:"
              Just _  -> either Just (const Nothing) sources
        , case strs "citation-institution" meta of
              Just xs | length xs /= length authors && length authors /= 1 ->
                  Just ("citation-institution: " ++ show (length xs) ++ " for "
                        ++ show (length authors) ++ " authors; give one per author, in order (\"\" for none)")
              _ -> Nothing
        , if numeric "citation-arxiv" meta
              then Just "citation-arxiv is a number to YAML, which drops trailing zeros: quote it (\"2611.01230\")"
              else arxiv >>= \a -> if validArxiv a then Nothing
                  else Just ("citation-arxiv " ++ show a ++ " is not an arXiv identifier (2611.01234 or math/0601001)")
        , if numeric "citation-doi" meta
              then Just "citation-doi is a number to YAML: quote it"
              else doi >>= \d -> if validDoi d then Nothing
                  else Just ("citation-doi " ++ show d ++ " is not a bare DOI (10.NNNN/…)")
        , if metadataFlag "noindex" meta
              then Just "noindex: a page kept out of search engines cannot be offered to Scholar"
              else Nothing
        ]

    orElse (Just a) _ = Just a
    orElse Nothing  b = b

-- | The files a page's PDF path may name, in the order they are taken: a
--   path on the site (@/papers/x.pdf@, from @static/@ or from @content/@
--   through the sections' addresses) or one relative to the page's own
--   directory.
pdfSources :: SiteConfig -> FilePath -> String -> Either String [FilePath]
pdfSources cfg page p
    | "http:" `isPrefixOf` p || "https:" `isPrefixOf` p || "//" `isPrefixOf` p =
        Left ("PDF " ++ show p ++ " is a URL: give its path on this site; Scholar takes only a PDF the site serves")
    | map toLower (takeExtension p) /= ".pdf" = Left ("PDF " ++ show p ++ " does not end in .pdf")
    | ".." `elem` splitDirectories p = Left ("PDF " ++ show p ++ " leaves its directory")
    | Just rest <- stripPrefix "/" p = Right (servedFrom cfg rest)
    | otherwise = Right [normalise (takeDirectory page </> p)]

-- | The files that would be served at @path@ (no leading slash): a
--   section's file at its address, a file under @content/@ at its own
--   path, a file under @static/@ at the root.
servedFrom :: SiteConfig -> FilePath -> [FilePath]
servedFrom cfg path = nub $
       [ "content" </> sectionDir s </> r
       | s <- siteSections cfg
       , Just r <- [stripPrefix (sectionAddress s ++ "/") path] ]
    ++ ["content" </> path, "static" </> path]

-- | Where the engine's rules serve a file under @static/@ or @content/@.
servedAt :: SiteConfig -> FilePath -> Maybe FilePath
servedAt cfg src
    | Just r <- stripPrefix "static/" src = Just r
    | "content/" `isPrefixOf` src         = Just (contentAddress cfg src)
    | otherwise                           = Nothing

-- | Where a paper's PDF is, before any copy is named: served beside the
--   page by its own rule already, or to be copied there under the first
--   free one of the names a copy may take.
data Located
    = Beside FilePath FilePath            -- ^ the file, where it is served
    | NeedsCopy FilePath FilePath [String] -- ^ the file, the page's directory, names

-- | The PDF's file, and whether a rule already serves it beside the page.
locatePdf :: SiteConfig -> Identifier -> Paper -> IO (Either String Located)
locatePdf cfg ident p = do
    existing <- filterM doesFileExist (paperPdfSources p)
    case existing of
        [] -> return (Left ("PDF " ++ show (paperPdf p) ++ " is no file: looked for "
                            ++ intercalate ", " (paperPdfSources p)))
        (src : _) -> do
            size <- getFileSize src
            return $ if size > 5000000
                then Left ("PDF " ++ src ++ " is " ++ show (size `div` 1000000)
                           ++ " MB; Scholar takes none over 5 MB")
                else case servedAt cfg src of
                    Nothing -> Left ("PDF " ++ src ++ " is under neither static/ nor content/, "
                                     ++ "so the site does not serve it")
                    Just at
                        | takeDirectory at == pageDir -> Right (Beside src at)
                        | otherwise -> Right (NeedsCopy src pageDir (copyNames ident src))
  where
    pageDir = takeDirectory (replaceExtension (contentAddress cfg (toFilePath ident)) "html")

-- | The names a copy of @src@ beside the page may take, first first: a
--   flat page's own name (@essays/x.md@, @essays/x.pdf@), and for a
--   directory page the PDF's; then the same with the PDF's name added, then
--   numbered, without end ('placePdfs' takes the first free one).
copyNames :: Identifier -> FilePath -> [String]
copyNames ident src = [first, stem ++ "-" ++ takeFileName src]
                      ++ [ stem ++ "-" ++ show n ++ ".pdf" | n <- [2 :: Integer ..] ]
  where
    page    = toFilePath ident
    isIndex = takeFileName page == "index.md"
    stem    = if isIndex then "index" else takeBaseName page
    first   = if isIndex then takeFileName src else stem ++ ".pdf"

-- | Every flagged page's PDF, placed together, so no two copies take one
--   address. A name is free when no copy has it and no file is served
--   there (a file is served at its own address, and the copy moves). First
--   each flat page takes its own name, if it is free: no other page is
--   called @x@ beside it, so no page loses its own name to another's
--   fallback. Then the directory pages take their PDF's name, and last
--   whoever is left takes the first free name of the rest of its list, in
--   order of the pages' paths.
--
--   A choice can change with no change to its page: a file comes to be
--   served at the copy's address, a page comes to want the name. Each
--   placement is therefore named by a marker item ('placementMarker'),
--   and the page and its copy depend on their page's markers.
placePdfs :: SiteConfig -> [(Identifier, Paper)] -> IO [(Identifier, Either String Placement)]
placePdfs cfg papers = do
    located <- forM papers $ \(i, p) -> (\l -> (i, l)) <$> locatePdf cfg i p
    let besides = Set.fromList [ at | (_, Right (Beside _ at)) <- located ]
        copies  = sortOn (toFilePath . fst) [ (i, l) | (i, Right l@NeedsCopy {}) <- located ]
        isIndex i = takeFileName (toFilePath i) == "index.md"
        -- Round one: flat pages, their own names; round two: directory
        -- pages, theirs; round three: everyone left, the rest.
        firsts want = [ (i, src, [inDir dir n]) | (i, NeedsCopy src dir (n : _)) <- copies, want i ]
        rests       = [ (i, src, map (inDir dir) ns) | (i, NeedsCopy src dir (_ : ns)) <- copies ]
    (one, claimed1) <- claim besides (firsts (not . isIndex))
    (two, claimed2) <- claim claimed1 (firsts isIndex)
    let done = Map.fromList (one ++ two)
    (three, _) <- claim claimed2 [ r | r@(i, _, _) <- rests, i `Map.notMember` done ]
    let chosen = Map.union done (Map.fromList three)
    return [ (i, placed)
           | (i, l) <- located
           , let placed = case l of
                     Left e             -> Left e
                     Right (Beside s a) -> Right (Placement s a False)
                     Right (NeedsCopy src _ _) -> case Map.lookup i chosen of
                         Just at -> Right (Placement src at True)
                         Nothing -> Left ("no free name beside the page for a copy of " ++ src) ]
  where
    inDir d n = if d == "." then n else d </> n
    -- Each claims the first free name of its list, in order; a page whose
    -- list runs out (a list of one, in the first rounds) claims nothing.
    claim claimed [] = return ([], claimed)
    claim claimed ((i, src, names) : rest) = do
        free <- firstFree claimed src names
        case free of
            Just at -> do
                (more, claimed') <- claim (Set.insert at claimed) rest
                return ((i, at) : more, claimed')
            Nothing -> claim claimed rest
    firstFree _ _ [] = return Nothing
    firstFree claimed src (at : ats)
        | at `Set.member` claimed = firstFree claimed src ats
        | otherwise = do
            others <- filterM doesFileExist [ f | f <- servedFrom cfg at, f /= src ]
            if null others then return (Just at) else firstFree claimed src ats

-- | The marker of a page's placement: an item no rule routes, named for
--   the page and for where its PDF comes from and goes. The page and its
--   copy read their page's markers ('watchPlacement'); when the placement
--   changes, the marker is another, the set they read differs, and Hakyll
--   rebuilds both.
placementMarker :: Identifier -> Placement -> Identifier
placementMarker ident pl = fromFilePath $
    markerDir ident ++ "/" ++ fnv (placedSource pl ++ "\0" ++ placedPath pl ++ "\0" ++ show (placedCopy pl))

markerDir :: Identifier -> FilePath
markerDir ident = "scholar-placement/" ++ fnv (toFilePath ident)

-- | 64-bit FNV-1a of the UTF-8 bytes, in hexadecimal: a stable name.
fnv :: String -> String
fnv = flip showHex "" . BS.foldl' step (14695981039346656037 :: Word64) . TE.encodeUtf8 . T.pack
  where step h w = (h `xor` fromIntegral w) * 1099511628211

-- | A citation date as Scholar asks for it: the full date when there is
--   one (@2026/11/13@), and otherwise the year. A month alone gives its
--   year.
citationDate :: String -> Maybe String
citationDate d = case splitOnDash d of
    [y, m, dd] | year y, Just (_ :: Day) <- parseTimeM False defaultTimeLocale "%Y-%m-%d" d
                 -> Just (y ++ "/" ++ m ++ "/" ++ dd)
    [y, m]     | year y, two m, read m >= (1 :: Int), read m <= (12 :: Int) -> Just y
    [y]        | year y -> Just y
    _          -> Nothing
  where
    splitOnDash s = case break (== '-') s of
        (a, '-' : rest) -> a : splitOnDash rest
        (a, _)          -> [a]
    year y = length y == 4 && all isDigit y
    two m  = length m == 2 && all isDigit m

validDoi :: String -> Bool
validDoi d = case break (== '/') d of
    ('1' : '0' : '.' : reg, '/' : suffix) ->
        length reg >= 4 && all (\c -> isDigit c || c == '.') reg
        && not (null suffix) && not (any isSpace suffix)
    _ -> False

-- | New style (@2611.01234@, @v2@ allowed) or old (@math/0601001@,
--   @math.CO/0601001@).
validArxiv :: String -> Bool
validArxiv a = case break (== '/') a of
    (archive, '/' : num) ->
        not (null archive) && all (\c -> isAsciiLower c || c == '-' || c == '.' || isAsciiUpper c) archive
        && numbered 7 num
    (new, _) -> case break (== '.') new of
        (yymm, '.' : num) -> length yymm == 4 && all isDigit yymm
                             && (numbered 4 num || numbered 5 num)
        _ -> False
  where
    numbered n s = let (ds, v) = splitAt n s
                   in length ds == n && all isDigit ds
                      && (null v || (take 1 v == "v" && length v > 1 && all isDigit (drop 1 v)))

-- | A title as plain text: the inline markup a title may carry (@<em>@,
--   @<sup>@, …) taken off, character references read as a browser reads
--   them (pandoc's HTML5 table: @&eacute;@, @&ndash;@, @&le;@), whitespace
--   collapsed. Only those
--   tags are markup: @a < b@ and @x<y and y>z@ are text. 'citationTags'
--   escapes the result once.
citationTitle :: String -> String
citationTitle = unwords . words . T.unpack . fromEntities . T.pack . stripInline

stripInline :: String -> String
stripInline ('<' : rest)
    | (name, after) <- span isAlphaNum (dropWhile (== '/') rest)
    , map toLower name `elem` inlineTags
    , (inside, '>' : rest') <- break (== '>') after
    , all (\c -> c /= '<') inside
    , null inside || isSpace (head inside) || inside == "/"
    = stripInline rest'
stripInline (c : rest) = c : stripInline rest
stripInline [] = []

inlineTags :: [String]
inlineTags = [ "a", "abbr", "b", "bdi", "bdo", "br", "cite", "code", "data", "dfn", "em", "i"
             , "kbd", "mark", "q", "s", "samp", "small", "span", "strong", "sub", "sup"
             , "time", "u", "var", "wbr" ]

-- | A path, each segment percent-encoded as UTF-8 except for what RFC 3986
--   lets a segment hold as it is: @bound#1.pdf@ is @bound%231.pdf@.
encodeUrlPath :: FilePath -> String
encodeUrlPath = intercalate "/" . map segment . splitOn '/'
  where
    splitOn c s = case break (== c) s of
        (a, _ : rest) -> a : splitOn c rest
        (a, [])       -> [a]
    segment = concatMap byte . BS.unpack . TE.encodeUtf8 . T.pack
    byte w
        | c <- chr (fromIntegral w), w < 128, isAlphaNum c || c `elem` ("-._~!$&'()*+,;=:@" :: String) = [c]
        | otherwise = '%' : pad (map toUpperHex (showHex w ""))
    pad s = replicate (2 - length s) '0' ++ s
    toUpperHex c = if c >= 'a' && c <= 'f' then chr (ord c - 32) else c

-- ---------------------------------------------------------------------------
-- Rules
-- ---------------------------------------------------------------------------

-- | Every flagged page among @pages@, read, placed and checked. The build
--   is refused, with every problem of every page, when one is wrong; and
--   when a published page the essay rules do not compile (a post, a poem,
--   a standalone page) is flagged, since nothing would give it its tags.
--   A withheld page (a draft) is not published, and nothing is asked of it.
--   The placements are made once, here, for the copy rules and the pages
--   alike ('scholarPdfRules', 'scholarFields').
scholarlyPapers :: SiteConfig -> Pattern -> Rules [(Identifier, Paper, Placement)]
scholarlyPapers cfg pages = do
    ids <- getMatches pages
    found <- fmap catMaybes $ forM ids $ \ident -> do
        meta <- getMetadata ident
        return $ if flagged meta then Just (ident, meta) else Nothing
    -- A PDF Hakyll does not see (a refused name: x.draft.pdf) is no file
    -- the site serves, nor is one withheld with a draft.
    resources <- getMatches ("content/**" .||. "static/**")
    let seen = Set.fromList (map toFilePath resources)
        readings = [ (i, readPaper cfg i meta) | (i, meta) <- found ]
    placed <- preprocess $ placePdfs cfg [ (i, p) | (i, Right p) <- readings ]
    let papers = [ (i, p) | (i, Right p) <- readings ]
        checked =
            [ (i, Left ps) | (i, Left ps) <- readings ] ++
            [ (i, either (Left . (: [])) (published p) pl)
            | (i, pl) <- placed, Just p <- [lookup i papers] ]
        published p pl
            | placedSource pl `Set.notMember` seen =
                Left ["PDF " ++ placedSource pl ++ " is never published (its name is refused, "
                      ++ "or the build does not read it)"]
            | isUnpublished currentUnpublished (placedSource pl) =
                Left ["PDF " ++ placedSource pl ++ " is withheld with a draft"]
            | any (\w -> matches (fromGlob ("static/" ++ w)) (fromFilePath (placedSource pl)))
                  (siteWithheld cfg) =
                Left ["PDF " ++ placedSource pl ++ " is withheld by site.yaml"]
            | otherwise = Right (p, pl)
    others <- getMatches ("content/**" .&&. complement pages .&&. complement "content/drafts/**")
    strays <- fmap catMaybes $
        forM [ i | i <- others, takeExtension (toFilePath i) == ".md"
                 , not (isUnpublished currentUnpublished (toFilePath i)) ] $ \i -> do
            meta <- getMetadata i
            return $ if flagged meta then Just (toFilePath i) else Nothing
    let bad = [ toFilePath i ++ ": " ++ intercalate "; " ps | (i, Left ps) <- checked ]
           ++ [ s' ++ ": scholarly: is read on essays only, and this page is not one" | s' <- strays ]
        -- Scholar's own example is an http:// URL; the URL must be absolute.
        absolute = any (`isPrefixOf` siteUrl cfg) ["https://", "http://"]
    unless (null bad && (null found || absolute)) $ preprocess $ ioError $ userError $
        "Scholarly pages (scholarly: true) that Scholar cannot take:\n  "
        ++ intercalate "\n  " (bad ++ [ "site.yaml: url " ++ show (siteUrl cfg)
                                        ++ " is not an absolute http(s) URL, as citation_pdf_url must be"
                                      | not absolute ])
    return [ (i, p, pl) | (i, Right (p, pl)) <- checked ]

-- | Depend on the page's placement: its markers ('placementMarker'). The
--   rules place every PDF again on every build; a page or copy built with
--   another placement read another marker, and is rebuilt.
watchPlacement :: Identifier -> Compiler ()
watchPlacement ident = void (getMatches (fromGlob (markerDir ident ++ "/*")))

-- | A flagged page's PDF beside it, when no rule of its own serves it there.
scholarPdfRules :: [(Identifier, Paper, Placement)] -> Rules ()
scholarPdfRules papers = do
    forM_ papers $ \(ident, _, pl) ->
        create [placementMarker ident pl] $ compile (makeItem ("" :: String))
    forM_ [ (i, pl) | (i, _, pl) <- papers, placedCopy pl ] $ \(ident, pl) ->
        match (fromList [ident]) $ version "scholar-pdf" $ do
            route (constRoute (placedPath pl))
            compile $ do
                watchPlacement ident
                -- The copy is out of date with the file, not the page.
                _ <- load (fromFilePath (placedSource pl)) :: Compiler (Item CopyFile)
                makeItem (CopyFile (placedSource pl))

-- ---------------------------------------------------------------------------
-- Fields
-- ---------------------------------------------------------------------------

-- | @$scholar-meta$@ (the tags, for the head), @$scholar-pdf$@ (the PDF
--   beside the page, for its Preprint link) and @$scholar-arxiv$@ (the
--   arXiv abstract page), on a flagged page of @papers@ ('scholarlyPapers');
--   no result on any other.
scholarFields :: [(Identifier, Paper, Placement)] -> Context a
scholarFields papers =
       field "scholar-meta" (\item -> do
           (p, url) <- paperOf item
           cfg <- loadConfig
           return (citationTags p (siteUrl cfg ++ url)))
    -- Escaped for the href it goes in: a name with @&copy;@ in it is a
    -- path, not a character reference.
    <> field "scholar-pdf" (fmap (escapeHtml . snd) . paperOf)
    <> field "scholar-arxiv" (\item -> do
           (p, _) <- paperOf item
           maybe (noResult "no arXiv identifier")
                 (return . ("https://arxiv.org/abs/" ++)) (paperArxiv p))
  where
    byPage = Map.fromList [ (i, (p, pl)) | (i, p, pl) <- papers ]
    -- The rules' reading and placement, and the PDF's own rule confirmed
    -- to serve it where the tags say.
    paperOf item = do
        let ident = setVersion Nothing (itemIdentifier item)
        (p, pl) <- maybe (noResult "not a scholarly page") return (Map.lookup ident byPage)
        watchPlacement ident
        let srcId = fromFilePath (placedSource pl)
        _ <- (load srcId :: Compiler (Item CopyFile)) `catchError` \_ ->
                 fail ("PDF " ++ placedSource pl ++ " is not published by any rule")
        when (not (placedCopy pl)) $ do
            served <- getRoute srcId
            unless (fmap normalise served == Just (normalise (placedPath pl))) $
                fail ("PDF " ++ placedSource pl ++ " is served at " ++ maybe "no address" show served
                      ++ ", not beside its page at " ++ placedPath pl)
        return (p, "/" ++ encodeUrlPath (placedPath pl))

-- | The tags, in Highwire order: title, each author (each followed by its
--   institutions), date, DOI, PDF.
citationTags :: Paper -> String -> String
citationTags p pdfUrl = unlines $
    [ tag "citation_title" (paperTitle p) ]
    ++ concat [ tag "citation_author" a : map (tag "citation_author_institution") is
              | (a, is) <- paperAuthors p ]
    ++ [ tag "citation_publication_date" d | Just d <- [citationDate (paperDate p)] ]
    ++ [ tag "citation_doi" d | Just d <- [paperDoi p] ]
    ++ [ tag "citation_pdf_url" pdfUrl ]
  where
    tag name v = "<meta name=\"" ++ name ++ "\" content=\"" ++ escapeHtml v ++ "\">"
