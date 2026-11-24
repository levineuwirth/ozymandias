{-# LANGUAGE GHC2021 #-}
-- | The URLs in a piece of HTML's attributes, read and rewritten where
--   they stand.
--
--   Hakyll's @withUrls@ parses the HTML and writes it out again, which
--   respells what it did not change (entities, quotes, empty elements).
--   This cuts the string at the values of the attributes that hold a URL
--   and leaves every other character as it was written, so rewriting a
--   feed entry's relative URLs changes those URLs and nothing else.
--
--   It reads tags, not documents: a comment and the text of a @script@
--   or @style@ element are passed over whole, and a value without quotes
--   is read to the next space.
module HtmlUrls
    ( urlsIn
    , mapUrls
    , mapUrlsRead
    , attributeValues
    , relativizeUrlsWith
    , isRelative
    , resolveAgainst
    ) where

import           Data.Char       (isAlpha, isAlphaNum, isSpace, ord, toLower)
import           Data.List       (intercalate, isPrefixOf, isSuffixOf)
import           Data.Maybe      (fromMaybe, isNothing)
import qualified Data.Text       as T
import           Text.Pandoc.XML (fromEntities)

-- | A stretch of the source kept as written, or a URL attribute's value.
data Piece = Kept String | Value String (Maybe Char) String   -- ^ attribute (lower case), its quote, value

-- | The attributes whose value is a URL, and the one whose value is a
--   list of them (@srcset@: candidates, each a URL and then an optional
--   width or density, read as 'srcsetCandidates' reads them).
urlAttributes :: [String]
urlAttributes = ["src", "href", "poster", "data", "srcset"]

-- | Every URL the HTML's attributes name, in order, as a reader of the
--   HTML has it: an attribute's value is text, so its character
--   references are read (@A&amp;B.webp@ names @A&B.webp@). 'mapUrls'
--   leaves them as written, since what it writes is HTML again.
urlsIn :: String -> [String]
urlsIn html = concat [ candidates name value | Value name _ value <- pieces html ]
  where
    -- The whole value first: a reference may write the space or the
    -- comma a srcset is cut at (@art.webp&#32;1x@).
    candidates "srcset" value = [ url | (_, url, _) <- fst (srcsetCandidates (readValue value)) ]
    candidates _        value = [trim (readValue value)]

-- | An attribute's value as text: its character references read.
readValue :: String -> String
readValue = T.unpack . fromEntities . T.pack

-- | The value of every @name@ attribute (lower case) in the HTML, read:
--   @id='here'@ and @id="r&amp;d"@ name the anchors @here@ and @r&d@.
attributeValues :: String -> String -> [String]
attributeValues name html = [ readValue v | Value _ _ v <- piecesFor [name] html ]

-- | The HTML with every URL in its attributes given to @f@.
mapUrls :: (String -> String) -> String -> String
mapUrls f = concatMap piece . pieces
  where
    piece (Kept s)             = s
    piece (Value "srcset" _ v) = let (cs, trailer) = srcsetCandidates v
                                 in  concat [ lead ++ f url ++ rest | (lead, url, rest) <- cs ] ++ trailer
    piece (Value _ _ v)        = let (lead, body) = span isSpace v
                                     (url, rest)  = (trimEnd body, drop (length (trimEnd body)) body)
                                 in  lead ++ f url ++ rest

-- | 'mapUrls' for a rewrite that reads each URL as a browser does: @f@
--   is given the URL with its character references read, and answers
--   'Nothing' to leave the value exactly as written, or the URL to write
--   in its place. Classifying the written value instead takes
--   @&#47;essays/c/@, which is rooted, for a relative path.
--
--   The value is read first and then taken apart as a browser takes it:
--   a URL loses the spaces and control characters around it and any tab
--   or newline inside it (the URL parser's own trimming), so
--   @&#32;/essays/c/@ is rooted; a @srcset@ is read whole before it is cut
--   into candidates ('srcsetCandidates'), since a reference may write its
--   separators (@a.svg&#32;1x&#44; b.svg 2x@) or a comma inside a URL
--   (@lake&#44;summer.svg@). If any candidate changes, the whole value is
--   written again, each URL as it reads (a @data:@ URL keeps the comma it
--   needs). What is written is escaped for where it stands: @&@ and the
--   value's own quote, or, in a value without quotes, also a space or a
--   @>@, which would end it (@href=field&#32;notes.txt@).
mapUrlsRead :: (String -> Maybe String) -> String -> String
mapUrlsRead f = concatMap piece . pieces
  where
    piece (Kept s) = s
    piece (Value "srcset" quoted v) =
        let (candidates, trailer) = srcsetCandidates (readValue v)
            changed               = [ f url | (_, url, _) <- candidates ]
        in  if all isNothing changed then v
            else escapeFor quoted (concat
                    [ lead ++ fromMaybe url new ++ rest
                    | ((lead, url, rest), new) <- zip candidates changed ] ++ trailer)
    piece (Value _ quoted v) =
        let text          = readValue v
            (lead, body)  = span isUrlSpace text
            url           = reverse (dropWhile isUrlSpace (reverse body))
            trail         = drop (length url) body
        in  maybe v (\new -> escapeFor quoted (lead ++ new ++ trail))
                    (f (filter (`notElem` "\t\n\r") url))
    -- What the URL parser strips from either end: C0 controls and space.
    isUrlSpace c = c <= ' '
    escapeFor quote = concatMap escape
      where
        escape c
            | c == '&' || Just c == quote = "&#" ++ show (ord c) ++ ";"
            | quote == Nothing && (isSpace c || c `elem` ("\"'>" :: String)) = "&#" ++ show (ord c) ++ ";"
            | otherwise = [c]

-- | Every URL rooted at the site (@\/essays\/@, not @\/\/host\/@) made
--   relative to a page whose path to the site root is @root@ (@..\/..@),
--   as Hakyll's @relativizeUrlsWith@ does, but reading each URL as
--   'mapUrlsRead' reads it: a srcset by the HTML standard's parser, so
--   @\/essays\/a\/lake,summer.svg 1x@ is one candidate, where Hakyll's cuts
--   it at the comma. Everything it does not change is left as written;
--   Hakyll's wrote the whole page out again through TagSoup.
relativizeUrlsWith :: String -> String -> String
relativizeUrlsWith root = mapUrlsRead $ \u ->
    if "/" `isPrefixOf` u && not ("//" `isPrefixOf` u) then Just (root ++ u) else Nothing

-- | A @srcset@ value's candidates as the HTML standard parses them
--   ("parsing a srcset attribute"): each is the separators before it
--   (whitespace and commas), its URL (everything up to whitespace, less
--   any commas it ends with) and what follows (those commas, or its
--   descriptors up to a comma outside parentheses); then whatever trails
--   the last. A comma inside a URL is the URL's: @lake,summer.svg 1x@ is
--   one candidate. The pieces put back together are the value.
srcsetCandidates :: String -> ([(String, String, String)], String)
srcsetCandidates value
    | null body = ([], lead)
    | otherwise = ((lead, url, rest) : more, trailer)
  where
    (lead, body)    = span (\c -> isHtmlSpace c || c == ',') value
    (word, after)   = break isHtmlSpace body
    url             = reverse (dropWhile (== ',') (reverse word))
    commas          = drop (length url) word
    (rest, next)
        | null commas = descriptors False "" after
        | otherwise   = (commas, after)
    (more, trailer) = srcsetCandidates next
    -- Up to a comma outside parentheses, which begins the next separators.
    descriptors _ acc []              = (reverse acc, [])
    descriptors False acc s@(',' : _) = (reverse acc, s)
    descriptors inParens acc (c : cs) =
        descriptors (if c == '(' then True else if c == ')' then False else inParens) (c : acc) cs
    isHtmlSpace c = c `elem` (" \t\n\f\r" :: String)

-- | Is this a reference to be resolved against the page that makes it:
--   not a URL with a scheme, not rooted at the site or the host, and not
--   only a fragment or a query of the page itself?
isRelative :: String -> Bool
isRelative url = case url of
    []      -> False
    (c : _) -> c `notElem` "/#?" && not (hasScheme url)
  where
    hasScheme u = case span (\c -> isAlphaNum c || c `elem` "+-.") u of
        (c : _, ':' : _) -> isAlpha c
        _                -> False

-- | A relative reference from a page whose directory is at @base@ (a URL
--   or a path ending in a slash), with @.@ and @..@ taken out of the
--   path: @resolveAgainst "https:\/\/a.example\/notes\/" "..\/x.png?v=2"@
--   is @https:\/\/a.example\/x.png?v=2@. A reference cannot climb above the
--   base's first directory. A final @.@ or @..@ names a directory, so the
--   result ends in a slash, as a browser's does: @..\/c\/.@ is @c\/@.
resolveAgainst :: String -> String -> String
resolveAgainst base reference = root ++ joined ++ slash ++ tail'
  where
    (path, tail')   = break (`elem` "?#") reference
    (root, dirs)    = splitRoot base
    parts           = splitOn '/' (dirs ++ path)
    joined          = intercalate "/" (foldl step [] parts)
    slash | last parts `elem` [".", ".."], not (null joined), not ("/" `isSuffixOf` joined) = "/"
          | otherwise = ""
    step acc "."    = acc
    step acc ".."   = if null acc then [] else init acc
    step acc part   = acc ++ [part]
    -- "https://host/" is not a directory to climb out of.
    splitRoot b = case breakOn "://" b of
        Just (scheme, rest) -> let (host, after) = break (== '/') rest
                               in  (scheme ++ "://" ++ host ++ "/", drop 1 after)
        Nothing             -> if "/" `isPrefixOf` b then ("/", drop 1 b) else ("", b)

-- ---------------------------------------------------------------------------

pieces :: String -> [Piece]
pieces = piecesFor urlAttributes

-- | 'pieces', with the values of these attributes (lower case) cut out.
piecesFor :: [String] -> String -> [Piece]
piecesFor names = text [] ""
  where
    kept acc rest = [Kept (reverse acc) | not (null acc)] ++ rest

    -- Outside a tag. @ns@ is the open elements that decide how HTML
    -- parses what follows: an @svg@ or @math@ (foreign content, where
    -- @style@ and @script@ are elements like any other and @<style/>@
    -- closes itself), or an integration point inside one (@foreignObject@,
    -- @desc@ and @title@ in SVG; @mi@, @mo@, @mn@, @ms@ and @mtext@ in
    -- MathML), where HTML resumes. A closing tag pops its own name.
    text _ acc [] = kept acc []
    text ns acc s@('<' : rest)
        | "<!--" `isPrefixOf` s = let (comment, after) = through "-->" s in text ns (reverse comment ++ acc) after
        | ('/' : c : _) <- rest, isAlpha c =
            let (name, after) = span tagChar (drop 1 rest)
                ns'           = case ns of
                    top : below | top == map toLower name -> below
                    _                                     -> ns
            in  text ns' (reverse name ++ "/<" ++ acc) after
        | (c : _) <- rest, isAlpha c =
            let (name, after) = span tagChar rest
            in  attributes ns (map toLower name) (reverse name ++ '<' : acc) after
    text ns acc (c : rest) = text ns (c : acc) rest

    tagChar x = isAlphaNum x || x `elem` ("-:" :: String)

    inForeign (top : _) = top `elem` ["svg", "math"]
    inForeign []        = False

    -- Inside an opening tag, between its attributes.
    attributes _ _ acc [] = kept acc []
    attributes ns tag acc ('>' : rest)
        -- In HTML a raw-text element's contents are not markup, whether or
        -- not its tag ends in a slash, up to its whole closing tag.
        | not (inForeign ns) && tag `elem` rawText =
            let (body, after) = untilClosing tag rest in text ns (reverse body ++ '>' : acc) after
        | otherwise = text ns' ('>' : acc) rest
      where
        ns' | take 1 acc == "/"                     = ns
            | tag `elem` ["svg", "math"]            = tag : ns
            | inForeign ns && tag `elem` integrations = tag : ns
            | otherwise                             = ns
    attributes ns tag acc (c : rest)
        | isSpace c || c == '/' = attributes ns tag (c : acc) rest
    attributes ns tag acc s =
        let (name, after)   = break (\c -> isSpace c || c `elem` ("=>/" :: String)) s
            (gap, after')   = span isSpace after
            acc'            = reverse gap ++ reverse name ++ acc
        in case after' of
            '=' : more ->
                let (gap', more') = span isSpace more
                    acc''         = reverse gap' ++ '=' : acc'
                in  value ns tag (map toLower name) acc'' more'
            _ | null name -> attributes ns tag (reverse (take 1 after') ++ acc') (drop 1 after')
              | otherwise -> attributes ns tag acc' after'

    -- An attribute's value: quoted, or bare to the next space.
    value ns tag name acc (q : rest)
        | q `elem` ("\"'" :: String) =
            let (v, after) = break (== q) rest
            in  emit ns tag name (Just q) (q : acc) v (take 1 after) (drop 1 after)
    value ns tag name acc s =
        let (v, after) = break (\c -> isSpace c || c == '>') s
        in  emit ns tag name Nothing acc v "" after
    emit ns tag name quoted acc v close after
        | name `elem` names = kept acc (Value name quoted v : attributes ns tag (reverse close) after)
        | otherwise         = attributes ns tag (reverse close ++ reverse v ++ acc) after

    -- HTML's raw-text and escapable raw-text elements. noscript is left
    -- out: its fallback markup is relativized, as Hakyll's pass did.
    rawText      = ["script", "style", "xmp", "iframe", "noembed", "noframes", "title", "textarea"]
    integrations = ["foreignobject", "desc", "title", "mi", "mo", "mn", "ms", "mtext"]

    -- Up to (not including) the element's closing tag: @</@, its whole
    -- name in any case, then a space, a @/@ or a @>@. @</scripture>@
    -- inside a script is the script's.
    untilClosing tag = go ""
      where
        end = "</" ++ tag
        go acc [] = (reverse acc, [])
        go acc r@(c : cs)
            | map toLower (take (length end) r) == end, closes (drop (length end) r) = (reverse acc, r)
            | otherwise = go (c : acc) cs
        closes []      = True
        closes (x : _) = x `elem` ("\t\n\f\r />" :: String)

    -- Up to and including the first occurrence of @end@, or everything
    -- when there is none.
    through end s = go "" s
      where
        go acc [] = (reverse acc, [])
        go acc r@(c : cs)
            | end `isPrefixOf` r = (reverse acc ++ end, drop (length end) r)
            | otherwise          = go (c : acc) cs

splitOn :: Char -> String -> [String]
splitOn sep s = case break (== sep) s of
    (a, _ : rest) -> a : splitOn sep rest
    (a, [])       -> [a]

breakOn :: String -> String -> Maybe (String, String)
breakOn needle = go ""
  where
    go _ [] = Nothing
    go acc r@(c : cs)
        | needle `isPrefixOf` r = Just (reverse acc, drop (length needle) r)
        | otherwise             = go (c : acc) cs

trim, trimEnd :: String -> String
trim = trimEnd . dropWhile isSpace
trimEnd = reverse . dropWhile isSpace . reverse
