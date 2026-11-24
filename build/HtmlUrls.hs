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
    , isRelative
    , resolveAgainst
    ) where

import           Data.Char       (isAlpha, isAlphaNum, isSpace, ord, toLower)
import           Data.List       (intercalate, isPrefixOf, isSuffixOf)
import           Data.Maybe      (fromMaybe, isNothing)
import qualified Data.Text       as T
import           Text.Pandoc.XML (fromEntities)

-- | A stretch of the source kept as written, or a URL attribute's value.
data Piece = Kept String | Value String Bool String   -- ^ attribute (lower case), quoted, value

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
--   written again, with a comma inside a URL as @%2C@, which names the
--   same file: Hakyll's own srcset parser (in @relativizeUrls@, which
--   every page goes through) cuts a value at every comma, and would split
--   @\/essays\/a\/lake,summer.svg@ in two. What is written is escaped for where it stands: in a
--   value without quotes, a space or a @>@ would end it
--   (@href=field&#32;notes.txt@).
mapUrlsRead :: (String -> Maybe String) -> String -> String
mapUrlsRead f = concatMap piece . pieces
  where
    piece (Kept s) = s
    piece (Value "srcset" quoted v) =
        let (candidates, trailer) = srcsetCandidates (readValue v)
            changed               = [ f url | (_, url, _) <- candidates ]
        in  if all isNothing changed then v
            else escapeFor quoted (concat
                    [ lead ++ concatMap comma (fromMaybe url new) ++ rest
                    | ((lead, url, rest), new) <- zip candidates changed ] ++ trailer)
    piece (Value _ quoted v) =
        let text          = readValue v
            (lead, body)  = span isUrlSpace text
            url           = reverse (dropWhile isUrlSpace (reverse body))
            trail         = drop (length url) body
        in  maybe v (\new -> escapeFor quoted (lead ++ new ++ trail))
                    (f (filter (`notElem` "\t\n\r") url))
    comma ',' = "%2C"
    comma c   = [c]
    -- What the URL parser strips from either end: C0 controls and space.
    isUrlSpace c = c <= ' '
    escapeFor quoted = concatMap escape
      where
        escape '&'  = "&amp;"
        escape '"'  = "&quot;"
        escape '\'' = "&#39;"
        escape c | not quoted && (isSpace c || c == '>') = "&#" ++ show (ord c) ++ ";"
                 | otherwise                             = [c]

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
piecesFor names = text ""
  where
    kept acc rest = [Kept (reverse acc) | not (null acc)] ++ rest

    -- Outside a tag.
    text acc [] = kept acc []
    text acc s@('<' : rest)
        | "<!--" `isPrefixOf` s = let (comment, after) = through "-->" s in text (reverse comment ++ acc) after
        | (c : _) <- rest, isAlpha c =
            let (name, after) = span (\x -> isAlphaNum x || x `elem` "-:") rest
            in  attributes (map toLower name) (reverse name ++ '<' : acc) after
    text acc (c : rest) = text (c : acc) rest

    -- Inside an opening tag, between its attributes.
    attributes _ acc [] = kept acc []
    attributes tag acc ('>' : rest)
        | tag `elem` ["script", "style"] =
            let (body, after) = through ("</" ++ tag) rest in text (reverse body ++ '>' : acc) after
        | otherwise = text ('>' : acc) rest
    attributes tag acc (c : rest)
        | isSpace c || c == '/' = attributes tag (c : acc) rest
    attributes tag acc s =
        let (name, after)   = break (\c -> isSpace c || c `elem` "=>/") s
            (gap, after')   = span isSpace after
            acc'            = reverse gap ++ reverse name ++ acc
        in case after' of
            '=' : more ->
                let (gap', more') = span isSpace more
                    acc''         = reverse gap' ++ '=' : acc'
                in  value tag (map toLower name) acc'' more'
            _ | null name -> attributes tag (reverse (take 1 after') ++ acc') (drop 1 after')
              | otherwise -> attributes tag acc' after'

    -- An attribute's value: quoted, or bare to the next space.
    value tag name acc (q : rest)
        | q `elem` "\"'" =
            let (v, after) = break (== q) rest
            in  emit tag name True (q : acc) v (take 1 after) (drop 1 after)
    value tag name acc s =
        let (v, after) = break (\c -> isSpace c || c == '>') s
        in  emit tag name False acc v "" after
    emit tag name quoted acc v close after
        | name `elem` names = kept acc (Value name quoted v : attributes tag (reverse close) after)
        | otherwise                 = attributes tag (reverse close ++ reverse v ++ acc) after

    -- Up to and including the first occurrence of @end@ (any case), or
    -- everything when there is none.
    through end s = go "" s
      where
        go acc [] = (reverse acc, [])
        go acc r@(c : cs)
            | map toLower (take (length end) r) == end = (reverse acc ++ take (length end) r, drop (length end) r)
            | otherwise = go (c : acc) cs

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
