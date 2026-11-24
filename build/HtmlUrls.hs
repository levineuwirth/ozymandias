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
--   list of them (@srcset@: candidates between commas, each a URL and
--   then an optional width or density).
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
    candidates "srcset" value = [ url | (_, url, _) <- srcsetCandidates (readValue value) ]
    candidates _        value = [trim (readValue value)]

-- | An attribute's value as text: its character references read.
readValue :: String -> String
readValue = T.unpack . fromEntities . T.pack

-- | The HTML with every URL in its attributes given to @f@.
mapUrls :: (String -> String) -> String -> String
mapUrls f = concatMap piece . pieces
  where
    piece (Kept s)             = s
    piece (Value "srcset" _ v) = intercalate "," [ lead ++ f url ++ rest | (lead, url, rest) <- srcsetCandidates v ]
    piece (Value _ _ v)        = let (lead, body) = span isSpace v
                                     (url, rest)  = (trimEnd body, drop (length (trimEnd body)) body)
                                 in  lead ++ f url ++ rest

-- | 'mapUrls' for a rewrite that reads each URL as a browser does: @f@
--   is given the URL with its character references read, and answers
--   'Nothing' to leave the value exactly as written, or the URL to write
--   in its place. Classifying the written value instead takes
--   @&#47;essays/c/@, which is rooted, for a relative path.
--
--   A @srcset@ is read whole before it is cut into candidates, since a
--   reference may write its separators (@a.svg&#32;1x&#44; b.svg 2x@); if
--   any candidate changes, the whole value is written again. What is
--   written is escaped for where it stands: in a value without quotes, a
--   space or a @>@ would end it (@href=field&#32;notes.txt@).
mapUrlsRead :: (String -> Maybe String) -> String -> String
mapUrlsRead f = concatMap piece . pieces
  where
    piece (Kept s) = s
    piece (Value "srcset" quoted v) =
        let candidates = srcsetCandidates (readValue v)
            changed    = [ f url | (_, url, _) <- candidates ]
        in  if all isNothing changed then v
            else escapeFor quoted (intercalate ","
                    [ lead ++ fromMaybe url new ++ rest
                    | ((lead, url, rest), new) <- zip candidates changed ])
    piece (Value _ quoted v) =
        let (lead, body) = span isSpace v
            (url, rest)  = (trimEnd body, drop (length (trimEnd body)) body)
        in  maybe v (\new -> lead ++ escapeFor quoted new ++ rest) (f (readValue url))
    escapeFor quoted = concatMap escape
      where
        escape '&'  = "&amp;"
        escape '"'  = "&quot;"
        escape '\'' = "&#39;"
        escape c | not quoted && (isSpace c || c == '>') = "&#" ++ show (ord c) ++ ";"
                 | otherwise                             = [c]

-- | A @srcset@ value's candidates: the space before each URL, the URL,
--   and what follows it up to the comma.
srcsetCandidates :: String -> [(String, String, String)]
srcsetCandidates value = [ cut c | c <- splitOn ',' value, not (all isSpace c) ]
  where
    cut c = let (lead, body) = span isSpace c
                (url, rest)  = break isSpace body
            in  (lead, url, rest)

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
pieces = text ""
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
        | name `elem` urlAttributes = kept acc (Value name quoted v : attributes tag (reverse close) after)
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
