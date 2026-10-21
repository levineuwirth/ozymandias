{-# LANGUAGE GHC2021 #-}
-- | Parser for custom fields on BibLaTeX entries that citeproc doesn't
--   surface on its own: @file:@ (path to a hosted PDF) and @keywords:@
--   (comma-separated list, shared vocabulary with essay-frontmatter
--   @keywords:@ for bibliography-page cross-linking). Also captures
--   @author:@ and @year:@ used for bibliography-page sorting.
--
--   Character-based scanner with brace-balance tracking, so fields
--   whose values span multiple lines parse correctly — e.g.:
--
--   @
--   \@inproceedings{kyber2018,
--     author = {Bos, Joppe W. and Ducas, Léo and ...
--                 and Stehlé, Damien},
--     title  = {{CRYSTALS -- Kyber}},
--     year   = {2018}
--   }
--   @
--
--   Field values enclosed in @{...}@ (balanced) or @"..."@ are both
--   recognized. Unknown fields are ignored.
module BibExtras
    ( BibExtra (..)
    , emptyBibExtra
    , parseBibExtras
    , firstAuthorSurname
    ) where

import Data.Char        (isAlphaNum, isSpace, toLower)
import Data.List        (dropWhileEnd)
import Data.Map.Strict  (Map)
import qualified Data.Map.Strict as Map
import System.IO        (readFile')
import Utils            (splitOn, trim)


-- | Custom fields we extract per citekey. Fields absent from the
--   entry normalize to @Nothing@ / @[]@.
data BibExtra = BibExtra
    { bibFile     :: Maybe FilePath   -- ^ @file:@ — URL path to a hosted PDF.
    , bibKeywords :: [String]         -- ^ @keywords:@ — comma-split, trimmed.
    , bibAuthor   :: Maybe String     -- ^ @author:@ — raw value, sort key only.
    , bibYear     :: Maybe String     -- ^ @year:@ — raw value, sort key only.
    } deriving (Show)

-- | Neutral default for a citekey with no custom fields.
emptyBibExtra :: BibExtra
emptyBibExtra = BibExtra Nothing [] Nothing Nothing

-- | First-author surname for alphabetic sort. Conservative extraction:
--   take everything up to the first comma of the first author entry.
--   BibLaTeX author format separates authors with " and ", so
--   "Nietzsche, Friedrich and Holub, Robert C." → "Nietzsche".
--   Corporate authors like "{National Institute of ...}" strip the
--   outer braces (the parser drops them) and sort by the full name.
--   Entries without an author sort under the empty string.
firstAuthorSurname :: BibExtra -> String
firstAuthorSurname extra = case bibAuthor extra of
    Just s  -> trim (takeWhile (/= ',') (stripOuterBraces s))
    Nothing -> ""
  where
    stripOuterBraces ('{':rest) = dropWhileEnd (== '}') rest
    stripOuterBraces s          = s


-- | Parse a @.bib@ file; returns a map @citekey -> 'BibExtra'@.
parseBibExtras :: FilePath -> IO (Map String BibExtra)
parseBibExtras path = Map.fromList . parseBib <$> readFile' path


-- ---------------------------------------------------------------------------
-- Character-based scanner
-- ---------------------------------------------------------------------------

-- | Enumerate all entries in a .bib file as (citekey, extra) pairs.
--   @\@string@ \/ @\@comment@ \/ @\@preamble@ blocks (case-insensitive)
--   carry no citekey and are skipped wholesale.
parseBib :: String -> [(String, BibExtra)]
parseBib input = go (dropTo '@' input)
  where
    -- Advance past any non-entry prefix to the first '@'.
    dropTo c = dropWhile (/= c)

    go [] = []
    go ('@':rest) =
        let -- Entry type, then '{', then citekey, then ',', then fields, then '}'.
            (typeName, r1) = span isAlphaNum rest
            r2 = dropWhile isSpace r1
        in case r2 of
            '{':r3
                -- Not citekey entries: a @string macro name (or the body
                -- of a @comment/@preamble) must never be parsed as a
                -- citekey. Skip the balanced brace group and carry on.
                | map toLower typeName `elem` ["string", "comment", "preamble"] ->
                    let (_, r4) = readBraces 1 "" r3
                    in  go (dropTo '@' r4)
                | otherwise ->
                    -- '}' ends the key too: a fieldless @misc{key} must not
                    -- take the brace, and with it the next entry's fields.
                    -- An '@' before the key's ',' or '}' is the next entry
                    -- starting: this one is malformed, and the scan must
                    -- stop there rather than read the next entry's fields
                    -- as its own.
                    let (citekey, r4) = span (\c -> c `notElem` (",}@{" :: String) && not (isSpace c)) r3
                        r5 = dropWhile (\c -> c /= ',' && c /= '}' && c /= '@') r4
                    in case r5 of
                        ',':r6 ->
                            let (flds, r7) = parseFields r6
                            in (trim citekey, toExtra flds) : go (dropTo '@' r7)
                        -- Fieldless entries: walk past and carry on.
                        '}':r6 -> (trim citekey, emptyBibExtra) : go (dropTo '@' r6)
                        -- Malformed: skip to the next entry rather than
                        -- dropping the rest of the file.
                        _ -> go (dropTo '@' r5)
            _ -> go (dropTo '@' r2)
    go (_:rest) = go (dropTo '@' rest)

-- | Parse fields until the closing '}' of the enclosing entry.
--   Accepts @name = {value}@, @name = "value"@, or trailing commas.
parseFields :: String -> ([(String, String)], String)
parseFields = go
  where
    go s =
        let s' = dropWhile isSkippable s
        in case s' of
            [] -> ([], [])
            '}':rest -> ([], rest)
            _ -> case parseField s' of
                Nothing         -> ([], s')   -- malformed; stop collecting
                Just (nv, rest) ->
                    let (more, rest') = go rest
                    in (nv : more, rest')

    isSkippable c = isSpace c || c == ','

-- | Parse a single @name = value@ field.
parseField :: String -> Maybe ((String, String), String)
parseField s =
    let (name, r1) = span (\c -> isAlphaNum c || c == '_') (dropWhile isSpace s)
        r2         = dropWhile isSpace r1
    in case r2 of
        '=':r3 -> do
            let r4 = dropWhile isSpace r3
            (value, r5) <- readFieldValue r4
            return ((map toLower (trim name), value), r5)
        _ -> Nothing

-- | Read a field's value, honoring nested braces and quoted forms.
readFieldValue :: String -> Maybe (String, String)
readFieldValue ('{':rest) = Just (readBraces 1 "" rest)
readFieldValue ('"':rest) = Just (readQuote  ""    rest)
readFieldValue _          = Nothing

-- | Read characters up to the matching @}@ that closes the outermost
--   @{@; preserves interior @{@ / @}@ pairs as part of the value.
readBraces :: Int -> String -> String -> (String, String)
readBraces 0 acc r        = (reverse acc, r)
readBraces _ acc []       = (reverse acc, [])
readBraces 1 acc ('}':r)  = (reverse acc, r)          -- outer close
readBraces n acc ('{':r)  = readBraces (n + 1) ('{' : acc) r
readBraces n acc ('}':r)  = readBraces (n - 1) ('}' : acc) r
readBraces n acc (c:r)    = readBraces n (c : acc) r

-- | Read characters up to the closing @"@.
readQuote :: String -> String -> (String, String)
readQuote acc ('"':r) = (reverse acc, r)
readQuote acc []      = (reverse acc, [])
readQuote acc (c:r)   = readQuote (c : acc) r

-- | Build a 'BibExtra' from the parsed fields list.
toExtra :: [(String, String)] -> BibExtra
toExtra flds = BibExtra
    { bibFile     = lookup "file" flds
    , bibKeywords = case lookup "keywords" flds of
        Nothing -> []
        Just s  -> filter (not . null) (map trim (splitOn ',' s))
    , bibAuthor   = lookup "author" flds
    , bibYear     = lookup "year" flds
    }
