{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Each page's Backlinks and Related entries, one file per page under
-- @data/footer/@, so that a page recompiles when its own footer changes and
-- not when anyone else's does (audit H01).
--
-- Hakyll has no early cutoff: a page that loads @data/backlinks.json@
-- recompiles whenever that item is recompiled, and the item is recompiled
-- whenever any page changes, because it is built from every page's links.
-- @data/similar-links.json@ is rewritten by @tools/embed.py@ whenever any
-- embedding moves, which almost any prose edit does. With every page
-- depending on both (as H01 requires, or a page never gets its first
-- backlink or Related section incrementally), every prose edit recompiled
-- every page twice: once for each file.
--
-- A file on disk is the one thing whose staleness Hakyll judges by content
-- history rather than by its inputs: it changes only when it is rewritten.
-- So @site footer-data@ ('writeFooterData', run by the Makefile between
-- the two compile passes) splits both maps into one file per page and
-- rewrites a file only when its bytes change. A prose edit then recompiles
-- the pages whose Backlinks or Related actually changed, usually none or
-- two. Scores are dropped and Related is cut to what is rendered, so a
-- shifted score or a change below the top 'maxRelated' rewrites nothing.
--
-- Every page records a dependency on its own file through
-- 'footerDepsField', whether or not the file exists yet: Hakyll treats an
-- identifier that appears as new, and so out of date, and that makes the
-- page out of date too. A file whose page loses all its entries is emptied,
-- never deleted, because Hakyll does not notice an identifier going away.
--
-- The files hold the last production build's data: the first compile pass
-- renders footers from the previous build, and the second pass corrects
-- them. Dev builds and @make watch@ read the files but never write them, so
-- a draft's links cannot reach a published page's Backlinks.
module FooterData
    ( footerRules
    , footerDepsField
    , footerEntries
    , footerKey
    , maxRelated
    , normaliseUrl
    , writeFooterData
    ) where

import           Control.Exception          (IOException, try)
import           Control.Monad              (forM, forM_, unless)
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Key             as Key
import qualified Data.Aeson.KeyMap          as KeyMap
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.Map.Strict            as Map
import           Data.Map.Strict            (Map)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Encoding.Error   as TE
import           System.Directory           (createDirectoryIfMissing,
                                             doesFileExist, listDirectory,
                                             renameFile)
import           System.FilePath            ((</>), takeExtension)
import           Text.Printf                (printf)

import           Hakyll
import           Hakyll.Core.Compiler.Internal (compilerAsk, compilerUniverse,
                                                compilerTellDependencies)

-- | Where the files live; matched by 'footerRules'.
footerDir :: FilePath
footerDir = "data/footer"

-- | Related entries rendered per page. The split keeps only these, so a
-- change further down the list rewrites nothing.
maxRelated :: Int
maxRelated = 3

-- ---------------------------------------------------------------------------
-- Keys
-- ---------------------------------------------------------------------------

-- | Normalise an internal URL as a map key: strip query string and
-- fragment; ensure a leading slash; strip a trailing @index.html@
-- (keeping the directory slash) before the bare @.html@ extension, so a
-- page routed @essays\/foo\/index.html@ and a link to @\/essays\/foo\/@
-- share a key; percent-decode the path so that @\/essays\/caf%C3%A9@ and
-- @\/essays\/café@ do too.
--
-- The one normaliser for both sides of every footer join: link targets in
-- 'Backlinks.targetKey', @tools/embed.py@'s URLs in 'writeFooterData', and
-- each page's own route in 'footerKey'.
normaliseUrl :: String -> String
normaliseUrl url =
    let t  = T.pack url
        t1 = fst (T.breakOn "?" (fst (T.breakOn "#" t)))
        t2 = if T.isPrefixOf "/" t1 then t1 else "/" `T.append` t1
        t3 = fromMaybe t2 (T.stripSuffix "index.html" t2)
        t4 = fromMaybe t3 (T.stripSuffix ".html" t3)
    in  percentDecode (T.unpack t4)

-- | Decode percent-escapes (@%XX@) into raw bytes, then re-interpret the
-- resulting bytestring as UTF-8. Invalid escapes are passed through
-- verbatim so this is safe to call on already-decoded input.
percentDecode :: String -> String
percentDecode = T.unpack . TE.decodeUtf8With TE.lenientDecode . BS.pack . go
  where
    go []                 = []
    go ('%':a:b:rest)
        | Just hi <- hexDigit a
        , Just lo <- hexDigit b
        = fromIntegral (hi * 16 + lo) : go rest
    -- Literal Unicode is already decoded text, not one byte per Char.
    -- Encode it before combining it with any percent-decoded UTF-8 bytes.
    go (c:rest)           = BS.unpack (TE.encodeUtf8 (T.singleton c)) ++ go rest

    hexDigit c
        | c >= '0' && c <= '9' = Just (fromEnum c - fromEnum '0')
        | c >= 'a' && c <= 'f' = Just (fromEnum c - fromEnum 'a' + 10)
        | c >= 'A' && c <= 'F' = Just (fromEnum c - fromEnum 'A' + 10)
        | otherwise            = Nothing

-- | The key of the page at a route.
footerKey :: String -> String
footerKey r = normaliseUrl ("/" ++ r)

-- | The file holding a key's entries: the key's UTF-8 bytes with everything
-- but letters, digits, @-@, @_@ and @.@ percent-encoded, so that every key
-- maps to its own flat file name (@\/essays\/foo\/@ is
-- @%2Fessays%2Ffoo%2F.json@).
footerFile :: String -> FilePath
footerFile key = footerDir </> (concatMap enc (BS.unpack (TE.encodeUtf8 (T.pack key))) ++ ".json")
  where
    enc b
        | plain c   = [c]
        | otherwise = printf "%%%02X" b
      where c = toEnum (fromIntegral b) :: Char
    plain c = isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` ("-_." :: String)

footerIdentifier :: String -> Identifier
footerIdentifier = fromFilePath . footerFile

-- ---------------------------------------------------------------------------
-- Hakyll side
-- ---------------------------------------------------------------------------

footerRules :: Rules ()
footerRules = match (fromGlob (footerDir ++ "/*.json")) $ compile getResourceBody

-- | Renders nothing, and records this page's dependency on its footer file.
-- Each template that renders Backlinks or Related evaluates it once, as
-- @$if(footer-deps)$$endif$@.
--
-- The dependency recorded inside 'footerEntries' is not enough: on a page
-- with no entries the field fails, so that @$if(…)$@ hides the section,
-- and Hakyll discards everything a failed field recorded. Such a page
-- would never depend on its file, and never get its first Related section
-- or backlink until a full rebuild (audit H01). Recorded directly rather
-- than through 'load', which refuses an identifier that does not exist
-- yet; and an identifier dependency costs nothing in the out-of-date
-- check, where a pattern is re-matched against every identifier.
footerDepsField :: Context a
footerDepsField = field "footer-deps" $ \item -> do
    mRoute <- getRoute (itemIdentifier item)
    forM_ mRoute $ \r ->
        compilerTellDependencies [IdentifierDependency (footerIdentifier (footerKey r))]
    return ""

-- | One part (@"backlinks"@ or @"related"@) of the page's entries; empty
-- when the page has no file.
footerEntries :: Aeson.FromJSON v => String -> Item a -> Compiler [v]
footerEntries part item = do
    mRoute <- getRoute (itemIdentifier item)
    case mRoute of
        Nothing -> return []
        Just r  -> do
            let ident = footerIdentifier (footerKey r)
            universe <- compilerUniverse <$> compilerAsk
            if ident `Set.notMember` universe
                then return []
                else do
                    body <- loadBody ident :: Compiler String
                    case Aeson.eitherDecodeStrict (TE.encodeUtf8 (T.pack body)) of
                        Left err  -> fail ("footer data " ++ toFilePath ident ++ ": " ++ err)
                        Right obj -> case Aeson.fromJSON <$> KeyMap.lookup (Key.fromString part) obj of
                            Nothing                -> return []
                            Just (Aeson.Success v) -> return v
                            Just (Aeson.Error err) -> fail ("footer data " ++ toFilePath ident ++ ": " ++ err)

-- ---------------------------------------------------------------------------
-- The split (@site footer-data@)
-- ---------------------------------------------------------------------------

-- | Split @<site>/data/backlinks.json@ (written by the first compile pass)
-- and @data/similar-links.json@ (written by @tools/embed.py@) into the
-- per-page files. A missing input counts as empty. Prints how many files
-- it rewrote, which is how many pages the second pass will recompile for
-- their footers.
writeFooterData :: FilePath -> IO ()
writeFooterData siteDir = do
    backlinks <- readMap (siteDir </> "data/backlinks.json")
    similar   <- readMap "data/similar-links.json"
    let related = Map.mapKeys (normaliseUrl . T.unpack) similar
        keys    = Set.union (Set.fromList (map T.unpack (Map.keys backlinks)))
                            (Map.keysSet related)
        entry k = Aeson.object
            [ "backlinks" Aeson..= Map.findWithDefault [] (T.pack k) backlinks
            , "related"   Aeson..= map dropScore (take maxRelated (Map.findWithDefault [] k related))
            ]
    createDirectoryIfMissing True footerDir
    -- A file whose key has no entries any more is emptied, not deleted.
    existing <- filter ((== ".json") . takeExtension) <$> listDirectory footerDir
    let wanted   = Map.fromList [ (footerFile k, entry k) | k <- Set.toList keys ]
        emptied  = Map.fromList [ (footerDir </> f, Aeson.object ["backlinks" Aeson..= noEntries, "related" Aeson..= noEntries])
                                | f <- existing ]
        files    = Map.union wanted emptied
    changed <- forM (Map.toList files) $ \(path, v) -> writeIfChanged path (BL.toStrict (Aeson.encode v) <> "\n")
    printf "footer-data: %d pages, %d rewritten\n" (Map.size files) (length (filter id changed))
  where
    noEntries = [] :: [Aeson.Value]
    dropScore (Aeson.Object o) = Aeson.Object (KeyMap.delete "score" o)
    dropScore v                = v

readMap :: FilePath -> IO (Map T.Text [Aeson.Value])
readMap path = do
    exists <- doesFileExist path
    if not exists then return Map.empty else do
        r <- Aeson.eitherDecodeFileStrict path
        either (\e -> ioError (userError ("footer-data: " ++ path ++ ": " ++ e))) return r

-- | Write through a temporary file, and not at all when the bytes are
-- already there: an untouched file keeps its mtime, which is all Hakyll
-- looks at.
writeIfChanged :: FilePath -> BS.ByteString -> IO Bool
writeIfChanged path bytes = do
    old <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
    let same = either (const False) (== bytes) old
    unless same $ do
        let tmp = path ++ ".tmp"
        BS.writeFile tmp bytes
        renameFile tmp path
    return (not same)
