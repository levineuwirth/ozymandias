{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | @draft: true@ in a page's front matter: unpublished in production.
--
-- The flag used to be honored only on poetry and fiction collection
-- landings; anywhere else it was silently ignored and the piece shipped
-- with its feed, sitemap and search entries (audit C06). Now a production
-- build withholds it in two places, together airtight:
--
--   * 'Patterns' excludes 'unpublishedPattern' from every content pattern,
--     so the lists Hakyll builds while the rules run — tags, authors,
--     pagination, keyword maps — never contain it;
--   * 'withoutUnpublished' drops every compiler and resource for it from
--     the finished rule set, so no rule (a literal glob, an asset copy)
--     can route it, and a compile-time @loadAll@, which only sees what
--     rules claimed, cannot find it.
--
-- (Hakyll's @ignoreFile@ would be the natural place, but it is given bare
-- file names, and a draft is known by its path.) @SITE_ENV=dev@ builds
-- ignore the flag, as they include @content/drafts/@.
--
-- What the flag withholds:
--
--   * the file itself — essay, poem, story, blog post, composition,
--     photograph, standalone or collection page;
--   * for a directory-form entry (@content/{essays,music,photography}/<slug>/index.md@),
--     its whole directory: figures, scores, recordings, a series' frames;
--   * for a flat photograph (@content/photography/<slug>.md@), the image
--     files that share its stem or its @photo:@ field's stem.
--
-- A poetry, fiction or blog collection's @index.md@ withholds only that
-- landing page; the pieces inside publish on their own flags.
--
-- Deliberately separate from @status: Draft@ (the epistemic state "Marks"
-- reads) and from @content/drafts/@ (a directory of unfinished essays).
module Drafts
    ( Unpublished
    , noneUnpublished
    , scanUnpublished
    , currentUnpublished
    , isUnpublished
    , unpublishedPattern
    , withoutUnpublished
    , unpublishedSummary
    , markdownUnder
    ) where

import           Control.Exception    (IOException, try)
import           Control.Monad        (filterM, forM)
import           Control.Monad.Writer.Class (censor)
import qualified Data.Aeson           as A
import qualified Data.Aeson.KeyMap    as KM
import qualified Data.ByteString.Char8 as BS
import           Data.Char            (isSpace)
import           Data.List            (isPrefixOf, sort)
import           Data.Maybe           (catMaybes, listToMaybe)
import qualified Data.Set             as Set
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Yaml            as Y
import           System.Directory     (doesDirectoryExist, listDirectory)
import           System.FilePath      (dropExtension, makeRelative, normalise,
                                       splitDirectories, takeDirectory,
                                       takeExtension, takeFileName, (</>))
import           System.IO.Unsafe     (unsafePerformIO)
import           Hakyll.Core.Provider.Metadata (parsePage)
import           Utils                (parseBool)
import           Hakyll               (Pattern, Rules, fromFilePath, fromGlob, fromList,
                                       toFilePath, (.||.))
import           Hakyll.Core.Rules.Internal (RuleSet (..), Rules (..))
import           Utils                (isDevBuild)

data Unpublished = Unpublished
    { upFiles :: Set.Set FilePath   -- ^ the flagged files themselves
    , upDirs  :: [FilePath]         -- ^ directories withheld whole
    , upStems :: [FilePath]         -- ^ @dir/stem.@ prefixes of a flat photo's images
    }

noneUnpublished :: Unpublished
noneUnpublished = Unpublished Set.empty [] []

-- | Walk @root@ (normally @content@) and collect every flagged file.
-- @content/drafts/@ is skipped: nothing there is published anyway.
scanUnpublished :: FilePath -> IO Unpublished
scanUnpublished root = do
    mds <- markdownUnder root
    flagged <- catMaybes <$> forM mds (\fp -> fmap (fp,) <$> draftFlag fp)
    let files = [fp | (fp, _) <- flagged]
        dirs  = [takeDirectory fp | (fp, _) <- flagged, isDirectoryEntry fp]
        stems = concat [flatPhotoStems fp photo | (fp, photo) <- flagged, isFlatPhoto fp]
    return (Unpublished (Set.fromList (map norm files)) (map norm dirs) (map norm stems))
  where
    isDirectoryEntry fp = case splitDirectories (makeRelative root fp) of
        [section, _, "index.md"] -> section `elem` ["essays", "music", "photography"]
        _                                   -> False
    isFlatPhoto fp = case splitDirectories (makeRelative root fp) of
        ["photography", name] -> name /= "index.md"
        _                                -> False
    flatPhotoStems fp photo =
        let dir = takeDirectory fp
        in  (dir </> dropExtension (takeFileName fp) ++ ".")
            : [dir </> dropExtension (takeFileName p) ++ "." | Just p <- [photo]]

-- | What this process withholds: the scan of @content/@ in a production
-- build, nothing under @SITE_ENV=dev@. Read once, when first needed;
-- 'Patterns' are constants, so the set must be one too.
{-# NOINLINE currentUnpublished #-}
currentUnpublished :: Unpublished
currentUnpublished = unsafePerformIO $ do
    dev <- isDevBuild
    if dev then return noneUnpublished else scanUnpublished "content"

-- | The withheld files as a Hakyll pattern, for 'Patterns' to exclude.
unpublishedPattern :: Unpublished -> Pattern
unpublishedPattern up =
    foldr (.||.) (fromList (map fromFilePath (Set.toList (upFiles up))))
        (  [fromGlob (d ++ "/**") | d <- upDirs up]
        ++ [fromGlob (s ++ "*")   | s <- upStems up] )

-- | Drop every compiler and resource for a withheld file from the rules'
-- output. Uses Hakyll's exposed 'Hakyll.Core.Rules.Internal'; a Hakyll
-- release that changes 'RuleSet' fails to compile here rather than
-- publishing a draft.
withoutUnpublished :: Unpublished -> Rules a -> Rules a
withoutUnpublished up (Rules m) = Rules (censor strip m)
  where
    withheld ident = isUnpublished up (toFilePath ident)
    strip rs = rs
        { rulesCompilers = filter (not . withheld . fst) (rulesCompilers rs)
        , rulesResources = Set.filter (not . withheld) (rulesResources rs)
        }

-- | Is this path withheld? Paths are relative to the project root
-- ("content/essays/foo.md").
isUnpublished :: Unpublished -> FilePath -> Bool
isUnpublished up path =
       p `Set.member` upFiles up
    || any (\d -> p == d || (d ++ "/") `isPrefixOf` p) (upDirs up)
    || any (`isPrefixOf` p) (upStems up)
  where
    p = norm path

-- | One line per withheld file, directory and image stem, sorted; for
-- @site list-unpublished@ and the tests.
unpublishedSummary :: Unpublished -> [String]
unpublishedSummary up = sort $
       ["file " ++ f | f <- Set.toList (upFiles up)]
    ++ ["dir "  ++ d | d <- upDirs up]
    ++ ["stem " ++ s | s <- upStems up]

-- ---------------------------------------------------------------------------

norm :: FilePath -> FilePath
norm fp = case normalise fp of
    '.' : '/' : rest -> rest
    other            -> other

-- | Every Markdown file under @root@, sorted, outside dot-directories and
-- @root/drafts/@.
markdownUnder :: FilePath -> IO [FilePath]
markdownUnder root = go root
  where
    go dir = do
        r <- try (listDirectory dir) :: IO (Either IOException [FilePath])
        case r of
            Left _      -> return []
            Right names -> do
                let paths = [dir </> n | n <- sort names, take 1 n /= "."]
                    rel   = makeRelative root
                dirs <- filterM doesDirectoryExist paths
                let files = [p | p <- paths, p `notElem` dirs, takeExtension p == ".md"]
                    keep  = [d | d <- dirs, rel d /= "drafts"]
                sub <- concat <$> mapM go keep
                return (files ++ sub)

-- | @Just photo@ (the @photo:@ field, if any) when the file's front matter
-- sets @draft@ to true; @Nothing@ otherwise, including for a file without
-- front matter or with front matter that does not parse.
draftFlag :: FilePath -> IO (Maybe (Maybe FilePath))
draftFlag fp = do
    r <- try (BS.readFile fp) :: IO (Either IOException BS.ByteString)
    return $ case r of
        Left _    -> Nothing
        Right src -> listToMaybe
            [ photoOf o | o <- frontMatterReadings src, truthy (KM.lookup "draft" o) ]
  where
    photoOf o = case KM.lookup "photo" o of
        Just (A.String t) -> Just (T.unpack t)
        _                 -> Nothing
    truthy (Just (A.Bool b))   = b
    truthy (Just (A.Number n)) = n == 1
    -- YAML 1.1 readers (PyYAML, the tests) take yes/on as true; accept the
    -- same spellings when they arrive here as strings.
    truthy (Just (A.String t)) = parseBool (T.unpack t) == Just True
    truthy _                   = False

-- | The front matter as Hakyll reads it ('parsePage': what the build will
-- see), and as 'frontMatter' reads it. A draft flag in either withholds
-- the page. Hakyll's alone missed nothing the build would publish as a
-- draft, but the lenient reading also catches an opening fence with
-- trailing whitespace, which Hakyll does not take as front matter: it
-- would render the YAML as the page's body, and a draft must not ship
-- that way either. Before 2026-10-04 only the lenient reading counted,
-- and it missed fences longer than three dashes, which Hakyll accepts.
frontMatterReadings :: BS.ByteString -> [A.Object]
frontMatterReadings src = catMaybes [hakyll, lenient]
  where
    hakyll = case parsePage (T.unpack (TE.decodeUtf8With TEE.lenientDecode src)) of
        Right (meta, _) -> Just meta
        Left _          -> Nothing
    lenient = do
        block <- frontMatter src
        case Y.decodeEither' block of
            Right (A.Object o) -> Just o
            _                  -> Nothing

-- | The YAML between a leading @---@ line and the next @---@ or @...@
-- line, each allowed trailing whitespace.
frontMatter :: BS.ByteString -> Maybe BS.ByteString
frontMatter src = case BS.lines src of
    (first : rest) | strip first == "---" ->
        let body = takeWhile (\l -> strip l `notElem` ["---", "..."]) rest
        in  if length body < length rest then Just (BS.unlines body) else Nothing
    _ -> Nothing
  where
    strip = BS.dropWhileEnd isSpace
