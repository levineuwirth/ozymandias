{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Filters.CodeRefs — tag body links to GitHub code that has a
--   build-time snapshot, so the hover popup can show the code itself.
--
--   @tools/code-refs.py fetch@ (run by @make build@ before the compile)
--   snapshots every GitHub @blob@ / @tree@ / @commit@ link in
--   @content/@ into @code-refs/@ and writes @code-refs/index.json@,
--   keyed by the link URL without its fragment. For each 'Link' whose
--   URL is in that index, this filter adds the @data-code-*@ attributes
--   that @static/js/popups.js@ ('codeRefContent') reads:
--
--     * @data-code-ref@    — @blob@, @tree@ or @commit@
--     * @data-code-src@    — the snapshot, same-origin under @/code-refs/@
--     * @data-code-repo@, @data-code-sha@, @data-code-path@, @data-code-date@
--     * @data-code-branch@ — only on a branch link, whose popup must say
--       which revision it shows
--
--   The link itself is untouched — href, classes, and the external-link
--   treatment 'Filters.Links' gives it afterwards. The fragment stays on
--   the href: GitHub honours it on click-through, and the popup reads it
--   to choose the excerpt.
--
--   The index is read once per process ('unsafePerformIO' CAF, the same
--   arrangement as "ArchiveIndex"). Absent or malformed, every link
--   passes through and GitHub links keep the repository-card popup.
--   Under @site watch@ a regenerated index is not re-read until restart;
--   and a page whose link failed to fetch keeps its untagged link until
--   the page next recompiles (bounded by build-freshness.sh's periodic
--   full rebuild).
module Filters.CodeRefs (apply) where

import qualified Data.Aeson             as A
import           Data.Aeson             ((.:), (.:?), (.!=))
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text              as T
import           System.Directory       (doesFileExist)
import           System.IO.Unsafe       (unsafePerformIO)
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk       (walk)

data CodeRef = CodeRef
    { crKind   :: Text
    , crOwner  :: Text
    , crRepo   :: Text
    , crRef    :: Text
    , crSha    :: Text
    , crPath   :: Text
    , crPinned :: Bool
    , crSrc    :: Text
    , crDate   :: Text
    }

instance A.FromJSON CodeRef where
    parseJSON = A.withObject "CodeRef" $ \o -> CodeRef
        <$> o .:  "kind"
        <*> o .:  "owner"
        <*> o .:  "repo"
        <*> o .:  "ref"
        <*> o .:  "sha"
        <*> (o .:? "path"   .!= "")
        <*> (o .:? "pinned" .!= True)
        <*> o .:  "src"
        <*> (o .:? "date"   .!= "")

indexPath :: FilePath
indexPath = "code-refs/index.json"

{-# NOINLINE codeIndex #-}
codeIndex :: Map Text CodeRef
codeIndex = unsafePerformIO $ do
    exists <- doesFileExist indexPath
    if not exists
        then pure Map.empty
        else either (const Map.empty) id <$> A.eitherDecodeFileStrict' indexPath

-- | The index key for a link URL: fragment and query dropped, trailing
--   slash dropped — the same normalisation the fetcher applies.
indexKey :: Text -> Text
indexKey = T.dropWhileEnd (== '/') . T.takeWhile (\c -> c /= '#' && c /= '?')

apply :: Pandoc -> Pandoc
apply doc
    | Map.null codeIndex = doc
    | otherwise          = walk tag doc

tag :: Inline -> Inline
tag l@(Link (ident, classes, kvs) ils (url, title))
    | Nothing <- lookup "data-code-ref" kvs
    , Just ref <- Map.lookup (indexKey url) codeIndex =
        Link (ident, classes, kvs ++ attrs ref) ils (url, title)
    | otherwise = l
tag x = x

attrs :: CodeRef -> [(Text, Text)]
attrs r =
    [ ("data-code-ref",    crKind r)
    , ("data-code-src",    crSrc r)
    , ("data-code-repo",   crOwner r <> "/" <> crRepo r)
    , ("data-code-sha",    crSha r)
    , ("data-code-path",   crPath r)
    , ("data-code-date",   crDate r)
    ]
    ++ if crPinned r then [] else [("data-code-branch", crRef r)]
