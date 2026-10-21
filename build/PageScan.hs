{-# LANGUAGE GHC2021 #-}
-- | The published Markdown pages under a content root, and the links on
--   each as Pandoc reads them, for tools that must agree with the build
--   about both (@site list-links ROOT@). tools/code-refs.py snapshots the
--   GitHub links it is given here: it used to scan raw Markdown with a
--   regex, which also took URLs in code blocks and prose that no
--   'Filters.CodeRefs' tag would ever use, and it kept its own copy of
--   the private-name and draft rules to decide which pages count.
module PageScan
    ( publishedPages
    , pageLinks
    ) where

import qualified Data.ByteString                as BS
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Data.Text.Encoding.Error       as TEE
import           Hakyll.Core.Provider.Metadata  (parsePage)
import           System.FilePath                (makeRelative)
import           Text.Pandoc                    (readMarkdown, runPure)
import           Text.Pandoc.Definition         (Inline (..))
import           Text.Pandoc.Walk               (query)

import           Compilers                      (readerOpts)
import           Config                         (SiteConfig)
import           Drafts                         (isUnpublished, markdownUnder, scanUnpublished)
import           Filters                        (preprocessSource)
import           Site                           (refusedPath)

-- | Every Markdown page under @root@ a production build publishes: not
--   under @drafts/@, no path component the build refuses
--   ('Site.refusedPath'), not withheld by a draft flag ("Drafts").
publishedPages :: FilePath -> IO [FilePath]
publishedPages root = do
    up  <- scanUnpublished root
    mds <- markdownUnder root
    return [ fp | fp <- mds
                , not (refusedPath (makeRelative root fp))
                , not (isUnpublished up fp) ]

-- | The target of every link in a page's body, read as the compilers read
--   it (Hakyll's split, the source preprocessors, 'Compilers.readerOpts').
--   A page whose front matter or Markdown does not parse has none.
pageLinks :: SiteConfig -> FilePath -> IO [T.Text]
pageLinks cfg fp = do
    src <- T.unpack . TE.decodeUtf8With TEE.lenientDecode <$> BS.readFile fp
    return $ case parsePage src of
        Left _          -> []
        Right (_, body) ->
            case runPure (readMarkdown readerOpts (T.pack (preprocessSource cfg body))) of
                Left _    -> []
                Right doc -> query target doc
  where
    target (Link _ _ (url, _)) = [url]
    target _                   = []
