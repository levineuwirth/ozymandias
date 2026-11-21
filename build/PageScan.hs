{-# LANGUAGE GHC2021 #-}
-- | The published Markdown pages under a content root, and the links on
--   each as Pandoc reads them, for tools that must agree with the build
--   about both (@site list-links ROOT@). tools/code-refs.py snapshots the
--   GitHub links it is given here: it used to scan raw Markdown with a
--   regex, which also took URLs in code blocks and prose that no
--   'Filters.CodeRefs' tag would ever use, and it kept its own copy of
--   the private-name and draft rules to decide which pages count.
--
--   And the files every page names, published or not (@site list-named
--   SUFFIX ROOT@), for tools/convert-images.sh: a WebP a page shows by
--   name is an image of its own, which the converter must not write over
--   with another image's companion.
module PageScan
    ( publishedPages
    , pageLinks
    , namedFiles
    ) where

import qualified Data.ByteString                as BS
import           Data.Char                      (chr, digitToInt, isAlphaNum, isHexDigit, toLower)
import           Data.List                      (isSuffixOf)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Data.Text.Encoding.Error       as TEE
import           Hakyll.Core.Provider.Metadata  (parsePage)
import           System.FilePath                (makeRelative, normalise, takeDirectory, takeExtension)
import           Text.Pandoc                    (readMarkdown, runPure)
import           Text.Pandoc.Definition         (Inline (..))
import           Text.Pandoc.Walk               (query)

import           Compilers                      (readerOpts)
import           Config                         (SiteConfig, switchOn)
import           PandocOptions                  (withMath)
import           BlogImages                     (referencedFiles, rootedFiles)
import           Drafts                         (filesUnder, isUnpublished, markdownUnder, scanUnpublished)
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
            let opts = withMath (switchOn cfg "math") readerOpts
            in case runPure (readMarkdown opts (T.pack (preprocessSource cfg body))) of
                Left _    -> []
                Right doc -> query target doc
  where
    target (Link _ _ (url, _)) = [url]
    target _                   = []

-- | Each file whose name ends in @suffix@ (in either case) that a Markdown
--   page under @root@ names, with the page: one it names by a relative
--   path as that path from where @root@ is given
--   (@content\/blog\/lake.webp@), one it names from the site's root as
--   @static:@ and the path there (@static:images\/lake.webp@). Every page
--   counts, a draft's and those under @root\/drafts\/@ too: a dev build
--   shows them, and their images are no less their own. The body is read
--   as "BlogImages" reads a blog page's.
--
--   Reading every page costs seconds on a site of a few hundred, so a
--   page is read only if its source could spell the suffix: it holds the
--   suffix's last letters and digits as they are, or one of the two
--   things that write a letter another way: a percent-escape of one of
--   them, or a numeric character reference.
namedFiles :: String -> FilePath -> IO [(FilePath, FilePath)]
namedFiles suffix root = do
    pages <- filter ((== ".md") . takeExtension) <$> filesUnder root
    fmap concat $ mapM named pages
  where
    wanted file = lower suffix `isSuffixOf` lower file
    lower = map toLower
    ending = T.pack (reverse (takeWhile isAlphaNum (reverse (lower suffix))))
    couldSpell src =
           T.null ending
        || ending `T.isInfixOf` T.toLower src
        || T.pack "&#" `T.isInfixOf` src
        || any escape (T.breakOnAll (T.pack "%") src)
    -- A percent-escape of one of those letters or digits.
    escape (_, rest) = case T.unpack (T.take 2 (T.drop 1 rest)) of
        [a, b] | isHexDigit a && isHexDigit b ->
            toLower (chr (digitToInt a * 16 + digitToInt b)) `T.elem` ending
        _ -> False
    named fp = do
        src <- TE.decodeUtf8With TEE.lenientDecode <$> BS.readFile fp
        if not (couldSpell src) then return [] else do
            let text = T.unpack src
                body = either (const text) snd (parsePage text)
                page = normalise fp
            return $  [ (file, page) | file <- referencedFiles (takeDirectory page) body, wanted file ]
                   ++ [ ("static:" ++ file, page) | file <- rootedFiles body, wanted file ]
