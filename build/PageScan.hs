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
--   ROOT SUFFIX...@), for tools/convert-images.sh and
--   tools/generate-thumbnails.py: a WebP a page shows by name is an image
--   of its own, which neither may write over with one it makes.
module PageScan
    ( publishedPages
    , pageLinks
    , namedFiles
    ) where

import qualified Data.ByteString                as BS
import           Data.Char                      (chr, digitToInt, isAlphaNum, isHexDigit, toLower)
import           Data.List                      (isPrefixOf, isSuffixOf, stripPrefix)
import qualified Data.Aeson                     as A
import qualified Data.Aeson.KeyMap              as KM
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Data.Text.Encoding.Error       as TEE
import           Hakyll.Core.Provider.Metadata  (parsePage)
import           System.FilePath                (makeRelative, normalise, takeDirectory, takeExtension, (</>))
import           Text.Pandoc                    (readMarkdown, runPure)
import           Text.Pandoc.XML                (fromEntities)
import           Text.Pandoc.Definition         (Inline (..))
import           Text.Pandoc.Walk               (query)

import           Compilers                      (readerOpts)
import           Config                         (Section (..), SiteConfig (..), switchOn)
import           PandocOptions                  (withMath)
import           BlogImages                     (namedUrls, relativeFiles, rootedPaths)
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

-- | Each file whose name ends in one of @suffixes@ (in either case) that a
--   Markdown page under @root@ names, with the page.
--
--   A page names a file in its body, as "BlogImages" reads a blog page's
--   (an image, a link, a URL attribute of raw HTML), and in its front
--   matter: a field's value may be a file's name (@photo: harbor.webp@,
--   which "Contexts" takes from beside the page), so every string there is
--   read as a path would be.
--
--   A relative path is the file at that path from the page, under @root@
--   as it is given (@content\/blog\/lake.webp@). A path from the site's
--   root (@\/images\/lake.webp@) is whatever is served there, and the
--   site serves three trees at its root: @static\/@ (given here as
--   @static:images\/lake.webp@, the site's file or the engine's),
--   @content\/@ at the same path, and a section's directory at its
--   address ('Sections.contentRoute'). Each that could be meant is given;
--   whoever asks has the files and can tell which are there.
--
--   Every page counts, a draft's and those under @root\/drafts\/@ too: a
--   dev build shows them, and their images are no less their own.
--
--   Reading every body costs seconds on a site of a few hundred pages, so
--   a body is read only if it could spell one of the suffixes: it holds
--   each run of the suffix's letters and digits as they are, or one of
--   the things that write a letter another way, a percent-escape of one
--   of them or a character reference. The front matter is always read.
namedFiles :: SiteConfig -> [String] -> FilePath -> IO [(FilePath, FilePath)]
namedFiles cfg suffixes root = do
    pages <- filter ((== ".md") . takeExtension) <$> filesUnder root
    fmap concat $ mapM named pages
  where
    wanted file = any (\suffix -> lower suffix `isSuffixOf` lower file) suffixes
    lower = map toLower

    named fp = do
        src <- T.unpack . TE.decodeUtf8With TEE.lenientDecode <$> BS.readFile fp
        let (meta, body) = either (const (mempty, src)) id (parsePage src)
            page = normalise fp
            urls = strings (A.Object meta)
                   ++ (if any (couldSpell (T.pack body)) suffixes then namedUrls body else [])
            files = relativeFiles (takeDirectory page) urls ++ concatMap served (rootedPaths urls)
        return [ (file, page) | file <- files, wanted file ]

    -- Where the site serves a path from its root. A section's directory
    -- is served at its address and not, when that is another name, at its
    -- own.
    served path =
          ("static:" ++ path)
        : [ normalise (root </> path) | not (any (moved path) (siteSections cfg)) ]
        ++ [ normalise (root </> sectionDir s </> rest)
           | s <- siteSections cfg
           , Just rest <- [stripPrefix (sectionAddress s ++ "/") path] ]
    moved path s = sectionDir s /= sectionAddress s && (sectionDir s ++ "/") `isPrefixOf` path

    strings (A.String t) = [T.unpack t]
    strings (A.Array xs) = concatMap strings xs
    strings (A.Object o) = concatMap strings (KM.elems o)
    strings _            = []

    couldSpell body suffix = any spells [body, fromEntities body]
      where
        runs = filter (not . T.null) (T.split (not . isAlphaNum) (T.toLower (T.pack suffix)))
        letters = T.concat runs
        spells text =
               all (`T.isInfixOf` T.toLower text) runs
            || T.pack "&#" `T.isInfixOf` text
            || any escape (T.breakOnAll (T.pack "%") text)
        -- A percent-escape of one of those letters or digits.
        escape (_, rest) = case T.unpack (T.take 2 (T.drop 1 rest)) of
            [a, b] | isHexDigit a && isHexDigit b ->
                toLower (chr (digitToInt a * 16 + digitToInt b)) `T.elem` letters
            _ -> False
