{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Which image files under @content/blog/@ a page of the blog shows.
--
--   A blog entry lives among its neighbors (@content/blog/first.md@), so
--   an image beside it has no directory to say whose it is. What says so
--   is the page that names it: the build publishes an image that a
--   published entry or collection landing refers to by a relative path,
--   and withholds every other ("Drafts"). An image only a draft shows is
--   therefore withheld with the draft, whatever it is called, and one a
--   draft and a published entry both show stays public.
--
--   A reference is read from the page's Markdown as the compilers read it
--   ('PandocOptions.readerOpts'): an image, a link (to the full-size
--   file, say), or a URL attribute of raw HTML (@\<img src>@,
--   @\<source srcset>@). The source preprocessors ("Filters") write no
--   image and are not run. A reference rooted at the site
--   (@\/notebook\/x.png@) or carrying a host is not a relative path and
--   names nothing here.
module BlogImages
    ( imageExtensions
    , isImage
    , isMonogram
    , referencedFiles
    , imageInputs
    ) where

import           Data.Char              (digitToInt, isHexDigit, toLower)
import           Data.List              (isSuffixOf, nub)
import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.Encoding.Error as TEE
import           System.FilePath        (replaceExtension, takeExtension, takeFileName)
import           Text.Pandoc            (readMarkdown, runPure)
import           Text.Pandoc.Definition (Block (..), Format (..), Inline (..))
import           Text.Pandoc.Walk       (query)

import           HtmlUrls               (isRelative, resolveAgainst, urlsIn)
import           PandocOptions          (readerOpts)

-- | The extensions of the image files a blog page may show, in lower
--   case; a file's own may be in either ('isImage'). A WebP companion's
--   (tools/convert-images.sh) is one of them.
imageExtensions :: [String]
imageExtensions = ["avif", "gif", "jpeg", "jpg", "png", "svg", "webp"]

isImage :: FilePath -> Bool
isImage p = map toLower (drop 1 (takeExtension p)) `elem` imageExtensions

-- | A monogram ("Marks") is read into a page and never published.
isMonogram :: FilePath -> Bool
isMonogram p = takeFileName p == "mark.svg" || ".mark.svg" `isSuffixOf` p

-- | The files a page's Markdown body refers to by relative path, from
--   the page's directory (@dir@, with no trailing slash). The path is
--   taken as written and with its percent-escapes read, so a file with a
--   space in its name is found under either spelling. A body that does
--   not parse refers to nothing.
referencedFiles :: FilePath -> String -> [FilePath]
referencedFiles dir body = nub
    [ resolveAgainst (dir ++ "/") path | url <- namedUrls body, isRelative url, path <- spellings url ]

-- | Every URL a body's Markdown names: an image's, a link's, and those in
--   the URL attributes of raw HTML.
namedUrls :: String -> [String]
namedUrls body = case runPure (readMarkdown readerOpts (T.pack body)) of
    Left _    -> []
    Right doc -> query inline doc ++ query block doc
  where
    inline (Image _ _ (url, _))            = [T.unpack url]
    inline (Link _ _ (url, _))             = [T.unpack url]
    inline (RawInline (Format "html") raw) = urlsIn (T.unpack raw)
    inline _                               = []
    block (RawBlock (Format "html") raw)   = urlsIn (T.unpack raw)
    block _                                = []

-- | A URL's path, as written and with its percent-escapes read.
spellings :: String -> [String]
spellings url = filter (not . null) (nub [bare, unescape bare])
  where bare = takeWhile (`notElem` ("?#" :: String)) url

-- | What "Filters.Images" reads from disk for an image a page shows, and
--   so what the page is built from besides its own source: the WebP
--   companion of a raster image, whose presence decides between
--   @\<picture>@ and @\<img>@, and the dimension sidecar.
imageInputs :: FilePath -> [FilePath]
imageInputs image =
    [ replaceExtension image ".webp" | map toLower (takeExtension image) `elem` [".jpg", ".jpeg", ".png", ".gif"] ]
    ++ [ image ++ ".dims.yaml" ]

-- | A path with its percent-escapes read as UTF-8.
unescape :: String -> String
unescape = T.unpack . TE.decodeUtf8With TEE.lenientDecode . BS.pack . bytes
  where
    bytes ('%' : a : b : rest)
        | isHexDigit a && isHexDigit b = fromIntegral (digitToInt a * 16 + digitToInt b) : bytes rest
    bytes (c : rest) = BS.unpack (TE.encodeUtf8 (T.singleton c)) ++ bytes rest
    bytes []         = []
