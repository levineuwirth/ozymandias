{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Image filter: lazy loading, lightbox markers, and WebP <picture> wrappers.
--
--   For local raster images (JPG, JPEG, PNG, GIF) whose @.webp@ companion
--   exists on disk at build time, emits a @<picture>@ element with a WebP
--   @<source>@ and the original format as the @<img>@ fallback. When the
--   webp companion is absent (cwebp not installed, @convert-images.sh@ not
--   yet run, or a single file missed), the filter emits a plain @<img>@ so
--   the image still renders. This matters because browsers do NOT fall back
--   from a 404'd @<source>@ inside @<picture>@ to the nested @<img>@ — the
--   source is selected up front and a broken one leaves the area blank.
--
--   @tools/convert-images.sh@ produces the companion .webp files at build
--   time. When cwebp is not installed the script is a no-op, and this
--   filter degrades gracefully to plain @<img>@.
--
--   SVG files and external URLs are passed through with only lazy loading
--   (and lightbox markers for standalone images).
module Filters.Images (apply) where

import           Data.Char            (toLower)
import           Data.List            (isPrefixOf)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           System.Directory     (doesFileExist)
import           System.FilePath      (replaceExtension, takeExtension, (</>))
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk     (walkM)
import qualified Utils                as U

-- | Apply image attribute injection and WebP wrapping to the entire document.
--
--   @srcDir@ is the directory of the source Markdown file, used to resolve
--   relative image paths when probing for the corresponding @.webp@
--   companion file. Absolute paths (leading @/@) are resolved against
--   @static/@ instead, matching the layout @convert-images.sh@ writes to.
apply :: FilePath -> Pandoc -> IO Pandoc
apply srcDir = walkM (transformInline srcDir)

-- ---------------------------------------------------------------------------
-- Core transformation
-- ---------------------------------------------------------------------------

transformInline :: FilePath -> Inline -> IO Inline
transformInline srcDir (Link lAttr ils lTarget) = do
    -- Recurse into link contents; images inside a link get no lightbox marker.
    ils' <- mapM (wrapLinkedImg srcDir) ils
    pure (Link lAttr ils' lTarget)
transformInline srcDir (Image attr alt target) =
    renderImg srcDir attr alt target True
transformInline _ x = pure x

wrapLinkedImg :: FilePath -> Inline -> IO Inline
wrapLinkedImg srcDir (Image iAttr alt iTarget) =
    renderImg srcDir iAttr alt iTarget False
wrapLinkedImg _ x = pure x

-- | Dispatch on image type:
--   * Local raster with webp companion on disk → @<picture>@ with WebP @<source>@
--   * Local raster without companion → plain @<img>@ (graceful degradation)
--   * Everything else (SVG, URL) → plain @<img>@ with loading/lightbox attrs
renderImg :: FilePath -> Attr -> [Inline] -> Target -> Bool -> IO Inline
renderImg srcDir attr alt target@(src, _) lightbox
    | isLocalRaster (T.unpack src) = do
        hasWebp <- doesFileExist (webpPhysicalPath srcDir src)
        if hasWebp
            then pure $ RawInline (Format "html")
                                  (renderPicture attr alt target lightbox)
            else pure $ Image (addLightbox lightbox (addAttr "loading" "lazy" attr))
                              alt target
    | otherwise =
        pure $ Image (addLightbox lightbox (addAttr "loading" "lazy" attr)) alt target
  where
    addLightbox True  a = addAttr "data-lightbox" "true" a
    addLightbox False a = a

-- | Physical on-disk path of the @.webp@ companion for a Markdown image src.
--
--   Absolute paths (@/images/foo.jpg@) resolve under @static/@ because that
--   is where Hakyll's static-asset rule writes them from. Relative paths
--   resolve against the source file's directory, where Pandoc already
--   expects co-located assets to live.
webpPhysicalPath :: FilePath -> Text -> FilePath
webpPhysicalPath srcDir src =
    let s = T.unpack src
        physical = if "/" `isPrefixOf` s
                   then "static" ++ s
                   else srcDir </> s
    in replaceExtension physical ".webp"

-- ---------------------------------------------------------------------------
-- <picture> rendering
-- ---------------------------------------------------------------------------

-- | Emit a @<picture>@ element with a WebP @<source>@ and an @<img>@ fallback.
renderPicture :: Attr -> [Inline] -> Target -> Bool -> Text
renderPicture (ident, classes, kvs) alt (src, title) lightbox =
    T.concat
        [ "<picture>"
        , "<source srcset=\"", T.pack webpSrc, "\" type=\"image/webp\">"
        , "<img"
        , attrId ident
        , attrClasses classes
        , " src=\"",     esc src,                "\""
        , attrAlt alt
        , attrTitle title
        , " loading=\"lazy\""
        , if lightbox then " data-lightbox=\"true\"" else ""
        , renderKvs passedKvs
        , ">"
        , "</picture>"
        ]
  where
    webpSrc   = replaceExtension (T.unpack src) ".webp"
    -- Strip attrs we handle explicitly above (id/class/alt/title) and the
    -- attrs we always emit ourselves (loading, data-lightbox), so they don't
    -- appear twice on the <img>.
    passedKvs = filter
        (\(k, _) -> k `notElem`
            ["loading", "data-lightbox", "id", "class", "alt", "title", "src"])
        kvs

attrId :: Text -> Text
attrId t = if T.null t then "" else " id=\"" <> esc t <> "\""

attrClasses :: [Text] -> Text
attrClasses [] = ""
attrClasses cs = " class=\"" <> T.intercalate " " (map esc cs) <> "\""

attrAlt :: [Inline] -> Text
attrAlt ils = let t = stringify ils
              in  if T.null t then "" else " alt=\"" <> esc t <> "\""

attrTitle :: Text -> Text
attrTitle t = if T.null t then "" else " title=\"" <> esc t <> "\""

renderKvs :: [(Text, Text)] -> Text
renderKvs = T.concat . map (\(k, v) -> " " <> k <> "=\"" <> esc v <> "\"")

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | True for local (non-URL) images with a raster format we can convert.
isLocalRaster :: FilePath -> Bool
isLocalRaster src = not (isUrl src) && lowerExt src `elem` [".jpg", ".jpeg", ".png", ".gif"]

isUrl :: String -> Bool
isUrl s = any (`isPrefixOf` s) ["http://", "https://", "//", "data:"]

-- | Extension of a path, lowercased (e.g. ".JPG" → ".jpg").
--   Returns the empty string for paths with no extension.
lowerExt :: FilePath -> String
lowerExt = map toLower . takeExtension

-- | Prepend a key=value pair if not already present.
addAttr :: Text -> Text -> Attr -> Attr
addAttr k v (i, cs, kvs)
    | any ((== k) . fst) kvs = (i, cs, kvs)
    | otherwise               = (i, cs, (k, v) : kvs)

-- | Plain-text content of a list of inlines (for alt text).
stringify :: [Inline] -> Text
stringify = T.concat . map go
  where
    go (Str t)            = t
    go Space              = " "
    go SoftBreak          = " "
    go LineBreak          = " "
    go (Emph ils)         = stringify ils
    go (Strong ils)       = stringify ils
    go (Strikeout ils)    = stringify ils
    go (Superscript ils)  = stringify ils
    go (Subscript ils)    = stringify ils
    go (SmallCaps ils)    = stringify ils
    go (Underline ils)    = stringify ils
    go (Quoted _ ils)     = stringify ils
    go (Cite _ ils)       = stringify ils
    go (Code _ t)         = t
    go (Math _ t)         = t
    go (RawInline _ _)    = ""
    go (Link _ ils _)     = stringify ils
    go (Image _ ils _)    = stringify ils
    go (Span _ ils)       = stringify ils
    go (Note _)           = ""

-- | HTML-escape a text value for use in attribute values.
--   Defers to the canonical 'Utils.escapeHtmlText'.
esc :: Text -> Text
esc = U.escapeHtmlText
