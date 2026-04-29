{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Image filter: lazy loading, lightbox markers, WebP <picture>
--   wrappers, and CLS-preventing width/height attrs.
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
--
--   Width / height attrs are looked up from @{image}.dims.yaml@ sidecars
--   produced by @tools/extract-dimensions.py@ at build time, on the same
--   path-resolution rules as the WebP companion check (absolute paths
--   under @static/@, relative under the source-file directory). When a
--   sidecar is missing the filter emits an attr-free <img> rather than
--   guessing — partial dimensions are worse than no dimensions, since
--   the browser would then size the image wrong on first paint.
module Filters.Images (apply) where

import           Data.Char            (toLower)
import           Data.Default         (def)
import           Data.List            (isPrefixOf)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Aeson.KeyMap    as KM
import qualified Data.Scientific      as Sci
import qualified Data.Yaml            as Y
import           Text.Pandoc.Definition
import qualified Text.Pandoc          as Pandoc
import           Text.Pandoc.Walk     (walkM)
import           System.Directory     (doesFileExist)
import           System.FilePath      (replaceExtension, takeExtension, (</>))
import qualified Utils                as U

-- | Apply image attribute injection and WebP wrapping to the entire document.
--
--   @srcDir@ is the directory of the source Markdown file, used to resolve
--   relative image paths when probing for the corresponding @.webp@
--   companion file. Absolute paths (leading @/@) are resolved against
--   @static/@ instead, matching the layout @convert-images.sh@ writes to.
--
--   Two-pass walk:
--
--     1. Block-level pass (@transformBlock@) intercepts standalone
--        figures so we can synthesize the entire @<figure>@ ourselves
--        when WebP wrapping kicks in. Without this pass, replacing the
--        inner @Image@ with a @RawInline@ would break Pandoc's
--        alt-vs-caption comparison and we'd lose the
--        @aria-hidden="true"@ hint on identical-text figcaptions.
--     2. Inline-level pass (@transformInline@) handles every remaining
--        @Image@ — inline-in-prose, inside @Link@s, etc. Pandoc's writer
--        still applies its accessibility heuristics for figures we
--        didn't synthesize (notably the no-WebP case).
apply :: FilePath -> Pandoc -> IO Pandoc
apply srcDir doc = do
    doc' <- walkM (transformBlock srcDir) doc
    walkM (transformInline srcDir) doc'

-- ---------------------------------------------------------------------------
-- Core transformations
-- ---------------------------------------------------------------------------

-- | Block-level pass. Currently only acts on the simple-figure shape
--   that Pandoc's Markdown reader produces for @![alt](src)@ standalone:
--
--     @Figure attr caption [Plain [Image imgAttr alt target]]@
--
--   When the image has a WebP companion on disk, we replace the whole
--   Figure with a @RawBlock@ containing the equivalent HTML — but with
--   the @<picture>@ wrapper inside and a manually-emitted
--   @aria-hidden="true"@ on the figcaption when alt text equals the
--   caption text. Anything more exotic (multi-image figures, mixed
--   block content inside the figure, no-WebP images) is left to
--   Pandoc's default emission, which is already correct for those
--   cases.
transformBlock :: FilePath -> Block -> IO Block
transformBlock srcDir b@(Figure figAttr caption [Plain [Image imgAttr alt target]]) = do
    let src = T.unpack (fst target)
    if not (isLocalRaster src)
        then pure b
        else do
            hasWebp <- doesFileExist (webpPhysicalPath srcDir (fst target))
            if not hasWebp
                then pure b  -- Pandoc handles aria-hidden naturally on the no-WebP path.
                else synthesizeFigure srcDir figAttr caption imgAttr alt target
transformBlock _ b = pure b

-- | Build a @<figure>@ block from an Image and its surrounding
--   metadata. Used only on the WebP branch; the no-WebP branch leaves
--   Pandoc to emit the figure naturally.
--
--   Aria-hiding rule: when the caption's plain-text content equals the
--   alt text and both are non-empty, mark the @<figcaption>@ with
--   @aria-hidden="true"@. Screen readers then announce the alt
--   (via the @<img>@) and skip the figcaption (which would just
--   duplicate it). Non-matching captions render as visible content.
--
--   Caption inline rendering goes through Pandoc's HTML writer, so
--   formatting (italic, links, code, etc.) is preserved.
synthesizeFigure :: FilePath -> Attr -> Caption -> Attr -> [Inline] -> Target -> IO Block
synthesizeFigure srcDir figAttr caption imgAttr alt target = do
    dims <- readDims srcDir (fst target)
    let pictureHtml = renderPicture imgAttr alt target True dims
        capInlines  = captionInlines caption
        capText     = stringify capInlines
        altText     = stringify alt
        useAriaHide = capText == altText && not (T.null altText)
    pure $ RawBlock (Format "html") $
        renderFigure figAttr pictureHtml (renderFigcaption capInlines useAriaHide)

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
--
--   In all three branches, when a @{image}.dims.yaml@ sidecar is
--   present, @width@ and @height@ attrs are emitted on the rendered
--   @<img>@. The sidecar lookup is skipped for non-local sources
--   (HTTP URLs, data URIs) since there's no local file to measure.
renderImg :: FilePath -> Attr -> [Inline] -> Target -> Bool -> IO Inline
renderImg srcDir attr alt target@(src, _) lightbox = do
    let s        = T.unpack src
        isRaster = isLocalRaster s
        local    = not (isUrl s)
    dims <- if local then readDims srcDir src else pure Nothing
    if isRaster
        then do
            hasWebp <- doesFileExist (webpPhysicalPath srcDir src)
            if hasWebp
                then pure $ RawInline (Format "html")
                                      (renderPicture attr alt target lightbox dims)
                else pure $ Image (commonAttrs dims) alt target
        else
            pure $ Image (commonAttrs dims) alt target
  where
    commonAttrs dims =
        withDims dims
        $ addAttr "decoding" "async"
        $ addLightbox lightbox
        $ addAttr "loading" "lazy" attr

    addLightbox True  a = addAttr "data-lightbox" "true" a
    addLightbox False a = a

    withDims Nothing       a = a
    withDims (Just (w, h)) a =
        addAttr "width"  (T.pack (show w))
            (addAttr "height" (T.pack (show h)) a)

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

-- | Physical on-disk path of the @.dims.yaml@ sidecar for a Markdown
--   image src. Same path-resolution rules as 'webpPhysicalPath'; the
--   sidecar lives next to the original image with the literal
--   extension @.dims.yaml@ appended.
dimsPhysicalPath :: FilePath -> Text -> FilePath
dimsPhysicalPath srcDir src =
    let s = T.unpack src
        physical = if "/" `isPrefixOf` s
                   then "static" ++ s
                   else srcDir </> s
    in physical ++ ".dims.yaml"

-- | Read the @{image}.dims.yaml@ sidecar and return @(width, height)@
--   when present and parseable. Returns 'Nothing' on absent file,
--   parse error, missing keys, or non-integer values — all of which
--   cause the filter to emit no width/height attrs (rather than a
--   guess that would size the image wrong on first paint).
readDims :: FilePath -> Text -> IO (Maybe (Int, Int))
readDims srcDir src = do
    let path = dimsPhysicalPath srcDir src
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            decoded <- Y.decodeFileEither path
            pure $ case decoded of
                Right (Y.Object obj) -> do
                    w <- intValue =<< KM.lookup "width"  obj
                    h <- intValue =<< KM.lookup "height" obj
                    Just (w, h)
                _ -> Nothing
  where
    intValue :: Y.Value -> Maybe Int
    intValue (Y.Number n) = Sci.toBoundedInteger n
    intValue _            = Nothing

-- ---------------------------------------------------------------------------
-- <picture> rendering
-- ---------------------------------------------------------------------------

-- | Emit a @<picture>@ element with a WebP @<source>@ and an @<img>@ fallback.
renderPicture :: Attr -> [Inline] -> Target -> Bool -> Maybe (Int, Int) -> Text
renderPicture (ident, classes, kvs) alt (src, title) lightbox dims =
    T.concat
        [ "<picture>"
        , "<source srcset=\"", T.pack webpSrc, "\" type=\"image/webp\">"
        , "<img"
        , attrId ident
        , attrClasses classes
        , " src=\"",     esc src,                "\""
        , attrAlt alt
        , attrTitle title
        , dimsAttrs dims
        , " loading=\"lazy\""
        , " decoding=\"async\""
        , if lightbox then " data-lightbox=\"true\"" else ""
        , renderKvs passedKvs
        , ">"
        , "</picture>"
        ]
  where
    webpSrc   = replaceExtension (T.unpack src) ".webp"
    -- Strip attrs we handle explicitly above (id/class/alt/title) and the
    -- attrs we always emit ourselves (loading, decoding, data-lightbox,
    -- width, height), so they don't appear twice on the <img>.
    passedKvs = filter
        (\(k, _) -> k `notElem`
            [ "loading", "decoding", "data-lightbox"
            , "id", "class", "alt", "title", "src"
            , "width", "height"
            ])
        kvs

    dimsAttrs Nothing       = ""
    dimsAttrs (Just (w, h)) =
        " width=\"" <> T.pack (show w)
        <> "\" height=\"" <> T.pack (show h) <> "\""

-- ---------------------------------------------------------------------------
-- <figure> synthesis (Block walk, WebP path only)
-- ---------------------------------------------------------------------------

-- | Build a @<figure>@ HTML element wrapping pre-rendered inner
--   content (typically a @<picture>@) and a pre-rendered figcaption.
--   Preserves any id / classes / kvs from the surrounding Pandoc
--   'Figure' attr.
renderFigure :: Attr -> Text -> Text -> Text
renderFigure (figId, figClasses, figKvs) inner figcaption =
    T.concat
        [ "<figure"
        , attrId figId
        , attrClasses figClasses
        , renderKvs figKvs
        , ">\n"
        , inner
        , "\n"
        , figcaption
        , "\n</figure>"
        ]

-- | Build a @<figcaption>@ element. When @ariaHidden@ is true, emits
--   @aria-hidden="true"@ — used when the caption text exactly
--   duplicates the image alt (so screen readers don't announce the
--   same content twice). Caption inlines render through Pandoc's HTML
--   writer to preserve formatting.
renderFigcaption :: [Inline] -> Bool -> Text
renderFigcaption ils ariaHidden =
    let body  = renderInlinesToHtml ils
        attrs = if ariaHidden then " aria-hidden=\"true\"" else ""
    in  "<figcaption" <> attrs <> ">" <> body <> "</figcaption>"

-- | Pandoc 'Caption' has a long form (@[Block]@) and an optional short
--   form (@Maybe ShortCaption@). We use the long form, flattening any
--   @Plain@ / @Para@ blocks into a single inline list. Multi-block
--   captions (rare) collapse to the inlines of their text-bearing
--   blocks; non-text blocks (like nested lists) are dropped, since
--   they don't make sense in a figcaption anyway.
captionInlines :: Caption -> [Inline]
captionInlines (Caption _ blocks) = concatMap go blocks
  where
    go (Plain ils) = ils
    go (Para  ils) = ils
    go _           = []

-- | Render Pandoc 'Inline' nodes to HTML using Pandoc's own writer.
--   Wrapping the inlines in a @Plain@ block (rather than @Para@)
--   avoids the surrounding @<p>@ tag the writer would otherwise emit.
--   On writer failure (extremely unlikely for inline-only input),
--   falls back to the plain-text 'stringify' rendering — a worse but
--   still safe figcaption.
renderInlinesToHtml :: [Inline] -> Text
renderInlinesToHtml ils =
    case Pandoc.runPure (Pandoc.writeHtml5String def doc) of
        Right t -> T.strip t
        Left  _ -> stringify ils
  where
    doc = Pandoc mempty [Plain ils]

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
