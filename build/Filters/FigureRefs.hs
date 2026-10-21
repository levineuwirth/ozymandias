{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Opt-in figure numbering and cross-references.
--
-- Set @figure-numbering: true@ in a page's YAML frontmatter. Every
-- captioned figure on that page is then numbered in document order, gains
-- an @id@, and its caption is prefixed with @Figure N.@ An empty link
-- pointing at one resolves to its number:
--
-- > ::: {.figure #fig-decomp script="figures/fig_decomp.py" caption="…"}
-- > :::
-- >
-- > The decomposition in [](#fig-decomp) shows …
--
-- renders as @The decomposition in <a href="#fig-decomp">Figure 1</a> shows@.
--
-- __Why opt-in.__ Three essays already number by hand, in three different
-- conventions: @beyond-comorbidity-indices@ carries 28 manual @Figure N@ /
-- @Table N@ references over plain images, @specification-dilemma@ uses
-- explicit @{#table-1}@ anchors, and the PQC essay mixes a hand-written
-- @Table 1@ caption with unnumbered figures. Numbering everything by
-- default would double-number all three. A page opts in when its author is
-- ready to convert it; every other page is untouched.
--
-- __Why an empty link and not @[\@fig:x]@.__ The bracketed-citation form is
-- what pandoc-crossref uses, but citeproc runs over this document first
-- (see "Compilers") and would resolve @\@fig:x@ against the bibliography,
-- reporting it as a missing reference. @[](#anchor)@ is ordinary Markdown
-- that no earlier pass claims, and it degrades to a plain anchor link if
-- this filter never runs. A link that already has text is left alone, so
-- @[the decomposition](#fig-decomp)@ still says what the author wrote.
--
-- __Scope.__ Figures only — tables keep whatever convention the page
-- already uses.
--
-- Two block shapes have to be handled, because the page's figures do not
-- all arrive the same way. "Filters.Viz" emits a @RawBlock@ of rendered
-- @\<figure\>@ HTML, and so does "Filters.Images" on its WebP path; but an
-- image that never reaches that path — a PDF, say, which Pandoc writes as
-- @\<embed\>@ — stays a Pandoc 'Figure' node until the HTML writer runs.
-- Handling only the raw HTML would silently skip those, which is exactly
-- what happened on first wiring: the PDF figure kept its @id@ but never
-- got a number, so the reference to it resolved to @Figure ?@. This runs
-- after every producer and counts both shapes in one pass.
module Filters.FigureRefs (apply) where

import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk       (walk)

-- ---------------------------------------------------------------------------
-- Public entry point
-- ---------------------------------------------------------------------------

-- | Number the captioned figures in a document and resolve empty anchor
--   links to their numbers. Call only for pages that opted in.
apply :: Pandoc -> Pandoc
apply (Pandoc meta blocks) =
    let (numbered, index) = numberBlocks blocks
    in  Pandoc meta (walk (resolveRef index) numbered)

-- ---------------------------------------------------------------------------
-- Pass 1 — numbering
-- ---------------------------------------------------------------------------

-- | Rewrite every captioned @\<figure\>@ in document order, returning the
--   rewritten blocks and a map from anchor id to figure number.
--
--   A plain left-to-right fold rather than 'Text.Pandoc.Walk.walkM' in a
--   state monad: numbering has to follow reading order, and @walkM@ visits
--   a nested block before the container that holds it, which would number a
--   figure inside a div ahead of one that precedes the div on the page.
numberBlocks :: [Block] -> ([Block], Map T.Text Int)
numberBlocks = go 1 M.empty
  where
    go _ index [] = ([], index)
    go n index (b : bs) =
        case b of
            RawBlock fmt html
                | fmt == Format "html", isCaptionedFigure html ->
                    let (html', anchor) = numberFigure n html
                        index'          = M.insert anchor n index
                        (rest, index'') = go (n + 1) index' bs
                    in  (RawBlock fmt html' : rest, index'')
            Figure (figId, cls, kvs) capt inner
                | captionHasText capt ->
                    let anchor          = if T.null figId
                                              then defaultAnchor n
                                              else figId
                        block'          = Figure (anchor, cls, kvs)
                                                 (prefixCaptionAST n capt)
                                                 (keepAltInStep n capt inner)
                        index'          = M.insert anchor n index
                        (rest, index'') = go (n + 1) index' bs
                    in  (block' : rest, index'')
            -- Recurse into containers so a figure inside a div or a
            -- blockquote is still numbered, and still in reading order.
            Div attr inner ->
                let (inner', index') = go n index inner
                    n'               = n + countFigures inner
                    (rest, index'')  = go n' index' bs
                in  (Div attr inner' : rest, index'')
            BlockQuote inner ->
                let (inner', index') = go n index inner
                    n'               = n + countFigures inner
                    (rest, index'')  = go n' index' bs
                in  (BlockQuote inner' : rest, index'')
            _ ->
                let (rest, index') = go n index bs
                in  (b : rest, index')

-- | How many captioned figures a block subtree contributes to the count.
countFigures :: [Block] -> Int
countFigures = sum . map one
  where
    one (RawBlock (Format "html") html) | isCaptionedFigure html = 1
    one (Figure _ capt _)               | captionHasText capt    = 1
    one (Div _ inner)                                            = countFigures inner
    one (BlockQuote inner)                                       = countFigures inner
    one _                                                        = 0

-- | Whether a Pandoc 'Caption' carries any inline content to label.
captionHasText :: Caption -> Bool
captionHasText (Caption _ blocks) = any hasInlines blocks
  where
    hasInlines (Plain ils) = not (null ils)
    hasInlines (Para  ils) = not (null ils)
    hasInlines _           = False

-- | The AST counterpart of 'prefixCaption': put the label at the head of
--   the caption's first text-bearing block, in the same @figure-number@
--   span the HTML path uses so one CSS rule covers both.
prefixCaptionAST :: Int -> Caption -> Caption
prefixCaptionAST n (Caption short blocks) = Caption short (goBlocks blocks)
  where
    goBlocks []       = []
    goBlocks (b : bs) = case b of
        Plain ils | not (null ils) -> Plain (labelInlines n <> ils) : bs
        Para  ils | not (null ils) -> Para  (labelInlines n <> ils) : bs
        _                          -> b : goBlocks bs


-- | Keep an image's alt text equal to its caption when it already was.
--
--   Pandoc's HTML writer marks a figcaption @aria-hidden="true"@ when it
--   duplicates the image's alt, so a screen reader announces the text once
--   rather than twice (see the no-WebP note in "Filters.Images"). Labelling
--   only the caption breaks that equality and the deduplication silently
--   stops — the reader hears the whole caption a second time. Prefixing both
--   sides identically keeps the invariant, and "Figure 3." is worth
--   announcing anyway.
--
--   Alt text that already differs from the caption is deliberate and is
--   left alone.
keepAltInStep :: Int -> Caption -> [Block] -> [Block]
keepAltInStep n capt = map (walk relabel)
  where
    captionIls = captionInlines capt

    relabel img@(Image attr alt tgt)
        | alt == captionIls = Image attr (labelInlines n <> alt) tgt
        | otherwise         = img
    relabel i = i

captionInlines :: Caption -> [Inline]
captionInlines (Caption _ blocks) = concatMap go blocks
  where
    go (Plain ils) = ils
    go (Para  ils) = ils
    go _           = []

labelInlines :: Int -> [Inline]
labelInlines n =
    [ Span ("", ["figure-number"], [])
           [Str ("Figure " <> T.pack (show n) <> ".")]
    , Space
    ]

-- | A @\<figure\>@ is numbered only when it carries a caption to number.
--   An uncaptioned figure — a decorative image, a diagram the prose
--   describes inline — is left alone rather than given an invisible number.
isCaptionedFigure :: T.Text -> Bool
isCaptionedFigure html =
    "<figure" `T.isInfixOf` html && "<figcaption" `T.isInfixOf` html

-- | Give one figure its number: ensure it has an @id@, and prefix its
--   caption with @Figure N.@ Returns the rewritten HTML and the anchor to
--   register, which is the figure's own id when it has one and a generated
--   @fig-N@ otherwise.
numberFigure :: Int -> T.Text -> (T.Text, T.Text)
numberFigure n html =
    let existing        = existingId html
        anchor          = fromMaybe (defaultAnchor n) existing
        withId          = case existing of
                              Just _  -> html
                              Nothing -> insertId anchor html
    in  (prefixCaption n withId, anchor)

defaultAnchor :: Int -> T.Text
defaultAnchor n = "fig-" <> T.pack (show n)

-- | The value of the @\<figure\>@ tag's own @id@ attribute, if it has one.
--   Only the opening tag is searched, so an @id@ on a descendant (a
--   matplotlib @\<g id="text_1"\>@, say) is never mistaken for the
--   figure's.
existingId :: T.Text -> Maybe T.Text
existingId html = do
    openTag <- figureOpenTag html
    let (_, rest) = T.breakOn " id=\"" openTag
    if T.null rest
        then Nothing
        else Just (T.takeWhile (/= '"') (T.drop 5 rest))

-- | The text of the opening @\<figure …\>@ tag, exclusive of its @\>@.
figureOpenTag :: T.Text -> Maybe T.Text
figureOpenTag html =
    let (_, rest) = T.breakOn "<figure" html
    in  if T.null rest then Nothing else Just (T.takeWhile (/= '>') rest)

-- | Add an @id@ to the opening @\<figure\>@ tag.
insertId :: T.Text -> T.Text -> T.Text
insertId anchor html =
    let (before, rest) = T.breakOn "<figure" html
    in  case T.stripPrefix "<figure" rest of
            Nothing -> html
            Just r  -> before <> "<figure id=\"" <> anchor <> "\"" <> r

-- | Insert @Figure N.@ at the start of the figure's caption text, inside a
--   span so the label can be styled separately from the caption prose.
prefixCaption :: Int -> T.Text -> T.Text
prefixCaption n html =
    let (before, rest) = T.breakOn "<figcaption" html
    in  if T.null rest
            then html
            else
                -- Split after the figcaption's own '>' so the label lands
                -- inside the element rather than inside its attributes.
                let (openTag, body) = T.breakOn ">" rest
                in  case T.stripPrefix ">" body of
                        Nothing -> html
                        Just b  -> before <> openTag <> ">" <> label <> b
  where
    label = "<span class=\"figure-number\">Figure "
         <> T.pack (show n) <> ".</span> "

-- ---------------------------------------------------------------------------
-- Pass 2 — cross-references
-- ---------------------------------------------------------------------------

-- | Fill in the text of an empty link whose target is a numbered figure.
--
--   An unresolved reference renders as @Figure ?@ rather than staying empty:
--   an empty @\<a\>@ is invisible in the page, so a typo in an anchor would
--   silently delete the reference from the sentence.
resolveRef :: Map T.Text Int -> Inline -> Inline
resolveRef index inline@(Link attr [] target@(url, _))
    | Just anchor <- T.stripPrefix "#" url
    , looksLikeFigureRef anchor =
        Link attr [Str (labelFor (M.lookup anchor index))] target
    | otherwise = inline
resolveRef _ inline = inline

-- | Only claim anchors that name a figure. Leaving every other empty link
--   alone keeps this filter out of the way of section anchors and of the
--   @{#table-1}@ convention two essays already use.
looksLikeFigureRef :: T.Text -> Bool
looksLikeFigureRef anchor =
    any (`T.isPrefixOf` anchor) ["fig-", "fig:"]

labelFor :: Maybe Int -> T.Text
labelFor (Just n) = "Figure " <> T.pack (show n)
labelFor Nothing  = "Figure ?"
