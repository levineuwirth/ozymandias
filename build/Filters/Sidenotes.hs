{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Convert Pandoc @Note@ inlines to inline sidenote HTML.
--
--   Each footnote becomes:
--   * A @<sup class="sidenote-ref">@ anchor in the body text.
--   * An @<aside class="sidenote">@ immediately following it, containing
--     the rendered note content.
--
--   On wide viewports, sidenotes.css floats asides into the right margin.
--   On narrow viewports they are hidden; the standard Pandoc-generated
--   @<section class="footnotes">@ at the document end serves as fallback.
module Filters.Sidenotes (apply) where

import           Control.Monad.State.Strict
import           Data.Default               (def)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Pandoc.Class          (runPure)
import           Text.Pandoc.Definition
import           Text.Pandoc.Options        (WriterOptions)
import           Text.Pandoc.Walk           (walkM)
import           Text.Pandoc.Writers.HTML   (writeHtml5String)

-- | Transform all @Note@ inlines in the document to inline sidenote HTML.
apply :: Pandoc -> Pandoc
apply doc = evalState (walkM convertNote doc) (1 :: Int)

convertNote :: Inline -> State Int Inline
convertNote (Note blocks) = do
    n <- get
    put (n + 1)
    return $ RawInline "html" (renderNote n blocks)
convertNote x = return x

-- | Convert a 1-based counter to a letter label using base-26 expansion
--   (Excel-column style): 1→a, 2→b, … 26→z, 27→aa, 28→ab, … 52→az,
--   53→ba, … 702→zz, 703→aaa.  Guarantees a unique label per counter so
--   no two sidenotes in a single document collide on @id="sn-…"@.
toLabel :: Int -> Text
toLabel n
    | n <= 0    = "?"
    | otherwise = T.pack (go n)
  where
    go k
        | k <= 0    = ""
        | otherwise =
            let (q, r) = (k - 1) `divMod` 26
            in go q ++ [toEnum (fromEnum 'a' + r)]

renderNote :: Int -> [Block] -> Text
renderNote n blocks =
    let inner = blocksToInlineHtml blocks
        lbl   = toLabel n
    in T.concat
        [ "<sup class=\"sidenote-ref\" id=\"snref-", lbl, "\">"
        ,   "<a href=\"#sn-", lbl, "\">", lbl, "</a>"
        , "</sup>"
        , "<span class=\"sidenote\" id=\"sn-", lbl, "\">"
        ,   "<sup class=\"sidenote-num\">", lbl, "</sup>\x00a0"
        ,   inner
        , "</span>"
        ]

-- | Render a list of Pandoc blocks for inclusion inside an inline @<span
--   class="sidenote">@.  Each top-level @Para@ is wrapped in a
--   @<span class="sidenote-para">@ instead of a @<p>@ (which would be
--   invalid inside a @<span>@); other block types are rendered with the
--   regular Pandoc HTML writer.
--
--   Operating on the AST is preferred over post-rendered string
--   substitution because the latter mangles content that legitimately
--   contains the literal text @<p>@ (e.g. code samples discussing HTML).
blocksToInlineHtml :: [Block] -> Text
blocksToInlineHtml = T.concat . map renderOne
  where
    renderOne :: Block -> Text
    renderOne (Para inlines) =
        "<span class=\"sidenote-para\">"
        <> inlinesToHtml inlines
        <> "</span>"
    renderOne (Plain inlines) =
        inlinesToHtml inlines
    renderOne b =
        blocksToHtml [b]

-- | Render a list of inlines to HTML (no surrounding @<p>@).
inlinesToHtml :: [Inline] -> Text
inlinesToHtml inlines =
    case runPure (writeHtml5String (def :: WriterOptions) (Pandoc mempty [Plain inlines])) of
        Left  _ -> T.empty
        Right t -> t

-- | Render a list of Pandoc blocks to an HTML fragment via a pure writer run.
blocksToHtml :: [Block] -> Text
blocksToHtml blocks =
    case runPure (writeHtml5String (def :: WriterOptions) (Pandoc mempty blocks)) of
        Left _  -> T.empty
        Right t -> t
