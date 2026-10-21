{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Convert Pandoc @Note@ inlines to note references and one list of notes.
--
--   Each footnote becomes a @<sup class="sidenote-ref">@ reference in the
--   body text, and its content a @<li id="fn-…">@ in the
--   @<section class="footnotes">@ appended at the document end. That list
--   item is the note's only copy: its blocks render as blocks (paragraphs,
--   lists, quotations, code), keep their ids, and carry a link back to the
--   reference.
--
--   The list is what a reader gets without JavaScript, on narrow screens,
--   and on paper. On wide screens sidenotes.js sets the same list items in
--   the right margin, level with their references, and on narrow ones it
--   lends an item to a reading sheet and puts it back. It pairs a
--   reference with its note through the reference's @href@ and the note's
--   back-link, so a transclusion that renames both keeps the pair.
module Filters.Sidenotes (apply) where

import           Control.Monad.State.Strict
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Pandoc.Class          (runPure)
import           Text.Pandoc.Definition
import           Text.Pandoc.Shared         (stringify)
import           Text.Pandoc.Walk           (walkM)
import           Text.Pandoc.Writers.HTML   (writeHtml5String)
import           PandocOptions              (writerOpts)
import           Utils                      (escapeHtmlText)

-- | Accumulator: next label counter plus collected notes
--   (newest-first; reversed before rendering the list).
type NoteState = (Int, [(Text, [Block])])

-- | Replace every @Note@ inline with its reference, and append the
--   collected notes as a @section.footnotes@ block.
apply :: Pandoc -> Pandoc
apply doc =
    let (Pandoc m blocks, (_, collected)) =
            runState (walkM convertNote doc) (1, [])
        notes = reverse collected
    in  Pandoc m $
            if null notes
                then blocks
                else blocks ++ [footnotesSection notes]

convertNote :: Inline -> State NoteState Inline
convertNote (Note blocks) = do
    (n, acc) <- get
    let lbl = toLabel n
    put (n + 1, (lbl, blocks) : acc)
    return $ RawInline "html" (renderRef lbl)
convertNote x = return x

-- | The in-text reference. Its @href@ names the note, which every layout
--   shows somewhere: in the list, in the margin, or in the sheet.
renderRef :: Text -> Text
renderRef lbl = T.concat
    [ "<sup class=\"sidenote-ref\" id=\"snref-", lbl, "\">"
    , "<a href=\"#fn-", lbl, "\" role=\"doc-noteref\">", lbl, "</a>"
    , "</sup>"
    ]

-- | The list of notes. Letter labels are rendered explicitly (an
--   @<ol>@'s automatic numbering would disagree with the in-text
--   letters), so the list itself is unstyled.
footnotesSection :: [(Text, [Block])] -> Block
footnotesSection notes = RawBlock "html" $ T.concat $
    [ "<section class=\"footnotes\" role=\"doc-endnotes\">"
    , "<ol class=\"footnotes-list\">"
    ]
    ++ map item notes ++
    [ "</ol>"
    , "</section>"
    ]
  where
    item (lbl, blocks) = T.concat
        [ "<li id=\"fn-", lbl, "\" class=\"footnote-item\">"
        , "<span class=\"footnote-label\" aria-hidden=\"true\">", lbl, "</span>"
        , blocksToHtml (withBacklink lbl blocks)
        , "</li>"
        ]

-- | The back-link closes the note's last paragraph, as Pandoc's own
--   footnotes do, instead of taking a line to itself; after a list or a
--   code block there is no paragraph to close, and it follows on its own.
withBacklink :: Text -> [Block] -> [Block]
withBacklink lbl blocks = case reverse blocks of
    Para  xs : rest -> reverse (Para  (xs ++ [back]) : rest)
    Plain xs : rest -> reverse (Plain (xs ++ [back]) : rest)
    _               -> blocks ++ [Plain [back]]
  where
    back = RawInline "html" $ T.concat
        [ "<a href=\"#snref-", lbl
        , "\" class=\"footnote-back\" role=\"doc-backlink\""
        , " aria-label=\"Back to reference ", lbl, "\">\x21a9\xfe0e</a>"
        ]

-- | Convert a 1-based counter to a letter label using base-26 expansion
--   (Excel-column style): 1→a, 2→b, … 26→z, 27→aa, 28→ab, … 52→az,
--   53→ba, … 702→zz, 703→aaa.  Guarantees a unique label per counter so
--   no two notes in a single document collide on @id="fn-…"@.
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

-- | Render a list of Pandoc blocks to an HTML fragment via a pure writer
--   run. Should the writer ever fail, the note keeps its text, escaped,
--   instead of rendering empty without a word.
blocksToHtml :: [Block] -> Text
blocksToHtml blocks =
    either (const (escapeHtmlText (stringify blocks))) id
        (runPure (writeHtml5String writerOpts (Pandoc mempty blocks)))
