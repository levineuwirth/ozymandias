{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | MarkdownSections — render a Markdown source file to a sectioned JSON
--   document for the source/code-reference hover popups.
--
--   A link to @analysis/b200-session-4-archive.md#results@ should preview
--   the Results section as prose — table and all — not the first eighty
--   lines of raw Markdown. The popup (@renderMarkdownPopup@ in
--   @static/js/popups.js@) needs the document split at its headings and
--   each heading's GitHub anchor, so it can show the section a fragment
--   names, or the whole file when there is none.
--
--   Output shape:
--
--   > { "sections": [ { "id": "results", "level": 2,
--   >                   "heading": "<inline html>", "html": "<blocks>" }, … ] }
--
--   Content before the first heading is a section of level 0 with an
--   empty id. A section's @html@ holds only its own blocks, up to the
--   next heading of any level; the popup reassembles subsections.
--
--   Identifiers use GitHub's algorithm ('Ext_gfm_auto_identifiers'), so a
--   fragment copied from GitHub's rendered view resolves here too.
--
--   The Markdown can come from someone else's repository, and the HTML is
--   inserted into this site's pages. So the AST is sanitised before it is
--   written: raw HTML is disabled at the reader, and 'sanitize' reduces
--   every link that is not absolute http(s) to its text (relative links
--   would resolve against the essay, and fragments would jump within it)
--   and every image to its alt text. No block keeps an identifier, so
--   nothing can collide with an id on the host page. Footnotes are off:
--   each section is written separately, and a footnote's back-reference
--   would target the host page's own @#fn1@.
module MarkdownSections (markdownSectionsCompiler, renderSections) where

import           Data.Aeson             ((.=))
import qualified Data.Aeson             as A
import qualified Data.ByteString.Lazy   as LBS
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Hakyll
import           Text.Pandoc            (def, disableExtension, enableExtension,
                                         githubMarkdownExtensions)
import           Text.Pandoc.Class      (runPure)
import           Text.Pandoc.Definition
import           Text.Pandoc.Extensions (Extension (..))
import           Text.Pandoc.Options    (ReaderOptions (..), WriterOptions (..))
import           Text.Pandoc.Readers    (readCommonMark)
import           Text.Pandoc.Walk       (walk)
import           Text.Pandoc.Writers    (writeHtml5String)

-- | Hakyll compiler: the resource's Markdown → its sections JSON.
--   An unparseable file yields an empty section list; the popup then
--   falls back to showing nothing rather than failing the build.
markdownSectionsCompiler :: Compiler (Item LBS.ByteString)
markdownSectionsCompiler = do
    body <- itemBody <$> getResourceString
    makeItem (renderSections (T.pack body))

renderSections :: Text -> LBS.ByteString
renderSections src = A.encode $ A.object ["sections" .= map toJson sections]
  where
    sections = case runPure (readCommonMark readerOpts src) of
        Left _                -> []
        Right (Pandoc _ blks) -> split blks
    -- Split first, on the reader's identifiers; sanitising afterwards
    -- strips the ids of any heading nested inside a quote or list.
    toJson (ident, lvl, heading, blks) = A.object
        [ "id"      .= ident
        , "level"   .= lvl
        , "heading" .= blockHtml [Plain (walk sanitizeInline heading)]
        , "html"    .= blockHtml (map sanitize blks)
        ]

readerOpts :: ReaderOptions
readerOpts = def
    { readerExtensions =
          disableExtension Ext_raw_html
        . disableExtension Ext_footnotes
        . enableExtension  Ext_gfm_auto_identifiers
        . enableExtension  Ext_auto_identifiers
        $ githubMarkdownExtensions
    }

writerOpts :: WriterOptions
writerOpts = def { writerHighlightStyle = Nothing }

-- | Top-level blocks → (id, level, heading inlines, body blocks).
split :: [Block] -> [(Text, Int, [Inline], [Block])]
split = go ("", 0, [])
  where
    go (i, l, h) blks =
        let (body, rest) = break isHeader blks
            here         = [(i, l, h, body) | l > 0 || not (null body)]
        in  here ++ case rest of
                Header l' (i', _, _) h' : more -> go (i', l', h') more
                _                              -> []
    isHeader Header{} = True
    isHeader _        = False

sanitize :: Block -> Block
sanitize = walk sanitizeInline . walk block
  where
    block (RawBlock _ _)          = Plain []
    block (Header l (_, c, _) i)  = Header l ("", c, []) i
    block (CodeBlock (_, c, _) t) = CodeBlock ("", c, []) t
    block (Div (_, c, _) bs)      = Div ("", c, []) bs
    block (Table (_, c, _) ca cs th tb tf) = Table ("", c, []) ca cs th tb tf
    block b                       = b

sanitizeInline :: Inline -> Inline
sanitizeInline = inline
  where
    inline (RawInline _ _)        = Str ""
    inline (Image _ alt _)        = Span nullAttr alt
    inline (Link _ ils (url, _))
        | "https://" `T.isPrefixOf` url || "http://" `T.isPrefixOf` url =
            Link ("", [], [("target", "_blank"), ("rel", "noopener noreferrer")])
                 ils (url, "")
        | otherwise = Span nullAttr ils
    inline (Span (_, c, _) ils)   = Span ("", c, []) ils
    inline (Code (_, c, _) t)     = Code ("", c, []) t
    inline x                      = x

blockHtml :: [Block] -> Text
blockHtml [] = ""
blockHtml bs = either (const "") id $
    runPure (writeHtml5String writerOpts (Pandoc nullMeta bs))

