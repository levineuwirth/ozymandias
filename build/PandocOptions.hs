{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The site's Pandoc reader and writer options, in a leaf module so the
--   filters can use them: "Compilers" imports "Filters", so a filter that
--   needed these used to keep a mirrored copy (Sidenotes' note bodies,
--   Viz's captions) and say so in a comment.
module PandocOptions
    ( readerOpts
    , poetryReaderOpts
    , writerOpts
    ) where

import Hakyll                 (defaultHakyllReaderOptions, defaultHakyllWriterOptions)
import Text.Pandoc.Extensions (Extension (..), enableExtension)
import Text.Pandoc.Options    (HTMLMathMethod (..), ReaderOptions (..), WriterOptions (..))

readerOpts :: ReaderOptions
readerOpts = defaultHakyllReaderOptions

-- | Reader options with hard_line_breaks enabled — every source newline within
--   a paragraph becomes a <br>. Used for poetry so stanza lines render as-is.
poetryReaderOpts :: ReaderOptions
poetryReaderOpts = readerOpts
    { readerExtensions = enableExtension Ext_hard_line_breaks
                            (readerExtensions readerOpts) }

-- | KaTeX math, which @static/js/katex-bootstrap.js@ typesets from each
--   @\<span class="math"\>@'s bare TeX: any other method wraps it in
--   delimiters, or degrades it to italics, and the client never sees it.
writerOpts :: WriterOptions
writerOpts = defaultHakyllWriterOptions
    { writerHTMLMathMethod  = KaTeX ""
    , writerHighlightStyle  = Nothing
    , writerNumberSections  = False
    , writerTableOfContents = False
    }
