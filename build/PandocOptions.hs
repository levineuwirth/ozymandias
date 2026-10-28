{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The site's Pandoc reader and writer options, in a leaf module so the
--   filters can use them: "Compilers" imports "Filters", so a filter that
--   needed these used to keep a mirrored copy (Sidenotes' note bodies,
--   Viz's captions) and say so in a comment.
module PandocOptions
    ( readerOpts
    , poetryReaderOpts
    , withMath
    , writerOpts
    ) where

import Hakyll                 (defaultHakyllReaderOptions, defaultHakyllWriterOptions)
import Text.Pandoc.Extensions (Extension (..), disableExtension, enableExtension)
import Text.Pandoc.Options    (HTMLMathMethod (..), ReaderOptions (..), WriterOptions (..))

readerOpts :: ReaderOptions
readerOpts = defaultHakyllReaderOptions

-- | Reader options with hard_line_breaks enabled — every source newline within
--   a paragraph becomes a <br>. Used for poetry so stanza lines render as-is.
poetryReaderOpts :: ReaderOptions
poetryReaderOpts = readerOpts
    { readerExtensions = enableExtension Ext_hard_line_breaks
                            (readerExtensions readerOpts) }

-- | Reader options for a site with the math switch on (as they are) or
--   off: then @$@ and @$$@ are text, not TeX, since no typesetter ships,
--   and so are TeX commands, which raw_tex would otherwise read and the
--   HTML writer drop (@\\pi@ vanished from @$e^{i\\pi}$@ as text). Every
--   reader of a page's Markdown, abstract, summary or caption asks.
withMath :: Bool -> ReaderOptions -> ReaderOptions
withMath True  opts = opts
withMath False opts = opts { readerExtensions = foldr disableExtension (readerExtensions opts)
                                                    [Ext_tex_math_dollars, Ext_raw_tex, Ext_latex_macros] }

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
