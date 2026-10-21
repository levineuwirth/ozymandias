{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Prepend "language-" to fenced-code-block class names so that
--   Prism.js can find and highlight them.
--
--   Pandoc (with writerHighlightStyle = Nothing) outputs
--     <pre class="python"><code>
--   Prism.js requires
--     <pre class="language-python"><code class="language-python">
--
--   We transform the AST before writing rather than post-processing HTML,
--   so the class appears on both <pre> and <code> via Pandoc's normal output.
module Filters.Code (apply) where

import qualified Data.Text            as T
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk     (walk)

apply :: Pandoc -> Pandoc
apply = walk addLangPrefix

addLangPrefix :: Block -> Block
addLangPrefix (CodeBlock (ident, classes, kvs) code) =
    CodeBlock (ident, map prefix classes, kvs) code
  where
    prefix c
        | "language-" `T.isPrefixOf` c = c
        | otherwise                     = "language-" <> c
addLangPrefix x = x
