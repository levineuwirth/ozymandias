{-# LANGUAGE GHC2021 #-}
-- | Dropcap support.
--
--   The dropcap on the opening paragraph is implemented entirely in CSS
--   via @#markdownBody > p:first-of-type::first-letter@, so no AST
--   transformation is required.  This module is a placeholder for future
--   work (e.g. adding a @.lead-paragraph@ class when the first block is
--   not a Para, or decorative initial-capital images).
module Filters.Dropcaps (apply) where

import Text.Pandoc.Definition (Pandoc)

-- | Identity — dropcaps are handled by CSS.
apply :: Pandoc -> Pandoc
apply = id
