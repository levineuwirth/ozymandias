{-# LANGUAGE GHC2021 #-}
-- | Math filter placeholder.
--
--   The spec calls for converting simple LaTeX to Unicode at build time.
--   For now, all math (inline and display) is handled client-side by KaTeX,
--   which is loaded conditionally on pages that contain math.  Server-side
--   KaTeX rendering is a Phase 3 task.
module Filters.Math (apply) where

import Text.Pandoc.Definition (Pandoc)

-- | Identity — math rendering is handled by KaTeX.
apply :: Pandoc -> Pandoc
apply = id
