{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
module Filters.Aftermatter (apply) where

import Text.Pandoc.Definition (Pandoc (..), Block (..), Format (..))

-- | @linked@: the mark links the New page (the new switch).
apply :: Bool -> Pandoc -> Pandoc
apply linked (Pandoc meta blocks) = Pandoc meta (concatMap go blocks)
  where
    go (Div attr@(_, classes, _) content)
        | "aftermatter" `elem` classes
        = [dividerBlock linked, Div attr content]
    go b = [b]

-- | The rule and the mark are drawn by CSS, so nothing here needs
--   @aria-hidden@; the anchor is a real destination with an accessible
--   name. Hiding the divider left a focusable link that assistive
--   technology could not see (Sep-A04), fixed in templates/reading.html
--   but not here (audit H07). Without the New page the mark is only
--   drawn, hidden from assistive technology as the rule is.
dividerBlock :: Bool -> Block
dividerBlock True = RawBlock (Format "html")
    "<div class=\"aftermatter-divider\">\
    \<a href=\"/new.html\" class=\"aftermatter-logo\"><span class=\"visually-hidden\">New</span></a>\
    \</div>"
dividerBlock False = RawBlock (Format "html")
    "<div class=\"aftermatter-divider\">\
    \<span class=\"aftermatter-logo\" aria-hidden=\"true\"></span>\
    \</div>"
