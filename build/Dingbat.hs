{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Section-break ornament selection.
--
-- Every page exposes a @$dingbat$@ template variable naming the ornament to
-- use in place of the standard @<hr>@. The template renders this as
-- @data-dingbat="..."@ on @<body>@; CSS attribute selectors then swap the
-- @hr::after@ glyph.
--
-- Resolution order:
--
--   1. @dingbat:@ frontmatter key on the page (one the stylesheets draw).
--   2. Default for the item's route: its standalone page's, or its
--      section's ('routeDefault').
--   3. Fallback ('fallbackDingbat').
--
-- Best practice: set @dingbat:@ explicitly in frontmatter. The section
-- defaults are a safety net, not a substitute.
--
-- The template asks the dingbats switch: without it no page names an
-- ornament (a page's @dingbat:@ would reach @$dingbat$@ from its metadata,
-- whatever this field answered), and the stylesheet's fallback serves them.
--
-- Adding a new ornament: a rule for @body[data-dingbat="NAME"]@ in the
-- site's stylesheets (site.css), and its SVG under @images/dingbats/@ if it
-- draws one. The rule is what makes the name one ('Themes.scanDingbats',
-- @site dingbats@); then a page's @dingbat:@ or site.yaml's defaults may
-- name it.
module Dingbat
    ( dingbatField
    ) where

import Control.Applicative ((<|>))
import Data.Maybe          (fromMaybe)
import Hakyll
import Data.List           (find)
import Config              (SiteConfig (..), Standalone (..), loadConfig)
import Sections            (Section (..), sectionOfRoute)
import Themes              (loadDingbats)

-- | Last-resort default when neither frontmatter nor section rule applies.
fallbackDingbat :: String
fallbackDingbat = "asterism"

-- | The default ornament for a route (site.yaml): its standalone page's,
--   which is the more specific (memento-mori's is @memento-mori.html@,
--   beside its directory; matching only @memento-mori/@ gave it the
--   fallback until 2026-10-04), else its section's. Anything else uses
--   'fallbackDingbat'.
routeDefault :: SiteConfig -> String -> String
routeDefault cfg r = fromMaybe fallbackDingbat $
    (find ((== r) . standaloneRoute) (siteStandalone cfg) >>= standaloneDingbat)
    <|> (sectionOfRoute cfg r >>= sectionDingbat)

-- | @$dingbat$@: name of the ornament to use on this page.
dingbatField :: Context a
dingbatField = field "dingbat" $ \item -> do
    cfg  <- loadConfig
    meta <- getMetadata (itemIdentifier item)
    r    <- fromMaybe "" <$> getRoute (itemIdentifier item)
    known <- loadDingbats
    let sectionD = routeDefault cfg r
    case lookupString "dingbat" meta of
        Nothing -> return sectionD
        Just name
            | name `elem` known -> return name
            | otherwise -> do
                let ident = toFilePath (itemIdentifier item)
                unsafeCompiler $ putStrLn $
                    "[Dingbat] " ++ ident ++ ": unknown dingbat \""
                    ++ name ++ "\" — using \"" ++ sectionD ++ "\""
                return sectionD
