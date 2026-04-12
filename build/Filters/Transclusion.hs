{-# LANGUAGE GHC2021 #-}
-- | Source-level transclusion preprocessor.
--
--   Rewrites block-level {{slug}} and {{slug#section}} directives to raw
--   HTML placeholders that transclude.js resolves at runtime.
--
--   A directive must be the sole content of a line (after trimming) to be
--   replaced — this prevents accidental substitution inside prose or code.
--
--   Examples:
--     {{my-essay}}              → full-page transclusion of /my-essay.html
--     {{essays/deep-dive}}      → /essays/deep-dive.html (full body)
--     {{my-essay#introduction}} → section "introduction" of /my-essay.html
module Filters.Transclusion (preprocess) where

import Data.List (isSuffixOf, isPrefixOf, stripPrefix)
import qualified Utils as U

-- | Apply transclusion substitution to the raw Markdown source string.
preprocess :: String -> String
preprocess = unlines . map processLine . lines

processLine :: String -> String
processLine line =
    case parseDirective (U.trim line) of
        Nothing             -> line
        Just (url, secAttr) ->
            "<div class=\"transclude\" data-src=\"" ++ escAttr url ++ "\""
            ++ secAttr ++ "></div>"

-- | Parse a {{slug}} or {{slug#section}} directive.
--   Returns (absolute-url, section-attribute-string) or Nothing.
--
--   The section name is HTML-escaped before being interpolated into the
--   @data-section@ attribute, so a stray @\"@, @&@, @<@, or @>@ in a
--   section name cannot break the surrounding markup.
parseDirective :: String -> Maybe (String, String)
parseDirective s = do
    inner <- stripPrefix "{{" s >>= stripSuffix "}}"
    case break (== '#') inner of
        ("", _)            -> Nothing
        (slug, "")         -> Just (slugToUrl slug, "")
        (slug, '#' : sec)
            | null sec     -> Just (slugToUrl slug, "")
            | otherwise    -> Just (slugToUrl slug,
                                    " data-section=\"" ++ escAttr sec ++ "\"")
        _                  -> Nothing

-- | Convert a slug (possibly with leading slash, possibly with path segments)
--   to a root-relative .html URL.  Idempotent for slugs that already end in
--   @.html@ so callers can safely pass either form.
slugToUrl :: String -> String
slugToUrl slug
    | ".html" `isSuffixOf` slug, "/" `isPrefixOf` slug = slug
    | ".html" `isSuffixOf` slug                        = "/" ++ slug
    | "/" `isPrefixOf` slug                            = slug ++ ".html"
    | otherwise                                        = "/" ++ slug ++ ".html"

-- | Minimal HTML attribute-value escape.
escAttr :: String -> String
escAttr = concatMap esc
  where
    esc '&'  = "&amp;"
    esc '<'  = "&lt;"
    esc '>'  = "&gt;"
    esc '"'  = "&quot;"
    esc '\'' = "&#39;"
    esc c    = [c]

-- | Strip a suffix from a string, returning Nothing if not present.
stripSuffix :: String -> String -> Maybe String
stripSuffix suf str
    | suf `isSuffixOf` str = Just (take (length str - length suf) str)
    | otherwise             = Nothing

