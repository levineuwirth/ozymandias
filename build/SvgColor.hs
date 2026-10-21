{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Pure black in inlined SVG becomes @currentColor@, so a score page, a
--   figure or a frontmatter mark follows the text colour of the theme.
module SvgColor (blackToCurrentColor) where

import           Data.Char (isAlphaNum, isHexDigit, isSpace)
import qualified Data.Text as T

-- | Replace pure-black fill/stroke values with @currentColor@ so the embedded
--   SVG inherits the CSS text colour in both light and dark modes.
--
--   Two syntaxes have to be covered, because the site inlines SVG from two
--   generators that disagree about which one to emit:
--
--   * Quoted presentation attributes — @stroke="#000000"@ — which is what
--     Lilypond writes, and what 'Filters.Score' was written against. These
--     are self-delimiting: the closing quote bounds the match, so plain
--     'T.replace' is safe.
--
--   * @style=@ properties — @stroke: #000000@ — which is what matplotlib's
--     SVG backend writes, with a space after the colon. These need
--     'replaceBlackProp': whitespace is optional, and a match must not fire
--     on the prefix of a longer colour (@fill:#000080@ → @fill:currentColor80@
--     would be invalid CSS).
--
--   Shared by every SVG inliner — 'Filters.Viz', 'Filters.Score' and
--   "Marks" — which used to keep three copies of this table that had
--   drifted: Marks matched with plain 'T.replace', so @fill:#000080@
--   became @fill:currentColor80@, and Score's @fill:black@ fired inside
--   @fill:blackish@.
--
--   Viz's copy originally mirrored Score's table verbatim, matching only
--   the quoted and unspaced-property forms. Matplotlib emits neither, so on
--   the live corpus every rule matched zero times and the whole pass was
--   inert — dark mode was carried entirely by the @!important@ overrides in
--   @static\/css\/viz.css@, which is also what made those overrides trample a
--   figure's deliberate non-black colours. Handling the spaced form here is
--   what let those overrides be narrowed.
blackToCurrentColor :: T.Text -> T.Text
blackToCurrentColor
    = T.replace "fill=\"#000\""       "fill=\"currentColor\""
    . T.replace "fill=\"#000000\""    "fill=\"currentColor\""
    . T.replace "fill=\"black\""      "fill=\"currentColor\""
    . T.replace "stroke=\"#000\""     "stroke=\"currentColor\""
    . T.replace "stroke=\"#000000\""  "stroke=\"currentColor\""
    . T.replace "stroke=\"black\""    "stroke=\"currentColor\""
    . replaceBlackProp "fill"
    . replaceBlackProp "stroke"

-- | Rewrite every @\<prop\>:@ declaration whose value is pure black to
--   @currentColor@, tolerating the whitespace matplotlib puts after the
--   colon and leaving every other value untouched.
--
--   @prop@ is matched with its colon attached (@\"stroke:\"@), so the scan
--   cannot stray into a longhand that merely starts the same way
--   (@stroke-width:@, @stroke-linecap:@).
replaceBlackProp :: T.Text -> T.Text -> T.Text
replaceBlackProp prop = go
  where
    needle = prop <> ":"

    go t =
        let (pre, rest) = T.breakOn needle t
        in  if T.null rest
                then pre
                else
                    let value     = T.drop (T.length needle) rest
                        (ws, tok) = T.span isSpace value
                    in  case blackToken tok of
                            Just n  -> pre <> needle <> ws <> "currentColor"
                                           <> go (T.drop n tok)
                            -- Not black: re-scan from just past the colon.
                            -- The value can never itself contain @needle@,
                            -- so each step consumes at least that much and
                            -- the walk terminates.
                            Nothing -> pre <> needle <> go value

-- | Length of the pure-black colour token at the head of the input, if there
--   is one. The trailing-character checks keep @#000@ from matching inside
--   @#0008@ \/ @#000080@, and @black@ from matching inside a longer
--   identifier.
blackToken :: T.Text -> Maybe Int
blackToken t
    | Just r <- T.stripPrefix "#000000" t, boundary isHexDigit r = Just 7
    | Just r <- T.stripPrefix "#000"    t, boundary isHexDigit r = Just 4
    | Just r <- T.stripPrefix "black"   t, boundary isWordChar r = Just 5
    | otherwise                                                  = Nothing
  where
    isWordChar c = isAlphaNum c || c == '-' || c == '_'
    boundary p r = maybe True (not . p . fst) (T.uncons r)
