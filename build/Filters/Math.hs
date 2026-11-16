{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | TeX math, written as MathML at build time.
--
--   Every @Math@ inline becomes HTML: a @\<span class="math inline"\>@ or
--   @\<span class="math display"\>@ around a @\<math\>@ element that the
--   browser draws itself. Nothing runs in the reader's browser and nothing
--   but Pandoc's own texmath runs in the build, and a feed carries the
--   maths too. The face is the site's (@--font-math@, a font with an
--   OpenType MATH table); the engine's stylesheet sizes it.
--
--   Pandoc's MathML writer would emit the same @\<math\>@ element; this
--   module writes it itself for three things that writer does not do:
--
--   * The wrapper. @static/js/math.js@ gives an overflowing display formula
--     a tab stop, the selection popup and the exhibits overlay look for
--     @.math@, and a @\<math\>@ box does not scroll, its wrapper does.
--   * Space after a function name. texmath marks @\\log@, @\\max@ and
--     @\\operatorname@ with U+2061 FUNCTION APPLICATION, which MathML
--     spaces at zero; TeX puts a thin space between an operator and what
--     it applies to, unless that opens with a bracket (@\\log n@, but
--     @\\log(n)@). 'spaceOperators' adds it, after any script on the name.
--   * TeX it does not read. Plain TeX's font switches: texmath reads
--     @\\mathrm{in}@ but not @{\\rm in}@, and 'fontSwitches' rewrites the
--     old form. Four size commands: texmath 0.12 has @\\bigl@ but not
--     @\\Bigl@, nor the @m@ (middle) sizes, and 'sizeCommands' writes
--     them as the unmarked size, the same height.
--
--   The TeX stays in the formula as its annotation (@application/x-tex@),
--   as Pandoc writes it, for copying and for the overlay. A formula
--   texmath cannot read is left as its TeX, escaped, in the same wrapper:
--   Pandoc's own fallback.
module Filters.Math (apply, render, compat) where

import           Data.Char              (isLetter)
import           Data.Maybe             (listToMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Text.Pandoc.Definition (Format (..), Inline (..), MathType (..), Pandoc)
import           Text.Pandoc.Walk       (walk)
import           Text.TeXMath           (DisplayType (..), readTeX, writeMathML)
import           Text.XML.Light

-- | Every formula in the document, as MathML.
apply :: Pandoc -> Pandoc
apply = walk inline
  where
    inline (Math t tex) = RawInline (Format "html") (render t tex)
    inline x            = x

-- | One formula: its wrapper and MathML, or its TeX where texmath fails.
render :: MathType -> Text -> Text
render t tex = "<span class=\"math " <> kind <> "\">" <> body <> "</span>"
  where
    kind = case t of { DisplayMath -> "display"; InlineMath -> "inline" }
    dt   = case t of { DisplayMath -> DisplayBlock; InlineMath -> DisplayInline }
    body = case readTeX (compat tex) of
        Right exps -> T.pack (ppcElement config (annotate tex (spaceOperators (writeMathML dt exps))))
        Left _     -> escape tex
    config = useShortEmptyTags (const False) defaultConfigPP

-- | The formula's TeX, kept beside its presentation as Pandoc keeps it.
annotate :: Text -> Element -> Element
annotate tex e = e { elContent = [Elem (unode "semantics" [presentation, annotation])] }
  where
    presentation = case elChildren e of
        []  -> unode "mrow" ()
        [x] -> x
        xs  -> unode "mrow" xs
    annotation = add_attr (Attr (unqual "encoding") "application/x-tex")
                          (unode "annotation" (T.unpack tex))

-- | Thin spaces around each function name (and its scripts), at every
--   depth, where TeX puts them: after it unless what it applies to opens
--   with a bracket (@\\log n@, @\\log(n)@), and before it after a symbol,
--   a number or a closing bracket (@n\\log n@, @2\\log\\log n@), where an
--   operator, a relation or an opening bracket spaces itself.
spaceOperators :: Element -> Element
spaceOperators e = e { elContent = go Nothing (map deeper (elContent e)) }
  where
    deeper (Elem c) = Elem (spaceOperators c)
    deeper c        = c
    go prev (Elem a : rest)
        | operatorUnit a =
            let before = [Elem thinSpace | Just p <- [prev], spacedFrom p]
                after  = [Elem thinSpace | Just n <- [firstElem rest], not (opensBracket n)]
            in before ++ Elem a : after ++ go (Just (if null after then a else thinSpace)) rest
        | otherwise = Elem a : go (Just a) rest
    go prev (c : rest) = c : go prev rest
    go _ []            = []

    spacedFrom p = case name p of
        "mo"     -> findAttr (unqual "form") p == Just "postfix"
        "mspace" -> False
        _        -> True

    firstElem cs = listToMaybe [x | Elem x <- cs]
    name = qName . elName
    thinSpace = add_attr (Attr (unqual "width") "0.1667em") (unode "mspace" ())

    -- @\<mrow\>\<mi\>log\</mi\>\<mo\>&#x2061;\</mo\>\</mrow\>@, or a script
    -- element whose base is one (@\\log_2@, @\\max_{x}@, @\\sin^2@).
    operatorUnit a = function a
        || (name a `elem` ["msub", "msup", "msubsup", "munder", "mover", "munderover"]
            && maybe False function (listToMaybe (elChildren a)))
    function a = name a == "mrow" && case elChildren a of
        [i, o] -> name i == "mi" && name o == "mo" && strContent o == "\x2061"
        _      -> False
    opensBracket n = name n == "mo" && findAttr (unqual "form") n == Just "prefix"

-- | TeX that texmath does not read, as TeX it does.
compat :: Text -> Text
compat = sizeCommands . fontSwitches

-- | @\\Bigl(@ as @\\Big(@, and the middle sizes as their plain ones.
sizeCommands :: Text -> Text
sizeCommands tex = foldr command tex
    [("Bigl", "Big"), ("bigm", "big"), ("Bigm", "Big"), ("biggm", "bigg"), ("Biggm", "Bigg")]
  where
    -- Only the whole command: @\\Bigl@ but not @\\Biglx@.
    command (old, new) t = case T.breakOn ("\\" <> old) t of
        (before, rest)
            | T.null rest -> t
            | otherwise ->
                let after = T.drop (T.length old + 1) rest
                    whole = maybe True (not . isLetter . fst) (T.uncons after)
                in before <> (if whole then "\\" <> new else "\\" <> old) <> command (old, new) after

-- | @{\\rm in}@ as @\\mathrm{in}@, and the other plain-TeX switches alike.
fontSwitches :: Text -> Text
fontSwitches tex = case T.breakOn "{\\" tex of
    (before, rest)
        | T.null rest -> tex
        | Just (cmd, after) <- switchAt (T.drop 2 rest)
        , (inner, close) <- inGroup after
        -> before <> "\\" <> cmd <> "{" <> T.strip inner <> "}" <> fontSwitches close
        | otherwise -> before <> "{\\" <> fontSwitches (T.drop 2 rest)
  where
    switches = [("rm", "mathrm"), ("bf", "mathbf"), ("it", "mathit"),
                ("sf", "mathsf"), ("tt", "mathtt"), ("cal", "mathcal")]
    switchAt s = listToMaybe
        [ (new, T.drop (T.length old) s)
        | (old, new) <- switches
        , old `T.isPrefixOf` s
        , maybe True (not . isLetter . fst) (T.uncons (T.drop (T.length old) s)) ]
    -- The rest of the group the switch opened, and what follows its brace.
    inGroup = scan (0 :: Int) T.empty
    scan depth acc s = case T.uncons s of
        Nothing -> (acc, T.empty)
        Just ('\\', s') | Just (c, s'') <- T.uncons s' -> scan depth (acc <> T.pack ['\\', c]) s''
        Just ('{', s') -> scan (depth + 1) (T.snoc acc '{') s'
        Just ('}', s') | depth == 0 -> (acc, s')
                       | otherwise  -> scan (depth - 1) (T.snoc acc '}') s'
        Just (c, s') -> scan depth (T.snoc acc c) s'

escape :: Text -> Text
escape = T.concatMap $ \c -> case c of
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    '"' -> "&quot;"
    _   -> T.singleton c
