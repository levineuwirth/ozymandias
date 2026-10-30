{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Drawn initials.
--
--   The dropcap itself is CSS: the opening paragraph's first letter
--   (@#markdownBody > p:first-of-type::first-letter@) and the first letter
--   of every paragraph in a @::: dropcap@ block (typography.css). A page
--   that names a set of drawn initials (frontmatter @initials:@, resolved
--   by 'initialsDir'), or several in order, has each of those letters
--   drawn instead, from the first set that has it: its SVG is put at the
--   head of the paragraph, in
--   @<span class="initial" aria-hidden="true">@, and the paragraph's own
--   letter goes in @<span class="initial-letter">@, which typography.css
--   sets at no size, so the text keeps the letter.
--
--   A paragraph keeps the CSS dropcap when no set has an SVG for its
--   letter, or when it opens with anything but a capital (a quotation
--   mark, a digit, a lower-case letter, a capital with a combining mark),
--   since @::first-letter@ would hide that with it. The ids in a set's SVG
--   (its gradient) are made unique to each paragraph, so a page may carry
--   any number of initials, and another page's transcluded beside them.
module Filters.Dropcaps (apply, initialsDir) where

import           Control.Monad          (filterM)
import           Data.Bits              (xor)
import           Data.Char              (GeneralCategory (EnclosingMark, NonSpacingMark,
                                                          SpacingCombiningMark),
                                         generalCategory, isLetter, isUpper)
import           Data.IORef             (IORef, atomicModifyIORef', newIORef,
                                         readIORef, modifyIORef')
import           Data.List              (findIndex, isPrefixOf)
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Word              (Word32)
import           Numeric                (showHex)
import           System.Directory       (doesDirectoryExist, doesFileExist)
import           System.FilePath        (splitDirectories, (</>))
import           Text.Pandoc.Definition
import           Text.Pandoc.Shared     (stringify)
import           Text.Pandoc.Walk       (walkM)

-- | Where a page's @initials:@ names its set: a path under the site's
--   static directory when it begins with @/@ (@/initials/eb-garamond@ is
--   @static/initials/eb-garamond@), else a path beside the page. Nothing
--   for an empty value or one that climbs out with @..@.
initialsDir :: FilePath -> String -> Maybe FilePath
initialsDir srcDir v
    | null v                         = Nothing
    | ".." `elem` splitDirectories v = Nothing
    | "/" `isPrefixOf` v             = Just ("static" </> dropWhile (== '/') v)
    | otherwise                      = Just (srcDir </> v)

-- | Draw the initials of a page from its sets, in order; the identity
--   when it has none, or none of them exists.
apply :: [FilePath] -> Pandoc -> IO Pandoc
apply named doc@(Pandoc meta blocks) = do
    dirs <- filterM doesDirectoryExist named
    if null dirs then pure doc else do
        cache   <- newIORef Map.empty
        counter <- newIORef (0 :: Int)
        let draw    = drawInitial dirs cache counter
            opening = findIndex isPara (takeWhile (not . isHeader) blocks)
            visit (i, b)
                | Just i == opening, Para ils <- b = Para <$> draw ils
                | otherwise                        = walkM (dropcapBlock draw) b
        blocks' <- mapM visit (zip [0 :: Int ..] blocks)
        pure (Pandoc meta blocks')
  where
    isPara (Para _) = True
    isPara _        = False
    -- The opening paragraph is the CSS's: the first <p> directly in
    -- #markdownBody. collapse.js wraps what follows a heading in its
    -- section's body, so only a paragraph before the first heading stays
    -- it; a page whose first paragraph comes later keeps whatever the CSS
    -- gives it, never a drawn initial the CSS would not have dropped.
    isHeader (Header {}) = True
    isHeader _           = False

-- | Every paragraph in a @::: dropcap@ block, however deep.
dropcapBlock :: ([Inline] -> IO [Inline]) -> Block -> IO Block
dropcapBlock draw (Div attr@(_, classes, _) bs)
    | "dropcap" `elem` classes = Div attr <$> walkM para bs
  where
    para (Para ils) = Para <$> draw ils
    para b          = pure b
dropcapBlock _ b = pure b

-- | The paragraph with its letter's initial at its head, or as it was.
drawInitial :: [FilePath] -> IORef (Map.Map Char (Maybe T.Text)) -> IORef Int
            -> [Inline] -> IO [Inline]
drawInitial dirs cache counter ils
    | drawn ils = pure ils
    | otherwise = case firstCapital ils of
        Nothing -> pure ils
        Just c  -> do
            found <- letterSvg dirs cache c
            case found of
                Nothing  -> pure ils
                Just svg -> do
                    n <- atomicModifyIORef' counter (\k -> (k + 1, k + 1))
                    let suffix = T.pack (show n) <> "-" <> fnv (stringify ils)
                        html   = "<span class=\"initial\" aria-hidden=\"true\">"
                                 <> uniqueIds suffix svg <> "</span>"
                    pure (RawInline (Format "html") html : markLetter ils)
  where
    drawn (RawInline _ t : _) = "<span class=\"initial\"" `T.isPrefixOf` t
    drawn _                   = False

-- | The paragraph's own first letter in a span of its own, which
--   typography.css sets at no size. Every ::first-letter dropcap is
--   measured in em, so it comes to nothing with it, whichever rule gives
--   it; the letter stays in the text.
markLetter :: [Inline] -> [Inline]
markLetter []       = []
markLetter (x : xs) = case x of
    Str t | Just (c, rest) <- T.uncons t ->
        Span ("", ["initial-letter"], []) [Str (T.singleton c)]
            : [Str rest | not (T.null rest)] ++ xs
    Emph ys       -> Emph (markLetter ys) : xs
    Strong ys     -> Strong (markLetter ys) : xs
    Underline ys  -> Underline (markLetter ys) : xs
    SmallCaps ys  -> SmallCaps (markLetter ys) : xs
    Span a ys     -> Span a (markLetter ys) : xs
    Link a ys tgt -> Link a (markLetter ys) tgt : xs
    _             -> x : xs

-- | The capital a paragraph's dropcap would show: the first character of
--   its first word, through emphasis, small caps, spans and links.
firstCapital :: [Inline] -> Maybe Char
firstCapital []      = Nothing
firstCapital (x : _) = case x of
    Str t -> case T.uncons t of
        Just (c, rest) | isUpper c && isLetter c && not (combining rest) -> Just c
        _ -> Nothing
    Emph xs      -> firstCapital xs
    Strong xs    -> firstCapital xs
    Underline xs -> firstCapital xs
    SmallCaps xs -> firstCapital xs
    Span _ xs    -> firstCapital xs
    Link _ xs _  -> firstCapital xs
    _            -> Nothing
  where
    combining rest = case T.uncons rest of
        Just (m, _) -> generalCategory m `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark]
        Nothing     -> False

-- | The first set's SVG for a capital, read once per page.
letterSvg :: [FilePath] -> IORef (Map.Map Char (Maybe T.Text)) -> Char -> IO (Maybe T.Text)
letterSvg dirs cache c = do
    known <- Map.lookup c <$> readIORef cache
    case known of
        Just svg -> pure svg
        Nothing  -> do
            found <- filterM doesFileExist [dir </> (c : ".svg") | dir <- dirs]
            svg <- case found of
                path : _ -> Just . stripDeclaration . T.strip <$> TIO.readFile path
                []       -> pure Nothing
            modifyIORef' cache (Map.insert c svg)
            pure svg
  where
    stripDeclaration t
        | "<?xml" `T.isPrefixOf` t = T.strip (T.drop 2 (snd (T.breakOn "?>" t)))
        | otherwise                = t

-- | Each id the SVG declares, and every reference to it (@url(#…)@,
--   @href="#…"@), with the suffix added.
uniqueIds :: T.Text -> T.Text -> T.Text
uniqueIds suffix svg = foldr rename svg ids
  where
    ids = [ T.takeWhile (/= '"') (T.drop 4 after)
          | (_, after) <- T.breakOnAll "id=\"" svg ]
    rename i = T.replace ("id=\"" <> i <> "\"") ("id=\"" <> i' <> "\"")
             . T.replace ("url(#" <> i <> ")") ("url(#" <> i' <> ")")
             . T.replace ("href=\"#" <> i <> "\"") ("href=\"#" <> i' <> "\"")
      where i' = i <> "-" <> suffix

-- | FNV-1a over the paragraph's text: a short, stable tag that keeps two
--   pages' initials apart when one is transcluded into the other.
fnv :: T.Text -> T.Text
fnv = T.pack . flip showHex "" . T.foldl' step (2166136261 :: Word32)
  where
    step h ch = (h `xor` fromIntegral (fromEnum ch)) * 16777619
