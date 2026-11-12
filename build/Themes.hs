{-# LANGUAGE GHC2021 #-}
-- | A site's themes, as its stylesheets declare them: tokens.css, then
--   theme.css (each the site's own, or else the engine's).
--
--   A theme is a rule whose selector list has @[data-theme="ID"]@ alone in
--   it, at the top level or in an @\@layer@ block (not in @\@media@ or
--   another at-rule); the themes are those ids, in the order they first
--   appear, and the settings panel offers them so (@$for(themes)$@,
--   'themeContext'). The theme listed beside @:root@, as in tokens.css's
--   @:root, [data-theme="light"]@, is the one a reader who chose none
--   wears. Each theme declares @color-scheme@, @light@ or @dark@, in one of
--   its rules (the last declared counts, as in the cascade); the first dark
--   one is what a reader who chose none wears when the system prefers dark,
--   and the build writes each of its rules again for that ('systemDark'),
--   so no stylesheet repeats them by hand.
module Themes
    ( Themes (..), Theme (..), CssRule (..)
    , scanThemes, systemDark, themeLabel, cssRules, themeSelector
    , themesId, loadThemes, themeContext
    ) where

import Control.Monad (forM, forM_, unless)
import Data.Char     (isSpace, toLower, toUpper)
import Data.List     (intercalate, isPrefixOf, nub)
import Data.Maybe    (listToMaybe)
import Hakyll

data Theme = Theme
    { themeId     :: String
    , themeScheme :: String         -- ^ light or dark
    , themeBg     :: Maybe String   -- ^ its --bg, as declared (tools/audit-viz.py's page)
    } deriving (Eq, Show, Read)

data Themes = Themes
    { themeList  :: [Theme]
    , themeLight :: String          -- ^ worn with none chosen
    , themeDark  :: Maybe String    -- ^ worn with none chosen, the system preferring dark
    } deriving (Eq, Show, Read)

-- | A rule of a stylesheet, as 'systemDark' and 'scanThemes' need it.
data CssRule = CssRule
    { ruleTop       :: Bool       -- ^ at the top level or in an @layer
    , ruleSelectors :: [String]   -- ^ comments out, whitespace collapsed
    , ruleBody      :: String     -- ^ between its braces, as written
    , ruleEnd       :: Int        -- ^ the offset just past its closing brace
    } deriving (Eq, Show)

-- | The themes of these stylesheets, in cascade order, or what is wrong.
--   None at all is no error: a probe site may have no stylesheets.
scanThemes :: [String] -> Either String Themes
scanThemes sources = if null ids then Right (Themes [] "" Nothing) else do
    light <- case beside of
        [i] -> Right i
        []  -> Left "no theme is listed beside :root (as in `:root, [data-theme=\"light\"]`)"
        _   -> Left ("one theme may be listed beside :root, not " ++ intercalate ", " beside)
    -- What a theme declares: in its own rules, and the one beside :root in
    -- :root's too; the last, as the cascade has it.
    let value name i = listToMaybe (reverse
            [ v | r <- rules, i `elem` alone r || (i == light && ruleSelectors r == [":root"])
                , Just v <- [declared name (ruleBody r)] ])
    forM_ ids $ \i -> unless (slug i) $ Left
        ("theme ids must be lowercase letters, digits and hyphens: " ++ i)
    themes <- forM ids $ \i -> case value "color-scheme" i of
        Just v | v `elem` ["light", "dark"] -> Right (Theme i v (value "--bg" i))
        Just v  -> Left ("theme " ++ i ++ "'s color-scheme must be light or dark, not " ++ v)
        Nothing -> Left ("theme " ++ i ++ " declares no color-scheme (light or dark) in its rule")
    pure (Themes themes light (listToMaybe [themeId t | t <- themes, themeScheme t == "dark"]))
  where
    rules  = [r | source <- sources, r <- cssRules source, ruleTop r]
    alone r = [i | s <- ruleSelectors r, Just (i, "") <- [themeSelector s]]
    ids    = nub (concatMap alone rules)
    beside = nub [i | r <- rules, ":root" `elem` ruleSelectors r, i <- alone r]
    slug v = case v of
        c : _ -> c /= '-' && all (`elem` (['a'..'z'] ++ ['0'..'9'] ++ "-")) v
        _     -> False

-- | A stylesheet with each of its rules for the theme @dark@ followed by a
--   copy for a reader who chose no theme while the system prefers dark:
--   @[data-theme="dark"] .x@ again as @:root:not([data-theme]) .x@, in
--   @\@media (prefers-color-scheme: dark)@. Only a rule 'scanThemes' reads
--   is copied (none in another at-rule), and only its selectors for that
--   theme, but for one its list already has.
systemDark :: Maybe String -> String -> String
systemDark Nothing css = css
systemDark (Just dark) css = go 0 css [(ruleEnd r, copy r) | r <- cssRules css, ruleTop r, not (null (wanted r))]
  where
    wanted r = [ s' | s <- ruleSelectors r, Just (i, rest) <- [themeSelector s], i == dark
                    , let s' = ":root:not([data-theme])" ++ rest, s' `notElem` ruleSelectors r ]
    copy r = "\n@media (prefers-color-scheme: dark) {\n" ++ intercalate ",\n" (wanted r)
             ++ " {" ++ ruleBody r ++ "}}\n"
    go _ rest [] = rest
    go at rest ((end, text) : more) =
        let (before, after) = splitAt (end - at) rest
        in  before ++ text ++ go end after more

-- | A selector that begins with @[data-theme="ID"]@ (or @'ID'@, or @ID@):
--   the id and what follows it.
themeSelector :: String -> Maybe (String, String)
themeSelector s = do
    r <- stripPrefix' "[data-theme=" s
    case r of
        q : t | q == '"' || q == '\'' -> case break (== q) t of
            (i, _ : ']' : rest) | not (null i) -> Just (i, rest)
            _                                  -> Nothing
        t -> case span (\c -> c /= ']' && not (isSpace c)) t of
            (i, ']' : rest) | not (null i) -> Just (i, rest)
            _                              -> Nothing
  where stripPrefix' p v = if p `isPrefixOf` v then Just (drop (length p) v) else Nothing

-- | A theme's name in the settings panel: its id, capitalized, hyphens as
--   spaces.
themeLabel :: String -> String
themeLabel i = case map (\c -> if c == '-' then ' ' else c) i of
    c : rest -> toUpper c : rest
    []       -> []

-- | The last value of a property declared in a rule's body.
declared :: String -> String -> Maybe String
declared name body = listToMaybe (reverse
    [ collapse (drop 1 value)
    | decl <- splitTop ';' (stripComments body)
    , let (prop, value) = break (== ':') decl
    , map toLower (collapse prop) == name, not (null value) ])

-- | The rules of a stylesheet, in order. A block at-rule's rules are its
--   children; only an @layer's count as top level.
cssRules :: String -> [CssRule]
cssRules = (\(rules, _, _) -> rules) . items True 0
  where
    items top n s = case s of
        []                   -> ([], n, [])
        '}' : r              -> ([], n + 1, r)
        c : r | isSpace c    -> items top (n + 1) r
        '/' : '*' : r        -> let (k, r') = comment r in items top (n + 2 + k) r'
        _ -> let (prelude, k, r) = upTo s in case r of
            ';' : r' -> items top (n + k + 1) r'
            '{' : r'
                | "@" `isPrefixOf` prelude ->
                    let inner = top && atName prelude == "layer"
                        (rules, n', r'') = items inner (n + k + 1) r'
                        (more, n'', r''') = items top n' r''
                    in  (rules ++ more, n'', r''')
                | otherwise ->
                    let (body, k', r'') = block r'
                        end = n + k + 1 + k'
                        rule = CssRule top (map collapse (splitTop ',' prelude)) body end
                        (more, n'', r''') = items top end r''
                    in  (rule : more, n'', r''')
            _ -> items top (n + k) r     -- a stray '}' (closes this level) or the end
    atName = takeWhile (\c -> not (isSpace c) && c /= '{') . drop 1

-- | A prelude, comments out, up to the '{', ';' or '}' that ends it: the
--   text, how many characters it took, and the rest from that one on.
upTo :: String -> (String, Int, String)
upTo = go [] 0
  where
    go acc n s = case s of
        []                     -> (reverse acc, n, [])
        c : _ | c `elem` "{;}" -> (reverse acc, n, s)
        '/' : '*' : r          -> let (k, r') = comment r in go (' ' : acc) (n + 2 + k) r'
        q : r | q == '"' || q == '\'' ->
            let (str, k, r') = quoted q r in go (reverse (q : str) ++ acc) (n + 1 + k) r'
        '\\' : c : r           -> go (c : '\\' : acc) (n + 2) r
        c : r                  -> go (c : acc) (n + 1) r

-- | A block's body, after its '{', to the matching '}': the body as written,
--   how many characters it took with the '}', and the rest.
block :: String -> (String, Int, String)
block = go (0 :: Int) [] 0
  where
    go depth acc n s = case s of
        []          -> (reverse acc, n, [])
        '}' : r
            | depth == 0 -> (reverse acc, n + 1, r)
            | otherwise  -> go (depth - 1) ('}' : acc) (n + 1) r
        '{' : r     -> go (depth + 1) ('{' : acc) (n + 1) r
        '/' : '*' : r -> let (k, r') = comment r
                         in  go depth (reverse ("/*" ++ take k r) ++ acc) (n + 2 + k) r'
        q : r | q == '"' || q == '\'' ->
            let (str, k, r') = quoted q r in go depth (reverse (q : str) ++ acc) (n + 1 + k) r'
        '\\' : c : r -> go depth (c : '\\' : acc) (n + 2) r
        c : r       -> go depth (c : acc) (n + 1) r

-- | After "/*": how many characters to the end of the comment, with its
--   "*/", and the rest.
comment :: String -> (Int, String)
comment = go 0
  where
    go n s = case s of
        '*' : '/' : r -> (n + 2, r)
        _ : r         -> go (n + 1) r
        []            -> (n, [])

-- | After an opening quote: the string as written to its closing quote
--   (included), how many characters that is, and the rest.
quoted :: Char -> String -> (String, Int, String)
quoted q = go [] 0
  where
    go acc n s = case s of
        '\\' : c : r       -> go (c : '\\' : acc) (n + 2) r
        c : r | c == q     -> (reverse (c : acc), n + 1, r)
              | c == '\n'  -> (reverse acc, n, s)     -- unterminated: ends at the line
              | otherwise  -> go (c : acc) (n + 1) r
        []                 -> (reverse acc, n, [])

-- | Split at a separator outside strings, parentheses and brackets.
splitTop :: Char -> String -> [String]
splitTop sep = go (0 :: Int) []
  where
    go depth acc s = case s of
        [] -> [reverse acc]
        c : r
            | c == sep && depth == 0 -> reverse acc : go depth [] r
            | c `elem` "([" -> go (depth + 1) (c : acc) r
            | c `elem` ")]" -> go (max 0 (depth - 1)) (c : acc) r
            | c == '"' || c == '\'' -> let (str, _, r') = quoted c r in go depth (reverse (c : str) ++ acc) r'
            | otherwise -> go depth (c : acc) r

stripComments :: String -> String
stripComments s = case s of
    '/' : '*' : r -> ' ' : stripComments (snd (comment r))
    q : r | q == '"' || q == '\'' -> let (str, _, r') = quoted q r in q : str ++ stripComments r'
    c : r -> c : stripComments r
    [] -> []

collapse :: String -> String
collapse = unwords . words

-- ---------------------------------------------------------------------------
-- In the build
-- ---------------------------------------------------------------------------

-- | The item 'EngineFiles.staticRules' keeps the site's themes in.
themesId :: Identifier
themesId = fromFilePath "themes"

-- | The site's themes; none on a probe site without 'EngineFiles.staticRules'.
loadThemes :: Compiler Themes
loadThemes = do
    known <- getMatches (fromList [themesId])
    if null known
        then pure (Themes [] "" Nothing)
        else read . itemBody <$> (load themesId :: Compiler (Item String))

-- | @$for(themes)$@ with @$theme-id$@ and @$theme-label$@, in the settings
--   panel's order; @$themes$@ the ids, space-separated; @$theme-light$@
--   the theme worn with none chosen, and @$theme-dark$@ the one worn with
--   none chosen when the system prefers dark (absent when no theme is dark).
themeContext :: Context a
themeContext =
       listField "themes" fields (loadThemes >>= mapM makeItem . themeList)
    <> field "theme-ids" (const (unwords . map (escapeHtml . themeId) . themeList <$> loadThemes))
    <> field "theme-light" (const (escapeHtml . themeLight <$> loadThemes))
    <> field "theme-dark" (const (loadThemes >>= maybe (noResult "no dark theme") (pure . escapeHtml) . themeDark))
  where
    fields = field "theme-id" (pure . escapeHtml . themeId . itemBody)
          <> field "theme-label" (pure . escapeHtml . themeLabel . themeId . itemBody)
