{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The engine's files inside a site's build.
--
-- A site is built from its own directory, which is also Hakyll's provider
-- directory, and the engine's templates, static files, tools and data sit
-- beneath it at @engine/@. Each engine file is the default for the path it
-- has without that prefix, and a site file at that path overrides it:
--
--   * __Templates__ are matched in both places, and an engine template no
--     site file overrides is also created under its site name, so the
--     @"templates/…"@ names the rules and @$partial()$@ calls use reach
--     whichever one wins. The alias loads the engine template, so an edit
--     to it reaches every page, and a site override arriving later is a new
--     identifier, modified whatever its mtime. One removed is not: the
--     alias returns with the override's body in the store and nothing it
--     depends on changed.
--   * __Static files__ are routed with @engine/@ stripped. One a site file
--     overrides is not matched at all, so the two never claim one route.
--   * __Raw reads__ outside Hakyll go through 'resolveFile'.
--
-- Which file wins is decided as the rules are evaluated, so adding or
-- removing an override needs a clean build (removing one always does):
-- tools/build-freshness.sh counts overrides and template aliases into its
-- rules hash. A root template moved into the engine must also acquire the
-- alias dependency instead of keeping its cached resource compiler result.
module EngineFiles
    ( engineDir
    , siteRelative
    , existingFile
    , resolveFile
    , readThemes
    , filesIn
    , engineTwins
    , fromEngine
    , templateRules
    , staticRules
    , Sheet (..)
    , cssBundles
    , bundlePlan
    , scriptModules
    , modulePlan
    , assetsField
    , staticImageDimensions
    , hasImport
    ) where

import           Control.Exception (IOException, catch)
import           Control.Monad     (forM, forM_, unless, when)
import           Crypto.Hash       (Digest, SHA256, hash)
import qualified Data.ByteString   as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Char         (chr, digitToInt, isAsciiLower, isAsciiUpper, isDigit,
                                    isHexDigit, toLower)
import           Data.List         (intercalate, sort, stripPrefix)
import           Data.Maybe        (fromMaybe)
import qualified Data.Set          as Set
import qualified Data.Text         as T
import qualified Data.Text.Encoding as TE
import           Hakyll
import           System.Directory  (doesFileExist, listDirectory)
import           System.FilePath   ((</>))

import           Themes            (Themes (..), scanThemes, systemDark, themesId)
import           Utils             (stripHtmlComments, stripPrefixRoute)

-- | Where the engine sits in the site.
engineDir :: FilePath
engineDir = "engine"

-- | The site path an engine file stands in for: @engine/templates/x.html@
--   is @templates/x.html@. Any other path is returned unchanged.
siteRelative :: FilePath -> FilePath
siteRelative fp = fromMaybe fp (stripPrefix (engineDir ++ "/") fp)

-- | The file the build reads for a site path: the site's own, else the
--   engine's, else 'Nothing'.
existingFile :: FilePath -> IO (Maybe FilePath)
existingFile rel = do
    inSite <- doesFileExist rel
    if inSite then pure (Just rel) else do
        let fromEngineDir = engineDir </> rel
        inEngine <- doesFileExist fromEngineDir
        pure (if inEngine then Just fromEngineDir else Nothing)

-- | 'existingFile', or the site path when neither exists, so whatever
--   fails to read it names the file where a site would put it.
resolveFile :: FilePath -> IO FilePath
resolveFile rel = fromMaybe rel <$> existingFile rel

-- | The site's themes ("Themes"), from its tokens.css and theme.css, each
--   its own or else the engine's (one neither has reads as empty), or a
--   failure naming what is wrong with them.
readThemes :: IO Themes
readThemes = do
    sources <- forM themeSheets $ \rel ->
        existingFile rel >>= maybe (pure "") (fmap (T.unpack . TE.decodeUtf8) . BS.readFile)
    either (fail . ("themes: " ++)) pure (scanThemes sources)

-- | The stylesheets that declare a site's themes, in cascade order.
themeSheets :: [FilePath]
themeSheets = ["static/css/tokens.css", "static/css/theme.css"]

-- | The entries directly in a site directory and in the engine's, each
--   name once, at the path 'resolveFile' would give it. A missing directory
--   has none.
filesIn :: FilePath -> IO [FilePath]
filesIn dir = do
    site   <- names dir
    engine <- names (engineDir </> dir)
    let own = Set.fromList site
    pure $ [dir </> n | n <- site]
        ++ [engineDir </> dir </> n | n <- engine, not (Set.member n own)]
  where
    names d = sort <$> listDirectory d `catch` \(_ :: IOException) -> pure []

-- | The engine's files that no site file overrides, by a glob written as
--   for the site: @twins "templates/**"@ matches @engine/templates/**@.
--   Read from the provider once per rules run.
engineTwins :: Rules (String -> Pattern)
engineTwins = do
    engine     <- getMatches (fromGlob (engineDir ++ "/**"))
    overridden <- Set.fromList <$> getMatches (fromList (map twin engine))
    let live = [i | i <- engine, not (Set.member (twin i) overridden)]
    pure $ \glob -> fromList (filter (matches (fromGlob (engineDir ++ "/" ++ glob))) live)
  where
    twin = fromFilePath . siteRelative . toFilePath

-- | Route an engine file as its site twin would be.
fromEngine :: Routes -> Routes
fromEngine = composeRoutes (customRoute (siteRelative . toFilePath))

-- | Templates, the site's and the engine's. Their HTML comments are notes
--   for whoever edits them (audit IDs, why a partial is shaped as it is)
--   and are dropped as each template compiles: a comment in a partial
--   rendered inside a loop shipped once per item (about 750 copies of
--   photo-card's across the photography pages), 1.3 MB of comments across
--   the site. Comments in content are untouched.
templateRules :: (String -> Pattern) -> Rules ()
templateRules twins = do
    let engineTemplates = twins "templates/**"
    match ("templates/**" .||. engineTemplates) $ compile $ cached "Site.templateWithoutComments" $ do
        item <- getResourceBody
        tpl  <- compileTemplateItem (fmap stripHtmlComments item)
        return (itemSetBody tpl item)
    aliases <- getMatches engineTemplates
    forM_ aliases $ \engineTemplate ->
        create [fromFilePath (siteRelative (toFilePath engineTemplate))] $
            compile $ (loadBody engineTemplate :: Compiler Template) >>= makeItem

-- | A stylesheet in a bundle: its name under @static/css/@ without @.css@,
--   the site's file or else the engine's, and when it is in.
data Sheet
    = Always String
    | Switched [String] String   -- ^ with any of these switches on

-- | Stylesheets served as one, each a run of the files a page links in
--   @head.html@ (or the score reader's template) at that place, in that
--   order, so the cascade is the one their separate links made: a page
--   asks for a handful of files where it asked for thirty. The files stay
--   apart in the source, where a site overrides each by name, and are
--   still served alone for what loads one on demand. A bundle with a
--   switch is built only with it on; a page links one with
--   @$if(css-NAME)$@ (@Config.bundleContext@), which is there when any of
--   its files is in. What a page needs for itself (@has-code@, a
--   collection's styles, the site's @stylesheets:@) stays linked by page,
--   between them.
cssBundles :: [(String, Maybe String, [Sheet])]
cssBundles =
    [ ("core", Nothing, map Always
        [ "tokens", "theme", "fonts", "reset", "layout", "typography", "nav", "toc"
        , "metadata", "aftermatter", "collapse", "search", "callouts", "frontmatter"
        , "images", "stage", "site" ])
    , ("interact", Nothing,
        [ Switched ["popups"] "popups", Switched ["selection-toolbar"] "selection-popup"
        , Switched ["annotations"] "annotations" ])
    , ("modules", Nothing,
        [ Switched ["code"] "modules/copy", Switched ["transclusion"] "modules/transclusion"
        , Switched ["epistemic"] "modules/epistemic", Switched ["viz"] "modules/viz"
        , Switched ["pdf-viewer"] "modules/pdf", Switched ["marks", "epistemic"] "modules/marks"
        , Switched ["math", "music"] "modules/exhibits" ])
    , ("score", Just "music",
        map Always [ "tokens", "theme", "fonts", "reset", "nav", "toc", "metadata", "aftermatter"
                   , "collapse", "search", "callouts", "stage" ]
        ++ [ Switched ["epistemic"] "modules/epistemic", Switched ["transclusion"] "modules/transclusion"
           , Switched ["code"] "modules/syntax", Switched ["code"] "modules/copy"
           , Switched ["pdf-viewer"] "modules/pdf" ]
        ++ map Always [ "touch", "modules/score-reader", "site" ])
    ]

-- | Each bundle the site builds, with the files in it, by its switches.
bundlePlan :: (String -> Bool) -> [(String, [String])]
bundlePlan on =
    [ (name, files)
    | (name, switch, sheets) <- cssBundles, maybe True on switch
    , let files = concatMap (sheetFiles on) sheets
    , not (null files) ]

-- | A sheet's file, if the site's switches let it in.
sheetFiles :: (String -> Bool) -> Sheet -> [String]
sheetFiles _ (Always file) = [file]
sheetFiles on (Switched switches file) = [file | any on switches]

-- | Scripts served with their switches' modules after them. Each module
--   is a file of its own in the source (@static/js/NAME/…@, which a site
--   overrides by path as any other) and, on the page, part of the one
--   file its script always was, so a switch costs no request. A module
--   comes in only with its switch, or one of them, on, in the order here,
--   which is the order they run in: one that another uses comes first
--   (@popups/source@ before the source and code popups). The modules are
--   not published alone.
scriptModules :: [(String, [Sheet])]
scriptModules =
    [ ("popups",
        [ Switched ["epistemic"] "popups/epistemic"
        , Switched ["source-refs", "code-refs"] "popups/source"
        , Switched ["source-refs"] "popups/source-refs"
        , Switched ["code-refs"] "popups/code-refs"
        , Switched ["pdf-viewer"] "popups/pdf"
        , Switched ["signature-link"] "popups/signature"
        , Switched ["music"] "popups/music"
        , Switched ["photography"] "popups/photography" ])
    ]

-- | Each script with modules, and the modules the site serves with it.
modulePlan :: (String -> Bool) -> [(String, [String])]
modulePlan on = [ (name, concatMap (sheetFiles on) sheets) | (name, sheets) <- scriptModules ]

-- | Static files, the site's and the engine's, served without @static/@,
--   but for @withheld@: those of the switches the site has off
--   ('Config.siteWithheld'). @on@ says which switches the site has on,
--   for the stylesheet bundles ('cssBundles'); @themes@ are its themes
--   ('readThemes'), kept for pages as 'Themes.themesId'.
staticRules :: (String -> Pattern) -> [String] -> (String -> Bool) -> Themes -> Rules ()
staticRules twins withheld on themes = do
    let inBoth glob = fromGlob glob .||. fromGlob (engineDir ++ "/" ++ glob)
        unused      = foldr ((.||.) . inBoth . ("static/" ++)) (fromList []) withheld
        served glob = (fromGlob glob .||. twins glob) .&&. complement unused
        firstParty  = served "static/js/**" .||. served "static/css/**"

    -- First-party JS and CSS, published under a hash of the whole set
    -- (/assets/<hash>/js/…), so a page gets the scripts it was built with
    -- and any new one a new URL: they call each other through globals, so
    -- they move together. Any change to the set moves all of them, and every
    -- page ('assetsField').
    -- The bundles are made from those files, but which files go in follows
    -- the switches, so the plan is part of what the version names; as are
    -- the modules each script is served with.
    let plan    = bundlePlan on
        modules = modulePlan on
        hosts   = foldr ((.||.) . inBoth . (\n -> "static/js/" ++ n ++ ".js")) (fromList []) (map fst modules)
        parts   = foldr ((.||.) . inBoth . (\n -> "static/js/" ++ n ++ "/**")) (fromList []) (map fst modules)
    assets  <- getMatches firstParty
    current <- preprocess (assetVersion (show (plan, modules)) assets)
    changed <- makePatternDependency firstParty
    let versioned = fromEngine (stripPrefixRoute "static/")
            `composeRoutes` customRoute (\i -> "assets/" ++ current ++ "/" ++ toFilePath i)

    rulesExtraDependencies [changed] $ do
        create [assetVersionId] $ compile (makeItem current)
        -- Read from the stylesheets as the rules are, and made again with
        -- the version: any change to the CSS is a change to it.
        create [themesId] $ compile (makeItem (show themes))

        -- CSS, at any depth (modules/, seasons/) — must be matched before
        -- the broad rule to avoid double-matching (compressCssCompiler vs.
        -- copyFileCompiler).
        match (served "static/css/**") $ do
            route   versioned
            compile $ do
                path <- toFilePath <$> getUnderlying
                -- The theme files' rules for the dark theme again, for a
                -- reader who chose none while the system prefers dark
                -- (Themes.systemDark): no stylesheet repeats them by hand.
                let copies | siteRelative path `elem` themeSheets = systemDark (themeDark themes)
                           | otherwise                            = id
                fmap (compressCss . copies) <$> getResourceString

        match (firstParty .&&. complement (inBoth "static/css/**" .||. hosts .||. parts)) $ do
            route   versioned
            compile copyFileCompiler

        -- A script's modules ('scriptModules'), read for it and not
        -- published alone.
        match (served "static/js/**" .&&. parts) $ compile getResourceString

        -- Each script with modules: itself, then those the switches let in,
        -- the site's file or else the engine's. With none, its own bytes.
        forM_ modules $ \(name, files) -> do
            sources <- concat <$> mapM (siteOrEngine . (\f -> "static/js/" ++ f ++ ".js")) files
            match (served ("static/js/" ++ name ++ ".js")) $ do
                route   versioned
                compile $ do
                    host   <- getResourceString
                    bodies <- mapM loadBody sources
                    makeItem (intercalate "\n" (itemBody host : bodies))

        -- Each bundle: its files, the site's or else the engine's, as the
        -- rule above compiled them, one after another.
        -- A file neither has is left out, for a probe site without the
        -- engine's stylesheets; `site css-bundles` lists the plan, and the
        -- engine's tests hold every file in it to exist. A file with an
        -- @import fails the build: an @import counts only before a sheet's
        -- other rules, so after the files ahead of it every browser drops
        -- it, and with it the rules it would have brought.
        forM_ plan $ \(name, files) -> do
            sources <- concat <$> mapM (siteOrEngine . (\f -> "static/css/" ++ f ++ ".css")) files
            unless (null sources) $ create [fromFilePath ("css-bundle/" ++ name)] $ do
                route   $ constRoute ("assets/" ++ current ++ "/css/" ++ name ++ ".css")
                compile $ do
                    bodies <- forM sources $ \source -> do
                        body <- loadBody source
                        when (hasImport body) $ fail $ toFilePath source
                            ++ ": @import in a stylesheet bundled into " ++ name ++ ".css, where it"
                            ++ " is no longer first and browsers ignore it; put the rules in the"
                            ++ " file itself, or load the sheet from a page's `stylesheets:`"
                        pure body
                    makeItem (intercalate "\n" bodies)

    -- All other static files (fonts, images, …). Build-time
    -- sidecars produced by the Python tooling (.dims.yaml, .exif.yaml,
    -- .palette.yaml) are excluded — they're consumed by Hakyll at
    -- compile time and have no role in the deployed site.
    match (   served "static/**"
        .&&. complement (inBoth "static/js/**" .||. inBoth "static/css/**")
        .&&. complement (inBoth "static/**/*.dims.yaml")
        .&&. complement (inBoth "static/**/*.exif.yaml")
        .&&. complement (inBoth "static/**/*.palette.yaml")
        ) $ do
        route   $ fromEngine (stripPrefixRoute "static/")
        compile copyFileCompiler

    -- Static-image dimension sidecars (@static\/images\/foo.jpg.dims.yaml@).
    --
    -- Excluded from the copy rule above (they are compile-time inputs, not
    -- deliverables), which left them claimed by no rule at all and so
    -- outside Hakyll's universe — where a pattern dependency naming them
    -- could never fire, because the modified set is intersected with the
    -- claimed identifiers. Matched here without a route for exactly the
    -- reason the essay sidecars are: to make them trackable. Audit B11.
    -- Claim shadowed sidecars too: the dependency pattern covers both
    -- roots, and every identifier it sees must have an unversioned rule.
    match staticImageDimensions $ compile getResourceLBS

-- | A static file's identifier: the site's, or else the engine's; none
--   when neither has it.
siteOrEngine :: FilePath -> Rules [Identifier]
siteOrEngine rel = do
    let own    = fromFilePath rel
        engine = fromFilePath (engineDir ++ "/" ++ rel)
    site <- getMatches (fromList [own])
    eng  <- getMatches (fromList [engine])
    pure $ case (site, eng) of
        (_ : _, _) -> [own]
        (_, _ : _) -> [engine]
        _          -> []

-- | Whether a stylesheet has an @import rule: whether CSS's tokenizer
--   (CSS Syntax 3, § 4.3) finds an at-keyword named @import in it,
--   however spelled — any case, escapes such as @\69mport — and not in a
--   comment, a string or a url(). Read alone, as a page's stylesheet is;
--   a nested or misplaced one counts too.
--
--   Only what decides where those three begin and end is followed:
--   escapes, and the tokens that would otherwise be taken for the start of
--   a url() (a hash such as #url, a number's unit such as 1url, an
--   at-keyword such as @url). Every other code point is a token of its own
--   or ends one, and is passed over.
hasImport :: String -> Bool
hasImport = scan . newlines
  where
    scan s = case s of
        [] -> False
        '/' : '*' : r -> scan (afterComment r)
        q : r | q == '"' || q == '\'' -> scan (afterString q r)
        '#' : r | hashName r -> scan (snd (identSeq r))
        '@' : r | startsIdent r -> let (name, r') = identSeq r
                                   in  map asciiLower name == "import" || scan r'
        '<' : '!' : '-' : '-' : r -> scan r
        c : r | startsNumber s -> scan (numeric s)
              | c == '-', '-' : '>' : r' <- r -> scan r'
              | startsIdent s -> identLike s
        _ : r -> scan r

    -- An ident, a function or a url(); only a url() hides what follows.
    identLike s = case identSeq s of
        (name, '(' : r) | map asciiLower name == "url" -> case dropWhile space r of
            q : _ | q == '"' || q == '\'' -> scan r     -- url("…") is a function
            r'                             -> scan (afterUrl r')
        (_, r) -> scan r

    -- A number and any unit it has (which is never a url()).
    numeric s = let r = power (fraction (digits (sign s)))
                in  if startsIdent r then snd (identSeq r) else r
      where sign (c : r) | c == '+' || c == '-' = r
            sign r = r
            digits = dropWhile isDigit
            fraction ('.' : d : r) | isDigit d = digits r
            fraction r = r
            power (e : d : r) | e `elem` ("eE" :: String), isDigit d = digits r
            power (e : p : d : r)
                | e `elem` ("eE" :: String), p == '+' || p == '-', isDigit d = digits r
            power r = r

    afterComment ('*' : '/' : r) = r
    afterComment (_ : r)         = afterComment r
    afterComment []              = []

    -- A string ends at its quote, or unclosed before a line break.
    afterString q s = case s of
        c : r | c == q        -> r
        '\n' : _              -> s
        '\\' : '\n' : r       -> afterString q r
        '\\' : r              -> afterString q (snd (escaped r))
        _ : r                 -> afterString q r
        []                    -> []

    -- An unquoted url() ends at its first unescaped parenthesis.
    afterUrl s = case s of
        ')' : r                       -> r
        '\\' : r | not (lineStart r)  -> afterUrl (snd (escaped r))
        _ : r                         -> afterUrl r
        []                            -> []

    identSeq s = case s of
        c : r | identChar c -> first (c :) (identSeq r)
        '\\' : r | not (lineStart r) -> let (c, r') = escaped r in first (c :) (identSeq r')
        _ -> ([], s)
      where first f (a, b) = (f a, b)

    -- What follows a backslash: up to six hex digits and one space, or
    -- one code point.
    escaped s = case s of
        c : _ | isHexDigit c ->
            let (hex, r) = splitAt (length (takeWhile isHexDigit (take 6 s))) s
                n        = foldl (\a d -> a * 16 + digitToInt d) 0 hex
                c'       = if n == 0 || (n >= 0xD800 && n <= 0xDFFF) || n > 0x10FFFF
                               then '\xFFFD' else chr n
            in  (c', case r of w : r' | space w -> r'; _ -> r)
        c : r -> (c, r)
        []    -> ('\xFFFD', [])

    startsIdent s = case s of
        '-' : c : r -> identStart c || c == '-' || validEscape (c : r)
        c : _       -> identStart c || validEscape s
        []          -> False
    startsNumber s = case s of
        c : d : r | c == '+' || c == '-' -> isDigit d || (d == '.' && digitNext r)
        '.' : r -> digitNext r
        c : _   -> isDigit c
        []      -> False
      where digitNext (d : _) = isDigit d
            digitNext []      = False
    hashName s = case s of
        c : _ -> identChar c || validEscape s
        []    -> False
    validEscape ('\\' : r) = not (lineStart r)
    validEscape _          = False
    lineStart ('\n' : _) = True
    lineStart _          = False

    identStart c = isAsciiUpper c || isAsciiLower c || c == '_' || c >= '\x80'
    identChar c  = identStart c || isDigit c || c == '-'
    space c      = c == ' ' || c == '\t' || c == '\n'
    asciiLower c = if isAsciiUpper c then toLower c else c

    -- Line breaks as CSS reads them; NUL as it reads that.
    newlines s = case s of
        '\r' : '\n' : r -> '\n' : newlines r
        c : r | c == '\r' || c == '\f' -> '\n' : newlines r
              | c == '\0'               -> '\xFFFD' : newlines r
              | otherwise               -> c : newlines r
        [] -> []

-- | The item holding the first-party assets' version.
assetVersionId :: Identifier
assetVersionId = fromFilePath "asset-version"

-- | A hash of the files' paths as served, and their contents, and of
--   anything else that decides what is served there (the bundles' plan).
assetVersion :: String -> [Identifier] -> IO String
assetVersion extra ids = do
    parts <- forM (sort [(siteRelative (toFilePath i), toFilePath i) | i <- ids]) $ \(served, path) -> do
        bytes <- BS.readFile path
        pure (BC.pack served <> "\0" <> BC.pack (sha bytes))
    pure (take 12 (sha (BS.concat (BC.pack extra : parts))))
  where
    sha :: BS.ByteString -> String
    sha = show . (hash :: BS.ByteString -> Digest SHA256)

-- | @$assets$@, the root-relative path of this build's first-party JS and
--   CSS. Loading the version is what recompiles a page when it changes.
assetsField :: Context a
assetsField = field "assets" $ \_ -> do
    known <- getMatches (fromList [assetVersionId])
    if null known
        then pure "/assets/none"   -- a probe site without 'staticRules'
        else ("/assets/" ++) . itemBody <$> (load assetVersionId :: Compiler (Item String))

-- | Dimension inputs read outside Hakyll by Filters.Images. Both roots
-- are tracked, including new sidecars and site overrides.
staticImageDimensions :: Pattern
staticImageDimensions = "static/**/*.dims.yaml" .||. "engine/static/**/*.dims.yaml"
