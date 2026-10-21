{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | @site render-fixture <file.md>@: one Markdown file through the essay
--   pipeline, printed to stdout, for the golden-page tests
--   (tests/test_golden.py, fixtures in tests/golden/).
--
--   The filters are the part of the site with the most behaviour and, until
--   these tests, none of their own: a page either built or it did not. A
--   golden test needs a page whose source never changes, and a fixture in
--   content/ would be published, so this renders the file outside Hakyll
--   instead. It shares every step that matters with 'Compilers.essayCompiler'
--   — 'preprocessSource', the Hakyll reader options, heading normalisation,
--   'Compilers.transformDocument', the writer options and 'Compilers.buildTOC'
--   — and skips only what needs a running build: templates, contexts, and the
--   dependency tracking.
--
--   The fixture's own files (its bibliography, the images beside it) are
--   named from its directory, so a fixture keeps working wherever the
--   engine's tests sit (A5). Other relative paths (the CSL style, the
--   archive and code-ref indexes) resolve from the working directory, so run
--   it from the site's root, as the test does.
module Golden (renderFixture) where

import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import qualified Data.Yaml             as Yaml
import           Data.Maybe            (fromMaybe)
import           Hakyll                (lookupString, lookupStringList)
import           System.Exit           (exitFailure)
import           System.FilePath       (takeDirectory, (</>))
import           System.IO             (hPutStrLn, stderr)
import           Text.Pandoc           (readMarkdown, runPure, writeHtml5String)

import           Config                (readSiteConfig)
import           Compilers             (readerOpts, writerOpts, transformDocument,
                                        buildTOC)
import           Hakyll.Core.Provider.Metadata (parsePage)
import           Utils                 (parseBool)
import           Filters               (preprocessSource)
import qualified Filters.Headings      as Headings

-- | Render one fixture: the body, then the table of contents, the
--   bibliography and the further-reading list, each under a marker line.
renderFixture :: FilePath -> IO ()
renderFixture path = do
    cfg <- readSiteConfig
    (meta, body) <- parsePage <$> readFile path
                    >>= either (die . Yaml.prettyPrintParseException) return
    let frKeys        = map T.pack (fromMaybe [] (lookupStringList "further-reading" meta))
        bibPath       = T.pack (maybe "data/bibliography.bib" (takeDirectory path </>)
                                      (lookupString "bibliography" meta))
        numberFigures = fromMaybe False (lookupString "figure-numbering" meta >>= parseBool)
    parsed <- either (die . show) return $
        runPure (readMarkdown readerOpts (T.pack (preprocessSource cfg body)))
    (doc, bibHtml, furtherHtml) <-
        transformDocument cfg frKeys bibPath numberFigures (takeDirectory path)
                          (Headings.normalizeLevels parsed)
    html <- either (die . show) return $ runPure (writeHtml5String writerOpts doc)
    mapM_ (\(name, part) -> TIO.putStrLn ("<!-- render-fixture: " <> name <> " -->")
                         >> TIO.putStrLn part)
        [ ("body",            html)
        , ("toc",             T.pack (buildTOC doc))
        , ("bibliography",    bibHtml)
        , ("further-reading", furtherHtml)
        ]
  where
    die msg = hPutStrLn stderr ("render-fixture: " ++ path ++ ": " ++ msg) >> exitFailure
