{-# LANGUAGE GHC2021 #-}
-- | Body heading-level normalisation.
--
--   The site puts the document title in an @\<h1 class="page-title"\>@ from
--   the template, so a body @h1@ is a /second/ top-level heading on the
--   page, not a title. Most of the corpus is authored that way and starts
--   its sections at @##@.
--
--   The imported research essays are not: they were exported from LaTeX
--   sources whose @\\section@ became @#@. Audit F09 counted 19 body @h1@
--   elements on the near-critical domination page, 12 on ball-occupation
--   and 8 on branch-capture. Two consequences, both structural rather than
--   cosmetic:
--
--     * @Compilers.collectHeadings@ collects @h2@ and @h3@ only, so every
--       major section of those documents was missing from the table of
--       contents and from the collapse system built on it — the sections
--       that /were/ listed were the subsections;
--     * the document had many @h1@ elements, so its outline said the page
--       had a dozen equal top-level topics.
--
--   Rather than rewrite the imported Markdown (which is regenerated from
--   the manuscript sources in @~\/Repos\/research\/meyniel@ and would drift
--   back), the levels are normalised here: a document that contains /any/
--   body @h1@ has every one of its headings pushed down a level, so
--   @h1@ sections become @h2@ sections beneath the page title and their
--   @h2@ subsections become @h3@. Documents that already start at @h2@ are
--   returned untouched, which is every other page on the site.
--
--   @h6@ is the floor: HTML has no @h7@, so a document nested that deep
--   loses one level of distinction rather than producing invalid markup.
--   Nothing in the corpus is anywhere near it.
module Filters.Headings (normalizeLevels) where

import Text.Pandoc.Definition (Pandoc, Block (..))
import Text.Pandoc.Walk       (query, walk)

-- | Demote every heading by one level iff the document contains a body
--   @h1@. Identity otherwise.
normalizeLevels :: Pandoc -> Pandoc
normalizeLevels doc
    | hasTopLevelHeader doc = walk demote doc
    | otherwise             = doc
  where
    demote :: Block -> Block
    demote (Header lvl attr inlines) = Header (min 6 (lvl + 1)) attr inlines
    demote b                         = b

-- | Does the document contain an @h1@ anywhere in its body?
hasTopLevelHeader :: Pandoc -> Bool
hasTopLevelHeader = not . null . query collect
  where
    collect :: Block -> [()]
    collect (Header 1 _ _) = [()]
    collect _              = []
