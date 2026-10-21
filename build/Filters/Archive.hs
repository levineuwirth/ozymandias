{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Filters.Archive — annotate (and, for dead links, redirect) body links
--   to archived works.
--
--   For every @Link@ whose URL matches an entry in @data/archive-index.json@
--   (the equivalent-URL alias set included):
--
--     * a 'live', 'moved' or (inconclusive) 'error' target keeps its
--       original link and gains a small superscript affordance pointing at
--       the local @/archive/<slug>/@ page — purely additive;
--
--     * a 'rotted' target (confirmed dead by @archive.py check@'s
--       hysteresis) has its primary link flipped to the archived copy, so
--       a reader of an old essay reaches a working snapshot instead of a
--       404. A "archived" marker replaces the affordance.
--
--   Registered in 'Filters.applyAll' immediately after @Smallcaps@ and
--   before @Links@: it must see the smallcaps-rewritten text, and it emits
--   the affordance/marker as @RawInline@ so the downstream @Links@ pass
--   never re-classifies it.
--
--   No-op when @data/archive-index.json@ is absent. When no rot scan has
--   run, every entry is 'Live' — no link is ever flipped.
--
--   'annotateBlock' exposes the same pass for rendered blocks that never
--   travel the body filter chain — @Citations@ applies it to each
--   CSL-rendered bibliography entry, so a bibliography URL gets the same
--   affordance (and the same rotted-link flip) as a body link.
module Filters.Archive (apply, annotateBlock) where

import qualified Data.Text              as T
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk       (walk)
import           ArchiveIndex           (ArchiveStatus (..), archiveIndexIsEmpty,
                                         archiveSlugFor, archiveStatusForSlug)

-- | Annotate body links. Links inside headings are left alone at
--   /every/ nesting depth — an affordance there would be noise, and a
--   top-level pattern match would miss a @Header@ inside a @Div@ or
--   @BlockQuote@. Header links are tagged with a sentinel class before
--   the annotation walk and stripped of it afterwards, so the sentinel
--   can never leak into the writer. Identity when the index is empty.
apply :: Pandoc -> Pandoc
apply doc
    | archiveIndexIsEmpty = doc
    | otherwise           =
        walk unprotectLink . walk annotateInlines . walk protectHeader $ doc

-- | The annotation pass for a single already-rendered block, outside the
--   body filter chain. No header protection — the callers' blocks
--   (CSL bibliography entries) contain none. Identity when the index is
--   absent.
annotateBlock :: Block -> Block
annotateBlock b
    | archiveIndexIsEmpty = b
    | otherwise           = walk annotateInlines b

-- | Sentinel class marking a link the annotation walk must skip. It
--   only exists between the protect and unprotect walks inside 'apply'.
skipClass :: T.Text
skipClass = "archive-header-skip"

protectHeader :: Block -> Block
protectHeader (Header lvl attr ils) = Header lvl attr (walk protect ils)
  where
    protect (Link (ident, cls, kvs) text target) =
        Link (ident, skipClass : cls, kvs) text target
    protect x = x
protectHeader b = b

unprotectLink :: Inline -> Inline
unprotectLink (Link (ident, cls, kvs) text target)
    | skipClass `elem` cls =
        Link (ident, filter (/= skipClass) cls, kvs) text target
unprotectLink x = x

-- | For each archived @Link@: flip it if the target is 'Rotted', else
--   append the affordance. Non-archived links — and links protected by
--   'protectHeader' — pass through untouched.
annotateInlines :: [Inline] -> [Inline]
annotateInlines = concatMap expand
  where
    expand l@(Link (_, cls, _) _ _)
        | skipClass `elem` cls = [l]
    expand l@(Link attr text (url, _)) =
        case archiveSlugFor url of
            Nothing   -> [l]
            Just slug -> case archiveStatusForSlug slug of
                Rotted -> [flipped slug attr text, marker slug "rotted"
                                "The original is a dead link &mdash; \
                                \opens the local archived copy"]
                _      -> [l, marker slug "" "Archived &mdash; \
                                            \local preservation copy"]
    expand x = [x]

-- | A 'Rotted' link, redirected to the local archived copy. Keeps the
--   link text; the @archive-rotted@ class lets CSS mark it.
flipped :: String -> Attr -> [Inline] -> Inline
flipped slug (ident, classes, kvs) text =
    Link (ident, "archive-rotted" : classes, kvs) text
         ( T.pack ("/archive/" ++ slug ++ "/")
         , "Original link is dead \8212 opens the local archived copy" )

-- | The superscript marker after the link: "A" for a normal affordance,
--   "archived" for a flipped dead link. Emitted as raw HTML so the
--   downstream @Links@ filter (which classifies @Link@ nodes) leaves it
--   alone. Slugs are @[a-z0-9-]@ by construction in @archive.py@.
marker :: String -> String -> T.Text -> Inline
marker slug modifier title = RawInline "html" $ T.concat
    [ "<sup class=\"archive-affordance", modifierClass, "\">"
    , "<a href=\"/archive/", T.pack slug, "/\" title=\"", title, "\">"
    , label, "</a></sup>"
    ]
  where
    modifierClass = if null modifier
                    then ""
                    else " archive-affordance--" <> T.pack modifier
    label = if null modifier then "A" else "archived"
