{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Vita page: renders education, publications, presentations, and
-- experience for @/about.html@ from @yaml-source/data/*.yml@ — the same
-- files that drive the CV and résumé PDFs via the Jinja/xelatex pipeline.
--
-- The point is single-sourcing. Before this module the vita page carried a
-- hand-typed copy of all four sections, and it had drifted from the PDFs in
-- roughly fifteen places (stale roles, a superseded line count, a talk still
-- listed as forthcoming after it was given). Anything rendered here is read
-- from the same YAML the PDFs are built from, so the two cannot disagree.
--
-- Sections the site already owns are deliberately absent: in-progress work
-- belongs to @/current@ (data\/now.yaml, see "Now"), the engineering index to
-- @/cv/projects/@, and the personal narrative to @/me/@. This module renders
-- only what nothing else did.
--
-- The YAML is LaTeX-flavoured, because its first consumer is xelatex. Values
-- may contain @\\textbf{}@, @\\href{}{}@, @$\\times$@, @~@, @--@ and friends,
-- so every value goes through 'latexToHtml' on the way out. See that function
-- for the full supported set and why escaping happens before conversion.
module Vita
    ( vitaCtx
    , projectsCtx
    ) where

import Data.Aeson         (FromJSON (..), Object, Value (..), withObject, (.:), (.:?), (.!=))
import Data.Aeson.Types   (Parser, typeMismatch)
import Data.Char          (toLower)
import Data.List          (intercalate, isPrefixOf, sortOn)
import Data.Maybe         (mapMaybe)
import Data.Scientific    (isInteger, toRealFloat)
import qualified Data.Aeson.Key        as K
import qualified Data.Text             as T
import Hakyll hiding (escapeHtml)
import Contexts (siteCtx)
import Utils    (escapeHtml, loadYaml)

-- ---------------------------------------------------------------------------
-- Loose scalars
-- ---------------------------------------------------------------------------

-- | A YAML scalar that may be written as either a string or a number for the
--   same logical field. @publications.yml@ has both @year: 2026@ (parsed as a
--   number) and @year: "2026--2027"@ (a string); @experience.yml@ has
--   @start: "2025"@ next to @start: July 2026@. Accept either and normalise
--   to 'String' rather than forcing the YAML to be quoted consistently — the
--   PDF pipeline does not care, and this module should not make it care.
newtype Loose = Loose { unLoose :: String }

instance FromJSON Loose where
    parseJSON (String t) = pure (Loose (T.unpack t))
    parseJSON (Number n)
        | isInteger n = pure (Loose (show (truncate (toRealFloat n :: Double) :: Integer)))
        | otherwise   = pure (Loose (show (toRealFloat n :: Double)))
    parseJSON (Bool b)   = pure (Loose (if b then "true" else "false"))
    parseJSON v          = typeMismatch "string or number" v

reqLoose :: Object -> String -> Parser String
reqLoose o k = unLoose <$> o .: K.fromString k

optLoose :: Object -> String -> Parser (Maybe String)
optLoose o k = fmap unLoose <$> o .:? K.fromString k

-- | Whether an entry appears on this page.
--
--   The YAML has carried two visibility axes since it drove two documents:
--   @cv_visible@ and @resume_visible@, so the CV and the résumé can disagree
--   about an entry without duplicating it. Generating the vita page from the
--   same data added a third surface, and folding it into @cv_visible@ would
--   have silently collapsed a distinction the file already knew how to make
--   — an entry can be worth keeping on a document handed to a reader while
--   being wrong for a page that is crawled.
--
--   @web_visible@ therefore defaults to @cv_visible@: existing entries behave
--   exactly as before, and the axis only exists where someone sets it.
webVisible :: Object -> Parser Bool
webVisible o = do
    cv <- o .:? "cv_visible" .!= True
    o .:? "web_visible" .!= cv

-- ---------------------------------------------------------------------------
-- Entry types
-- ---------------------------------------------------------------------------

data Link = Link
    { lkLabel :: String
    , lkHref  :: String
    }

instance FromJSON Link where
    parseJSON = withObject "Link" $ \o -> Link
        <$> o .: "label"
        <*> o .: "href"

data Edu = Edu
    { edInstitution :: String
    , edLocation    :: Maybe String
    , edDegree      :: String
    , edStart       :: String
    , edEnd         :: Maybe String
    , edGpa         :: Maybe String
    , edNotes       :: Maybe String
    , edWeb         :: Bool
    }

instance FromJSON Edu where
    parseJSON = withObject "Edu" $ \o -> Edu
        <$> o .:  "institution"
        <*> optLoose o "location"
        <*> reqLoose o "degree"
        <*> reqLoose o "start"
        <*> optLoose o "end"
        <*> optLoose o "gpa"
        <*> optLoose o "notes_cv"
        <*> webVisible o

newtype EduDoc = EduDoc { unEduDoc :: [Edu] }

instance FromJSON EduDoc where
    parseJSON = withObject "EduDoc" $ \o -> EduDoc <$> o .: "education"

data Pub = Pub
    { pbAuthors :: String
    , pbTitle   :: Maybe String
    , pbVenue   :: String
    , pbYear    :: String
    , pbMonth   :: Maybe String
    , pbTarget  :: Maybe String
    , pbLinks   :: [Link]
    , pbNote    :: Maybe String
    , pbWeb     :: Bool
    }

instance FromJSON Pub where
    parseJSON = withObject "Pub" $ \o -> Pub
        <$> reqLoose o "authors"
        <*> optLoose o "title"
        <*> reqLoose o "venue"
        <*> reqLoose o "year"
        <*> optLoose o "month"
        <*> optLoose o "target"
        <*> o .:? "links" .!= []
        <*> optLoose o "equal_contrib_note"
        <*> webVisible o

newtype PubDoc = PubDoc { unPubDoc :: [Pub] }

instance FromJSON PubDoc where
    parseJSON = withObject "PubDoc" $ \o -> PubDoc <$> o .: "publications"

data Pres = Pres
    { prAuthors :: String
    , prTitle   :: String
    , prVenue   :: String
    , prKind    :: Maybe String
    , prYear    :: String
    , prMonth   :: Maybe String
    , prStatus  :: Maybe String
    , prWeb     :: Bool
    }

instance FromJSON Pres where
    parseJSON = withObject "Pres" $ \o -> Pres
        <$> reqLoose o "authors"
        <*> reqLoose o "title"
        <*> reqLoose o "venue"
        <*> optLoose o "kind"
        <*> reqLoose o "year"
        <*> optLoose o "month"
        <*> optLoose o "status"
        <*> webVisible o

newtype PresDoc = PresDoc { unPresDoc :: [Pres] }

instance FromJSON PresDoc where
    parseJSON = withObject "PresDoc" $ \o -> PresDoc <$> o .: "presentations"

data Exp = Exp
    { exOrg      :: String
    , exRole     :: Maybe String
    , exLocation :: Maybe String
    , exLocUrl   :: Maybe String
    , exStart    :: String
    , exEnd      :: Maybe String
    , exSection  :: Maybe String
    , exOrder    :: Int
    , exPreamble :: Maybe String
    , exBullets  :: [String]
    , exWeb      :: Bool
    }

instance FromJSON Exp where
    parseJSON = withObject "Exp" $ \o -> Exp
        <$> reqLoose o "organization"
        <*> optLoose o "role"
        <*> optLoose o "location"
        <*> optLoose o "location_url"
        <*> reqLoose o "start"
        <*> optLoose o "end"
        <*> optLoose o "cv_section"
        <*> o .:? "cv_order" .!= 99
        <*> optLoose o "cv_preamble"
        <*> o .:? "bullets" .!= []
        <*> webVisible o

newtype ExpDoc = ExpDoc { unExpDoc :: [Exp] }

instance FromJSON ExpDoc where
    parseJSON = withObject "ExpDoc" $ \o -> ExpDoc <$> o .: "experience"

data Proj = Proj
    { pjName        :: String
    , pjGroup       :: String
    , pjEssay       :: Maybe String
    , pjStart       :: String
    , pjEnd         :: Maybe String
    , pjDescription :: String
    , pjLinks       :: [Link]
    , pjWeb         :: Bool
    }

instance FromJSON Proj where
    parseJSON = withObject "Proj" $ \o -> Proj
        <$> reqLoose o "name"
        <*> o .:? "group" .!= "Projects"
        <*> optLoose o "essay"
        <*> reqLoose o "start"
        <*> optLoose o "end"
        <*> reqLoose o "description"
        <*> o .:? "links" .!= []
        <*> webVisible o

newtype ProjDoc = ProjDoc { unProjDoc :: [Proj] }

instance FromJSON ProjDoc where
    parseJSON = withObject "ProjDoc" $ \o -> ProjDoc <$> o .: "projects"

-- | @personal.yml@ also carries a @display@ string per link (the value the
--   CV prints in full, since paper cannot be clicked). It is deliberately
--   not read here — see 'renderContact'.
data ProfileLink = ProfileLink
    { plLabel   :: String
    , plHref    :: String
    , plWeb     :: Bool
    }

instance FromJSON ProfileLink where
    parseJSON = withObject "ProfileLink" $ \o -> ProfileLink
        <$> reqLoose o "label"
        <*> reqLoose o "href"
        <*> webVisible o

-- | Contact details from @personal.yml@. The phone number is deliberately
--   not parsed: it is printed on the CV PDF, which is a document handed to
--   a chosen reader, whereas this page is crawled. Nothing here should hand
--   a scraper a phone number it did not already have to go looking for.
data Person = Person
    { pnEmail   :: String
    , pnLinks   :: [ProfileLink]
    }

instance FromJSON Person where
    parseJSON = withObject "Person" $ \o -> Person
        <$> reqLoose o "email"
        <*> o .:? "links" .!= []

-- ---------------------------------------------------------------------------
-- LaTeX → HTML
-- ---------------------------------------------------------------------------

-- | Convert the LaTeX subset that actually appears in the CV YAML into HTML.
--
--   Callers must escape HTML /before/ calling this, never after: this
--   function emits real tags, so escaping afterwards would turn them into
--   visible @&lt;strong&gt;@. Escaping first is safe because none of the
--   LaTeX constructs contain @<@, @>@ or @&@ — and an @&@ inside an
--   @\\href@ URL becomes @&amp;@, which is what an HTML attribute wants
--   anyway.
--
--   The supported set is deliberately closed and matches what the YAML
--   contains today (@\\textbf@, @\\textit@, @\\texttt@, @\\href@,
--   @$\\times$@, @$\\delta$@, @\\#@, @{,}@, @~@, @--@, @---@). An unhandled
--   command passes through verbatim and is therefore visible on the page —
--   the intended failure mode, since a silently swallowed @\\emph@ would
--   drop its argument's text.
latexToHtml :: String -> String
latexToHtml =
      substAll "---" "&mdash;"
    . substAll "--"  "&ndash;"
    . substAll "$\\times$" "&times;"
    . substAll "$\\delta$" "&delta;"
    . substAll "$\\rightarrow$" "&rarr;"
    -- Approximation, not a non-breaking space. Bare `~` is LaTeX's nbsp, so
    -- "~10 crates" silently renders as "( 10 crates" and loses the "about".
    . substAll "$\\sim$" "~"
    . substAll "\\#" "#"
    . substAll "{,}" ","
    . substAll "~" "&nbsp;"
    . rewriteCmd2 "href" (\u t -> "<a href=\"" ++ u ++ "\">" ++ t ++ "</a>")
    . rewriteCmd1 "textbf" (\x -> "<strong>" ++ x ++ "</strong>")
    . rewriteCmd1 "textit" (\x -> "<em>" ++ x ++ "</em>")
    . rewriteCmd1 "texttt" (\x -> "<code>" ++ x ++ "</code>")

-- | Escape, then convert. The one-step form every renderer should use.
tex :: String -> String
tex = latexToHtml . escapeHtml

substAll :: String -> String -> String -> String
substAll _   _   [] = []
substAll pat rep s@(c:cs)
    | pat `isPrefixOf` s = rep ++ substAll pat rep (drop (length pat) s)
    | otherwise          = c : substAll pat rep cs

-- | Split a leading @{...}@ group, tracking brace depth so nested groups
--   survive. Returns the group's contents and whatever follows it.
takeGroup :: String -> Maybe (String, String)
takeGroup ('{':rest) = go (0 :: Int) "" rest
  where
    go _ _   []       = Nothing
    go d acc ('}':cs)
        | d == 0      = Just (reverse acc, cs)
        | otherwise   = go (d - 1) ('}':acc) cs
    go d acc ('{':cs) = go (d + 1) ('{':acc) cs
    go d acc (c:cs)   = go d (c:acc) cs
takeGroup _ = Nothing

-- | Rewrite every @\\cmd{arg}@ with a function of its argument.
rewriteCmd1 :: String -> (String -> String) -> String -> String
rewriteCmd1 name f = go
  where
    marker = '\\' : name
    go [] = []
    go s@(c:cs)
        | marker `isPrefixOf` s
        , Just (arg, rest) <- takeGroup (drop (length marker) s)
        = f (go arg) ++ go rest
        | otherwise = c : go cs

-- | Rewrite every @\\cmd{a}{b}@ with a function of both arguments.
rewriteCmd2 :: String -> (String -> String -> String) -> String -> String
rewriteCmd2 name f = go
  where
    marker = '\\' : name
    go [] = []
    go s@(c:cs)
        | marker `isPrefixOf` s
        , Just (a, rest1) <- takeGroup (drop (length marker) s)
        , Just (b, rest2) <- takeGroup rest1
        = f a (go b) ++ go rest2
        | otherwise = c : go cs

-- ---------------------------------------------------------------------------
-- Shared rendering pieces
-- ---------------------------------------------------------------------------

-- | @start – end@, or just @start@ when the entry has no end.
dateRange :: String -> Maybe String -> String
dateRange s me = tex s ++ maybe "" (\e -> " &ndash; " ++ tex e) me

-- | Title and date on one baseline, title left and date flush right — the
--   layout item-card.css already establishes for every other list on the
--   site. An earlier version stacked the date on its own line below the
--   title, which read as a stray indented fragment: @#markdownBody p + p@
--   applies the essay prose indent of 1.5em to consecutive paragraphs, and a
--   date is not prose.
--
--   Dates stay in a @span@ rather than a @time@ element on purpose. Half of
--   them are "Fall 2024", "September 2026", "Present" — no valid @datetime@
--   value exists for those, and a @time@ without one is worse than no @time@.
headerRow :: String -> String -> String
headerRow titleHtml dates = concat
    [ "<div class=\"item-card-header\">"
    , "<h3 class=\"vita-entry-title\">", titleHtml, "</h3>"
    , "<span class=\"item-card-date\">", dates, "</span>"
    , "</div>"
    ]

-- | The line under the header: role or degree, then whatever secondary facts
--   the entry carries (location, GPA), middot-separated in a quieter ink so
--   the role still leads.
subLine :: Maybe String -> [String] -> String
subLine mrole extras
    | null parts = ""
    | otherwise  = "<p class=\"vita-role\">" ++ lead ++ trailing ++ "</p>"
  where
    parts    = maybe [] (pure . tex) mrole ++ extras
    lead     = head parts
    rest     = tail parts
    trailing
        | null rest = ""
        | otherwise = "<span class=\"vita-quiet\"> &middot; "
                   ++ intercalate " &middot; " rest
                   ++ "</span>"

-- | A location, linked when the entry gives a URL for it.
locationPart :: Maybe String -> Maybe String -> [String]
locationPart Nothing    _    = []
locationPart (Just loc) murl = pure $ case murl of
    Just u  -> "<a class=\"vita-location\" href=\"" ++ escapeHtml u ++ "\">" ++ tex loc ++ "</a>"
    Nothing -> tex loc

-- | Link chips. The affordance the print CV cannot offer: every artifact one
--   click away, rather than a bracketed label the reader has to retype.
renderLinks :: [Link] -> String
renderLinks [] = ""
renderLinks ls = concat
    [ "<p class=\"vita-links\">"
    , concatMap one ls
    , "</p>"
    ]
  where
    one l = concat
        [ "<a class=\"vita-chip\" href=\"", escapeHtml (lkHref l), "\">"
        , tex (lkLabel l)
        , "</a>"
        ]

renderBullets :: [String] -> String
renderBullets [] = ""
renderBullets bs = concat
    [ "<ul class=\"vita-bullets\">"
    , concatMap (\b -> "<li>" ++ tex b ++ "</li>") bs
    , "</ul>"
    ]

section :: String -> String -> String -> String
section slug heading inner = concat
    [ "<section class=\"vita-section library-section\" id=\"", slug, "\">"
    , "<h2 class=\"vita-section-heading\">", heading, "</h2>"
    , inner
    , "</section>"
    ]

-- ---------------------------------------------------------------------------
-- Section renderers
-- ---------------------------------------------------------------------------

-- | A vita section's list of cards.
cardList :: String -> String
cardList items = "<ul class=\"item-card-list vita-list\">" ++ items ++ "</ul>"

-- | One card in it, from its parts in order.
card :: [String] -> String
card parts = concat $
    ["<li class=\"item-card vita-card\">", "<div class=\"item-card-main\">"]
    ++ parts ++ ["</div>", "</li>"]

renderEducation :: [Edu] -> String
renderEducation es
    | null visible = ""
    | otherwise    = section "education" "Education" $
        cardList (concatMap one visible)
  where
    visible = filter edWeb es
    one e = card
        [ headerRow (tex (edInstitution e)) (dateRange (edStart e) (edEnd e))
        , subLine (Just (edDegree e))
            (  maybe [] (\g -> ["GPA " ++ tex g]) (edGpa e)
            ++ locationPart (edLocation e) Nothing
            )
        , maybe "" (\n -> "<p class=\"vita-note\">" ++ tex n ++ "</p>") (edNotes e)
        ]

renderPublications :: [Pub] -> String
renderPublications ps
    | null visible = ""
    | otherwise    = section "publications" "Publications and Preprints" $
        cardList (concatMap one visible) ++ footnote
  where
    visible = filter pbWeb ps
    -- The dagger legend lives on whichever entry declares it, but reads as a
    -- section-level note, so it is rendered once at the foot of the list.
    footnote = case mapMaybe pbNote visible of
        (n:_) -> "<p class=\"vita-footnote\">" ++ tex n ++ "</p>"
        []    -> ""
    dateOf p = tex (pbYear p) ++ maybe "" (\m -> ", " ++ tex m) (pbMonth p)
    one p = card
        [ case pbTitle p of
            -- Entries without a title (work in preparation) put the label in
            -- the authors field; the CV template makes the same distinction.
            Nothing -> concat
                [ headerRow (tex (pbAuthors p)) (dateOf p)
                , "<p class=\"vita-venue\">", tex (pbVenue p)
                , maybe "" (\t -> " " ++ tex t) (pbTarget p)
                , "</p>"
                ]
            Just t -> concat
                [ headerRow (tex t) (dateOf p)
                , "<p class=\"vita-authors\">", tex (pbAuthors p), "</p>"
                , "<p class=\"vita-venue\">", tex (pbVenue p), "</p>"
                ]
        , renderLinks (pbLinks p)
        ]

renderPresentations :: [Pres] -> String
renderPresentations ps
    | null visible = ""
    | otherwise    = section "presentations" "Presentations" $
        cardList (concatMap one visible)
  where
    visible = filter prWeb ps
    dateOf p = maybe "" (\m -> tex m ++ " ") (prMonth p) ++ tex (prYear p)
    one p = card
        [ headerRow (tex (prTitle p)) (dateOf p)
        , "<p class=\"vita-authors\">", tex (prAuthors p), "</p>"
        , "<p class=\"vita-venue\">"
        , maybe "" (\k -> tex k ++ ", ") (prKind p)
        , tex (prVenue p)
        , maybe "" (\st -> "<span class=\"vita-status\">" ++ tex st ++ "</span>") (prStatus p)
        , "</p>"
        ]

-- | Experience keeps the CV's research/industry split — that division is
--   editorial, lives in @cv_section@, and a homepage that merged the two
--   would say something the PDF does not.
renderExperience :: [Exp] -> String
renderExperience xs = research ++ industry
  where
    visible  = sortOn exOrder (filter exWeb xs)
    isRes e  = exSection e == Just "research"
    research = group "experience-research" "Research Experience" (filter isRes visible)
    industry = group "experience-industry" "Industry Experience" (filter (not . isRes) visible)
    group slug heading es
        | null es   = ""
        | otherwise = section slug heading $
            cardList (concatMap one es)
    one e = card
        [ headerRow (tex (exOrg e)) (dateRange (exStart e) (exEnd e))
        , subLine (exRole e) (locationPart (exLocation e) (exLocUrl e))
        , maybe "" (\p -> "<p class=\"vita-note\">" ++ tex p ++ "</p>") (exPreamble e)
        , renderBullets (exBullets e)
        ]

-- | The @/cv/projects/@ index. Groups render in first-appearance order, the
--   same convention "Now" uses for its sections — reordering the YAML
--   reorders the page and no separate ordering key is needed.
--
--   Entry titles link to the project's essay where one exists. The essays are
--   deliberately not generated: a writeup is an informal presentation of a
--   project, not a record of it, and belongs in the same voice as the rest of
--   the essays.
renderProjects :: [Proj] -> String
renderProjects ps = concatMap one (groupOrder visible)
  where
    visible = filter pjWeb ps
    groupOrder = foldl (\acc g -> if g `elem` acc then acc else acc ++ [g]) []
               . map pjGroup
    -- "Machine Learning & Deployed" → "machine-learning-deployed".
    slugify s = case foldr step [] s of
        ('-':rest) -> rest
        cleaned    -> cleaned
      where
        step c acc
            | c `elem` (['a'..'z'] ++ ['0'..'9']) = c : acc
            | c `elem` ['A'..'Z']                 = toLower c : acc
            | null acc || head acc == '-'         = acc
            | otherwise                           = '-' : acc
    one g = section ("projects-" ++ slugify g) (escapeHtml g) $
        cardList (concatMap entry (filter ((== g) . pjGroup) visible))
    entry p = card
        [ headerRow
            (case pjEssay p of
                Just u  -> "<a href=\"" ++ escapeHtml u ++ "\">" ++ tex (pjName p) ++ "</a>"
                Nothing -> tex (pjName p))
            (dateRange (pjStart p) (pjEnd p))
        , "<p class=\"vita-note\">", tex (pjDescription p), "</p>"
        , renderLinks (pjLinks p)
        ]

renderContact :: Person -> String
renderContact p = section "contact" "Contact" $ concat
    [ "<p class=\"vita-links\">"
    , "<a class=\"vita-chip\" href=\"mailto:", escapeHtml (pnEmail p), "\">"
    , escapeHtml (pnEmail p)
    , "</a>"
    , concatMap one (filter plWeb (pnLinks p))
    , "</p>"
    ]
  where
    -- Chips carry the label, not personal.yml's `display` value. The CV
    -- prints "ORCID: 0009-0002-0162-3587" because a printed page cannot be
    -- clicked; a chip reading "0009-0002-0162-3587" alone identifies
    -- nothing, and "github.com/example" is a URL doing a label's job.
    one l = concat
        [ "<a class=\"vita-chip\" href=\"", escapeHtml (plHref l), "\">"
        , tex (plLabel l)
        , "</a>"
        ]

-- ---------------------------------------------------------------------------
-- Load
-- ---------------------------------------------------------------------------

-- | Render a section, or drop the field entirely when it comes out empty so
--   the template's @$if(...)$@ guards behave.
sectionField :: String -> Compiler String -> Context String
sectionField name gen = field name $ \_ -> do
    html <- gen
    if null html then noResult (name ++ ": empty") else return html

-- ---------------------------------------------------------------------------
-- Context
-- ---------------------------------------------------------------------------

vitaCtx :: Context String
vitaCtx =
    constField "vita" "true"
    <> sectionField "vita-education-html"
        (renderEducation . unEduDoc <$> loadYaml "yaml-source/data/education.yml")
    <> sectionField "vita-publications-html"
        (renderPublications . unPubDoc <$> loadYaml "yaml-source/data/publications.yml")
    <> sectionField "vita-presentations-html"
        (renderPresentations . unPresDoc <$> loadYaml "yaml-source/data/presentations.yml")
    <> sectionField "vita-experience-html"
        (renderExperience . unExpDoc <$> loadYaml "yaml-source/data/experience.yml")
    <> sectionField "vita-contact-html"
        (renderContact <$> loadYaml "yaml-source/data/personal.yml")
    <> siteCtx

-- | The @/cv/projects/@ index. Reuses the vita flag so it picks up the same
--   stylesheets and reads as the same kind of surface.
projectsCtx :: Context String
projectsCtx =
    constField "vita" "true"
    <> sectionField "vita-projects-html"
        (renderProjects . unProjDoc <$> loadYaml "yaml-source/data/projects.yml")
    <> siteCtx
