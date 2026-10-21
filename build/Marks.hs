{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Frontmatter marks: the monogram (a hand-authored SVG per piece) and
-- the epistemic figure (a build-time SVG generated from frontmatter).
-- See MARKS.md for the full specification.
--
-- Two Hakyll context fields are exported:
--
--   * @$monogramSvg$@  — the inlined monogram for the current item, or
--                        'noResult' when no co-located @mark.svg@ exists.
--   * @$epistemicSvg$@ — the generated epistemic figure, or 'noResult'
--                        when the item has no @status:@ frontmatter
--                        (MARKS.md §3.1).
--
-- Both fields are deterministic: byte-identical inputs produce
-- byte-identical SVGs, so the GPG signing pipeline is undisturbed.
module Marks
    ( monogramSvgField
    , hasMonogramField
    , monogramSvgFieldFor
    , hasMonogramFieldFor
    , epistemicSvgField
    , hasMonogram
    , peerStatusValues
    , epistemicVocabulary
    ) where

import           Control.Exception      (IOException, try)
import           Data.Char              (toLower)
import           Data.List              (elemIndex)
import           Data.Maybe             (catMaybes, isJust)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Numeric                (showFFloat)
import           System.Directory       (doesFileExist)
import           System.FilePath        (takeBaseName, takeDirectory,
                                         takeFileName, (</>))
import           System.IO              (hPutStrLn, stderr)
import           Text.Read              (readMaybe)

import           Hakyll hiding (trim)
import Utils (isProvedConfidence, confidencePercent, trustScore, trim)
import           Stability              (resolveStability, stabilityLabels)
import           SvgColor               (blackToCurrentColor)

-- ---------------------------------------------------------------------------
-- Monogram path resolution
-- ---------------------------------------------------------------------------

-- | Candidate monogram paths for a given source path. The build picks the
--   first one that exists on disk. This dual-form resolver matches the
--   site's mixed flat / directory essay convention:
--
--   > content/essays/foo.md            → content/essays/foo.mark.svg
--   > content/essays/foo/index.md      → content/essays/foo/mark.svg
monogramCandidates :: FilePath -> [FilePath]
monogramCandidates fp =
    let dir   = takeDirectory fp
        fname = takeFileName fp
    in  if fname == "index.md"
            then [dir </> "mark.svg"]
            else [dir </> takeBaseName fp ++ ".mark.svg"]

-- | Predicate form of 'resolveMonogramPath' — used by Stats.hs to
--   compute monogram coverage on @/build/@. Returns 'True' when at
--   least one of the dual-form candidate paths exists on disk.
hasMonogram :: Item a -> Compiler Bool
hasMonogram item = isJust <$> resolveMonogramPath item

-- | @$has-monogram$@ — present (renders as @"true"@) only when the
--   item has an actual @mark.svg@ on disk; 'noResult' for the
--   placeholder-roundel case. Templates that don't want to display
--   placeholder roundels (e.g. item-card listings, popup previews)
--   gate on this flag instead of @$monogramSvg$@, which the
--   frontmatter header relies on always rendering for symmetric
--   column layout.
hasMonogramField :: Context String
hasMonogramField = field "has-monogram" $ \item -> do
    has <- hasMonogram item
    if has then return "true" else noResult "no real monogram"

-- | Return the first candidate path that exists on disk, or 'Nothing'.
resolveMonogramPath :: Item a -> Compiler (Maybe FilePath)
resolveMonogramPath item =
    unsafeCompiler $ firstExisting (monogramCandidates fp)
  where
    fp = toFilePath (itemIdentifier item)
    firstExisting []     = return Nothing
    firstExisting (p:ps) = do
        e <- doesFileExist p
        if e then return (Just p) else firstExisting ps

-- ---------------------------------------------------------------------------
-- Monogram inlining
-- ---------------------------------------------------------------------------

-- | @$monogramSvg$@. Reads the resolved @mark.svg@, normalizes black
--   fills/strokes to @currentColor@ (defensive — authors using AI-assist
--   tools may produce hardcoded blacks; the contract still holds), strips
--   the @width@/@height@ presentation attributes from the root @<svg>@,
--   and wraps the result in @<figure class="frontmatter-mark
--   frontmatter-mark--monogram">@.
--
--   When no @mark.svg@ exists, returns the placeholder roundel — an
--   empty outer ring at lower opacity that visually balances the
--   epistemic-figure column and signals "monogram not yet authored".
--   Read failures fall back to the same placeholder.
monogramSvgField :: Context String
monogramSvgField = field "monogramSvg" $ \item -> resolveMonogramPath item >>= monogramHtml

-- | The inlined mark at a path, or the placeholder roundel when there is
--   none or it cannot be read.
monogramHtml :: Maybe FilePath -> Compiler String
monogramHtml Nothing     = return (T.unpack monogramPlaceholder)
monogramHtml (Just path) = do
    result <- unsafeCompiler $ try (TIO.readFile path)
                :: Compiler (Either IOException T.Text)
    case result of
        Left e -> do
            unsafeCompiler $ hPutStrLn stderr $
                "[Marks] failed to read " ++ path ++ ": " ++ show e
            return (T.unpack monogramPlaceholder)
        Right svg -> return $ T.unpack $ wrapMonogram (processSvg svg)

-- | Empty-roundel placeholder used while a piece's monogram is still
--   to be authored (Phase 2 of MARKS.md). The @--placeholder@ modifier
--   class lets CSS render it at reduced opacity so it reads as a
--   neutral frame rather than a finished glyph.
monogramPlaceholder :: T.Text
monogramPlaceholder = T.concat
    [ "<figure class=\"frontmatter-mark frontmatter-mark--monogram"
    , " frontmatter-mark--placeholder\" aria-hidden=\"true\">"
    , "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 280 280\">"
    , "<circle cx=\"140\" cy=\"140\" r=\"128\" fill=\"none\""
    , " stroke=\"currentColor\" stroke-width=\"0.6\"/>"
    , "</svg>"
    , "</figure>"
    ]

-- | Wrap inlined monogram SVG in its outer figure element.
wrapMonogram :: T.Text -> T.Text
wrapMonogram svg = T.concat
    [ "<figure class=\"frontmatter-mark frontmatter-mark--monogram\">"
    , svg
    , "</figure>"
    ]

-- | @$monogramSvg$@ override for synthesized pages whose item identifier
--   doesn't live under @content/@ (e.g. @/build/@, @/stats/@), so the
--   auto-resolver in 'monogramSvgField' can't find a co-located mark.
--   Reads from the supplied path; falls back to the placeholder roundel
--   when the file is absent or unreadable.
monogramSvgFieldFor :: FilePath -> Context a
monogramSvgFieldFor path = field "monogramSvg" $ \_ -> do
    exists <- unsafeCompiler $ doesFileExist path
    monogramHtml (if exists then Just path else Nothing)

-- | @$has-monogram$@ override paired with 'monogramSvgFieldFor'. Present
--   (as @"true"@) only when the path exists; 'noResult' otherwise.
hasMonogramFieldFor :: FilePath -> Context a
hasMonogramFieldFor path = field "has-monogram" $ \_ -> do
    exists <- unsafeCompiler $ doesFileExist path
    if exists then return "true" else noResult "no real monogram"

-- | Replace hardcoded black fills/strokes with @currentColor@ and strip
--   the root @<svg>@'s @width@/@height@ attributes (presentation lives
--   in CSS via the @.frontmatter-mark svg@ selector). Colours go through
--   'SvgColor.blackToCurrentColor', as every inlined SVG's do.
processSvg :: T.Text -> T.Text
processSvg = stripRootDims . blackToCurrentColor

-- | Remove @width="..."@ and @height="..."@ from the root @<svg>@ start
--   tag, so the mark sizes from CSS. Only that tag: none of the marks'
--   roots carries dimensions, and a search of the whole file stripped them
--   from the first @<rect>@ instead, which then drew nothing (audit H02:
--   eight marks, among them /build/'s and proof-broker's).
stripRootDims :: T.Text -> T.Text
stripRootDims txt = case T.breakOn "<svg" txt of
    (pre, rest)
        | T.null rest -> txt
        | otherwise   ->
            let (tag, body) = T.breakOn ">" rest
            in  pre <> stripAttr "width" (stripAttr "height" tag) <> body
  where
    stripAttr attr tag =
        let key = T.pack (" " ++ attr ++ "=\"")
        in case T.breakOn key tag of
            (before, after)
                | T.null after -> tag
                | otherwise ->
                    case T.breakOn "\"" (T.drop (T.length key) after) of
                        (_, rest) | T.null rest -> tag
                                  | otherwise   -> before <> T.drop 1 rest

-- ---------------------------------------------------------------------------
-- Epistemic figure: data extraction
-- ---------------------------------------------------------------------------

-- | Captures the frontmatter inputs the figure consumes. Constructed
--   once per item by 'readEpistemicData', then handed to the pure
--   geometry below. Keeps the I/O step (metadata + git) separate from
--   the SVG-string formatter, so the formatter is testable in isolation
--   without mocking Hakyll.
data EpistemicData = EpistemicData
    { epConfidence       :: Maybe Int       -- ^ Numeric confidence; 'Nothing' if proved/absent/unparseable.
    , epConfidenceProved :: Bool            -- ^ True when @confidence: proved@ / @proven@.
    , epImportance       :: Maybe Int       -- ^ 1–5 ordinal.
    , epEvidence         :: Maybe Int       -- ^ 1–5 ordinal.
    , epScope            :: Maybe String    -- ^ Validated scope value.
    , epNovelty          :: Maybe String    -- ^ Validated novelty value.
    , epPracticality     :: Maybe String    -- ^ Validated practicality value.
    , epPeerStatus       :: Maybe String    -- ^ Validated peer-status slug ('Nothing' when absent / unreviewed / invalid).
    , epResultShape      :: Maybe String    -- ^ Validated result-shape value.
    , epStability        :: String          -- ^ Always one of the five stability labels.
    , epTrust            :: Maybe Int       -- ^ Trust score 0–100 (60/40 weighted; @proved@ substitutes 100 for confidence). 'Nothing' when confidence or evidence is missing — no label is rendered.
    }

-- | Read the figure inputs from a Hakyll item's metadata + git history.
readEpistemicData :: Item a -> Compiler EpistemicData
readEpistemicData item = do
    meta <- getMetadata (itemIdentifier item)
    stab <- resolveStability item
    let confRaw     = lookupString "confidence" meta
        proved      = isProvedConfidence confRaw
        confInt     = confidencePercent confRaw
        confNumeric = if proved then Nothing else confInt
        importance  = readMaybe =<< lookupString "importance" meta
        evidence    = readMaybe =<< lookupString "evidence"   meta
        scope       = validate scopeValues       =<< lookupString "scope"        meta
        novelty     = validate noveltyValues     =<< lookupString "novelty"      meta
        practical   = validate practicalityValues =<< lookupString "practicality" meta
        peer        = validatePeerStatus =<< lookupString "peer-status"  meta
        resultShape = validate resultShapeValues =<< lookupString "result-shape" meta
        trust       = trustScore confInt evidence
    return EpistemicData
        { epConfidence       = confNumeric
        , epConfidenceProved = proved
        , epImportance       = importance
        , epEvidence         = evidence
        , epScope            = scope
        , epNovelty          = novelty
        , epPracticality     = practical
        , epPeerStatus       = peer
        , epResultShape      = resultShape
        , epStability        = stab
        , epTrust            = trust
        }

-- | Validate a value against an enum list. Returns the lowercase form
--   on hit, 'Nothing' otherwise (no warning here — Contexts.hs's parsers
--   already warn on invalid frontmatter; the figure simply degrades).
validate :: [String] -> String -> Maybe String
validate vs raw =
    let s = map toLower (trim raw)
    in  if s `elem` vs then Just s else Nothing

-- | Peer-status validator: matches @peerStatusField@ in Contexts.hs but
--   maps @unreviewed@ to 'Nothing' so the figure's outer ring stays
--   neutral by default.
validatePeerStatus :: String -> Maybe String
validatePeerStatus raw =
    let s = map toLower (trim raw)
    in  if s `elem` peerStatusValues && s /= "unreviewed"
            then Just s
            else Nothing  -- includes "unreviewed" and any unknown value

-- | Every @peer-status@ the site recognizes; @unreviewed@ is the default.
--   'Contexts.peerStatusField' validates against this list too.
peerStatusValues :: [String]
peerStatusValues = ["unreviewed", "under-review", "peer-reviewed", "published", "retracted"]

-- | The epistemic fields' vocabularies, least to most, as
--   @site shared-rules@ prints them for the tools and tests that must
--   agree with the generator (audit-marks.py, search-filters.js, the
--   filter buttons on /search.html). A copy of these lists in search-filters.js
--   once lacked @local@ and @low@, and those pages matched no filter.
epistemicVocabulary :: [(String, [String])]
epistemicVocabulary =
    [ ("scope",        scopeValues)
    , ("novelty",      noveltyValues)
    , ("practicality", practicalityValues)
    , ("result-shape", resultShapeValues)
    , ("peer-status",  peerStatusValues)
    , ("stability",    stabilityLabels)
    ]

scopeValues, noveltyValues, practicalityValues, resultShapeValues :: [String]
scopeValues        = ["personal", "local", "average", "broad", "civilizational"]
noveltyValues      = ["conventional", "moderate", "idiosyncratic", "innovative"]
practicalityValues = ["abstract", "low", "moderate", "high", "exceptional"]
resultShapeValues  = ["positive", "negative", "mixed", "comparative", "descriptive"]

-- | Map an ordinal value to its numeric rank (1-based).
ordinalRank :: [String] -> String -> Maybe Int
ordinalRank vs s = lookup s (zip vs [1..])

-- ---------------------------------------------------------------------------
-- Epistemic figure: geometry
-- ---------------------------------------------------------------------------

-- | Centre of the figure (viewBox coordinates).
fxCenter, fyCenter :: Double
fxCenter = 100
fyCenter = 100

-- | Inner / outer radii of the roundel and the polygon's full extent.
fxOuter, fxOuterPlus, fxAxisFull :: Double
fxOuter      = 88   -- inner roundel circle
fxOuterPlus  = 90   -- outer roundel circle
fxAxisFull   = 80   -- full axis length (polygon vertex when value = 1.0)

-- | Six axis angles, clockwise from 12 o'clock, in degrees.
--   Index → field:  0 confidence, 1 novelty, 2 practicality,
--                   3 scope, 4 evidence, 5 importance.
axisAngles :: [Double]
axisAngles = [0, 60, 120, 180, 240, 300]

-- | Convert a (clockwise-angle-from-12-o'clock, distance-from-centre) pair
--   to absolute viewBox coordinates.
polar :: Double -> Double -> (Double, Double)
polar angleDeg dist =
    let theta = (angleDeg - 90) * pi / 180
    in  (fxCenter + dist * cos theta, fyCenter + dist * sin theta)

-- | Axis index → normalized [0,1] value, or 'Nothing' when the
--   underlying frontmatter field is absent / unparseable.
axisValue :: EpistemicData -> Int -> Maybe Double
axisValue d i = case i of
    0 -> if epConfidenceProved d
            then Just 1.0
            else fmap (\c -> fromIntegral c / 100.0) (epConfidence d)
    1 -> normalizeOrdinal noveltyValues      (epNovelty d)
    2 -> normalizeOrdinal practicalityValues (epPracticality d)
    3 -> normalizeOrdinal scopeValues        (epScope d)
    4 -> normalizeIntScale 5 (epEvidence d)
    5 -> normalizeIntScale 5 (epImportance d)
    _ -> Nothing

-- | Map a 1..n ordinal-name value to a [0,1] value via @(rank-1)/(n-1)@.
normalizeOrdinal :: [String] -> Maybe String -> Maybe Double
normalizeOrdinal vs (Just s) = do
    r <- ordinalRank vs s
    return $ fromIntegral (r - 1) / fromIntegral (length vs - 1)
normalizeOrdinal _ Nothing = Nothing

-- | Map a 1..n integer to [0,1] via @(v-1)/(n-1)@.
normalizeIntScale :: Int -> Maybe Int -> Maybe Double
normalizeIntScale n (Just v) = Just $ fromIntegral (v - 1) / fromIntegral (n - 1)
normalizeIntScale _ Nothing  = Nothing

-- ---------------------------------------------------------------------------
-- Epistemic figure: SVG rendering
-- ---------------------------------------------------------------------------

-- | Format a Double with two decimal places. Determinism (§8.1) requires
--   no platform-dependent floating-point formatting.
ff :: Double -> T.Text
ff x = T.pack (showFFloat (Just 2) x "")

-- | Format a "x,y" coordinate pair.
xy :: Double -> Double -> T.Text
xy x y = ff x <> T.singleton ',' <> ff y

-- | Render the full epistemic figure SVG.
renderEpistemicFigure :: EpistemicData -> T.Text
renderEpistemicFigure d = T.concat
    [ "<svg xmlns=\"http://www.w3.org/2000/svg\""
    , " viewBox=\"0 0 200 200\""
    , " role=\"img\""
    , " aria-label=\"Epistemic figure: "
      -- "82 trust", as the figure shows it: a link around the figure takes
      -- this as its name, and a name that holds the visible text in its
      -- order is one a voice user can say (WCAG 2.5.3).
    , maybe "" (\t -> T.pack (show t) <> " trust, ") (epTrust d)
    , "stability ", T.pack (epStability d), "\">"
    , renderRoundel
    , renderGuides
    , renderAxes
    , renderPolygon d
    , renderVertexMarks d
    , renderTicks (epStability d) (epPeerStatus d)
    , maybe "" renderTrustLabel (epTrust d)
    , renderResultShape (epResultShape d) (epTrust d)
    , "</svg>"
    ]

-- | Two thin concentric circles forming the outer roundel.
renderRoundel :: T.Text
renderRoundel = T.concat
    [ "<circle cx=\"", ff fxCenter, "\" cy=\"", ff fyCenter
    , "\" r=\"", ff fxOuter
    , "\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"0.5\" opacity=\"0.7\"/>"
    , "<circle cx=\"", ff fxCenter, "\" cy=\"", ff fyCenter
    , "\" r=\"", ff fxOuterPlus
    , "\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"0.5\" opacity=\"0.7\"/>"
    ]

-- | Four concentric guide circles at 0.2 R, 0.4 R, 0.6 R, 0.8 R.
renderGuides :: T.Text
renderGuides = T.concat $ map oneGuide [0.2, 0.4, 0.6, 0.8 :: Double]
  where
    oneGuide t = T.concat
        [ "<circle cx=\"", ff fxCenter, "\" cy=\"", ff fyCenter
        , "\" r=\"", ff (fxAxisFull * t)
        , "\" fill=\"none\" stroke=\"currentColor\""
        , " stroke-width=\"0.25\" opacity=\"0.4\"/>"
        ]

-- | Six radial axes from centre to the inner roundel.
renderAxes :: T.Text
renderAxes = T.concat $ map oneAxis axisAngles
  where
    oneAxis a =
        let (x, y) = polar a fxAxisFull
        in  T.concat
                [ "<line x1=\"", ff fxCenter, "\" y1=\"", ff fyCenter
                , "\" x2=\"", ff x, "\" y2=\"", ff y
                , "\" stroke=\"currentColor\" stroke-width=\"0.3\" opacity=\"0.55\"/>"
                ]

-- | Polygon connecting the present field values along their axes.
--   When all six axes have a value the polygon is closed; otherwise
--   it's an open polyline through the present vertices in axis order.
renderPolygon :: EpistemicData -> T.Text
renderPolygon d =
    let pairs = [ (i, axisValue d i) | i <- [0..5] ]
        verts = [ polar a (fxAxisFull * v)
                | (i, Just v) <- pairs
                , let a = axisAngles !! i ]
    in  case verts of
            [] -> ""
            _  ->
                let pointsTxt = T.intercalate " " [ xy x y | (x, y) <- verts ]
                    allPresent = all (isJust . snd) pairs
                    tag = if allPresent then "polygon" else "polyline"
                    fillAttr = if allPresent
                                  then " fill=\"currentColor\" fill-opacity=\"0.08\""
                                  else " fill=\"none\""
                in  T.concat
                        [ "<", tag
                        , " points=\"", pointsTxt
                        , "\" stroke=\"currentColor\" stroke-width=\"1.1\""
                        , fillAttr
                        , " stroke-linejoin=\"round\" stroke-linecap=\"round\"/>"
                        ]

-- | One vertex point per present axis. Confidence axis gets a 3×3 square
--   instead of a 2-px circle when @confidence: proved@ is in effect — the
--   "proof cap" marker (MARKS.md §4.3).
renderVertexMarks :: EpistemicData -> T.Text
renderVertexMarks d = T.concat $ catMaybes
    [ vertexMark d i | i <- [0..5] ]

vertexMark :: EpistemicData -> Int -> Maybe T.Text
vertexMark d i = do
    v <- axisValue d i
    let (x, y) = polar (axisAngles !! i) (fxAxisFull * v)
        squareCap = i == 0 && epConfidenceProved d
    return $ if squareCap
        then T.concat
            [ "<rect x=\"", ff (x - 1.5), "\" y=\"", ff (y - 1.5)
            , "\" width=\"3\" height=\"3\""
            , " fill=\"currentColor\" stroke=\"none\"/>"
            ]
        else T.concat
            [ "<circle cx=\"", ff x, "\" cy=\"", ff y
            , "\" r=\"2\" fill=\"currentColor\" stroke=\"none\"/>"
            ]

-- | Outer-ring stability ticks at the top of the figure. Always five
--   positions; inactive ticks render at opacity 0.4 so the full scale
--   stays visible. Peer-status modulates tick *style*; see
--   'renderPeerStatusOverlay'.
renderTicks :: String -> Maybe String -> T.Text
renderTicks stability peerStatus =
    let activeCount = maybe 1 (+ 1) (elemIndex stability stabilityLabels)
        tickAngles :: [Double]
        tickAngles = [0, -15, 15, -30, 30]
        tickOne :: Int -> Double -> T.Text
        tickOne idx a =
            let (x1, y1) = polar a fxOuterPlus
                (x2, y2) = polar a (fxOuterPlus + 1.5)
                op       = if idx < activeCount then "1.0" else "0.4"
            in  T.concat
                    [ "<line x1=\"", ff x1, "\" y1=\"", ff y1
                    , "\" x2=\"", ff x2, "\" y2=\"", ff y2
                    , "\" stroke=\"currentColor\" stroke-width=\"1\""
                    , " stroke-linecap=\"round\" opacity=\"", op, "\"/>"
                    ]
    in  T.concat (zipWith tickOne [0..] tickAngles)
        <> renderPeerStatusOverlay peerStatus

-- | Per-peer-status decorations layered on top of the tick group.
--   Geometry per MARKS.md §4.1.
renderPeerStatusOverlay :: Maybe String -> T.Text
renderPeerStatusOverlay Nothing                = ""
renderPeerStatusOverlay (Just "under-review")  =
    -- Small unfilled circle just outside the outermost tick, at the top.
    let (x, y) = polar 0 (fxOuterPlus + 3.5)
    in T.concat
        [ "<circle cx=\"", ff x, "\" cy=\"", ff y
        , "\" r=\"1\" fill=\"none\" stroke=\"currentColor\""
        , " stroke-width=\"0.6\"/>"
        ]
renderPeerStatusOverlay (Just "peer-reviewed") =
    -- Single horizontal bar above the outer roundel arc, centred on top.
    T.concat
        [ "<line x1=\"", ff (fxCenter - 6), "\" y1=\"", ff (fyCenter - fxOuterPlus - 3)
        , "\" x2=\"", ff (fxCenter + 6), "\" y2=\"", ff (fyCenter - fxOuterPlus - 3)
        , "\" stroke=\"currentColor\" stroke-width=\"0.7\""
        , " stroke-linecap=\"round\"/>"
        ]
renderPeerStatusOverlay (Just "published")     =
    -- Printer's-bracket: short vertical marks at ±15° on the outer roundel.
    let (lx1, ly1) = polar (-15) (fxOuterPlus + 1)
        (lx2, ly2) = polar (-15) (fxOuterPlus + 4)
        (rx1, ry1) = polar   15  (fxOuterPlus + 1)
        (rx2, ry2) = polar   15  (fxOuterPlus + 4)
    in  T.concat
            [ "<line x1=\"", ff lx1, "\" y1=\"", ff ly1
            , "\" x2=\"", ff lx2, "\" y2=\"", ff ly2
            , "\" stroke=\"currentColor\" stroke-width=\"0.8\""
            , " stroke-linecap=\"round\"/>"
            , "<line x1=\"", ff rx1, "\" y1=\"", ff ry1
            , "\" x2=\"", ff rx2, "\" y2=\"", ff ry2
            , "\" stroke=\"currentColor\" stroke-width=\"0.8\""
            , " stroke-linecap=\"round\"/>"
            ]
renderPeerStatusOverlay (Just "retracted")     =
    -- Horizontal strikethrough across the tick group.
    T.concat
        [ "<line x1=\"", ff (fxCenter - 9), "\" y1=\"", ff (fyCenter - fxOuterPlus - 1)
        , "\" x2=\"", ff (fxCenter + 9), "\" y2=\"", ff (fyCenter - fxOuterPlus - 1)
        , "\" stroke=\"currentColor\" stroke-width=\"1.5\""
        , " stroke-linecap=\"round\"/>"
        ]
renderPeerStatusOverlay (Just _)               = ""

-- | Trust score (Spectral, 16 px) and the small "TRUST" label below it.
renderTrustLabel :: Int -> T.Text
renderTrustLabel score = T.concat
    [ "<text x=\"", ff fxCenter, "\" y=\"", ff (fyCenter + 4)
    , "\" text-anchor=\"middle\""
    , " fill=\"currentColor\" stroke=\"none\""
    , " font-family=\"Spectral, serif\" font-weight=\"500\" font-size=\"16\">"
    , T.pack (show score)
    , "</text>"
    , "<text x=\"", ff fxCenter, "\" y=\"", ff (fyCenter + 14)
    , "\" text-anchor=\"middle\""
    , " fill=\"currentColor\" stroke=\"none\""
    , " font-family=\"&quot;Fira Sans&quot;, sans-serif\""
    , " font-size=\"5\" letter-spacing=\"0.18em\""
    , " opacity=\"0.7\">TRUST</text>"
    ]

-- | Result-shape glyph immediately to the right of the trust score —
--   or centred in its place when no trust score is rendered.
renderResultShape :: Maybe String -> Maybe Int -> T.Text
renderResultShape Nothing _ = ""
renderResultShape (Just shape) mScore =
    let glyph = case shape of
            "positive"    -> "+"
            "negative"    -> "\x2212"     -- minus sign (not hyphen-minus)
            "mixed"       -> "\x00B1"     -- ±
            "comparative" -> "\x223C"     -- ∼
            "descriptive" -> "\x25A1"     -- □
            _             -> ""
        -- Offset proportional to the trust number's width (digits ≈ 8 px
        -- each); with no trust label the glyph takes the centre itself.
        (x, anchor) = case mScore of
            Just score ->
                let digitCount = length (show score)
                    offset     = fromIntegral digitCount * 4.5 + 3 :: Double
                in  (fxCenter + offset, "start")
            Nothing -> (fxCenter, "middle")
    in  if T.null (T.pack glyph)
            then ""
            else T.concat
                [ "<text x=\"", ff x
                , "\" y=\"", ff (fyCenter + 4)
                , "\" text-anchor=\"", anchor, "\""
                , " fill=\"currentColor\" stroke=\"none\""
                , " font-family=\"Spectral, serif\" font-size=\"16\">"
                , T.pack glyph
                , "</text>"
                ]

-- ---------------------------------------------------------------------------
-- Field exports
-- ---------------------------------------------------------------------------

-- | @$epistemicSvg$@. Returns 'noResult' when @status:@ is absent
--   (matches the existing visibility rule for the epistemic block —
--   MARKS.md §3.1). Otherwise returns the inline SVG string ready for
--   template interpolation.
epistemicSvgField :: Context String
epistemicSvgField = field "epistemicSvg" $ \item -> do
    meta <- getMetadata (itemIdentifier item)
    case lookupString "status" meta of
        Nothing -> noResult "no status; epistemic figure suppressed"
        Just _  -> do
            d <- readEpistemicData item
            return $ T.unpack (wrapEpistemic (renderEpistemicFigure d))

wrapEpistemic :: T.Text -> T.Text
wrapEpistemic svg = T.concat
    [ "<figure class=\"frontmatter-mark frontmatter-mark--epistemic\">"
    , svg
    , "</figure>"
    ]

