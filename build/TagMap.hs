{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The tag map: a drawing, made at build time, of the tags and the pieces
--   filed under them, as one monochrome SVG.
--
--   Tags are labeled circles sized by their count, top-level tags (portals)
--   larger than subtags; a subtag joins its parent by a dashed line. Each
--   piece is a dot joined to the tags it is filed under as written (the most
--   specific ones), and links to the piece.
--
--   'layoutMap' places everything once; 'renderMap' draws that one layout
--   either whole or with one tag's region lit, and 'renderPieceMap' a
--   detail of it around one piece, so every page that shows the map shows
--   the same map. Both are pure and deterministic: the same input
--   gives a byte-identical SVG.
--
--   The layout is force-directed (Fruchterman–Reingold forces, cooled over a
--   fixed number of steps from a fixed start), scaled into a frame; each
--   tag's label goes on the side of its circle where it crosses the fewest
--   lines, and the drawing is relaxed until no label overlaps another node,
--   then fitted to a viewBox. All ink comes from @static/css/tag-map.css@,
--   through classes; the SVG names no colors, and the label widths estimated
--   here assume that stylesheet's label faces.
--
--   The drawing is hidden from assistive technology (@aria-hidden@), its
--   links kept out of the tab order, since the page's own lists of tags and
--   pieces are the accessible equivalent; a visually hidden paragraph before
--   it says what it shows. (As an image, @role="img"@, it would hold links,
--   which axe reports as nested interactive controls.) It carries no ids, so any number
--   of maps may share a page. URLs are written as given, root-relative.
module TagMap
    ( MapTag (..)
    , MapPiece (..)
    , Layout
    , layoutMap
    , renderMap
    , renderPieceMap
    ) where

import Data.Bits ((.&.), xor)
import Data.Char (ord)
import Data.List (foldl', intercalate, isPrefixOf, sort, sortOn)
import qualified Data.Map as LazyMap
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Word (Word64)
import Utils (escapeHtml)

-- ---------------------------------------------------------------------------
-- Input
-- ---------------------------------------------------------------------------

-- | A tag as the map draws it.
data MapTag = MapTag
    { mtTag    :: String        -- ^ the tag as written, @ai/llm@
    , mtName   :: String        -- ^ its display name, @LLMs@
    , mtUrl    :: String        -- ^ root-relative, @/ai/llm/@
    , mtCount  :: Int           -- ^ pieces under it, its subtags' included
    , mtParent :: Maybe String  -- ^ the parent tag, for a subtag
    , mtFacet  :: String        -- ^ @research@ draws hollow
    } deriving (Eq, Show)

-- | A piece filed under tags.
data MapPiece = MapPiece
    { mpId    :: String    -- ^ stable identity, for ordering only
    , mpTitle :: String
    , mpUrl   :: String    -- ^ root-relative
    , mpTags  :: [String]  -- ^ as written: the most specific ones
    } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Layout
-- ---------------------------------------------------------------------------

data V = V !Double !Double

-- | Where everything is, in the viewBox's units.
data Layout = Layout
    { lyTags   :: [(MapTag, V, Place)]
    , lyPieces :: [(MapPiece, V)]   -- ^ tags filtered to the map's own
    , lyWidth  :: Double
    , lyHeight :: Double
    }

data Kind = Portal | Subtag | Dot deriving (Eq)

-- | Which side of its circle a tag's label sits on.
data Place = Below | Above | RightOf | LeftOf deriving (Eq, Show, Enum, Bounded)

-- | A node as the layout sees it: its kind, its radius, and its label.
data Spec = Spec { sKind :: Kind, sRadius :: Double, sLabel :: Maybe String }

-- | The frame the node centers are scaled into, before labels push them
--   apart: wider than tall, to sit in a text column.
frameW, frameH :: Double
frameW = 640
frameH = 420

-- | Steps of the force simulation, and its ideal edge length (in frame
--   units: the simulation runs at about the frame's own scale).
iterations :: Int
iterations = 400

idealLength :: Double
idealLength = 40

-- | How strongly each kind of node pushes others away, and how strongly
--   each kind of line pulls its ends together.
repulsion :: Kind -> Double
repulsion Portal = 3.2
repulsion Subtag = 2.1
repulsion Dot    = 1.0

filedPull, parentPull :: Double
filedPull  = 1.0
parentPull = 1.6

-- | Pull toward the center, stronger vertically so the cloud comes out
--   wider than tall.
gravityX, gravityY :: Double
gravityX = 0.06
gravityY = 0.12

-- | Margin around the drawing, and the clearance kept between a label and
--   any other node.
margin, clearance :: Double
margin    = 8
clearance = 2.5

kindOf :: MapTag -> Kind
kindOf t = maybe Portal (const Subtag) (mtParent t)

-- | Circle radii: area grows with the count.
radius :: Kind -> Int -> Double
radius Portal n = 3.6 + 1.45 * sqrt (fromIntegral (max 1 n))
radius Subtag n = 2.6 + 1.0  * sqrt (fromIntegral (max 1 n))
radius Dot    _ = 2.3

-- | Place the tags and pieces. Input order does not matter: both are sorted
--   first. A piece filed under no tag of the map is left out; a piece's
--   tags outside the map are dropped.
layoutMap :: [MapTag] -> [MapPiece] -> Layout
layoutMap tags0 pieces0 = fitted placed2 relaxed2
  where
    tags    = sortOn mtTag tags0
    known   = Set.fromList (map mtTag tags)
    pieces  = sortOn mpId
        [ p { mpTags = ts }
        | p <- pieces0
        , let ts = Set.toList (Set.fromList (filter (`Set.member` known) (mpTags p)))
        , not (null ts) ]
    nTags   = length tags
    tagIx   = Map.fromList (zip (map mtTag tags) [0 ..])
    specs   = [ Spec (kindOf t) (radius (kindOf t) (mtCount t)) (Just (mtName t)) | t <- tags ]
           ++ [ Spec Dot (radius Dot 0) Nothing | _ <- pieces ]
    edges   = [ (i, j, parentPull)
              | (i, t) <- zip [0 ..] tags
              , Just p <- [mtParent t], Just j <- [Map.lookup p tagIx] ]
           ++ [ (nTags + i, j, filedPull)
              | (i, p) <- zip [0 ..] pieces
              , Just j <- map (`Map.lookup` tagIx) (mpTags p) ]
    segments = [ (a, b) | (a, b, _) <- edges ]
    settled  = simulate (map sKind specs) edges (initial tags pieces)
    scaled   = scaleToFrame settled
    -- Labels go where they cross least, then the drawing is relaxed until
    -- nothing overlaps; twice, since relaxing moves what the choice saw.
    placed1  = choosePlaces specs segments scaled
    relaxed1 = relax specs placed1 scaled
    placed2  = choosePlaces specs segments relaxed1
    relaxed2 = relax specs placed2 relaxed1
    fitted _ [] = Layout [] [] 0 0
    fitted places ps =
        let boxes = zipWith3 nodeBox specs places ps
            x0 = minimum [ a | (a, _, _, _) <- boxes ] - margin
            y0 = minimum [ b | (_, b, _, _) <- boxes ] - margin
            x1 = maximum [ c | (_, _, c, _) <- boxes ] + margin
            y1 = maximum [ d | (_, _, _, d) <- boxes ] + margin
            sh (V x y) = V (x - x0) (y - y0)
            (tp, pp) = splitAt nTags (map sh ps)
        in Layout { lyTags   = zip3 tags tp places
                  , lyPieces = zip pieces pp
                  , lyWidth  = fromIntegral (ceiling (x1 - x0) :: Int)
                  , lyHeight = fromIntegral (ceiling (y1 - y0) :: Int) }

-- | A fixed start. Portals go round an ellipse in an order that puts those
--   sharing pieces next to each other; subtags just outside their parent;
--   pieces at the mean of their tags, nudged by a hash of their identity so
--   that pieces filed alike do not coincide.
initial :: [MapTag] -> [MapPiece] -> [V]
initial tags pieces = map tagAt tags ++ map pieceAt pieces
  where
    portals  = [ mtTag t | t <- tags, mtParent t == Nothing ]
    rootOf s = takeWhile (/= '/') s
    shared a b = length [ () | p <- pieces
                             , let rs = map rootOf (mpTags p)
                             , a `elem` rs, b `elem` rs ]
    countOf  = Map.fromList [ (mtTag t, mtCount t) | t <- tags ]
    order    = ring portals
    ring []  = []
    ring ps  = go [first] (filter (/= first) ps)
      where first = minimumOn (\p -> (Down (Map.findWithDefault 0 p countOf), p)) ps
            go acc [] = reverse acc
            go acc rest =
                let lastP = take 1 acc
                    next = minimumOn (\p -> ( Down (sum (map (shared p) lastP))
                                            , Down (sum (map (shared p) acc))
                                            , Down (Map.findWithDefault 0 p countOf)
                                            , p )) rest
                in go (next : acc) (filter (/= next) rest)
    m        = max 1 (length order)
    angleOf  = Map.fromList (zip order [ 2 * pi * fromIntegral i / fromIntegral m | i <- [0 :: Int ..] ])
    rx = frameW * 0.30
    ry = frameH * 0.30
    portalAt p = let a = Map.findWithDefault 0 p angleOf in V (rx * cos a) (ry * sin a)
    siblings = Map.fromListWith (flip (++))
        [ (p, [mtTag t]) | t <- tags, Just p <- [mtParent t] ]
    -- Lazy: a subtag's place depends on its parent's.
    tagPos   = LazyMap.fromList [ (mtTag t, tagAt t) | t <- tags ]
    tagAt t = case mtParent t of
        Nothing -> portalAt (mtTag t)
        Just p  ->
            let V px py = fromMaybe (V 0 0) (Map.lookup p tagPos)
                sibs = Map.findWithDefault [mtTag t] p siblings
                j = fromIntegral (length (takeWhile (/= mtTag t) sibs)) :: Double
                c = fromIntegral (length sibs - 1) / 2
                a = atan2 py px + (j - c) * 0.45
                d = sqrt (px * px + py * py) + idealLength * 1.5
            in V (d * cos a) (d * sin a)
    pieceAt p =
        let ps = mapMaybe (`Map.lookup` tagPos) (mpTags p)
            n = fromIntegral (length ps)
            V sx sy = foldl' addV (V 0 0) ps
            h = hashUnit (mpId p)
            a = 2 * pi * h
            r = idealLength * (0.4 + 0.5 * hashUnit (reverse (mpId p)))
        in V (sx / n + r * cos a) (sy / n + r * sin a)

-- | FNV-1a over the characters, as a fraction in [0, 1).
hashUnit :: String -> Double
hashUnit s = fromIntegral (h .&. 0xffffff) / 16777216
  where
    h = foldl' (\acc c -> (acc `xor` fromIntegral (ord c)) * 1099511628211)
               (14695981039346656037 :: Word64) s

addV :: V -> V -> V
addV (V a b) (V c d) = V (a + c) (b + d)

-- | Fruchterman–Reingold: every pair repels by k²/d (weighted by both
--   nodes' 'repulsion'), every line attracts by d²/k (weighted by its
--   pull), gravity draws each node to the center in proportion to its
--   weight, and each step moves a node at most the current temperature,
--   which falls linearly to nearly nothing.
simulate :: [Kind] -> [(Int, Int, Double)] -> [V] -> [V]
simulate kinds edges start = foldl' step start [0 .. iterations - 1]
  where
    k  = idealLength
    ws = map repulsion kinds
    t0 = frameW / 8
    step ps i =
        let temp  = t0 * (1 - fromIntegral i / fromIntegral iterations) + 0.2
            sq    = Seq.fromList ps
            pulls = Map.fromListWith addV (concatMap (pull sq) edges)
            moved = [ move temp (addV (pushes j p w) (Map.findWithDefault (V 0 0) j pulls)) w p
                    | (j, p, w) <- zip3 [0 ..] ps ws ]
        in forceAll moved
      where
        pushes j (V x y) w = foldl' addV (V 0 0)
            [ V (dx / d * f) (dy / d * f)
            | (o, V x' y', w') <- zip3 [0 :: Int ..] ps ws, o /= j
            , let (dx, dy) = separate o j (x - x') (y - y')
                  d = max 0.01 (sqrt (dx * dx + dy * dy))
                  f = k * k * w * w' / d * reach d ]
    -- Repulsion fades out beyond a few edge lengths, so that gravity can
    -- bring a part of the graph joined to nothing in beside the rest.
    reach d = let x = d / (5 * k) in if x >= 1 then 0 else (1 - x) * (1 - x)
    pull sq (a, b, s) =
        let V xa ya = Seq.index sq a
            V xb yb = Seq.index sq b
            dx = xb - xa; dy = yb - ya
            d = max 0.01 (sqrt (dx * dx + dy * dy))
            f = s * d * d / k
            v = V (dx / d * f) (dy / d * f)
        in [(a, v), (b, negV v)]
    move temp (V fx fy) w (V x y) =
        let gx = fx - gravityX * w * x * k / 10
            gy = fy - gravityY * w * y * k / 10
            m  = sqrt (gx * gx + gy * gy)
            s  = if m < 1e-9 then 0 else min m temp / m
        in V (x + gx * s) (y + gy * s)
    -- Coincident nodes are parted along a fixed direction, by index.
    separate i j dx dy
        | abs dx < 1e-6 && abs dy < 1e-6 = if i < j then (-0.01, -0.01) else (0.01, 0.01)
        | otherwise = (dx, dy)

negV :: V -> V
negV (V a b) = V (negate a) (negate b)

forceAll :: [V] -> [V]
forceAll vs = foldr seq () vs `seq` vs

-- | Scale the node centers uniformly into the frame, centered on it.
scaleToFrame :: [V] -> [V]
scaleToFrame [] = []
scaleToFrame ps = [ V ((x - cx) * s) ((y - cy) * s) | V x y <- ps ]
  where
    xs = [ x | V x _ <- ps ]
    ys = [ y | V _ y <- ps ]
    cx = (minimum xs + maximum xs) / 2
    cy = (minimum ys + maximum ys) / 2
    s  = min (frameW / max 1 (maximum xs - minimum xs))
             (frameH / max 1 (maximum ys - minimum ys))

-- ---------------------------------------------------------------------------
-- Labels and overlap
-- ---------------------------------------------------------------------------

-- | Label faces, which @tag-map.css@ must match: portals in spaced capitals,
--   subtags in italic, at these sizes (viewBox units).
labelSize :: Kind -> Double
labelSize Portal = 12.5
labelSize Subtag = 13.5
labelSize Dot    = 0

-- | The letter-spacing of portal labels, in ems.
tracking :: Double
tracking = 0.09

-- | The gap between a circle and the top of its label.
labelGap :: Double
labelGap = 3

-- | A conservative estimate of a label's width: per-character advances for
--   a book face, capitals for portals.
labelWidth :: Kind -> String -> Double
labelWidth Portal s = labelSize Portal * sum [ capAdvance c + tracking | c <- s ]
labelWidth Subtag s = labelSize Subtag * sum (map advance s)
labelWidth Dot    _ = 0

capAdvance :: Char -> Double
capAdvance c
    | c == ' '            = 0.28
    | c `elem` ("IJ" :: String) = 0.38
    | c `elem` ("MW" :: String) = 0.92
    | c `elem` ("-./,'" :: String) = 0.32
    | otherwise           = 0.72

advance :: Char -> Double
advance c
    | c == ' '                         = 0.26
    | c `elem` ("ijlrtf.,'-" :: String) = 0.30
    | c `elem` ("mw" :: String)         = 0.76
    | c `elem` ("MW" :: String)         = 0.90
    | c >= 'A' && c <= 'Z'             = 0.68
    | otherwise                        = 0.50

-- | Where a label's text is anchored, for a circle of radius r at (x, y):
--   its x, its baseline, and its text-anchor.
labelAnchor :: Kind -> Double -> Place -> V -> (Double, Double, String)
labelAnchor kind r place (V x y) = case place of
    Below   -> (x, y + r + labelGap + capHeight, "middle")
    Above   -> (x, y - r - labelGap - descent kind, "middle")
    RightOf -> (x + r + labelGap, y + middle, "start")
    LeftOf  -> (x - r - labelGap, y + middle, "end")
  where
    size      = labelSize kind
    capHeight = 0.70 * size
    -- The baseline that centers the label's capitals on the circle.
    middle    = 0.33 * size

-- | How far a label reaches below its baseline.
descent :: Kind -> Double
descent Portal = 0.05 * labelSize Portal
descent _      = 0.24 * labelSize Subtag

type Box = (Double, Double, Double, Double)

-- | A label's own box.
labelBox :: Kind -> Double -> String -> Place -> V -> Box
labelBox kind r name place at =
    case anchor of
        "middle" -> (x - w / 2, top, x + w / 2, bottom)
        "start"  -> (x, top, x + w, bottom)
        _        -> (x - w, top, x, bottom)
  where
    (x, base, anchor) = labelAnchor kind r place at
    w      = labelWidth kind name
    top    = base - 0.72 * labelSize kind
    bottom = base + descent kind

circleBox :: Double -> V -> Box
circleBox r (V x y) = (x - r, y - r, x + r, y + r)

-- | A node's box: its circle, and its label if it has one.
nodeBox :: Spec -> Place -> V -> Box
nodeBox s place at = case sLabel s of
    Nothing   -> circleBox (sRadius s) at
    Just name -> union (circleBox (sRadius s) at) (labelBox (sKind s) (sRadius s) name place at)
  where
    union (a0, b0, a1, b1) (c0, d0, c1, d1) = (min a0 c0, min b0 d0, max a1 c1, max b1 d1)

-- | Choose each tag's label side, largest tags first: the side whose box
--   crosses the fewest lines, covers the fewest other nodes and labels
--   already placed, with a slight preference for below, then above.
choosePlaces :: [Spec] -> [(Int, Int)] -> [V] -> [Place]
choosePlaces specs segments ps =
    [ Map.findWithDefault Below i chosen | i <- [0 .. length specs - 1] ]
  where
    sq     = Seq.fromList ps
    at i   = Seq.index sq i
    order  = sortOn (\(i, s) -> (Down (sRadius s), i))
                    [ (i, s) | (i, s) <- zip [0 :: Int ..] specs, sLabel s /= Nothing ]
    chosen = foldl' pick Map.empty order
    pick done (i, s) =
        let name = fromMaybe "" (sLabel s)
            others = [ labelBox (sKind o) (sRadius o) (fromMaybe "" (sLabel o)) pl (at j)
                     | (j, pl) <- Map.toList done, let o = specs !! j ]
            cost pl =
                let b = labelBox (sKind s) (sRadius s) name pl (at i)
                    crossings = length [ () | (u, v) <- segments, segmentHits (at u) (at v) b ]
                    covered = length [ () | (j, o) <- zip [0 ..] specs, j /= i
                                          , hits (circleBox (sRadius o + clearance) (at j)) b ]
                    clashes = length (filter (hits b) others)
                in fromIntegral crossings + 3 * fromIntegral covered
                   + 6 * fromIntegral clashes + preference pl
        in Map.insert i (minimumOn cost [minBound .. maxBound]) done
    hits a b = overlap 0 a b /= Nothing
    preference :: Place -> Double
    preference Below   = 0
    preference Above   = 0.4
    preference RightOf = 0.6
    preference LeftOf  = 0.7

-- | Whether the segment from one point to another enters a box
--   (Liang–Barsky clipping).
segmentHits :: V -> V -> Box -> Bool
segmentHits (V x0 y0) (V x1 y1) (bx0, by0, bx1, by1) =
    go [(-dx, x0 - bx0), (dx, bx1 - x0), (-dy, y0 - by0), (dy, by1 - y0)] 0 1
  where
    dx = x1 - x0
    dy = y1 - y0
    go [] t0 t1 = t0 <= t1
    go ((p, q) : rest) t0 t1
        | p == 0    = q >= 0 && go rest t0 t1
        | p < 0     = let r = q / p in r <= t1 && go rest (max t0 r) t1
        | otherwise = let r = q / p in r >= t0 && go rest t0 (min t1 r)

-- | Push overlapping boxes apart, along whichever axis they overlap less,
--   until none overlap or the sweeps run out. Dots move more readily than
--   tags. Two dots may touch; nothing may touch a tag's label.
relax :: [Spec] -> [Place] -> [V] -> [V]
relax specs places = go (300 :: Int)
  where
    n     = length specs
    nodes = Seq.fromList (zip specs places)
    mass s = if sKind s == Dot then 1 else 3
    go 0 ps = ps
    go k ps =
        let sq = Seq.fromList ps
            moves =
                [ mv
                | i <- [0 .. n - 1], j <- [i + 1 .. n - 1]
                , let (si, pi') = Seq.index nodes i
                      (sj, pj)  = Seq.index nodes j
                      pad = if sKind si == Dot && sKind sj == Dot then 0.5 else clearance
                , Just (dx, dy) <- [overlap pad (nodeBox si pi' (Seq.index sq i))
                                                (nodeBox sj pj (Seq.index sq j))]
                , let mi = mass si; mj = mass sj
                      fi = mj / (mi + mj); fj = mi / (mi + mj)
                , mv <- [(i, V (-dx * fi) (-dy * fi)), (j, V (dx * fj) (dy * fj))] ]
        in if null moves then ps
           else let d = Map.fromListWith addV moves
                in go (k - 1) (forceAll [ addV p (Map.findWithDefault (V 0 0) i d)
                                        | (i, p) <- zip [0 ..] ps ])

-- | Half of how far the second box must move to clear the first (with a
--   pad), on the cheaper axis; Nothing when they are already clear.
overlap :: Double -> Box -> Box -> Maybe (Double, Double)
overlap pad (a0, b0, a1, b1) (c0, d0, c1, d1)
    | ox <= 0 || oy <= 0 = Nothing
    | ox < oy   = Just (sgn ((c0 + c1) - (a0 + a1)) * ox * 0.5, 0)
    | otherwise = Just (0, sgn ((d0 + d1) - (b0 + b1)) * oy * 0.5)
  where
    ox = min a1 c1 - max a0 c0 + pad
    oy = min b1 d1 - max b0 d0 + pad
    sgn v = if v < 0 then -1 else 1

-- | The element with the least key; the first such, on a tie.
minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f = foldr1 (\a b -> if f a <= f b then a else b)

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

-- | How lit a node or line is: neither, the focused region's neighborhood,
--   or the region itself. A line is as lit as its less lit end.
data Lit = Dim | Near | Lit deriving (Eq, Ord)

-- | What a drawing lights: nothing (the whole map), a tag's region, or
--   one piece (by its index in 'lyPieces').
data Focus = Whole | OnTag String | OnPiece Int deriving (Eq)

-- | Draw the map: whole ('Nothing'), or with one tag's region lit, after a
--   visually hidden paragraph that describes it. The lit
--   region is the tag and its descendants, the pieces filed under any of
--   them, and the lines among those; one notch less lit are the other tags
--   those pieces carry and the tag's ancestors. The rest is dimmed by the
--   stylesheet, still visible. A tag not on the map draws it whole.
renderMap :: Layout -> Maybe String -> String
renderMap ly focus0 = concat
    [ "<p class=\"visually-hidden\">", escapeHtml label, ". ", escapeHtml description, "</p>"
    , draw ly focus Nothing ]
  where
    tagAt = Map.fromList [ (mtTag t, t) | (t, _, _) <- lyTags ly ]
    focus = case focus0 of
        Just t | Map.member t tagAt -> OnTag t
        _                           -> Whole
    (_, corePieces, near) = region ly focus
    nameOf g = maybe g mtName (Map.lookup g tagAt)
    label = case focus of
        OnTag f -> "Map of tags, with " ++ nameOf f ++ " lit"
        _       -> "Map of tags"
    description = concat
        [ "A map of the ", show (length (lyTags ly)), " tags and the "
        , countWords (length (lyPieces ly)), " filed under them. Each top-level tag "
        , "is a labeled circle, sized by how many pieces it holds, and each subtag a "
        , "smaller one joined to its parent by a dashed line; each piece is a dot "
        , "joined to the tags it is filed under."
        , case focus of
            OnTag f -> concat
                [ " The region of ", nameOf f, " is lit: "
                , countWords (Set.size corePieces)
                , case [ nameOf g | g <- Set.toList near, g `notElem` ancestorsOn ly f ] of
                    []  -> "."
                    gs  -> ", and the tags they are also filed under: "
                           ++ intercalate ", " (sort gs) ++ "." ]
            _ -> "" ]

-- | The detail of the map around one piece, for the foot of its page: the
--   piece ringed, the tags it is filed under lit with its lines to them,
--   their ancestors one notch less, and the rest dimmed; cropped to that
--   neighborhood, centered on the piece, so that its labels read at a
--   third of a page's width. 'Nothing' for a piece not on the map (by its
--   'mpId'). Nothing outside the crop is drawn. It has no visually hidden
--   description: the page lists the same tags in words beside it.
renderPieceMap :: Layout -> String -> Maybe String
renderPieceMap ly pid =
    case [ i | (i, (p, _)) <- zip [0 ..] (lyPieces ly), mpId p == pid ] of
        (i : _) -> Just (draw ly (OnPiece i) (Just (pieceCrop ly i)))
        []      -> Nothing

-- | The least width of a piece's crop, in viewBox units: a piece filed
--   under one tag nearby is not drawn larger than about twice the whole.
cropMinW :: Double
cropMinW = 300

-- | The crop's proportions, width over height, at the least and most.
cropAspect :: (Double, Double)
cropAspect = (1.05, 1.6)

-- | The crop around a piece: the box of everything lit or near (circles
--   and labels), padded, made symmetric about the piece, held to
--   'cropMinW' and 'cropAspect', then moved back toward the map where it
--   overhangs it by more than the padding (so that a lit label at the
--   map's edge clears the fade); no larger than the map and its padding.
pieceCrop :: Layout -> Int -> Box
pieceCrop ly i = (x0', y0', x0' + w', y0' + h')
  where
    (_, V px py) = lyPieces ly !! i
    (core, _, near) = region ly (OnPiece i)
    boxes = circleBox (radius Dot 0) (V px py)
          : [ nodeBox (Spec (kindOf t) r (Just (mtName t))) place at
            | (t, at, place) <- lyTags ly
            , mtTag t `Set.member` core || mtTag t `Set.member` near
            , let r = radius (kindOf t) (mtCount t) ]
    pad = 24
    bx0 = minimum [ a | (a, _, _, _) <- boxes ] - pad
    by0 = minimum [ b | (_, b, _, _) <- boxes ] - pad
    bx1 = maximum [ c | (_, _, c, _) <- boxes ] + pad
    by1 = maximum [ d | (_, _, _, d) <- boxes ] + pad
    hw0 = max (cropMinW / 2) (max (px - bx0) (bx1 - px))
    hh0 = max (py - by0) (by1 - py)
    (lo, hi) = cropAspect
    (hw, hh) | hw0 < lo * hh0 = (lo * hh0, hh0)
             | hw0 > hi * hh0 = (hw0, hw0 / hi)
             | otherwise      = (hw0, hh0)
    w' = min (lyWidth ly + 2 * pad) (2 * hw)
    h' = min (lyHeight ly + 2 * pad) (2 * hh)
    x0' = max (-pad) (min (lyWidth ly + pad - w') (px - w' / 2))
    y0' = max (-pad) (min (lyHeight ly + pad - h') (py - h' / 2))

-- | The tags of a focus's lit region, the pieces lit with them, and the
--   tags one notch less lit. A tag's region: the tag and its descendants,
--   the pieces filed under any of them, and near, the other tags those
--   pieces carry and the tag's ancestors. A piece's: the tags it is filed
--   under, itself, and near, their ancestors.
region :: Layout -> Focus -> (Set.Set String, Set.Set Int, Set.Set String)
region ly focus = case focus of
    Whole -> (Set.empty, Set.empty, Set.empty)
    OnTag f ->
        let core = Set.fromList [ mtTag t | (t, _, _) <- lyTags ly
                                          , mtTag t == f || (f ++ "/") `isPrefixOf` mtTag t ]
            lit  = Set.fromList [ i | (i, (p, _)) <- pieces, any (`Set.member` core) (mpTags p) ]
            near = (Set.fromList [ g | (i, (p, _)) <- pieces, i `Set.member` lit, g <- mpTags p ]
                    `Set.union` Set.fromList (ancestorsOn ly f))
                   `Set.difference` core
        in (core, lit, near)
    OnPiece i ->
        let core = Set.fromList (mpTags (fst (lyPieces ly !! i)))
            near = Set.fromList (concatMap (ancestorsOn ly) (Set.toList core))
                   `Set.difference` core
        in (core, Set.singleton i, near)
  where
    pieces = zip [0 :: Int ..] (lyPieces ly)

-- | A tag's ancestors that are on the map.
ancestorsOn :: Layout -> String -> [String]
ancestorsOn ly f = [ a | (n, c) <- zip [0 ..] f, c == '/', let a = take n f
                       , any (\(t, _, _) -> mtTag t == a) (lyTags ly) ]

-- | The SVG of a drawing: the whole map, or one with a focus lit, and
--   either the whole frame or a crop of it (only what meets the crop
--   drawn).
draw :: Layout -> Focus -> Maybe Box -> String
draw ly focus crop = concat
    [ "<svg xmlns=\"http://www.w3.org/2000/svg\" class=\"tag-map"
    , if focused then " tag-map--focus" else ""
    , maybe "" (const " tag-map--detail") crop
    , "\" viewBox=\"", viewBox
    , "\" aria-hidden=\"true\" focusable=\"false\""
    , case focus of
        OnTag t -> " data-focus=\"" ++ escapeHtml t ++ "\""
        _       -> ""
    , ">"
    , "<g class=\"tag-map-edges\">", concat parentLines, concat filedLines, "</g>"
    , "<g class=\"tag-map-pieces\">", concat dots, "</g>"
    , "<g class=\"tag-map-tags\">", concat tagNodes, "</g>"
    , "</svg>" ]
  where
    focused = focus /= Whole
    tagAt   = Map.fromList [ (mtTag t, (t, p)) | (t, p, _) <- lyTags ly ]
    pieces  = zip [0 :: Int ..] (lyPieces ly)
    (core, corePieces, near) = region ly focus

    viewBox = case crop of
        Nothing -> "0 0 " ++ num (lyWidth ly) ++ " " ++ num (lyHeight ly)
        Just (a, b, c, d) -> unwords (map num [a, b, c - a, d - b])
    -- Whether a box, or a line, meets the crop: everything does uncropped.
    shows' bx = case crop of
        Nothing -> True
        Just c  -> overlap 0 c bx /= Nothing
    showsLine u v = case crop of
        Nothing -> True
        Just c  -> segmentHits u v c

    tagLit g
        | not focused          = Lit
        | g `Set.member` core  = Lit
        | g `Set.member` near  = Near
        | otherwise            = Dim
    pieceLit i
        | not focused || i `Set.member` corePieces = Lit
        | otherwise = Dim

    litClass Lit  = if focused then " is-lit" else ""
    litClass Near = " is-near"
    litClass Dim  = ""

    radiusOf t = radius (kindOf t) (mtCount t)

    -- A line between two circles, trimmed to their rims.
    line cls extra a@(V x1 y1) r1 b@(V x2 y2) r2 =
        let dx = x2 - x1; dy = y2 - y1
            d  = max 0.01 (sqrt (dx * dx + dy * dy))
            ux = dx / d; uy = dy / d
        in if d <= r1 + r2 || not (showsLine a b) then "" else concat
            [ "<line class=\"", cls, "\"", extra
            , " x1=\"", num (x1 + ux * r1), "\" y1=\"", num (y1 + uy * r1)
            , "\" x2=\"", num (x2 - ux * r2), "\" y2=\"", num (y2 - uy * r2), "\"/>" ]

    parentLines =
        [ line ("tag-map-edge tag-map-edge--parent" ++ litClass (min (tagLit (mtTag t)) (tagLit p)))
               (" data-tag=\"" ++ escapeHtml (mtTag t) ++ "\" data-parent=\"" ++ escapeHtml p ++ "\"")
               at (radiusOf t) pat (radiusOf pt)
        | (t, at, _) <- lyTags ly, Just p <- [mtParent t], Just (pt, pat) <- [Map.lookup p tagAt] ]

    filedLines =
        [ line ("tag-map-edge tag-map-edge--filed" ++ litClass lvl)
               (" data-piece=\"" ++ show i ++ "\" data-tag=\"" ++ escapeHtml g ++ "\"")
               at (radius Dot 0) tat (radiusOf t)
        | (i, (p, at)) <- pieces, g <- mpTags p, Just (t, tat) <- [Map.lookup g tagAt]
        , let lvl = if pieceLit i == Dim then Dim else tagLit g ]

    dots =
        [ concat
            [ "<a class=\"tag-map-piece", litClass (pieceLit i), if here then " is-here" else ""
            , "\" href=\"", escapeHtml (mpUrl p)
            , "\" tabindex=\"-1\" data-piece=\"", show i
            , "\" data-tags=\"", escapeHtml (unwords (mpTags p)), "\">"
            , "<title>", escapeHtml (mpTitle p), "</title>"
            , if here then ring x y (radius Dot 0 + 4.5) else ""
            , "<circle cx=\"", num x, "\" cy=\"", num y, "\" r=\"", num (radius Dot 0), "\"/></a>" ]
        | (i, (p, at@(V x y))) <- pieces
        , shows' (circleBox (radius Dot 0 + 5) at)
        , let here = focus == OnPiece i ]

    ring x y r = concat [ "<circle class=\"tag-map-here\" cx=\"", num x, "\" cy=\"", num y
                        , "\" r=\"", num r, "\"/>" ]

    -- Portals last, so that their labels sit over subtags' lines.
    tagNodes =
        [ concat
            [ "<a class=\"tag-map-tag tag-map-tag--", kindName (kindOf t)
            , if mtFacet t == "research" then " tag-map-tag--research" else ""
            , litClass (tagLit (mtTag t)), if here then " is-here" else ""
            , "\" href=\"", escapeHtml (mtUrl t)
            , "\" tabindex=\"-1\" data-tag=\"", escapeHtml (mtTag t), "\">"
            , "<title>", escapeHtml (mtName t ++ " (" ++ countWords (mtCount t) ++ ")"), "</title>"
            , if here then ring x y (r + 3.5) else ""
            , "<circle cx=\"", num x, "\" cy=\"", num y, "\" r=\"", num r, "\"/>"
            , "<text x=\"", num lx, "\" y=\"", num ly', "\" text-anchor=\"", anchor, "\">"
            , escapeHtml (mtName t), "</text></a>" ]
        | (t, V x y, place) <- sortOn (\(t, _, _) -> kindOf t == Portal) (lyTags ly)
        , let r = radiusOf t
              here = focus == OnTag (mtTag t)
              (lx, ly', anchor) = labelAnchor (kindOf t) r place (V x y)
        , shows' (nodeBox (Spec (kindOf t) r (Just (mtName t))) place (V x y)) ]

    kindName Portal = "portal"
    kindName _      = "subtag"

countWords :: Int -> String
countWords 1 = "1 piece"
countWords n = show n ++ " pieces"

-- | A coordinate to one decimal, with no negative zero.
num :: Double -> String
num v = (if t < 0 then "-" else "") ++ show q ++ (if r == 0 then "" else '.' : show r)
  where
    t = round (v * 10) :: Int
    (q, r) = abs t `quotRem` 10
