module Alphababy.Strokes
  ( Point
  , Stroke
  , Template
  , template
  , hasTemplate
  , spacing
  , bounds
  , Bounds
  , mirror
  , Trail
  , trail
  , templateLength
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (maximum, minimum, sum)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Tuple (Tuple(..))

type Point = { x :: Number, y :: Number }

-- | A stroke is a polyline, sampled at roughly `spacing` intervals and in
-- | writing direction.
type Stroke = Array Point

-- | How a glyph is written, stroke by stroke, in the order a child is
-- | taught. Coordinates follow three-line handwriting paper: `y = 0` is the
-- | top line, `0.5` the dashed middle line, `1` the baseline and `1.45`
-- | the bottom of descenders. `x` starts at 0 on the left.
type Template = Array Stroke

type Bounds = { left :: Number, right :: Number, top :: Number, bottom :: Number }

-- | Distance between neighbouring sample points.
spacing :: Number
spacing = 0.02

template :: String -> Maybe Template
template glyph = Map.lookup glyph templates

hasTemplate :: String -> Boolean
hasTemplate glyph = Map.member glyph templates

bounds :: Array Point -> Bounds
bounds points =
  { left: fromMaybe 0.0 (minimum xs)
  , right: fromMaybe 0.0 (maximum xs)
  , top: fromMaybe 0.0 (minimum ys)
  , bottom: fromMaybe 0.0 (maximum ys)
  }
  where
  xs = map _.x points
  ys = map _.y points

-- | Left-right mirror image within the same bounds.
mirror :: Template -> Template
mirror strokes = do
  let
    b = bounds (Array.concat strokes)
  map (map (\p -> p { x = b.left + b.right - p.x })) strokes

-- | Part of a template, as when drawn by a moving pen.
type Trail = { drawn :: Template, pen :: Maybe Point, finished :: Boolean }

strokeLength :: Stroke -> Number
strokeLength s = sum (Array.zipWith distance s (Array.drop 1 s))

-- | Total length of all strokes plus `gap` between each.
templateLength :: Number -> Template -> Number
templateLength gap t = sum (map strokeLength t) + gap * Int.toNumber (max 0 (Array.length t - 1))

-- | What a pen has drawn after travelling `travelled` along the strokes in
-- | order, lifting for `gap` between strokes.
trail :: Number -> Number -> Template -> Trail
trail gap travelled t = go travelled [] (Array.uncons t)
  where
  go left drawn = case _ of
    Nothing -> { drawn, pen: Nothing, finished: true }
    Just { head, tail } -> do
      let
        len = strokeLength head
      if left >= len + gap || left >= len && Array.null tail then go (left - len - gap) (Array.snoc drawn head) (Array.uncons tail)
      else if left >= len then { drawn: Array.snoc drawn head, pen: Array.last head, finished: false }
      else do
        let
          part = prefix left head
        { drawn: Array.snoc drawn part, pen: Array.last part, finished: false }

  -- The first `len` of a stroke, ending exactly there.
  prefix len s = walk len [] (Array.zip s (Array.drop 1 s))
    where
    walk left acc segments = case Array.uncons segments of
      Nothing -> s
      Just { head: Tuple a b, tail } -> do
        let
          d = distance a b
          acc' = if Array.null acc then [ a ] else acc
        if left <= d then Array.snoc acc' (if d <= 0.0 then b else { x: a.x + (b.x - a.x) * left / d, y: a.y + (b.y - a.y) * left / d })
        else walk (left - d) (Array.snoc acc' b) tail

distance :: Point -> Point -> Number
distance a b = Number.sqrt ((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y))

-- Pieces ---------------------------------------------------------------------

-- | Arcs are elliptical, with angles in degrees measured clockwise on
-- | screen from 3 o'clock (so 90 is straight down). A decreasing angle
-- | draws counterclockwise, the way `c` and `o` are written.
data Piece
  = Line Point Point
  | Arc { cx :: Number, cy :: Number, rx :: Number, ry :: Number, from :: Number, to :: Number }
  | Dot Point

sample :: Piece -> Array Point
sample = case _ of
  Dot p -> [ p ]
  Line a b -> do
    let
      n = steps (Number.sqrt ((b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y)))
    map (\i -> lerp a b (Int.toNumber i / Int.toNumber n)) (Array.range 0 n)
  Arc a -> do
    let
      sweep = a.to - a.from
      n = steps (Number.abs sweep * Number.pi / 180.0 * Number.max a.rx a.ry)
      at t = do
        let
          angle = (a.from + sweep * t) * Number.pi / 180.0
        { x: a.cx + a.rx * Number.cos angle, y: a.cy + a.ry * Number.sin angle }
    map (\i -> at (Int.toNumber i / Int.toNumber n)) (Array.range 0 n)
  where
  steps len = max 1 (Int.ceil (len / spacing))
  lerp a b t = { x: a.x + (b.x - a.x) * t, y: a.y + (b.y - a.y) * t }

-- | Joins pieces into one stroke, dropping the duplicated joint points.
stroke :: Array Piece -> Stroke
stroke pieces = Array.concat (Array.mapWithIndex (\i p -> if i == 0 then sample p else Array.drop 1 (sample p)) pieces)

pt :: Number -> Number -> Point
pt x y = { x, y }

line :: Number -> Number -> Number -> Number -> Piece
line x1 y1 x2 y2 = Line (pt x1 y1) (pt x2 y2)

-- | A polyline through the given points.
path :: Array Point -> Array Piece
path points = Array.zipWith Line points (Array.drop 1 points)

arc :: Number -> Number -> Number -> Number -> Number -> Number -> Piece
arc cx cy rx ry from to = Arc { cx, cy, rx, ry, from, to }

dot :: Number -> Number -> Piece
dot x y = Dot (pt x y)

-- Glyphs ---------------------------------------------------------------------

-- Shapes follow ball-and-stick manuscript print, as taught in preschool.
templates :: Map String Template
templates = Map.fromFoldable (map (\(Tuple k pieces) -> Tuple k (map stroke pieces)) glyphs)

glyphs :: Array (Tuple String (Array (Array Piece)))
glyphs =
  [ Tuple "A" [ [ line 0.4 0.0 0.0 1.0 ], [ line 0.4 0.0 0.8 1.0 ], [ line 0.15 0.62 0.65 0.62 ] ]
  , Tuple "B"
      [ [ line 0.0 0.0 0.0 1.0 ]
      , [ line 0.0 0.0 0.36 0.0, arc 0.36 0.25 0.25 0.25 (-90.0) 90.0, line 0.36 0.5 0.0 0.5 ]
      , [ line 0.0 0.5 0.4 0.5, arc 0.4 0.75 0.27 0.25 (-90.0) 90.0, line 0.4 1.0 0.0 1.0 ]
      ]
  , Tuple "C" [ [ arc 0.47 0.5 0.47 0.5 (-40.0) (-320.0) ] ]
  , Tuple "D" [ [ line 0.0 0.0 0.0 1.0 ], [ line 0.0 0.0 0.3 0.0, arc 0.3 0.5 0.5 0.5 (-90.0) 90.0, line 0.3 1.0 0.0 1.0 ] ]
  , Tuple "E" [ [ line 0.0 0.0 0.0 1.0 ], [ line 0.0 0.0 0.65 0.0 ], [ line 0.0 0.5 0.5 0.5 ], [ line 0.0 1.0 0.65 1.0 ] ]
  , Tuple "F" [ [ line 0.0 0.0 0.0 1.0 ], [ line 0.0 0.0 0.65 0.0 ], [ line 0.0 0.5 0.5 0.5 ] ]
  , Tuple "G" [ [ arc 0.47 0.5 0.47 0.5 (-40.0) (-330.0), line 0.877 0.75 0.877 0.55, line 0.877 0.55 0.55 0.55 ] ]
  , Tuple "H" [ [ line 0.0 0.0 0.0 1.0 ], [ line 0.7 0.0 0.7 1.0 ], [ line 0.0 0.5 0.7 0.5 ] ]
  , Tuple "I" [ [ line 0.2 0.0 0.2 1.0 ], [ line 0.0 0.0 0.4 0.0 ], [ line 0.0 1.0 0.4 1.0 ] ]
  , Tuple "J" [ [ line 0.55 0.0 0.55 0.72, arc 0.3 0.72 0.25 0.28 0.0 180.0 ] ]
  , Tuple "K" [ [ line 0.0 0.0 0.0 1.0 ], path [ pt 0.65 0.0, pt 0.0 0.55, pt 0.65 1.0 ] ]
  , Tuple "L" [ path [ pt 0.0 0.0, pt 0.0 1.0, pt 0.6 1.0 ] ]
  , Tuple "M" [ [ line 0.0 0.0 0.0 1.0 ], path [ pt 0.0 0.0, pt 0.45 0.7, pt 0.9 0.0, pt 0.9 1.0 ] ]
  , Tuple "N" [ [ line 0.0 0.0 0.0 1.0 ], path [ pt 0.0 0.0, pt 0.7 1.0, pt 0.7 0.0 ] ]
  , Tuple "O" [ [ arc 0.45 0.5 0.45 0.5 (-90.0) (-450.0) ] ]
  , Tuple "P" [ [ line 0.0 0.0 0.0 1.0 ], [ line 0.0 0.0 0.38 0.0, arc 0.38 0.25 0.25 0.25 (-90.0) 90.0, line 0.38 0.5 0.0 0.5 ] ]
  , Tuple "Q" [ [ arc 0.45 0.5 0.45 0.5 (-90.0) (-450.0) ], [ line 0.55 0.7 0.9 1.02 ] ]
  , Tuple "R"
      [ [ line 0.0 0.0 0.0 1.0 ]
      , [ line 0.0 0.0 0.38 0.0, arc 0.38 0.25 0.25 0.25 (-90.0) 90.0, line 0.38 0.5 0.0 0.5 ]
      , [ line 0.28 0.5 0.7 1.0 ]
      ]
  , Tuple "S" [ [ arc 0.35 0.25 0.32 0.25 (-20.0) (-270.0), arc 0.35 0.75 0.35 0.25 (-90.0) 160.0 ] ]
  , Tuple "T" [ [ line 0.35 0.0 0.35 1.0 ], [ line 0.0 0.0 0.7 0.0 ] ]
  , Tuple "U" [ [ line 0.0 0.0 0.0 0.65, arc 0.35 0.65 0.35 0.35 180.0 0.0, line 0.7 0.65 0.7 0.0 ] ]
  , Tuple "V" [ path [ pt 0.0 0.0, pt 0.4 1.0, pt 0.8 0.0 ] ]
  , Tuple "W" [ path [ pt 0.0 0.0, pt 0.25 1.0, pt 0.5 0.3, pt 0.75 1.0, pt 1.0 0.0 ] ]
  , Tuple "X" [ [ line 0.0 0.0 0.7 1.0 ], [ line 0.7 0.0 0.0 1.0 ] ]
  , Tuple "Y" [ [ line 0.0 0.0 0.35 0.5 ], path [ pt 0.7 0.0, pt 0.35 0.5, pt 0.35 1.0 ] ]
  , Tuple "Z" [ path [ pt 0.0 0.0, pt 0.7 0.0, pt 0.0 1.0, pt 0.7 1.0 ] ]
  -- Little letters sit between the middle line and the baseline.
  , Tuple "a" [ littleCircle, [ line 0.5 0.5 0.5 1.0 ] ]
  , Tuple "b" [ [ line 0.0 0.0 0.0 1.0 ], [ arc 0.25 0.75 0.25 0.25 180.0 540.0 ] ]
  , Tuple "c" [ [ arc 0.25 0.75 0.24 0.25 (-40.0) (-320.0) ] ]
  , Tuple "d" [ littleCircle, [ line 0.5 0.0 0.5 1.0 ] ]
  , Tuple "e" [ [ line 0.0 0.75 0.5 0.75, arc 0.25 0.75 0.25 0.25 0.0 (-320.0) ] ]
  , Tuple "f" [ [ arc 0.33 0.18 0.15 0.16 (-20.0) (-180.0), line 0.18 0.18 0.18 1.0 ], [ line 0.0 0.5 0.4 0.5 ] ]
  , Tuple "g" [ littleCircle, [ line 0.5 0.5 0.5 1.25, arc 0.27 1.25 0.23 0.2 0.0 160.0 ] ]
  , Tuple "h" [ [ line 0.0 0.0 0.0 1.0 ], [ arc 0.25 0.75 0.25 0.25 180.0 360.0, line 0.5 0.75 0.5 1.0 ] ]
  , Tuple "i" [ [ line 0.05 0.5 0.05 1.0 ], [ dot 0.05 0.3 ] ]
  , Tuple "j" [ [ line 0.38 0.5 0.38 1.25, arc 0.18 1.25 0.2 0.2 0.0 160.0 ], [ dot 0.38 0.3 ] ]
  , Tuple "k" [ [ line 0.0 0.0 0.0 1.0 ], path [ pt 0.42 0.5, pt 0.02 0.78, pt 0.45 1.0 ] ]
  , Tuple "l" [ [ line 0.05 0.0 0.05 1.0 ] ]
  , Tuple "m"
      [ [ line 0.0 0.5 0.0 1.0 ]
      , [ arc 0.2 0.72 0.2 0.22 180.0 360.0, line 0.4 0.72 0.4 1.0 ]
      , [ arc 0.6 0.72 0.2 0.22 180.0 360.0, line 0.8 0.72 0.8 1.0 ]
      ]
  , Tuple "n" [ [ line 0.0 0.5 0.0 1.0 ], [ arc 0.25 0.75 0.25 0.25 180.0 360.0, line 0.5 0.75 0.5 1.0 ] ]
  , Tuple "o" [ [ arc 0.25 0.75 0.25 0.25 (-90.0) (-450.0) ] ]
  , Tuple "p" [ [ line 0.0 0.5 0.0 1.45 ], [ arc 0.25 0.75 0.25 0.25 180.0 540.0 ] ]
  , Tuple "q" [ littleCircle, [ line 0.5 0.5 0.5 1.45 ] ]
  , Tuple "r" [ [ line 0.0 0.5 0.0 1.0 ], [ arc 0.22 0.72 0.22 0.22 180.0 315.0 ] ]
  , Tuple "s" [ [ arc 0.2 0.625 0.18 0.125 (-20.0) (-270.0), arc 0.2 0.875 0.2 0.125 (-90.0) 160.0 ] ]
  , Tuple "t" [ [ line 0.2 0.15 0.2 1.0 ], [ line 0.0 0.5 0.4 0.5 ] ]
  , Tuple "u" [ [ line 0.0 0.5 0.0 0.75, arc 0.25 0.75 0.25 0.25 180.0 0.0, line 0.5 0.75 0.5 0.5 ], [ line 0.5 0.5 0.5 1.0 ] ]
  , Tuple "v" [ path [ pt 0.0 0.5, pt 0.25 1.0, pt 0.5 0.5 ] ]
  , Tuple "w" [ path [ pt 0.0 0.5, pt 0.2 1.0, pt 0.4 0.6, pt 0.6 1.0, pt 0.8 0.5 ] ]
  , Tuple "x" [ [ line 0.0 0.5 0.5 1.0 ], [ line 0.5 0.5 0.0 1.0 ] ]
  , Tuple "y" [ [ line 0.0 0.5 0.25 1.0 ], [ line 0.5 0.5 0.05 1.45 ] ]
  , Tuple "z" [ path [ pt 0.0 0.5, pt 0.5 0.5, pt 0.0 1.0, pt 0.5 1.0 ] ]
  -- Digits are as tall as capitals.
  , Tuple "0" [ [ arc 0.3 0.5 0.3 0.5 (-90.0) (-450.0) ] ]
  , Tuple "1" [ path [ pt 0.02 0.2, pt 0.25 0.0, pt 0.25 1.0 ] ]
  , Tuple "2" [ [ arc 0.3 0.28 0.28 0.26 (-160.0) 30.0, line 0.542 0.41 0.0 1.0, line 0.0 1.0 0.6 1.0 ] ]
  , Tuple "3" [ [ arc 0.28 0.25 0.27 0.25 (-160.0) 90.0, arc 0.28 0.75 0.3 0.25 (-90.0) 160.0 ] ]
  , Tuple "4" [ path [ pt 0.45 0.0, pt 0.0 0.68, pt 0.65 0.68 ], [ line 0.45 0.0 0.45 1.0 ] ]
  , Tuple "5" [ [ line 0.05 0.0 0.07 0.5, arc 0.3 0.68 0.28 0.3 (-145.0) 150.0 ], [ line 0.05 0.0 0.55 0.0 ] ]
  , Tuple "6" [ [ arc 0.55 0.72 0.5 0.7 (-100.0) (-180.0), arc 0.3 0.72 0.25 0.28 180.0 (-180.0) ] ]
  , Tuple "7" [ path [ pt 0.0 0.0, pt 0.6 0.0, pt 0.2 1.0 ] ]
  , Tuple "8" [ [ arc 0.28 0.25 0.23 0.25 (-20.0) (-270.0), arc 0.28 0.75 0.27 0.25 (-90.0) 270.0, arc 0.28 0.25 0.23 0.25 90.0 (-20.0) ] ]
  , Tuple "9" [ [ arc 0.27 0.28 0.25 0.26 (-10.0) (-370.0) ], [ line 0.52 0.24 0.5 1.0 ] ]
  ]
  where
  -- The round part of a, d, g and q: counterclockwise from 2 o'clock.
  littleCircle = [ arc 0.25 0.75 0.25 0.25 (-20.0) (-380.0) ]
