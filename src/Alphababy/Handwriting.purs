module Alphababy.Handwriting
  ( Ink
  , Match
  , Verdict(..)
  , Judgement
  , passed
  , TraceProgress
  , startTrace
  , lightUp
  , litFraction
  , strokeDone
  , traceComplete
  , nextStroke
  , judgeTrace
  , FreehandTask
  , judgeFreehand
  , match
  , densify
  , normalize
  , overlay
  , extent
  ) where

import Prelude

import Alphababy.Strokes (Point, Stroke, Template)
import Alphababy.Strokes as Strokes
import Data.Array as Array
import Data.Foldable (foldl, maximumBy, sum)
import Data.Function (on)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Tuple (Tuple(..), snd)

-- | What she drew: one array of points per finger stroke, in the same
-- | units as the templates (the height of a capital letter is 1).
type Ink = Array Stroke

-- | How well ink and a template agree. `coverage` is how much of the
-- | template has ink near it, `precision` how much of the ink lies near the
-- | template, and `score` their harmonic mean. Each is in `[0, 1]`.
type Match = { coverage :: Number, precision :: Number, score :: Number }

-- | Why an attempt did or did not work. `LooksLike` carries whatever
-- | identifies the look-alike she wrote instead.
data Verdict a
  = Neat
  | OffPath
  | Unfinished
  | Backwards
  | LooksLike a
  | Messy
  | TooSmall

derive instance Eq a => Eq (Verdict a)
derive instance Functor Verdict

type Judgement a = { verdict :: Verdict a, score :: Number }

passed :: forall a. Verdict a -> Boolean
passed = case _ of
  Neat -> true
  _ -> false

-- Geometry -------------------------------------------------------------------

distance :: Point -> Point -> Number
distance a b = Number.sqrt ((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y))

distanceToSegment :: Point -> Point -> Point -> Number
distanceToSegment p a b = do
  let
    dx = b.x - a.x
    dy = b.y - a.y
    len2 = dx * dx + dy * dy
    t = if len2 <= 0.0 then 0.0 else Number.max 0.0 (Number.min 1.0 (((p.x - a.x) * dx + (p.y - a.y) * dy) / len2))
  distance p { x: a.x + dx * t, y: a.y + dy * t }

-- | Adds points along a stroke so that neighbours are at most
-- | `Strokes.spacing` apart; fast finger movements report sparse points.
densify :: Stroke -> Stroke
densify s = case Array.head s of
  Nothing -> []
  Just first -> Array.cons first (Array.concat (Array.zipWith fill s (Array.drop 1 s)))
  where
  fill a b = do
    let
      n = max 1 (Int.ceil (distance a b / Strokes.spacing))
    map (\i -> lerp a b (Int.toNumber i / Int.toNumber n)) (Array.range 1 n)
  lerp a b t = { x: a.x + (b.x - a.x) * t, y: a.y + (b.y - a.y) * t }

-- | The larger of the ink's width and height.
extent :: Ink -> Number
extent ink = do
  let
    b = Strokes.bounds (Array.concat ink)
  if Array.null (Array.concat ink) then 0.0 else Number.max (b.right - b.left) (b.bottom - b.top)

-- | Centres strokes on the origin and scales them so that their larger
-- | dimension is 1, keeping the aspect ratio (a tall `l` stays thin).
normalize :: Array Stroke -> Array Stroke
normalize strokes = do
  let
    b = Strokes.bounds (Array.concat strokes)
    cx = (b.left + b.right) / 2.0
    cy = (b.top + b.bottom) / 2.0
    size = Number.max 1.0e-6 (Number.max (b.right - b.left) (b.bottom - b.top))
  map (map (\p -> { x: (p.x - cx) / size, y: (p.y - cy) / size })) strokes

-- | The template scaled and moved to sit where the ink is, to show her
-- | how her attempt should have looked.
overlay :: Ink -> Template -> Template
overlay ink t = do
  let
    b = Strokes.bounds (Array.concat ink)
    cx = (b.left + b.right) / 2.0
    cy = (b.top + b.bottom) / 2.0
    size = Number.max 0.5 (extent ink)
  map (map (\p -> { x: cx + p.x * size, y: cy + p.y * size })) (normalize t)

-- Scoring --------------------------------------------------------------------

-- | A sample point with the undirected direction of the stroke through it
-- | (`Nothing` for a dot). Comparing directions as well as positions keeps
-- | a scribble, or an `O` crossing the stem of a `b`, from counting as a
-- | match just because it passes close by.
type Oriented = { x :: Number, y :: Number, angle :: Maybe Number }

orient :: Stroke -> Array Oriented
orient s0 = Array.mapWithIndex at s
  where
  s = densify s0
  last = Array.length s - 1
  at i p = do
    let
      angle = do
        a <- Array.index s (max 0 (i - 2))
        b <- Array.index s (min last (i + 2))
        if distance a b < 1.0e-6 then Nothing else Just (Number.atan2 (b.y - a.y) (b.x - a.x))
    { x: p.x, y: p.y, angle }

-- | How many units of distance each radian between directions costs.
turnCost :: Number
turnCost = 0.1

orientedDistance :: Oriented -> Oriented -> Number
orientedDistance p q = do
  let
    turn = case p.angle, q.angle of
      Just a, Just b -> do
        let
          d = Number.abs (a - b) `Number.remainder` Number.pi
        Number.min d (Number.pi - d)
      _, _ -> 0.0
    dx = p.x - q.x
    dy = p.y - q.y
    dt = turn * turnCost
  Number.sqrt (dx * dx + dy * dy + dt * dt)

nearest :: Array Oriented -> Oriented -> Number
nearest points p = foldl (\best q -> Number.min best (orientedDistance p q)) Number.infinity points

-- | Full credit within 40% of the tolerance, falling to none at it.
credit :: Number -> Number -> Number
credit tolerance d = Number.max 0.0 (Number.min 1.0 ((tolerance - d) / (tolerance * 0.6)))

mean :: Array Number -> Number
mean xs = if Array.null xs then 0.0 else sum xs / Int.toNumber (Array.length xs)

match :: Number -> Template -> Ink -> Match
match tolerance t ink = do
  let
    templatePoints = Array.concatMap orient t
    inkPoints = Array.concatMap orient ink
    coverage = mean (map (credit tolerance <<< nearest inkPoints) templatePoints)
    precision = mean (map (credit tolerance <<< nearest templatePoints) inkPoints)
    score = if coverage + precision <= 0.0 then 0.0 else 2.0 * coverage * precision / (coverage + precision)
  { coverage, precision, score }

-- Tracing --------------------------------------------------------------------

-- | Which template points her finger has passed over, stroke by stroke.
type TraceProgress = Array (Array Boolean)

-- | How close (in capital heights) the finger must pass to light a point.
litRadius :: Number
litRadius = 0.11

traceTolerance :: Number
traceTolerance = 0.16

startTrace :: Template -> TraceProgress
startTrace = map (map (const false))

-- | Lights the template points near the finger's path from `a` to `b`.
lightUp :: Template -> Point -> Point -> TraceProgress -> TraceProgress
lightUp t a b progress = Array.zipWith (Array.zipWith (\p lit -> lit || distanceToSegment p a b <= litRadius)) t progress

fraction :: Array Boolean -> Number
fraction xs = if Array.null xs then 1.0 else Int.toNumber (Array.length (Array.filter identity xs)) / Int.toNumber (Array.length xs)

litFraction :: TraceProgress -> Number
litFraction = fraction <<< Array.concat

strokeDone :: Array Boolean -> Boolean
strokeDone lit = fraction lit >= 0.8

-- | Every stroke is mostly lit, so that she can't skip a short one such
-- | as the bar of an `A`.
traceComplete :: TraceProgress -> Boolean
traceComplete progress = Array.all strokeDone progress && litFraction progress >= 0.9

-- | The first stroke still to be traced.
nextStroke :: TraceProgress -> Maybe Int
nextStroke = Array.findIndex (not <<< strokeDone)

-- | Judges tracing in place: the ink must follow the template where it is
-- | drawn, not just have the right shape.
judgeTrace :: Template -> Ink -> Judgement Void
judgeTrace t ink
  | Array.null (Array.concat ink) = { verdict: Unfinished, score: 0.0 }
  | otherwise = do
      let
        m = match traceTolerance t ink
        verdict
          | m.precision < 0.6 = OffPath
          | m.coverage < 0.75 = Unfinished
          | otherwise = Neat
      { verdict, score: m.score }

-- Writing freehand -----------------------------------------------------------

-- | `rivals` are look-alikes she might write instead (`b` for `d`).
type FreehandTask a = { target :: Template, rivals :: Array (Tuple a Template) }

-- | Tolerance in normalized units, where the letter's larger dimension is 1.
-- | This and `passScore` were tuned on templates distorted the way a
-- | preschooler's letters are (stretched, slanted, wobbly): moderately
-- | wobbly letters pass, scribbles never do.
freehandTolerance :: Number
freehandTolerance = 0.12

passScore :: Number
passScore = 0.55

-- | Slants tried when matching, since young children rarely write upright.
slants :: Array Number
slants = [ -0.2, 0.0, 0.2 ]

slant :: Number -> Ink -> Ink
slant k = map (map (\p -> p { x = p.x + k * p.y }))

-- | A look-alike must fit clearly better than the target to be blamed.
rivalMargin :: Number
rivalMargin = 0.05

-- | Judges shape only: position and size on the screen don't matter (as
-- | long as it isn't tiny), nor does stroke order or direction.
judgeFreehand :: forall a. FreehandTask a -> Ink -> Judgement a
judgeFreehand task ink
  | extent ink < 0.25 = { verdict: TooSmall, score: 0.0 }
  | otherwise = do
      let
        drawn = map (\k -> normalize (slant k ink)) slants
        fit t = do
          let
            model = normalize t
          fromMaybe { coverage: 0.0, precision: 0.0, score: 0.0 }
            (maximumBy (compare `on` _.score) (map (match freehandTolerance model) drawn))
        target = fit task.target
        -- A symmetric letter's mirror image fits no better than the
        -- letter itself, so it can never be blamed.
        mirrored = Strokes.mirror task.target
        beats m = m.score >= passScore && m.score > target.score + rivalMargin
        rival = maximumBy (compare `on` (_.score <<< snd)) (map (\(Tuple a t) -> Tuple a (fit t)) task.rivals)
        verdict
          | beats (fit mirrored) = Backwards
          | Just (Tuple a m) <- rival, beats m = LooksLike a
          | target.score >= passScore = Neat
          | target.coverage < 0.6 = Unfinished
          | otherwise = Messy
      { verdict, score: target.score }
