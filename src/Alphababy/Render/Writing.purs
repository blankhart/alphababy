module Alphababy.Render.Writing
  ( Layout
  , layoutFor
  , toScreen
  , fromScreen
  , View
  , draw
  ) where

import Prelude

import Alphababy.Handwriting (Ink, TraceProgress)
import Alphababy.Render as Render
import Alphababy.Strokes (Point, Stroke, Template, Trail)
import Alphababy.Strokes as Strokes
import Alphababy.World (Size, World)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Data.Number as Number
import Effect (Effect)
import Graphics.Canvas (Context2D, LineCap(..), LineJoin(..))
import Graphics.Canvas as Canvas

-- | Where template coordinates land on screen: `unit` CSS pixels per
-- | capital height, with the top line at `y`.
type Layout = { x :: Number, y :: Number, unit :: Number, size :: Size }

-- | A big letter, centred, with room below the baseline for descenders.
layoutFor :: Size -> Template -> Layout
layoutFor size t = do
  let
    b = Strokes.bounds (Array.concat t)
    width = Number.max 0.5 (b.right - b.left)
    unit = Number.min 520.0 (Number.min (size.height * 0.5) (size.width * 0.8 / width))
  { x: size.width / 2.0 - (b.left + b.right) / 2.0 * unit
  , y: size.height * 0.48 - 0.5 * unit
  , unit
  , size
  }

toScreen :: Layout -> Point -> Point
toScreen l p = { x: l.x + p.x * l.unit, y: l.y + p.y * l.unit }

fromScreen :: Layout -> Point -> Point
fromScreen l p = { x: (p.x - l.x) / l.unit, y: (p.y - l.y) / l.unit }

-- | Everything on the writing screen. `path` is the tracing guide with the
-- | parts she has lit and the stroke to do next; `demo` is a demonstration
-- | in progress; `ghost` shows how an unsuccessful attempt should have
-- | looked; `model` is a small copy to look at while writing freehand.
type View =
  { world :: World
  , layout :: Layout
  , template :: Template
  , path :: Maybe { lit :: TraceProgress, next :: Maybe Int }
  , ink :: Ink
  , demo :: Maybe Trail
  , ghost :: Maybe Template
  , model :: Boolean
  }

draw :: Context2D -> Number -> View -> Effect Unit
draw ctx pixelRatio view = do
  Canvas.setTransform ctx { a: pixelRatio, b: 0.0, c: 0.0, d: pixelRatio, e: 0.0, f: 0.0 }
  Render.drawBackground ctx view.world
  paper ctx view.layout
  Canvas.setLineCap ctx Round
  Canvas.setLineJoin ctx RoundJoin
  for_ view.path \p -> guide ctx view.layout view.world.time view.template p.lit p.next
  ink ctx view.layout view.ink
  for_ view.ghost \g -> ghost ctx view.layout g
  for_ view.demo \d -> demo ctx view.layout view.world.time d
  when view.model (model ctx view.layout view.template)
  Render.drawParticles ctx view.world.particles

-- | Three-line handwriting paper: top line, dashed middle line, baseline.
paper :: Context2D -> Layout -> Effect Unit
paper ctx l = Canvas.withContext ctx do
  let
    left = l.size.width * 0.06
    right = l.size.width * 0.94
    row y = do
      Canvas.beginPath ctx
      Canvas.moveTo ctx left (l.y + y * l.unit)
      Canvas.lineTo ctx right (l.y + y * l.unit)
      Canvas.stroke ctx
  Canvas.setFillStyle ctx "rgba(255,240,250,0.16)"
  Canvas.fillRect ctx { x: left, y: l.y - 0.2 * l.unit, width: right - left, height: 1.7 * l.unit }
  Canvas.setLineWidth ctx 3.0
  Canvas.setStrokeStyle ctx "rgba(255,209,240,0.55)"
  row 0.0
  row 1.0
  Canvas.setLineDash ctx [ 14.0, 12.0 ]
  Canvas.setStrokeStyle ctx "rgba(255,209,240,0.4)"
  row 0.5

polyline :: Context2D -> Layout -> Stroke -> Effect Unit
polyline ctx l s = do
  Canvas.beginPath ctx
  for_ (Array.mapWithIndex (\i p -> { i, p: toScreen l p }) s) \{ i, p } ->
    if i == 0 then Canvas.moveTo ctx p.x p.y else Canvas.lineTo ctx p.x p.y
  -- A lone point (the dot on an i) still needs something to stroke.
  when (Array.length s == 1) $ for_ (Array.head s) \p0 -> do
    let
      p = toScreen l p0
    Canvas.lineTo ctx (p.x + 0.01) p.y
  Canvas.stroke ctx

-- | The sparkly road to trace: faint where she hasn't been, gold where she
-- | has, with a pulsing star where the next stroke starts.
guide :: Context2D -> Layout -> Number -> Template -> TraceProgress -> Maybe Int -> Effect Unit
guide ctx l time t lit next = Canvas.withContext ctx do
  Canvas.setLineWidth ctx (l.unit * 0.2)
  Canvas.setStrokeStyle ctx "rgba(255,255,255,0.16)"
  for_ t (polyline ctx l)
  Canvas.setLineWidth ctx (Number.max 3.0 (l.unit * 0.012))
  Canvas.setLineDash ctx [ 2.0, l.unit * 0.05 ]
  Canvas.setStrokeStyle ctx "rgba(255,255,255,0.75)"
  for_ t (polyline ctx l)
  Canvas.setLineDash ctx []
  Canvas.setShadowColor ctx "#ffe14d"
  Canvas.setShadowBlur ctx 18.0
  Canvas.setStrokeStyle ctx "#ffe14d"
  Canvas.setLineWidth ctx (l.unit * 0.11)
  for_ (Array.concat (Array.zipWith litRuns t lit)) (polyline ctx l)
  Canvas.setShadowBlur ctx 0.0
  for_ (next >>= Array.index t) \s -> for_ (Array.head s) \start -> do
    let
      p = toScreen l start
      pulse = 1.0 + 0.18 * Number.sin (time * 6.0)
    -- An arrow a little way along shows which way to go.
    for_ (Array.index s (min (Array.length s - 1) 8)) \ahead -> do
      let
        q = toScreen l ahead
        angle = Number.atan2 (q.y - p.y) (q.x - p.x)
        tip = { x: p.x + Number.cos angle * l.unit * 0.2, y: p.y + Number.sin angle * l.unit * 0.2 }
        wing a = { x: tip.x - Number.cos (angle + a) * l.unit * 0.07, y: tip.y - Number.sin (angle + a) * l.unit * 0.07 }
      when (Array.length s > 1) do
        Canvas.setStrokeStyle ctx "#ffffff"
        Canvas.setLineWidth ctx (Number.max 4.0 (l.unit * 0.02))
        Canvas.beginPath ctx
        Canvas.moveTo ctx (wing 0.6).x (wing 0.6).y
        Canvas.lineTo ctx tip.x tip.y
        Canvas.lineTo ctx (wing (-0.6)).x (wing (-0.6)).y
        Canvas.stroke ctx
    Canvas.setShadowColor ctx "#ffffff"
    Canvas.setShadowBlur ctx 20.0
    Canvas.setFillStyle ctx "#fff27a"
    Render.starPath ctx p.x p.y 5 (l.unit * 0.075 * pulse) (l.unit * 0.035 * pulse) (time - Number.pi / 2.0)
    Canvas.fill ctx

-- | The runs of consecutive lit points in a stroke.
litRuns :: Stroke -> Array Boolean -> Array Stroke
litRuns s flags = Array.filter (not <<< Array.null) (finish (Array.foldl step { runs: [], current: [] } (Array.zipWith { p: _, on: _ } s flags)))
  where
  step acc { p, on }
    | on = acc { current = Array.snoc acc.current p }
    | otherwise = { runs: Array.snoc acc.runs acc.current, current: [] }
  finish acc = Array.snoc acc.runs acc.current

ink :: Context2D -> Layout -> Ink -> Effect Unit
ink ctx l strokes = Canvas.withContext ctx do
  let
    width = Number.max 8.0 (l.unit * 0.05)
  Canvas.setShadowColor ctx "#ff7ad9"
  Canvas.setShadowBlur ctx 14.0
  Canvas.setStrokeStyle ctx "#ff7ad9"
  Canvas.setLineWidth ctx (width + 6.0)
  for_ strokes (polyline ctx l)
  Canvas.setShadowBlur ctx 0.0
  Canvas.setStrokeStyle ctx "#fff4fb"
  Canvas.setLineWidth ctx width
  for_ strokes (polyline ctx l)

ghost :: Context2D -> Layout -> Template -> Effect Unit
ghost ctx l t = Canvas.withContext ctx do
  Canvas.setShadowColor ctx "#ffe14d"
  Canvas.setShadowBlur ctx 16.0
  Canvas.setStrokeStyle ctx "rgba(255,225,77,0.85)"
  Canvas.setLineWidth ctx (Number.max 6.0 (l.unit * 0.045))
  Canvas.setLineDash ctx [ l.unit * 0.06, l.unit * 0.05 ]
  for_ t (polyline ctx l)

-- | A magic wand's star drawing the letter.
demo :: Context2D -> Layout -> Number -> Trail -> Effect Unit
demo ctx l time d = Canvas.withContext ctx do
  Canvas.setShadowColor ctx "#ffe14d"
  Canvas.setShadowBlur ctx 20.0
  Canvas.setStrokeStyle ctx "#ffe14d"
  Canvas.setLineWidth ctx (Number.max 8.0 (l.unit * 0.06))
  for_ d.drawn (polyline ctx l)
  for_ d.pen \pen -> do
    let
      p = toScreen l pen
    Canvas.setShadowColor ctx "#ffffff"
    Canvas.setFillStyle ctx "#fffbe0"
    Render.starPath ctx p.x p.y 5 (l.unit * 0.1) (l.unit * 0.045) (time * 3.0)
    Canvas.fill ctx

-- | A little card at the top of the screen showing the letter to copy.
model :: Context2D -> Layout -> Template -> Effect Unit
model ctx l t = Canvas.withContext ctx do
  let
    b = Strokes.bounds (Array.concat t)
    boxH = Number.min 110.0 (l.size.height * 0.13)
    unit = boxH * 0.8 / Number.max 1.0 (b.bottom - Number.min 0.0 b.top)
    w = Number.max boxH (unit * (b.right - b.left) + boxH * 0.4)
    top = 12.0
    left = l.size.width / 2.0 - w / 2.0
    card = { x: left + w / 2.0 - (b.left + b.right) / 2.0 * unit, y: top + boxH * 0.1, unit, size: l.size }
  Canvas.setShadowColor ctx "rgba(255,225,77,0.8)"
  Canvas.setShadowBlur ctx 18.0
  Canvas.setFillStyle ctx "rgba(255,247,252,0.94)"
  Canvas.fillRect ctx { x: left, y: top, width: w, height: boxH }
  Canvas.setShadowBlur ctx 0.0
  Canvas.setStrokeStyle ctx "#6a1fb0"
  Canvas.setLineWidth ctx (Number.max 4.0 (unit * 0.1))
  for_ t (polyline ctx card)
