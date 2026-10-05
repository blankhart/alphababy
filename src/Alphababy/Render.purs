module Alphababy.Render
  ( draw
  , drawBackground
  , drawParticles
  , fontSpecs
  , circle
  , sparklePath
  , starPath
  ) where

import Prelude

import Alphababy.Content (Face(..), Segment)
import Alphababy.Palette as Palette
import Alphababy.Round (Theme(..))
import Alphababy.World (Ball, Particle, ParticleShape(..), Twinkle, World)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Int as Int
import Data.Number as Number
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Graphics.Canvas (Composite(..), Context2D, LineJoin(..), TextAlign(..), TextBaseline(..))
import Graphics.Canvas as Canvas

-- | Ink extents relative to the drawing point: `left` and `ascent` extend
-- | left and up, `right` and `descent` extend right and down.
type InkBounds = { left :: Number, right :: Number, ascent :: Number, descent :: Number }

foreign import inkBoundsImpl :: Context2D -> String -> Effect InkBounds

textFace :: String
textFace = "Andika, \"Comic Sans MS\", system-ui, sans-serif"

emojiFace :: String
emojiFace = "\"Apple Color Emoji\", \"Segoe UI Emoji\", \"Noto Color Emoji\", sans-serif"

-- | Fonts to load before the first frame.
fontSpecs :: Array String
fontSpecs = [ "700 64px Andika" ]

draw :: Context2D -> Number -> World -> Effect Unit
draw ctx pixelRatio world = do
  Canvas.setTransform ctx { a: pixelRatio, b: 0.0, c: 0.0, d: pixelRatio, e: 0.0, f: 0.0 }
  drawBackground ctx world
  for_ world.balls (drawBall ctx world)
  drawParticles ctx world.particles

-- Background -----------------------------------------------------------------

drawBackground :: forall r. Context2D -> { size :: { width :: Number, height :: Number }, time :: Number, twinkles :: Array Twinkle | r } -> Effect Unit
drawBackground ctx world = do
  let
    w = world.size.width
    h = world.size.height
  sky <- Canvas.createLinearGradient ctx { x0: 0.0, y0: 0.0, x1: 0.0, y1: h }
  Canvas.addColorStop sky 0.0 "#2a0a45"
  Canvas.addColorStop sky 0.55 "#6b1f8f"
  Canvas.addColorStop sky 1.0 "#c2408f"
  Canvas.setGradientFillStyle ctx sky
  Canvas.fillRect ctx { x: 0.0, y: 0.0, width: w, height: h }
  for_ world.twinkles \t -> do
    let
      glow = 0.5 + 0.5 * Number.sin (world.time * t.rate + t.phase)
    Canvas.setGlobalAlpha ctx (0.25 + 0.75 * glow)
    Canvas.setFillStyle ctx "#fff6d8"
    sparklePath ctx (t.fx * w) (t.fy * h) (t.size * (1.5 + glow)) (t.phase)
    Canvas.fill ctx
  Canvas.setGlobalAlpha ctx 1.0
  drawMoon ctx (w * 0.86) (h * 0.12) (Number.min w h * 0.06)
  drawHills ctx w h
  drawCastle ctx w h

drawMoon :: Context2D -> Number -> Number -> Number -> Effect Unit
drawMoon ctx x y r = Canvas.withContext ctx do
  -- Clip to everything outside the "bite" so the crescent needs no
  -- knowledge of the sky color behind it.
  Canvas.beginPath ctx
  Canvas.rect ctx { x: x - 2.0 * r, y: y - 2.0 * r, width: 4.0 * r, height: 4.0 * r }
  Canvas.moveTo ctx (x + r * 1.3) (y - r * 0.2)
  Canvas.arc ctx { x: x + r * 0.45, y: y - r * 0.2, radius: r * 0.85, start: Number.tau, end: 0.0, useCounterClockwise: true }
  Canvas.clip ctx
  Canvas.setShadowColor ctx "rgba(255,240,200,0.8)"
  Canvas.setShadowBlur ctx 30.0
  Canvas.setFillStyle ctx "#fff4d6"
  circle ctx x y r
  Canvas.fill ctx

drawHills :: Context2D -> Number -> Number -> Effect Unit
drawHills ctx w h = do
  Canvas.setFillStyle ctx "rgba(74,20,110,0.55)"
  Canvas.beginPath ctx
  Canvas.moveTo ctx 0.0 h
  Canvas.lineTo ctx 0.0 (h * 0.86)
  Canvas.quadraticCurveTo ctx { cpx: w * 0.25, cpy: h * 0.76, x: w * 0.55, y: h * 0.88 }
  Canvas.quadraticCurveTo ctx { cpx: w * 0.8, cpy: h * 0.8, x: w, y: h * 0.84 }
  Canvas.lineTo ctx w h
  Canvas.closePath ctx
  Canvas.fill ctx

drawCastle :: Context2D -> Number -> Number -> Effect Unit
drawCastle ctx w h = do
  let
    cw = Number.min (w * 0.55) 380.0
    ch = cw * 0.62
    x0 = w * 0.5 - cw / 2.0
    ground = h * 0.9
    u = cw / 20.0
    tower x width top = do
      Canvas.fillRect ctx { x: x0 + x * u, y: ground - top * u, width: width * u, height: top * u }
      Canvas.beginPath ctx
      Canvas.moveTo ctx (x0 + (x - 0.4) * u) (ground - top * u)
      Canvas.lineTo ctx (x0 + (x + width / 2.0) * u) (ground - (top + width * 1.3) * u)
      Canvas.lineTo ctx (x0 + (x + width + 0.4) * u) (ground - top * u)
      Canvas.closePath ctx
      Canvas.fill ctx
  Canvas.setFillStyle ctx "rgba(40,8,64,0.5)"
  Canvas.fillRect ctx { x: x0 + 2.0 * u, y: ground - ch * 0.45, width: 16.0 * u, height: ch * 0.45 + h * 0.1 }
  tower 0.0 3.0 9.0
  tower 17.0 3.0 9.0
  tower 5.0 2.5 11.0
  tower 12.5 2.5 11.0
  tower 8.5 3.0 14.0
  Canvas.setFillStyle ctx "rgba(255,214,120,0.35)"
  Canvas.fillRect ctx { x: x0 + 9.6 * u, y: ground - 11.0 * u, width: 0.8 * u, height: 1.4 * u }
  Canvas.fillRect ctx { x: x0 + 5.9 * u, y: ground - 8.5 * u, width: 0.7 * u, height: 1.2 * u }
  Canvas.fillRect ctx { x: x0 + 13.4 * u, y: ground - 8.5 * u, width: 0.7 * u, height: 1.2 * u }

-- Balls ----------------------------------------------------------------------

drawBall :: Context2D -> World -> Ball -> Effect Unit
drawBall ctx world b = Canvas.withContext ctx do
  let
    r = b.radius
    base = Palette.swatch b.color
    tint = Number.min 1.0 (b.anger * 1.4)
    light = Palette.mix tint base.light Palette.angry.light
    mid = Palette.mix tint base.base Palette.angry.base
    dark = Palette.mix tint base.dark Palette.angry.dark
    shake = b.anger * 4.0 * Number.sin (world.time * 45.0 + b.phase)
    wobble = b.squash * 0.2 * Number.sin (b.age * 18.0)
    -- Letters rock gently but never turn far enough to be misread.
    rock = 0.08 * Number.sin (world.time * 1.3 + b.phase)
  Canvas.translate ctx { translateX: b.x + shake, translateY: b.y }
  Canvas.scale ctx { scaleX: 1.0 + wobble, scaleY: 1.0 - wobble }
  when (world.hint && b.target) do
    Canvas.setShadowColor ctx "#fff27a"
    Canvas.setShadowBlur ctx (26.0 + 16.0 * Number.sin (world.time * 6.0))
  body <- Canvas.createRadialGradient ctx { x0: -r * 0.35, y0: -r * 0.4, r0: r * 0.08, x1: 0.0, y1: 0.0, r1: r * 1.1 }
  Canvas.addColorStop body 0.0 (Palette.css light)
  Canvas.addColorStop body 0.55 (Palette.css mid)
  Canvas.addColorStop body 1.0 (Palette.css dark)
  Canvas.setGradientFillStyle ctx body
  Canvas.setStrokeStyle ctx "rgba(255,255,255,0.55)"
  Canvas.setLineWidth ctx 2.5
  Canvas.setLineJoin ctx RoundJoin
  labelScale <- case world.theme of
    Bubbles -> do
      circle ctx 0.0 0.0 r
      Canvas.fill ctx
      Canvas.setShadowBlur ctx 0.0
      Canvas.stroke ctx
      gloss ctx r
      rimSparkles ctx r b.spin
      pure 1.0
    Balloons -> do
      balloonString ctx r world.time b.phase
      Canvas.setGradientFillStyle ctx body
      Canvas.beginPath ctx
      Canvas.moveTo ctx 0.0 (r * 1.02)
      Canvas.lineTo ctx (-r * 0.12) (r * 1.2)
      Canvas.lineTo ctx (r * 0.12) (r * 1.2)
      Canvas.closePath ctx
      Canvas.fill ctx
      ellipse ctx 0.0 (-r * 0.05) r (r * 1.1)
      Canvas.fill ctx
      Canvas.setShadowBlur ctx 0.0
      Canvas.stroke ctx
      gloss ctx r
      pure 1.0
    Stars -> do
      starPath ctx 0.0 0.0 5 (r * 1.25) (r * 0.78) (b.spin * 0.4 - Number.pi / 2.0)
      Canvas.fill ctx
      Canvas.setShadowBlur ctx 0.0
      Canvas.stroke ctx
      gloss ctx (r * 0.8)
      pure 0.82
    Hearts -> do
      heartPath ctx 0.0 0.0 (r * 1.08)
      Canvas.fill ctx
      Canvas.setShadowBlur ctx 0.0
      Canvas.stroke ctx
      gloss ctx (r * 0.9)
      pure 0.84
  if b.anger > 0.05 then angryFace ctx r b.anger else cheeks ctx r
  Canvas.rotate ctx rock
  drawFace ctx (r * labelScale) (Palette.css dark) (if world.theme == Hearts then r * 0.08 else 0.0) b.label.face

gloss :: Context2D -> Number -> Effect Unit
gloss ctx r = Canvas.withContext ctx do
  Canvas.setFillStyle ctx "rgba(255,255,255,0.5)"
  Canvas.translate ctx { translateX: -r * 0.5, translateY: -r * 0.52 }
  Canvas.rotate ctx (-0.7)
  ellipse ctx 0.0 0.0 (r * 0.24) (r * 0.12)
  Canvas.fill ctx

rimSparkles :: Context2D -> Number -> Number -> Effect Unit
rimSparkles ctx r spin = do
  Canvas.setFillStyle ctx "rgba(255,255,255,0.85)"
  for_ [ 0.0, 2.1, 4.2 ] \offset -> do
    let
      a = spin + offset
    sparklePath ctx (Number.cos a * r * 0.84) (Number.sin a * r * 0.84) (r * 0.09) (spin * 2.0)
    Canvas.fill ctx

balloonString :: Context2D -> Number -> Number -> Number -> Effect Unit
balloonString ctx r time phase = do
  let
    sway = 10.0 * Number.sin (time * 2.0 + phase)
  Canvas.setStrokeStyle ctx "rgba(255,255,255,0.6)"
  Canvas.setLineWidth ctx 2.0
  Canvas.beginPath ctx
  Canvas.moveTo ctx 0.0 (r * 1.2)
  Canvas.bezierCurveTo ctx { cp1x: sway, cp1y: r * 1.6, cp2x: -sway, cp2y: r * 1.9, x: sway * 0.5, y: r * 2.3 }
  Canvas.stroke ctx
  Canvas.setStrokeStyle ctx "rgba(255,255,255,0.55)"
  Canvas.setLineWidth ctx 2.5

cheeks :: Context2D -> Number -> Effect Unit
cheeks ctx r = do
  Canvas.setFillStyle ctx "rgba(255,120,190,0.35)"
  ellipse ctx (-r * 0.62) (r * 0.4) (r * 0.13) (r * 0.08)
  Canvas.fill ctx
  ellipse ctx (r * 0.62) (r * 0.4) (r * 0.13) (r * 0.08)
  Canvas.fill ctx

angryFace :: Context2D -> Number -> Number -> Effect Unit
angryFace ctx r anger = Canvas.withContext ctx do
  Canvas.setGlobalAlpha ctx (Number.min 1.0 (anger * 3.0))
  Canvas.setFillStyle ctx "#2a0010"
  Canvas.setStrokeStyle ctx "#2a0010"
  Canvas.setLineWidth ctx (r * 0.07)
  Canvas.setLineCap ctx Canvas.Round
  for_ [ -1.0, 1.0 ] \side -> do
    circle ctx (side * r * 0.62) (-r * 0.3) (r * 0.075)
    Canvas.fill ctx
    Canvas.beginPath ctx
    Canvas.moveTo ctx (side * r * 0.8) (-r * 0.5)
    Canvas.lineTo ctx (side * r * 0.45) (-r * 0.4)
    Canvas.stroke ctx
  Canvas.beginPath ctx
  Canvas.arc ctx { x: 0.0, y: r * 0.95, radius: r * 0.28, start: Number.pi * 1.3, end: Number.pi * 1.7, useCounterClockwise: false }
  Canvas.stroke ctx

-- Labels ---------------------------------------------------------------------

drawFace :: Context2D -> Number -> String -> Number -> Face -> Effect Unit
drawFace ctx r outline dy = case _ of
  Glyphs segments -> drawSegments ctx r outline dy segments
  Picture emoji -> do
    let
      size = r * 1.1
    Canvas.setFont ctx (px size <> " " <> emojiFace)
    Canvas.setTextAlign ctx AlignCenter
    Canvas.setTextBaseline ctx BaselineMiddle
    Canvas.setFillStyle ctx "#000"
    Canvas.fillText ctx emoji 0.0 (dy + size * 0.06)
  Pips n -> drawPips ctx r n

drawSegments :: Context2D -> Number -> String -> Number -> Array Segment -> Effect Unit
drawSegments ctx r outline dy segments = do
  let
    text = Array.foldMap _.text segments
    nominal = r * case String.length text of
      1 -> 1.3
      2 -> 1.05
      3 -> 0.82
      _ -> 0.7
  Canvas.setFont ctx (fontAt nominal)
  total <- map _.width (Canvas.measureText ctx text)
  let
    size = if total > r * 1.6 then nominal * r * 1.6 / total else nominal
  Canvas.setFont ctx (fontAt size)
  widths <- traverse (map _.width <<< Canvas.measureText ctx <<< _.text) segments
  Canvas.setTextAlign ctx AlignLeft
  Canvas.setTextBaseline ctx BaselineMiddle
  Canvas.setLineJoin ctx RoundJoin
  Canvas.setLineWidth ctx (size * 0.16)
  Canvas.setStrokeStyle ctx outline
  -- Centre the ink rather than the em box, which sits high for lowercase
  -- and off to one side for glyphs with uneven side bearings.
  ink <- inkBoundsImpl ctx text
  let
    start = (ink.left - ink.right) / 2.0
    y = dy + (ink.ascent - ink.descent) / 2.0
  for_ (Array.zipWith (\s x -> { s, x }) segments (Array.cons start (Array.scanl (+) start widths))) \{ s, x } -> do
    Canvas.strokeText ctx s.text x y
    Canvas.setFillStyle ctx (if s.emphasis then "#ffe14d" else "#ffffff")
    Canvas.fillText ctx s.text x y
  where
  fontAt size = "700 " <> px size <> " " <> textFace

drawPips :: Context2D -> Number -> Int -> Effect Unit
drawPips ctx r n = do
  let
    s = r * 0.42
    spots = case n of
      1 -> [ { x: 0.0, y: 0.0 } ]
      2 -> [ { x: -1.0, y: -1.0 }, { x: 1.0, y: 1.0 } ]
      3 -> [ { x: -1.0, y: -1.0 }, { x: 0.0, y: 0.0 }, { x: 1.0, y: 1.0 } ]
      4 -> [ { x: -1.0, y: -1.0 }, { x: 1.0, y: -1.0 }, { x: -1.0, y: 1.0 }, { x: 1.0, y: 1.0 } ]
      5 -> [ { x: -1.0, y: -1.0 }, { x: 1.0, y: -1.0 }, { x: 0.0, y: 0.0 }, { x: -1.0, y: 1.0 }, { x: 1.0, y: 1.0 } ]
      _ -> [ { x: -1.0, y: -1.1 }, { x: 1.0, y: -1.1 }, { x: -1.0, y: 0.0 }, { x: 1.0, y: 0.0 }, { x: -1.0, y: 1.1 }, { x: 1.0, y: 1.1 } ]
  Canvas.setFillStyle ctx "#ffe14d"
  Canvas.setStrokeStyle ctx "#7a3b00"
  Canvas.setLineWidth ctx 2.0
  for_ spots \p -> do
    starPath ctx (p.x * s) (p.y * s) 5 (r * 0.2) (r * 0.09) (-Number.pi / 2.0)
    Canvas.fill ctx
    Canvas.stroke ctx

-- Particles ------------------------------------------------------------------

drawParticles :: Context2D -> Array Particle -> Effect Unit
drawParticles ctx particles = Canvas.withContext ctx do
  Canvas.setGlobalCompositeOperation ctx Lighter
  for_ particles \p -> do
    let
      t = p.age / p.life
      fade = 1.0 - t * t
    Canvas.setGlobalAlpha ctx fade
    Canvas.setFillStyle ctx p.color
    case p.shape of
      Sparkle -> do
        sparklePath ctx p.x p.y (p.size * (0.6 + 0.4 * Number.sin (p.age * 20.0 + p.spin))) p.spin
        Canvas.fill ctx
      Dot -> do
        circle ctx p.x p.y (p.size * (1.0 - 0.5 * t))
        Canvas.fill ctx
      Heart -> do
        heartPath ctx p.x p.y p.size
        Canvas.fill ctx
      Ring -> do
        Canvas.setStrokeStyle ctx p.color
        Canvas.setLineWidth ctx (8.0 * (1.0 - t))
        circle ctx p.x p.y (p.size * (1.0 + 1.3 * t))
        Canvas.stroke ctx

-- Paths ----------------------------------------------------------------------

circle :: Context2D -> Number -> Number -> Number -> Effect Unit
circle ctx x y r = do
  Canvas.beginPath ctx
  Canvas.arc ctx { x, y, radius: Number.max 0.0 r, start: 0.0, end: Number.tau, useCounterClockwise: false }

ellipse :: Context2D -> Number -> Number -> Number -> Number -> Effect Unit
ellipse ctx x y rx ry = do
  Canvas.beginPath ctx
  for_ (Array.range 0 32) \i -> do
    let
      a = Int.toNumber i * Number.tau / 32.0
      px' = x + rx * Number.cos a
      py' = y + ry * Number.sin a
    if i == 0 then Canvas.moveTo ctx px' py' else Canvas.lineTo ctx px' py'
  Canvas.closePath ctx

-- | A four-pointed glint.
sparklePath :: Context2D -> Number -> Number -> Number -> Number -> Effect Unit
sparklePath ctx x y size rotation = starPath ctx x y 4 size (size * 0.3) rotation

starPath :: Context2D -> Number -> Number -> Int -> Number -> Number -> Number -> Effect Unit
starPath ctx x y points outer inner rotation = do
  Canvas.beginPath ctx
  for_ (Array.range 0 (2 * points - 1)) \i -> do
    let
      a = rotation + Int.toNumber i * Number.pi / Int.toNumber points
      rad = if i `mod` 2 == 0 then outer else inner
      px' = x + Number.cos a * rad
      py' = y + Number.sin a * rad
    if i == 0 then Canvas.moveTo ctx px' py' else Canvas.lineTo ctx px' py'
  Canvas.closePath ctx

heartPath :: Context2D -> Number -> Number -> Number -> Effect Unit
heartPath ctx x y r = do
  Canvas.beginPath ctx
  Canvas.moveTo ctx x (y + r * 0.95)
  Canvas.bezierCurveTo ctx { cp1x: x - r * 1.45, cp1y: y + r * 0.15, cp2x: x - r * 1.05, cp2y: y - r * 1.15, x: x, y: y - r * 0.45 }
  Canvas.bezierCurveTo ctx { cp1x: x + r * 1.05, cp1y: y - r * 1.15, cp2x: x + r * 1.45, cp2y: y + r * 0.15, x: x, y: y + r * 0.95 }
  Canvas.closePath ctx

px :: Number -> String
px n = show (Number.round n) <> "px"
