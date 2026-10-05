module Alphababy.World
  ( World
  , Ball
  , Particle
  , ParticleShape(..)
  , Twinkle
  , TapOutcome(..)
  , Size
  , create
  , step
  , tap
  , resize
  , remainingTargets
  , setHint
  , popAll
  , sparkleBurst
  , glitter
  ) where

import Prelude

import Alphababy.Content (Label)
import Alphababy.Palette as Palette
import Alphababy.Random (Gen, Seed)
import Alphababy.Random as Random
import Alphababy.Round (PlannedBall, Theme(..))
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl, minimumBy)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

type Size = { width :: Number, height :: Number }

type World =
  { size :: Size
  , theme :: Theme
  , balls :: Array Ball
  , particles :: Array Particle
  , twinkles :: Array Twinkle
  , time :: Number
  , hint :: Boolean
  , pace :: Number
  , seed :: Seed
  }

-- | Positions and velocities are in CSS pixels and pixels per second.
-- | `anger` (0 to 1) rises on a wrong tap and fades; it drives the angry
-- | color, faster movement and the grumpy face. `squash` is the amplitude
-- | of the jelly wobble after being poked. `speed` is the cruising speed;
-- | balloons and stars cruise up or down according to `rising` and turn
-- | around at the edges so that none is ever out of sight. `lift` is a
-- | heart's launch speed off the floor.
type Ball =
  { id :: Int
  , label :: Label
  , target :: Boolean
  , x :: Number
  , y :: Number
  , vx :: Number
  , vy :: Number
  , radius :: Number
  , speed :: Number
  , rising :: Boolean
  , lift :: Number
  , color :: Int
  , spin :: Number
  , spinRate :: Number
  , anger :: Number
  , squash :: Number
  , phase :: Number
  , age :: Number
  }

data ParticleShape
  = Sparkle
  | Dot
  | Ring
  | Heart

type Particle =
  { x :: Number
  , y :: Number
  , vx :: Number
  , vy :: Number
  , age :: Number
  , life :: Number
  , size :: Number
  , color :: String
  , shape :: ParticleShape
  , spin :: Number
  , spinRate :: Number
  , gravity :: Number
  }

-- | Background stars, positioned as fractions of the screen so that a
-- | resize needs no update.
type Twinkle = { fx :: Number, fy :: Number, size :: Number, phase :: Number, rate :: Number }

data TapOutcome
  = Missed
  | Found Ball
  | Mistake Ball

-- | Scaling gravity with the square of the pace keeps hop heights the same
-- | while the hops get quicker or slower.
gravityFor :: Number -> Size -> Number
gravityFor pace size = 1.1 * size.height * pace * pace

-- | `pace` scales every speed (1.0 is the default).
create :: Number -> Size -> Theme -> Array PlannedBall -> Seed -> World
create pace size theme planned seed = do
  let
    generated = Random.runGen generate seed
  { size
  , theme
  , balls: generated.value.balls
  , particles: []
  , twinkles: generated.value.twinkles
  , time: 0.0
  , hint: false
  , pace
  , seed: generated.seed
  }
  where
  n = Array.length planned
  short = Number.min size.width size.height
  scale = Number.max 0.7 (Number.min 1.6 (short / 500.0))
  radius = Number.max 44.0 (Number.min (short * 0.16) (Number.sqrt (size.width * size.height * 0.28 / (Number.pi * Int.toNumber (max n 1)))))

  generate = do
    balls <- Array.foldM place [] (Array.mapWithIndex Tuple planned)
    twinkles <- traverse (const twinkle) (Array.range 1 40)
    pure { balls, twinkles }

  twinkle = do
    fx <- Random.float
    fy <- Random.range 0.0 0.8
    sz <- Random.range 1.0 3.0
    phase <- Random.range 0.0 Number.tau
    rate <- Random.range 0.8 2.5
    pure { fx, fy, size: sz, phase, rate }

  place placed (Tuple i p) = do
    r <- map (_ * radius) (Random.range 0.92 1.08)
    pos <- findSpot r placed 40
    angle <- Random.range 0.0 Number.tau
    speed <- map (\s -> s * scale * pace) themeSpeed
    hop <- Random.range 0.44 0.61
    color <- Random.int 0 (NonEmptyArray.length Palette.swatches - 1)
    spin <- Random.range 0.0 Number.tau
    spinRate <- Random.range (-0.8) 0.8
    phase <- Random.range 0.0 Number.tau
    let
      -- Hearts hop to between a half and two thirds of the screen height.
      lift = if theme == Hearts then Number.sqrt (2.0 * gravityFor pace size * hop * size.height) else 0.0
      velocity = case theme of
        Hearts -> { vx: if Number.cos angle < 0.0 then -speed else speed, vy: 0.0 }
        Balloons -> { vx: 0.0, vy: -speed }
        Stars -> { vx: 0.0, vy: speed }
        Bubbles -> { vx: Number.cos angle * speed, vy: Number.sin angle * speed }
    pure $ Array.snoc placed
      { id: i
      , label: p.label
      , target: p.target
      , x: pos.x
      , y: pos.y
      , vx: velocity.vx
      , vy: velocity.vy
      , radius: r
      , speed
      , rising: theme == Balloons
      , lift
      , color
      , spin
      , spinRate
      , anger: 0.0
      , squash: 0.0
      , phase
      , age: 0.0
      }

  themeSpeed = case theme of
    Bubbles -> Random.range 54.0 71.0
    Balloons -> Random.range 36.0 49.0
    Stars -> Random.range 30.0 40.0
    Hearts -> Random.range 36.0 49.0

  findSpot :: Number -> Array Ball -> Int -> Gen { x :: Number, y :: Number }
  findSpot r placed attempts = do
    x <- Random.range r (Number.max r (size.width - r))
    y <- Random.range (r + 30.0) (Number.max (r + 30.0) (size.height - r))
    let
      clear = Array.all (\b -> dist x y b.x b.y > b.radius + r + 8.0) placed
    if clear || attempts <= 0 then pure { x, y } else findSpot r placed (attempts - 1)

dist :: Number -> Number -> Number -> Number -> Number
dist x1 y1 x2 y2 = Number.sqrt ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))

resize :: Size -> World -> World
resize size world = world { size = size, balls = map (clampInside size) world.balls }
  where
  clampInside s b = b
    { x = Number.max b.radius (Number.min (s.width - b.radius) b.x)
    , y = Number.max b.radius (Number.min (s.height - b.radius) b.y)
    }

remainingTargets :: World -> Int
remainingTargets = Array.length <<< Array.filter _.target <<< _.balls

setHint :: Boolean -> World -> World
setHint hint = _ { hint = hint }

-- | Advances the simulation by `dt` seconds.
step :: Number -> World -> World
step rawDt world = do
  let
    -- A long frame (e.g. after the tab was hidden) must not tunnel balls
    -- through walls or each other.
    dt = Number.min 0.05 (Number.max 0.0 rawDt)
    time = world.time + dt
    moved = map (move world.theme world.pace world.size time dt) world.balls
  world
    { time = time
    , balls = collide moved
    , particles = Array.mapMaybe (stepParticle dt) world.particles
    }

approach :: Number -> Number -> Number -> Number -> Number
approach rate dt current target = current + (target - current) * (1.0 - Number.exp (-rate * dt))

move :: Theme -> Number -> Size -> Number -> Number -> Ball -> Ball
move theme pace size time dt b0 = do
  let
    b = b0
      { anger = Number.max 0.0 (b0.anger - dt / 4.0)
      , squash = b0.squash * Number.exp (-2.5 * dt)
      , spin = b0.spin + b0.spinRate * dt * (1.0 + 3.0 * b0.anger)
      , age = b0.age + dt
      }
    urgency = 1.0 + 1.3 * b.anger
    sway = Number.sin (time * 0.9 + b.phase)
  case theme of
    Bubbles -> do
      let
        current = Number.sqrt (b.vx * b.vx + b.vy * b.vy)
        wanted = b.speed * urgency
        newSpeed = approach 1.5 dt current wanted
        turn = sway * 0.35 * dt
        heading = (if current < 1.0 then b.phase else Number.atan2 b.vy b.vx) + turn
        vx = Number.cos heading * newSpeed
        vy = Number.sin heading * newSpeed
      bounceWalls size (b { vx = vx, vy = vy, x = b.x + vx * dt, y = b.y + vy * dt })
    -- Balloons rise and sink back more slowly; stars fall and float back up.
    Balloons -> drift (if b.rising then 1.0 else 0.7) 0.5 b
    Stars -> drift (if b.rising then 0.7 else 1.0) 0.6 b
    Hearts -> do
      let
        g = gravityFor pace size
        vy = b.vy + g * dt
        vx = approach 0.8 dt b.vx (signum b.vx * b.speed * urgency)
        y = b.y + vy * dt
        launch = Number.min (Number.sqrt (2.0 * g * (size.height - 2.0 * b.radius))) (b.lift * (1.0 + 0.25 * b.anger))
        landed = b { vx = vx, x = b.x + vx * dt, y = size.height - b.radius, vy = -launch, squash = Number.max b.squash 0.35 }
        flying = b { vx = vx, x = b.x + vx * dt, y = y, vy = vy }
      bounceSides size
        if y + b.radius >= size.height && vy > 0.0 then landed
        else if y - b.radius < 0.0 && vy < 0.0 then flying { y = b.radius, vy = -vy }
        else flying
  where
  signum v = if v < 0.0 then -1.0 else 1.0

  -- A soft bump at the top or bottom edge turns the ball around.
  drift vertical sideways b = do
    let
      urgency = 1.0 + 1.3 * b.anger
      sway = Number.sin (time * 0.9 + b.phase)
      vy = approach 1.2 dt b.vy ((if b.rising then -1.0 else 1.0) * b.speed * vertical * urgency)
      vx = approach 1.0 dt b.vx (sway * b.speed * sideways * urgency)
      y = b.y + vy * dt
      moved = b { vx = vx, vy = vy, x = b.x + vx * dt, y = y }
    bounceSides size
      if y - b.radius < 0.0 && vy < 0.0 then moved { y = b.radius, vy = -0.3 * vy, rising = false }
      else if y + b.radius > size.height && vy > 0.0 then moved { y = size.height - b.radius, vy = -0.3 * vy, rising = true }
      else moved

bounceSides :: Size -> Ball -> Ball
bounceSides size b
  | b.x - b.radius < 0.0 = b { x = b.radius, vx = Number.abs b.vx }
  | b.x + b.radius > size.width = b { x = size.width - b.radius, vx = -(Number.abs b.vx) }
  | otherwise = b

bounceWalls :: Size -> Ball -> Ball
bounceWalls size ball = do
  let
    b = bounceSides size ball
  if b.y - b.radius < 0.0 then b { y = b.radius, vy = Number.abs b.vy }
  else if b.y + b.radius > size.height then b { y = size.height - b.radius, vy = -(Number.abs b.vy) }
  else b

-- | Equal-mass elastic collisions. Each ball's correction is computed from
-- | the positions at the start of the step, so the result does not depend
-- | on array order.
collide :: Array Ball -> Array Ball
collide balls = map resolve balls
  where
  resolve b = foldl (push b) b balls

  push self acc other
    | other.id == self.id = acc
    | otherwise = do
        let
          dx = self.x - other.x
          dy = self.y - other.y
          d = Number.sqrt (dx * dx + dy * dy)
          overlap = self.radius + other.radius - d
        if overlap <= 0.0 || d < 0.001 then acc
        else do
          let
            nx = dx / d
            ny = dy / d
            approachSpeed = (self.vx - other.vx) * nx + (self.vy - other.vy) * ny
            dv = if approachSpeed < 0.0 then approachSpeed else 0.0
          acc
            { x = acc.x + nx * overlap * 0.5
            , y = acc.y + ny * overlap * 0.5
            , vx = acc.vx - dv * nx
            , vy = acc.vy - dv * ny
            }

stepParticle :: Number -> Particle -> Maybe Particle
stepParticle dt p
  | p.age + dt >= p.life = Nothing
  | otherwise = do
      let
        drag = Number.exp (-1.4 * dt)
        vx = p.vx * drag
        vy = p.vy * drag + p.gravity * dt
      Just p
        { x = p.x + vx * dt
        , y = p.y + vy * dt
        , vx = vx
        , vy = vy
        , age = p.age + dt
        , spin = p.spin + p.spinRate * dt
        }

-- | Resolves a tap at `(x, y)` against the nearest ball under the finger,
-- | with a generous margin for small fingers.
tap :: Number -> Number -> World -> { world :: World, outcome :: TapOutcome }
tap x y world = do
  let
    hits = Array.filter (\b -> dist x y b.x b.y <= b.radius * 1.2 + 6.0) world.balls
    nearest = minimumBy (comparing (\b -> dist x y b.x b.y / b.radius)) hits
  case nearest of
    Nothing -> { world, outcome: Missed }
    Just ball
      | ball.target -> do
          let
            burst = Random.runGen (burstParticles 1.0 ball) world.seed
          { world: world
              { balls = Array.filter ((_ /= ball.id) <<< _.id) world.balls
              , particles = world.particles <> burst.value
              , seed = burst.seed
              }
          , outcome: Found ball
          }
      | otherwise -> do
          let
            puff = Random.runGen (grumpyPuff ball) world.seed
            upset = ball
              { anger = 1.0
              , squash = 1.0
              , vx = ball.vx * 1.6 + (if ball.vx < 0.0 then -40.0 else 40.0)
              , vy = ball.vy * 1.6 + (if world.theme == Hearts then 0.0 else if ball.vy < 0.0 then -40.0 else 40.0)
              }
          { world: world
              { balls = map (\b -> if b.id == ball.id then upset else b) world.balls
              , particles = world.particles <> puff.value
              , seed = puff.seed
              }
          , outcome: Mistake upset
          }

-- | Pops every remaining ball, for the end-of-round celebration.
popAll :: World -> World
popAll world = do
  let
    burst = Random.runGen (map Array.concat (traverse (burstParticles 0.6) world.balls)) world.seed
  world { balls = [], particles = world.particles <> burst.value, seed = burst.seed }

-- | A sparkle burst like a found ball's, anywhere. `color` picks a
-- | `Palette` swatch for the ring.
sparkleBurst :: Number -> { x :: Number, y :: Number, radius :: Number, color :: Int } -> World -> World
sparkleBurst intensity at world = do
  let
    result = Random.runGen (burstParticles intensity at) world.seed
  world { particles = world.particles <> result.value, seed = result.seed }

-- | A few tiny sparkles left behind by a moving finger.
glitter :: Number -> Number -> World -> World
glitter x y world = do
  let
    result = Random.runGen (traverse (const speck) (Array.range 1 2)) world.seed
  world { particles = world.particles <> result.value, seed = result.seed }
  where
  speck = do
    angle <- Random.range 0.0 Number.tau
    v <- Random.range 10.0 60.0
    life <- Random.range 0.4 0.9
    sz <- Random.range 3.0 7.0
    color <- Random.pick Palette.sparkles
    spin <- Random.range 0.0 Number.tau
    pure
      { x
      , y
      , vx: Number.cos angle * v
      , vy: Number.sin angle * v
      , age: 0.0
      , life
      , size: sz
      , color
      , shape: Sparkle
      , spin
      , spinRate: 4.0
      , gravity: 30.0
      }

burstParticles :: forall r. Number -> { x :: Number, y :: Number, radius :: Number, color :: Int | r } -> Gen (Array Particle)
burstParticles intensity ball = do
  let
    count k = Int.round (Int.toNumber k * intensity)
  sparkles <- traverse (const (particle Sparkle 180.0 520.0 0.7 1.4 (ball.radius * 0.12) (ball.radius * 0.26))) (Array.range 1 (count 26))
  dots <- traverse (const (particle Dot 80.0 380.0 0.5 1.1 2.0 5.0)) (Array.range 1 (count 18))
  hearts <- traverse (const (particle Heart 60.0 260.0 0.9 1.6 (ball.radius * 0.14) (ball.radius * 0.22))) (Array.range 1 (count 5))
  let
    base = Palette.swatch ball.color
    ring =
      { x: ball.x
      , y: ball.y
      , vx: 0.0
      , vy: 0.0
      , age: 0.0
      , life: 0.55
      , size: ball.radius
      , color: Palette.css base.light
      , shape: Ring
      , spin: 0.0
      , spinRate: 0.0
      , gravity: 0.0
      }
  pure ([ ring ] <> sparkles <> dots <> hearts)
  where
  particle shape vMin vMax lifeMin lifeMax sizeMin sizeMax = do
    angle <- Random.range 0.0 Number.tau
    v <- Random.range vMin vMax
    offset <- Random.range 0.0 (ball.radius * 0.7)
    life <- Random.range lifeMin lifeMax
    sz <- Random.range sizeMin sizeMax
    color <- Random.pick Palette.sparkles
    spin <- Random.range 0.0 Number.tau
    spinRate <- Random.range (-6.0) 6.0
    pure
      { x: ball.x + Number.cos angle * offset
      , y: ball.y + Number.sin angle * offset
      , vx: Number.cos angle * v
      , vy: Number.sin angle * v - 60.0
      , age: 0.0
      , life
      , size: sz
      , color
      , shape
      , spin
      , spinRate
      , gravity: if shape == Heart then -40.0 else 240.0
      }

grumpyPuff :: Ball -> Gen (Array Particle)
grumpyPuff ball = traverse (const puff) (Array.range 1 7)
  where
  puff = do
    angle <- Random.range (-Number.pi) 0.0
    v <- Random.range 40.0 110.0
    life <- Random.range 0.4 0.8
    sz <- Random.range 4.0 9.0
    pure
      { x: ball.x + Number.cos angle * ball.radius
      , y: ball.y + Number.sin angle * ball.radius
      , vx: Number.cos angle * v
      , vy: Number.sin angle * v
      , age: 0.0
      , life
      , size: sz
      , color: "rgba(90,40,70,0.55)"
      , shape: Dot
      , spin: 0.0
      , spinRate: 0.0
      , gravity: -30.0
      }

derive instance Eq ParticleShape
