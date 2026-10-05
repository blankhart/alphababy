module Alphababy.Component.Writing
  ( Input
  , Output(..)
  , Query
  , component
  ) where

import Prelude

import Alphababy.Capability.Pointer as Pointer
import Alphababy.Capability.Screen as Screen
import Alphababy.Capability.Sound (Audio)
import Alphababy.Capability.Sound as Sound
import Alphababy.Capability.Speech (SpeechOptions)
import Alphababy.Capability.Speech as Speech
import Alphababy.Chimes as Chimes
import Alphababy.Component.Hold as Hold
import Alphababy.Content as Content
import Alphababy.Handwriting (Ink, Judgement, TraceProgress)
import Alphababy.Handwriting as Handwriting
import Alphababy.Progress (WritingResult)
import Alphababy.Random as Random
import Alphababy.Render.Writing (Layout)
import Alphababy.Render.Writing as RenderWriting
import Alphababy.Round (Theme(..), WritingMode(..), WritingPlan)
import Alphababy.Round as Round
import Alphababy.Strokes (Point, Template)
import Alphababy.Strokes as Strokes
import Alphababy.World (World)
import Alphababy.World as World
import Data.Array as Array
import Data.Foldable (for_, sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust)
import Data.Number as Number
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Graphics.Canvas (CanvasElement, Context2D)
import Graphics.Canvas as Canvas
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, EventType(..))

type Input =
  { plan :: WritingPlan
  , speech :: SpeechOptions
  , audio :: Maybe Audio
  , seed :: Int
  , greeting :: String
  }

data Output
  = Finished WritingResult
  | Quit

data Query :: Type -> Type
data Query a

-- | Like the finding game, the drawing lives outside Halogen state: ink
-- | changes with every finger movement and no HTML depends on it. Ink is
-- | kept in template coordinates so that it stays on the letter when the
-- | screen is resized.
type Scene =
  { world :: Ref World
  , ctx :: Context2D
  , canvas :: CanvasElement
  , pixelRatio :: Ref Number
  , layout :: Ref Layout
  , ink :: Ref Ink
  , pen :: Ref (Maybe Int)
  , lit :: Ref TraceProgress
  , demo :: Ref (Maybe Number)
  , ghost :: Ref (Maybe Template)
  , idle :: Ref Number
  }

-- | `Watching` a demonstration (touching the screen ends it), `Drawing`,
-- | or `Judging` an attempt, when drawing is ignored.
data Phase
  = Watching
  | Drawing
  | Judging

derive instance Eq Phase

type State =
  { input :: Input
  , scene :: Maybe Scene
  , phase :: Phase
  , attempt :: Int
  , scores :: Array Number
  , hasInk :: Boolean
  , nudged :: Boolean
  }

data Action
  = Initialize
  | Frame Number
  | Resized
  | PenDown Event
  | PenMove Event
  | PenUp Event
  | DoneDrawing
  | Repeat
  | Exit

type Slots = (hold :: H.Slot Hold.Query Hold.Output Unit)

canvasRef :: H.RefLabel
canvasRef = H.RefLabel "writing-canvas"

-- | Demonstration speed in capital heights per second, and the pause
-- | (as a distance) between strokes.
demoSpeed :: Number
demoSpeed = 0.9

demoGap :: Number
demoGap = 0.3

-- | Seconds of stillness before a freehand attempt is judged without her
-- | tapping the star.
freehandPause :: Number
freehandPause = 5.0

-- | Seconds of stillness before tracing gets a reminder, and then before a
-- | partly traced letter is judged as it is.
tracePause :: Number
tracePause = 7.0

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where
  initialState input =
    { input
    , scene: Nothing
    , phase: Watching
    , attempt: 1
    , scores: []
    , hasInk: false
    , nudged: false
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.class_ (H.ClassName "game") ]
      [ HH.canvas
          [ HP.ref canvasRef
          , HP.class_ (H.ClassName "game-canvas")
          , HE.handler (EventType "pointerdown") PenDown
          , HE.handler (EventType "pointermove") PenMove
          , HE.handler (EventType "pointerup") PenUp
          , HE.handler (EventType "pointercancel") PenUp
          ]
      , HH.button
          [ HP.class_ (H.ClassName "hud-speak")
          , HP.title "Say it again"
          , HE.onClick \_ -> Repeat
          ]
          [ HH.text "🔊" ]
      , if state.input.plan.mode == Freehand && state.hasInk && state.phase == Drawing then
          HH.button
            [ HP.class_ (H.ClassName "write-done")
            , HP.title "Done"
            , HE.onClick \_ -> DoneDrawing
            ]
            [ HH.text "⭐" ]
        else HH.text ""
      , HH.slot (Proxy :: _ "hold") unit Hold.component { label: "✕", title: "Hold to go home" } (const Exit)
      ]

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      let
        plan = state.input.plan
      H.getHTMLElementRef canvasRef >>= case _ of
        Nothing -> pure unit
        Just el -> do
          let
            canvas = unsafeCoerce el :: CanvasElement
          scene <- liftEffect do
            ctx <- Canvas.getContext2D canvas
            size <- Screen.elementSize canvas
            ratio <- fitCanvas canvas
            -- The world supplies the twinkling sky and the sparkles; it
            -- has no balls.
            world <- Ref.new (World.create 1.0 size Bubbles [] (Random.mkSeed state.input.seed))
            pixelRatio <- Ref.new ratio
            layout <- Ref.new (RenderWriting.layoutFor size plan.template)
            ink <- Ref.new []
            pen <- Ref.new Nothing
            lit <- Ref.new (Handwriting.startTrace plan.template)
            -- Writing from memory starts without a demonstration.
            demo <- Ref.new (if plan.mode == Tracing || plan.showModel then Just 0.0 else Nothing)
            ghost <- Ref.new Nothing
            idle <- Ref.new 0.0
            pure { world, ctx, canvas, pixelRatio, layout, ink, pen, lit, demo, ghost, idle }
          H.modify_ _ { scene = Just scene }
          liftEffect (drawScene plan scene)
          void $ H.subscribe (map Frame Screen.animationFrames)
          void $ H.subscribe (map (const Resized) (Screen.elementResizes canvas))
          say (state.input.greeting <> plan.prompt)

    Frame dt -> do
      state <- H.get
      for_ state.scene \scene -> do
        idleFor <- liftEffect do
          Ref.modify_ (World.step dt) scene.world
          demo <- Ref.read scene.demo
          for_ demo \travelled -> do
            let
              -- Linger a moment on the finished letter.
              total = Strokes.templateLength demoGap state.input.plan.template + 0.6 * demoSpeed
            Ref.write (if travelled > total then Nothing else Just (travelled + dt * demoSpeed)) scene.demo
          drawScene state.input.plan scene
          pen <- Ref.read scene.pen
          if isJust pen || isJust demo then Ref.write 0.0 scene.idle *> pure 0.0
          else Ref.modify (_ + dt) scene.idle
        when (state.phase /= Judging) (whileIdle scene idleFor)

    Resized -> do
      state <- H.get
      for_ state.scene \scene -> liftEffect do
        ratio <- fitCanvas scene.canvas
        Ref.write ratio scene.pixelRatio
        size <- Screen.elementSize scene.canvas
        Ref.modify_ (World.resize size) scene.world
        Ref.write (RenderWriting.layoutFor size state.input.plan.template) scene.layout
        drawScene state.input.plan scene

    PenDown event -> do
      state <- H.get
      for_ state.scene \scene -> when (state.phase /= Judging) do
        free <- liftEffect (map (_ == Nothing) (Ref.read scene.pen))
        when free do
          H.modify_ _ { phase = Drawing }
          liftEffect do
            Ref.write (Just (Pointer.pointerId event)) scene.pen
            Ref.write Nothing scene.demo
            Ref.write Nothing scene.ghost
            layout <- Ref.read scene.layout
            let
              points = map (RenderWriting.fromScreen layout) (Pointer.positions event)
            Ref.modify_ (\ink -> Array.snoc ink (Array.take 1 points)) scene.ink
            for_ (Array.head points) \p -> addPoint state.input.plan scene p p

    PenMove event -> do
      state <- H.get
      for_ state.scene \scene -> do
        pen <- liftEffect (Ref.read scene.pen)
        when (pen == Just (Pointer.pointerId event) && state.phase == Drawing) do
          liftEffect do
            layout <- Ref.read scene.layout
            for_ (map (RenderWriting.fromScreen layout) (Pointer.positions event)) \p -> do
              ink <- Ref.read scene.ink
              let
                previous = Array.last ink >>= Array.last
              Ref.write (extendLast p ink) scene.ink
              addPoint state.input.plan scene (maybeOr p previous) p

    PenUp event -> do
      state <- H.get
      for_ state.scene \scene -> do
        pen <- liftEffect (Ref.read scene.pen)
        when (pen == Just (Pointer.pointerId event)) do
          liftEffect do
            Ref.write Nothing scene.pen
            Ref.write 0.0 scene.idle
          when (state.phase == Drawing) do
            H.modify_ _ { hasInk = true }
            traceProgress

    DoneDrawing -> do
      state <- H.get
      when (state.phase == Drawing) judge

    Repeat -> do
      state <- H.get
      when (state.phase /= Judging) do
        for_ state.scene \scene -> liftEffect do
          pen <- Ref.read scene.pen
          when (pen == Nothing) (Ref.write (Just 0.0) scene.demo)
        say state.input.plan.prompt

    Exit -> do
      liftEffect Speech.cancel
      H.raise Quit

  -- Lights the tracing path and leaves a little glitter behind the finger.
  addPoint :: WritingPlan -> Scene -> Point -> Point -> Effect Unit
  addPoint plan scene from to = do
    when (plan.mode == Tracing) (Ref.modify_ (Handwriting.lightUp plan.template from to) scene.lit)
    layout <- Ref.read scene.layout
    let
      p = RenderWriting.toScreen layout to
    Ref.modify_ (World.glitter p.x p.y) scene.world

  -- Tracing is judged when she lifts her finger with all of it lit, so
  -- that she is never cut off mid-stroke.
  traceProgress :: H.HalogenM State Action Slots Output m Unit
  traceProgress = do
    state <- H.get
    for_ state.scene \scene -> when (state.input.plan.mode == Tracing) do
      lit <- liftEffect (Ref.read scene.lit)
      when (Handwriting.traceComplete lit) judge

  whileIdle :: Scene -> Number -> H.HalogenM State Action Slots Output m Unit
  whileIdle scene seconds = do
    state <- H.get
    let
      plan = state.input.plan
      restart = liftEffect (Ref.write 0.0 scene.idle)
    case plan.mode of
      Freehand
        | state.hasInk && seconds > freehandPause -> judge
        | not state.hasInk && seconds > 2.0 * freehandPause -> do
            restart
            say (Content.writeNudge plan.task)
        | otherwise -> pure unit
      Tracing
        | seconds > tracePause && state.nudged && state.hasInk -> judge
        | seconds > tracePause -> do
            restart
            H.modify_ _ { nudged = true }
            lit <- liftEffect (Ref.read scene.lit)
            say (if Handwriting.litFraction lit > 0.0 then Content.traceNextStroke else Content.traceNudge)
        | otherwise -> pure unit

  judge :: H.HalogenM State Action Slots Output m Unit
  judge = do
    state <- H.get
    for_ state.scene \scene -> do
      H.modify_ _ { phase = Judging }
      ink <- liftEffect (Ref.read scene.ink)
      let
        plan = state.input.plan

        judgement :: Judgement Content.WritingRival
        judgement = case plan.mode of
          Tracing -> do
            let
              j = Handwriting.judgeTrace plan.template ink
            { verdict: map absurd j.verdict, score: j.score }
          Freehand -> Handwriting.judgeFreehand { target: plan.template, rivals: plan.rivals } ink
        scores = Array.snoc state.scores judgement.score
      H.modify_ _ { scores = scores }
      if Handwriting.passed judgement.verdict then celebrate scene ink
      else if state.attempt == 1 then do
        playSound Chimes.mistake
        showGhost scene ink
        speak (Content.writingFeedback (plan.mode == Tracing) plan.task judgement.verdict)
        liftEffect do
          Ref.write [] scene.ink
          Ref.write (Handwriting.startTrace plan.template) scene.lit
          Ref.write Nothing scene.ghost
          Ref.write (Just 0.0) scene.demo
          Ref.write 0.0 scene.idle
        H.modify_ _ { phase = Watching, attempt = 2, hasInk = false, nudged = false }
      else do
        showGhost scene ink
        speak (Content.writingGoodTry plan.task)
        H.liftAff (delay (Milliseconds 400.0))
        finish

  -- For freehand, the letter as it should have looked, drawn over hers.
  showGhost :: Scene -> Ink -> H.HalogenM State Action Slots Output m Unit
  showGhost scene ink = do
    state <- H.get
    when (state.input.plan.mode == Freehand && Handwriting.extent ink > 0.0) $ liftEffect $
      Ref.write (Just (Handwriting.overlay ink state.input.plan.template)) scene.ghost

  celebrate :: Scene -> Ink -> H.HalogenM State Action Slots Output m Unit
  celebrate scene ink = do
    state <- H.get
    playSound Chimes.fanfare
    liftEffect do
      layout <- Ref.read scene.layout
      let
        plan = state.input.plan
        -- Burst along what she drew; for tracing that is the letter.
        along = case plan.mode of
          Tracing -> Array.concat plan.template
          Freehand -> Array.concatMap Handwriting.densify ink
        every = max 1 (Array.length along / 12)
        spots = Array.mapMaybe (\i -> Array.index along (i * every)) (Array.range 0 11)
      for_ (Array.mapWithIndex (\i p -> { i, p: RenderWriting.toScreen layout p }) spots) \{ i, p } ->
        Ref.modify_ (World.sparkleBurst 0.45 { x: p.x, y: p.y, radius: layout.unit * 0.18, color: i }) scene.world
    let
      praiseWord = Random.evalGen (Random.pick Round.praise) (Random.mkSeed (state.input.seed + state.attempt))
    speak (praiseWord <> " " <> Content.writingPraise state.input.plan.task)
    H.liftAff (delay (Milliseconds 500.0))
    finish

  finish :: H.HalogenM State Action Slots Output m Unit
  finish = do
    state <- H.get
    H.raise $ Finished
      { skill: state.input.plan.skill.id
      , score: sum state.scores / Int.toNumber (max 1 (Array.length state.scores))
      , traced: state.input.plan.mode == Tracing
      , firstTry: state.attempt == 1
      }

  say :: String -> H.HalogenM State Action Slots Output m Unit
  say text = do
    state <- H.get
    liftEffect (Speech.say state.input.speech text)

  speak :: String -> H.HalogenM State Action Slots Output m Unit
  speak text = do
    state <- H.get
    H.liftAff (Speech.speak state.input.speech text)

  playSound notes = do
    state <- H.get
    for_ state.input.audio \audio -> liftEffect (Sound.play audio notes)

extendLast :: Point -> Ink -> Ink
extendLast p ink = case Array.unsnoc ink of
  Nothing -> [ [ p ] ]
  Just { init, last } -> Array.snoc init (Array.snoc last p)

maybeOr :: forall a. a -> Maybe a -> a
maybeOr fallback = case _ of
  Just a -> a
  Nothing -> fallback

drawScene :: WritingPlan -> Scene -> Effect Unit
drawScene plan scene = do
  world <- Ref.read scene.world
  ratio <- Ref.read scene.pixelRatio
  layout <- Ref.read scene.layout
  ink <- Ref.read scene.ink
  lit <- Ref.read scene.lit
  demo <- Ref.read scene.demo
  ghost <- Ref.read scene.ghost
  RenderWriting.draw scene.ctx ratio
    { world
    , layout
    , template: plan.template
    , path: case plan.mode of
        Tracing -> Just { lit, next: Handwriting.nextStroke lit }
        Freehand -> Nothing
    , ink
    , demo: map (\travelled -> Strokes.trail demoGap travelled plan.template) demo
    , ghost
    , model: plan.mode == Freehand && plan.showModel
    }

-- | Matches the canvas backing store to its size on screen, as the finding
-- | game does.
fitCanvas :: CanvasElement -> Effect Number
fitCanvas canvas = do
  size <- Screen.elementSize canvas
  ratio <- map (Number.min 2.0) Screen.devicePixelRatio
  Canvas.setCanvasDimensions canvas { width: Number.floor (size.width * ratio), height: Number.floor (size.height * ratio) }
  pure ratio
