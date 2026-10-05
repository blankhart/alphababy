module Alphababy.Component.Game
  ( Input
  , Output(..)
  , Outcome(..)
  , Query
  , component
  ) where

import Prelude

import Alphababy.Capability.Screen as Screen
import Alphababy.Capability.Sound (Audio)
import Alphababy.Capability.Sound as Sound
import Alphababy.Capability.Speech (SpeechOptions)
import Alphababy.Capability.Speech as Speech
import Alphababy.Chimes as Chimes
import Alphababy.Component.Hold as Hold
import Alphababy.Content (Label)
import Alphababy.Progress (RoundResult)
import Alphababy.Random as Random
import Alphababy.Render as Render
import Alphababy.Round (Plan, RoundKind(..))
import Alphababy.Round as Round
import Alphababy.UI.Face as UIFace
import Alphababy.World (TapOutcome(..), World)
import Alphababy.World as World
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
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
import Web.Event.Event (EventType(..))
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

type Input =
  { plan :: Plan
  , speech :: SpeechOptions
  , audio :: Maybe Audio
  , pace :: Number
  , seed :: Int
  , greeting :: String
  }

data Outcome
  = LessonDone RoundResult
  | PartyDone

data Output
  = Finished Outcome
  | Quit

data Query :: Type -> Type
data Query a

-- | The simulation lives outside Halogen state: it changes every frame and
-- | nothing in the HTML depends on it.
type Scene =
  { world :: Ref World
  , ctx :: Context2D
  , canvas :: CanvasElement
  , pixelRatio :: Ref Number
  , quiet :: Ref Number
  }

data Phase
  = Hunting
  | Celebrating

type State =
  { plan :: Plan
  , speech :: SpeechOptions
  , audio :: Maybe Audio
  , pace :: Number
  , seed :: Int
  , greeting :: String
  , scene :: Maybe Scene
  , phase :: Phase
  , found :: Int
  , mistakes :: Int
  , hinted :: Boolean
  , showHint :: Boolean
  , praiseSeed :: Int
  }

data Action
  = Initialize
  | Frame Number
  | Resized
  | Tap MouseEvent
  | Repeat
  | Exit

type Slots = (hold :: H.Slot Hold.Query Hold.Output Unit)

canvasRef :: H.RefLabel
canvasRef = H.RefLabel "game-canvas"

-- | Seconds without finding anything before the targets start to glow.
hintDelay :: Number
hintDelay = 12.0

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
    { plan: input.plan
    , speech: input.speech
    , audio: input.audio
    , pace: input.pace
    , seed: input.seed
    , greeting: input.greeting
    , scene: Nothing
    , phase: Hunting
    , found: 0
    , mistakes: 0
    , hinted: false
    , showHint: false
    , praiseSeed: input.seed
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.class_ (H.ClassName "game") ]
      [ HH.canvas
          [ HP.ref canvasRef
          , HP.class_ (H.ClassName "game-canvas")
          , HE.handler (EventType "pointerdown") (Tap <<< unsafeCoerce)
          ]
      , HH.button
          [ HP.class_ (H.ClassName "hud-speak")
          , HP.title "Say it again"
          , HE.onClick \_ -> Repeat
          ]
          [ HH.text "🔊" ]
      , case state.showHint, state.plan.kind of
          true, Lesson skill ->
            HH.div
              [ HP.class_ (H.ClassName "hud-hint") ]
              [ UIFace.face (NonEmptyArray.head skill.targets).face ]
          _, _ -> HH.text ""
      , HH.slot (Proxy :: _ "hold") unit Hold.component { label: "✕", title: "Hold to go home" } (const Exit)
      ]

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      H.getHTMLElementRef canvasRef >>= case _ of
        Nothing -> pure unit
        Just el -> do
          let
            canvas = htmlCanvasToCanvas (unsafeCoerce el)
          ctx <- liftEffect (Canvas.getContext2D canvas)
          size <- liftEffect (Screen.elementSize canvas)
          pixelRatio <- liftEffect (fitCanvas canvas)
          worldRef <- liftEffect (Ref.new (World.create state.pace size state.plan.theme state.plan.balls (Random.mkSeed state.seed)))
          ratioRef <- liftEffect (Ref.new pixelRatio)
          quiet <- liftEffect (Ref.new 0.0)
          let
            scene = { world: worldRef, ctx, canvas, pixelRatio: ratioRef, quiet }
          H.modify_ _ { scene = Just scene }
          liftEffect (drawScene scene)
          void $ H.subscribe (map Frame Screen.animationFrames)
          void $ H.subscribe (map (const Resized) (Screen.elementResizes canvas))
          H.liftAff (Screen.fontsReady Render.fontSpecs)
          say (state.greeting <> state.plan.prompt)

    Frame dt -> do
      state <- H.get
      for_ state.scene \scene -> do
        quietFor <- liftEffect do
          Ref.modify_ (World.step dt) scene.world
          drawScene scene
          Ref.modify (_ + dt) scene.quiet
        case state.phase of
          Hunting | quietFor > hintDelay -> do
            liftEffect do
              Ref.write 0.0 scene.quiet
              Ref.modify_ (World.setHint true) scene.world
            H.modify_ _ { hinted = true, showHint = true }
            say state.plan.reminder
          _ -> pure unit

    Resized -> do
      state <- H.get
      for_ state.scene \scene -> liftEffect do
        ratio <- fitCanvas scene.canvas
        Ref.write ratio scene.pixelRatio
        size <- Screen.elementSize scene.canvas
        Ref.modify_ (World.resize size) scene.world
        drawScene scene

    Tap event -> do
      state <- H.get
      case state.scene, state.phase of
        Just scene, Hunting -> do
          let
            x = Int.toNumber (MouseEvent.clientX event)
            y = Int.toNumber (MouseEvent.clientY event)
          result <- liftEffect do
            world <- Ref.read scene.world
            let
              result = World.tap x y world
            Ref.write result.world scene.world
            pure result
          case result.outcome of
            Missed -> pure unit
            Found ball -> do
              liftEffect (Ref.write 0.0 scene.quiet)
              H.modify_ _ { found = state.found + 1 }
              playSound (Chimes.found state.found)
              if World.remainingTargets result.world == 0 then celebrate scene
              else cheer ball.label
            Mistake ball -> do
              H.modify_ _ { mistakes = state.mistakes + 1 }
              playSound Chimes.mistake
              case state.plan.kind of
                Lesson skill -> say (skill.explain ball.label <> " " <> state.plan.reminder)
                Party -> pure unit
        _, _ -> pure unit

    Repeat -> do
      state <- H.get
      say state.plan.prompt

    Exit -> do
      liftEffect Speech.cancel
      H.raise Quit

  cheer :: Label -> H.HalogenM State Action Slots Output m Unit
  cheer label = do
    state <- H.get
    case state.plan.kind of
      Party -> pure unit
      Lesson skill -> do
        let
          next = Random.runGen (Random.pick Round.praise) (Random.mkSeed state.praiseSeed)
        H.modify_ _ { praiseSeed = Random.seedToInt next.seed }
        -- Alternate plain naming and praise so it doesn't get repetitive.
        say (if state.found `mod` 2 == 0 then next.value <> " " <> skill.cheer label else skill.cheer label)

  celebrate :: Scene -> H.HalogenM State Action Slots Output m Unit
  celebrate scene = do
    H.modify_ _ { phase = Celebrating, showHint = false }
    state <- H.get
    playSound Chimes.fanfare
    let
      praiseWord = Random.evalGen (Random.pick Round.praise) (Random.mkSeed state.praiseSeed)
      line = case state.plan.kind of
        Party -> state.plan.finale
        Lesson _ -> praiseWord <> " You found them all! " <> state.plan.finale
    H.liftAff (delay (Milliseconds 350.0))
    liftEffect (Ref.modify_ World.popAll scene.world)
    H.liftAff (Speech.speak state.speech line)
    H.liftAff (delay (Milliseconds 500.0))
    final <- H.get
    H.raise $ Finished case final.plan.kind of
      Party -> PartyDone
      Lesson skill -> LessonDone
        { skill: skill.id
        , found: final.found
        , mistakes: final.mistakes
        , hinted: final.hinted
        }

  say :: String -> H.HalogenM State Action Slots Output m Unit
  say text = do
    state <- H.get
    liftEffect (Speech.say state.speech text)

  playSound notes = do
    state <- H.get
    for_ state.audio \audio -> liftEffect (Sound.play audio notes)

drawScene :: Scene -> Effect Unit
drawScene scene = do
  world <- Ref.read scene.world
  ratio <- Ref.read scene.pixelRatio
  Render.draw scene.ctx ratio world

-- | Matches the canvas backing store to the viewport. The pixel ratio is
-- | capped at 2 to keep phones with 3x screens at a smooth frame rate.
fitCanvas :: CanvasElement -> Effect Number
fitCanvas canvas = do
  size <- Screen.elementSize canvas
  ratio <- map (Number.min 2.0) Screen.devicePixelRatio
  Canvas.setCanvasDimensions canvas { width: Number.floor (size.width * ratio), height: Number.floor (size.height * ratio) }
  pure ratio

htmlCanvasToCanvas :: HTMLCanvasElement -> CanvasElement
htmlCanvasToCanvas = unsafeCoerce
