module Alphababy.Component.Building
  ( Input
  , Output(..)
  , Query
  , component
  ) where

import Prelude

import Alphababy.Capability.Sound (Audio)
import Alphababy.Capability.Sound as Sound
import Alphababy.Capability.Speech (SpeechOptions)
import Alphababy.Capability.Speech as Speech
import Alphababy.Chimes as Chimes
import Alphababy.Component.Hold as Hold
import Alphababy.Content as Content
import Alphababy.Random as Random
import Alphababy.Round (BuildingPlan, BuildingTheme(..))
import Alphababy.Round as Round
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))

type Input =
  { plan :: BuildingPlan
  , speech :: SpeechOptions
  , audio :: Maybe Audio
  , seed :: Int
  , greeting :: String
  }

data Output
  = Finished Boolean
  | Quit

data Query :: Type -> Type
data Query a

type State =
  { input :: Input
  , placed :: Int
  , wrong :: Maybe String
  , hint :: Boolean
  , firstTry :: Boolean
  , hintTimer :: Maybe H.ForkId
  , status :: String
  }

data Action
  = Initialize
  | Pick String
  | Repeat
  | Finish
  | Exit

type Slots = (hold :: H.Slot Hold.Query Hold.Output Unit)

component :: forall m. MonadAff m => H.Component Query Input Output m
component = H.mkComponent
  { initialState: \input ->
      { input, placed: 0, wrong: Nothing, hint: false, firstTry: true, hintTimer: Nothing, status: "" }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.main
    [ HP.classes (map H.ClassName [ "game", "word-builder", sceneClass ]) ]
    [ HH.header [ HP.class_ (H.ClassName "word-heading") ]
        [ HH.h1_ [ HH.text title ]
        , HH.p_ [ HH.text "Tap the letters in order." ]
        ]
    , HH.div [ HP.class_ (H.ClassName "word-stage") ]
        [ HH.div [ HP.class_ (H.ClassName "word-stage-top") ]
            [ HH.button
                [ HP.class_ (H.ClassName "word-clue")
                , ARIA.label ("Hear the word " <> plan.word)
                , HP.title "Hear the word"
                , HP.disabled complete
                , HE.onClick \_ -> Repeat
                ]
                [ HH.span [ ARIA.hidden "true" ] [ HH.text plan.picture ]
                , HH.span [ HP.class_ (H.ClassName "word-clue-speak"), ARIA.hidden "true" ] [ HH.text "🔊" ]
                ]
            , case plan.theme of
                Potions -> dragon complete
                Train -> HH.text ""
            ]
        , HH.div [ HP.class_ (H.ClassName "word-platform") ]
            [ HH.div
                [ HP.classes (map H.ClassName ([ "word-pieces" ] <> if complete then [ "is-complete" ] else []))
                , HP.attr (HH.AttrName "role") "group"
                , ARIA.label "Word slots"
                ]
                (Array.mapWithIndex (piece state.placed) plan.letters <> engine)
            ]
        ]
    , HH.div
        [ HP.class_ (H.ClassName "word-tiles")
        , HP.attr (HH.AttrName "role") "group"
        , ARIA.label "Letter choices"
        ]
        (map tile plan.tiles)
    , HH.div [ HP.class_ (H.ClassName "word-status"), ARIA.live "polite" ] [ HH.text state.status ]
    , HH.button
        [ HP.class_ (H.ClassName "hud-speak")
        , HP.title "Say it again"
        , ARIA.label "Say it again"
        , HP.disabled complete
        , HE.onClick \_ -> Repeat
        ]
        [ HH.text "🔊" ]
    , HH.slot (Proxy :: _ "hold") unit Hold.component { label: "✕", title: "Hold to go home" } (const Exit)
    ]
  where
  plan = state.input.plan
  next = nextLetter state
  complete = not (isJust next)
  sceneClass = case plan.theme of
    Potions -> "word-potions"
    Train -> "word-train"
  title = case plan.theme of
    Potions -> "Potion words"
    Train -> "Word train"
  engine = case plan.theme of
    Potions -> []
    Train -> [ HH.span [ HP.class_ (H.ClassName "word-engine"), ARIA.hidden "true" ] [] ]

  tile letter =
    HH.button
      [ HP.classes
          ( map H.ClassName
              ( [ "word-tile" ]
                  <> (if state.wrong == Just letter then [ "is-wrong" ] else [])
                  <> (if state.hint && next == Just letter then [ "is-hint" ] else [])
              )
          )
      , HP.disabled complete
      , HE.onClick \_ -> Pick letter
      ]
      [ HH.text letter ]

piece :: forall w i. Int -> Int -> String -> HH.HTML w i
piece placed index letter =
  HH.div
    [ HP.classes
        ( map H.ClassName
            ( [ "word-piece" ]
                <> (if filled then [ "is-filled" ] else [])
                <> (if index == placed then [ "is-next" ] else [])
            )
        )
    , HP.attr (HH.AttrName "role") "img"
    , ARIA.label ("Letter " <> show (index + 1) <> if filled then ", " <> letter else ", empty")
    ]
    [ HH.span [ HP.class_ (H.ClassName "word-piece-glyph") ] [ HH.text (if filled then letter else "") ]
    , if filled then
        HH.span [ HP.class_ (H.ClassName "word-sparkles"), ARIA.hidden "true" ]
          [ HH.span_ [ HH.text "✦" ], HH.span_ [ HH.text "✦" ], HH.span_ [ HH.text "✧" ] ]
      else HH.text ""
    ]
  where
  filled = index < placed

dragon :: forall w i. Boolean -> HH.HTML w i
dragon well =
  HH.div
    [ HP.class_ (H.ClassName (if well then "word-dragon is-well" else "word-dragon"))
    , HP.attr (HH.AttrName "role") "img"
    , ARIA.label (if well then "The baby dragon feels better" else "The baby dragon feels sick")
    ]
    [ portrait "dragon-sick"
    , portrait "dragon-well"
    ]
  where
  portrait name =
    HH.img
      [ HP.class_ (H.ClassName name)
      , HP.src ("./word-building/" <> name <> ".png")
      , HP.alt ""
      ]

nextLetter :: State -> Maybe String
nextLetter state = Array.index state.input.plan.letters state.placed

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    say (state.input.greeting <> state.input.plan.prompt)
    startHintTimer
  Repeat -> do
    state <- H.get
    for_ (nextLetter state) \letter ->
      if state.hint then
        say ("The word is " <> state.input.plan.word <> ". Try " <> Content.letterName letter <> " next.")
      else do
        say state.input.plan.prompt
        startHintTimer
  Exit -> do
    liftEffect Speech.cancel
    H.raise Quit
  Finish -> do
    state <- H.get
    H.raise (Finished state.firstTry)
  Pick letter -> do
    state <- H.get
    for_ (nextLetter state) \expected -> do
      stopHintTimer
      if letter == expected then do
        let
          placed = state.placed + 1
          complete = placed == Array.length state.input.plan.letters
          status =
            if complete then "You spelled " <> state.input.plan.word <> "!"
            else letter <> " fits. Find letter " <> show (placed + 1) <> "."
        H.modify_ _ { placed = placed, wrong = Nothing, hint = false, status = status }
        playSound (Chimes.found state.placed)
        if complete then do
          { emitter, listener } <- liftEffect HS.create
          -- Dispatch completion outside the fork, which is killed when App removes this screen.
          void $ H.subscribe (Finish <$ emitter)
          void $ H.fork (celebrate (HS.notify listener unit))
        else do
          say (Content.letterName letter <> "!")
          startHintTimer
      else do
        H.modify_ _ { wrong = Just letter, firstTry = false, status = "Try again." }
        playSound Chimes.mistake
        if isJust state.wrong || state.hint then giveHint expected
        else do
          say ("Try again. Listen: " <> state.input.plan.word <> ".")
          startHintTimer

startHintTimer :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
startHintTimer = do
  stopHintTimer
  timer <- H.fork do
    H.liftAff (delay (Milliseconds 12000.0))
    H.modify_ _ { hintTimer = Nothing }
    state <- H.get
    for_ (nextLetter state) giveHint
  H.modify_ _ { hintTimer = Just timer }

stopHintTimer :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
stopHintTimer = do
  state <- H.get
  for_ state.hintTimer H.kill
  H.modify_ _ { hintTimer = Nothing }

giveHint :: forall m. MonadAff m => String -> H.HalogenM State Action Slots Output m Unit
giveHint letter = do
  H.modify_ _ { hint = true, firstTry = false, status = "Try " <> letter <> " next." }
  say ("Try " <> Content.letterName letter <> " next.")

celebrate :: forall m. MonadAff m => Effect Unit -> H.HalogenM State Action Slots Output m Unit
celebrate finish = do
  state <- H.get
  let
    plan = state.input.plan
    praise = Random.evalGen (Random.pick Round.praise) (Random.mkSeed state.input.seed)
    result = case plan.theme of
      Potions -> "The dragon feels better!"
      Train -> "There goes the train!"
  H.liftAff (delay (Milliseconds 240.0))
  playSound Chimes.fanfare
  H.liftAff (Speech.speak state.input.speech (praise <> " You spelled " <> plan.word <> "! " <> result))
  H.liftAff (delay (Milliseconds 1400.0))
  liftEffect finish

say :: forall m. MonadAff m => String -> H.HalogenM State Action Slots Output m Unit
say line = do
  state <- H.get
  liftEffect (Speech.say state.input.speech line)

playSound :: forall m. MonadAff m => Array Sound.Note -> H.HalogenM State Action Slots Output m Unit
playSound notes = do
  state <- H.get
  for_ state.input.audio \audio -> liftEffect (Sound.play audio notes)
