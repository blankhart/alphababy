module Alphababy.Component.App
  ( component
  ) where

import Prelude

import Alphababy.Capability.Screen as Screen
import Alphababy.Capability.Sound (Audio)
import Alphababy.Capability.Sound as Sound
import Alphababy.Capability.Speech (SpeechOptions, Voice)
import Alphababy.Capability.Speech as Speech
import Alphababy.Capability.Storage as Storage
import Alphababy.Chimes as Chimes
import Alphababy.Component.Game as Game
import Alphababy.Component.Hold as Hold
import Alphababy.Component.Settings as SettingsComponent
import Alphababy.Component.Writing as Writing
import Alphababy.Content (Skill)
import Alphababy.Content as Content
import Alphababy.Progress (Progress)
import Alphababy.Progress as Progress
import Alphababy.Random (Seed)
import Alphababy.Random as Random
import Alphababy.Round (Activity(..), Theme)
import Alphababy.Round as Round
import Alphababy.Settings (Settings)
import Alphababy.Settings as Settings
import Alphababy.UI.Progress as ProgressPanel
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Number as Number
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

data Screen
  = Welcome
  | Playing Activity
  | Resting ProgressPanel.Rest
  | Adjusting

type State =
  { screen :: Screen
  , settings :: Settings
  , progress :: Progress
  , skills :: Array Skill
  , voices :: Array Voice
  , audio :: Maybe Audio
  , seed :: Seed
  , lastTheme :: Maybe Theme
  , round :: Int
  , sessionRounds :: Int
  }

data Action
  = Initialize
  | VoicesLoaded (Array Voice)
  | Begin
  | Continue
  | GameOutput Game.Output
  | WritingOutput Writing.Output
  | OpenSettings
  | SettingsOutput SettingsComponent.Output

type Slots =
  ( game :: H.Slot Game.Query Game.Output Int
  , writing :: H.Slot Writing.Query Writing.Output Int
  , settings :: H.Slot SettingsComponent.Query SettingsComponent.Output Unit
  , hold :: H.Slot Hold.Query Hold.Output Unit
  )

component :: forall query input output m. MonadAff m => H.Component query input output m
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
  initialState _ =
    { screen: Welcome
    , settings: Settings.defaultSettings
    , progress: Progress.emptyProgress
    , skills: Content.curriculum (Settings.curriculumOptions Settings.defaultSettings)
    , voices: []
    , audio: Nothing
    , seed: Random.mkSeed 1
    , lastTheme: Nothing
    , round: 0
    , sessionRounds: 0
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state = case state.screen of
    Welcome -> renderWelcome state
    Playing (Hunt plan) ->
      HH.slot (Proxy :: _ "game") state.round Game.component
        { plan
        , speech: speechOptions state
        , audio: if state.settings.soundEffects then state.audio else Nothing
        , pace: state.settings.motionSpeed
        , seed: Random.seedToInt state.seed
        , greeting: if state.sessionRounds == 0 then greeting state <> "Tap the ones I ask for. " else ""
        }
        GameOutput
    Playing (Draw plan) ->
      HH.slot (Proxy :: _ "writing") state.round Writing.component
        { plan
        , speech: speechOptions state
        , audio: if state.settings.soundEffects then state.audio else Nothing
        , seed: Random.seedToInt state.seed
        , greeting: if state.sessionRounds == 0 then greeting state else ""
        }
        WritingOutput
    Resting rest ->
      HH.div
        [ HP.class_ (H.ClassName "screen rest-screen") ]
        [ ProgressPanel.panel
            { rest
            , progress: state.progress
            , skills: Settings.enabledSkills state.settings state.skills
            }
            Continue
        , grownUpButton
        ]
    Adjusting ->
      HH.slot (Proxy :: _ "settings") unit SettingsComponent.component
        { settings: state.settings
        , progress: state.progress
        , skills: state.skills
        , voices: state.voices
        }
        SettingsOutput

  renderWelcome state =
    HH.div
      [ HP.class_ (H.ClassName "screen welcome") ]
      [ HH.div [ HP.class_ (H.ClassName "welcome-sparkles") ] []
      , HH.div [ HP.class_ (H.ClassName "welcome-unicorn") ] [ HH.text "🦄" ]
      , HH.h1 [ HP.class_ (H.ClassName "logo") ] [ HH.text "Alphababy" ]
      , if String.null state.settings.childName then HH.text ""
        else HH.p [ HP.class_ (H.ClassName "welcome-name") ] [ HH.text ("Hi, " <> state.settings.childName <> "! 👑") ]
      , HH.button
          [ HP.class_ (H.ClassName "play-button")
          , HE.onClick \_ -> Begin
          ]
          [ HH.text "▶" ]
      , HH.p [ HP.class_ (H.ClassName "stars-line") ]
          [ HH.text (if state.progress.stars > 0 then "⭐ " <> show state.progress.stars else "") ]
      , grownUpButton
      , HH.p [ HP.class_ (H.ClassName "grown-up-hint") ] [ HH.text "Grown-ups: hold ⚙️ for settings" ]
      ]

  grownUpButton =
    HH.div [ HP.class_ (H.ClassName "corner-hold") ]
      [ HH.slot (Proxy :: _ "hold") unit Hold.component { label: "⚙️", title: "Hold for grown-up settings" } (const OpenSettings) ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      saved <- liftEffect Storage.load
      Milliseconds ms <- liftEffect (map unInstant now)
      audio <- liftEffect Sound.create
      for_ saved \s -> H.modify_ _ { settings = s.settings, progress = s.progress }
      H.modify_ \st -> st
        { seed = Random.mkSeed (Int.round (ms `Number.remainder` 2.0e9))
        , audio = audio
        , skills = Content.curriculum (Settings.curriculumOptions st.settings)
        }
      voices <- H.liftAff Speech.voices
      handleAction (VoicesLoaded voices)

    VoicesLoaded voices -> H.modify_ _ { voices = voices }

    Begin -> do
      liftEffect do
        Speech.unlock
        Screen.requestFullscreen
        Screen.keepAwake
      H.modify_ _ { sessionRounds = 0 }
      state <- H.get
      for_ state.audio \audio -> liftEffect do
        Sound.resume audio
        when state.settings.soundEffects (Sound.play audio Chimes.button)
      startRound

    Continue -> do
      state <- H.get
      for_ state.audio \audio -> liftEffect do
        Sound.resume audio
        when state.settings.soundEffects (Sound.play audio Chimes.button)
      startRound

    GameOutput output -> case output of
      Game.Quit -> quit
      Game.Finished outcome -> case outcome of
        Game.LessonDone result -> finishRound (Just result.skill) (Progress.recordRound result)
        Game.PartyDone -> finishRound Nothing Progress.recordParty

    WritingOutput output -> case output of
      Writing.Quit -> quit
      Writing.Finished result -> finishRound (Just result.skill) (Progress.recordWriting result)

    OpenSettings -> do
      liftEffect Speech.cancel
      H.modify_ _ { screen = Adjusting }

    SettingsOutput output -> case output of
      SettingsComponent.Close -> H.modify_ _ { screen = Welcome }
      SettingsComponent.Preview text -> do
        state <- H.get
        liftEffect (Speech.say (speechOptions state) text)
      SettingsComponent.UpdateSettings settings -> do
        H.modify_ _ { settings = settings, skills = Content.curriculum (Settings.curriculumOptions settings) }
        persist
      SettingsComponent.UpdateProgress progress -> do
        H.modify_ _ { progress = progress }
        persist

  quit = do
    liftEffect do
      Screen.exitFullscreen
      Screen.allowSleep
    H.modify_ _ { screen = Welcome }

  startRound = do
    state <- H.get
    let
      enabled = Settings.enabledSkills state.settings state.skills
      progress = Progress.refreshUnlocks enabled state.progress
      generated = Random.runGen (Round.planActivity { writing: state.settings.writing } enabled progress state.lastTheme) state.seed
    case generated.value of
      Nothing -> do
        liftEffect (Speech.say (speechOptions state) "Ask a grown-up to turn on some games!")
        H.modify_ _ { screen = Welcome }
      Just activity ->
        H.modify_ _
          { progress = progress
          , seed = generated.seed
          , screen = Playing activity
          , lastTheme = case activity of
              Hunt plan -> Just plan.theme
              Draw _ -> state.lastTheme
          , round = state.round + 1
          }

  -- `record` updates progress for the round just played, given the sticker
  -- it earned.
  finishRound practiced record = do
    state <- H.get
    let
      pickSticker = Random.runGen (Random.pick Content.stickers) state.seed
      sticker = pickSticker.value
      before = state.progress
      after = record sticker before
      enabled = Settings.enabledSkills state.settings state.skills
      unlocked = Progress.refreshUnlocks enabled after
      rest =
        { sticker
        , earned: after.stars - before.stars
        , practiced
        , nowMastered: maybe false (\id -> Progress.isMastered after id && not (Progress.isMastered before id)) practiced
        }
    H.modify_ _ { progress = unlocked, seed = pickSticker.seed, screen = Resting rest, sessionRounds = state.sessionRounds + 1 }
    persist
    liftEffect (Speech.say (speechOptions state) (restLine state rest unlocked))

  persist = do
    state <- H.get
    liftEffect (Storage.save { settings: state.settings, progress: state.progress })

greeting :: State -> String
greeting state = do
  let
    name = if String.null state.settings.childName then "" else " " <> state.settings.childName
  "Hi" <> name <> "! Let's play! "

restLine :: State -> ProgressPanel.Rest -> Progress -> String
restLine state rest progress = do
  let
    name = if String.null state.settings.childName then "" else ", " <> state.settings.childName
    learned = if rest.nowMastered then " You learned something new!" else ""
  "You got a sticker" <> name <> "!" <> learned <> " You have " <> show progress.stars <> " stars! Tap the rainbow button to play again."

-- | Picks a voice: the one the grown-up chose, otherwise a pleasant
-- | English voice when one of the well-known ones is installed.
speechOptions :: State -> SpeechOptions
speechOptions state =
  { voice: chooseVoice state.settings.voiceUri state.voices
  , rate: state.settings.speechRate
  , pitch: 1.1
  }

chooseVoice :: Maybe String -> Array Voice -> Maybe Voice
chooseVoice chosen voices = case chosen >>= \uri -> Array.find ((_ == uri) <<< _.uri) voices of
  Just v -> Just v
  Nothing -> do
    let
      english = Array.filter (String.contains (String.Pattern "en") <<< String.toLower <<< String.take 2 <<< _.lang) voices
      preferred = [ "Samantha", "Google US English", "Aria", "Jenny", "Ava", "Karen", "Moira", "Tessa", "Serena", "Zira", "Female" ]
      byName = Array.findMap (\p -> Array.find (String.contains (String.Pattern p) <<< _.name) english) preferred
      usEnglish = Array.find ((_ == "en-US") <<< String.replaceAll (String.Pattern "_") (String.Replacement "-") <<< _.lang) english
    case byName of
      Just v -> Just v
      Nothing -> case usEnglish of
        Just v -> Just v
        Nothing -> Array.head english
