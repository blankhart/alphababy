module Alphababy.Component.Settings
  ( Input
  , Output(..)
  , Query
  , component
  ) where

import Prelude

import Alphababy.Capability.Speech (Voice)
import Alphababy.Content (Group, Skill, SkillId)
import Alphababy.Content as Content
import Alphababy.Progress (Progress, Status(..))
import Alphababy.Progress as Progress
import Alphababy.Settings (Settings)
import Alphababy.Settings as Settings
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Number.Format as NumberFormat
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML (window)
import Web.HTML.Window as Window

type Input =
  { settings :: Settings
  , progress :: Progress
  , skills :: Array Skill
  , voices :: Array Voice
  }

data Output
  = UpdateSettings Settings
  | UpdateProgress Progress
  | Preview String
  | Close

data Query :: Type -> Type
data Query a

type State =
  { input :: Input
  , expanded :: Set Group
  }

data Action
  = Receive Input
  | SetName String
  | SetVoice String
  | SetRate String
  | ToggleSound
  | SetMotionSpeed String
  | ToggleWriting
  | ToggleGroup Group
  | ToggleSkill SkillId
  | Expand Group
  | MarkLearned Group
  | Forget Group
  | SetWords String
  | ResetAll
  | TestVoice
  | Done

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: \input -> { input, expanded: Set.empty }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, receive = Just <<< Receive }
    }
  where
  render :: State -> H.ComponentHTML Action () m
  render { input, expanded } = do
    let
      s = input.settings
      english = Array.sortWith _.name (Array.filter (\v -> String.take 2 (String.toLower v.lang) == "en") input.voices)
      unlocked = Array.length (Array.filter (\sk -> isUnlocked (Progress.status input.progress sk.id)) input.skills)
      mastered = Array.length (Array.filter (\sk -> Progress.isMastered input.progress sk.id) input.skills)
    HH.div
      [ HP.class_ (H.ClassName "screen settings") ]
      [ HH.header [ HP.class_ (H.ClassName "settings-header") ]
          [ HH.h1_ [ HH.text "Grown-up settings" ]
          , HH.button [ HP.class_ (H.ClassName "done-button"), HE.onClick \_ -> Done ] [ HH.text "Done" ]
          ]
      , section "How it works"
          [ HH.p_ [ HH.text "Tap ▶ and hand the device over. Instructions are spoken aloud. Each round asks her to find every ball that matches (a letter, a sound, a number, a word…). Right taps burst into sparkles; wrong ones get grumpy and speed up. After 12 seconds without progress the answers glow." ]
          , HH.p_ [ HH.text "The game tracks how well each item is known. It keeps at most four new things in rotation, introduces the next item (in phonics order, starting with s a t p i n) as each is mastered, and brings mastered items back now and then for review. Every fifth round is a sparkle party with nothing to get wrong." ]
          , HH.p_ [ HH.text "For true full-screen play, add this page to the home screen (Share → Add to Home Screen on iPhone/iPad; ⋮ → Install app on Android/Chrome)." ]
          , HH.p_ [ HH.text ("Rounds played: " <> show input.progress.rounds <> " · Stars: " <> show input.progress.stars <> " · Items started: " <> show unlocked <> " · Mastered: " <> show mastered) ]
          ]
      , section "Player"
          [ HH.label [ HP.class_ (H.ClassName "field") ]
              [ HH.span_ [ HH.text "Child's name (used in praise and a \"letters in my name\" game)" ]
              , HH.input [ HP.value s.childName, HP.placeholder "e.g. Lily", HE.onValueChange SetName ]
              ]
          ]
      , section "Voice and sound"
          [ HH.label [ HP.class_ (H.ClassName "field") ]
              [ HH.span_ [ HH.text "Voice" ]
              , HH.select [ HE.onValueChange SetVoice ]
                  ( [ HH.option [ HP.value "", HP.selected (s.voiceUri == Nothing) ] [ HH.text "Automatic" ] ]
                      <> map (\v -> HH.option [ HP.value v.uri, HP.selected (s.voiceUri == Just v.uri) ] [ HH.text (v.name <> " (" <> v.lang <> ")") ]) english
                  )
              ]
          , HH.label [ HP.class_ (H.ClassName "field") ]
              [ HH.span_ [ HH.text ("Speaking speed: " <> NumberFormat.toStringWith (NumberFormat.fixed 2) s.speechRate) ]
              , HH.input
                  [ HP.type_ HP.InputRange
                  , HP.min 0.5
                  , HP.max 1.3
                  , HP.step (HP.Step 0.05)
                  , HP.value (show s.speechRate)
                  , HE.onValueChange SetRate
                  ]
              ]
          , HH.button [ HP.class_ (H.ClassName "small-button"), HE.onClick \_ -> TestVoice ] [ HH.text "🔊 Test voice" ]
          , HH.label [ HP.class_ (H.ClassName "check") ]
              [ HH.input [ HP.type_ HP.InputCheckbox, HP.checked s.soundEffects, HE.onChange \_ -> ToggleSound ]
              , HH.text " Sound effects"
              ]
          ]
      , section "Game"
          [ HH.label [ HP.class_ (H.ClassName "field") ]
              [ HH.span_ [ HH.text ("Ball speed: " <> NumberFormat.toStringWith (NumberFormat.fixed 2) s.motionSpeed <> "×") ]
              , HH.input
                  [ HP.type_ HP.InputRange
                  , HP.min 0.25
                  , HP.max 2.0
                  , HP.step (HP.Step 0.05)
                  , HP.value (show s.motionSpeed)
                  , HE.onValueChange SetMotionSpeed
                  ]
              ]
          , HH.label [ HP.class_ (H.ClassName "check") ]
              [ HH.input [ HP.type_ HP.InputCheckbox, HP.checked s.writing, HE.onChange \_ -> ToggleWriting ]
              , HH.text " Tracing and writing rounds (letters and numbers she has met: she traces them with a finger, then writes them freehand once tracing goes well; one retry per round)"
              ]
          ]
      , section "What to practise"
          ( [ HH.p_ [ HH.text "Untick a group to leave it out. Open a group to switch individual items on or off (crossed-out items are off). ★ = mastered, ● = learning, ○ = not started, ✏️ = writes it. “Mark learned” skips ahead if she already knows a group." ] ]
              <> map (groupSettings input expanded) Content.allGroups
          )
      , section "Extra sight words"
          [ HH.label [ HP.class_ (H.ClassName "field") ]
              [ HH.span_ [ HH.text "Add your own words, separated by commas" ]
              , HH.input
                  [ HP.value (String.joinWith ", " s.extraSightWords)
                  , HP.placeholder "e.g. unicorn, castle, Mia"
                  , HE.onValueChange SetWords
                  ]
              ]
          ]
      , section "Start over"
          [ HH.button [ HP.class_ (H.ClassName "small-button danger"), HE.onClick \_ -> ResetAll ] [ HH.text "Reset all progress" ] ]
      ]

  section title body =
    HH.section [ HP.class_ (H.ClassName "settings-section") ] ([ HH.h2_ [ HH.text title ] ] <> body)

  groupSettings input expanded group = do
    let
      s = input.settings
      members = Array.filter ((_ == group) <<< _.group) input.skills
      enabled = Settings.isGroupEnabled s group
      open = Set.member group expanded
      masteredCount = Array.length (Array.filter (Progress.isMastered input.progress <<< _.id) members)
    HH.div [ HP.class_ (H.ClassName ("group-setting" <> if enabled then "" else " off")) ]
      [ HH.div [ HP.class_ (H.ClassName "group-line") ]
          [ HH.label [ HP.class_ (H.ClassName "check") ]
              [ HH.input [ HP.type_ HP.InputCheckbox, HP.checked enabled, HE.onChange \_ -> ToggleGroup group ]
              , HH.span [ HP.class_ (H.ClassName "group-badge") ] [ HH.text (Content.groupBadge group) ]
              , HH.text (" " <> Content.groupTitle group)
              ]
          , HH.span [ HP.class_ (H.ClassName "group-count") ]
              [ HH.text (if Array.null members then "set a name" else show masteredCount <> "/" <> show (Array.length members) <> " ★") ]
          , HH.button [ HP.class_ (H.ClassName "small-button"), HE.onClick \_ -> Expand group ] [ HH.text (if open then "Close" else "Open") ]
          ]
      , if not open then HH.text ""
        else HH.div_
          [ HH.div [ HP.class_ (H.ClassName "skill-chips") ] (map (skillChip input) members)
          , HH.div [ HP.class_ (H.ClassName "group-actions") ]
              [ HH.button [ HP.class_ (H.ClassName "small-button"), HE.onClick \_ -> MarkLearned group ] [ HH.text "Mark learned" ]
              , HH.button [ HP.class_ (H.ClassName "small-button"), HE.onClick \_ -> Forget group ] [ HH.text "Reset group" ]
              ]
          ]
      ]

  skillChip input skill = do
    let
      on = not (Set.member skill.id input.settings.disabledSkills)
      mark = case Progress.status input.progress skill.id of
        Mastered _ -> "★ "
        Learning _ -> "● "
        Locked -> "○ "
      writes = case Progress.statsFor skill.id input.progress of
        Just st | Progress.canWrite st -> " ✏️"
        _ -> ""
    HH.button
      [ HP.class_ (H.ClassName ("skill-chip" <> if on then "" else " off"))
      , HE.onClick \_ -> ToggleSkill skill.id
      ]
      [ HH.text (mark <> skill.name <> writes) ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Receive input -> H.modify_ _ { input = input }
    SetName name -> updateSettings _ { childName = String.trim name }
    SetVoice uri -> updateSettings _ { voiceUri = if uri == "" then Nothing else Just uri }
    SetRate raw -> for_ (Number.fromString raw) \rate -> updateSettings _ { speechRate = rate }
    SetMotionSpeed raw -> for_ (Number.fromString raw) \speed -> updateSettings _ { motionSpeed = speed }
    ToggleSound -> updateSettings \s -> s { soundEffects = not s.soundEffects }
    ToggleWriting -> updateSettings \s -> s { writing = not s.writing }
    ToggleGroup group -> updateSettings (Settings.toggleGroup group)
    ToggleSkill id -> updateSettings (Settings.toggleSkill id)
    Expand group -> H.modify_ \st -> st { expanded = if Set.member group st.expanded then Set.delete group st.expanded else Set.insert group st.expanded }
    MarkLearned group -> updateProgress (Progress.markLearned group)
    Forget group -> updateProgress (Progress.forgetGroup group)
    SetWords raw -> updateSettings _ { extraSightWords = Array.filter (not <<< String.null) (map String.trim (String.split (Pattern ",") raw)) }
    ResetAll -> do
      ok <- liftEffect (Window.confirm "Erase all progress, stars and stickers?" =<< window)
      when ok (H.raise (UpdateProgress Progress.emptyProgress))
    TestVoice -> do
      name <- H.gets _.input.settings.childName
      H.raise (Preview ("Hi" <> (if String.null name then "" else " " <> name) <> "! Find every letter pee!"))
    Done -> H.raise Close

  updateSettings f = do
    settings <- H.gets _.input.settings
    H.raise (UpdateSettings (f settings))

  updateProgress f = do
    input <- H.gets _.input
    H.raise (UpdateProgress (f input.skills input.progress))

  isUnlocked = case _ of
    Locked -> false
    _ -> true

