module Alphababy.Round
  ( Theme(..)
  , allThemes
  , themeNoun
  , RoundKind(..)
  , PlannedBall
  , Plan
  , planLesson
  , planParty
  , WritingMode(..)
  , WritingPlan
  , planWriting
  , Activity(..)
  , ActivityOptions
  , planActivity
  , writingChance
  , chooseTheme
  , praise
  ) where

import Prelude

import Alphababy.Content (Face(..), Label, Skill, WritingRival, WritingTask)
import Alphababy.Content as Content
import Alphababy.Progress (Progress)
import Alphababy.Progress as Progress
import Alphababy.Random (Gen)
import Alphababy.Random as Random
import Alphababy.Strokes (Template)
import Alphababy.Strokes as Strokes
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))

-- | How the balls look and move.
data Theme
  = Bubbles
  | Balloons
  | Stars
  | Hearts

derive instance Eq Theme

allThemes :: NonEmptyArray Theme
allThemes = NonEmptyArray.cons' Bubbles [ Balloons, Stars, Hearts ]

themeNoun :: Theme -> String
themeNoun = case _ of
  Bubbles -> "bubbles"
  Balloons -> "balloons"
  Stars -> "stars"
  Hearts -> "hearts"

data RoundKind
  = Lesson Skill
  | Party

type PlannedBall = { label :: Label, target :: Boolean }

type Plan =
  { kind :: RoundKind
  , theme :: Theme
  , balls :: Array PlannedBall
  , prompt :: String
  , reminder :: String
  , finale :: String
  }

chooseTheme :: Maybe Theme -> Gen Theme
chooseTheme previous = do
  let
    others = NonEmptyArray.filter (\t -> Just t /= previous) allThemes
  maybe (Random.pick allThemes) Random.pick (NonEmptyArray.fromArray others)

praise :: NonEmptyArray String
praise = NonEmptyArray.cons' "Yay!"
  [ "Great job!", "You got it!", "Sparkly!", "Wonderful!", "Hooray!", "Super!", "Magical!", "Beautiful!", "Yes!", "Amazing!" ]

-- | Builds a round for `skill`. Difficulty follows mastery: a skill she is
-- | just meeting gets few balls and mostly unrelated distractors; a mastered
-- | one gets more balls and more look-alikes. `skills` supplies the
-- | distractor pool (everything else in the same group).
planLesson :: Array Skill -> Progress -> Maybe Theme -> Skill -> Gen Plan
planLesson skills progress previousTheme skill = do
  theme <- chooseTheme previousTheme
  let
    difficulty = maybe 0.0 _.mastery (Progress.statsFor skill.id progress)
    targetKeys = map _.key (NonEmptyArray.toArray skill.targets)
    usable l = not (Array.elem l.key targetKeys) && not (Array.elem l.key skill.avoid)
    siblings = Array.filter (\s -> s.group == skill.group && s.id /= skill.id) skills
    pool = Array.nubByEq (\a b -> a.key == b.key) $ Array.filter usable $
      Array.concatMap (\s -> NonEmptyArray.toArray s.targets <> s.confusables) siblings
    lookAlikes = Array.filter usable skill.confusables
    kinds = min 4 (NonEmptyArray.length skill.targets)
  extra <- Random.int 0 1
  let
    nTargets = min 5 (max kinds (2 + extra + if difficulty >= 0.5 then 1 else 0))
  nDistractors <- Random.int (3 + Int.round (difficulty * 3.0)) (5 + Int.round (difficulty * 3.0))
  targets <- chooseTargets nTargets skill.targets
  distractors <- chooseDistractors nDistractors (0.25 + 0.5 * difficulty) lookAlikes pool
  balls <- Random.shuffle
    (map (\label -> { label, target: true }) targets <> map (\label -> { label, target: false }) distractors)
  pure
    { kind: Lesson skill
    , theme
    , balls
    , prompt: skill.prompt
    , reminder: skill.reminder
    , finale: skill.finale
    }

-- | Distinct targets where the skill has enough of them, otherwise repeats,
-- | always showing every kind of target at least once.
chooseTargets :: Int -> NonEmptyArray Label -> Gen (Array Label)
chooseTargets n targets = do
  shuffled <- Random.shuffle (NonEmptyArray.toArray targets)
  let
    k = Array.length shuffled
  pure (Array.mapMaybe (\i -> Array.index shuffled (i `mod` k)) (Array.range 0 (max n 1 - 1)))

chooseDistractors :: Int -> Number -> Array Label -> Array Label -> Gen (Array Label)
chooseDistractors n lookAlikeRate lookAlikes pool = do
  shuffledPool <- Random.shuffle pool
  shuffledLookAlikes <- Random.shuffle lookAlikes
  picks <- sequence (Array.replicate n (Random.chance lookAlikeRate))
  let
    fromLookAlikes = Array.length (Array.filter identity picks) `min` Array.length shuffledLookAlikes
    chosen = Array.take fromLookAlikes shuffledLookAlikes <> Array.take (n - fromLookAlikes) shuffledPool
    fallback = if Array.null shuffledPool then shuffledLookAlikes else shuffledPool
  -- A small pool (e.g. only two sibling skills) repeats distractors rather
  -- than leaving the screen nearly empty.
  padding <- sequence (Array.replicate (n - Array.length chosen) (Random.pickOr placeholder fallback))
  pure (if Array.null fallback then chosen else chosen <> padding)
  where
  placeholder = { key: "p:star", face: Picture "⭐", spoken: "a star" }

-- | A reward round: every ball is a sticker and every tap is right.
planParty :: Maybe Theme -> Gen Plan
planParty previousTheme = do
  theme <- chooseTheme previousTheme
  n <- Random.int 10 13
  faces <- sequence (Array.replicate n (Random.pick Content.stickers))
  pure
    { kind: Party
    , theme
    , balls: map (\emoji -> { label: { key: "party:" <> emoji, face: Picture emoji, spoken: "" }, target: true }) faces
    , prompt: "Sparkle party! Pop all the " <> themeNoun theme <> "!"
    , reminder: "Pop them all!"
    , finale: "What a sparkly party!"
    }

-- Writing --------------------------------------------------------------------

data WritingMode
  = Tracing
  | Freehand

derive instance Eq WritingMode

-- | A round of tracing or writing one letter or digit. `showModel` is
-- | whether a model to copy stays on screen while writing freehand.
type WritingPlan =
  { skill :: Skill
  , task :: WritingTask
  , template :: Template
  , rivals :: Array (Tuple WritingRival Template)
  , mode :: WritingMode
  , showModel :: Boolean
  , prompt :: String
  }

-- | Plans a writing round for a skill she has already met in a finding
-- | round. She traces until tracing goes well, then writes freehand, first
-- | with a model to copy and then, once she writes it well, from memory.
planWriting :: Progress -> Skill -> Maybe WritingPlan
planWriting progress skill = do
  stats <- Progress.statsFor skill.id progress
  task <- Content.writingTask skill
  template <- Strokes.template task.glyph
  if stats.rounds < 1 then Nothing
  else do
    let
      mode = if Progress.canWrite stats then Freehand else Tracing
      showModel = stats.writing < 0.8
    pure
      { skill
      , task
      , template
      , rivals: Array.mapMaybe (\r -> map (Tuple r) (Strokes.template r.glyph)) task.rivals
      , mode
      , showModel
      , prompt: case mode of
          Tracing -> Content.tracePrompt task
          Freehand -> Content.writePrompt showModel task
      }

data Activity
  = Hunt Plan
  | Draw WritingPlan

type ActivityOptions = { writing :: Boolean }

-- | How often a skill that can be written gets a writing round.
writingChance :: Number
writingChance = 0.6

-- | The next round: a party every tenth round, otherwise a skill chosen
-- | by `Progress.chooseSkill`, sometimes practised by writing it.
planActivity :: ActivityOptions -> Array Skill -> Progress -> Maybe Theme -> Gen (Maybe Activity)
planActivity options enabled progress lastTheme
  | Progress.isPartyRound progress && not (Array.null enabled) = map (Just <<< Hunt) (planParty lastTheme)
  | otherwise = do
      skill <- Progress.chooseSkill enabled progress
      write <- Random.chance writingChance
      flip traverse skill \s -> case planWriting progress s of
        Just plan | options.writing && write -> pure (Draw plan)
        _ -> map Hunt (planLesson enabled progress lastTheme s)
