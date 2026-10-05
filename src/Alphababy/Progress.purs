module Alphababy.Progress
  ( Progress
  , SkillStats
  , Status(..)
  , RoundResult
  , WritingResult
  , emptyProgress
  , masteryThreshold
  , learningCap
  , status
  , statsFor
  , isMastered
  , refreshUnlocks
  , chooseSkill
  , roundScore
  , recordRound
  , recordWriting
  , canWrite
  , recordParty
  , markLearned
  , forgetGroup
  , isPartyRound
  ) where

import Prelude

import Alphababy.Content (Group, Skill, SkillId)
import Alphababy.Random (Gen)
import Alphababy.Random as Random
import Data.Array as Array
import Data.Foldable (all, foldl)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Number as Number
import Data.Tuple (Tuple(..))

-- | A skill is unlocked exactly when it has an entry in `skills`.
type Progress =
  { skills :: Map SkillId SkillStats
  , rounds :: Int
  , lastSkill :: Maybe SkillId
  , stars :: Int
  , stickers :: Array String
  }

-- | `mastery` is how well she recognises the item. `writing` is how well
-- | she writes it, kept apart so that practising writing doesn't hold back
-- | recognition, or the reverse.
type SkillStats =
  { mastery :: Number
  , rounds :: Int
  , lastRound :: Int
  , writing :: Number
  }

data Status
  = Locked
  | Learning SkillStats
  | Mastered SkillStats

type RoundResult =
  { skill :: SkillId
  , found :: Int
  , mistakes :: Int
  , hinted :: Boolean
  }

-- | `score` is in `[0, 1]`; `traced` is true when she traced over a guide
-- | rather than writing freehand.
type WritingResult =
  { skill :: SkillId
  , score :: Number
  , traced :: Boolean
  , firstTry :: Boolean
  }

emptyProgress :: Progress
emptyProgress =
  { skills: Map.empty
  , rounds: 0
  , lastSkill: Nothing
  , stars: 0
  , stickers: []
  }

masteryThreshold :: Number
masteryThreshold = 0.75

-- | How many not-yet-mastered skills may be in rotation at once. New
-- | material is only introduced when fewer than this are being learned.
learningCap :: Int
learningCap = 4

statsFor :: SkillId -> Progress -> Maybe SkillStats
statsFor id progress = Map.lookup id progress.skills

status :: Progress -> SkillId -> Status
status progress id = case statsFor id progress of
  Nothing -> Locked
  Just stats
    | stats.mastery >= masteryThreshold -> Mastered stats
    | otherwise -> Learning stats

isMastered :: Progress -> SkillId -> Boolean
isMastered progress id = case status progress id of
  Mastered _ -> true
  _ -> false

isLearning :: Progress -> SkillId -> Boolean
isLearning progress id = case status progress id of
  Learning _ -> true
  _ -> false

-- | Unlocks the earliest eligible skills while fewer than `learningCap` are
-- | being learned. `skills` are the enabled skills in curriculum order. A
-- | prerequisite that is not among them (disabled or unknown) counts as met.
refreshUnlocks :: Array Skill -> Progress -> Progress
refreshUnlocks skills progress = do
  let
    enabledIds = map _.id skills
    learning = Array.length (Array.filter (isLearning progress <<< _.id) skills)
    prerequisiteMet id = not (Array.elem id enabledIds) || isMastered progress id
    eligible skill = not (Map.member skill.id progress.skills) && all prerequisiteMet skill.requires
    newcomers = Array.take (learningCap - learning) (Array.filter eligible skills)
    fresh = { mastery: 0.0, rounds: 0, lastRound: progress.rounds, writing: 0.0 }
  progress { skills = foldl (\m s -> Map.insert s.id fresh m) progress.skills newcomers }

-- | Picks the next skill to practise among unlocked, enabled skills.
-- | Skills still being learned get most of the rounds; mastered ones come
-- | back for review, more often the longer since they were last seen.
chooseSkill :: Array Skill -> Progress -> Gen (Maybe Skill)
chooseSkill skills progress = do
  let
    unlocked = Array.mapMaybe (\s -> map (Tuple s) (statsFor s.id progress)) skills
    notRepeated = Array.filter (\(Tuple s _) -> Just s.id /= progress.lastSkill) unlocked
    pool = if Array.null notRepeated then unlocked else notRepeated
    learning = Array.filter (\(Tuple _ st) -> st.mastery < masteryThreshold) pool
    review = Array.filter (\(Tuple _ st) -> st.mastery >= masteryThreshold) pool
    learningWeight (Tuple s st) = Tuple (if st.rounds == 0 then 6.0 else 3.0 + (1.0 - st.mastery)) s
    reviewWeight (Tuple s st) = do
      let
        since = Int.toNumber (progress.rounds - st.lastRound)
      Tuple ((1.2 - st.mastery) * (1.0 + Number.min 3.0 (since / 8.0))) s
  preferLearning <- Random.chance 0.7
  case Array.null learning, Array.null review of
    true, true -> pure Nothing
    false, true -> Random.weighted (map learningWeight learning)
    true, false -> Random.weighted (map reviewWeight review)
    false, false
      | preferLearning -> Random.weighted (map learningWeight learning)
      | otherwise -> Random.weighted (map reviewWeight review)

-- | A round's score in `[0, 1]`. Needing a hint caps the score so that a
-- | round finished only with help does not count toward mastery.
roundScore :: RoundResult -> Number
roundScore r = do
  let
    total = r.found + r.mistakes
    accuracy = if total == 0 then 0.0 else Int.toNumber r.found / Int.toNumber total
  if r.hinted then Number.min 0.5 accuracy else accuracy

-- | A first round played perfectly marks the skill as mastered straight
-- | away, so that things she already knows are not drilled.
recordRound :: RoundResult -> String -> Progress -> Progress
recordRound result sticker progress = do
  let
    score = roundScore result
    update stats
      | stats.rounds == 0 = stats { mastery = score * 0.8, rounds = 1, lastRound = progress.rounds }
      | otherwise = stats { mastery = stats.mastery + 0.5 * (score - stats.mastery), rounds = stats.rounds + 1, lastRound = progress.rounds }
    base = maybe { mastery: 0.0, rounds: 0, lastRound: progress.rounds, writing: 0.0 } identity (statsFor result.skill progress)
  progress
    { skills = Map.insert result.skill (update base) progress.skills
    , rounds = progress.rounds + 1
    , lastSkill = Just result.skill
    , stars = progress.stars + (if result.mistakes == 0 && not result.hinted then 2 else 1)
    , stickers = Array.snoc progress.stickers sticker
    }

-- | Tracing can only take writing so far: it counts for less than
-- | writing freehand, so that writing freehand follows two or so good
-- | traces.
tracingCredit :: Number
tracingCredit = 0.7

-- | Writing well enough to be asked to write freehand.
canWrite :: SkillStats -> Boolean
canWrite stats = stats.writing >= 0.5

recordWriting :: WritingResult -> String -> Progress -> Progress
recordWriting result sticker progress = do
  let
    earned = if result.traced then tracingCredit * result.score else result.score
    update stats = stats { writing = stats.writing + 0.5 * (earned - stats.writing), lastRound = progress.rounds }
  progress
    { skills = Map.update (Just <<< update) result.skill progress.skills
    , rounds = progress.rounds + 1
    , lastSkill = Just result.skill
    , stars = progress.stars + (if result.firstTry then 2 else 1)
    , stickers = Array.snoc progress.stickers sticker
    }

recordParty :: String -> Progress -> Progress
recordParty sticker progress = progress
  { rounds = progress.rounds + 1
  , stars = progress.stars + 1
  , stickers = Array.snoc progress.stickers sticker
  }

-- | Every fifth round is a party round with nothing to get wrong.
isPartyRound :: Progress -> Boolean
isPartyRound progress = progress.rounds `mod` 10 == 9

markLearned :: Group -> Array Skill -> Progress -> Progress
markLearned group skills progress = do
  let
    mark m skill = Map.alter (Just <<< maybe (learned progress.rounds) (\st -> st { mastery = Number.max st.mastery 0.8 })) skill.id m
  progress { skills = foldl mark progress.skills (Array.filter ((_ == group) <<< _.group) skills) }
  where
  learned round = { mastery: 0.8, rounds: 1, lastRound: round, writing: 0.0 }

forgetGroup :: Group -> Array Skill -> Progress -> Progress
forgetGroup group skills progress =
  progress { skills = foldl (flip Map.delete) progress.skills (map _.id (Array.filter ((_ == group) <<< _.group) skills)) }
