module Alphababy.Settings
  ( Settings
  , defaultSettings
  , curriculumOptions
  , enabledSkills
  , isGroupEnabled
  , isSkillEnabled
  , toggleGroup
  , toggleSkill
  ) where

import Prelude

import Alphababy.Content (CurriculumOptions, Group, Skill, SkillId)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set

type Settings =
  { childName :: String
  , voiceUri :: Maybe String
  , speechRate :: Number
  , soundEffects :: Boolean
  , motionSpeed :: Number
  , writing :: Boolean
  , disabledGroups :: Set Group
  , disabledSkills :: Set SkillId
  , extraSightWords :: Array String
  }

defaultSettings :: Settings
defaultSettings =
  { childName: ""
  , voiceUri: Nothing
  , speechRate: 0.9
  , soundEffects: true
  , motionSpeed: 1.0
  , writing: true
  , disabledGroups: Set.empty
  , disabledSkills: Set.empty
  , extraSightWords: []
  }

curriculumOptions :: Settings -> CurriculumOptions
curriculumOptions s = { childName: s.childName, extraSightWords: s.extraSightWords }

isGroupEnabled :: Settings -> Group -> Boolean
isGroupEnabled s g = not (Set.member g s.disabledGroups)

isSkillEnabled :: Settings -> Skill -> Boolean
isSkillEnabled s skill = isGroupEnabled s skill.group && not (Set.member skill.id s.disabledSkills)

enabledSkills :: Settings -> Array Skill -> Array Skill
enabledSkills s = Array.filter (isSkillEnabled s)

toggleGroup :: Group -> Settings -> Settings
toggleGroup g s = s { disabledGroups = toggle g s.disabledGroups }

toggleSkill :: SkillId -> Settings -> Settings
toggleSkill id s = s { disabledSkills = toggle id s.disabledSkills }

toggle :: forall a. Ord a => a -> Set a -> Set a
toggle a set
  | Set.member a set = Set.delete a set
  | otherwise = Set.insert a set
