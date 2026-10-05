module Alphababy.Settings.Codec
  ( Saved
  , currentVersion
  , encode
  , decode
  ) where

import Prelude

import Alphababy.Content (SkillId(..))
import Alphababy.Content as Content
import Alphababy.Progress (Progress, SkillStats)
import Alphababy.Settings (Settings)
import Alphababy.Settings as Settings
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson, (.:))
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

type Saved = { settings :: Settings, progress :: Progress }

-- | The persisted shape. Changing it requires a new version and a
-- | migration in `decode`.
type Stored settings stats =
  { version :: Int
  , settings :: settings
  , progress ::
      { skills :: Object stats
      , rounds :: Int
      , lastSkill :: Maybe String
      , stars :: Int
      , stickers :: Array String
      }
  }

type SettingsV1 =
  { childName :: String
  , voiceUri :: Maybe String
  , speechRate :: Number
  , soundEffects :: Boolean
  , disabledGroups :: Array String
  , disabledSkills :: Array String
  , extraSightWords :: Array String
  }

-- | Version 2 adds `motionSpeed`.
type SettingsV2 =
  { childName :: String
  , voiceUri :: Maybe String
  , speechRate :: Number
  , soundEffects :: Boolean
  , motionSpeed :: Number
  , disabledGroups :: Array String
  , disabledSkills :: Array String
  , extraSightWords :: Array String
  }

-- | Version 3 adds `writing` to the settings and to each skill's stats.
type SettingsV3 =
  { childName :: String
  , voiceUri :: Maybe String
  , speechRate :: Number
  , soundEffects :: Boolean
  , motionSpeed :: Number
  , writing :: Boolean
  , disabledGroups :: Array String
  , disabledSkills :: Array String
  , extraSightWords :: Array String
  }

type SkillStatsV2 =
  { mastery :: Number
  , rounds :: Int
  , lastRound :: Int
  }

currentVersion :: Int
currentVersion = 3

encode :: Saved -> Json
encode { settings, progress } = encodeJson stored
  where
  stored :: Stored SettingsV3 SkillStats
  stored =
    { version: currentVersion
    , settings:
        { childName: settings.childName
        , voiceUri: settings.voiceUri
        , speechRate: settings.speechRate
        , soundEffects: settings.soundEffects
        , motionSpeed: settings.motionSpeed
        , writing: settings.writing
        , disabledGroups: map Content.groupCode (Set.toUnfoldable settings.disabledGroups)
        , disabledSkills: map (\(SkillId s) -> s) (Set.toUnfoldable settings.disabledSkills)
        , extraSightWords: settings.extraSightWords
        }
    , progress:
        { skills: Object.fromFoldable (map (\(Tuple (SkillId k) v) -> Tuple k v) (Map.toUnfoldable progress.skills :: Array _))
        , rounds: progress.rounds
        , lastSkill: map (\(SkillId s) -> s) progress.lastSkill
        , stars: progress.stars
        , stickers: progress.stickers
        }
    }

decode :: Json -> Either JsonDecodeError Saved
decode json = do
  obj <- decodeJson json
  version <- obj .: "version"
  stored <- case version of
    1 -> do
      v1 :: Stored SettingsV1 SkillStatsV2 <- decodeJson json
      pure (migrateV2 (v1 { settings = migrateV1 v1.settings }))
    2 -> do
      v2 :: Stored SettingsV2 SkillStatsV2 <- decodeJson json
      pure (migrateV2 v2)
    3 -> decodeJson json
    _ -> Left (UnexpectedValue json)
  pure
    { settings:
        { childName: stored.settings.childName
        , voiceUri: stored.settings.voiceUri
        , speechRate: stored.settings.speechRate
        , soundEffects: stored.settings.soundEffects
        , motionSpeed: stored.settings.motionSpeed
        , writing: stored.settings.writing
        , disabledGroups: Set.fromFoldable (Array.mapMaybe Content.groupFromCode stored.settings.disabledGroups)
        , disabledSkills: Set.fromFoldable (map SkillId stored.settings.disabledSkills)
        , extraSightWords: stored.settings.extraSightWords
        }
    , progress:
        { skills: Map.fromFoldable (map (\(Tuple k v) -> Tuple (SkillId k) v) (Object.toUnfoldable stored.progress.skills :: Array _))
        , rounds: stored.progress.rounds
        , lastSkill: map SkillId stored.progress.lastSkill
        , stars: stored.progress.stars
        , stickers: stored.progress.stickers
        }
    }

migrateV1 :: SettingsV1 -> SettingsV2
migrateV1 s =
  { childName: s.childName
  , voiceUri: s.voiceUri
  , speechRate: s.speechRate
  , soundEffects: s.soundEffects
  , motionSpeed: Settings.defaultSettings.motionSpeed
  , disabledGroups: s.disabledGroups
  , disabledSkills: s.disabledSkills
  , extraSightWords: s.extraSightWords
  }

migrateV2 :: Stored SettingsV2 SkillStatsV2 -> Stored SettingsV3 SkillStats
migrateV2 stored = stored
  { version = 3
  , settings =
      { childName: s.childName
      , voiceUri: s.voiceUri
      , speechRate: s.speechRate
      , soundEffects: s.soundEffects
      , motionSpeed: s.motionSpeed
      , writing: Settings.defaultSettings.writing
      , disabledGroups: s.disabledGroups
      , disabledSkills: s.disabledSkills
      , extraSightWords: s.extraSightWords
      }
  , progress = stored.progress { skills = map (\st -> { mastery: st.mastery, rounds: st.rounds, lastRound: st.lastRound, writing: 0.0 }) stored.progress.skills }
  }
  where
  s = stored.settings
