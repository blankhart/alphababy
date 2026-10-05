module Alphababy.UI.Progress
  ( Rest
  , panel
  ) where

import Prelude

import Alphababy.Content (Skill, SkillId)
import Alphababy.Content as Content
import Alphababy.Progress (Progress, Status(..))
import Alphababy.Progress as Progress
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | What happened in the round just finished.
type Rest =
  { sticker :: String
  , earned :: Int
  , practiced :: Maybe SkillId
  , nowMastered :: Boolean
  }

type PanelInput =
  { rest :: Rest
  , progress :: Progress
  , skills :: Array Skill
  }

-- | The between-rounds screen: the new sticker, the sticker book, and the
-- | things she knows, as a row of gems per group.
panel :: forall w i. PanelInput -> i -> HH.HTML w i
panel { rest, progress, skills } onPlay =
  HH.div
    [ HP.class_ (H.ClassName "rest") ]
    [ HH.div [ HP.class_ (H.ClassName "rest-reward") ]
        [ HH.div [ HP.class_ (H.ClassName "new-sticker") ] [ HH.text rest.sticker ]
        , HH.div [ HP.class_ (H.ClassName "earned") ]
            [ HH.text ("+" <> show rest.earned <> " ⭐") ]
        ]
    , HH.button
        [ HP.class_ (H.ClassName "play-button rainbow")
        , HE.onClick \_ -> onPlay
        ]
        [ HH.text "▶" ]
    , HH.div [ HP.class_ (H.ClassName "star-total") ]
        [ HH.text ("⭐ " <> show progress.stars) ]
    , HH.div [ HP.class_ (H.ClassName "sticker-book") ]
        (map sticker (Array.takeEnd 40 progress.stickers))
    , HH.div [ HP.class_ (H.ClassName "know-list") ]
        (Array.mapMaybe groupRow Content.allGroups)
    ]
  where
  sticker s = HH.span [ HP.class_ (H.ClassName "sticker") ] [ HH.text s ]

  groupRow group = do
    let
      unlocked = Array.filter (\s -> s.group == group && isUnlocked (Progress.status progress s.id)) skills
    if Array.null unlocked then Nothing
    else Just $ HH.div [ HP.class_ (H.ClassName "know-group") ]
      [ HH.span [ HP.class_ (H.ClassName "know-badge"), HP.title (Content.groupTitle group) ] [ HH.text (Content.groupBadge group) ]
      , HH.div [ HP.class_ (H.ClassName "know-chips") ] (map chip unlocked)
      ]

  chip skill = do
    let
      state = case Progress.status progress skill.id of
        Mastered _ -> "mastered"
        Learning st | st.rounds == 0 -> "new"
        _ -> "learning"
      current = if Just skill.id == rest.practiced then [ "current" ] else []
      writes = case Progress.statsFor skill.id progress of
        Just st | Progress.canWrite st -> [ "writes" ]
        _ -> []
    HH.span
      [ HP.classes (map H.ClassName ([ "chip", state ] <> current <> writes)) ]
      [ HH.text skill.name ]

  isUnlocked = case _ of
    Locked -> false
    _ -> true
