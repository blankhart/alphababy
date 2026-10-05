module Alphababy.UI.Face
  ( face
  ) where

import Prelude

import Alphababy.Content (Face(..))
import Data.Array as Array
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | A ball label as HTML, for hints and the progress panel.
face :: forall w i. Face -> HH.HTML w i
face = case _ of
  Glyphs segments ->
    HH.span [ HP.class_ (H.ClassName "face glyphs") ]
      (map (\s -> HH.span [ HP.class_ (H.ClassName (if s.emphasis then "em" else "plain")) ] [ HH.text s.text ]) segments)
  Picture emoji -> HH.span [ HP.class_ (H.ClassName "face picture") ] [ HH.text emoji ]
  Pips n -> HH.span [ HP.class_ (H.ClassName "face pips") ] [ HH.text (Array.fold (Array.replicate n "⭐")) ]
