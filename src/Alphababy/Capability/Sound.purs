module Alphababy.Capability.Sound
  ( Audio
  , Wave(..)
  , Note
  , create
  , resume
  , play
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

foreign import data Audio :: Type

data Wave
  = Sine
  | Triangle
  | Square

-- | `start` and `duration` are in seconds from now; `slideTo` of 0 means a
-- | steady pitch.
type Note =
  { frequency :: Number
  , slideTo :: Number
  , start :: Number
  , duration :: Number
  , gain :: Number
  , wave :: Wave
  }

foreign import createImpl :: Effect (Nullable Audio)
foreign import resume :: Audio -> Effect Unit
foreign import playImpl :: Audio -> Array { frequency :: Number, slideTo :: Number, start :: Number, duration :: Number, gain :: Number, wave :: String } -> Effect Unit

-- | Must first be resumed from a user gesture before anything is audible.
create :: Effect (Maybe Audio)
create = map toMaybe createImpl

play :: Audio -> Array Note -> Effect Unit
play audio = playImpl audio <<< map (\n -> n { wave = waveName n.wave })
  where
  waveName = case _ of
    Sine -> "sine"
    Triangle -> "triangle"
    Square -> "square"
