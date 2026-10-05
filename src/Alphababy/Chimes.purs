module Alphababy.Chimes
  ( found
  , mistake
  , fanfare
  , button
  ) where

import Prelude

import Alphababy.Capability.Sound (Note, Wave(..))
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Number as Number

-- | Semitones above A4.
pitch :: Int -> Number
pitch semitones = 440.0 * Number.pow 2.0 (Int.toNumber semitones / 12.0)

-- | Major pentatonic steps, so any sequence of pops sounds pleasant.
pentatonic :: Int -> Int
pentatonic i = do
  let
    steps = [ 0, 2, 4, 7, 9 ]
    octave = i / 5
  12 * octave + fromMaybe 0 (Array.index steps (i `mod` 5))

note :: Wave -> Number -> Number -> Number -> Int -> Note
note wave start duration gain semis = { frequency: pitch semis, slideTo: 0.0, start, duration, gain, wave }

-- | The `n`th target found this round: each pop is a little higher.
found :: Int -> Array Note
found n = do
  let
    root = 3 + pentatonic (min n 9)
  [ { frequency: pitch (root - 12), slideTo: pitch (root + 12), start: 0.0, duration: 0.12, gain: 0.35, wave: Sine }
  , note Triangle 0.03 0.25 0.3 (root + 12)
  , note Sine 0.09 0.3 0.22 (root + 16)
  , note Sine 0.15 0.45 0.18 (root + 19)
  , note Sine 0.21 0.6 0.12 (root + 24)
  ]

mistake :: Array Note
mistake =
  [ { frequency: pitch (-12), slideTo: pitch (-19), start: 0.0, duration: 0.28, gain: 0.35, wave: Triangle }
  , { frequency: pitch (-10), slideTo: pitch (-17), start: 0.12, duration: 0.3, gain: 0.25, wave: Triangle }
  ]

fanfare :: Array Note
fanfare =
  [ note Triangle 0.0 0.2 0.3 3
  , note Triangle 0.12 0.2 0.3 7
  , note Triangle 0.24 0.2 0.3 10
  , note Triangle 0.36 0.9 0.3 15
  , note Sine 0.36 0.9 0.2 19
  , note Sine 0.36 0.9 0.2 22
  , note Sine 0.5 0.8 0.12 27
  ]

button :: Array Note
button = [ note Sine 0.0 0.15 0.25 15, note Sine 0.06 0.2 0.2 22 ]
