module Alphababy.Palette
  ( RGB
  , Swatch
  , swatches
  , swatch
  , angry
  , sparkles
  , mix
  , css
  , cssAlpha
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Number as Number

type RGB = { r :: Number, g :: Number, b :: Number }

-- | A ball color: `light` is the glossy highlight, `dark` the shaded rim.
type Swatch = { light :: RGB, base :: RGB, dark :: RGB }

swatches :: NonEmptyArray Swatch
swatches = NonEmptyArray.cons'
  (sw 0xffc2e2 0xff6fb5 0xc2185b)
  [ sw 0xe9d0ff 0xb772ff 0x6a1fb0
  , sw 0xffd6f5 0xf15bd8 0x9c1a86
  , sw 0xd9c8ff 0x8e6cff 0x4b2aa8
  , sw 0xffe0ea 0xff8fab 0xc0405f
  , sw 0xf3d9ff 0xd98cff 0x8a3ab9
  , sw 0xcdeeff 0x6ec6ff 0x2a6bb0
  , sw 0xfff1c2 0xffc83d 0xc07a00
  ]

swatch :: Int -> Swatch
swatch i = fromMaybe (NonEmptyArray.head swatches) (NonEmptyArray.index swatches (i `mod` NonEmptyArray.length swatches))

angry :: Swatch
angry = sw 0xff9d7a 0xf0263c 0x7a0018

sparkles :: NonEmptyArray String
sparkles = NonEmptyArray.cons' "#ffffff" [ "#fff3a6", "#ffd1f0", "#ff7ad9", "#d9a6ff", "#b388ff", "#ffe066", "#a6f0ff" ]

sw :: Int -> Int -> Int -> Swatch
sw light base dark = { light: hex light, base: hex base, dark: hex dark }

hex :: Int -> RGB
hex n =
  { r: Int.toNumber ((n / 65536) `mod` 256)
  , g: Int.toNumber ((n / 256) `mod` 256)
  , b: Int.toNumber (n `mod` 256)
  }

-- | Linear interpolation: `mix 0.0 a b == a`.
mix :: Number -> RGB -> RGB -> RGB
mix t a b = do
  let
    u = Number.max 0.0 (Number.min 1.0 t)
    lerp x y = x + (y - x) * u
  { r: lerp a.r b.r, g: lerp a.g b.g, b: lerp a.b b.b }

css :: RGB -> String
css c = "rgb(" <> channel c.r <> "," <> channel c.g <> "," <> channel c.b <> ")"

cssAlpha :: Number -> RGB -> String
cssAlpha a c = "rgba(" <> channel c.r <> "," <> channel c.g <> "," <> channel c.b <> "," <> show (Number.max 0.0 (Number.min 1.0 a)) <> ")"

channel :: Number -> String
channel x = show (Int.round (Number.max 0.0 (Number.min 255.0 x)))
