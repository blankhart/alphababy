module Alphababy.Random
  ( Seed
  , Gen
  , mkSeed
  , seedToInt
  , runGen
  , evalGen
  , float
  , range
  , int
  , chance
  , pick
  , pickOr
  , shuffle
  , weighted
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Tuple (Tuple(..), fst, snd)

-- | Park-Miller minimal standard generator state. Kept strictly inside
-- | `[1, 2^31 - 2]` so the multiplication stays exact in a double.
newtype Seed = Seed Number

modulus :: Number
modulus = 2147483647.0

mkSeed :: Int -> Seed
mkSeed n = do
  let
    s = Number.abs (Int.toNumber n) Number.% (modulus - 1.0)
  Seed (if s < 1.0 then 1.0 else s)

seedToInt :: Seed -> Int
seedToInt (Seed s) = fromMaybe 1 (Int.fromNumber s)

next :: Seed -> Seed
next (Seed s) = Seed ((s * 48271.0) Number.% modulus)

newtype Gen a = Gen (Seed -> Tuple a Seed)

instance Functor Gen where
  map f (Gen g) = Gen \s -> case g s of
    Tuple a s' -> Tuple (f a) s'

instance Apply Gen where
  apply = ap

instance Applicative Gen where
  pure a = Gen (Tuple a)

instance Bind Gen where
  bind (Gen g) k = Gen \s -> case g s of
    Tuple a s' -> case k a of
      Gen h -> h s'

instance Monad Gen

runGen :: forall a. Gen a -> Seed -> { value :: a, seed :: Seed }
runGen (Gen g) seed = do
  let
    result = g seed
  { value: fst result, seed: snd result }

evalGen :: forall a. Gen a -> Seed -> a
evalGen g = _.value <<< runGen g

-- | Uniform in `[0, 1)`.
float :: Gen Number
float = Gen \s -> do
  let
    s'@(Seed n) = next s
  Tuple ((n - 1.0) / (modulus - 1.0)) s'

-- | Uniform in `[lo, hi)`.
range :: Number -> Number -> Gen Number
range lo hi = map (\u -> lo + u * (hi - lo)) float

-- | Uniform in `[lo, hi]`.
int :: Int -> Int -> Gen Int
int lo hi
  | hi <= lo = pure lo
  | otherwise = do
      u <- float
      pure (min hi (lo + Int.floor (u * Int.toNumber (hi - lo + 1))))

chance :: Number -> Gen Boolean
chance p = map (_ < p) float

pick :: forall a. NonEmptyArray a -> Gen a
pick xs = do
  i <- int 0 (NonEmptyArray.length xs - 1)
  pure (fromMaybe (NonEmptyArray.head xs) (NonEmptyArray.index xs i))

pickOr :: forall a. a -> Array a -> Gen a
pickOr fallback xs = case NonEmptyArray.fromArray xs of
  Just nel -> pick nel
  Nothing -> pure fallback

shuffle :: forall a. Array a -> Gen (Array a)
shuffle xs = do
  keys <- traverseGen (const float) xs
  pure (map snd (Array.sortWith fst (Array.zip keys xs)))
  where
  traverseGen f = Array.foldM (\acc x -> map (Array.snoc acc) (f x)) []

-- | Chooses an element with probability proportional to its non-negative
-- | weight. Returns `Nothing` when every weight is zero.
weighted :: forall a. Array (Tuple Number a) -> Gen (Maybe a)
weighted choices = do
  let
    positive = Array.filter ((_ > 0.0) <<< fst) choices
    total = sum (map fst positive)
  if total <= 0.0 then pure Nothing
  else do
    u <- range 0.0 total
    pure (walk u positive)
  where
  walk u xs = case Array.uncons xs of
    Nothing -> Nothing
    Just { head: Tuple w a, tail }
      | u < w || Array.null tail -> Just a
      | otherwise -> walk (u - w) tail
