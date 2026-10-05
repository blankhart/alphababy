module Alphababy.Capability.Speech
  ( Voice
  , VoiceHandle
  , SpeechOptions
  , isAvailable
  , voices
  , speak
  , say
  , cancel
  , unlock
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Aff as Aff

foreign import data VoiceHandle :: Type

type Voice = { name :: String, lang :: String, uri :: String, local :: Boolean, handle :: VoiceHandle }

type SpeechOptions = { voice :: Maybe Voice, rate :: Number, pitch :: Number }

foreign import isAvailable :: Effect Boolean
foreign import getVoicesImpl :: (Array Voice -> Effect Unit) -> Effect (Effect Unit)
foreign import speakImpl
  :: { text :: String, voice :: Nullable Voice, rate :: Number, pitch :: Number, timeoutMs :: Number }
  -> Effect Unit
  -> Effect (Effect Unit)

foreign import cancel :: Effect Unit

-- | Call synchronously from a tap handler before relying on speech.
foreign import unlock :: Effect Unit

voices :: Aff (Array Voice)
voices = makeAff \k -> do
  stop <- getVoicesImpl (k <<< Right)
  pure (effectCanceler stop)

-- | Speaks `text`, interrupting anything already being said, and completes
-- | when the utterance ends. Killing the fiber stops the speech.
speak :: SpeechOptions -> String -> Aff Unit
speak options text = makeAff \k -> do
  let
    -- Generous upper bound on speaking time, for engines that never
    -- report the end of an utterance.
    timeoutMs = 2500.0 + Int.toNumber (String.length text) * 110.0 / options.rate
  stop <- speakImpl
    { text, voice: toNullable options.voice, rate: options.rate, pitch: options.pitch, timeoutMs }
    (k (Right unit))
  pure (effectCanceler stop)

-- | Fire-and-forget speech.
say :: SpeechOptions -> String -> Effect Unit
say options text = Aff.launchAff_ (speak options text)
