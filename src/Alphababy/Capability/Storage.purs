module Alphababy.Capability.Storage
  ( load
  , save
  ) where

import Prelude

import Alphababy.Settings.Codec (Saved)
import Alphababy.Settings.Codec as Codec
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (hush)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Exception (try)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage as Storage

storageKey :: String
storageKey = "alphababy"

-- | `Nothing` when nothing was saved, storage is unavailable (private
-- | browsing), or the saved data cannot be decoded.
load :: Effect (Maybe Saved)
load = map join $ map hush $ try do
  raw <- Storage.getItem storageKey =<< localStorage =<< window
  pure (raw >>= (hush <<< jsonParser) >>= (hush <<< Codec.decode))

save :: Saved -> Effect Unit
save saved = void $ try do
  Storage.setItem storageKey (stringify (Codec.encode saved)) =<< localStorage =<< window
