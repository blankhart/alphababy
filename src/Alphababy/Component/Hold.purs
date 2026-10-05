module Alphababy.Component.Hold
  ( Input
  , Output
  , Query
  , component
  ) where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Timer (clearTimeout, setTimeout)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Web.Event.Event (EventType(..))

-- | A button for grown-ups: it only fires after being held down, so a
-- | child tapping around does not leave the game or open settings.
type Input = { label :: String, title :: String }

type Output = Unit

data Query :: Type -> Type
data Query a

type State = { input :: Input, holding :: Maybe H.SubscriptionId }

data Action
  = Press
  | Release
  | Fire
  | Receive Input

holdMilliseconds :: Int
holdMilliseconds = 1500

-- | Firing from a subscription rather than a fork matters: the parent
-- | usually removes this button in response, which kills the child's forks,
-- | and a fork must not be killed while it is the one raising the output.
after :: Int -> HS.Emitter Unit
after ms = HS.makeEmitter \emit -> do
  timer <- setTimeout ms (emit unit)
  pure (clearTimeout timer)

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: \input -> { input, holding: Nothing }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, receive = Just <<< Receive }
    }
  where
  render state =
    HH.button
      [ HP.classes (map H.ClassName ([ "hold-button" ] <> if state.holding == Nothing then [] else [ "holding" ]))
      , HP.title state.input.title
      , HE.handler (EventType "pointerdown") (const Press)
      , HE.handler (EventType "pointerup") (const Release)
      , HE.handler (EventType "pointerleave") (const Release)
      , HE.handler (EventType "pointercancel") (const Release)
      , HE.handler (EventType "contextmenu") (const Release)
      ]
      [ HH.span [ HP.class_ (H.ClassName "hold-fill") ] []
      , HH.span [ HP.class_ (H.ClassName "hold-label") ] [ HH.text state.input.label ]
      ]

  handleAction = case _ of
    Press -> do
      release
      subscription <- H.subscribe (map (const Fire) (after holdMilliseconds))
      H.modify_ _ { holding = Just subscription }
    Release -> release
    Fire -> do
      release
      H.raise unit
    Receive input -> H.modify_ _ { input = input }

  release = do
    held <- H.gets _.holding
    for_ held H.unsubscribe
    H.modify_ _ { holding = Nothing }
