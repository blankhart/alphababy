module Alphababy.Capability.Screen
  ( requestFullscreen
  , exitFullscreen
  , keepAwake
  , allowSleep
  , fontsReady
  , devicePixelRatio
  , elementSize
  , animationFrames
  , elementResizes
  ) where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Now (now)
import Effect.Ref as Ref
import Halogen.Subscription as HS
import Web.HTML (window)
import Web.HTML.Window as Window

foreign import requestFullscreen :: Effect Unit
foreign import exitFullscreen :: Effect Unit
foreign import keepAwake :: Effect Unit
foreign import allowSleep :: Effect Unit
foreign import fontsReadyImpl :: Array String -> Effect Unit -> Effect Unit
foreign import devicePixelRatio :: Effect Number
-- | The element's laid-out size in CSS pixels, falling back to the
-- | viewport before it has been laid out.
foreign import elementSize :: forall element. element -> Effect { width :: Number, height :: Number }

foreign import observeResizeImpl :: forall element. element -> Effect Unit -> Effect (Effect Unit)

-- | Waits (at most a few seconds) for the given CSS font specs to load, so
-- | that canvas text is not first drawn in a fallback face.
fontsReady :: Array String -> Aff Unit
fontsReady fonts = makeAff \k -> do
  fontsReadyImpl fonts (k (Right unit))
  pure nonCanceler

-- | Emits the seconds elapsed since the previous animation frame.
animationFrames :: HS.Emitter Number
animationFrames = HS.makeEmitter \emit -> do
  running <- Ref.new true
  last <- Ref.new Nothing
  w <- window
  let
    loop = do
      active <- Ref.read running
      when active do
        Milliseconds t <- map unInstant now
        previous <- Ref.read last
        Ref.write (Just t) last
        case previous of
          Just p -> emit ((t - p) / 1000.0)
          Nothing -> pure unit
        void (Window.requestAnimationFrame loop w)
  void (Window.requestAnimationFrame loop w)
  pure (Ref.write false running)

-- | Fires whenever the element's size changes, which on mobile browsers
-- | can happen well after the window's resize event (toolbars sliding
-- | away, entering full screen).
elementResizes :: forall element. element -> HS.Emitter Unit
elementResizes el = HS.makeEmitter \emit -> observeResizeImpl el (emit unit)
