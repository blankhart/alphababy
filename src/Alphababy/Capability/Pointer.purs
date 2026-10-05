module Alphababy.Capability.Pointer
  ( pointerId
  , positions
  ) where

import Web.Event.Event (Event)

-- | Identifies the finger (or mouse or pen) behind a pointer event.
foreign import pointerId :: Event -> Int

-- | Where the pointer has been since the previous event, in CSS pixels
-- | relative to the viewport, oldest first.
foreign import positions :: Event -> Array { x :: Number, y :: Number }
