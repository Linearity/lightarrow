{-

A cue is a message that identifies an event. GUIs have common events that we
identify here.

-}
module Graphics.UI.Lightarrow.Cue where

import Simulation.Lightarrow.Cue

class Cue a => MouseEnter a where
    cueMouseEnter       :: a
    reactMouseEnter     :: a -> Maybe b

class Cue a => MouseExit a where
    cueMouseExit    :: a
    reactMouseExit  :: a -> Maybe b

class Cue a => MouseDown a where
    cueMouseDown    :: a
    reactMouseDown  :: a -> Maybe b


