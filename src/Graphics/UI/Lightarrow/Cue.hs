{-

A cue is a message that identifies an event. GUIs have common events that we
identify here.

-}
module Graphics.UI.Lightarrow.Cue where

import FRP.BearRiver

class MouseEnter a where
    cueMouseEnter       :: a
    reactMouseEnter     :: a -> Event b

class MouseExit a where
    cueMouseExit    :: a
    reactMouseExit  :: a -> Event b

class MouseDown a where
    cueMouseDown    :: a
    reactMouseDown  :: a -> Event b


