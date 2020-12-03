{-

A cue is a message that identifies an event. We define some abstract functions
of cues.

-}
module Simulation.Lightarrow.Cue where

import Control.Applicative
import FRP.BearRiver
import Simulation.Lightarrow.Event()

class Entrance a s where
    cueEnter    :: s -> a
    reactEnter  :: (s -> b) -> a -> Event b

instance Entrance a s => Entrance (Event a) s where
    cueEnter a              = Event (cueEnter a)
    reactEnter _f NoEvent   = NoEvent
    reactEnter f (Event a)  = reactEnter f a

class Exit a k where
    cueExit     :: k -> a
    reactExit   :: (k -> b) -> a -> Event b

class Collision a k where
    cueCollide    :: k -> k -> a
    reactCollide  :: (k -> k -> b) -> a -> Event b

class Teleport a x where
    cueTeleport     :: x -> a
    reactTeleport   :: (x -> b) -> a -> Event b

class Done a k where
    cueDone     :: k -> a
    reactDone   :: (k -> b) -> a -> Event b

class Routable a k where
    routing :: a -> Maybe k

reactWith :: [a -> Event b] -> a -> Event b
reactWith rs a = foldl (<|>) NoEvent (fmap ($ a) rs)