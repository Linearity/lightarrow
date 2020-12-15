{-|

A cue is a message that identifies an event. We define some abstract functions
of cues.

-}
module Simulation.Lightarrow.Cue where

import Control.Applicative
import FRP.BearRiver
import Simulation.Lightarrow.Event()

-- | The appearance of some entity in the scene
class Entrance a s where
    -- | Cue an entrance in a given state
    cueEnter    :: s -> a
    -- | React to an entrance in a given state
    reactEnter  :: (s -> b) -> a -> Event b

instance Entrance a s => Entrance (Event a) s where
    cueEnter a              = Event (cueEnter a)
    reactEnter _f NoEvent   = NoEvent
    reactEnter f (Event a)  = reactEnter f a

-- | The vanishing of some entity from the scene
class Exit a k where
    -- | Cue the exit of the entity identified by a given key
    cueExit     :: k -> a
    -- | React to the exit of the entity identified by a given key
    reactExit   :: (k -> b) -> a -> Event b

-- | A discontinuous change in position
class Teleport a x where
    -- | Cue a teleportation to a given location
    cueTeleport     :: x -> a
    -- | React to a teleportation to a given location
    reactTeleport   :: (x -> b) -> a -> Event b

-- | Something being completed
class Done a k where
    -- | Cue the completion of something identified by a given key
    cueDone     :: k -> a
    -- | React to the completion of something identified by a given key
    reactDone   :: (k -> b) -> a -> Event b

-- | Cues that can be routed to certain entities
class Routable a k where
    -- | Get the recipient of the cue
    routing :: a -> Maybe k

{-|
React to a cue in one of several ways.  Try each reaction in order, and produce
the first positive result.
-}
reactWith :: [a -> Event b] -- ^ possible reactions
                -> a        -- ^ cue
                -> Event b  -- ^ first positive result, negative if none
reactWith rs a = foldl (<|>) NoEvent (fmap ($ a) rs)