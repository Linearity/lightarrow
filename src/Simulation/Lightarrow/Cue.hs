{-# LANGUAGE UndecidableInstances #-}
{-|

A cue is a message that identifies an event. We define some abstract functions
of cues.

-}
module Simulation.Lightarrow.Cue where

import Control.Applicative
import Data.Foldable
import FRP.BearRiver
import Simulation.Lightarrow.Event

-- | The appearance of some entity in the scene
class Entrance s a where
    -- | Cue an entrance in a given state
    cueEnter    :: s -> a
    -- | React to an entrance in a given state
    reactEnter  :: (s -> b) -> a -> Event b

instance Entrance s a => Entrance s (Event a) where
    cueEnter a              = Event (cueEnter a)
    reactEnter _f NoEvent   = NoEvent
    reactEnter f (Event a)  = reactEnter f a

-- | The vanishing of some entity from the scene
class Exit k a where
    -- | Cue the exit of the entity identified by a given key
    cueExit     :: k -> a
    -- | React to the exit of the entity identified by a given key
    reactExit   :: (k -> b) -> a -> Event b

-- | A discontinuous change in position
class Teleport x a | a -> x where
    -- | Cue a teleportation to a given location
    cueTeleport     :: x -> a
    -- | React to a teleportation to a given location
    reactTeleport   :: (x -> b) -> a -> Event b

instance Teleport s a => Teleport s (Event a) where
    cueTeleport x              = Event (cueTeleport x)
    reactTeleport b (Event a)  = reactTeleport b a
    reactTeleport _b NoEvent   = NoEvent

instance Teleport s a => Teleport s [a] where
    cueTeleport x           = [cueTeleport x]
    reactTeleport f as      = mergeEvents (map (reactTeleport f) as)

-- | Something being completed
class Done a where
    -- | Cue the completion of something
    cueDone     :: a
    -- | React to the completion of something
    reactDone   :: b -> a -> Event b

instance Done a => Done (Event a) where
    cueDone                 = Event cueDone
    reactDone b (Event a)   = reactDone b a
    reactDone b NoEvent     = NoEvent

instance Done a => Done [a] where
    cueDone             = [cueDone]
    reactDone b as      = mergeEvents (map (reactDone b) as)

-- | Nothing happening; a non-cue
class NoCue a where
    -- | Signal nothing
    noCue       :: a
    -- | Acknowledge nothing
    reactNoCue  :: a -> Event ()

instance NoCue a => NoCue [a] where
    noCue               = [noCue]
    reactNoCue as       = mergeEvents (map reactNoCue as)

-- | Cues that can be routed to certain entities
class Routable k a where
    -- | Get the recipient of the cue
    routing :: a -> Maybe k

doReactWith :: Monad m => [a -> Event (m b)] -> m b -> a -> m b
doReactWith rs fallback = fromEvent fallback . reactWith rs

doReactWithAll :: (Foldable t, Functor t, Monad m) =>
                    [a -> Event (m b)]
                        -> m b
                        -> t a
                        -> m b
doReactWithAll rs fallback = fromEvent fallback . reactAll (reactWith rs)

{-|
React to a cue in one of several ways.  Try each reaction in order, and produce
the first positive result.
-}
reactWith :: [a -> Event b] -- ^ possible reactions
                -> a        -- ^ cue
                -> Event b  -- ^ first positive result, negative if none
reactWith rs a = foldl (<|>) NoEvent (fmap ($ a) rs)

reactAll :: (Foldable t, Functor t) => (a -> Event b) -> t a -> Event b
reactAll r = foldr lMerge NoEvent . fmap r