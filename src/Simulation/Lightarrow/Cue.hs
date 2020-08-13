{-

A cue is a message that identifies an event. We define some abstract functions
of cues.

-}
module Simulation.Lightarrow.Cue where

import Control.Applicative
import Data.Maybe
import FRP.BearRiver

class Entrance a s where
    cueEnter    :: s -> a
    reactEnter  :: (s -> b) -> a -> Maybe b

class Exit a k where
    cueExit     :: k -> a
    reactExit   :: (k -> b) -> a -> Maybe b

class Collision a k where
    cueCollide    :: k -> k -> a
    reactCollide  :: (k -> k -> b) -> a -> Maybe b

class Teleport a x where
    cueTeleport     :: x -> a
    reactTeleport   :: (x -> b) -> a -> Maybe b

class Done a k where
    cueDone     :: k -> a
    reactDone   :: (k -> b) -> a -> Maybe b

class Cue a where
    type CueMessage a
    fromMessage :: CueMessage a -> a
    toMessage :: a -> CueMessage a

class Cue a => Routable a k where
    routing :: a -> Maybe k

fromCue :: (Cue a, Cue b, CueMessage a ~ CueMessage b) => a -> b
fromCue = fromMessage . toMessage



react :: [a -> Maybe b] -> a -> Maybe b
react rs a = foldl (<|>) Nothing (fmap ($ a) rs)

select :: b -> [a -> Maybe b] -> a -> b
select b rs a = maybe b id (react rs a)

reactAll :: [a -> Maybe b] -> [a] -> [b]
reactAll rs as = catMaybes (fmap (\a -> react rs a) as)

reactSome :: [a -> Maybe b] -> [a] -> Maybe [b]
reactSome rs as = case reactAll rs as of
                        []  -> Nothing
                        bs  -> Just bs
