module Simulation.Lightarrow.Collision where

import Data.Lightarrow.IdentityList hiding (filter)
import FRP.BearRiver

-- | Directed collision between two entities identified by the given types
class Collision a k1 k2 where
    -- | Indicate the collision of one entity into another
    cueCollide      :: k1 -> k2 -> a

    -- | React to the collision of one entity into another
    reactCollide    :: (k1 -> k2 -> b) -> a -> Event b

-- | Detect collisions between two identity lists
collisions :: Collision c Int Int =>
                (a -> b -> Bool)    -- ^ whether two elements are colliding
                    -> IL a         -- ^ first list
                    -> IL b         -- ^ second list
                    -> [c]          -- ^ collision cues
collisions f xs ys = respond <$> filter detect xys
    where   xys                 = [(x, y, kX, kY) | (kX, x) <- assocs xs,
                                                    (kY, y) <- assocs ys]
            detect (x,y,_,_)    = f x y
            respond (_,_,kX,kY) = cueCollide kX kY
