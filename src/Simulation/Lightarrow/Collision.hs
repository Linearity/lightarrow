module Simulation.Lightarrow.Collision where

import Data.Lightarrow.IdentityList hiding (filter)
import Data.List
import FRP.BearRiver
import Simulation.Lightarrow.Cue

collisions :: Collision c Int
                => (a -> b -> Bool) -> IL a -> IL b -> [c]
collisions f xs ys = respond <$> filter detect xys
    where   xys                 = [(x, y, kX, kY) | (kX, x) <- assocs xs,
                                                    (kY, y) <- assocs ys]
            detect (x,y,_,_)    = f x y
            respond (_,_,kX,kY) = cueCollide kX kY
