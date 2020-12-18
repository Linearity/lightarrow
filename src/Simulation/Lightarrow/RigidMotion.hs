{-

A rigid motion is a translation or rotation that occurs over time. We can
define some primitive motions, some means of combining them into more complex
motions, and some means of abstracting them over their details

-}
module Simulation.Lightarrow.RigidMotion where

import Control.Monad.Fix
import Data.Fixed
import FRP.BearRiver
import Linear
import Linear.Affine
import Simulation.Lightarrow.Task
{-

The most basic motion is linear translation, or `sliding`, from one point to
another. It is a point that varies linearly with time; when it is done it
remains at the last point forever.

-}

slide :: (Monad m, Affine p) =>
            p Time
                -> p Time
                -> Time
                -> Task a (p Time) m ()
slide p1 p2 dt = interval dt move
    where   move    = proc _ -> do
                        t      <-  time  -< ()
                        let k  = (dt - t) / dt
                            p  = p1 .+^ (1 - k) *^ (p2 .-. p1)
                        returnA -< p

rotation :: (MonadFix m, RealFloat a) => SF m a a
rotation   = proc ω  -> do
                t   <-  time    -< ()
                returnA -< realToFrac t * ω  `mod'` (2 * pi)
