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

-- | Over a given interval, put out a point that slides in a straight line from
-- one point to another.
slide :: (Monad m, Affine p) =>
            p Time                          -- ^ initial point
                -> p Time                   -- ^ final point
                -> Time                     -- ^ interval
                -> Task a (p Time) m ()     -- ^ sliding activity
slide p1 p2 dt = interval dt move
    where   move    = proc _ -> do
                        t      <-  time  -< ()
                        let k  = (dt - t) / dt
                            p  = p1 .+^ (1 - k) *^ (p2 .-. p1)
                        returnA -< p

-- | A signal function whose input signal is an angular velocity in radians
-- per second and whose output signal is the integrated angle, starting at 0
-- and never exceeding 2π.
rotation :: (MonadFix m, RealFloat a) => SF m a a
rotation   = proc ω  -> do
                t   <-  time    -< ()
                returnA -< realToFrac t * ω  `mod'` (2 * pi)
