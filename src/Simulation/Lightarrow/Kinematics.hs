{-|

Kinematics is the study of motion. It is the part of classical mechanics that
relates location, velocity, and acceleration, both linear and rotational. It
does not consider mass, momentum, or force.

-}
module Simulation.Lightarrow.Kinematics where

import Data.Lightarrow.Vector
import Data.AffineSpace
import Data.VectorSpace
import FRP.BearRiver

{-|

Integrate a velocity via the forward Euler method to determine a location,
with a given initial location.

-}
euler :: (Monad m, AffineSpace p v s) => p -> SF m v p
euler x0 = integral >>> arr (x0 .+^)
{-

Note that we don't use |integralFrom| here.  That is because |x0| is a point,
not a vector, and |integralFrom| only works with a vector offset.

-}

{-|

Integrate both acceleration and velocity via the forward Euler method to
determine a location and velocity, starting from an initial location and velocity.

-}
euler2 :: (Monad m, AffineSpace p v s) => (p, v) -> SF m v (p, v)
euler2 (x0, v0) = integralFrom v0 >>> (euler x0 &&& identity)

{-|

A straightforward constraint on velocity such that its magnitude is less than
or equal to a maximum speed

-}
speedLimit :: (Ord a, VectorSpace v a) => a -> v -> v
speedLimit = clampLength 0

{-|

Attenuate acceleration before a speed limit is exceeded.  When the speed `s` is
more than three quarters of the maximum speed, we subtract a proportion `k` of
the component of acceleration parallel to the velocity, where `k` is equal to the
proportion of the last quarter of maximum speed `s` has reached.

-}
govern :: (Ord a, VectorSpace v a) => a -> v -> v -> v
govern s v a = a ^-^ k *^ forward v a
    where   k   = min 1 (max 0 (norm v - (3/4 * s)) / (s / 4))