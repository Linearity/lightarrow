{-

Kinematics is the study of motion. It is the part of classical mechanics that
relates location, velocity, and acceleration, both linear and rotational. It
does not consider mass, momentum, or force.

-}
module Simulation.Lightarrow.Kinematics where

import Data.Lightarrow.Vector
import Data.AffineSpace
import Data.VectorSpace
import FRP.BearRiver
{-

One of the most basic functions in kinematics is that giving the position of a
point with changing velocity. We can compute this function with the Euler
method of integration.

-}
euler :: (Monad m, AffineSpace p v s) => p -> SF m v p
euler x0 = integral >>> arr (x0 .+^)
{-

Another basic function gives the position of a point with both changing
velocity and changing acceleration. We can compute this by chaining two Euler
integrals together.

-}
euler2 :: (Monad m, AffineSpace p v s) => (p, v) -> SF m v (p, v)
euler2 (x0, v0) = integralFrom v0 >>> (euler x0 &&& identity)
{-

It is convenient to define a straightforward constraint on velocity such that
its magnitude is less than or equal to a maximum speed.

-}
speedLimit :: (Ord a, VectorSpace v a) => a -> v -> v
speedLimit = clampLength 0
{-

The governor is a little more subtle. It attenuates the acceleration before the
speed limit is exceeded. When the speed `s` is more than three quarters of the
maximum speed, we subtract a proportion `k` of the component of acceleration
parallel to the velocity, where `k` is equal to the proportion of the last
quarter of maximum speed `s` has reached.

-}
govern :: (Ord a, VectorSpace v a) => a -> v -> v -> v
govern s v a = a ^-^ k *^ forward v a
    where   k   = min 1 (max 0 (norm v - (3/4 * s)) / (s / 4))
