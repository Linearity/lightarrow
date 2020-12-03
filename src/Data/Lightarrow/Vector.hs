module Data.Lightarrow.Vector where

import           Control.Applicative
import           Data.VectorSpace (VectorSpace, zeroVector, normalize, dot, (*^))
import qualified Data.VectorSpace as VS
import           Linear (Conjugate, Epsilon, Quaternion, V2(..), V3(..), (^-^))
import qualified Linear as L
import           Linear.Affine (Affine, Diff, Point, (.-.))

clampLength :: (Ord a, VectorSpace v a) => a -> a -> v -> v
clampLength short long v    | m > long      = (long / m) *^ v
                            | m < short     = (short / m) *^ v
                            | otherwise     = v
    where   m = VS.norm v

pointInBox2 :: (Epsilon a, Floating a, Ord a)
                => (Point V2 a, V2 a) -> a -> Point V2 a -> Bool
pointInBox2 (center, V2 dx dy) r p = L.norm x < dx/2 && L.norm y < dy/2
    where   dp  = p .-. center
            x   = L.project (L.angle r) dp
            y   = dp ^-^ x

pointInBox3 :: (Affine p, Conjugate a, RealFloat a, Linear.Affine.Diff p ~ V3)
                => (p a, V3 a) -> Quaternion a -> p a -> Bool 
pointInBox3 (c, d) r = pointInBox (c, d) (L.rotate r)

pointInBox :: (Affine p, Foldable t, Applicative t, Ord a1, Fractional a1, Num a2)
                => (p a2, t a1) -> (Diff p a2 -> t a1) -> p a2 -> Bool
pointInBox (center, dimensions) r p
        = and (liftA2 ((<=) . abs) (r dp) (fmap (/2) dimensions))
    where   dp  = p .-. center
{-

The forward component of one vector with respect to another is like the
projection of the first on the second. However, if their dot product is
negative, we define the forward component to be zero.

-}
forward :: (Ord a, VectorSpace v a) => v -> v -> v
forward to from
    | VS.norm to == 0       = to
    | VS.norm from == 0     = zeroVector
    | otherwise             = max 0 (normalize to `dot` from) *^ normalize to
