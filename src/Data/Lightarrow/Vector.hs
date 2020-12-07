module Data.Lightarrow.Vector where

import           Control.Applicative
import           Data.VectorSpace (VectorSpace, zeroVector, normalize, dot, (*^))
import qualified Data.VectorSpace as VS
import           Linear (Conjugate, Epsilon, Quaternion, V2(..), V3(..), (^-^))
import qualified Linear as L
import           Linear.Affine (Affine, Diff, Point, (.-.))

-- | Truncate or extend a vector so its magnitude lies in the given bounds
clampLength :: (Ord a, VectorSpace v a) =>
                    a               -- ^ minimum magnitude
                        -> a        -- ^ maximum magnitude
                        -> v        -- ^ original vector
                        -> v        -- ^ clamped vector
clampLength short long v    | m > long      = (long / m) *^ v
                            | m < short     = (short / m) *^ v
                            | otherwise     = v
    where   m = VS.norm v

-- | Whether a 2D point lies on or within a given rectangle
pointInBox2 :: (Epsilon a, Floating a, Ord a) =>
                    (Point V2 a, V2 a)      -- ^ (center, diagonal) of box
                        -> a                -- ^ rotation of box about center
                        -> Point V2 a       -- ^ the point in question
                        -> Bool             -- ^ whether the point is inside the box
pointInBox2 :: (Epsilon a, Floating a, Ord a)
                => (Point V2 a, V2 a) -> a -> Point V2 a -> Bool
pointInBox2 (center, V2 dx dy) r p = L.norm x < dx/2 && L.norm y < dy/2
    where   dp  = p .-. center
            x   = L.project (L.angle r) dp
            y   = dp ^-^ x

-- | Whether a 3D point lies on or within a given rectangular prism
pointInBox3 :: (Affine p, Conjugate a, RealFloat a, Linear.Affine.Diff p ~ V3) =>
                    (p a, V3 a)             -- ^ (center, diagonal) of box
                        -> Quaternion a     -- ^ rotation of box about center
                        -> p a              -- ^ the point in question
                        -> Bool             -- ^ whether the point is inside the box
pointInBox3 :: (Affine p, Conjugate a, RealFloat a, Linear.Affine.Diff p ~ V3)
                => (p a, V3 a) -> Quaternion a -> p a -> Bool 
pointInBox3 (c, d) r = pointInBox (c, d) (L.rotate r)
{-|

Whether a point lies within (or on the boundary of) a space bounded by
orthogonal hyperplanes.  The space may be transformed from world space, for
example by a rotation.

-}
pointInBox :: (Affine p, Foldable t, Applicative t, Ord a, Fractional a, Num b) =>
                (p b, t a)                  -- ^ (center, diagonal) of box
                    -> (Diff p b -> t a)    -- ^ world-to-box coordinate transform
                    -> p b                  -- ^ the point in question
                    -> Bool                 -- ^ whether the point is inside the box
pointInBox (center, dimensions) r p
        = and (liftA2 ((<=) . abs) (r dp) (fmap (/2) dimensions))
    where   dp  = p .-. center

{-|

The forward component of one vector with respect to another.  This is like the
projection of the first on the second, but if their dot product is
negative we define the forward component to be zero.

-}
forward :: (Ord a, VectorSpace v a) => v -> v -> v
forward to from
    | VS.norm to == 0       = to
    | VS.norm from == 0     = zeroVector
    | otherwise             = max 0 (normalize to `dot` from) *^ normalize to
