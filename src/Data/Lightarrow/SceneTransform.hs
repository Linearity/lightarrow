module Data.Lightarrow.SceneTransform where

import Data.Lightarrow.Bitmap
import qualified Linear (rotate)
import Linear hiding (rotate)
import Linear.Affine
import Optics

-- | A transformation of 3D space with multiple representations
data SceneTransform a
        -- | Decomposition into translation, rotation, and scaling
        = TRS {
            trsT :: V3 a,           -- ^ translation vector
            trsR :: Quaternion a,   -- ^ rotation quaternion
            trsS :: V3 a            -- ^ scaling vector
            }
        -- | Generic 4×4 matrix
        | MatrixTransform (M44 a)
    deriving Show

-- | The translation part of a given transformation
getTranslation :: SceneTransform a -> V3 a
getTranslation (TRS t _ _) = t
getTranslation (MatrixTransform m) = V3 x y z
    where   V4 _ _ _ (V4 x y z _w) = transpose m

-- | Replace the translation part of a transformation
setTranslation :: V3 a -> SceneTransform a -> SceneTransform a
setTranslation t (TRS _ r s) = TRS t r s
setTranslation (V3 x y z) (MatrixTransform m) = MatrixTransform m'
    where   m'                          = transpose (V4 c1 c2 c3 (V4 x y z w))
            V4 c1 c2 c3 (V4 _ _ _ w)    = transpose m

-- | Read/write access to the translation part of a transformation
_translation :: Lens' (SceneTransform a) (V3 a)
_translation = lens getTranslation (flip setTranslation)

-- | The rotation part of a given transformation
getRotation :: (Epsilon a, Floating a, Ord a) => SceneTransform a -> Quaternion a
getRotation (TRS _ r _) = r
getRotation (MatrixTransform m) = Quaternion q1 (V3     (signum (r32 - r23) * q2)
                                                        (signum (r13 - r31) * q3)
                                                        (signum (r21 - r12) * q4))
    where   q1  | r11 + r22 + r33 > eta     = sqrt (1 + r11 + r22 + r33) / 2
                | otherwise                 = sqrt (    (       (r32 - r23) ** 2
                                                            +   (r13 - r31) ** 2
                                                            +   (r21 - r12) ** 2)
                                                        / (3 - r11 - r22 - r33)     )
            q2  | r11 - r22 - r33 > eta     = sqrt (1 + r11 - r22 - r33) / 2
                | otherwise                 = sqrt (    (       (r32 - r23) ** 2
                                                            +   (r12 + r21) ** 2
                                                            +   (r31 - r13) ** 2)
                                                        / (3 - r11 + r22 + r33)     )
            q3  | (-r11) + r22 - r33 > eta  = sqrt (1 - r11 + r22 - r33) / 2
                | otherwise                 = sqrt (    (       (r13 - r31) ** 2
                                                            +   (r12 + r21) ** 2
                                                            +   (r23 + r32) ** 2)
                                                        / (3 + r11 - r22 + r33)     )
            q4  | (-r11) - r22 + r33 > eta  = sqrt (1 - r11 - r22 + r33) / 2
                | otherwise                 = sqrt (    (       (r21 - r12) ** 2
                                                            +   (r31 + r13) ** 2
                                                            +   (r32 + r23) ** 2)
                                                        / (3 + r11 + r22 - r33)     )
            eta                             = 0
            V4 r11 r21 r31 _                = normalize c1
            V4 r12 r22 r32 _                = normalize c2
            V4 r13 r23 r33 _                = normalize c3
            V4 c1 c2 c3 _                   = transpose m

-- | Replace the rotation part of a transformation
setRotation :: Floating a => Quaternion a -> SceneTransform a -> SceneTransform a
setRotation r (TRS t _ s) = TRS t r s
setRotation r (MatrixTransform m) = MatrixTransform m'
    where   m'              = scaled (V4 (norm c1) (norm c2) (norm c3) 1)
                                !*! mkTransformation r (V3 x y z)
            V4 x y z _      = c4
            V4 c1 c2 c3 c4  = transpose m

-- | Read/write access to the translation part of a transformation
_rotation :: (Epsilon a, Floating a, Ord a) => Lens' (SceneTransform a) (Quaternion a)
_rotation = lens getRotation (flip setRotation)

-- | The scaling part of a given transformation
getScale :: Floating a => SceneTransform a -> V3 a
getScale (TRS _ _ s) = s
getScale (MatrixTransform m) = V3 sx sy sz
    where   V4 c1 c2 c3 _   = transpose m
            sx              = norm c1
            sy              = norm c2
            sz              = norm c3

-- | Replace the scaling part of a transformation
setScale :: (Epsilon a, Floating a) => V3 a -> SceneTransform a -> SceneTransform a
setScale s (TRS t r _) = TRS t r s
setScale (V3 sx sy sz) (MatrixTransform m) = MatrixTransform m'
    where   m'              = transpose (V4 c1' c2' c3' c4)
            c1'             = sx *^ normalize c1
            c2'             = sy *^ normalize c2
            c3'             = sz *^ normalize c3
            V4 c1 c2 c3 c4  = transpose m

-- | Read/write access to the scaling part of a transformation
_scale :: (Epsilon a, Floating a) => Lens' (SceneTransform a) (V3 a)
_scale = lens getScale (flip setScale)

-- | Compose two transformations from right to left
composeXf :: (Conjugate a, RealFloat a)
                => SceneTransform a
                    -> SceneTransform a
                    -> SceneTransform a
composeXf (MatrixTransform m1) xf2 = MatrixTransform (m1 !*! toMatrix xf2)
composeXf xf1 (MatrixTransform m2) = MatrixTransform (toMatrix xf1 !*! m2)
composeXf (TRS t1 r1 s1) (TRS t2 r2 s2) = composite
    where   composite   = TRS tC rC sC
            tC          = t1 ^+^ Linear.rotate r1 (s1 * t2)
            rC          = r1 * r2
            sC          = s1 * s2

-- | The identity transformation
identityXf :: Num a => SceneTransform a
identityXf = TRS (V3 0 0 0) (Quaternion 1 (V3 0 0 0)) (V3 1 1 1)

-- | The inverse of a given transformation, always in the matrix representation
inverseXf :: (Conjugate a, RealFloat a) => SceneTransform a -> SceneTransform a
inverseXf t@TRS {}              = inverseXf (MatrixTransform (toMatrix t))
inverseXf (MatrixTransform m)   = MatrixTransform (inv44 m)

-- | A matrix representing a given transformation
toMatrix :: Num a => SceneTransform a -> M44 a
toMatrix (TRS t r s) = mTR !*! mS
    where   mTR = mkTransformation r t
            mS  = scaled (point s)
toMatrix (MatrixTransform m) = m

-- | A translate-rotate-scale transformation in the \(xy\)-plane
trs2 :: (Epsilon a, Floating a) => V2 a -> a -> V2 a -> SceneTransform a
trs2 (V2 x y) r (V2 sX sY) = TRS (V3 x y 0) (axisAngle (V3 0 0 1) r) (V3 sX sY 1)

-- | Translation and rotation in 3D
rigid :: (Epsilon a, RealFloat a)
         => Point V3 a
            -> Quaternion a
            -> SceneTransform a
rigid (P t) r = TRS t r (V3 1 1 1)

-- | A rigid transformation (translation and rotation) in the \(xy\)-plane
rigid2 :: (Epsilon a, Floating a) => Point V2 a -> a -> SceneTransform a
rigid2 (P (V2 x y)) r = TRS (V3 x y 0) (axisAngle (V3 0 0 1) r) (V3 1 1 1)

-- | Translation in 3D
translate :: Num a => V3 a -> SceneTransform a
translate t = TRS t (Quaternion 1 (V3 0 0 0)) (V3 1 1 1)

-- | Translation in the \(xy\)-plane
translate2 :: (Conjugate a, RealFloat a) => V2 a -> SceneTransform a
translate2 (V2 x y) = TRS (V3 x y 0) (Quaternion 1 (V3 0 0 0)) (V3 1 1 1)

-- | Add an extra offset to the translation part of a transformation
addTranslate :: Num a => V3 a -> SceneTransform a -> SceneTransform a
addTranslate t (TRS t0 r s) = TRS (t ^+^ t0) r s

-- | Translation to a given point in 3D
locate :: Num a => Point V3 a -> SceneTransform a
locate (P t) = translate t

-- | Translation to a given point in the \(xy\)-plane
locate2 :: (Conjugate a, RealFloat a) => Point V2 a -> SceneTransform a
locate2 (P t) = translate2 t

-- | Rotation in 3D
rotate :: RealFloat a => Quaternion a -> SceneTransform a
rotate r = TRS (V3 0 0 0) r (V3 1 1 1)

-- | Rotation about the \(x\)-axis
rotateX :: (Epsilon a, RealFloat a) => a -> SceneTransform a
rotateX r = TRS (V3 0 0 0) (axisAngle (V3 1 0 0) r) (V3 1 1 1)

-- | Rotation about the \(y\)-axis
rotateY :: (Epsilon a, RealFloat a) => a -> SceneTransform a
rotateY r = TRS (V3 0 0 0) (axisAngle (V3 0 1 0) r) (V3 1 1 1)

-- | Rotation about the \(z\)-axis
rotateZ :: (Epsilon a, RealFloat a) => a -> SceneTransform a
rotateZ r = TRS (V3 0 0 0) (axisAngle (V3 0 0 1) r) (V3 1 1 1)

-- | Multiply the rotation part of a transformation by an extra rotation
addRotate :: (Epsilon a, RealFloat a) => Quaternion a -> SceneTransform a -> SceneTransform a
addRotate :: RealFloat a => Quaternion a -> SceneTransform a -> SceneTransform a
addRotate r (TRS t r0 s) = TRS t (r * r0) s

-- | Scaling in 3D
scale :: Num a => V3 a -> SceneTransform a
scale s = TRS (V3 0 0 0) (Quaternion 1 (V3 0 0 0)) s

-- | Scaling in the \(xy\)-plane
scale2 :: Num a => V2 a -> SceneTransform a
scale2 (V2 x y) = scale (V3 x y 1)

-- | Multiply the scaling part of a transformation by an extra scaling factor
multScale :: Num a => V3 a -> SceneTransform a -> SceneTransform a
multScale s (TRS t r s0) = TRS t r (s * s0)

{-|

Add an offset of half the dimensions of a bitmap, in the \(-x\) and \(+y\)
directions, to the translation part of a transformation

-}
center :: (BitmapPlatform m, Floating b)
            => Bitmap m -> SceneTransform b -> SceneTransform b
center b (TRS t r s) = TRS (t ^-^ offset) r s
    where   offset  = s * fmap ((/ 2) . fromIntegral) (V3 w h 0)
            (w, h)  = dimensions b
center b (MatrixTransform m) = MatrixTransform (transpose (V4 c1 c2 c3 (t ^-^ point offset)))
    where   offset          = s * fmap ((/ 2) . fromIntegral) (V3 w h 0)
            (w, h)          = dimensions b
            s               = V3 (norm c1) (norm c2) 1
            V4 c1 c2 c3 t   = transpose m

{-|

A parallax translation in the \(xy\)-plane that emulates perspective projection
in 3D. It uses a given, assumed distance from the eye to the screen, and
divides it by the distance from the eye to the transformed origin (the given
screen distance minus the \(z\)-component of the translation).

-}
parallax :: Floating a => a -> SceneTransform a -> SceneTransform a 
parallax ds (TRS (V3 x y z) r s) = TRS (V3 (x * k) (y * k) z) r s
    where   k = ds / (ds - z)
parallax ds (MatrixTransform m1) = MatrixTransform m2
    where   V4 c1 c2 c3 c4  = transpose m1
            V4 x y z w      = c4
            k               = ds / (ds - z)
            m2              = transpose (V4 c1 c2 c3 (V4 (k * x) (k * y) z w))