module Data.Lightarrow.SceneTransform where

import Data.Lightarrow.Bitmap
import qualified Linear (rotate)
import Linear hiding (rotate)
import Linear.Affine
import Optics

data SceneTransform a   = TRS {     trsT :: V3 a,
                                    trsR :: Quaternion a,
                                    trsS :: V3 a    }
                        | MatrixTransform (M44 a)
    deriving Show

getTranslation :: SceneTransform a -> V3 a
getTranslation (TRS t _ _) = t
getTranslation (MatrixTransform m) = V3 x y z
    where   V4 _ _ _ (V4 x y z w) = transpose m

setTranslation :: V3 a -> SceneTransform a -> SceneTransform a
setTranslation t (TRS _ r s) = TRS t r s
setTranslation (V3 x y z) (MatrixTransform m) = MatrixTransform m'
    where   m'                          = transpose (V4 c1 c2 c3 (V4 x y z w))
            V4 c1 c2 c3 (V4 _ _ _ w)    = transpose m

_translation :: Lens' (SceneTransform a) (V3 a)
_translation = lens getTranslation (flip setTranslation)

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

setRotation :: Floating a => Quaternion a -> SceneTransform a -> SceneTransform a
setRotation r (TRS t _ s) = TRS t r s
setRotation r (MatrixTransform m) = MatrixTransform m'
    where   m'              = scaled (V4 (norm c1) (norm c2) (norm c3) 1)
                                !*! mkTransformation r (V3 x y z)
            V4 x y z _      = c4
            V4 c1 c2 c3 c4  = transpose m

_rotation :: (Epsilon a, Floating a, Ord a) => Lens' (SceneTransform a) (Quaternion a)
_rotation = lens getRotation (flip setRotation)

getScale :: Floating a => SceneTransform a -> V3 a
getScale (TRS _ _ s) = s
getScale (MatrixTransform m) = V3 sx sy sz
    where   V4 c1 c2 c3 _   = transpose m
            sx              = norm c1
            sy              = norm c2
            sz              = norm c3

setScale :: (Epsilon a, Floating a) => V3 a -> SceneTransform a -> SceneTransform a
setScale s (TRS t r _) = TRS t r s
setScale (V3 sx sy sz) (MatrixTransform m) = MatrixTransform m'
    where   m'              = transpose (V4 c1' c2' c3' c4)
            c1'             = sx *^ normalize c1
            c2'             = sy *^ normalize c2
            c3'             = sz *^ normalize c3
            V4 c1 c2 c3 c4  = transpose m

_scale :: (Epsilon a, Floating a) => Lens' (SceneTransform a) (V3 a)
_scale = lens getScale (flip setScale)

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

identityXf :: Num a => SceneTransform a
identityXf = TRS (V3 0 0 0) (Quaternion 1 (V3 0 0 0)) (V3 1 1 1)

inverseXf :: (Conjugate a, RealFloat a) => SceneTransform a -> SceneTransform a
inverseXf t@(TRS _ _ _)         = inverseXf (MatrixTransform (toMatrix t))
inverseXf (MatrixTransform m)   = MatrixTransform (inv44 m)

toMatrix :: Num a => SceneTransform a -> M44 a
toMatrix (TRS t r s) = mTR !*! mS
    where   mTR = mkTransformation r t
            mS  = scaled (point s)
toMatrix (MatrixTransform m) = m

trs2 (V2 x y) r (V2 sX sY) = TRS (V3 x y 0) (axisAngle (V3 0 0 1) r) (V3 sX sY 1)

rigid :: (Epsilon a, RealFloat a)
         => Point V3 a
            -> Quaternion a
            -> SceneTransform a
rigid (P t) r = TRS t r (V3 1 1 1)
{-

A subset of such rigid transforms assumes that translation is always in the
$x$-$y$ plane and that rotation is always about the $z$ axis.

-}
rigid2 (P (V2 x y)) r = TRS (V3 x y 0) (axisAngle (V3 0 0 1) r) (V3 1 1 1)

translate :: Num a => V3 a -> SceneTransform a
translate t = TRS t (Quaternion 1 (V3 0 0 0)) (V3 1 1 1)

translate2 :: (Conjugate a, RealFloat a) => V2 a -> SceneTransform a
translate2 (V2 x y) = TRS (V3 x y 0) (Quaternion 1 (V3 0 0 0)) (V3 1 1 1)

addTranslate :: Num a => V3 a -> SceneTransform a -> SceneTransform a
addTranslate t (TRS t0 r s) = TRS (t ^+^ t0) r s

locate :: Num a => Point V3 a -> SceneTransform a
locate (P t) = translate t

locate2 :: (Conjugate a, RealFloat a) => Point V2 a -> SceneTransform a
locate2 (P t) = translate2 t

rotate :: RealFloat a => Quaternion a -> SceneTransform a
rotate r = TRS (V3 0 0 0) r (V3 1 1 1)

rotateX :: (Epsilon a, RealFloat a) => a -> SceneTransform a
rotateX r = TRS (V3 0 0 0) (axisAngle (V3 1 0 0) r) (V3 1 1 1)

rotateY :: (Epsilon a, RealFloat a) => a -> SceneTransform a
rotateY r = TRS (V3 0 0 0) (axisAngle (V3 0 1 0) r) (V3 1 1 1)

rotateZ :: (Epsilon a, RealFloat a) => a -> SceneTransform a
rotateZ r = TRS (V3 0 0 0) (axisAngle (V3 0 0 1) r) (V3 1 1 1)

addRotate :: RealFloat a => Quaternion a -> SceneTransform a -> SceneTransform a
addRotate r (TRS t r0 s) = TRS t (r * r0) s

scale :: Num a => V3 a -> SceneTransform a
scale s = TRS (V3 0 0 0) (Quaternion 1 (V3 0 0 0)) s

scale2 :: Num a => V2 a -> SceneTransform a
scale2 (V2 x y) = scale (V3 x y 1)

multScale :: Num a => V3 a -> SceneTransform a -> SceneTransform a
multScale s (TRS t r s0) = TRS t r (s * s0)
{-

Blitting bitmap images calls for some special, convenient compositions. One is
an automatic centering translation based on the dimensions of the bitmap.

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
{-

Another is a parallax effect that emulates perspective projection in $R^3$. It
uses a given distance from the eye to the screen, and divides it by distance to
the transformed origin. The latter is the given screen distance plus the
negative of either the $Z$ component of the translation or a given $Z$-value
(assuming a translation in $R^2$ with no $Z$ component).

-}
parallax :: Floating a => a -> SceneTransform a -> SceneTransform a 
parallax ds (TRS (V3 x y z) r s) = TRS (V3 (x * k) (y * k) z) r s
    where   k = ds / (ds - z)
parallax ds (MatrixTransform m1) = MatrixTransform m2
    where   V4 c1 c2 c3 c4  = transpose m1
            V4 x y z w      = c4
            k               = ds / (ds - z)
            m2              = transpose (V4 c1 c2 c3 (V4 (k * x) (k * y) z w))
