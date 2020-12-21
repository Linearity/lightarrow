module Graphics.Lightarrow.Blit where

import Data.Lightarrow.Bitmap
import Data.Lightarrow.Color
import Data.Lightarrow.SceneTransform
import Linear
import System.Lightarrow.Actuation

-- | Platforms that copy images to a screen
class (BitmapPlatform m, ActuatePlatform m) => BlitPlatform m where
    {-|
    
    Copy a bitmap to the screen.  The bitmap may be tinted; use 'White' for no
    tint.  Its position in the frame has three components: horizontal and
    vertical offset of the upper-left corner from the center of the frame, and
    depth.  Bitmaps are drawn in order of increasing depth value, that is
    bitmaps with greater depth are drawn over those with lesser depth.

    -}
    blit    :: Bitmap m                             -- ^ the bitmap to copy
                    -> Color                        -- ^ tint
                    -> (Double, Double)             -- ^ horizontal and vertical scaling
                    -> (Double, Double, Double)     -- ^ position in frame
                    -> Actuation m                  -- ^ the copy command

-- | An actuation that draws a bitmap, filtered by a given color, with
-- its center at the origin in a given transformed space and scaled by the
-- scaling part of the transformation
sceneBlit :: BlitPlatform p =>
                Bitmap p                        -- ^ the bitmap to draw
                    -> Color                    -- ^ tint
                    -> SceneTransform Double    -- ^ transformation
                    -> Actuation p              -- ^ the drawing command
sceneBlit b c xf = blit b c (sX, sY) (x, y, z)
    where   V3 x y z    = t
            V3 sX sY _  = s
            (t, s)      = case center b xf of
                            TRS t _r s
                                -> (t, s)
                            MatrixTransform m
                                -> let  V4 c1 c2 c3 c4  = transpose m
                                        V4 x y z _w     = c4
                                    in (    V3 x y z,
                                            V3 (norm c1) (norm c2) (norm c3)    )