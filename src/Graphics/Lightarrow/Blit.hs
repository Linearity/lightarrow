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
    vertical offset of the bitmap's center from the center of the frame, and
    depth.  Bitmaps are drawn in order of increasing depth value, that is
    bitmaps with greater depth are drawn over those with lesser depth.

    -}
    blit    :: Bitmap m                             -- ^ the bitmap to copy
                    -> Color                        -- ^ tint
                    -> V2 Double                    -- ^ horizontal and vertical scaling
                    -> Double                       -- ^ counterclockwise rotation
                    -> V3 Double                    -- ^ position in frame
                    -> Actuation m                  -- ^ the copy command

-- | An actuation that draws a bitmap, filtered by a given color, with
-- its center at the origin in a given transformed space, rotated by the
-- rotation part of the transformation, and scaled by the scaling part of
-- the transformation
sceneBlit :: BlitPlatform p =>
                Bitmap p                        -- ^ the bitmap to draw
                    -> Color                    -- ^ tint
                    -> SceneTransform Double    -- ^ transformation
                    -> Actuation p              -- ^ the drawing command
sceneBlit b c !xf = blit b c (V2 sX sY) r (getTranslation xf)
    where   !r              = unangle (V2 rX rY)
            !(V3 rX rY _)   = Linear.rotate (getRotation xf) (V3 1 0 0)
            !(V3 sX sY _)   = getScale xf
            -- xf'         = center b xf