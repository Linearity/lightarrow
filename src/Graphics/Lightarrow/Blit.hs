module Graphics.Lightarrow.Blit where

import Data.Lightarrow.Bitmap
import Data.Lightarrow.Color
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