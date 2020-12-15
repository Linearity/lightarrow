module Graphics.Lightarrow.Rectangle where

import Data.Lightarrow.Color
import Data.Lightarrow.SceneTransform
import Linear
import System.Lightarrow.Actuation

-- | Platforms that draw filled rectangles on the screen
class ActuatePlatform m => RectanglePlatform m where
    {-|
    
    Draw a filled rectangle on the screen.  Its position in the frame has
    three components: horizontal and vertical offset of the upper-left corner
    from the center of the frame, and depth.  Bitmaps are drawn in order of
    increasing depth value, that is bitmaps with greater depth are drawn over
    those with lesser depth.

    -}
    rectangle   :: Color                            -- ^ fill color
                    -> (Double, Double)             -- ^ rectangle dimensions
                    -> (Double, Double, Double)     -- ^ position in frame
                    -> Actuation m                  -- ^ the drawing command

drawRectangle c (w, h) xf = rectangle c (sw, sh) (x - sw/2, y - sh/2, z)
    where   (sw, sh)        = (sX * w, sY * h)
            V3 x y z        = getTranslation xf
            V3 sX sY _      = getScale xf