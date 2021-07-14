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
                    -> V2 Double                    -- ^ rectangle dimensions
                    -> V3 Double                    -- ^ position in frame
                    -> Actuation m                  -- ^ the drawing command

-- | An actuation that draws a filled rectangle of the given dimensions, with
-- its upper-left corner at the origin in local space and scaled by the
-- scaling part of the transformation
sceneRectangle :: RectanglePlatform m =>
                    Color                           -- ^ fill color
                        -> (Double, Double)         -- ^ rectangle dimensions
                        -> SceneTransform Double    -- ^ transformation
                        -> Actuation m              -- ^ the drawing command
sceneRectangle c (w, h) xf = rectangle c (V2 sX sY) (getTranslation xf - halfScale)
    where   halfScale       = V3 (1/2) (1/2) 1 * s
            s@(V3 sX sY _)  = getScale xf * V3 w h 0