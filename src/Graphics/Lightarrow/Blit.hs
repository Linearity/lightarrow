module Graphics.Lightarrow.Blit where

import Data.Lightarrow.Bitmap
import Data.Lightarrow.Color
import System.Lightarrow.Actuation

class (BitmapPlatform m, ActuatePlatform m) => BlitPlatform m where
    blit    :: Bitmap m
                    -> Color
                    -> (Double, Double)
                    -> (Double, Double, Double)
                    -> Actuation m
