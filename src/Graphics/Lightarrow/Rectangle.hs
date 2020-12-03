module Graphics.Lightarrow.Rectangle where

import Data.Lightarrow.Color
import System.Lightarrow.Actuation

class ActuatePlatform m => RectanglePlatform m where
    rectangle   :: Color
                    -> (Double, Double)
                    -> (Double, Double, Double)
                    -> Actuation m
