module System.Lightarrow.Mouse where

import System.Lightarrow.Sensation

class (SensePlatform m, MouseButton (MouseButtonCode m)) => MousePlatform m where
    type MouseButtonCode m
    cursorPosition      :: Sensation m -> (Double, Double)
    setCursorPosition   :: Sensation m -> (Double, Double) -> Sensation m
    mouseVelocity       :: Sensation m -> (Double, Double)
    setMouseVelocity    :: Sensation m -> (Double, Double) -> Sensation m
    mousePressed        :: MouseButtonCode m -> Sensation m -> Bool
    setMousePressed     :: MouseButtonCode m -> Sensation m -> Bool -> Sensation m

class SensePlatform m => ScrollPlatform m where
    scrollSpeed :: Sensation m -> Double

class MouseButton a where
    leftMouseButton     :: a
    middleMouseButton   :: a
    otherMouseButton    :: Int -> a
    rightMouseButton    :: a
