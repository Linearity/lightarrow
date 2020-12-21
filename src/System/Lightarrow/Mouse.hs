module System.Lightarrow.Mouse where

import System.Lightarrow.Sensation

-- | Platforms that receive mouse input
class (SensePlatform m, MouseButton (MouseButtonCode m)) => MousePlatform m where
    -- | The identifiers for mouse buttons
    type MouseButtonCode m
    -- | The position of the cursor on the screen
    cursorPosition      :: Sensation m -> (Double, Double)
    -- | A sensation with a modified cursor position
    setCursorPosition   :: Sensation m -> (Double, Double) -> Sensation m
    -- | The velocity of the cursor on the screen
    mouseVelocity       :: Sensation m -> (Double, Double)
    -- | A sensation with a modified cursor position
    setMouseVelocity    :: Sensation m -> (Double, Double) -> Sensation m
    -- | Whether a given mouse button is pressed
    mousePressed        :: MouseButtonCode m -> Sensation m -> Bool
    -- | A sensation with a modified mouse button state
    setMousePressed     :: MouseButtonCode m -> Sensation m -> Bool -> Sensation m

-- | Platforms that receive scroll-wheel input
class SensePlatform m => ScrollPlatform m where
    scrollSpeed :: Sensation m -> Double

-- | Types that identify mouse buttons
class MouseButton a where
    leftMouseButton     :: a
    middleMouseButton   :: a
    -- | The mouse button identified by the given integer
    otherMouseButton    :: Int -> a
    rightMouseButton    :: a
