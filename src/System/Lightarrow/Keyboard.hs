module System.Lightarrow.Keyboard where

import System.Lightarrow.Sensation

-- | Platforms that receive keyboard input
class (SensePlatform m, Keyboard (KeyCode m)) => KeyboardPlatform m where
    -- | The identifiers for keys
    type KeyCode m
    -- | Whether a given key is pressed
    keyPressed :: KeyCode m -> Sensation m -> Bool

-- | The side of the keyboard a key (like the shift-key) may be on
data Side = LeftSide | RightSide

-- | Types that identify keys
class Keyboard a where
    altKey          :: Side -> a
    arrowDownKey    :: a
    arrowLeftKey    :: a
    arrowRightKey   :: a
    arrowUpKey      :: a
    backspaceKey    :: a
    capsLockKey     :: a
    -- | Letter, number, and symbol keys, spacebar
    charKey         :: Char -> a
    commandKey      :: Side -> a
    controlKey      :: Side -> a
    deleteKey       :: a
    enterKey        :: a
    escapeKey       :: a
    fnKey           :: a
    functionKey     :: Int -> a
    optionKey       :: Side -> a
    otherKey        :: Int -> a
    returnKey       :: a
    shiftKey        :: Side -> a
    tabKey          :: a
    windowsKey      :: a
