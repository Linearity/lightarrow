module System.Lightarrow.Keyboard where

import System.Lightarrow.Sensation

class (SensePlatform m, Keyboard (KeyCode m)) => KeyboardPlatform m where
    type KeyCode m
    keyPressed :: KeyCode m -> Sensation m -> Bool

data Side = LeftSide | RightSide

class Keyboard a where
    altKey          :: Side -> a
    arrowDownKey    :: a
    arrowLeftKey    :: a
    arrowRightKey   :: a
    arrowUpKey      :: a
    backspaceKey    :: a
    capsLockKey     :: a
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
