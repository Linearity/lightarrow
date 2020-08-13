module System.Lightarrow.Joystick where

import System.Lightarrow.Sensation

class (     SensePlatform m,
            JoystickAxis (JoystickAxisCode m),
            JoystickButton (JoystickButtonCode m)   ) => JoystickPlatform m where
    type JoystickAxisCode m
    type JoystickButtonCode m
    joystickPressed     :: JoystickButtonCode m -> Sensation m -> Bool
    joystickTilt        :: JoystickAxisCode m -> Sensation m -> Double

class JoystickButton a where
    otherJoyButton :: Int -> a

class JoystickAxis a where
    joyAxisX        :: a
    joyAxisY        :: a
    joyAxisLeftX    :: a
    joyAxisLeftY    :: a
    joyAxisRightX   :: a
    joyAxisRightY   :: a
    otherJoyAxis    :: Int -> a
