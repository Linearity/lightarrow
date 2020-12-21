module System.Lightarrow.Joystick where

import System.Lightarrow.Sensation

-- | Platforms that receive joystick input
class (     SensePlatform m,
            JoystickAxis (JoystickAxisCode m),
            JoystickButton (JoystickButtonCode m)   ) => JoystickPlatform m where
    -- | The identifiers for joystick axes
    type JoystickAxisCode m
    -- | The identifiers for joystick buttons
    type JoystickButtonCode m
    -- | Whether a given joystick button is pressed
    joystickPressed     :: JoystickButtonCode m -> Sensation m -> Bool
    -- | The amount by which the joystick is tilted on the given axis
    joystickTilt        :: JoystickAxisCode m -> Sensation m -> Double

-- | Types that identify joystick buttons
class JoystickButton a where
    -- | The joystick button identified by the given integer
    otherJoyButton :: Int -> a

-- | Types that identify joystick axes
class JoystickAxis a where
    joyAxisX        :: a
    joyAxisY        :: a
    -- | The left joystick's \(x\)-axis
    joyAxisLeftX    :: a
    -- | The left joystick's \(y\)-axis
    joyAxisLeftY    :: a
    -- | The right joystick's \(x\)-axis
    joyAxisRightX   :: a
    -- | The right joystick's \(y\)-axis
    joyAxisRightY   :: a
    -- | The joystick axis identified by the given integer
    otherJoyAxis    :: Int -> a
