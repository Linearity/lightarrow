module Sound.Lightarrow.Mixer (     MixerPlatform(..),
                                    UnitReal,
                                    unitReal,
                                    fromUnitReal    ) where

import Data.Lightarrow.Audio
import System.Lightarrow.Actuation

-- | A value clamped between 0 and 1.
newtype UnitReal = UnitReal Double

-- | A 'UnitReal' with a given value clamped between 0 and 1
unitReal :: Double -> UnitReal
unitReal x = UnitReal (clamp x)

-- | The constrained value of a 'UnitReal' between 0 and 1
fromUnitReal :: UnitReal -> Double
fromUnitReal (UnitReal x) = x

-- | Clamp a value between 0 and 1
clamp :: Double -> Double
clamp x = min 1 (max 0 x)

instance Num UnitReal where
    fromInteger             = UnitReal . clamp . fromInteger
    UnitReal x + UnitReal y = UnitReal (clamp (x + y))
    UnitReal x - UnitReal y = UnitReal (clamp (x - y))
    UnitReal x * UnitReal y = UnitReal (clamp (x * y))
    abs                     = id
    signum (UnitReal 0)     = 0
    signum (UnitReal _)     = 1

instance Fractional UnitReal where
    fromRational x              = UnitReal (clamp (fromRational x))
    UnitReal x / UnitReal y     = UnitReal (clamp (x / y))

-- | Platforms that mix multiple audio streams into one and play it back
class (AudioPlatform m, ActuatePlatform m) => MixerPlatform m where
    data Channel m
    channel :: Int -> Channel m
    fade    :: Channel m -> UnitReal -> Actuation m
    stop    :: Channel m -> Actuation m
    play    :: Channel m -> Audio m -> Actuation m
    onLoop  :: Channel m -> Audio m -> Actuation m
