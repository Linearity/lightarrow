module Sound.Lightarrow.Playback where

import Data.Lightarrow.Audio
import System.Lightarrow.Actuation

newtype UnitReal = UnitReal Double

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

class (AudioPlatform m, ActuatePlatform m) => MixerPlatform m where
    data Channel m
    channel :: Int -> Channel m
    fade    :: Channel m -> UnitReal -> Actuation m
    stop    :: Channel m -> Actuation m

class (AudioPlatform m, ActuatePlatform m) => PlaybackPlatform m where
    play    :: Channel m -> Audio m -> Actuation m
    onLoop  :: Channel m -> Audio m -> Actuation m
