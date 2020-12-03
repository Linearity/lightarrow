module Sound.Lightarrow.Playback (  fromUnitReal,
                                    MixerPlatform(..),
                                    PlaybackPlatform(..),
                                    UnitReal,
                                    unitReal    ) where

import Data.Lightarrow.Audio
import System.Lightarrow.Actuation

newtype UnitReal = UnitReal Double
{-

To make sure every |UnitReal| lies between 0 and 1, we hide the normal data
constructor |UnitReal| and instead export a smart constructor, |unitReal|.

-}
unitReal :: Double -> UnitReal
unitReal x = UnitReal (clamp x)

fromUnitReal :: UnitReal -> Double
fromUnitReal (UnitReal x) = x

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

class (AudioPlatform m, ActuatePlatform m) => MixerPlatform m where
    data Channel m
    channel :: Int -> Channel m
    fade    :: Channel m -> UnitReal -> Actuation m
    stop    :: Channel m -> Actuation m

class (AudioPlatform m, ActuatePlatform m) => PlaybackPlatform m where
    play    :: Channel m -> Audio m -> Actuation m
    onLoop  :: Channel m -> Audio m -> Actuation m
