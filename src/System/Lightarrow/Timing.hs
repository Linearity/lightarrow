module System.Lightarrow.Timing where

import FRP.BearRiver

import System.Lightarrow.Platform

-- | Identifier for different time steps
type family TimeGroup

-- | Platforms that are aware of the passage of time
class Platform m => TimingPlatform m where
    -- | Determine how big of a time step to take
    timeStep :: Resources m -> TimeGroup -> m DTime
    -- | Determine the sampling rate in time steps per second
    stepRate :: Resources m -> TimeGroup -> m DTime
