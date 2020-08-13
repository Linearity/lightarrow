module System.Lightarrow.Timing where

import FRP.BearRiver

import System.Lightarrow.Platform

type family TimeGroup

class Platform m => TimingPlatform m where
    timeStep :: Resources m -> TimeGroup -> m DTime
    stepRate :: Resources m -> TimeGroup -> m DTime
