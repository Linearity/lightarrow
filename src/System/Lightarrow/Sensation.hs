module System.Lightarrow.Sensation where

import System.Lightarrow.Platform

-- | Platforms that record input signals from their environments
class (Platform m, Monoid (Sensation m)) => SensePlatform m where
    -- | Input signal
    data Sensation m :: *
    -- | Record an input sample from the environment
    sense   :: Resources m -> m (Sensation m)
