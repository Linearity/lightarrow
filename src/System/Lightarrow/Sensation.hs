module System.Lightarrow.Sensation where

import System.Lightarrow.Platform

class (Platform m, Monoid (Sensation m)) => SensePlatform m where
    data Sensation m :: *
    sense   :: Resources m -> m (Sensation m)
