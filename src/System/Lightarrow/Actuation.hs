module System.Lightarrow.Actuation where

import System.Lightarrow.Platform

class (Platform m, Monoid (Actuation m)) => ActuatePlatform m where
    data Actuation m :: *
    actuate :: Resources m -> Actuation m -> m ()
