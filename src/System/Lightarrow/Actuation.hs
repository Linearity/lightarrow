module System.Lightarrow.Actuation where

import System.Lightarrow.Platform

-- | Platforms that do something in response to actuation commands
class (Platform m, Monoid (Actuation m)) => ActuatePlatform m where
    -- | Actuation command
    data Actuation m :: *
    -- | Take action in response to an actuation command
    actuate :: Resources m -> Actuation m -> m ()
