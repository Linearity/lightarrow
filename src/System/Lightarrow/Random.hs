module System.Lightarrow.Random where

import Data.Word
import System.Lightarrow.Platform

class Platform m => RandomPlatform m where
    getRandom :: Resources m -> m Word32