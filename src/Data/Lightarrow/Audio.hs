module Data.Lightarrow.Audio where

import Data.Lightarrow.Artifact

class ArtifactPlatform (Audio a) a => AudioPlatform a where
    data Audio a
