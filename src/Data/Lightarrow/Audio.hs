module Data.Lightarrow.Audio where

import Data.Lightarrow.Artifact

-- | Platforms that represent audio recordings
class ArtifactPlatform (Audio a) a => AudioPlatform a where
    -- | The type of the audio representation
    data Audio a