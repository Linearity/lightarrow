module Data.Lightarrow.Artifact where

import System.Lightarrow.Platform

class Platform m => ArtifactPlatform a m where
    dummy :: Resources m -> a
