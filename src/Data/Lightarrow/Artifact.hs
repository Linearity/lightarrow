module Data.Lightarrow.Artifact where

import System.Lightarrow.Platform

{-|

Platforms that represent some type of artifact of the game production
other than source code, otherwise called a "resource" or "asset"

-}
class Platform m => ArtifactPlatform a m where
    -- | Produce a default artifact
    dummy :: Resources m -> a