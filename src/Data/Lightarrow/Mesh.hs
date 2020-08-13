module Data.Lightarrow.Mesh where

import Data.Lightarrow.Artifact
import Data.Typeable

class Typeable a => Vertex a where
    -- empty class

class ArtifactPlatform (Mesh v m) m => MeshPlatform v m where
    data Mesh v m
    --fromTris :: [(v, v, v)] -> Resources m -> Mesh v m
