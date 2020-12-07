module Data.Lightarrow.Mesh where

import Data.Lightarrow.Artifact
import Data.Typeable

-- | Mesh vertices
class Typeable a => Vertex a where
    -- empty class

-- | Platforms that represent meshes approximating 3D surfaces
class ArtifactPlatform (Mesh v m) m => MeshPlatform v m where
    -- | The type of the mesh representation
    data Mesh v m
    --fromTris :: [(v, v, v)] -> Resources m -> Mesh v m