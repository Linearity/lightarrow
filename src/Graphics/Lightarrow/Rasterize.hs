module Graphics.Lightarrow.Rasterize where

import Data.Lightarrow.Mesh
import Data.Lightarrow.Shading
import System.Lightarrow.Actuation

class (MeshPlatform v m, ShadingPlatform v u m, ActuatePlatform m, Vertex v, Uniform u)
        => RasterizePlatform v u m where
    rasterize   :: Mesh v m -> Shading v u m -> Actuation m
