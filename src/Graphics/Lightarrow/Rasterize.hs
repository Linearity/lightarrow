module Graphics.Lightarrow.Rasterize where

import Data.Lightarrow.Mesh
import Data.Lightarrow.Shading
import System.Lightarrow.Actuation

-- | Platforms that rasterize surface meshes on a screen
class (MeshPlatform v m, ShadingPlatform v u m, ActuatePlatform m, Vertex v, Uniform u)
        => RasterizePlatform v u m where
    -- | Draw a rasterized mesh on the screen using a given shading
    rasterize   :: Mesh v m             -- ^ the mesh to rasterize
                    -> Shading v u m    -- ^ how to shade the mesh
                    -> Actuation m      -- ^ the drawing command
