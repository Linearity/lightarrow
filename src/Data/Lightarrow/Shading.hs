module Data.Lightarrow.Shading where

import Data.Lightarrow.Artifact
import Data.Typeable
import Linear

-- | Uniform shading parameters
class Typeable a => Uniform a where

    -- | Adjust the view transformation
    mapView :: RealFrac b =>
                    (M44 b -> M44 b)    -- ^ adjustment function
                        -> a            -- ^ original uniform
                        -> a            -- ^ adjusted uniform

    -- | Adjust the projection transformation
    mapProj :: RealFrac b =>
                    (M44 b -> M44 b)    -- ^ adjustment function
                        -> a            -- ^ original uniform
                        -> a            -- ^ adjusted uniform

-- | Platforms that represent shadings, i.e. shaders and uniforms
class ArtifactPlatform (Shading v u m) m => ShadingPlatform v u m where

    -- | The type of the shading representation
    data Shading (v :: *) u m

    -- | Adjust the uniform parameter of the shading
    mapUniform :: (u -> u)              -- ^ adjustment function
                    -> Shading v u m    -- ^ original shading
                    -> Shading v u m    -- ^ adjusted shading