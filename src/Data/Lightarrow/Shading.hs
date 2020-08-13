module Data.Lightarrow.Shading where

import Data.Lightarrow.Artifact
import Data.Typeable
import Linear

class Typeable a => Uniform a where
    mapView :: RealFrac b => (M44 b -> M44 b) -> a -> a
    mapProj :: RealFrac b => (M44 b -> M44 b) -> a -> a

class ArtifactPlatform (Shading v u m) m => ShadingPlatform v u m where
    data Shading (v :: *) u m
    mapUniform :: (u -> u) -> Shading v u m -> Shading v u m
